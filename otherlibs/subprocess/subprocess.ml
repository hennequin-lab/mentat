(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Capture = struct
  type policy = All | Limit of int | Head_tail of { head : int; tail : int }

  type split = {
    head_limit : int;
    tail_limit : int;
    head : Buffer.t;
    mutable tail : string;
    mutable omitted : int;
  }

  type t = Plain of { limit : int option; buf : Buffer.t } | Rolling of split

  type parts =
    | Whole of string
    | Split of { head : string; omitted : int; tail : string }

  let initial_size limit = max 16 (min limit 4096)

  let create = function
    | All -> Plain { limit = None; buf = Buffer.create 4096 }
    | Limit limit ->
        if limit < 0 then invalid_arg "capture limit must be non-negative";
        Plain { limit = Some limit; buf = Buffer.create (initial_size limit) }
    | Head_tail { head; tail } ->
        if head < 0 || tail < 0 then
          invalid_arg "capture head and tail must be non-negative";
        Rolling
          {
            head_limit = head;
            tail_limit = tail;
            head = Buffer.create (initial_size head);
            tail = "";
            omitted = 0;
          }

  (* Rolling tail: keep the last [tail_limit] bytes seen, counting every byte
     dropped on the way as omitted. *)
  let add_tail s chunk offset len =
    if len = 0 then ()
    else if s.tail_limit = 0 then s.omitted <- s.omitted + len
    else
      let current = String.length s.tail in
      let total = current + len in
      if total <= s.tail_limit then
        s.tail <- s.tail ^ String.sub chunk offset len
      else if len >= s.tail_limit then begin
        s.omitted <- s.omitted + total - s.tail_limit;
        s.tail <- String.sub chunk (offset + len - s.tail_limit) s.tail_limit
      end
      else begin
        let keep_current = s.tail_limit - len in
        s.omitted <- s.omitted + current - keep_current;
        s.tail <-
          String.sub s.tail (current - keep_current) keep_current
          ^ String.sub chunk offset len
      end

  let add_split s chunk =
    let len = String.length chunk in
    if len = 0 then ()
    else
      let head_len = Buffer.length s.head in
      if head_len < s.head_limit then begin
        let keep = min len (s.head_limit - head_len) in
        Buffer.add_substring s.head chunk 0 keep;
        add_tail s chunk keep (len - keep)
      end
      else add_tail s chunk 0 len

  let add t chunk =
    match t with
    | Plain { limit = None; buf } ->
        Buffer.add_string buf chunk;
        `Ok
    | Plain { limit = Some limit; buf } ->
        let remaining = limit - Buffer.length buf in
        if String.length chunk <= remaining then begin
          Buffer.add_string buf chunk;
          `Ok
        end
        else begin
          if remaining > 0 then Buffer.add_substring buf chunk 0 remaining;
          `Exceeded limit
        end
    | Rolling s ->
        add_split s chunk;
        `Ok

  let parts = function
    | Plain { buf; _ } -> Whole (Buffer.contents buf)
    | Rolling { head; tail; omitted; _ } ->
        Split { head = Buffer.contents head; omitted; tail }
end

type termination =
  | Exited of Eio.Process.exit_status
  | Timed_out
  | Stopped
  | Output_limit of { stream : [ `Stdout | `Stderr ]; limit : int }
  | Supervision_failed of Eio.Exn.err

type outcome = {
  termination : termination;
  stdout : Capture.parts;
  stdout_complete : bool;
  stderr : Capture.parts;
  stderr_complete : bool;
  duration : Mtime.Span.t;
}

exception Launch of exn

(* Default time a signalled child may take to die before SIGKILL. *)
let default_grace = 0.2

(* Default time the drains may keep reading after the terminal cause resolved.
   Bounded so an orphaned descendant holding the pipes open cannot stall the
   return (leader-only cleanup: descendants may survive). A stream still
   reading when the grace expires is reported not complete, never silently
   short. *)
let default_drain_grace = 0.2

(* Default cooperative-stop poll period. The predicate is a cheap read owned by
   the caller's controller; polling it is the contract shape. *)
let default_poll = 0.05

let launch fn =
  match fn () with
  | value -> value
  | exception (Eio.Exn.Io _ as exn) -> raise (Launch exn)
  | exception (Unix.Unix_error _ as exn) -> raise (Launch exn)

(* Leader-only termination: SIGTERM, a bounded grace, then SIGKILL, and reap —
   exactly once, under [Cancel.protect] so a pending parent cancellation
   cannot interrupt the bounded cleanup.

   The reach is the direct child and no further. Widening it to the child's
   process group would mean signalling [-pid], and a pid only names our group
   while the child is unreaped: Eio reaps on its own and publishes no liveness
   a caller can test — a promise we resolve ourselves settles strictly after
   its reap — so the handle can go stale and name a stranger's group. Probing
   the pid first does not close that: a reused pid is live, so the probe passes
   and the signal lands on the wrong group. *)
let terminate ~mono ?(grace = default_grace) child =
  Eio.Cancel.protect @@ fun () ->
  Eio.Process.signal child Sys.sigterm;
  match
    Eio.Time.Timeout.run (Eio.Time.Timeout.seconds mono grace) (fun () ->
        Ok (Eio.Process.await child))
  with
  | Ok _ -> ()
  | Error `Timeout ->
      Eio.Process.signal child Sys.sigkill;
      ignore (Eio.Process.await child : Eio.Process.exit_status)

(* A drain reaches [`Eof] on clean end-of-stream, [`Exceeded] when a [Limit]
   policy overflows, or [`Failed] on an unexpected read failure. *)
let drain source state =
  let buffer = Cstruct.create 8192 in
  let rec loop () =
    match Eio.Flow.single_read source buffer with
    | n -> (
        match
          Capture.add state (Cstruct.to_string (Cstruct.sub buffer 0 n))
        with
        | `Ok -> loop ()
        | `Exceeded limit -> `Exceeded limit)
    | exception End_of_file -> `Eof
    | exception Eio.Exn.Io (err, _) -> `Failed err
  in
  loop ()

(* A child closing its stdin surfaces as EPIPE (a reset connection on the
   pipe); it ends the feed normally. Any other write failure, or a failure
   reading the caller's source, is a supervision failure. *)
let is_epipe = function
  | Eio.Exn.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> true
  | Eio.Exn.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EPIPE, _, _)), _) -> true
  | _ -> false

(* The stdin feeder: a manual read/write loop so a source-read failure is
   distinguished from the child closing stdin (EPIPE-only tolerance). *)
let feed source sink =
  let buffer = Cstruct.create 8192 in
  let rec loop () =
    match Eio.Flow.single_read source buffer with
    | exception End_of_file -> `Done
    | exception Eio.Exn.Io (err, _) -> `Failed err
    | n -> (
        match Eio.Flow.write sink [ Cstruct.sub buffer 0 n ] with
        | () -> loop ()
        | exception exn when is_epipe exn -> `Done
        | exception Eio.Exn.Io (err, _) -> `Failed err)
  in
  loop ()

let run ~proc_mgr ~mono ~fs ~cwd ~env ~executable ?stdin ~capture ~timeout
    ~cancelled ?(grace = default_grace) ?(drain_grace = default_drain_grace)
    ?(poll = default_poll) argv =
  let start = Eio.Time.Mono.now mono in
  let stdout_state = Capture.create capture in
  let stderr_state = Capture.create capture in
  (* Per-stream completeness: a stream is complete only when its drain reached
     clean EOF, so an exceeded limit, a supervision failure, or a drain still
     reading past the grace all leave it truncated. *)
  let stdout_complete = ref false and stderr_complete = ref false in
  let termination =
    Eio.Switch.run @@ fun sw ->
    let out_r, out_w = launch (fun () -> Eio.Process.pipe ~sw proc_mgr) in
    let err_r, err_w = launch (fun () -> Eio.Process.pipe ~sw proc_mgr) in
    let spawn_child stdin =
      launch (fun () ->
          Eio.Process.spawn ~sw proc_mgr ~cwd ~stdin ~stdout:out_w ~stderr:err_w
            ~env ~executable argv)
    in
    (* The single linearization point: the first [settle] wins, every later
       cause is a lost race. Natural completion carries the exit status; every
       other cause is preemptive and terminates the child. *)
    let result, resolve_result = Eio.Promise.create () in
    let settle cause =
      ignore (Eio.Promise.try_resolve resolve_result cause : bool)
    in
    let out_done, resolve_out_done = Eio.Promise.create () in
    let err_done, resolve_err_done = Eio.Promise.create () in
    (* Omitted stdin is an explicit /dev/null; a caller source is fed through
       an explicit pipe by a daemon fiber that tolerates the child closing
       stdin (EPIPE) and surfaces any other failure as a supervision failure.
       The parent's copies of every child-held end are closed right after
       spawn, so the drains' EOF and the feeder's EPIPE depend on the child
       holding the last ends. *)
    let child =
      match stdin with
      | None ->
          let null =
            launch (fun () ->
                Eio.Path.open_in ~sw (Eio.Path.( / ) fs "/dev/null"))
          in
          let child = spawn_child (null :> Eio.Flow.source_ty Eio.Std.r) in
          Eio.Flow.close null;
          child
      | Some source ->
          let stdin_r, stdin_w =
            launch (fun () -> Eio.Process.pipe ~sw proc_mgr)
          in
          let child = spawn_child (stdin_r :> Eio.Flow.source_ty Eio.Std.r) in
          Eio.Flow.close stdin_r;
          Eio.Fiber.fork_daemon ~sw (fun () ->
              (match feed source stdin_w with
              | `Done -> ()
              | `Failed err -> settle (Supervision_failed err));
              Eio.Flow.close stdin_w;
              `Stop_daemon);
          child
    in
    Eio.Flow.close out_w;
    Eio.Flow.close err_w;
    (* Drains are daemons so an orphaned descendant holding a pipe open cannot
       block switch completion; each records completeness and resolves its
       done-promise once. An excess settles [Output_limit] before it resolves
       done, so a pending excess in the pipe is always observed before natural
       completion can finalize [Exited]: output-limit before exit. *)
    let drain_stream source state complete stream resolve_done =
      Eio.Fiber.fork_daemon ~sw (fun () ->
          (match drain source state with
          | `Eof -> complete := true
          | `Exceeded limit -> settle (Output_limit { stream; limit })
          | `Failed err -> settle (Supervision_failed err));
          Eio.Promise.resolve resolve_done ();
          `Stop_daemon)
    in
    drain_stream out_r stdout_state stdout_complete `Stdout resolve_out_done;
    drain_stream err_r stderr_state stderr_complete `Stderr resolve_err_done;
    (* The waiter reaps the child on natural exit and records the status; it
       never settles directly, because natural completion also needs both
       drains to finish. *)
    let exited, resolve_exited = Eio.Promise.create () in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Promise.resolve resolve_exited (Eio.Process.await child);
        `Stop_daemon);
    let settle_grace () =
      match
        Eio.Time.Timeout.run (Eio.Time.Timeout.seconds mono drain_grace)
          (fun () ->
            Eio.Promise.await out_done;
            Eio.Promise.await err_done;
            Ok ())
      with
      | Ok () | Error `Timeout -> ()
    in
    (* Natural completion: the child exited and both drains finished. A bounded
       grace covers a surviving descendant holding a pipe — after it the run is
       [Exited] with the still-open streams reported not complete, never
       silently short. A pending excess settles [Output_limit] first, so a
       fast-exiting over-limit child is deterministically [Output_limit]. *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
        let status = Eio.Promise.await exited in
        settle_grace ();
        settle (Exited status);
        `Stop_daemon);
    (match cancelled with
    | None -> ()
    | Some stop ->
        Eio.Fiber.fork_daemon ~sw (fun () ->
            let rec poll_loop () =
              if stop () then settle Stopped
              else begin
                Eio.Time.Mono.sleep mono poll;
                poll_loop ()
              end
            in
            poll_loop ();
            `Stop_daemon));
    (* The timeout races the whole run, not just child exit, so a descendant
       that keeps a pipe open past the grace still yields a bounded
       [Timed_out]. *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
        (match
           Eio.Time.Timeout.run timeout (fun () ->
               Ok (Eio.Promise.await result))
         with
        | Ok _ -> ()
        | Error `Timeout -> settle Timed_out);
        `Stop_daemon);
    let termination = Eio.Promise.await result in
    (match termination with
    | Exited _ -> ()
    | Timed_out | Stopped | Output_limit _ | Supervision_failed _ ->
        terminate ~mono ~grace child;
        (* The child is now dead; give the drains the same bounded grace to
           finish reading what is buffered before returning. *)
        settle_grace ());
    termination
  in
  let duration = Mtime.span start (Eio.Time.Mono.now mono) in
  {
    termination;
    stdout = Capture.parts stdout_state;
    stdout_complete = !stdout_complete;
    stderr = Capture.parts stderr_state;
    stderr_complete = !stderr_complete;
    duration;
  }
