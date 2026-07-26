(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = {
  pty : Pty.t;
  pid : int;
  raw : Buffer.t;
  buffer : Bytes.t;
  mutable exited : bool;
  mutable closed : bool;
  mutable status : Unix.process_status option;
}

type forced_cleanup = {
  sigterm_sent : bool;
  sigkill_sent : bool;
  child_reaped : bool;
  resources_closed : bool;
}

let deadline_s = 10.
let quiet_s = 0.25
let terminate_grace_s = 0.1
let monotonic () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) *. 1e-9

let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec loop index =
    index + substring_length <= text_length
    && (String.equal (String.sub text index substring_length) substring
       || loop (index + 1))
  in
  substring_length = 0 || loop 0

let raw t = Buffer.contents t.raw

let escaped_tail t =
  let contents = raw t in
  let length = String.length contents in
  let retained = min length 2_048 in
  String.sub contents (length - retained) retained |> String.escaped

let fail t message =
  failwith (Printf.sprintf "%s; raw output tail: %s" message (escaped_tail t))

let pump t ~wait_s =
  if not t.exited then begin
    let readable =
      match Unix.select [ Pty.file_descr t.pty ] [] [] wait_s with
      | descriptors, _, _ -> descriptors <> []
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> false
    in
    if readable then
      match Pty.read t.pty t.buffer 0 (Bytes.length t.buffer) with
      | 0 -> t.exited <- true
      | count -> Buffer.add_subbytes t.raw t.buffer 0 count
      | (exception Unix.Unix_error (Unix.EAGAIN, _, _))
      | (exception Unix.Unix_error (Unix.EWOULDBLOCK, _, _))
      | (exception Unix.Unix_error (Unix.EINTR, _, _)) ->
          ()
      | exception Unix.Unix_error (Unix.EIO, _, _) -> t.exited <- true
  end

let wait_raw t predicate =
  let limit = monotonic () +. deadline_s in
  let rec loop () =
    if predicate (raw t) then ()
    else if t.exited then fail t "terminal child exited before output arrived"
    else if monotonic () >= limit then fail t "terminal output timed out"
    else begin
      pump t ~wait_s:0.02;
      loop ()
    end
  in
  loop ()

let wait_quiet t =
  let limit = monotonic () +. deadline_s in
  let rec loop last_length quiet_since =
    if t.exited then fail t "terminal child exited before becoming quiet"
    else if monotonic () >= limit then
      fail t "terminal output did not become quiet"
    else begin
      pump t ~wait_s:0.02;
      let now = monotonic () in
      let length = Buffer.length t.raw in
      if length <> last_length then loop length now
      else if now -. quiet_since >= quiet_s then ()
      else loop last_length quiet_since
    end
  in
  loop (Buffer.length t.raw) (monotonic ())

let send t text =
  let length = String.length text in
  let limit = monotonic () +. deadline_s in
  let rec loop offset =
    if offset < length then begin
      if monotonic () >= limit then fail t "terminal input timed out";
      match Pty.write_string t.pty text offset (length - offset) with
      | 0 ->
          Unix.sleepf 0.001;
          loop offset
      | count -> loop (offset + count)
      | (exception Unix.Unix_error (Unix.EAGAIN, _, _))
      | (exception Unix.Unix_error (Unix.EWOULDBLOCK, _, _))
      | (exception Unix.Unix_error (Unix.EINTR, _, _)) ->
          Unix.sleepf 0.001;
          loop offset
    end
  in
  loop 0

let rec reap_blocking pid =
  match Unix.waitpid [] pid with
  | _, status -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap_blocking pid

let rec reap_nonblocking pid =
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | 0, _ -> None
  | _, status -> Some status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap_nonblocking pid

let rec reap_before pid limit =
  match reap_nonblocking pid with
  | Some status -> Some status
  | None when monotonic () >= limit -> None
  | None ->
      let remaining = limit -. monotonic () in
      ignore (Unix.select [] [] [] (Float.min 0.01 (Float.max 0. remaining)));
      reap_before pid limit

let send_signal pid signal =
  match Unix.kill pid signal with
  | () -> true
  | exception Unix.Unix_error _ -> false

let close_resources t =
  (* [Pty.close] intentionally hides its signal and waitpid results. Fallback
     cleanup owns those operations above so the host contract can observe them;
     close only the master descriptor here to avoid signalling a reaped, reused
     pid through Pty's still-populated private handle. *)
  match Unix.close (Pty.file_descr t.pty) with
  | () -> true
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> true
  | exception Unix.Unix_error _ -> false

let force_cleanup t =
  let sigterm_sent = send_signal t.pid Sys.sigterm in
  let graceful_status = reap_before t.pid (monotonic () +. terminate_grace_s) in
  let sigkill_sent, status =
    match graceful_status with
    | Some status -> (false, Some status)
    | None ->
        let sigkill_sent = send_signal t.pid Sys.sigkill in
        let status =
          if sigkill_sent then Some (reap_blocking t.pid)
          else reap_before t.pid (monotonic () +. deadline_s)
        in
        (sigkill_sent, status)
  in
  let resources_closed = close_resources t in
  t.exited <- Option.is_some status;
  t.closed <- true;
  t.status <- status;
  {
    sigterm_sent;
    sigkill_sent;
    child_reaped = Option.is_some status;
    resources_closed;
  }

let reap_after_close t =
  let limit = monotonic () +. deadline_s in
  let rec loop () =
    match Unix.waitpid [ Unix.WNOHANG ] t.pid with
    | 0, _ when monotonic () < limit ->
        Unix.sleepf 0.01;
        loop ()
    | 0, _ ->
        (try Unix.kill t.pid Sys.sigkill with Unix.Unix_error _ -> ());
        let status = reap_blocking t.pid in
        t.status <- Some status;
        fail t "terminal child did not become reapable"
    | _, status -> t.status <- Some status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let wait_exit t =
  let limit = monotonic () +. deadline_s in
  while (not t.exited) && monotonic () < limit do
    pump t ~wait_s:0.02
  done;
  if not t.exited then fail t "terminal child did not close its PTY";
  (* Close without waiting before reaping ourselves. At EOF the unreaped child
     still owns its pid, so Pty's best-effort SIGTERM cannot target a reused
     process. Taking ownership of waitpid here lets [quit] verify the real exit
     status while the exceptional path below still uses bounded cleanup. *)
  Pty.close ~wait:false t.pty;
  t.closed <- true;
  reap_after_close t

let resize t ~rows ~cols = Pty.resize t.pty ~rows ~cols

let quit t =
  send t "\003";
  wait_raw t (fun output -> contains output "press ctrl+c again to quit");
  send t "\003";
  wait_exit t;
  match t.status with
  | Some (Unix.WEXITED 0) -> ()
  | Some (Unix.WEXITED status) ->
      fail t (Printf.sprintf "terminal child exited with status %d" status)
  | Some (Unix.WSIGNALED signal) ->
      fail t (Printf.sprintf "terminal child was killed by signal %d" signal)
  | Some (Unix.WSTOPPED signal) ->
      fail t (Printf.sprintf "terminal child stopped on signal %d" signal)
  | None -> assert false

let with_spawn ?env ?cwd ?(rows = 24) ?(cols = 80) ?observe_forced_cleanup ~prog
    ~args f =
  let winsize = Pty.{ rows; cols; xpixel = 0; ypixel = 0 } in
  let pty = Pty.spawn ?env ?cwd ~winsize ~prog ~args () in
  (match Pty.set_nonblock pty with
  | () -> ()
  | exception exn ->
      Pty.close pty;
      raise exn);
  let pid =
    match Pty.pid pty with
    | Some pid -> pid
    | None ->
        Pty.close pty;
        invalid_arg "Terminal_process.with_spawn: PTY has no child process"
  in
  let t =
    {
      pty;
      pid;
      raw = Buffer.create 16_384;
      buffer = Bytes.create 4_096;
      exited = false;
      closed = false;
      status = None;
    }
  in
  Fun.protect
    ~finally:(fun () ->
      if not t.closed then begin
        let report = force_cleanup t in
        Option.iter (fun observe -> observe report) observe_forced_cleanup
      end)
    (fun () -> f t)
