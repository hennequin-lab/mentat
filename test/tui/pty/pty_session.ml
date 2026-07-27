(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Project = Tui_harness.Project
module Screen = Tui_harness.Screen

type t = {
  pty : Pty.t;
  vte : Vte.t;
  raw : Buffer.t;
  buffer : Bytes.t;
  project : Project.t;
  mutable sync_active : bool;
  mutable sync_tail : string;
  mutable probe_tail : string;
  mutable exited : bool;
  mutable last_nonblank : string option;
  (* Frame-commit observability (G1): the child brackets each frame in
     begin/end-synchronized-update, so an end-marker (?2026l closing an open
     frame) is a committed frame. [frames] counts them; [first_frame_at] and
     [last_frame_at] hold the monotonic instant each was observed. The stamps
     are read by frame-cadence measurements; nothing in the synchronizing
     predicates depends on them, so existing suites are unaffected. *)
  mutable frames : int;
  mutable first_frame_at : float option;
  mutable last_frame_at : float option;
}

let deadline = 10.
let monotonic () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) *. 1e-9

(* The select pump's poll interval. Tightened from the historical 20 ms so a
   frame-commit stamp lands within a few ms of the actual ?2026l transition —
   the resolution a 7 ms frame budget needs. Quiescence detection ({!settle}) is
   kept time-based so its window does not shrink with this interval. *)
let pump_interval = 0.005

(* The window of continuous frame-boundary stability that confirms the render
   has quiesced — held at the historical 3 × 20 ms so screen goldens captured
   after {!settle} are unchanged by the tighter pump. *)
let quiescence_window = 0.06

let nonblank screen =
  let rec loop index =
    index < String.length screen
    &&
    match String.unsafe_get screen index with
    | ' ' | '\n' -> loop (index + 1)
    | _ -> true
  in
  loop 0

let screen t =
  let current = Vte.to_string t.vte in
  if nonblank current then current
  else Option.value t.last_nonblank ~default:current

let exited t = t.exited

(* Frame-commit observability (G1/G2). [frames] is the count of committed frames
   the child has bracketed so far; [first_frame_at] is the monotonic instant of
   the first — the launch→first-frame boot span is [first_frame_at t] minus the
   spawn instant a caller records before {!run} — and [last_frame_at] the most
   recent, so a caller measures frame cadence and input-to-frame latency off
   these stamps. [reset_frames] rezeros the run so a measurement window starts
   from a known point after warm-up. *)
let frames t = t.frames
let first_frame_at t = t.first_frame_at
let last_frame_at t = t.last_frame_at

let reset_frames t =
  t.frames <- 0;
  t.first_frame_at <- None;
  t.last_frame_at <- None

(* The complete undecoded byte stream the child has written so far. Stream-only
   terminal effects the VTE consumes into cursor and screen state - the OSC
   window title in particular - never reach {!screen}, so a test observes them
   here. *)
let raw t = Buffer.contents t.raw

let update_sync_state ~sync_active ~tail delta =
  let enable = "\027[?2026h" in
  let disable = "\027[?2026l" in
  let delta = tail ^ delta in
  let length = String.length delta in
  let starts_with_at index needle =
    let needle_length = String.length needle in
    index + needle_length <= length
    && String.equal (String.sub delta index needle_length) needle
  in
  (* [commits] counts active→inactive transitions — each an end-of-frame — so a
     malformed lone disable while already inactive is not miscounted. A marker
     split across two deltas is completed (and counted) only in the delta that
     carries its final byte, since [tail] keeps at most [enable]-1 bytes and an
     incomplete marker never matches. *)
  let rec loop index active commits =
    if index >= length then (active, commits)
    else if starts_with_at index enable then
      loop (index + String.length enable) true commits
    else if starts_with_at index disable then
      loop
        (index + String.length disable)
        false
        (if active then commits + 1 else commits)
    else loop (index + 1) active commits
  in
  let active, commits = loop 0 sync_active 0 in
  let tail_length = min (String.length enable - 1) length in
  let tail = String.sub delta (length - tail_length) tail_length in
  (active, tail, commits)

let send t text =
  let length = String.length text in
  let rec loop offset =
    if offset < length then
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
  in
  loop 0

(* A real modern terminal answers the capability probe the child emits at
   startup. The child's Matrix caps handshake releases as soon as the Primary
   Device Attributes reply arrives, rather than blocking on its full timeout
   before the first frame, and — told the terminal supports synchronized output
   (DECRQM 2026) — it brackets each frame with begin/end-synchronized-update so
   {!settle} can key on frame commits instead of a stability window. We reply
   exactly as such a terminal would; the DA1 reply is child input and never
   enters {!raw}, while the 2026 support reply makes the child emit the bracket
   bytes that {!update_sync_state} already tracks. *)
let probe_replies =
  [ ("\027[c", "\027[?62;22c"); ("\027[?2026$p", "\027[?2026;1$y") ]

let max_query_length =
  List.fold_left (fun acc (q, _) -> max acc (String.length q)) 0 probe_replies

let answer_probe t ~tail delta =
  let combined = tail ^ delta in
  let length = String.length combined in
  let starts_with_at index needle =
    let needle_length = String.length needle in
    index + needle_length <= length
    && String.equal (String.sub combined index needle_length) needle
  in
  let rec loop index =
    if index >= length then ()
    else
      match
        List.find_opt (fun (q, _) -> starts_with_at index q) probe_replies
      with
      | Some (query, reply) ->
          send t reply;
          loop (index + String.length query)
      | None -> loop (index + 1)
  in
  loop 0;
  let tail_length = min (max_query_length - 1) length in
  String.sub combined (length - tail_length) tail_length

let pump t ~wait_s =
  if not t.exited then
    let readable =
      match Unix.select [ Pty.file_descr t.pty ] [] [] wait_s with
      | descriptors, _, _ -> descriptors <> []
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> false
    in
    if readable then
      match Pty.read t.pty t.buffer 0 (Bytes.length t.buffer) with
      | 0 -> t.exited <- true
      | count ->
          let delta = Bytes.sub_string t.buffer 0 count in
          Buffer.add_string t.raw delta;
          Vte.feed t.vte t.buffer 0 count;
          let sync_active, sync_tail, commits =
            update_sync_state ~sync_active:t.sync_active ~tail:t.sync_tail delta
          in
          t.sync_active <- sync_active;
          t.sync_tail <- sync_tail;
          if commits > 0 then begin
            let at = monotonic () in
            t.frames <- t.frames + commits;
            if Option.is_none t.first_frame_at then t.first_frame_at <- Some at;
            t.last_frame_at <- Some at
          end;
          t.probe_tail <- answer_probe t ~tail:t.probe_tail delta;
          if not sync_active then
            let current = Vte.to_string t.vte in
            if nonblank current then t.last_nonblank <- Some current
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
      | exception Unix.Unix_error _ -> t.exited <- true

let fail_with_screen t message =
  Printf.printf "--- %s; last screen ---\n" message;
  Screen.print ~project:t.project (screen t);
  failwith message

let wait t predicate =
  let limit = monotonic () +. deadline in
  let rec loop () =
    if (not t.sync_active) && predicate (screen t) then ()
    else if t.exited then
      fail_with_screen t "PTY child exited before its ready frame"
    else if monotonic () >= limit then
      fail_with_screen t "PTY ready frame timed out"
    else begin
      pump t ~wait_s:pump_interval;
      loop ()
    end
  in
  loop ()

(* Like {!wait}, but keys on the append-only byte stream instead of the frame
   grid. A continuously animating screen (the home's brand shimmer) may never
   present a [not sync_active] grid the pump samples, yet the text it renders is
   still in {!raw} verbatim — so a caller that only needs to observe that some
   content was emitted watches here and sidesteps the frame-boundary gate. *)
let wait_raw t predicate =
  let limit = monotonic () +. deadline in
  let rec loop () =
    if predicate (raw t) then ()
    else if t.exited then
      fail_with_screen t "PTY child exited before its ready bytes"
    else if monotonic () >= limit then
      fail_with_screen t "PTY ready bytes timed out"
    else begin
      pump t ~wait_s:pump_interval;
      loop ()
    end
  in
  loop ()

(* The child advertises synchronized output (answered in {!answer_probe}), so it
   brackets each frame in begin/end-synchronized-update. A checkpoint is a frame
   boundary that holds: [not sync_active] rejects any mid-frame state outright,
   and a [quiescence_window] of no new bytes — hence no new begin-sync opening the
   next frame — confirms the render has quiesced. This keys on frame commits, not
   a guessed render duration. The window is a wall duration, not a poll count, so
   the tighter {!pump_interval} raises stamp resolution without shrinking it. *)
let settle t =
  let limit = monotonic () +. deadline in
  let rec loop previous_raw stable_since =
    if t.exited then ()
    else if monotonic () >= limit then
      fail_with_screen t "PTY frame did not reach quiescence"
    else begin
      pump t ~wait_s:pump_interval;
      let raw_length = Buffer.length t.raw in
      let at_frame_boundary =
        (not t.sync_active) && Int.equal raw_length previous_raw
      in
      let stable_since =
        if at_frame_boundary then stable_since else monotonic ()
      in
      if monotonic () -. stable_since < quiescence_window then
        loop raw_length stable_since
    end
  in
  loop (Buffer.length t.raw) (monotonic ())

let wait_exit t =
  let limit = monotonic () +. deadline in
  while (not t.exited) && monotonic () < limit do
    pump t ~wait_s:pump_interval
  done;
  if not t.exited then fail_with_screen t "PTY child did not exit"

let resize t ~rows ~cols =
  Pty.resize t.pty ~rows ~cols;
  Vte.resize t.vte ~rows ~cols

let quit ?(discard_draft = false) t =
  if discard_draft then begin
    (* Ctrl+C intentionally discards a non-empty composer before it can arm
       quit. Observe that public transition so launch-draft journeys do not
       have to mutate their fixture merely to tear the process down. *)
    send t "\003";
    wait t (Screen.has "❯ message mentat")
  end;
  send t "\003";
  wait t (Screen.has "press ctrl+c again to quit");
  send t "\003";
  wait_exit t

let seed_trust ~environment project =
  let executable = Project.resolve_env_path "MENTAT_BIN" in
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let arguments = [| executable; "trust"; Project.root project |] in
  let pid =
    Unix.create_process_env executable arguments environment null null null
  in
  Unix.close null;
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code ->
      failwith (Printf.sprintf "trust seed exited with status %d" code)
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
      failwith (Printf.sprintf "trust seed stopped by signal %d" signal)

let home_ready screen =
  Screen.contains screen "❯ message mentat"
  && Screen.contains screen "! /login — no connected account"
  && Screen.contains screen "no recent sessions"
  && Screen.contains screen "! not logged in · /login"

let run ?(trust = true) ?(env = []) ?(unset = []) ?(command = []) ?(rows = 24)
    ?(cols = 80) ?(ready = home_ready) ?ready_raw project f =
  let executable = Project.resolve_env_path "MENTAT_BIN" in
  let environment = Project.env_array ~extra:env ~unset project in
  if trust then seed_trust ~environment project;
  let winsize = Pty.{ rows; cols; xpixel = 0; ypixel = 0 } in
  let pty =
    Pty.spawn ~cwd:(Project.root project) ~env:environment ~winsize
      ~prog:executable
      ~args:(command @ [ "--cwd"; Project.root project ])
      ()
  in
  Pty.set_nonblock pty;
  let session =
    {
      pty;
      vte = Vte.create ~rows ~cols ();
      raw = Buffer.create 4096;
      buffer = Bytes.create 4096;
      project;
      sync_active = false;
      sync_tail = "";
      probe_tail = "";
      exited = false;
      last_nonblank = None;
      frames = 0;
      first_frame_at = None;
      last_frame_at = None;
    }
  in
  Fun.protect
    ~finally:(fun () -> Pty.close session.pty)
    (fun () ->
      (match ready_raw with
      | Some predicate -> wait_raw session predicate
      | None -> wait session ready);
      f session)
