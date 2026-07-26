(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Subprocess supervision runs real, bounded children (sh, sleep, cat, printf).
   Every child is bounded — a limit, a timeout, or a cooperative stop — so a
   broken run fails fast rather than hanging. The pure capture policies are
   exercised directly, without a process, against their exact boundaries. *)

open Windtrap
module Capture = Subprocess.Capture

(* Pure capture policies. *)

let capture_all () =
  let c = Capture.create Capture.All in
  is_true ~msg:"empty add is Ok" (Capture.add c "" = `Ok);
  is_true ~msg:"first add is Ok" (Capture.add c "hello" = `Ok);
  is_true ~msg:"second add is Ok" (Capture.add c " world" = `Ok);
  match Capture.parts c with
  | Capture.Whole s -> equal string ~msg:"All keeps every byte" "hello world" s
  | Capture.Split _ -> fail "All is a Whole"

let capture_limit_boundary () =
  let c = Capture.create (Capture.Limit 5) in
  is_true ~msg:"within the bound is Ok" (Capture.add c "abc" = `Ok);
  is_true ~msg:"the first excess reports the exact bound"
    (Capture.add c "defg" = `Exceeded 5);
  (match Capture.parts c with
  | Capture.Whole s ->
      equal string ~msg:"exactly the bound is kept, the excess dropped" "abcde"
        s
  | Capture.Split _ -> fail "Limit is a Whole");
  (* An add that exactly fills the bound is still Ok; the next byte exceeds. *)
  let exact = Capture.create (Capture.Limit 3) in
  is_true ~msg:"an add that exactly fills the bound is Ok"
    (Capture.add exact "abc" = `Ok);
  is_true ~msg:"one byte past the full bound exceeds"
    (Capture.add exact "d" = `Exceeded 3)

let capture_head_tail () =
  let split ~head ~tail chunks =
    let c = Capture.create (Capture.Head_tail { head; tail }) in
    List.iter
      (fun s -> ignore (Capture.add c s : [ `Ok | `Exceeded of int ]))
      chunks;
    match Capture.parts c with
    | Capture.Split { head; omitted; tail } -> (head, omitted, tail)
    | Capture.Whole _ -> fail "Head_tail is a Split"
  in
  equal (triple string int string)
    ~msg:"under head+tail passes through with nothing omitted" ("abcd", 0, "fgh")
    (split ~head:4 ~tail:3 [ "abcd"; "fgh" ]);
  equal (triple string int string)
    ~msg:"one byte over elides exactly one byte from the middle"
    ("abcd", 1, "fgh")
    (split ~head:4 ~tail:3 [ "abcdefgh" ]);
  equal (triple string int string)
    ~msg:"a zero tail drops and counts the whole overflow" ("ab", 4, "")
    (split ~head:2 ~tail:0 [ "abcdef" ])

let capture_rejects_negative () =
  raises_invalid_arg "capture limit must be non-negative" (fun () ->
      Capture.create (Capture.Limit (-1)));
  raises_invalid_arg "capture head and tail must be non-negative" (fun () ->
      Capture.create (Capture.Head_tail { head = -1; tail = 0 }))

(* Supervised real children. *)

let run_child env ?stdin ?(capture = Capture.All)
    ?(timeout = Eio.Time.Timeout.none) ?cancelled ~executable argv =
  Subprocess.run
    ~proc_mgr:(Eio.Stdenv.process_mgr env)
    ~mono:(Eio.Stdenv.mono_clock env)
    ~fs:(Eio.Stdenv.fs env) ~cwd:(Eio.Stdenv.cwd env) ~env:(Unix.environment ())
    ~executable ?stdin ~capture ~timeout ~cancelled argv

let exited outcome =
  match outcome.Subprocess.termination with
  | Subprocess.Exited (`Exited code) -> code
  | Subprocess.Exited (`Signaled signal) ->
      failf "signaled %d, not exited" signal
  | _ -> fail "expected an ordinary exit"

let whole = function
  | Capture.Whole s -> s
  | Capture.Split _ -> fail "expected a whole capture"

let exit_codes_pass_through () =
  Eio_main.run @@ fun env ->
  equal int ~msg:"a zero exit is preserved" 0
    (exited (run_child env ~executable:"/bin/sh" [ "/bin/sh"; "-c"; "exit 0" ]));
  equal int ~msg:"a non-zero exit code is preserved" 3
    (exited (run_child env ~executable:"/bin/sh" [ "/bin/sh"; "-c"; "exit 3" ]))

let both_streams_captured () =
  Eio_main.run @@ fun env ->
  let outcome =
    run_child env ~executable:"/bin/sh"
      [ "/bin/sh"; "-c"; "printf out; printf err >&2" ]
  in
  equal int ~msg:"the child exits cleanly" 0 (exited outcome);
  equal string ~msg:"stdout is captured whole" "out"
    (whole outcome.Subprocess.stdout);
  equal string ~msg:"stderr is captured whole" "err"
    (whole outcome.Subprocess.stderr);
  is_true ~msg:"a fully-drained stream reads complete"
    (outcome.Subprocess.stdout_complete && outcome.Subprocess.stderr_complete)

(* A child that writes far past the bound and exits at once must still be an
   Output_limit, never a silently truncated Exited: the pending excess is
   observed before the concurrent exit is finalized. *)
let output_limit_beats_a_fast_exit () =
  Eio_main.run @@ fun env ->
  let payload = String.make 4096 'x' in
  let outcome =
    run_child env ~capture:(Capture.Limit 1000) ~executable:"/usr/bin/printf"
      [ "/usr/bin/printf"; "%s"; payload ]
  in
  (match outcome.Subprocess.termination with
  | Subprocess.Output_limit { stream = `Stdout; limit } ->
      equal int ~msg:"the termination names the configured bound" 1000 limit
  | _ -> fail "an over-limit fast exit must terminate as Output_limit");
  equal int ~msg:"exactly the bound is retained" 1000
    (String.length (whole outcome.Subprocess.stdout));
  is_true ~msg:"a limited stream is not complete"
    (not outcome.Subprocess.stdout_complete)

let timeout_kills_the_leader () =
  Eio_main.run @@ fun env ->
  let timeout = Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 0.15 in
  (* [exec sleep] makes the leader itself hold stdout, so the partial output
     drains cleanly after the kill. *)
  let outcome =
    run_child env ~capture:Capture.All ~timeout ~executable:"/bin/sh"
      [ "/bin/sh"; "-c"; "printf partial; exec sleep 5" ]
  in
  (match outcome.Subprocess.termination with
  | Subprocess.Timed_out -> ()
  | _ -> fail "a sleeping child must time out");
  is_true ~msg:"the measured duration is at least the budget"
    (Mtime.Span.to_float_ns outcome.Subprocess.duration >= 0.149e9);
  equal string ~msg:"the partial output before the timeout is preserved"
    "partial"
    (whole outcome.Subprocess.stdout)

let cooperative_stop_yields_stopped () =
  Eio_main.run @@ fun env ->
  let outcome =
    run_child env
      ~cancelled:(fun () -> true)
      ~executable:"/bin/sleep" [ "/bin/sleep"; "5" ]
  in
  match outcome.Subprocess.termination with
  | Subprocess.Stopped -> ()
  | _ -> fail "a cooperative stop must terminate as Stopped"

(* A child that closes stdin without reading surfaces as EPIPE to the feeder,
   which ends the feed normally — never a supervision failure. *)
let stdin_epipe_is_tolerated () =
  Eio_main.run @@ fun env ->
  let stdin =
    (Eio.Flow.string_source (String.make 100_000 'z')
      :> Eio.Flow.source_ty Eio.Std.r)
  in
  let outcome =
    run_child env ~stdin ~executable:"/bin/sh" [ "/bin/sh"; "-c"; "exit 0" ]
  in
  match outcome.Subprocess.termination with
  | Subprocess.Exited (`Exited 0) -> ()
  | Subprocess.Supervision_failed _ ->
      fail "a child closing stdin is EPIPE, not a supervision failure"
  | _ -> fail "expected a clean exit despite the unread stdin"

let stdin_feeds_the_child () =
  Eio_main.run @@ fun env ->
  let stdin =
    (Eio.Flow.string_source "fed bytes" :> Eio.Flow.source_ty Eio.Std.r)
  in
  let outcome = run_child env ~stdin ~executable:"/bin/cat" [ "/bin/cat" ] in
  equal int ~msg:"cat exits cleanly" 0 (exited outcome);
  equal string ~msg:"the caller's source is fed to the child" "fed bytes"
    (whole outcome.Subprocess.stdout)

(* A child that cannot run the program reports why across the fork, and the
   reason survives as a process error naming the executable rather than as a
   bare errno the caller would have to re-derive. *)
let execve_failures_are_classified () =
  Eio_main.run @@ fun env ->
  let spawn_error path =
    match run_child env ~executable:path [ path ] with
    | _ -> fail "expected the launch to fail"
    | exception Subprocess.Launch (Eio.Exn.Io (Eio.Process.E error, _)) -> error
    | exception Subprocess.Launch exn ->
        failf "expected a process error, got %s" (Printexc.to_string exn)
  in
  let dir = Filename.get_temp_dir_name () in
  let unrunnable = Filename.concat dir "mentat-subprocess-not-executable" in
  let out = open_out unrunnable in
  output_string out "not a program\n";
  close_out out;
  Unix.chmod unrunnable 0o644;
  Fun.protect
    ~finally:(fun () ->
      try Unix.unlink unrunnable with Unix.Unix_error _ -> ())
    (fun () ->
      (match spawn_error "/nonexistent/program" with
      | Eio.Process.Executable_not_found program ->
          equal string ~msg:"a missing executable names itself"
            "/nonexistent/program" program
      | error ->
          failf "expected Executable_not_found, got %a" Eio.Exn.pp
            (Eio.Process.err error));
      match spawn_error unrunnable with
      | Eio.Process.Permission_denied program ->
          equal string ~msg:"an unrunnable file names itself" unrunnable program
      | error ->
          failf "expected Permission_denied, got %a" Eio.Exn.pp
            (Eio.Process.err error))

let launch_failure_escapes () =
  Eio_main.run @@ fun env ->
  raises_match
    (function Subprocess.Launch _ -> true | _ -> false)
    (fun () ->
      run_child env ~executable:"/nonexistent/program"
        [ "/nonexistent/program" ])

let () =
  run "subprocess"
    [
      test "capture: All keeps every byte" capture_all;
      test "capture: Limit reports the first excess" capture_limit_boundary;
      test "capture: Head_tail elision boundary" capture_head_tail;
      test "capture: negative bounds raise" capture_rejects_negative;
      test "execve failures are classified" execve_failures_are_classified;
      test "exit codes pass through" exit_codes_pass_through;
      test "both streams are captured" both_streams_captured;
      test "output limit beats a fast exit" output_limit_beats_a_fast_exit;
      test "a timeout kills the leader" timeout_kills_the_leader;
      test "a cooperative stop yields Stopped" cooperative_stop_yields_stopped;
      test "stdin EPIPE is tolerated" stdin_epipe_is_tolerated;
      test "a caller source feeds the child" stdin_feeds_the_child;
      test "a launch failure escapes as Launch" launch_failure_escapes;
    ]
