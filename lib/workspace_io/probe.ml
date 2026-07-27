(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let probe_timeout = 1.0

let unavailable reason =
  Mentat_sandbox.Error.Unavailable ("Linux Bubblewrap unavailable: " ^ reason)

(* WSL1 lacks the user namespaces Bubblewrap needs; WSL2 has them. *)
let is_wsl1 () =
  match In_channel.with_open_bin "/proc/version" In_channel.input_all with
  | version ->
      let version = String.lowercase_ascii version in
      String.includes ~affix:"microsoft" version
      && not (String.includes ~affix:"wsl2" version)
  | exception Sys_error _ -> false

(* The probe exercises every operation a lowered policy can emit, not just the
   binds. [--perms] arrived in bubblewrap 0.5.0, and a probe that omits it
   passes on an older one and then fails every command in the session — the
   failure lands on the first thing the agent runs rather than at startup, where
   the resolver could have reported an unusable backend. The denial trio is
   applied to [/tmp] because it exists on every host and the probe's own
   [/bin/true] has no use for it. *)
let probe_argv executable =
  [
    executable;
    "--unshare-user";
    "--unshare-pid";
    "--ro-bind";
    "/";
    "/";
    "--perms";
    "000";
    "--tmpfs";
    "/tmp";
    "--remount-ro";
    "/tmp";
    "--dev";
    "/dev";
    "--proc";
    "/proc";
    "--";
    "/bin/true";
  ]

let status_text = function
  | `Exited code -> Printf.sprintf "exited %d" code
  | `Signaled signal -> Printf.sprintf "signaled %d" signal

(* The child belongs to the inner switch: timeout or caller cancellation
   releases it, which kills and reaps the child before the exception can
   leave [Switch.run]. Cancellation re-raises — it is not a failed probe. *)
let run_probe ~stdenv ~executable =
  let clock = Eio.Stdenv.clock stdenv in
  let process_mgr = Eio.Stdenv.process_mgr stdenv in
  let null_path = Eio.Path.( / ) (Eio.Stdenv.fs stdenv) "/dev/null" in
  try
    match
      Eio.Time.with_timeout clock probe_timeout (fun () ->
          Ok
            (Eio.Path.with_open_out ~create:`Never null_path (fun null ->
                 Eio.Switch.run (fun sw ->
                     let process =
                       Eio.Process.spawn ~sw process_mgr ~stdin:null
                         ~stdout:null ~stderr:null (probe_argv executable)
                     in
                     Eio.Process.await process))))
    with
    | Ok (`Exited 0) -> Ok ()
    | Ok status -> Error (status_text status)
    | Error `Timeout ->
        Error (Printf.sprintf "timed out after %gs" probe_timeout)
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (Printexc.to_string exn)

let bubblewrap ~stdenv executable =
  if is_wsl1 () then Error (unavailable "WSL1 is not supported")
  else if not (Derive.is_executable_file executable) then
    Error (unavailable (executable ^ " is not an executable file"))
  else
    match run_probe ~stdenv ~executable with
    | Ok () -> Ok Mentat_sandbox.Backend.Bubblewrap
    | Error reason -> Error (unavailable ("probe failed: " ^ reason))

let seatbelt () =
  let executable =
    Mentat_sandbox.Backend.executable Mentat_sandbox.Backend.Seatbelt
  in
  if Derive.is_executable_file executable then
    Ok Mentat_sandbox.Backend.Seatbelt
  else
    Error
      (Mentat_sandbox.Error.Unavailable
         (Printf.sprintf
            "macOS Seatbelt unavailable: %s is not an executable file"
            executable))

(* Backend selection is platform-owned. The environment seams force only
   deterministic availability behavior for black-box tests. *)
let backend ~stdenv ~lookup =
  match lookup "_MENTAT_TEST_SANDBOX_UNAVAILABLE" with
  | Some "1" ->
      Error
        (Mentat_sandbox.Error.Unavailable
           "sandbox backend forced unavailable \
            (_MENTAT_TEST_SANDBOX_UNAVAILABLE=1)")
  | Some other ->
      Error
        (Mentat_sandbox.Error.Unavailable
           (Printf.sprintf "unknown _MENTAT_TEST_SANDBOX_UNAVAILABLE value %S"
              other))
  | None -> (
      match lookup "_MENTAT_TEST_BUBBLEWRAP_PROBE" with
      | Some executable -> bubblewrap ~stdenv executable
      | None when Derive.is_linux () ->
          bubblewrap ~stdenv
            (Mentat_sandbox.Backend.executable Mentat_sandbox.Backend.Bubblewrap)
      | None when Derive.is_executable_file "/usr/bin/sandbox-exec" ->
          seatbelt ()
      | None ->
          Error
            (Mentat_sandbox.Error.Unavailable
               "no supported sandbox backend on this platform"))
