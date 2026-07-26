(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open! Cmdliner
module Cfg = Mentat_config
module Sandbox = Mentat_sandbox

let docs = Cli_common.s_diagnostic

let configured_read t =
  Cfg.Resolved.get Cfg.Field.sandbox_read (Composition.config t)

let configured_network t =
  Cfg.Resolved.get Cfg.Field.sandbox_network (Composition.config t)

let json_roots roots =
  Output.Json.list
    (List.map
       (fun (label, path) ->
         Output.Json.obj
           [
             ("label", Output.Json.string label);
             ("path", Output.Json.string (Lpath.Abs.to_string path));
           ])
       roots)

(* F7: the run-start posture block. Configured-posture only (no seal), so a run
   pays nothing to announce what it is about to run under. *)
let print_run_posture t =
  Output.stderr_printf "mentat: sandbox: %s (read %s, network %s)\n"
    (Cfg.Mode.to_string (Composition.configured_sandbox_mode t))
    (Cfg.Read.to_string (configured_read t))
    (Sandbox.Policy.Network.to_string (configured_network t))

(* status. *)

let status json cwd =
  Composition.with_base ~cwd ~overrides:[] (fun t ->
      match
        Composition.resolve_workspace t
          ~mode:(Composition.configured_sandbox_mode t)
          ~network:(configured_network t)
      with
      | Error status -> status
      | Ok cap ->
          let mode =
            Cfg.Mode.to_string (Composition.configured_sandbox_mode t)
          in
          let read = Cfg.Read.to_string (configured_read t) in
          let network =
            Sandbox.Policy.Network.to_string (configured_network t)
          in
          let evidence = Mentat_workspace_io.evidence cap in
          let roots = Mentat_workspace_io.describe_roots cap in
          if json then
            Output.stdout_printf "%s\n"
              (Output.Json.to_string
                 (Output.Json.envelope ~type_:"sandbox.status"
                    [
                      ("mode", Output.Json.string mode);
                      ("read", Output.Json.string read);
                      ("network", Output.Json.string network);
                      ("evidence", Sandbox.Evidence.to_json evidence);
                      ("roots", json_roots roots);
                    ]))
          else (
            Output.stdout_printf "mode=%s\n" mode;
            Output.stdout_printf "read=%s\n" read;
            Output.stdout_printf "network=%s\n" network;
            Output.stdout_printf "evidence=%s\n"
              (Format.asprintf "%a" Sandbox.Evidence.pp evidence);
            (* The enforced profile digest covers the per-run scratch root, so
                it varies by invocation; identity is the scratch-invariant
                fingerprint (sandbox explain). *)
            (match evidence with
            | Sandbox.Evidence.Enforced _ ->
                Output.stdout_printf
                  "note=profile digest is per-invocation; identity is the \
                   stable fingerprint\n"
            | Sandbox.Evidence.Not_requested | Sandbox.Evidence.Refused _
            | Sandbox.Evidence.Declared_external ->
                ());
            List.iter
              (fun (label, path) ->
                Output.stdout_printf "root=%s %s\n" label
                  (Lpath.Abs.to_string path))
              roots);
          Exit_status.Success)

let status_cmd =
  let doc = "Show the effective sandbox posture." in
  Cmd.v
    (Cmd.info "status" ~doc ~docs ~exits:Cli_common.exits)
    (Exit_status.term Term.(const status $ Cli_common.json $ Cli_common.cwd))

(* explain. *)

let json_abs_list paths =
  Output.Json.list
    (List.map (fun p -> Output.Json.string (Lpath.Abs.to_string p)) paths)

let policy_json p =
  let reads =
    match Sandbox.Policy.reads p with
    | Sandbox.Policy.All -> Output.Json.string "all"
    | Sandbox.Policy.Only roots -> json_abs_list roots
  in
  Output.Json.obj
    [
      ("reads", reads);
      ("writable_roots", json_abs_list (Sandbox.Policy.writable_roots p));
      ("protected_paths", json_abs_list (Sandbox.Policy.protected_paths p));
      ( "network",
        Output.Json.string
          (Sandbox.Policy.Network.to_string (Sandbox.Policy.network p)) );
    ]

let explain json cwd =
  Composition.with_base ~cwd ~overrides:[] (fun t ->
      match
        Composition.resolve_workspace t
          ~mode:(Composition.configured_sandbox_mode t)
          ~network:(configured_network t)
      with
      | Error status -> status
      | Ok cap ->
          let identity =
            Format.asprintf "%a" Sandbox.Identity.pp
              (Mentat_workspace_io.identity cap)
          in
          let evidence = Mentat_workspace_io.evidence cap in
          let policy = Mentat_workspace_io.policy cap in
          let roots = Mentat_workspace_io.describe_roots cap in
          if json then
            Output.stdout_printf "%s\n"
              (Output.Json.to_string
                 (Output.Json.envelope ~type_:"sandbox.explain"
                    [
                      ("identity", Output.Json.string identity);
                      ("evidence", Sandbox.Evidence.to_json evidence);
                      ( "policy",
                        match policy with
                        | None -> Output.Json.null
                        | Some p -> policy_json p );
                      ("roots", json_roots roots);
                    ]))
          else (
            Output.stdout_printf "identity=%s\n" identity;
            Output.stdout_printf "evidence=%s\n"
              (Format.asprintf "%a" Sandbox.Evidence.pp evidence);
            (match policy with
            | None -> Output.stdout_printf "policy=unconfined\n"
            | Some p ->
                Output.stdout_printf "network=%s\n"
                  (Sandbox.Policy.Network.to_string (Sandbox.Policy.network p));
                (match Sandbox.Policy.reads p with
                | Sandbox.Policy.All -> Output.stdout_printf "read=all\n"
                | Sandbox.Policy.Only rs ->
                    List.iter
                      (fun r ->
                        Output.stdout_printf "read-root=%s\n"
                          (Lpath.Abs.to_string r))
                      rs);
                List.iter
                  (fun r ->
                    Output.stdout_printf "write-root=%s\n"
                      (Lpath.Abs.to_string r))
                  (Sandbox.Policy.writable_roots p);
                List.iter
                  (fun r ->
                    Output.stdout_printf "protected=%s\n"
                      (Lpath.Abs.to_string r))
                  (Sandbox.Policy.protected_paths p));
            List.iter
              (fun (label, path) ->
                Output.stdout_printf "root=%s %s\n" label
                  (Lpath.Abs.to_string path))
              roots);
          Exit_status.Success)

let explain_cmd =
  let doc = "Explain the concrete sealed sandbox policy." in
  Cmd.v
    (Cmd.info "explain" ~doc ~docs ~exits:Cli_common.exits)
    (Exit_status.term Term.(const explain $ Cli_common.json $ Cli_common.cwd))

let cmd =
  let doc = "Inspect the sandbox posture." in
  Cmd.group
    (Cmd.info "sandbox" ~doc ~docs ~exits:Cli_common.exits)
    [ status_cmd; explain_cmd ]
