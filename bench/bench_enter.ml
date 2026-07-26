(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Enter overhead: the work between "start a turn" and "send the request".

    Before the first provider byte of a turn, mentat projects the model-visible
    transcript, snapshots the workspace context and skills, and assembles and
    digests the request. The felt moment is the gap between submitting a prompt
    and the model beginning to answer — the part mentat owns, before the
    network.

    The pure group is the alloc gate: [model_transcript] projects the provider
    view from replayed state, [request/make] assembles the checked request, and
    [request/digest] canonicalizes it for the cache key — all deterministic, so
    a regression here is a real one. The fs-trend group loads the context and
    skills from a fixed on-disk tree: its allocation is deterministic and gated,
    but its wall time is filesystem- and OS-dependent (directory walks, stat
    latency), recorded as trend only, never a gate.

    NOTE (verify): confirms [Mentat_context.Context]/[Skills] module paths,
    [Environment.none], the [Mentat_config.resolve] arguments, and that a plain
    user/assistant transcript ending in a user message is request-ready. The
    whole suite runs inside [Eio_main.run] so the load cases have an stdenv;
    --deterministic keeps fork = Never, so no subprocess is spawned under Eio.
*)

module Session = Mentat_session
module Llm = Mentat_llm
module Context = Mentat_context.Context
module Skills = Mentat_context.Skills

let resolved_config =
  match
    Mentat_config.resolve
      ~env:(fun _ -> None)
      ~user:
        ( Lpath.Abs.of_string_exn "/mentat-no-such-home/config.json",
          Mentat_config.empty )
      ~extra:None
      ~workspace_config:
        (Mentat_config.Disabled
           { status = "bench"; project = None; project_local = None })
      ~overrides:[]
  with
  | Ok resolved -> resolved
  | Error error -> failwith (Mentat_config.Error.message error)

let user_config_file =
  Lpath.Abs.of_string_exn "/mentat-no-such-home/config.json"

let () =
  (* Pure setup, outside the measured region. *)
  let session = Bench_support.Enter.session in
  let state = Session.state session in
  let transcript = Session.State.model_transcript state in
  let model = Bench_support.Enter.model in
  let request = Llm.Request.make_exn ~model transcript in
  Eio_main.run @@ fun env ->
  let stdenv = (env :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir "mentat-bench-enter-" "" in
  let base = Unix.realpath raw in
  Bench_support.Workspace.materialize ~root:base;
  let root = Lpath.Abs.of_string_exn base in
  Thumper.run "enter"
    ~budgets:
      [
        Thumper.Budget.no_more_alloc_than 0.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.cpu_time 1000.0;
      ]
    Thumper.
      [
        group "pure"
          [
            bench "model_transcript" (fun () ->
                Session.State.model_transcript state);
            bench "request/make" (fun () -> Llm.Request.make ~model transcript);
            bench "request/digest" (fun () -> Llm.Request.digest request);
          ];
        group "fs-trend"
          [
            bench "context/load" (fun () ->
                Context.load ~environment:Context.Environment.none ~stdenv
                  ~nested_scan:false ~config:resolved_config ~trusted:true ~root
                  ~cwd:root ~user_config_file);
            bench "skills/load" (fun () ->
                Skills.load ~stdenv ~builtins:[] ~config:resolved_config
                  ~trusted:true ~root ~cwd:root ~user_config_file);
          ];
      ]
