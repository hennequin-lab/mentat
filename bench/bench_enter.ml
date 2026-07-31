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
    fs-trend cases each run under their own [Eio_main.run], inside the forked
    measurement worker — a forked child cannot inherit a live Eio scheduler,
    so nothing here runs under Eio at top level. Their measured unit includes
    the scheduler start, a constant cost well under the load walks. *)

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

(* Pure setup, outside the measured region; none of it needs Eio. *)
let session = Bench_support.Enter.session
let state = Session.state session
let transcript = Session.State.model_transcript state
let model = Bench_support.Enter.model
let request = Llm.Request.make_exn ~model transcript

let root =
  let raw = Filename.temp_dir "mentat-bench-enter-" "" in
  let base = Unix.realpath raw in
  Bench_support.Workspace.materialize ~root:base;
  Lpath.Abs.of_string_exn base

let with_stdenv f () =
  Eio_main.run @@ fun env -> f (env :> Eio_unix.Stdenv.base)

let () =
  Thumper.run "enter"
    ~budgets:
      [
        Thumper.Budget.no_more_alloc_than 0.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
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
        (* The load walks allocate path strings built from the temp root, and
           the root's absolute path differs by context (dune's sandbox
           redirects TMPDIR), so allocation is not exactly reproducible
           across contexts — same class as session_list's scan, same small
           tolerance. A real regression would be proportional and far
           larger. *)
        group "fs-trend" ~budgets:[ Thumper.Budget.no_more_alloc_than 0.05 ]
          [
            bench "context/load"
              (with_stdenv (fun stdenv ->
                   Context.load ~environment:Context.Environment.none ~stdenv
                     ~nested_scan:false ~config:resolved_config ~trusted:true
                     ~root ~cwd:root ~user_config_file));
            bench "skills/load"
              (with_stdenv (fun stdenv ->
                   Skills.load ~stdenv ~builtins:[] ~config:resolved_config
                     ~trusted:true ~root ~cwd:root ~user_config_file));
          ];
      ]
