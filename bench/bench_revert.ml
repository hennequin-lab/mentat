(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Revert-preview cost: planning an undo and rendering its diff.

    When a user asks to revert, two pure computations run before anything
    touches a file: preparation resolves the selection, checks each target
    against the recorded head, and three-way merges every path a later edit
    superseded; the display diff nets the same selection and lays out its hunks
    and merge preview. The felt moment is the pause between "revert this" and
    the preview appearing. The supersession axis (0/50/100 percent of paths)
    drives how much of that pause is the merge: at 0 percent it is a plain
    net-and-diff, at 100 percent every path is merged. Both cases measure the
    computation whatever its verdict — a refusal or a conflict does the same
    work a clean plan does, which is the work being guarded. *)

module M = Mentat_mutation
module Fixture = Bench_support.Ledger

let cases =
  List.map
    (fun (label, pct) -> (label, Fixture.revert_case ~superseded_pct:pct))
    Fixture.superseded_axis

let revert_id = M.Revert.Id.of_string "bench-revert"

let () =
  Thumper.run "revert"
    ~budgets:
      [
        Thumper.Budget.no_more_alloc_than 0.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
      ]
    Thumper.
      [
        group "prepare"
          (List.map
             (fun (label, { Fixture.state; selection; evidence; _ }) ->
               bench label (fun () ->
                   M.Revert.prepare ~id:revert_id ~evidence state selection))
             cases);
        group "diff-compute"
          (List.map
             (fun (label, { Fixture.state; selection; resolve; _ }) ->
               bench label (fun () ->
                   M.Diff.compute ~state ~selection ~resolve ()))
             cases);
      ]
