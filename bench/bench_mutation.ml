(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Claim-settle cost: folding the mutation ledger.

    Every consumer of edit history — the revert planner, the change log, crash
    recovery — reads a projection of one fold, [State.of_events]. The felt
    moment is the pause when a claim settles and its evidence is folded into
    state, and the same fold on every resume. The headline case rises the
    edits-per-claim axis at a fixed claim count: it guards the known quadratic
    in the per-claim change accumulation — if that regresses, a claim that made
    many edits settles in time growing with the square of its edit count. The
    linear reference rises the claim count at one edit each, so a regression
    that is merely more total work is told apart from one that is newly
    quadratic. *)

module M = Mentat_mutation
module Fixture = Bench_support.Ledger

let ledgers =
  List.map
    (fun (label, (claims, edits_per_claim)) ->
      (label, Fixture.build ~claims ~edits_per_claim))
    Fixture.settle_axis

let () =
  Thumper.run "mutation"
    ~budgets:
      [
        Thumper.Budget.no_more_alloc_than 0.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.cpu_time 1000.0;
      ]
    Thumper.
      [
        bench_param "of_events" ~params:ledgers ~f:(fun events ->
            M.State.of_events events);
      ]
