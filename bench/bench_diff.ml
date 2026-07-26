(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Diff render: the text a user reads after an edit or a revert preview.

    When the agent finishes editing, the gap before the user can see what
    changed is this render — the line diff laid out and serialized to the text
    the TUI and CLI print. The felt moment is that gap; the file-count axis says
    how it scales with the size of a whole-turn multi-file change, where a slow
    render is the difference between a review that feels instant and one that
    stalls. Stats is the cheaper summary a header shows (+/- counts) and is
    guarded alongside so a header that recomputes the whole diff is caught. *)

module Diff = Textdiff
module Fixture = Bench_support.Diff_fixture

let () =
  Thumper.run "diff"
    ~budgets:
      [
        Thumper.Budget.no_more_alloc_than 0.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.cpu_time 1000.0;
      ]
    Thumper.
      [
        group "render"
          [
            bench "one-line" (fun () ->
                Diff.render [ Fixture.small_change ]
                |> Diff.to_string |> String.length);
            bench "200-line-file" (fun () ->
                Diff.render [ Fixture.medium_change ]
                |> Diff.to_string |> String.length);
            bench "20-files" (fun () ->
                Diff.render Fixture.multi_changes
                |> Diff.to_string |> String.length);
          ];
        bench "stats/20-files" (fun () ->
            (Diff.Stats.of_changes Fixture.multi_changes).Diff.additions);
      ]
