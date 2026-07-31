(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Transcript render: the per-frame cost of the scrollback.

    The transcript mounts every settled block in one scroll box, so a dirty
    frame rebuilds the whole view tree — its cost is O(session length). The felt
    moment is the smoothness of a long conversation: every keystroke, delta, and
    scroll that dirties a frame pays this construction once, so a regression
    here is felt as a growing sluggishness proportional to how much has been
    said. The block axis (100/1k/5k) says how the frame scales with conversation
    length, which is exactly the windowing gap the render loop is known to have.
    Construction is measured through the public block constructors and [view];
    no terminal, no layout, no paint — just the node tree a frame builds. *)

module T = Mentat_tui.Transcript
module Fixture = Bench_support.Transcript_fixture

let transcripts =
  List.map
    (fun (label, blocks) -> (label, Fixture.build ~blocks))
    Fixture.block_sizes

let () =
  Thumper.run "transcript"
    ~budgets:
      [
        Thumper.Budget.no_more_alloc_than 0.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
      ]
    Thumper.
      [
        group "view"
          (List.map
             (fun (label, transcript) ->
               bench label (fun () ->
                   ignore
                     (T.view ~palette:Fixture.palette transcript : _ Mosaic.t)))
             transcripts);
      ]
