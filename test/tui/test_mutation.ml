(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Llm = Mentat_llm
module Output = Mentat_tools_output
module Session = Mentat_session
module Tool = Mentat_tool
module Mutation = Tui.Mutation_script
module Change = Mutation.Change

let rel = Lpath.Rel.of_string_exn

let call ~id ~name =
  Llm.Tool.Call.make ~id ~name ~input:(Jsont.Json.object' []) ()

let completed output = Tool.Result.completed ~output ()

let update_output ~files ~additions ~deletions =
  Output.Codec.encode Output.Update.jsont ~text:"updated workspace files"
    (Output.Update.make ~disposition:Output.Update.Applied ~files
       ~additions:(Some additions) ~deletions:(Some deletions) ~skipped:0)

let write_output ~lines =
  Output.Codec.encode Output.Write.jsont ~text:"wrote complete file"
    (Output.Write.wrote ~lines)

let settle_turn t prompt =
  Tui.settle t;
  Tui.keys t prompt;
  Tui.enter t;
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t

(* Mutation bytes are absent from tool output and remain independently owned.
   The complete settled frame must expose their cumulative evidence and the
   latest-change affordance; ctrl+d then opens the one review surface focused on
   the latest change's file (proving diffs are read in a single place), and
   Escape restores the unsent composer draft. *)
let%expect_test
    "settled evidence opens the review on the latest change and preserves the \
     draft" =
  let math_call = call ~id:"math-edit" ~name:"edit_file" in
  let notes_call = call ~id:"notes-create" ~name:"write_file" in
  let math_before = "let answer = 41\nlet stable = true\n" in
  let math_after = "let answer = 42\nlet stable = true\n" in
  let notes_after =
    "- Added structured mutation inspection for terminal users.\n\
     - Preserved the unsent composer draft while reviewing changes.\n"
  in
  (* The changelog is created first, the answer bumped last, so the latest change
     is [lib/math.ml]. *)
  let tools =
    [
      Tui.Turn_script.tool ~call:notes_call
        ~result:(completed (write_output ~lines:2));
      Tui.Turn_script.tool ~call:math_call
        ~result:(completed (update_output ~files:1 ~additions:1 ~deletions:1));
    ]
  in
  let mutations =
    [
      Mutation.make ~call_id:"notes-create"
        ~changes:
          [ Change.create ~path:(rel "CHANGELOG.md") ~contents:notes_after ]
        ();
      Mutation.make ~call_id:"math-edit"
        ~changes:
          [
            Change.update ~path:(rel "lib/math.ml") ~before:math_before
              ~after:math_after;
          ]
        ();
    ]
  in
  let turn =
    Tui.Turn_script.complete ~prompt:"update the release notes" ~tools
      ~mutations "The release notes and answer are updated."
  in
  (* The workspace review of the same worktree the mutations describe. ctrl+d
     opens it focused on the latest change, [lib/math.ml], not [CHANGELOG.md]
     (the review's first unit). *)
  let review =
    [
      ("lib/math.ml", Some math_before, Some math_after);
      ("CHANGELOG.md", None, Some notes_after);
    ]
  in
  Tui.run ~name:"mutation-exact" ~home:(fun _ -> None) ~turns:[ turn ] ~review
  @@ fun t ->
  settle_turn t "update the release notes";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-mutati6802be37.home/mentat-tui…
    04 |
    05 | ❯ update the release notes
    06 |
    07 | ⏺ Create
    08 |   ⎿  Wrote 2 lines
    09 |
    10 | ⏺ Edit
    11 |   ⎿  Updated 1 file (+1, -1)
    12 |
    13 | ⏺ The release notes and answer are updated.
    14 |
    15 | ⊙ workspace changed · 2 files · +3 −1 · revert available
    16 |
    17 |
    18 |
    19 |
    20 |   Δ 2 changes · +3 −1 · ctrl+d opens latest
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…
    |}];
  Tui.keys t "please keep this draft";
  Tui.settle t;
  Tui.keys t "\004";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 | ────────────────────────────────────────────────────────────────────────────────
    02 | Review  HEAD..worktree                                    0/2 reviewed · pending
    03 |    [ ] CHANGELOG.md           A │lib/math.ml · unreviewed · +1 −1
    04 |  ▾ lib                          │ 1 - let answer = 41
    05 |    ❯ [ ] math.ml              M │ 1 + let answer = 42
    06 |                                 │ 2   let stable = true
    07 |                                 │
    08 |                                 │
    09 |                                 │
    10 |                                 │
    11 |                                 │
    12 |                                 │
    13 |                                 │
    14 |                                 │
    15 |                                 │
    16 |                                 │
    17 |                                 │
    18 |                                 │
    19 |                                 │
    20 |                                 │
    21 |                                 │
    22 |                                 │
    23 |
    24 | tab focus diff · space mark · enter open · c comment · a approve · esc close
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-mutati6802be37.home/mentat-tui…
    04 |
    05 | ❯ update the release notes
    06 |
    07 | ⏺ Create
    08 |   ⎿  Wrote 2 lines
    09 |
    10 | ⏺ Edit
    11 |   ⎿  Updated 1 file (+1, -1)
    12 |
    13 | ⏺ The release notes and answer are updated.
    14 |
    15 | ⊙ workspace changed · 2 files · +3 −1 · revert available
    16 |
    17 |
    18 |
    19 |
    20 |   Δ 2 changes · +3 −1 · ctrl+d opens latest
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ please keep this draft
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…
    |}]

(* An ambiguous callback may have no exact transition while the mutation owner
   still reports paths observed inside its scope. The complete settled frame is
   the honesty contract: it must retain the unknown tool outcome, the observed
   path count, and incomplete revertability without inventing a diff action. *)
let%expect_test "ambiguous observed-only mutation evidence stays explicit" =
  let call = call ~id:"opaque-write" ~name:"opaque_workspace_writer" in
  let turn =
    Tui.Turn_script.complete ~prompt:"refresh the generated index"
      ~tools:[ Tui.Turn_script.ambiguous_tool ~call ]
      ~mutations:
        [
          Mutation.make ~call_id:"opaque-write"
            ~observed:[ rel "_generated/index.bin" ]
            ();
        ]
      "The index refresh could not be confirmed."
  in
  Tui.run ~name:"mutation-observed" ~home:(fun _ -> None) ~turns:[ turn ]
  @@ fun t ->
  settle_turn t "refresh the generated index";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-mutation-7e1b31fe.home/mentat-…
    04 |
    05 | ❯ refresh the generated index
    06 |
    07 | ⏺ Opaque_workspace_writer
    08 |   ⎿  outcome unknown; the tool may have run
    09 |
    10 | ⏺ The index refresh could not be confirmed.
    11 |
    12 | ⊙ workspace may still be changing · 1 path observed · revert incomplete
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · /tmp/mentat-tu… · openai/gp… · ! full access ? f…
    |}]

let resumed_id = Session.Id.of_string "session-mutation-recovery"

let resumed_session project =
  Session.create ~id:resumed_id
    ~cwd:(Lpath.Abs.of_string_exn (Project.root project))
    ~created_at:(Session.Time.of_unix_ms 1_000_000L)
    ()

(* Recovery ambiguity is process-local owner truth supplied at attach, not a
   fabricated journal fact. The complete frame keeps the warning adjacent to
   the otherwise ordinary resumed conversation. *)
let%expect_test "a resumed possibly-mutating session shows the recovery warning"
    =
  Tui.run ~name:"mutation-recovery"
    ~home:(fun _ -> None)
    ~sessions:(fun project -> [ resumed_session project ])
    ~session:resumed_id ~possibly_mutating:true
  @@ fun t ->
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-mutation-72b46fe1.home/mentat-…
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |   ! recovered session may still have a workspace mutation in flight
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · /tmp/ment… · openai/gpt-5.5 … · ! full access ? …
    |}]
