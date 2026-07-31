(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Real-binary coverage for executable-owned ergonomics paths a headless test
   cannot exercise: the external-editor escape (real suspend/spawn/read-back
   through $EDITOR), the keybindings.json load (a user file remaps an action
   end-to-end), and the launch-empty composer a quit leaves behind. All drive
   the real mentat binary through the pty. *)

open Tui_harness
open Cli_launch_fixture
module Pty = Pty_session

(* A fake $EDITOR that rewrites the temp file with fixed content. macOS assesses
   every fresh executable inode on its first exec (~130-165ms, and far longer
   when a full agent fleet floods syspolicyd); the TUI touches $EDITOR's path
   early enough that an unassessed inode can stall the launch. So write the
   script once and exec it here — outside the pty's ready deadline — so the
   assessment is already cached by the time the binary starts. *)
let fake_editor project =
  let path = Project.scratch project "editor/fake.sh" in
  Project.write_path path "#!/bin/sh\nprintf '%s' 'edited-in-pty' > \"$1\"\n";
  Unix.chmod path 0o755;
  (try
     let warm = Filename.temp_file "fake-editor-warm" "" in
     ignore
       (Sys.command (Filename.quote path ^ " " ^ Filename.quote warm) : int);
     Sys.remove warm
   with _ -> ());
  path

let saw terminal substring =
  ignore
    (Pty.wait terminal (fun screen ->
         Option.is_some (find_substring screen substring 0)))

(* Observe [substring] in the append-only byte stream. The editor case renders
   over the home's continuous brand-shimmer animation, whose cadence rarely
   presents [Pty.wait] a [not sync_active] grid to sample; the text it draws is
   nonetheless in the raw stream, so key on that instead of the frame grid. *)
let saw_raw terminal substring =
  Pty.wait_raw terminal (fun raw ->
      Option.is_some (find_substring raw substring 0))

let%expect_test "a keybindings.json remap loads and takes effect" =
  Project.with_temp "ergonomics-keys" @@ fun project ->
  (* Move history search onto Ctrl+T; the bogus entry exercises the diagnostic
     path (logged, degrades to default) without failing the load. Ctrl+G is the
     command palette's own chord, so an unrelated free chord is used here. *)
  Project.write_path
    (Project.scratch project "config/mentat/keybindings.json")
    {|{"history_search": "ctrl+t", "not_an_action": "ctrl+z"}|};
  Project.write_path
    (Project.scratch project "state/mentat/history.jsonl")
    {|{"schema_version":1,"type":"mentat.tui.history_entry","session_id":"s","timestamp":"1000","draft":{"text":"remembered prompt"}}|};
  Pty.run project @@ fun terminal ->
  Pty.send terminal "\020";
  (* Ctrl+T — the remapped history search. *)
  saw terminal "reverse-i-search";
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    06 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    07 |
    08 |                          dev · openai/gpt-5.6-sol medium
    09 |
    10 |      ▎ welcome — and thanks for trying mentat this early.
    11 |      ▎ it's experimental: sessions and config may change without migration.
    12 |
    13 | reverse-i-search:
    14 | ❯ remembered prompt
    15 | ────────────────────────────────────────────────────────────────────────────────
    16 | ⌕ search history
    17 | ────────────────────────────────────────────────────────────────────────────────
    18 |
    19 |                          ! /login — no connected account
    20 |                               ∅ no recent sessions
    21 |
    22 |
    23 |
    24 |   ↵ insert · esc cancel · type to search                             ⌕ history
    |}];
  Pty.quit terminal

let%expect_test
    "the editor chord round-trips the draft through the real $EDITOR" =
  Project.with_temp "ergonomics-editor" @@ fun project ->
  let editor = fake_editor project in
  (* Home ready and the read-back are observed on the raw byte stream (see
     [saw_raw]); the home's shimmer cadence makes [Pty.wait]'s frame-boundary
     gate unreliable here even though every frame is drawn. *)
  Pty.run
    ~ready_raw:(fun raw ->
      Option.is_some (find_substring raw "message mentat" 0))
    ~env:[ ("EDITOR", editor) ]
    ~unset:[ "VISUAL" ] project
  @@ fun terminal ->
  (* Type a draft, then the editor chord Ctrl+X Ctrl+E. Input is delivered and
     parsed in order, so the draft is in place before the chord arms. *)
  Pty.send terminal "draft body\024\005";
  (* The runtime suspends, spawns $EDITOR on the temp file seeded with the draft,
     and installs the read-back ("edited-in-pty"). *)
  saw_raw terminal "edited-in-pty";
  (* Best-effort: quiesce onto a stable frame for the golden. If the shimmer
     never quiesces, capture the latest grid anyway. *)
  (try Pty.settle terminal with _ -> ());
  Screen.print ~project (Pty.screen terminal);
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    06 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    07 |
    08 |                          dev · openai/gpt-5.6-sol medium
    09 |
    10 |      ▎ welcome — and thanks for trying mentat this early.
    11 |      ▎ it's experimental: sessions and config may change without migration.
    12 |
    13 |
    14 | ────────────────────────────────────────────────────────────────────────────────
    15 | ❯ edited-in-pty
    16 | ────────────────────────────────────────────────────────────────────────────────
    17 |
    18 |                          ! /login — no connected account
    19 |                               ∅ no recent sessions
    20 |
    21 |
    22 |
    23 |
    24 |   ! not logged in · /login · ~/me… · openai/gpt-5.6-sol … · ! full access ? f…
    |}];
  (* Teardown, observed on the raw stream for the same cadence reason: the
     composer is non-empty, so the first Ctrl+C discards the draft, the second
     arms quit, the third confirms. *)
  Pty.send terminal "\003";
  Pty.send terminal "\003";
  saw_raw terminal "press ctrl+c again to quit";
  Pty.send terminal "\003";
  Pty.wait_exit terminal

(* A discarded draft is remembered in prompt history only: quitting after a
   discard leaves no standing-draft file, and the next launch's composer is
   empty — its ready gate is the placeholder itself. Teardown is observed on
   the raw stream for the shimmer-cadence reason above. *)
let%expect_test "a discarded draft never seeds the next launch" =
  Project.with_temp "ergonomics-draft" @@ fun project ->
  let standing_draft () =
    if Sys.file_exists (Project.state project "draft.json") then
      print_string "standing draft"
    else print_string "no standing draft"
  in
  ( Pty.run project @@ fun terminal ->
    Pty.send terminal "keep me";
    saw terminal "keep me";
    (* The composer is non-empty, so the first Ctrl+C discards, the second
      arms quit, the third confirms. *)
    Pty.send terminal "\003";
    Pty.send terminal "\003";
    saw_raw terminal "press ctrl+c again to quit";
    Pty.send terminal "\003";
    Pty.wait_exit terminal );
  standing_draft ();
  [%expect {| no standing draft |}];
  (* The relaunch's ready gate sees the composer placeholder: nothing was
     restored from the discarded draft. *)
  ( Pty.run project @@ fun terminal ->
    Pty.send terminal "\003";
    saw_raw terminal "press ctrl+c again to quit";
    Pty.send terminal "\003";
    Pty.wait_exit terminal );
  standing_draft ();
  [%expect {| no standing draft |}]

