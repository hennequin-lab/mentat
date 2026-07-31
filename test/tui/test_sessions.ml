(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Session = Mentat_session
module Status = Session.Metadata.Status
module Protocol = Mentat_protocol

let provider = Mentat_llm.Provider.make "openai"
let api = Mentat_llm.Model.Api.make "responses"
let model = Mentat_llm.Model.make ~provider ~api ~id:"gpt-5.5"

let contract =
  Session.Contract.make ~mode:Session.Contract.Mode.Build ~model
    ~declarations:[] ~policy:Mentat_permission.Policy.default
    ~review:Mentat_permission.Review_behavior.Enforce
    ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
    ()

let time seconds =
  seconds |> Int64.of_int |> Int64.mul 1_000L |> Session.Time.of_unix_ms

let session ?(status = Status.Active) ?cwd ?prompt ~id ~title ~updated_at
    project =
  let id = Session.Id.of_string id in
  let cwd =
    Option.value cwd ~default:(Lpath.Abs.of_string_exn (Project.root project))
  in
  let metadata =
    Session.Metadata.make ~title ~status ~cwd ~created_at:(time 1)
      ~updated_at:(time updated_at) ()
  in
  let events =
    match prompt with
    | None -> []
    | Some prompt ->
        let turn =
          Session.Turn.make
            ~id:(Session.Turn.Id.of_string (Session.Id.to_string id ^ "-turn"))
            ~origin:Session.Turn.Origin.User
            ~input:(Session.Turn.Input.user_text prompt)
            ~max_steps:100 ~contract ()
        in
        [
          Session.Event.turn_started turn;
          Session.Event.turn_finished ~turn:(Session.Turn.id turn)
            Session.Turn.Outcome.completed;
        ]
  in
  match Session.make ~id ~metadata ~events with
  | Ok session -> session
  | Error error ->
      failwith ("invalid visual session: " ^ Session.Error.message error)

let browser_rows project =
  [
    session ~id:"session-parser" ~title:"Parser streaming cleanup"
      ~updated_at:1_000 project;
    session ~id:"session-archive" ~title:"Archived protocol notes"
      ~status:Status.Archived ~updated_at:970 project;
    session ~id:"session-auth" ~title:"Authentication dialog polish"
      ~updated_at:940 project;
    session ~id:"session-deleted" ~title:"Deleted scratch experiment"
      ~status:Status.Deleted ~updated_at:910 project;
  ]

let paged_rows project =
  List.init 18 (fun index ->
      let number = index + 1 in
      session
        ~id:(Printf.sprintf "session-page-%02d" number)
        ~title:(Printf.sprintf "Measured table row %02d" number)
        ~updated_at:(1_000 - index) project)

let numbered_resume_rows project =
  List.init 9 (fun index ->
      let number = index + 1 in
      let prompt =
        if number = 9 then Some "The ninth quick-switch session is active."
        else None
      in
      session ?prompt
        ~id:(Printf.sprintf "session-numbered-%02d" number)
        ~title:(Printf.sprintf "Numbered quick row %02d" number)
        ~updated_at:(1_000 - index) project)

let submit t text =
  Tui.paste t text;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let open_browser t =
  submit t "/sessions";
  Tui.keys t Key.tab;
  Tui.settle t

let switch_rows project =
  [
    session ~id:"session-admitted" ~title:"Admitted transcript"
      ~updated_at:1_000 project;
    session ~id:"session-candidate" ~title:"Candidate transcript"
      ~updated_at:900 project;
  ]

let detail_rows project =
  let root = Project.root project in
  [
    session ~id:"session-parser-detail" ~title:"Parser streaming cleanup"
      ~prompt:"Trace the parser's final streaming boundary."
      ~cwd:(Lpath.Abs.of_string_exn (Filename.concat root "parser"))
      ~updated_at:1_000 project;
    session ~id:"session-auth-detail" ~title:"Authentication dialog polish"
      ~prompt:"Keep the device login recovery action visible."
      ~cwd:(Lpath.Abs.of_string_exn (Filename.concat root "auth"))
      ~updated_at:900 project;
  ]

let unavailable message =
  Protocol.Error.Unavailable (Mentat_diagnostic.of_text message)

let home_is_project project =
  Some (Lpath.Abs.of_string_exn (Project.root project))

let%expect_test "browser selection replaces the complete owner detail" =
  Tui.run ~name:"sessions-detail" ~home:home_is_project ~sessions:detail_rows
  @@ fun t ->
  open_browser t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 2 sessions
    02 |
    03 |    session                               lifecycle phase  turns updated  recency
    04 | ❯  Parser streaming cleanup              active    idle  1 turn just now today
    05 |    Authentication dialog polish          active    idle  1 turn 1m ago   today
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   Trace the parser's final streaming boundary.
    15 |
    16 |   cwd ~/parser
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}];
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 2 sessions
    02 |
    03 |    session                               lifecycle phase  turns updated  recency
    04 |    Parser streaming cleanup              active    idle  1 turn just now today
    05 | ❯  Authentication dialog polish          active    idle  1 turn 1m ago   today
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   Keep the device login recovery action visible.
    15 |
    16 |   cwd ~/auth
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}]

let%expect_test "rename keeps Unicode-whitespace validation visible" =
  Tui.run ~name:"sessions-rename-validation" ~home:home_is_project
    ~sessions:(fun project ->
      [ session ~id:"session-rename" ~title:"X" ~updated_at:1_000 project ])
  @@ fun t ->
  open_browser t;
  Tui.keys t "r";
  Tui.settle t;
  Tui.keys t Key.backspace;
  Tui.keys t "\u{2003}";
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ──────────────────────────────────────────────────────────── 1 session
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  rename:  ▏                           active    idle  0 turns just now today
    05 |
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   rename:  ▏
    15 |
    16 |   Title must contain a non-whitespace character.
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ save · esc cancel
    |}]

let%expect_test "a replacement retires a stale initial start" =
  let replacement =
    Tui.Turn_script.complete ~prompt:"use admitted session"
      "replacement accepted the prompt"
  in
  Tui.run ~name:"sessions-stale-initial-start" ~drop_first_start:true
    ~sessions:(fun project ->
      [
        session ~id:"session-candidate" ~title:"Candidate transcript"
          ~updated_at:900 project;
      ])
    ~turns:[ replacement ]
  @@ fun t ->
  submit t "superseded initial prompt";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-stale-initi49b8631a
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
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat… · openai/gpt-5.5 … · ! full access ? f…
    |}];
  submit t "/sessions";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  submit t "use admitted session";
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-stale-initi49b8631a
    04 |
    05 | ❯ use admitted session
    06 |
    07 | ⏺ replacement accepted the prompt
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
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui… · openai/gpt… · ! full access ? fo…
    |}]

let%expect_test "failed resume preserves the admitted projection" =
  let first =
    Tui.Turn_script.complete ~prompt:"remember this" "kept transcript evidence"
  in
  Tui.run ~name:"sessions-failed-resume" ~sessions:switch_rows
    ~session:(Session.Id.of_string "session-admitted")
    ~turns:[ first ]
  @@ fun t ->
  submit t "remember this";
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-failebd85c4be
    04 |
    05 | ❯ remember this
    06 |
    07 | ⏺ kept transcript evidence
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
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tu… · openai/gpt… · ! full access ? for…
    |}];
  Tui.fail_next_resume t (unavailable "candidate follow refused");
  submit t "/sessions";
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-failebd85c4be
    04 |
    05 | ❯ remember this
    06 |
    07 | ⏺ kept transcript evidence
    08 |
    09 | ✗ candidate follow refused
    10 |   retry when ready
    11 |
    12 |
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
    24 |   candidate follow refused
    |}]

let%expect_test "terminal feed loss disables submit until explicit resume" =
  let before = Tui.Turn_script.complete ~prompt:"before loss" "before answer" in
  let after = Tui.Turn_script.complete ~prompt:"after resume" "after answer" in
  Tui.run ~name:"sessions-feed-loss" ~sessions:switch_rows
    ~session:(Session.Id.of_string "session-admitted")
    ~turns:[ before; after ]
  @@ fun t ->
  submit t "before loss";
  Tui.finish_turn t;
  Tui.settle t;
  Tui.lose_feed t ~message:"observation stopped";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-ffa51e691
    04 |
    05 | ❯ before loss
    06 |
    07 | ⏺ before answer
    08 |
    09 | ✗ observation stopped
    10 |   check the connection, then retry
    11 |
    12 |
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
    24 |   observation stopped
    |}];
  Tui.paste t "must not submit";
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-ffa51e691
    04 |
    05 | ❯ before loss
    06 |
    07 | ⏺ before answer
    08 |
    09 | ✗ observation stopped
    10 |   check the connection, then retry
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ must not submit
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   observation stopped
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.keys t Key.escape;
  Tui.settle t;
  submit t "/sessions";
  Tui.enter t;
  Tui.settle t;
  submit t "after resume";
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-ffa51e691
    04 |
    05 | ❯ before loss
    06 |
    07 | ⏺ before answer
    08 |
    09 | ❯ after resume
    10 |
    11 | ⏺ after answer
    12 |
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
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full access ? for…
    |}]

let%expect_test "retired feed messages cannot detach the replacement" =
  let after =
    Tui.Turn_script.complete ~prompt:"use replacement" "replacement answered"
  in
  Tui.run ~name:"sessions-stale-feed" ~sessions:switch_rows
    ~session:(Session.Id.of_string "session-admitted")
    ~turns:[ after ]
  @@ fun t ->
  Tui.hold_feed_pull t;
  submit t "/sessions";
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.lose_retired_feed t ~message:"stale feed failure";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-st0237a4b5
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
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/menta… · openai/gpt-5.5 … · ! full access ? fo…
    |}];
  submit t "use replacement";
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-st0237a4b5
    04 |
    05 | ❯ use replacement
    06 |
    07 | ⏺ replacement answered
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
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-t… · openai/gpt-… · ! full access ? for…
    |}]

let%expect_test "browser normalizes the shared hostile inline family" =
  let hostile =
    "A\tB\r\nC\u{2028}D\u{2029}E\000F\u{0085}G\255H e\u{0301}\u{2003}I"
  in
  (* The Matrix screen stores the base glyph rather than its zero-width
     combining cell. The surrounding em space makes that renderer behavior
     visible in the complete frame without reaching around the public TUI
     route. *)
  Tui.run ~name:"sessions-hostile-inline" ~sessions:(fun project ->
      [ session ~id:"session-hostile" ~title:hostile ~updated_at:1_000 project ])
  @@ fun t ->
  open_browser t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ──────────────────────────────────────────────────────────── 1 session
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  A B  C D E�F�G�H e I                 active    idle  0 turns just now today
    05 |
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~/mentat-tui-sessions-hostil6bf78577
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}]

(* The browser response travels through Runtime's session query and carries a
   report-only diagnostic beside healthy owner summaries. Filtering must keep
   the warning and still present the complete terminal for visual review. *)
let%expect_test "browser keeps partial listing diagnostics while filtering" =
  Tui.run ~name:"sessions-filter" ~sessions:browser_rows
    ~session_diagnostics:
      [
        Mentat_diagnostic.of_text "one damaged session document was skipped";
        Mentat_diagnostic.of_text "stale index ignored";
      ]
  @@ fun t ->
  open_browser t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |   ! one damaged session document was skipped · stale index ignored
    04 |    session                              lifecycle phase   turns updated  recency
    05 | ❯  Parser streaming cleanup             active    idle  0 turns just now today
    06 |    Archived protocol notes              archived  idle  0 turns just now today
    07 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    08 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~/mentat-tui-sessionacd22caf
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}];
  Tui.keys t "/archive";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |   /archive  1 match
    03 |
    04 |   ! one damaged session document was skipped · stale index ignored
    05 |    session                              lifecycle phase   turns updated  recency
    06 | ❯  Archived protocol notes              archived  idle  0 turns just now today
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |   cwd ~/mentat-tui-sessionacd22caf
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↑↓ select · esc clear filter
    |}]

let%expect_test "browser renders its complete empty state" =
  Tui.run ~name:"sessions-empty" @@ fun t ->
  open_browser t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 0 sessions
    02 |
    03 |   No sessions in this workspace.
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
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   / filter · esc back
    |}]

(* The quick switcher gives Mosaic every healthy row. These complete frames
   make the table's measured viewport, page movement, selected-row reveal, and
   exact-ID promotion into the browser visually inspectable. *)
let%expect_test "quick switcher pages and promotes the complete result" =
  Tui.run ~name:"sessions-quick-pages" ~sessions:paged_rows @@ fun t ->
  submit t "/sessions";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    06 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    07 |
    08 |                            dev · openai/gpt-5.5 medium
    09 |
    10 |      ▎ welcome — and thanks for trying mentat this early.
    11 |      ▎ it's experimental: sessions and config may change without migration.
    12 |
    13 |
    14 |
    15 |
    16 |
    17 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    18 |    sessions
    19 |
    20 |  ❯   Measured table row 01                                   idle     just now │
    21 |      Measured table row 02                                   idle     just now │
    22 |      Measured table row 03                                   idle     just now ↓
    23 |
    24 |
    |}];
  Tui.keys t Key.page_down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    06 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    07 |
    08 |                            dev · openai/gpt-5.5 medium
    09 |
    10 |      ▎ welcome — and thanks for trying mentat this early.
    11 |      ▎ it's experimental: sessions and config may change without migration.
    12 |
    13 |
    14 |
    15 |
    16 |
    17 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    18 |    sessions
    19 |
    20 |      Measured table row 03                                   idle     just now ↑
    21 |  ❯   Measured table row 04                                   idle     just now │
    22 |      Measured table row 05                                   idle     just now ↓
    23 |
    24 |
    |}];
  Tui.keys t Key.tab;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ────────────────────────────────────────────────────────── 18 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 |    Measured table row 01                active    idle  0 turns just now today
    05 |    Measured table row 02                active    idle  0 turns just now today
    06 |    Measured table row 03                active    idle  0 turns just now today
    07 | ❯  Measured table row 04                active    idle  0 turns just now today
    08 |    Measured table row 05                active    idle  0 turns just now today
    09 |    Measured table row 06                active    idle  0 turns just now today
    10 |    Measured table row 07                active    idle  0 turns just now today
    11 |    Measured table row 08                active    idle  0 turns just now today
    12 |    Measured table row 09                active    idle  0 turns just now today
    13 |
    14 |   cwd ~/mentat-tui-sessions-quiec3b152f
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}]

(* Number shortcuts address the complete filtered result rather than a
   Mentat-owned four-row window. The target transcript makes the selected
   identity observable in the complete resumed-chat frame. *)
let%expect_test "quick switcher digit nine resumes the ninth result" =
  Tui.run ~name:"sessions-quick-digit" ~sessions:numbered_resume_rows
  @@ fun t ->
  submit t "/sessions";
  Tui.keys t "9";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-sessions-qui48debcf3
    04 |
    05 | ❯ The ninth quick-switch session is active.
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
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tu… · openai/gpt… · ! full access ? for…
    |}]

(* PageUp and PageDown are intentionally sent to the focused Mosaic Table.
   Eighteen rows exceed this terminal's measured table allocation, so the
   selected row and viewport expose Mosaic's real page size rather than a
   Mentat-owned row count. *)
let%expect_test "browser pages by the measured Mosaic table viewport" =
  Tui.run ~name:"sessions-pages" ~sessions:paged_rows @@ fun t ->
  open_browser t;
  Tui.keys t Key.page_down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ────────────────────────────────────────────────────────── 18 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 |    Measured table row 06                active    idle  0 turns just now today
    05 |    Measured table row 07                active    idle  0 turns just now today
    06 |    Measured table row 08                active    idle  0 turns just now today
    07 |    Measured table row 09                active    idle  0 turns just now today
    08 | ❯  Measured table row 10                active    idle  0 turns just now today
    09 |    Measured table row 11                active    idle  0 turns just now today
    10 |    Measured table row 12                active    idle  0 turns just now today
    11 |    Measured table row 13                active    idle  0 turns just now today
    12 |    Measured table row 14                active    idle  0 turns just now today
    13 |
    14 |   cwd ~/mentat-tui-sessiod259e2f5
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}];
  Tui.keys t Key.page_up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ────────────────────────────────────────────────────────── 18 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  Measured table row 01                active    idle  0 turns just now today
    05 |    Measured table row 02                active    idle  0 turns just now today
    06 |    Measured table row 03                active    idle  0 turns just now today
    07 |    Measured table row 04                active    idle  0 turns just now today
    08 |    Measured table row 05                active    idle  0 turns just now today
    09 |    Measured table row 06                active    idle  0 turns just now today
    10 |    Measured table row 07                active    idle  0 turns just now today
    11 |    Measured table row 08                active    idle  0 turns just now today
    12 |    Measured table row 09                active    idle  0 turns just now today
    13 |
    14 |   cwd ~/mentat-tui-sessiod259e2f5
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}]

(* Each lifecycle command remains visibly pending at the exact client boundary.
   Conflicting keys cannot issue a second operation while that result is held;
   releasing it then exposes the correlated refreshed owner projection. Delete
   keeps its separate, cancelable first-key confirmation. *)
let%expect_test "browser lifecycle actions and delete dialog return visually" =
  let parser = Session.Id.of_string "session-parser" in
  let lifecycle_results =
    [
      Tui.Lifecycle_script.rename ~session:parser
        ~title:"Parser streaming cleanup v2" (Ok ());
      Tui.Lifecycle_script.archive ~session:parser (Ok ());
      Tui.Lifecycle_script.restore ~session:parser (Ok ());
      Tui.Lifecycle_script.delete ~session:parser (Ok ());
    ]
  in
  Tui.run ~name:"sessions-lifecycle" ~home:home_is_project
    ~sessions:browser_rows ~lifecycle_results ~hold_lifecycle_results:true
  @@ fun t ->
  open_browser t;
  Tui.keys t "r";
  Tui.settle t;
  Tui.keys t " v2";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  rename: Parser streaming cleanup v2▏ active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   rename: Parser streaming cleanup v2▏
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ save · esc cancel
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.keys t "a";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  saving: Parser streaming cleanup v2  active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   ⠋ saving “Parser streaming cleanup v2”…
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   updating session…
    |}];
  Tui.finish_lifecycle_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  Parser streaming cleanup v2          active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}];
  Tui.keys t "a";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  archiving…                           active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   ⠋ archiving “Parser streaming cleanup v2”…
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   updating session…
    |}];
  Tui.keys t "d";
  Tui.settle t;
  Tui.finish_lifecycle_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  Parser streaming cleanup v2          archived  idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   r rename · u restore · d delete · / filter · esc back
    |}];
  Tui.keys t "u";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  restoring…                           archived  idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   ⠋ restoring “Parser streaming cleanup v2”…
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   updating session…
    |}];
  Tui.keys t "a";
  Tui.settle t;
  Tui.finish_lifecycle_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  Parser streaming cleanup v2          active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}];
  Tui.keys t "d";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  DELETE? press d again                active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   Permanently delete “Parser streaming cleanup v2”?
    15 |
    16 |   Press d again to DELETE. Any other key cancels.
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   d DELETE · any key cancel
    |}];
  Tui.keys t "x";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  Parser streaming cleanup v2          active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}];
  Tui.keys t "dd";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  deleting…                            active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   ⠋ permanently deleting “Parser streaming cleanup v2”…
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   updating session…
    |}];
  Tui.keys t "d";
  Tui.settle t;
  Tui.finish_lifecycle_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  Parser streaming cleanup v2          deleted   idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   / filter · esc back
    |}]

(* A client-side lifecycle failure must replace the pending presentation with
   a visible diagnostic over the retained owner snapshot. The conflicting
   delete key is deliberately sent while archive is held: no confirmation or
   second lifecycle request may escape the pending-state gate. *)
let%expect_test "lifecycle failure stays visible over the unlocked browser" =
  let parser = Session.Id.of_string "session-parser" in
  let denied = unavailable "archive denied by another driver" in
  Tui.run ~name:"sessions-lifecycle-failure" ~home:home_is_project
    ~sessions:browser_rows
    ~lifecycle_results:
      [ Tui.Lifecycle_script.archive ~session:parser (Error denied) ]
    ~hold_lifecycle_results:true
  @@ fun t ->
  open_browser t;
  Tui.keys t "a";
  Tui.settle t;
  Tui.keys t "d";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  archiving…                           active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   ⠋ archiving “Parser streaming cleanup”…
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   updating session…
    |}];
  Tui.finish_lifecycle_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |   ! archive denied by another driver
    04 |    session                              lifecycle phase   turns updated  recency
    05 | ❯  Parser streaming cleanup             active    idle  0 turns just now today
    06 |    Archived protocol notes              archived  idle  0 turns just now today
    07 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    08 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}]

(* The first browser query has no rows to retain. Its loading and failure
   screens therefore remain complete modal surfaces, and reopening starts a
   fresh query whose successful owner snapshot recovers the browser. *)
let%expect_test "initial listing loading failure and recovery stay visual" =
  let results = [ Error (unavailable "session index unavailable"); Ok () ] in
  Tui.run ~name:"sessions-list-recovery" ~home:home_is_project
    ~sessions:browser_rows ~screen_session_results:results
    ~hold_screen_session_results:true
  @@ fun t ->
  open_browser t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ──────────────────────────────────────────────────────────────────────
    02 |
    03 |   ⠋ loading sessions…
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
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   esc back
    |}];
  Tui.finish_screen_session_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ──────────────────────────────────────────────────────────────────────
    02 |
    03 |   ! session index unavailable
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
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   esc back
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  open_browser t;
  Tui.finish_screen_session_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  Parser streaming cleanup             active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}]

(* A post-mutation listing error cannot pretend that the retained rows were
   refreshed. It keeps the last successful snapshot visible and unlocks its
   actions, while the diagnostic explains why the new archived owner state is
   not projected yet. *)
let%expect_test "failed lifecycle refresh retains the last complete listing" =
  let results =
    [ Ok (); Error (unavailable "refreshed session index unavailable") ]
  in
  Tui.run ~name:"sessions-refresh-failure" ~home:home_is_project
    ~sessions:browser_rows ~screen_session_results:results
  @@ fun t ->
  open_browser t;
  Tui.keys t "a";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |   ! refreshed session index unavailable
    04 |    session                              lifecycle phase   turns updated  recency
    05 | ❯  Parser streaming cleanup             active    idle  0 turns just now today
    06 |    Archived protocol notes              archived  idle  0 turns just now today
    07 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    08 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}]

(* Closing a loading browser does not cancel the already-issued result. The
   second query completes first; the older failure is then delivered and must
   leave every cell of the current complete frame unchanged. *)
let%expect_test "stale listing failure cannot replace the reopened browser" =
  let results = [ Error (unavailable "stale listing must be inert"); Ok () ] in
  Tui.run ~name:"sessions-stale-listing" ~home:home_is_project
    ~sessions:browser_rows ~screen_session_results:results
    ~hold_screen_session_results:true
  @@ fun t ->
  open_browser t;
  Tui.keys t Key.escape;
  Tui.settle t;
  open_browser t;
  Tui.finish_latest_screen_session_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  Parser streaming cleanup             active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}];
  Tui.finish_screen_session_result t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  sessions ─────────────────────────────────────────────────────────── 4 sessions
    02 |
    03 |    session                              lifecycle phase   turns updated  recency
    04 | ❯  Parser streaming cleanup             active    idle  0 turns just now today
    05 |    Archived protocol notes              archived  idle  0 turns just now today
    06 |    Authentication dialog polish         active    idle  0 turns 1m ago   today
    07 |    Deleted scratch experiment           deleted   idle  0 turns 1m ago   today
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |   cwd ~
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · f fork · r rename · a archive · d delete · / filter · esc back
    |}]
