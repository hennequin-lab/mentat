(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Json = Jsont.Json
module Llm = Mentat_llm
module Permission = Mentat_permission
module Session = Mentat_session
module Tool = Mentat_tool

let provider = Llm.Provider.make "openai"
let api = Llm.Model.Api.make "responses"
let model = Llm.Model.make ~provider ~api ~id:"gpt-5.5"

let contract =
  Session.Contract.make ~mode:Session.Contract.Mode.Build ~model
    ~declarations:[] ~policy:Permission.Policy.default
    ~review:Permission.Review_behavior.Enforce
    ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
    ()

let review request =
  let reasons =
    Permission.Request.normalized_accesses request
    |> List.map (fun access -> (access, Permission.Policy.Review.Unmatched))
  in
  match Permission.Policy.Review.restore request reasons with
  | Ok review -> review
  | Error _ -> failwith "invalid permission-review fixture"

let pending_session project ~title ~prompt ~tool_name request =
  let turn_id = Session.Turn.Id.of_string ("turn-" ^ tool_name) in
  let turn =
    Session.Turn.make ~id:turn_id ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text prompt)
      ~max_steps:100 ~contract ()
  in
  let call =
    Llm.Tool.Call.make ~id:("call-" ^ tool_name) ~name:tool_name
      ~input:(Json.object' []) ()
  in
  let provider_request =
    Session.Provider_request.Started.make ~turn:turn_id
      ~request_digest:(Mentat_digest.string ("permission visual " ^ tool_name))
  in
  let response =
    Llm.Response.make ~model ~stop:Llm.Response.Stop.tool_call
      (Llm.Message.Assistant.make [ Llm.Message.Assistant.tool_call call ])
  in
  let requested =
    Session.Decision.Requested.make ~turn:turn_id ~call ~stage:Tool.Stage.Direct
      (Session.Decision.Request.Permission (review request))
  in
  let events =
    [
      Session.Event.turn_started turn;
      Session.Event.provider_requested provider_request;
      Session.Event.provider_settled
        (Session.Provider_request.Settled.responded
           ~id:(Session.Provider_request.Started.id provider_request)
           response);
      Session.Event.decision_requested requested;
    ]
  in
  let created_at = Session.Time.of_unix_ms 1_000_000L in
  let metadata =
    Session.Metadata.make ~title
      ~cwd:(Lpath.Abs.of_string_exn (Project.root project))
      ~created_at ~updated_at:created_at ()
  in
  match
    Session.make
      ~id:(Session.Id.of_string ("session-permission-" ^ tool_name))
      ~metadata ~events
  with
  | Ok session -> session
  | Error error -> failwith (Session.Error.message error)

let open_pending t =
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let no_home = Fun.const None

let command_request project =
  let cwd = Permission.Access.Path_scope.unknown (Project.root project) in
  let command = "git status --short && dune build @check" in
  let access =
    Permission.Access.shell ~cwd ~execution:Permission.Access.Command.Direct
      command
  in
  Permission.Request.of_accesses ~source:"shell" ~display:command [ access ]

let%expect_test
    "command permission selection and exact approval remain whole-screen" =
  Tui.run ~name:"permission-command" ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Command permission"
          ~prompt:"inspect the workspace" ~tool_name:"shell"
          (command_request project);
      ])
    ~decision_answers:
      [
        Session.Decision.Answer.Permission
          { answer = Permission.Answer.exact_for_conversation; message = None };
      ]
  @@ fun t ->
  open_pending t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permission8b592900.home/mentat…
04 |
05 | ❯ inspect the workspace
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    permission
15 |
16 |   Run this command?
17 |   $ git status --short && dune build @check                                    ▀
18 |   access · command
19 | ❯ 1. Yes, run once
20 |   2. Yes, allow this exact command for this conversation
21 |   3. No, deny
22 |   4. No, deny and tell the model what to do differently
23 |
24 |   1-4 select · ↵ choose · ^O more|}];
  Tui.keys t "2";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permission8b592900.home/mentat…
04 |
05 | ❯ inspect the workspace
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    permission
15 |
16 |   Run this command?
17 |   $ git status --short && dune build @check                                    ▀
18 |   access · command
19 |   1. Yes, run once
20 | ❯ 2. Yes, allow this exact command for this conversation
21 |   3. No, deny
22 |   4. No, deny and tell the model what to do differently
23 |
24 |   1-4 select · ↵ choose · ^O more|}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permission8b592900.home/mentat…
04 |
05 | ❯ inspect the workspace
06 |
07 | ⠋ Working… (0s · esc to interrupt)
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
22 | ❯ queue a message — sends after this turn
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /tmp/mentat-tu… · openai/gp… · ! full access ? f…|}]

let%expect_test "decision failure re-enables the exact open dialog" =
  let answer =
    Session.Decision.Answer.Permission
      { answer = Permission.Answer.exact_for_conversation; message = None }
  in
  Tui.run ~name:"permission-retry-resolution" ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Retry decision"
          ~prompt:"retry exact answer" ~tool_name:"shell"
          (command_request project);
      ])
    ~decision_answers:[ answer; answer ] ~hold_decision_resolution:true
  @@ fun t ->
  open_pending t;
  Tui.keys t "2";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.fail_decision t
    (Mentat_protocol.Error.Unavailable
       (Mentat_diagnostic.of_text "answer unavailable"));
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permission-retry-reb5117737.ho…
04 |
05 | ❯ retry exact answer
06 |
07 | ✗ answer unavailable
08 |   retry when ready
09 |
10 | ⋯ Waiting for your answer
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    permission
15 |
16 |   Run this command?
17 |   $ git status --short && dune build @check                                    ▀
18 |   access · command
19 |   1. Yes, run once
20 | ❯ 2. Yes, allow this exact command for this conversation
21 |   3. No, deny
22 |   4. No, deny and tell the model what to do differently
23 |
24 |   1-4 select · ↵ choose · ^O more|}];
  Tui.enter t;
  Tui.settle t;
  Tui.acknowledge_decision t;
  Tui.resolve_decision t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permission-retry-reb5117737.ho…
04 |
05 | ❯ retry exact answer
06 |
07 | ✗ answer unavailable
08 |   retry when ready
09 |
10 | ⠋ Working… (0s · esc to interrupt)
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
22 | ❯ queue a message — sends after this turn
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /tmp/mentat-tu… · openai/gpt… · ! full access ? …|}]

let%expect_test "decision acknowledgement waits for durable resolution" =
  Tui.run ~name:"permission-held-resolution" ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Held decision"
          ~prompt:"inspect before settling" ~tool_name:"shell"
          (command_request project);
      ])
    ~decision_answers:
      [
        Session.Decision.Answer.Permission
          { answer = Permission.Answer.exact_for_conversation; message = None };
      ]
    ~hold_decision_resolution:true
  @@ fun t ->
  open_pending t;
  Tui.keys t "2";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.acknowledge_decision t;
  Tui.settle t;
  Tui.keys t "1";
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permission-held-rebbaf9c0c.hom…
04 |
05 | ❯ inspect before settling
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    permission
15 |
16 |   Run this command?
17 |   $ git status --short && dune build @check                                    ▀
18 |   access · command
19 |   1. Yes, run once
20 | ❯ 2. Yes, allow this exact command for this conversation
21 |   3. No, deny
22 |   4. No, deny and tell the model what to do differently
23 |
24 |   1-4 select · ↵ choose · ^O more|}];
  Tui.resolve_decision t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permission-held-rebbaf9c0c.hom…
04 |
05 | ❯ inspect before settling
06 |
07 | ⠋ Working… (0s · esc to interrupt)
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
22 | ❯ queue a message — sends after this turn
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /tmp/mentat-tu… · openai/gpt… · ! full access ? …|}]

let diff first last =
  let middle =
    List.init 12 (fun index ->
        Printf.sprintf "+let visual_line_%02d = %d" (index + 1) index)
  in
  String.concat "\n"
    ([ "--- " ^ first; "+++ " ^ last; "@@ -0,0 +1,12 @@" ] @ middle)

let change ?removals ~path () =
  Permission.Request.Change.make
    ~diff:(diff (path ^ ".before") path)
    ~additions:12 ?removals ()

let path_request () =
  let parser = Permission.Access.unknown_path ~op:`Modify "lib/parser.ml" in
  let lexer = Permission.Access.unknown_path ~op:`Create "lib/lexer.ml" in
  Permission.Request.make ~source:"apply_patch"
    [
      Permission.Request.Item.make ~display:"lib/parser.ml"
        ~change:(change ~path:"lib/parser.ml" ~removals:2 ())
        parser;
      Permission.Request.Item.make ~display:"lib/lexer.ml"
        ~change:(change ~path:"lib/lexer.ml" ())
        lexer;
    ]

let%expect_test
    "multi-path evidence expands, wraps selection, and denies by mnemonic" =
  Tui.run ~name:"permission-path" ~size:(80, 24) ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Patch permission"
          ~prompt:"apply the parser patch" ~tool_name:"apply_patch"
          (path_request ());
      ])
    ~decision_answers:
      [
        Session.Decision.Answer.Permission
          { answer = Permission.Answer.deny; message = None };
      ]
  @@ fun t ->
  open_pending t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permiss1f5e42a5.home/mentat-tu…
04 |
05 | ❯ apply the parser patch
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    permission
15 |
16 |   Apply these changes?
17 |   2 changes · +24 −?
18 |   item 1/2 · lib/parser.ml · +12 −2                                            ▀
19 | ❯ 1. Yes, apply these changes
20 |   2. Yes, allow these exact accesses for this conversation
21 |   3. No, deny
22 |   4. No, deny and tell the model what to do differently
23 |
24 |   1-4 select · ↵ choose · ^O more|}];
  Tui.keys t Key.ctrl_o;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permiss1f5e42a5.home/mentat-tu…
04 |
05 | ❯ apply the parser patch
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    permission
15 |
16 |   item 1/2 · lib/parser.ml · +12 −2                                            ▀
17 |   item 2/2 · lib/lexer.ml · +12 −?
18 |   1 + let visual_line_01 = 0
19 | ❯ 1. Yes, apply these changes
20 |   2. Yes, allow these exact accesses for this conversation
21 |   3. No, deny
22 |   4. No, deny and tell the model what to do differently
23 |
24 |   1-4 select · ↵ choose · ^O less|}];
  Tui.keys t Key.up;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permiss1f5e42a5.home/mentat-tu…
04 |
05 | ❯ apply the parser patch
06 |
07 | ⋯ Waiting for your answer
08 |
09 |
10 |
11 |
12 |
13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
14 |    permission
15 |
16 |   item 1/2 · lib/parser.ml · +12 −2                                            ▀
17 |   item 2/2 · lib/lexer.ml · +12 −?
18 |   1 + let visual_line_01 = 0
19 |   1. Yes, apply these changes
20 |   2. Yes, allow these exact accesses for this conversation
21 |   3. No, deny
22 | ❯ 4. No, deny and tell the model what to do differently
23 |
24 |   1-4 select · ↵ choose · ^O less|}];
  Tui.keys t "n";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  /tmp/mentat-tui-permiss1f5e42a5.home/mentat-tu…
04 |
05 | ❯ apply the parser patch
06 |
07 | ⠋ Working… (0s · esc to interrupt)
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
22 | ❯ queue a message — sends after this turn
23 | ────────────────────────────────────────────────────────────────────────────────
24 |   ! not logged in · /login · /tmp/mentat-t… · openai/gpt… · ! full access ? f…|}]

let network_custom_request () =
  let network =
    Permission.Access.network ~protocol:`Https ~host:"api.example.test\027[31m"
      ~port:8443 ()
  in
  let custom =
    Permission.Access.custom ~subject:"\r\n\027[2J deployment token" "\u{200D}"
  in
  Permission.Request.make ~source:"\u{200D}"
    ~display:
      "Connect to the deployment services\r\n\
       without allowing terminal controls to escape."
    [
      Permission.Request.Item.make
        ~display:"primary deployment endpoint\r\nwith a wrapped explanation"
        network;
      Permission.Request.Item.make custom;
    ]

let%expect_test
    "network/custom evidence stays inert and controls stay legible when narrow"
    =
  Tui.run ~name:"permission-network-custom" ~size:(40, 22) ~home:no_home
    ~sessions:(fun project ->
      [
        pending_session project ~title:"Network permission"
          ~prompt:"contact deployment services" ~tool_name:"network_access"
          (network_custom_request ());
      ])
    ~decision_answers:
      [
        Session.Decision.Answer.Permission
          { answer = Permission.Answer.once; message = None };
      ]
  @@ fun t ->
  open_pending t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
04 |  dev · openai/gpt-5.5 medium
05 |  /tmp/mentat-tui-permission-networ8e2b…
06 |
07 | ❯ contact deployment services
08 |
09 | ⋯ Waiting for your answer
10 |
11 |
12 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
13 |    permission
14 |
15 |   Allow these accesses?
16 |   Connect to the deployment services   ▀
17 | ❯ 1. Yes, allow once
18 |   2. Yes, allow these exact accesses ...
19 |   3. No, deny
20 |   4. No, deny and tell the model what...
21 |
22 |   1-4 select · ↵ choose · ^O more|}];
  Tui.keys t "y";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
04 |  dev · openai/gpt-5.5 medium
05 |  /tmp/mentat-tui-permission-networ8e2b…
06 |
07 | ❯ contact deployment services
08 |
09 | ⠋ Working… (0s · esc to interrupt)
10 |
11 |
12 |
13 |
14 |
15 |
16 |
17 |
18 |
19 | ────────────────────────────────────────
20 | ❯ queue a message — sends after this tur
21 | ────────────────────────────────────────
22 |   ! not logged in · /l… · ! full acc…|}]

[%%run_tests "mentat.tui.permission"]
