(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Notifications, reducer-side. The decision is the unit under test: given
   a launch-fixed policy and the feed, the reducer fires the policy gate
   (enabled, event in [notify.on], focus per [notify.when]) and emits one
   resolved Notify command. The [command] channel is observed through a fake
   Runtime.Local.notify hook that captures the runtime's dispatch. The
   in-terminal channels (bell, OSC 9/777) emit via Mosaic commands straight to
   the terminal, so they are not observable through the hook here. Terminal focus
   starts unfocused (no focus event seen), so [Unfocused] fires — the
   notify-always fallback; a focus event injected as the DEC-1004 escape flips it
   and suppresses the notification. *)

open Tui_harness
module App = Mentat_tui.App
module Config = Mentat_config
module Session = Mentat_session
module Llm = Mentat_llm
module Permission = Mentat_permission
module Json = Jsont.Json

let hello_turn =
  Tui.Turn_script.complete ~prompt:"say hello"
    "Hello from the scripted provider."

(* A session already parked at a decision request: resuming it replays the
   Decision_requested fact through the feed, which is the reducer's parked-turn
   notification trigger. Modeled on test_question. *)
let provider = Llm.Provider.make "openai"

let model =
  Llm.Model.make ~provider ~api:(Llm.Model.Api.make "responses") ~id:"gpt-5.5"

let contract =
  Session.Contract.make ~mode:Session.Contract.Mode.Build ~model
    ~declarations:[] ~policy:Permission.Policy.default
    ~review:Permission.Review_behavior.Enforce
    ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
    ()

let parked_session project =
  let turn_id = Session.Turn.Id.of_string "turn-question" in
  let turn =
    Session.Turn.make ~id:turn_id ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text "pick a place")
      ~max_steps:100 ~contract ()
  in
  let call =
    Llm.Tool.Call.make ~id:"call-question" ~name:"ask_user"
      ~input:(Json.object' []) ()
  in
  let provider_request =
    Session.Provider_request.Started.make ~turn:turn_id
      ~request_digest:(Mentat_digest.string "notify decision")
  in
  let response =
    Llm.Response.make ~model ~stop:Llm.Response.Stop.tool_call
      (Llm.Message.Assistant.make [ Llm.Message.Assistant.tool_call call ])
  in
  let question =
    match Session.Question.make ~prompt:"Where?" ~choices:[ "a"; "b" ] () with
    | Ok q -> q
    | Error e -> failwith (Session.Question.Error.message e)
  in
  let requested =
    Session.Decision.Requested.make ~turn:turn_id ~call
      ~stage:Mentat_tool.Stage.Direct
      (Session.Decision.Request.Question question)
  in
  let at = Session.Time.of_unix_ms 1_000_000L in
  let metadata =
    Session.Metadata.make ~title:"decision"
      ~cwd:(Lpath.Abs.of_string_exn (Project.root project))
      ~created_at:at ~updated_at:at ()
  in
  match
    Session.make
      ~id:(Session.Id.of_string "session-question")
      ~metadata
      ~events:
        [
          Session.Event.turn_started turn;
          Session.Event.provider_requested provider_request;
          Session.Event.provider_settled
            (Session.Provider_request.Settled.responded
               ~id:(Session.Provider_request.Started.id provider_request)
               response);
          Session.Event.decision_requested requested;
        ]
  with
  | Ok session -> session
  | Error e -> failwith (Session.Error.message e)

let reach_transcript t =
  Tui.settle t;
  Tui.keys t "say hello";
  Tui.enter t;
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t

let policy ?(enabled = true) ?(on = [ Config.Notify.Event.Turn_done ])
    ?(channel = Config.Notify.Channel.Command) () =
  {
    App.notify_enabled = enabled;
    notify_channel = channel;
    notify_focus = Config.Notify.When.Unfocused;
    notify_on = on;
  }

(* Drive one completed turn under [notify_policy] with a capturing hook and print
   what the runtime dispatched, or "(no notification)". *)
let drive ~name ~notify_policy () =
  let captured = ref [] in
  Tui.run ~name ~notify_policy
    ~notify:(fun ~title ~body ->
      captured := Printf.sprintf "%s | %s" title body :: !captured)
    ~turns:[ hello_turn ]
  @@ fun t ->
  reach_transcript t;
  match List.rev !captured with
  | [] -> print_endline "(no notification)"
  | lines -> List.iter print_endline lines

let%expect_test "a finished turn notifies the command hook when unfocused" =
  drive ~name:"notify-fires" ~notify_policy:(policy ()) ();
  [%expect {| mentat — mentat-tui-notia8975447 | Turn finished |}]

let%expect_test "an event outside notify.on does not notify" =
  drive ~name:"notify-event-filter"
    ~notify_policy:(policy ~on:[ Config.Notify.Event.Decision ] ())
    ();
  [%expect {| (no notification) |}]

let%expect_test "a disabled policy does not notify" =
  drive ~name:"notify-disabled" ~notify_policy:(policy ~enabled:false ()) ();
  [%expect {| (no notification) |}]

let%expect_test "a focused terminal suppresses an unfocused-only notification" =
  let captured = ref [] in
  Tui.run ~name:"notify-focused" ~notify_policy:(policy ())
    ~notify:(fun ~title ~body ->
      captured := Printf.sprintf "%s | %s" title body :: !captured)
    ~turns:[ hello_turn ]
  @@ fun t ->
  Tui.settle t;
  (* DEC-1004 focus-in: the parser decodes it to a Focus event, which the reducer
     folds via Sub.on_focus into terminal_focus true. *)
  Tui.keys t "\027[I";
  Tui.settle t;
  Tui.keys t "say hello";
  Tui.enter t;
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t;
  (match List.rev !captured with
  | [] -> print_endline "(no notification)"
  | lines -> List.iter print_endline lines);
  [%expect {| (no notification) |}]

let%expect_test "a parked decision notifies the command hook" =
  let captured = ref [] in
  Tui.run ~name:"notify-decision"
    ~home:(fun _ -> None)
    ~notify_policy:(policy ~on:[ Config.Notify.Event.Decision ] ())
    ~notify:(fun ~title ~body ->
      captured := Printf.sprintf "%s | %s" title body :: !captured)
    ~sessions:(fun project -> [ parked_session project ])
  @@ fun t ->
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  (match List.rev !captured with
  | [] -> print_endline "(no notification)"
  | lines -> List.iter print_endline lines);
  [%expect
    {| mentat — mentat-tui-notify-44d8cae7 | Waiting for your response |}]

let%expect_test "a bell/OSC-only channel emits nothing to the command hook" =
  (* Auto resolves to bell + OSC 9, both seamed on Gap A; with no command channel
     the hook is never called, proving the reducer resolved the channel and did
     not spuriously route to the command hook. *)
  drive ~name:"notify-auto"
    ~notify_policy:(policy ~channel:Config.Notify.Channel.Auto ())
    ();
  [%expect {| (no notification) |}]
