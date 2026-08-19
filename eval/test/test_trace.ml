(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Coverage for [Mentat_eval_trace.Trace.of_session], the reconstruction of a
   run's ordered, usage-attributed view from a persisted session document.

   Every fixture is a real {!Mentat_session.t} built through the current,
   replay-validated event API (turn openers/closers, provider and tool claims,
   compaction), so the reconstruction is exercised against exactly the event
   vocabulary production writes. Where the derivations overlap the session's own
   {!Mentat_session.Metrics}, the two are cross-checked so the trace's
   classification of executed / failed / rejected calls cannot silently drift
   from the journal's. *)

open Windtrap
module Session = Mentat_session
module Event = Session.Event
module Llm = Mentat_llm
module Tool = Mentat_tool
module Permission = Mentat_permission
module Sandbox = Mentat_sandbox
module Json = Jsont.Json
module Trace = Mentat_eval_trace.Trace
module Metrics = Mentat_eval_trace.Trace_metrics

(* Base fixtures. *)

let provider = Llm.Provider.make "openai"
let api = Llm.Model.Api.make "responses"
let model = Llm.Model.make ~provider ~api ~id:"gpt-5"
let cwd = Lpath.Abs.of_string_exn "/workspace"
let time ms = Session.Time.of_unix_ms (Int64.of_int ms)
let digest seed = Mentat_digest.string seed
let usage ~input ~output = Llm.Usage.make ~input ~output ()
let sandbox_identity = Sandbox.identity Sandbox.direct

let tool_call ?(id = "call-1") ?(name = "read_file") ?(input = Json.object' [])
    () =
  Llm.Tool.Call.make ~id ~name ~input ()

let assistant_calls calls =
  Llm.Message.Assistant.make (List.map Llm.Message.Assistant.tool_call calls)

let declaration name =
  Llm.Tool.make ~name ~input_schema:Llm.Tool.no_input_schema ()

let output text = Tool.Output.make ~text ()
let completed text = Tool.Result.completed ~output:(output text) ()

let make_contract ?(declarations = []) ?options () =
  Session.Contract.make ~mode:Session.Contract.Mode.Build ~model ?options
    ~declarations ~policy:Permission.Policy.default
    ~review:Permission.Review_behavior.Enforce ~sandbox:sandbox_identity ()

let turn ?(id = "turn-1") ?(input = Session.Turn.Input.user_text "Do it.")
    ?contract () =
  let contract =
    match contract with Some contract -> contract | None -> make_contract ()
  in
  Session.Turn.make
    ~id:(Session.Turn.Id.of_string id)
    ~origin:Session.Turn.Origin.User ~input ~max_steps:32 ~contract ()

let claim ?(seed = "req-1") turn_id =
  Session.Provider_request.Started.make ~turn:turn_id
    ~request_digest:(digest seed)

let claim_id claim = Session.Provider_request.Started.id claim

let settle_responded ?usage claim assistant =
  Event.provider_settled
    (Session.Provider_request.Settled.responded ~id:(claim_id claim)
       (Llm.Response.make ~model ?usage assistant))

let respond ?usage claim text =
  settle_responded ?usage claim (Llm.Message.Assistant.text text)

let settle_interrupted claim text =
  Event.provider_settled
    (Session.Provider_request.Settled.interrupted ~id:(claim_id claim) ~text ())

let tool_claim ~turn_id call =
  Session.Tool_claim.Started.make ~turn:turn_id ~stage:Tool.Stage.Direct ~call
    ~input:(Json.object' []) ~requests:[]

let tool_claim_id claim = Session.Tool_claim.Started.id claim

let settled_returned claim result =
  Event.tool_settled
    (Session.Tool_claim.Settled.returned ~id:(tool_claim_id claim) result)

let finish ?(outcome = Session.Turn.Outcome.completed) t =
  Event.turn_finished ~turn:(Session.Turn.id t) outcome

let metadata =
  Session.Metadata.make ~cwd ~created_at:(time 1) ~updated_at:(time 2) ()

let saved ?(id = "session-1") events =
  match Session.make ~id:(Session.Id.of_string id) ~metadata ~events with
  | Ok session -> session
  | Error error ->
      failf "session build failed: %s" (Session.Error.message error)

(* Both the trace and the journal's own metrics come from one session, so their
   counts can be cross-checked against each other. *)
let analyze events =
  let session = saved events in
  (Trace.of_session session, Session.metrics session)

let status_of = function
  | Trace.Call.Ok -> "ok"
  | Trace.Call.Failed -> "failed"
  | Trace.Call.Rejected -> "rejected"

let count_status trace status =
  List.length
    (List.filter
       (fun call -> Trace.Call.status call = status)
       (Trace.calls trace))

(* A step is one provider response; two settled responses in one turn are two
   steps, each attributed its own usage. *)
let two_step_turn () =
  let t = turn () in
  let tid = Session.Turn.id t in
  let p1 = claim ~seed:"s1" tid in
  let p2 = claim ~seed:"s2" tid in
  let trace, metrics =
    analyze
      [
        Event.turn_started t;
        Event.provider_requested p1;
        respond ~usage:(usage ~input:12 ~output:7) p1 "step one";
        Event.provider_requested p2;
        respond ~usage:(usage ~input:30 ~output:9) p2 "step two";
        finish t;
      ]
  in
  let steps = Trace.steps trace in
  equal int ~msg:"two steps" 2 (List.length steps);
  equal int ~msg:"no calls" 0 (List.length (Trace.calls trace));
  equal int ~msg:"one segment" 1 (List.length (Trace.segments trace));
  (match steps with
  | [ s0; s1 ] ->
      equal (option int) ~msg:"step 0 input tokens" (Some 12)
        (Option.map (fun u -> u.Llm.Usage.input) (Trace.Step.usage s0));
      equal (option int) ~msg:"step 1 output tokens" (Some 9)
        (Option.map (fun u -> u.Llm.Usage.output) (Trace.Step.usage s1))
  | _ -> fail "expected exactly two steps");
  equal (option string) ~msg:"single model recovered" (Some "gpt-5")
    (Option.map Llm.Model.id (Trace.model trace));
  equal int ~msg:"trace steps = journal responses" (List.length steps)
    metrics.Session.Metrics.responses

(* A claimed tool call whose settlement returned joins to the response call by
   claim id; the tool-domain result is lowered to a model answer whose error
   flag decides Ok vs Failed. *)
let claimed_returned_calls () =
  let c_read =
    tool_call ~id:"call-read" ~name:"read_file"
      ~input:(Json.object' [ (Json.name "path", Json.string "lib/a.ml") ])
      ()
  in
  let c_bad = tool_call ~id:"call-bad" ~name:"search_text" () in
  let contract =
    make_contract
      ~declarations:[ declaration "read_file"; declaration "shell" ]
      ()
  in
  let t = turn ~contract () in
  let tid = Session.Turn.id t in
  let p1 = claim ~seed:"r1" tid in
  let p2 = claim ~seed:"r2" tid in
  let read_claim = tool_claim ~turn_id:tid c_read in
  let bad_claim = tool_claim ~turn_id:tid c_bad in
  let trace, metrics =
    analyze
      [
        Event.turn_started t;
        Event.provider_requested p1;
        settle_responded p1 (assistant_calls [ c_read; c_bad ]);
        Event.tool_claimed read_claim;
        settled_returned read_claim (completed "file contents");
        Event.tool_claimed bad_claim;
        settled_returned bad_claim (Tool.Result.failed `Not_found "no match");
        Event.provider_requested p2;
        respond p2 "done";
        finish t;
      ]
  in
  equal int ~msg:"two calls" 2 (List.length (Trace.calls trace));
  (match Trace.calls trace with
  | [ read; bad ] ->
      equal string ~msg:"read name" "read_file" (Trace.Call.name read);
      equal string ~msg:"read ok" "ok" (status_of (Trace.Call.status read));
      equal string ~msg:"read result text" "file contents"
        (Trace.Call.result_text read);
      equal (option string) ~msg:"read path projected" (Some "lib/a.ml")
        (Trace.Call.read_path read);
      equal string ~msg:"bad name" "search_text" (Trace.Call.name bad);
      equal string ~msg:"bad failed" "failed"
        (status_of (Trace.Call.status bad))
  | _ -> fail "expected exactly two calls");
  equal (list string) ~msg:"declared tools recovered" [ "read_file"; "shell" ]
    (Trace.declared_tools trace);
  (* Journal cross-check: two returned calls, one of them a failure. *)
  equal int ~msg:"trace ok+failed = journal tool_calls"
    (count_status trace Trace.Call.Ok + count_status trace Trace.Call.Failed)
    metrics.Session.Metrics.tool_calls;
  equal int ~msg:"trace failed = journal tool_failures"
    (count_status trace Trace.Call.Failed)
    metrics.Session.Metrics.tool_failures

(* A tool result appended without a claim answers its response call as a
   rejection, distinct from an executed failure. *)
let rejected_unclaimed_call () =
  let c_reject = tool_call ~id:"call-reject" ~name:"write_file" () in
  let t = turn () in
  let tid = Session.Turn.id t in
  let p1 = claim ~seed:"j1" tid in
  let p2 = claim ~seed:"j2" tid in
  let trace, metrics =
    analyze
      [
        Event.turn_started t;
        Event.provider_requested p1;
        settle_responded p1 (assistant_calls [ c_reject ]);
        Event.message_appended
          (Llm.Message.tool_result
             (Llm.Tool.Result.text ~error:true c_reject "permission denied"));
        Event.provider_requested p2;
        respond p2 "acknowledged";
        finish t;
      ]
  in
  equal int ~msg:"one call" 1 (List.length (Trace.calls trace));
  (match Trace.calls trace with
  | [ call ] ->
      equal string ~msg:"reject name" "write_file" (Trace.Call.name call);
      equal string ~msg:"rejected status" "rejected"
        (status_of (Trace.Call.status call))
  | _ -> fail "expected exactly one call");
  equal int ~msg:"no executed tool calls" 0 metrics.Session.Metrics.tool_calls;
  equal int ~msg:"trace rejected = journal tool_rejections"
    (count_status trace Trace.Call.Rejected)
    metrics.Session.Metrics.tool_rejections

(* An interrupted provider settlement appends no response and is not a step. *)
let interrupted_turn_has_no_step () =
  let t = turn () in
  let tid = Session.Turn.id t in
  let p1 = claim ~seed:"i1" tid in
  let trace, metrics =
    analyze
      [
        Event.turn_started t;
        Event.provider_requested p1;
        Event.interrupt_requested ~turn:tid ~reason:"user stop" ();
        settle_interrupted p1 "stopping here";
        finish
          ~outcome:
            (Session.Turn.Outcome.interrupted ~reason:"user stop"
               ~cancelled:true ())
          t;
      ]
  in
  equal int ~msg:"no steps" 0 (List.length (Trace.steps trace));
  equal int ~msg:"no calls" 0 (List.length (Trace.calls trace));
  equal int ~msg:"journal has no responses either" 0
    metrics.Session.Metrics.responses

(* A compaction installs a segment boundary. The trace reconstructs from the
   full persisted event log, so both the pre- and post-compaction responses
   remain visible as steps in successive segments — the compacted model view
   would drop the pre-compaction detail. *)
let compaction_spans_the_full_log () =
  let contract = make_contract ~declarations:[ declaration "read_file" ] () in
  let t = turn ~contract () in
  let tid = Session.Turn.id t in
  let p1 = claim ~seed:"k1" tid in
  let p_summary = claim ~seed:"k-summary" tid in
  let p2 = claim ~seed:"k2" tid in
  let compaction =
    Session.Compaction.make ~reason:Session.Compaction.Reason.Context_pressure
      ~provider_claim:(claim_id p_summary) ~request_digest:(digest "k-summary")
      ~summary_messages:[ Llm.Message.user_text "summary so far" ]
      ~summarized_upto:0
      ~context:(Session.Compaction.Context_tokens.make ~before:900 ~after:80 ())
      ()
  in
  let trace, _metrics =
    analyze
      [
        Event.turn_started t;
        Event.provider_requested p1;
        respond ~usage:(usage ~input:100 ~output:10) p1 "before compaction";
        Event.provider_requested p_summary;
        Event.compaction_installed compaction;
        Event.provider_requested p2;
        respond ~usage:(usage ~input:40 ~output:5) p2 "after compaction";
        finish t;
      ]
  in
  let steps = Trace.steps trace in
  equal int ~msg:"both responses survive as steps" 2 (List.length steps);
  equal int ~msg:"two segments" 2 (List.length (Trace.segments trace));
  (match steps with
  | [ s0; s1 ] ->
      equal int ~msg:"pre-compaction step in segment 0" 0
        (Trace.Step.segment_index s0);
      equal int ~msg:"post-compaction step in segment 1" 1
        (Trace.Step.segment_index s1)
  | _ -> fail "expected exactly two steps");
  match Trace.segments trace with
  | [ [ _ ]; [ _ ] ] -> ()
  | _ -> fail "expected one step in each of two segments"

(* Trace metrics are derivable from a reconstructed trace without error. *)
let trace_metrics_derive () =
  let c_read = tool_call ~id:"call-read" ~name:"read_file" () in
  let t = turn () in
  let tid = Session.Turn.id t in
  let p1 = claim ~seed:"m1" tid in
  let read_claim = tool_claim ~turn_id:tid c_read in
  let trace, _ =
    analyze
      [
        Event.turn_started t;
        Event.provider_requested p1;
        settle_responded
          ~usage:(usage ~input:50 ~output:20)
          p1
          (assistant_calls [ c_read ]);
        Event.tool_claimed read_claim;
        settled_returned read_claim (completed "ok");
        finish t;
      ]
  in
  let m = Metrics.of_trace trace in
  equal int ~msg:"metrics input tokens" 50 m.Metrics.input_tokens;
  equal int ~msg:"metrics output tokens" 20 m.Metrics.output_tokens;
  equal int ~msg:"metrics tool calls" 1 m.Metrics.tool_calls

let () =
  Windtrap.run "mentat.eval.trace"
    [
      test "two-step turn attributes usage per response" two_step_turn;
      test "claimed returned calls classify ok and failed"
        claimed_returned_calls;
      test "unclaimed tool result is a rejection" rejected_unclaimed_call;
      test "interrupted settlement is not a step" interrupted_turn_has_no_step;
      test "compaction segments span the full log" compaction_spans_the_full_log;
      test "trace metrics derive from the reconstruction" trace_metrics_derive;
    ]
