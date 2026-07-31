(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Unit suite for [mentat_mutation], the durable mutation-evidence
   vocabulary. The suite pins the durable-evidence obligations: the
   lowering, fold determinism and rejection with located structured reasons,
   the append and at-claim laws, replay equivalence across an encode/decode
   restart, the revertability rules including the vacuous-Available law,
   netting as a law over generated edit sequences, selection
   canonicalization, preparation problems and the confirm-with-override
   ruling, the Phase-driven settlement classification with its positional
   pairing guards, crash recovery from the frozen record, and the strict
   codecs (unknown tags and members, retired wire shapes, id re-derivation).

   Edit evidence is minted the only honest way — through [Mentat_edit.apply]
   over a pure in-memory IO — so [Event.of_edit] and [Revert.settle] receive
   the same authoritative values production hands them. Invalid durable
   shapes are forged by tampering with encoded JSON — exactly the
   ledger-edit surface strict decoding guards. *)

open Windtrap
open Test_support
module M = Mentat_mutation
module Edit = Mentat_edit
module W = Mentat_workspace
module Session = Mentat_session
module Json = Jsont.Json

(* Workspace and identity fixtures. *)

let rel = Lpath.Rel.of_string_exn
let ws_root = W.Root.of_dir (Lpath.Abs.of_string_exn "/workspace")
let root_key = W.Root.key ws_root
let workspace = W.single ws_root
let wpath text = W.Path.make ~root_key (rel text)
let turn = Session.Turn.Id.of_string
let claim = Session.Tool_claim.Id.of_string
let rid = M.Revert.Id.of_string
let cref = Mentat_digest.Content_ref.of_contents
let text_image contents = M.Image.Text (cref contents)

let image_of = function
  | None -> M.Image.Missing
  | Some contents -> text_image contents

(* Pure in-memory edit IO. *)

let pure_io ?(fail_commit = fun _ -> false) fs =
  {
    Edit.Apply.with_write_lock = (fun _paths f -> f ());
    revalidate = (fun path -> Ok path);
    read =
      (fun path ->
        match List.find_opt (fun (p, _) -> W.Path.equal p path) fs with
        | Some (_, contents) -> Ok (Edit.Observed.Text contents)
        | None -> Ok Edit.Observed.Missing);
    commit =
      (fun ~path ~before:_ ~after:_ ->
        if fail_commit path then
          Error (Edit.Error.io ~path "injected commit failure")
        else Ok ());
  }

let plan_exn = function
  | Ok plan -> plan
  | Error error -> failf "plan construction failed: %a" Edit.Error.pp error

let concat_exn plans =
  match Edit.concat (List.map plan_exn plans) with
  | Ok plan -> plan
  | Error error -> failf "concat failed: %a" Edit.Error.pp error

let apply_ok ?fail_commit ~fs plans =
  match
    Edit.apply ~io:(pure_io ?fail_commit fs) ~workspace (concat_exn plans)
  with
  | Ok result -> result
  | Error error -> failf "apply failed: %a" Edit.Apply_error.pp error

let apply_err ?fail_commit ~fs plans =
  match
    Edit.apply ~io:(pure_io ?fail_commit fs) ~workspace (concat_exn plans)
  with
  | Ok _ -> fail "expected the apply to fail"
  | Error error -> error

let apply_plan_ok ?fail_commit ~fs plan =
  match Edit.apply ~io:(pure_io ?fail_commit fs) ~workspace plan with
  | Ok result -> result
  | Error error -> failf "apply failed: %a" Edit.Apply_error.pp error

let apply_plan_err ?fail_commit ~fs plan =
  match Edit.apply ~io:(pure_io ?fail_commit fs) ~workspace plan with
  | Ok _ -> fail "expected the apply to fail"
  | Error error -> error

(* Event fixtures. *)

let edit_event ?checkpoint ?(ordinal = 0) ~turn:t ~claim:c ~fs plans =
  M.Event.of_edit ~turn:(turn t) ~claim:(claim c) ~ordinal ~checkpoint
    (apply_ok ~fs plans)

let observed_event ~turn:t ~claim:c paths =
  M.Event.tool_observed ~turn:(turn t) ~claim:(claim c) paths

let changes_of = function
  | M.Event.Edit { changes; _ } -> changes
  | _ -> fail "expected an edit event"

let state_exn events =
  match M.State.of_events events with
  | Ok state -> state
  | Error error -> failf "replay failed: %a" M.State.Error.pp error

let state_err events =
  match M.State.of_events events with
  | Ok _ -> fail "expected the replay to reject"
  | Error error -> error

let evidence ?(current = []) ?(blobs = []) () =
  M.Revert.Evidence.make ~current
    ~blobs:(List.map (fun contents -> (cref contents, contents)) blobs)

let prepare_ok ?override ~id ~evidence state selection =
  match M.Revert.prepare ?override ~id ~evidence state selection with
  | Ok plan -> plan
  | Error problems ->
      failf "prepare refused: %a"
        (Format.pp_print_list M.Revert.Problem.pp)
        problems

let prepare_err ?override ~id ~evidence state selection =
  match M.Revert.prepare ?override ~id ~evidence state selection with
  | Ok _ -> fail "expected the preparation to refuse"
  | Error problems -> problems

(* Checkpoint fixtures. *)

let snapshot = M.Checkpoint.Snapshot.make ~backend:"git-tree" ~reference:"cafe"
let available = M.Checkpoint.Capture.Available { snapshot; excluded = 0 }

let degraded =
  M.Checkpoint.Capture.Degraded
    { failure = M.Checkpoint.Capture.Failure.make ~message:"git unavailable" }

let checkpoint ?(capture = available) boundary =
  M.Checkpoint.make ~boundary ~capture

let before_revert plan =
  let id = (M.Revert.Plan.started plan).M.Revert.Started.id in
  M.Event.checkpoint (checkpoint (M.Checkpoint.Before_revert id))

(* Id expectations come from the owning lowerings, never a parallel public
   spelling oracle. *)
let claim_derive ?(ordinal = 0) c path =
  match
    changes_of
      (edit_event ~ordinal ~turn:"id-fixture" ~claim:c ~fs:[]
         [ Edit.create ~path ~contents:"fixture\n" ])
  with
  | [ change ] -> M.Change.id change
  | _ -> fail "expected one lowered change"

let revert_derive r path =
  let contents = "fixture\n" in
  let event =
    edit_event ~turn:"id-fixture" ~claim:"id-source" ~fs:[]
      [ Edit.create ~path ~contents ]
  in
  let source = List.hd (changes_of event) in
  let state = state_exn [ event ] in
  let selection = M.Revert.Selection.changes [ M.Change.id source ] in
  let evidence =
    evidence
      ~current:[ (path, Edit.Observed.Text contents) ]
      ~blobs:[ contents ] ()
  in
  let plan = prepare_ok ~id:(rid r) ~evidence state selection in
  let result =
    apply_plan_ok ~fs:[ (path, contents) ] (M.Revert.Plan.edit plan)
  in
  let settled = M.Revert.settle (M.Revert.Plan.started plan) (Ok result) in
  match settled.M.Revert.Settled.changes with
  | [ change ] -> M.Change.id change
  | _ -> fail "expected one lowered restoration change"

(* Testables. *)

let event_value = Testable.make ~pp:M.Event.pp ~equal:M.Event.equal
let change_value = Testable.make ~pp:M.Change.pp ~equal:M.Change.equal
let change_id_value = Testable.make ~pp:M.Change.Id.pp ~equal:M.Change.Id.equal
let path_value = Testable.make ~pp:W.Path.pp ~equal:W.Path.equal
let image_value = Testable.make ~pp:M.Image.pp ~equal:M.Image.equal

let stats_value =
  let pp ppf (s : Textdiff.stats) =
    Format.fprintf ppf "{files=%d; additions=%d; deletions=%d}" s.Textdiff.files
      s.Textdiff.additions s.Textdiff.deletions
  in
  Testable.make ~pp ~equal:Textdiff.Stats.equal

let selection_value =
  Testable.make ~pp:M.Revert.Selection.pp ~equal:M.Revert.Selection.equal

let revertability_value =
  Testable.make ~pp:M.Revertability.pp ~equal:M.Revertability.equal

let problem_value = Testable.make ~pp:M.Revert.Problem.pp ~equal:( = )

let claim_id_value =
  Testable.make ~pp:Session.Tool_claim.Id.pp ~equal:Session.Tool_claim.Id.equal

let checkpoint_id_value =
  Testable.make ~pp:M.Checkpoint.Id.pp ~equal:M.Checkpoint.Id.equal

let started_value =
  Testable.make
    ~pp:(fun ppf (s : M.Revert.Started.t) ->
      Format.fprintf ppf "started(%a)" M.Revert.Id.pp s.M.Revert.Started.id)
    ~equal:M.Revert.Started.equal

let settled_value =
  Testable.make
    ~pp:(fun ppf (s : M.Revert.Settled.t) ->
      Format.fprintf ppf "settled(%a)" M.Revert.Id.pp s.M.Revert.Settled.revert)
    ~equal:M.Revert.Settled.equal

let net_entry_equal (a : M.Change.Net.entry) (b : M.Change.Net.entry) =
  W.Path.equal a.M.Change.Net.path b.M.Change.Net.path
  && M.Image.equal a.M.Change.Net.before b.M.Change.Net.before
  && M.Image.equal a.M.Change.Net.after b.M.Change.Net.after
  && Bool.equal a.M.Change.Net.contiguous b.M.Change.Net.contiguous
  && List.equal M.Change.Id.equal a.M.Change.Net.sources b.M.Change.Net.sources

let net_entry_value =
  Testable.make
    ~pp:(fun ppf (e : M.Change.Net.entry) ->
      Format.fprintf ppf "net(%a, %a -> %a, contiguous=%b)" W.Path.pp
        e.M.Change.Net.path M.Image.pp e.M.Change.Net.before M.Image.pp
        e.M.Change.Net.after e.M.Change.Net.contiguous)
    ~equal:net_entry_equal

let state_error_value = Testable.make ~pp:M.State.Error.pp ~equal:( = )

(* Two states are equivalent when every projection protocol reads agrees. Each
   claim's change rows carry their turn, so equal per-claim changes prove the
   per-turn grouping equal too. *)
let assert_state_equiv msg a b =
  equal (list event_value) ~msg (M.State.events a) (M.State.events b);
  equal (list claim_id_value) ~msg (M.State.claims a) (M.State.claims b);
  List.iter
    (fun c ->
      equal (list change_value) ~msg
        (M.State.changes a ~claim:c)
        (M.State.changes b ~claim:c);
      equal (list path_value) ~msg
        (M.State.observed a ~claim:c)
        (M.State.observed b ~claim:c))
    (M.State.claims a);
  equal (list started_value) ~msg
    (M.State.unresolved_reverts a)
    (M.State.unresolved_reverts b);
  equal (list net_entry_value) ~msg
    (M.State.net a M.Revert.Selection.all)
    (M.State.net b M.Revert.Selection.all);
  equal revertability_value ~msg
    (M.State.revertability a M.Revert.Selection.all)
    (M.State.revertability b M.Revert.Selection.all)

(* Durable-JSON tampering helpers. *)

let members_of = function
  | Jsont.Object (members, meta) -> (members, meta)
  | _ -> fail "expected a JSON object"

let add_member name value json =
  let members, meta = members_of json in
  Jsont.Object (members @ [ (Json.name name, value) ], meta)

let set_member name value json =
  let members, meta = members_of json in
  let members =
    List.map
      (fun ((n, m), v) ->
        if String.equal n name then ((n, m), value) else ((n, m), v))
      members
  in
  Jsont.Object (members, meta)

let get_member name json =
  let members, _ = members_of json in
  match List.find_opt (fun ((n, _), _) -> String.equal n name) members with
  | Some (_, value) -> value
  | None -> failf "missing %S member" name

let remove_member name json =
  let members, meta = members_of json in
  Jsont.Object
    (List.filter (fun ((n, _), _) -> not (String.equal n name)) members, meta)

let has_member name json =
  let members, _ = members_of json in
  List.exists (fun ((n, _), _) -> String.equal n name) members

let elements_of = function
  | Jsont.Array (elements, meta) -> (elements, meta)
  | _ -> fail "expected a JSON array"

let set_element index value json =
  let elements, meta = elements_of json in
  Jsont.Array
    (List.mapi (fun i v -> if i = index then value else v) elements, meta)

let get_element index json =
  let elements, _ = elements_of json in
  match List.nth_opt elements index with
  | Some value -> value
  | None -> failf "missing array element %d" index

let assert_decode_error ?contains msg codec json =
  match Json.decode codec json with
  | Ok _ -> failf "%s: expected decode error" msg
  | Error actual -> (
      match contains with
      | None -> ()
      | Some fragment ->
          is_true
            ~msg:(msg ^ ": message is " ^ actual)
            (String.includes ~affix:fragment actual))

let roundtrip msg codec equal_value value =
  let decoded = decode codec (encode codec value) in
  is_true ~msg (equal_value value decoded)

let event_json event = encode M.Event.jsont event

(* The high-level revert Scope/Outcome selectors that cross the wire (W3): a
   round-trip through the codec, asserted structurally (the vocabulary has no
   [equal], and the codec is the property under test). *)
let scope_outcome_group =
  let rt_scope v =
    decode M.Revert.Scope.jsont (encode M.Revert.Scope.jsont v)
  in
  let rt_outcome v =
    decode M.Revert.Outcome.jsont (encode M.Revert.Outcome.jsont v)
  in
  group "revert scope/outcome codecs"
    [
      test "Scope round-trips each arm" (fun () ->
          (match rt_scope M.Revert.Scope.Latest with
          | M.Revert.Scope.Latest -> ()
          | _ -> fail "latest arm did not round-trip");
          (match
             rt_scope (M.Revert.Scope.Change (M.Change.Id.of_string "chg-1"))
           with
          | M.Revert.Scope.Change id ->
              equal string ~msg:"change id survives" "chg-1"
                (M.Change.Id.to_string id)
          | _ -> fail "change arm did not round-trip");
          match rt_scope (M.Revert.Scope.Path (wpath "src/a.ml")) with
          | M.Revert.Scope.Path p ->
              is_true ~msg:"path survives" (W.Path.equal p (wpath "src/a.ml"))
          | _ -> fail "path arm did not round-trip");
      test "Outcome round-trips nothing and refused" (fun () ->
          (match rt_outcome M.Revert.Outcome.Nothing_to_revert with
          | M.Revert.Outcome.Nothing_to_revert -> ()
          | _ -> fail "nothing_to_revert did not round-trip");
          match
            rt_outcome (M.Revert.Outcome.Refused [ "a stale"; "an override" ])
          with
          | M.Revert.Outcome.Refused ms ->
              equal (list string) ~msg:"refusal messages survive"
                [ "a stale"; "an override" ]
                ms
          | _ -> fail "refused did not round-trip");
    ]

(* Shared histories. *)

let p1 = wpath "src/a.ml"
let p2 = wpath "src/b.ml"
let p3 = wpath "src/c.ml"
let watched = wpath "watched.txt"

(* Contiguous two-path history under one claim: p1 A1 -> B1, p2 A2 -> B2. *)
let base_history ?checkpoint () =
  edit_event ?checkpoint ~turn:"t1" ~claim:"c1"
    ~fs:[ (p1, "a1\n"); (p2, "a2\n") ]
    [
      Edit.rewrite ~path:p1 ~before:"a1\n" ~after:"b1\n";
      Edit.rewrite ~path:p2 ~before:"a2\n" ~after:"b2\n";
    ]

let base_evidence () =
  evidence
    ~current:
      [ (p1, Edit.Observed.Text "b1\n"); (p2, Edit.Observed.Text "b2\n") ]
    ~blobs:[ "a1\n"; "a2\n" ] ()

(* Non-contiguous history on [p1]: A -> B recorded, then C -> D recorded,
   the B -> C transition unrecorded. *)
let gap_history () =
  [
    edit_event ~turn:"t1" ~claim:"c1"
      ~fs:[ (p1, "va\n") ]
      [ Edit.rewrite ~path:p1 ~before:"va\n" ~after:"vb\n" ];
    edit_event ~turn:"t2" ~claim:"c2"
      ~fs:[ (p1, "vc\n") ]
      [ Edit.rewrite ~path:p1 ~before:"vc\n" ~after:"vd\n" ];
  ]

(* The full lifecycle ledger: checkpoint, two edits, an observation, and a
   confirmed revert of the first claim's rows. *)
let rich_ledger () =
  let cp = checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")) in
  let e1 = base_history ~checkpoint:(M.Checkpoint.id cp) () in
  let e2 =
    edit_event ~turn:"t2" ~claim:"c2" ~fs:[]
      [ Edit.create ~path:p3 ~contents:"three\n" ]
  in
  let obs = observed_event ~turn:"t2" ~claim:"c3" [ watched ] in
  let st = state_exn [ M.Event.checkpoint cp; e1; e2; obs ] in
  let selection =
    M.Revert.Selection.changes
      (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
  in
  let plan =
    prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st selection
  in
  let outcome =
    Edit.apply
      ~io:(pure_io [ (p1, "b1\n"); (p2, "b2\n") ])
      ~workspace (M.Revert.Plan.edit plan)
  in
  let settled = M.Revert.settle (M.Revert.Plan.started plan) outcome in
  ( [
      M.Event.checkpoint cp;
      e1;
      e2;
      obs;
      before_revert plan;
      M.Event.revert_started plan;
      M.Event.revert_settled settled;
    ],
    plan,
    settled )

let rich_events () =
  let events, _, _ = rich_ledger () in
  events

(* Lowering. *)

let lowering_group =
  group "lowering"
    [
      test "of_edit lowers create, modify, and delete with matching images"
        (fun () ->
          let event =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p2, "a\nb\n"); (p3, "x\ny\n") ]
              [
                Edit.create ~path:p1 ~contents:"one\ntwo\n";
                Edit.rewrite ~path:p2 ~before:"a\nb\n" ~after:"a\nc\n";
                Edit.delete ~path:p3 ~before:"x\ny\n";
              ]
          in
          match changes_of event with
          | [ created; modified; deleted ] ->
              equal path_value p1 (M.Change.path created);
              equal image_value M.Image.Missing (M.Change.before created);
              equal image_value (text_image "one\ntwo\n")
                (M.Change.after created);
              check "created kind" (M.Change.kind created = `Create);
              equal image_value (text_image "a\nb\n") (M.Change.before modified);
              equal image_value (text_image "a\nc\n") (M.Change.after modified);
              check "modified kind" (M.Change.kind modified = `Modify);
              equal image_value (text_image "x\ny\n") (M.Change.before deleted);
              equal image_value M.Image.Missing (M.Change.after deleted);
              check "deleted kind" (M.Change.kind deleted = `Delete)
          | changes -> failf "expected three rows, got %d" (List.length changes));
      test "of_edit records diff-backed line counts" (fun () ->
          let event =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p2, "a\nb\n"); (p3, "x\ny\n") ]
              [
                Edit.create ~path:p1 ~contents:"one\ntwo\n";
                Edit.rewrite ~path:p2 ~before:"a\nb\n" ~after:"a\nc\n";
                Edit.delete ~path:p3 ~before:"x\ny\n";
              ]
          in
          match changes_of event with
          | [ created; modified; deleted ] ->
              equal int ~msg:"create additions" 2 (M.Change.additions created);
              equal int ~msg:"create deletions" 0 (M.Change.deletions created);
              equal int ~msg:"modify additions" 1 (M.Change.additions modified);
              equal int ~msg:"modify deletions" 1 (M.Change.deletions modified);
              equal int ~msg:"delete additions" 0 (M.Change.additions deleted);
              equal int ~msg:"delete deletions" 2 (M.Change.deletions deleted)
          | _ -> fail "expected three rows");
      test "change ids derive from the claim, ordinal, and path" (fun () ->
          let event = base_history () in
          List.iter
            (fun change ->
              equal change_id_value
                (claim_derive "c1" (M.Change.path change))
                (M.Change.id change))
            (changes_of event);
          (* The ordinal is a derivation input: the same path under the same
             claim derives a different id at a different ordinal. *)
          not_equal change_id_value (claim_derive "c1" p1)
            (claim_derive ~ordinal:1 "c1" p1));
      test "of_edit is a pure function of the result" (fun () ->
          let result =
            apply_ok
              ~fs:[ (p1, "a1\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a1\n" ~after:"b1\n" ]
          in
          let make () =
            M.Event.of_edit ~turn:(turn "t1") ~claim:(claim "c1") ~ordinal:0
              ~checkpoint:None result
          in
          equal event_value (make ()) (make ()));
      test "a rename lowers to two rows and nets to two path entries" (fun () ->
          let event =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "body\n") ]
              [
                Edit.delete ~path:p1 ~before:"body\n";
                Edit.create ~path:p2 ~contents:"body\n";
              ]
          in
          let changes = changes_of event in
          equal int 2 (List.length changes);
          let entries = M.Change.net changes in
          equal (list path_value) [ p1; p2 ]
            (List.map
               (fun (e : M.Change.Net.entry) -> e.M.Change.Net.path)
               entries);
          match entries with
          | [ deleted; created ] ->
              equal image_value (text_image "body\n")
                deleted.M.Change.Net.before;
              equal image_value M.Image.Missing deleted.M.Change.Net.after;
              equal image_value M.Image.Missing created.M.Change.Net.before;
              equal image_value (text_image "body\n") created.M.Change.Net.after
          | _ -> fail "expected two net entries");
      test "an empty result is refused" (fun () ->
          expect_invalid_arg
            ~expected:"Mentat_mutation.Event.of_edit: result must not be empty"
            "empty result" (fun () ->
              M.Event.of_edit ~turn:(turn "t1") ~claim:(claim "c1") ~ordinal:0
                ~checkpoint:None Edit.Result.empty));
      test "a negative ordinal is refused" (fun () ->
          expect_invalid_arg
            ~expected:"Mentat_mutation.Event.edit: ordinal must be non-negative"
            "negative ordinal" (fun () ->
              edit_event ~ordinal:(-1) ~turn:"t1" ~claim:"c1"
                ~fs:[ (p1, "a\n") ]
                [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]));
      test
        "of_attempt lowers a commit failure to the confirmed prefix plus the \
         uncertain stopping target" (fun () ->
          let error =
            apply_err ~fail_commit:(W.Path.equal p2)
              ~fs:[ (p1, "a1\n"); (p2, "a2\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"a1\n" ~after:"b1\n";
                Edit.rewrite ~path:p2 ~before:"a2\n" ~after:"b2\n";
              ]
          in
          match
            M.Event.of_attempt ~turn:(turn "t1") ~claim:(claim "c1") ~ordinal:0
              ~checkpoint:None
              ~applied:(Edit.Apply_error.applied error)
              ~uncertain:(Some p2)
          with
          | M.Event.Edit { ordinal; changes; uncertain; _ } ->
              equal int ~msg:"ordinal" 0 ordinal;
              (match changes with
              | [ row ] ->
                  equal path_value ~msg:"confirmed prefix row" p1
                    (M.Change.path row);
                  equal image_value (text_image "a1\n") (M.Change.before row);
                  equal image_value (text_image "b1\n") (M.Change.after row);
                  equal change_id_value (claim_derive "c1" p1) (M.Change.id row)
              | rows -> failf "expected one row, got %d" (List.length rows));
              equal (option path_value) (Some p2) uncertain
          | _ -> fail "expected an edit event");
      test
        "of_attempt lowers a first-write commit failure to an uncertain-only \
         event" (fun () ->
          match
            M.Event.of_attempt ~turn:(turn "t1") ~claim:(claim "c1") ~ordinal:0
              ~checkpoint:None ~applied:[] ~uncertain:(Some p1)
          with
          | M.Event.Edit { changes = []; uncertain = Some path; _ } ->
              equal path_value p1 path
          | _ -> fail "expected an uncertain-only edit event");
      test "of_attempt refuses a non-effectful attempt" (fun () ->
          (* An attempt exists only when it left workspace effect: an empty
             prefix with no uncertain target records nothing, so the constructor
             refuses it rather than mint an empty occurrence. *)
          expect_invalid_arg
            ~expected:"Mentat_mutation.Event.edit: changes must not be empty"
            "non-effectful attempt" (fun () ->
              M.Event.of_attempt ~turn:(turn "t1") ~claim:(claim "c1")
                ~ordinal:0 ~checkpoint:None ~applied:[] ~uncertain:None));
      test "of_attempt with no uncertain target equals of_edit" (fun () ->
          let result =
            apply_ok
              ~fs:[ (p1, "a1\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a1\n" ~after:"b1\n" ]
          in
          equal event_value
            (M.Event.of_edit ~turn:(turn "t1") ~claim:(claim "c1") ~ordinal:2
               ~checkpoint:None result)
            (M.Event.of_attempt ~turn:(turn "t1") ~claim:(claim "c1") ~ordinal:2
               ~checkpoint:None
               ~applied:(Edit.Result.entries result)
               ~uncertain:None));
      test "tool_observed deduplicates and sorts paths" (fun () ->
          match observed_event ~turn:"t1" ~claim:"c1" [ p2; p1; p2 ] with
          | M.Event.Tool_observed { paths; _ } ->
              equal (list path_value) [ p1; p2 ] paths
          | _ -> fail "expected a tool_observed event");
      test "tool_observed refuses an empty path list" (fun () ->
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Event.tool_observed: paths must not be empty"
            "empty paths" (fun () -> observed_event ~turn:"t1" ~claim:"c1" []));
    ]

(* Fold rejection and admission. *)

let expect_reject msg events index reason_check =
  let error = state_err events in
  equal int ~msg:(msg ^ ": index") index error.M.State.Error.index;
  is_true ~msg:(msg ^ ": reason") (reason_check error.M.State.Error.reason)

let fold_group =
  group "replay: fold rejection"
    [
      test "a repeated ordinal for a claim is rejected" (fun () ->
          let e1 = base_history () in
          expect_reject "repeated ordinal" [ e1; e1 ] 1 (function
            | M.State.Error.Invalid_ordinal { claim = c; ordinal; expected } ->
                Session.Tool_claim.Id.equal c (claim "c1")
                && ordinal = 0 && expected = 1
            | _ -> false));
      test "ordinals must be dense in order" (fun () ->
          (* A first event carrying ordinal 1 is a gap. *)
          let gapped =
            edit_event ~ordinal:1 ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          expect_reject "gap" [ gapped ] 0 (function
            | M.State.Error.Invalid_ordinal { ordinal; expected; _ } ->
                ordinal = 1 && expected = 0
            | _ -> false);
          (* A skipped ordinal after a recorded apply is a gap too. *)
          let e0 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          let e2 =
            edit_event ~ordinal:2 ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "b\n") ]
              [ Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n" ]
          in
          expect_reject "skip" [ e0; e2 ] 1 (function
            | M.State.Error.Invalid_ordinal { ordinal; expected; _ } ->
                ordinal = 2 && expected = 1
            | _ -> false));
      test "two applies under one claim fold, same path twice included"
        (fun () ->
          (* The cardinality law: the ledger records occurrences as they
             occur — a claim scope with two applies records two events, and
             the same path can legitimately change twice under one claim
             across applies. *)
          let e0 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          let e1 =
            edit_event ~ordinal:1 ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "b\n"); (p2, "x\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n";
                Edit.rewrite ~path:p2 ~before:"x\n" ~after:"y\n";
              ]
          in
          let st = state_exn [ e0; e1 ] in
          (* Evidence is derived by folding the claim's events: rows
             concatenate in ordinal order. *)
          equal (list change_id_value)
            [
              claim_derive "c1" p1;
              claim_derive ~ordinal:1 "c1" p1;
              claim_derive ~ordinal:1 "c1" p2;
            ]
            (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")));
          (* The within-claim per-path law: the later apply's row is the
             ledger head, and adjacent applies net like any other deltas. *)
          (match M.State.head st p1 with
          | Some head ->
              equal change_id_value
                (claim_derive ~ordinal:1 "c1" p1)
                (M.Change.id head)
          | None -> fail "expected a head for p1");
          (match M.State.net st M.Revert.Selection.all with
          | [ one; _ ] ->
              equal path_value p1 one.M.Change.Net.path;
              equal image_value (text_image "a\n") one.M.Change.Net.before;
              equal image_value (text_image "c\n") one.M.Change.Net.after;
              is_true ~msg:"adjacent applies join" one.M.Change.Net.contiguous
          | entries ->
              failf "expected two entries, got %d" (List.length entries));
          (* The ordered per-apply outcome is read off the [changes] projection
             above: its rows are the two applies' rows concatenated in ordinal
             order (ordinal 0's one row, then ordinal 1's two), and the
             claim carries no uncertain target, so the selection is Available. *)
          equal revertability_value M.Revertability.Available
            (M.State.revertability st M.Revert.Selection.all));
      test "a non-joining second apply marks the entry non-contiguous"
        (fun () ->
          (* A -> B recorded at ordinal 0, C -> D at ordinal 1: the unrecorded
             B -> C transition breaks contiguity within one claim exactly as
             it would across claims. *)
          let e0 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          let e1 =
            edit_event ~ordinal:1 ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "c\n") ]
              [ Edit.rewrite ~path:p1 ~before:"c\n" ~after:"d\n" ]
          in
          let st = state_exn [ e0; e1 ] in
          match M.State.net st M.Revert.Selection.all with
          | [ entry ] -> is_false entry.M.Change.Net.contiguous
          | entries -> failf "expected one entry, got %d" (List.length entries));
      test "one claim pins one turn across its events" (fun () ->
          let e0 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          (* An observation window for the claim on another turn. *)
          expect_reject "edit then observed"
            [ e0; observed_event ~turn:"t2" ~claim:"c1" [ watched ] ]
            1
            (function
              | M.State.Error.Turn_conflict c ->
                  Session.Tool_claim.Id.equal c (claim "c1")
              | _ -> false);
          (* A second apply for the claim on another turn. *)
          let e1 =
            edit_event ~ordinal:1 ~turn:"t2" ~claim:"c1"
              ~fs:[ (p1, "b\n") ]
              [ Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n" ]
          in
          expect_reject "edit then edit" [ e0; e1 ] 1 (function
            | M.State.Error.Turn_conflict _ -> true
            | _ -> false);
          (* An observation then an edit on another turn. *)
          expect_reject "observed then edit"
            [ observed_event ~turn:"t2" ~claim:"c1" [ watched ]; e0 ]
            1
            (function M.State.Error.Turn_conflict _ -> true | _ -> false));
      test "a duplicate checkpoint id is rejected" (fun () ->
          let cp = checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")) in
          expect_reject "duplicate checkpoint"
            [ M.Event.checkpoint cp; M.Event.checkpoint cp ]
            1
            (function
              | M.State.Error.Duplicate_checkpoint id ->
                  M.Checkpoint.Id.equal id (M.Checkpoint.id cp)
              | _ -> false));
      test
        "available and degraded captures of one boundary collide on one \
         identity" (fun () ->
          let a = checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")) in
          let d =
            checkpoint ~capture:degraded
              (M.Checkpoint.Before_turn_tools (turn "t1"))
          in
          equal checkpoint_id_value (M.Checkpoint.id a) (M.Checkpoint.id d);
          expect_reject "boundary identity"
            [ M.Event.checkpoint a; M.Event.checkpoint d ]
            1
            (function
              | M.State.Error.Duplicate_checkpoint _ -> true | _ -> false));
      test "an edit referencing an unrecorded checkpoint is rejected" (fun () ->
          let cp = checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")) in
          let e1 = base_history ~checkpoint:(M.Checkpoint.id cp) () in
          expect_reject "dangling" [ e1 ] 0 (function
            | M.State.Error.Dangling_checkpoint id ->
                M.Checkpoint.Id.equal id (M.Checkpoint.id cp)
            | _ -> false));
      test "a checkpoint recorded after the edit is still dangling" (fun () ->
          let cp = checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")) in
          let e1 = base_history ~checkpoint:(M.Checkpoint.id cp) () in
          expect_reject "order"
            [ e1; M.Event.checkpoint cp ]
            0
            (function
              | M.State.Error.Dangling_checkpoint _ -> true | _ -> false));
      test "a checkpoint for another turn mismatches" (fun () ->
          let cp = checkpoint (M.Checkpoint.Before_turn_tools (turn "t9")) in
          let e1 = base_history ~checkpoint:(M.Checkpoint.id cp) () in
          expect_reject "other turn"
            [ M.Event.checkpoint cp; e1 ]
            1
            (function
              | M.State.Error.Checkpoint_mismatch _ -> true | _ -> false));
      test "an after-turn boundary mismatches an edit reference" (fun () ->
          let cp = checkpoint (M.Checkpoint.After_turn (turn "t1")) in
          let e1 = base_history ~checkpoint:(M.Checkpoint.id cp) () in
          expect_reject "wrong boundary"
            [ M.Event.checkpoint cp; e1 ]
            1
            (function
              | M.State.Error.Checkpoint_mismatch _ -> true | _ -> false));
      test "a degraded capture mismatches an edit reference" (fun () ->
          let cp =
            checkpoint ~capture:degraded
              (M.Checkpoint.Before_turn_tools (turn "t1"))
          in
          let e1 = base_history ~checkpoint:(M.Checkpoint.id cp) () in
          expect_reject "degraded"
            [ M.Event.checkpoint cp; e1 ]
            1
            (function
              | M.State.Error.Checkpoint_mismatch _ -> true | _ -> false));
      test "the turn's available conservative capture admits the reference"
        (fun () ->
          let cp = checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")) in
          let e1 = base_history ~checkpoint:(M.Checkpoint.id cp) () in
          let st = state_exn [ M.Event.checkpoint cp; e1 ] in
          equal
            (option
               (Testable.make ~pp:M.Checkpoint.pp ~equal:M.Checkpoint.equal))
            (Some cp)
            (M.State.checkpoint_at st (M.Checkpoint.boundary cp));
          (* An uncaptured boundary has no recorded capture. *)
          equal (option pass) None
            (M.State.checkpoint_at st (M.Checkpoint.After_recovery (turn "t1"))));
      test "a settlement with no prior start is unmatched" (fun () ->
          let st = state_exn [ base_history () ] in
          let plan =
            prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st
              (M.Revert.Selection.changes
                 (List.map M.Change.id (M.State.changes st ~claim:(claim "c1"))))
          in
          let settled =
            M.Revert.settle_ambiguous (M.Revert.Plan.started plan)
          in
          expect_reject "unmatched"
            [ base_history (); M.Event.revert_settled settled ]
            1
            (function
              | M.State.Error.Unmatched_settlement id ->
                  M.Revert.Id.equal id (rid "r1")
              | _ -> false));
      test "a second settlement for a settled revert is unmatched" (fun () ->
          let st = state_exn [ base_history () ] in
          let plan =
            prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st
              (M.Revert.Selection.changes
                 (List.map M.Change.id (M.State.changes st ~claim:(claim "c1"))))
          in
          let settled =
            M.Revert.settle_ambiguous (M.Revert.Plan.started plan)
          in
          expect_reject "double settlement"
            [
              base_history ();
              before_revert plan;
              M.Event.revert_started plan;
              M.Event.revert_settled settled;
              M.Event.revert_settled settled;
            ]
            4
            (function
              | M.State.Error.Unmatched_settlement _ -> true | _ -> false));
      test "a duplicate revert start is rejected" (fun () ->
          let st = state_exn [ base_history () ] in
          let plan =
            prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st
              (M.Revert.Selection.changes
                 (List.map M.Change.id (M.State.changes st ~claim:(claim "c1"))))
          in
          expect_reject "duplicate revert"
            [
              base_history ();
              before_revert plan;
              M.Event.revert_started plan;
              M.Event.revert_started plan;
            ]
            3
            (function
              | M.State.Error.Duplicate_revert id ->
                  M.Revert.Id.equal id (rid "r1")
              | _ -> false));
      test "a settlement not covering its started targets mismatches" (fun () ->
          (* Two started facts for the same id, frozen from different
             histories: the ledger carries the two-target start, the
             settlement covers the one-target one. *)
          let st2 = state_exn [ base_history () ] in
          let plan2 =
            prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st2
              (M.Revert.Selection.changes
                 (List.map M.Change.id
                    (M.State.changes st2 ~claim:(claim "c1"))))
          in
          let st1 =
            state_exn
              [
                edit_event ~turn:"t1" ~claim:"c1"
                  ~fs:[ (p1, "a1\n") ]
                  [ Edit.rewrite ~path:p1 ~before:"a1\n" ~after:"b1\n" ];
              ]
          in
          let plan1 =
            prepare_ok ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p1, Edit.Observed.Text "b1\n") ]
                   ~blobs:[ "a1\n" ] ())
              st1
              (M.Revert.Selection.changes
                 (List.map M.Change.id
                    (M.State.changes st1 ~claim:(claim "c1"))))
          in
          let settled1 =
            M.Revert.settle_ambiguous (M.Revert.Plan.started plan1)
          in
          expect_reject "coverage"
            [
              base_history ();
              before_revert plan2;
              M.Event.revert_started plan2;
              M.Event.revert_settled settled1;
            ]
            3
            (function
              | M.State.Error.Settlement_mismatch id ->
                  M.Revert.Id.equal id (rid "r1")
              | _ -> false));
      test "a start over an unresolved revert is an invalid start" (fun () ->
          (* No planner can mint a start while an older revert is unresolved
             (preparation refuses), so a ledger carrying one is forged. *)
          let st = state_exn [ base_history () ] in
          let sel =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
          in
          let plan_a =
            prepare_ok ~id:(rid "r-a") ~evidence:(base_evidence ()) st sel
          in
          let plan_b =
            prepare_ok ~id:(rid "r-b") ~evidence:(base_evidence ()) st sel
          in
          expect_reject "start under unresolved"
            [
              base_history ();
              before_revert plan_a;
              M.Event.revert_started plan_a;
              before_revert plan_b;
              M.Event.revert_started plan_b;
            ]
            4
            (function
              | M.State.Error.Invalid_start id ->
                  M.Revert.Id.equal id (rid "r-b")
              | _ -> false));
      test "fold projections: claims, changes, observed" (fun () ->
          let events, _, _ = rich_ledger () in
          let st = state_exn events in
          equal (list claim_id_value)
            [ claim "c1"; claim "c2"; claim "c3" ]
            (M.State.claims st);
          equal int 2 (List.length (M.State.changes st ~claim:(claim "c1")));
          equal int 1 (List.length (M.State.changes st ~claim:(claim "c2")));
          equal (list change_value) [] (M.State.changes st ~claim:(claim "c3"));
          equal (list path_value) [ watched ]
            (M.State.observed st ~claim:(claim "c3"));
          equal (list path_value) [] (M.State.observed st ~claim:(claim "c1")));
      test "change resolves a recorded row by id" (fun () ->
          let events, _, _ = rich_ledger () in
          let st = state_exn events in
          let row =
            match M.State.changes st ~claim:(claim "c1") with
            | row :: _ -> row
            | [] -> fail "expected a recorded change"
          in
          (match M.State.change st (M.Change.id row) with
          | Some found ->
              equal change_id_value (M.Change.id row) (M.Change.id found)
          | None -> fail "a recorded change must resolve by id");
          equal (option pass) None
            (M.State.change st (M.Change.Id.of_string "no-such-change")));
      test "observed paths merge and sort across windows of one claim"
        (fun () ->
          let st =
            state_exn
              [
                observed_event ~turn:"t1" ~claim:"c1" [ p2 ];
                observed_event ~turn:"t1" ~claim:"c1" [ p1; p2 ];
              ]
          in
          equal (list path_value) [ p1; p2 ]
            (M.State.observed st ~claim:(claim "c1")));
      test "of_events is deterministic" (fun () ->
          let events = rich_events () in
          assert_state_equiv "refold" (state_exn events) (state_exn events));
    ]

(* Revert-start validation. *)

(* Forged starts ride the durable codec — decode admits them (their local
   invariants hold), and the fold rejects what no planner could have minted
   at the prefix. *)
let tamper_started f json =
  set_member "started" (f (get_member "started" json)) json

let tamper_target f started =
  let targets = get_member "targets" started in
  set_member "targets"
    (set_element 0 (f (get_element 0 targets)) targets)
    started

let invalid_start_for id = function
  | M.State.Error.Invalid_start actual -> M.Revert.Id.equal actual id
  | _ -> false

let start_validation_group =
  group "replay: revert-start validation"
    [
      test "a change selection must resolve every selected id at its prefix"
        (fun () ->
          let st = state_exn [ base_history () ] in
          let plan =
            prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st
              (M.Revert.Selection.changes
                 (List.map M.Change.id (M.State.changes st ~claim:(claim "c1"))))
          in
          (* The same start replayed without its history resolves nothing. *)
          expect_reject "no history"
            [ M.Event.revert_started plan ]
            0
            (function
              | M.State.Error.Unknown_selected_change _ -> true | _ -> false));
      test "a start over a superseded selection is invalid" (fun () ->
          let e1 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2"
              ~fs:[ (p1, "b\n") ]
              [ Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n" ]
          in
          let st1 = state_exn [ e1 ] in
          let plan =
            prepare_ok ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p1, Edit.Observed.Text "b\n") ]
                   ~blobs:[ "a\n" ] ())
              st1
              (M.Revert.Selection.changes
                 (List.map M.Change.id
                    (M.State.changes st1 ~claim:(claim "c1"))))
          in
          (* Valid at [e1]; forged when a later recorded change supersedes
             the selection's net-after. *)
          expect_reject "superseded"
            [ e1; e2; M.Event.revert_started plan ]
            2
            (invalid_start_for (rid "r1")));
      test "a forged restore image is invalid" (fun () ->
          let json =
            event_json
              (M.Event.revert_started
                 (prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ())
                    (state_exn [ base_history () ])
                    M.Revert.Selection.all))
          in
          let forged =
            decode M.Event.jsont
              (tamper_started
                 (tamper_target
                    (set_member "restore"
                       (encode M.Image.jsont (text_image "zz\n"))))
                 json)
          in
          expect_reject "forged restore"
            [ base_history (); forged ]
            1
            (invalid_start_for (rid "r1")));
      test "a forged expected image on a contiguous target is invalid"
        (fun () ->
          let json =
            event_json
              (M.Event.revert_started
                 (prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ())
                    (state_exn [ base_history () ])
                    M.Revert.Selection.all))
          in
          let forged =
            decode M.Event.jsont
              (tamper_started
                 (tamper_target
                    (set_member "expected"
                       (encode M.Image.jsont (text_image "zz\n"))))
                 json)
          in
          expect_reject "forged expected"
            [ base_history (); forged ]
            1
            (invalid_start_for (rid "r1")));
      test "a forged source list is invalid" (fun () ->
          let json =
            event_json
              (M.Event.revert_started
                 (prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ())
                    (state_exn [ base_history () ])
                    M.Revert.Selection.all))
          in
          let forged =
            decode M.Event.jsont
              (tamper_started
                 (tamper_target
                    (set_member "sources"
                       (Json.list [ Json.string (String.make 64 'e') ])))
                 json)
          in
          expect_reject "forged sources"
            [ base_history (); forged ]
            1
            (invalid_start_for (rid "r1")));
      test "a stripped override on a non-contiguous target is invalid"
        (fun () ->
          let st = state_exn (gap_history ()) in
          let plan =
            prepare_ok
              ~override:(M.Revert.Override.accept_unrecorded_loss [ p1 ])
              ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p1, Edit.Observed.Text "unrecorded\n") ]
                   ~blobs:[ "va\n" ] ())
              st M.Revert.Selection.all
          in
          let json = event_json (M.Event.revert_started plan) in
          let forged =
            decode M.Event.jsont
              (tamper_started (remove_member "override") json)
          in
          expect_reject "stripped override"
            (gap_history () @ [ forged ])
            2
            (invalid_start_for (rid "r1"));
          (* And the untampered start folds cleanly: the consent is exactly
             the non-contiguous target paths. *)
          let ok =
            state_exn
              (gap_history ()
              @ [ before_revert plan; M.Event.revert_started plan ])
          in
          equal (list started_value)
            [ M.Revert.Plan.started plan ]
            (M.State.unresolved_reverts ok));
      test "a start dropping a contiguous target is invalid" (fun () ->
          let json =
            event_json
              (M.Event.revert_started
                 (prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ())
                    (state_exn [ base_history () ])
                    M.Revert.Selection.all))
          in
          (* base_history nets two contiguous entries; a start covering only
             one is one no planner could mint. *)
          let forged =
            decode M.Event.jsont
              (tamper_started
                 (fun started ->
                   let targets = get_member "targets" started in
                   set_member "targets"
                     (Json.list [ get_element 0 targets ])
                     started)
                 json)
          in
          expect_reject "dropped contiguous target"
            [ base_history (); forged ]
            1
            (invalid_start_for (rid "r1")));
    ]

(* The append law. *)

let append_group =
  group "replay: append law"
    [
      test "append equals the whole-ledger refold at every split point"
        (fun () ->
          let events = rich_events () in
          let full = state_exn events in
          let n = List.length events in
          for k = 0 to n do
            let prefix = List.filteri (fun i _ -> i < k) events in
            let batch = List.filteri (fun i _ -> i >= k) events in
            match M.State.append (state_exn prefix) batch with
            | Error error ->
                failf "append rejected at split %d: %a" k M.State.Error.pp error
            | Ok appended ->
                assert_state_equiv (Printf.sprintf "split %d" k) appended full
          done);
      test "append splits a multi-ordinal claim without reordering its rows"
        (fun () ->
          (* One claim's ordinal 0 lands in the finalized prefix and its
             ordinal 1 in the appended batch: the per-claim accumulator crosses
             the finalize/unfinalize seam, so its rows must still read in
             ordinal order (ordinal 0's one row, then ordinal 1's two). *)
          let e0 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          let e1 =
            edit_event ~ordinal:1 ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "b\n"); (p2, "x\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n";
                Edit.rewrite ~path:p2 ~before:"x\n" ~after:"y\n";
              ]
          in
          let full = state_exn [ e0; e1 ] in
          match M.State.append (state_exn [ e0 ]) [ e1 ] with
          | Error error -> failf "append rejected: %a" M.State.Error.pp error
          | Ok appended ->
              equal (list change_id_value)
                [
                  claim_derive "c1" p1;
                  claim_derive ~ordinal:1 "c1" p1;
                  claim_derive ~ordinal:1 "c1" p2;
                ]
                (List.map M.Change.id
                   (M.State.changes appended ~claim:(claim "c1")));
              assert_state_equiv "append-split multi-ordinal" appended full);
      test "append [] is the identity" (fun () ->
          let st = state_exn (rich_events ()) in
          match M.State.append st [] with
          | Ok st' -> assert_state_equiv "identity" st st'
          | Error error -> failf "rejected: %a" M.State.Error.pp error);
      test "a rejected batch reports the whole-ledger index and reason"
        (fun () ->
          let events = rich_events () in
          let dup = List.nth events 1 in
          let extra = observed_event ~turn:"t3" ~claim:"c9" [ watched ] in
          let whole =
            match M.State.of_events (events @ [ extra; dup ]) with
            | Error error -> error
            | Ok _ -> fail "expected the refold to reject"
          in
          let st = state_exn events in
          match M.State.append st [ extra; dup ] with
          | Ok _ -> fail "expected the append to reject"
          | Error error ->
              equal int (List.length events + 1) error.M.State.Error.index;
              equal state_error_value whole error);
    ]

(* at_claim. *)

let at_claim_group =
  group "replay: at_claim"
    [
      test "an unreferenced claim has no prefix" (fun () ->
          let st = state_exn [ base_history () ] in
          equal (option pass) None (M.State.at_claim st ~claim:(claim "ghost")));
      test "the prefix answer survives later superseding changes" (fun () ->
          let e1 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2"
              ~fs:[ (p1, "b\n") ]
              [ Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n" ]
          in
          let st = state_exn [ e1; e2 ] in
          let sel =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
          in
          let prefix =
            match M.State.at_claim st ~claim:(claim "c1") with
            | Some prefix -> prefix
            | None -> fail "expected a prefix"
          in
          equal (list event_value) [ e1 ] (M.State.events prefix);
          equal revertability_value M.Revertability.Available
            (M.State.revertability prefix sel);
          let by =
            match M.State.changes st ~claim:(claim "c2") with
            | [ change ] -> M.Change.id change
            | _ -> fail "expected one row"
          in
          equal revertability_value
            (M.Revertability.Unavailable
               [ M.Revertability.Superseded { path = p1; by } ])
            (M.State.revertability st sel));
      test "the prefix runs through the last referencing event" (fun () ->
          let e1 = base_history () in
          let obs = observed_event ~turn:"t1" ~claim:"c1" [ watched ] in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2" ~fs:[]
              [ Edit.create ~path:p3 ~contents:"three\n" ]
          in
          let st = state_exn [ e1; e2; obs ] in
          let prefix =
            match M.State.at_claim st ~claim:(claim "c1") with
            | Some prefix -> prefix
            | None -> fail "expected a prefix"
          in
          equal (list event_value) [ e1; e2; obs ] (M.State.events prefix));
    ]

(* Branch prefix. *)

(* [prefix_for_turns] is the copyable ledger a fork or rewind child seeds from
   its parent: the maximal event prefix referencing only the turns the child
   retains. A fork keeps every turn (the whole ledger); a rewind keeps the
   anchored prefix and drops the rest. *)
let edit_on ~turn:t ~claim:c ~path ~before ~after =
  edit_event ~turn:t ~claim:c
    ~fs:[ (path, before) ]
    [ Edit.rewrite ~path ~before ~after ]

let keep_only ids t =
  List.exists (fun id -> Session.Turn.Id.equal t (turn id)) ids

let prefix_group =
  group "branch prefix (prefix_for_turns)"
    [
      test "a fork keeps the whole ledger" (fun () ->
          let e1 =
            edit_on ~turn:"t1" ~claim:"c1" ~path:p1 ~before:"a\n" ~after:"b\n"
          in
          let e2 =
            edit_on ~turn:"t2" ~claim:"c2" ~path:p2 ~before:"x\n" ~after:"y\n"
          in
          let st = state_exn [ e1; e2 ] in
          equal (list event_value) [ e1; e2 ]
            (M.State.prefix_for_turns st ~keep:(fun _ -> true)));
      test "a rewind drops a later turn's edit" (fun () ->
          let e1 =
            edit_on ~turn:"t1" ~claim:"c1" ~path:p1 ~before:"a\n" ~after:"b\n"
          in
          let e2 =
            edit_on ~turn:"t2" ~claim:"c2" ~path:p2 ~before:"x\n" ~after:"y\n"
          in
          let st = state_exn [ e1; e2 ] in
          equal (list event_value) [ e1 ]
            (M.State.prefix_for_turns st ~keep:(keep_only [ "t1" ])));
      test
        "a kept-turn checkpoint rides but a dropped-turn observation ends the \
         prefix" (fun () ->
          let cp =
            M.Event.checkpoint
              (checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")))
          in
          let e1 =
            edit_on ~turn:"t1" ~claim:"c1" ~path:p1 ~before:"a\n" ~after:"b\n"
          in
          let obs = observed_event ~turn:"t2" ~claim:"c2" [ watched ] in
          let st = state_exn [ cp; e1; obs ] in
          equal (list event_value) [ cp; e1 ]
            (M.State.prefix_for_turns st ~keep:(keep_only [ "t1" ])));
      test "a revert over a non-turns selection rides the kept prefix"
        (fun () ->
          let e1 =
            edit_on ~turn:"t1" ~claim:"c1" ~path:p1 ~before:"a\n" ~after:"b\n"
          in
          let st1 = state_exn [ e1 ] in
          let change_id =
            match M.State.changes st1 ~claim:(claim "c1") with
            | [ c ] -> M.Change.id c
            | _ -> fail "expected one change"
          in
          let selection = M.Revert.Selection.changes [ change_id ] in
          let ev =
            evidence
              ~current:[ (p1, Edit.Observed.Text "b\n") ]
              ~blobs:[ "a\n" ] ()
          in
          let plan = prepare_ok ~id:(rid "r1") ~evidence:ev st1 selection in
          let cp = before_revert plan in
          let started_ev = M.Event.revert_started plan in
          let result =
            apply_plan_ok ~fs:[ (p1, "b\n") ] (M.Revert.Plan.edit plan)
          in
          let settled_ev =
            M.Event.revert_settled
              (M.Revert.settle (M.Revert.Plan.started plan) (Ok result))
          in
          let e2 =
            edit_on ~turn:"t2" ~claim:"c2" ~path:p2 ~before:"x\n" ~after:"y\n"
          in
          let st = state_exn [ e1; cp; started_ev; settled_ev; e2 ] in
          (* The revert selects a change, not a turn, so it references no turn
             and rides the kept-turn prefix; the second turn's edit ends it. *)
          equal (list event_value)
            [ e1; cp; started_ev; settled_ev ]
            (M.State.prefix_for_turns st ~keep:(keep_only [ "t1" ])));
    ]

(* Netting. *)

(* A step is one applied transition on one of three paths; changes are minted
   through [Mentat_edit.apply] plus [Event.of_edit], each under its own claim
   so change ids never collide. *)
let gen_path index = wpath (Printf.sprintf "gen/%d.txt" index)

let change_of_step index (path_index, before, after) =
  let path = gen_path path_index in
  let plan =
    match (before, after) with
    | None, Some contents -> Edit.create ~path ~contents
    | Some b, Some a -> Edit.rewrite ~path ~before:b ~after:a
    | Some b, None -> Edit.delete ~path ~before:b
    | None, None -> fail "invalid step"
  in
  let fs = match before with None -> [] | Some c -> [ (path, c) ] in
  let event =
    M.Event.of_edit ~turn:(turn "t-gen")
      ~claim:(claim (Printf.sprintf "gen-%d" index))
      ~ordinal:0 ~checkpoint:None (apply_ok ~fs [ plan ])
  in
  match changes_of event with [ c ] -> c | _ -> fail "expected one row"

(* The reference model: per path in first-seen order, first before image,
   last after image, contiguity of adjacent deltas, sources in order; equal
   endpoints drop. *)
let model_net steps_with_ids =
  let rec paths_seen acc = function
    | [] -> List.rev acc
    | ((i, _, _), _) :: rest ->
        if List.mem i acc then paths_seen acc rest
        else paths_seen (i :: acc) rest
  in
  List.filter_map
    (fun path_index ->
      let deltas =
        List.filter (fun ((i, _, _), _) -> i = path_index) steps_with_ids
      in
      let (_, first, _), _ = List.hd deltas in
      let (_, _, last), _ = List.hd (List.rev deltas) in
      let rec contiguous = function
        | ((_, _, a), _) :: (((_, b, _), _) as next) :: rest ->
            Option.equal String.equal a b && contiguous (next :: rest)
        | _ -> true
      in
      if M.Image.equal (image_of first) (image_of last) then None
      else
        Some
          ( gen_path path_index,
            image_of first,
            image_of last,
            contiguous deltas,
            List.map snd deltas ))
    (paths_seen [] steps_with_ids)

let steps_gen =
  let texts = [ None; Some "l1\n"; Some "l2\n"; Some "l3\n" ] in
  let valid_pairs =
    List.concat_map
      (fun b ->
        List.filter_map
          (fun a ->
            if Option.equal String.equal b a || (b = None && a = None) then None
            else Some (b, a))
          texts)
      texts
  in
  let step_gen =
    Gen.map
      (fun (index, (before, after)) -> (index, before, after))
      (Gen.pair (Gen.int_range 0 2) (Gen.of_list valid_pairs))
  in
  let pp_step ppf (i, b, a) =
    let text = function None -> "-" | Some s -> String.escaped s in
    Format.fprintf ppf "(%d, %s, %s)" i (text b) (text a)
  in
  Gen.with_pp
    (Format.pp_print_list pp_step)
    (Gen.list ~size:(Gen.int_range 1 10) step_gen)

let netting_group =
  group "netting"
    [
      prop "net agrees with the endpoint model over generated sequences"
        steps_gen (fun steps ->
          let steps_with_ids =
            List.mapi
              (fun index step ->
                (step, M.Change.id (change_of_step index step)))
              steps
          in
          let changes = List.mapi change_of_step steps in
          let entries = M.Change.net changes in
          let expected = model_net steps_with_ids in
          equal int ~msg:"entry count" (List.length expected)
            (List.length entries);
          List.iter2
            (fun (path, before, after, contiguous, sources)
                 (entry : M.Change.Net.entry) ->
              equal path_value ~msg:"path" path entry.M.Change.Net.path;
              equal image_value ~msg:"before" before entry.M.Change.Net.before;
              equal image_value ~msg:"after" after entry.M.Change.Net.after;
              equal bool ~msg:"contiguous" contiguous
                entry.M.Change.Net.contiguous;
              equal (list change_id_value) ~msg:"sources" sources
                entry.M.Change.Net.sources)
            expected entries);
      test "equal endpoints drop: a created-then-deleted path nets away"
        (fun () ->
          let create =
            changes_of
              (edit_event ~turn:"t1" ~claim:"c1" ~fs:[]
                 [ Edit.create ~path:p1 ~contents:"tmp\n" ])
          in
          let delete =
            changes_of
              (edit_event ~turn:"t1" ~claim:"c2"
                 ~fs:[ (p1, "tmp\n") ]
                 [ Edit.delete ~path:p1 ~before:"tmp\n" ])
          in
          equal (list net_entry_value) [] (M.Change.net (create @ delete)));
      test "contiguous flips exactly on a non-joining delta" (fun () ->
          let joined =
            changes_of
              (edit_event ~turn:"t1" ~claim:"c1"
                 ~fs:[ (p1, "a\n") ]
                 [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ])
            @ changes_of
                (edit_event ~turn:"t1" ~claim:"c2"
                   ~fs:[ (p1, "b\n") ]
                   [ Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n" ])
          in
          let gapped =
            changes_of
              (edit_event ~turn:"t1" ~claim:"c3"
                 ~fs:[ (p1, "a\n") ]
                 [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ])
            @ changes_of
                (edit_event ~turn:"t1" ~claim:"c4"
                   ~fs:[ (p1, "x\n") ]
                   [ Edit.rewrite ~path:p1 ~before:"x\n" ~after:"c\n" ])
          in
          (match M.Change.net joined with
          | [ entry ] -> is_true ~msg:"joined" entry.M.Change.Net.contiguous
          | _ -> fail "expected one entry");
          match M.Change.net gapped with
          | [ entry ] -> is_false ~msg:"gapped" entry.M.Change.Net.contiguous
          | _ -> fail "expected one entry");
      test "path order is first-seen and source order is observation order"
        (fun () ->
          let c_q =
            changes_of
              (edit_event ~turn:"t1" ~claim:"c1" ~fs:[]
                 [ Edit.create ~path:p2 ~contents:"q\n" ])
          in
          let c_p =
            changes_of
              (edit_event ~turn:"t1" ~claim:"c2" ~fs:[]
                 [ Edit.create ~path:p1 ~contents:"p\n" ])
          in
          let c_q2 =
            changes_of
              (edit_event ~turn:"t1" ~claim:"c3"
                 ~fs:[ (p2, "q\n") ]
                 [ Edit.rewrite ~path:p2 ~before:"q\n" ~after:"q2\n" ])
          in
          match M.Change.net (c_q @ c_p @ c_q2) with
          | [ q_entry; p_entry ] ->
              equal path_value p2 q_entry.M.Change.Net.path;
              equal path_value p1 p_entry.M.Change.Net.path;
              equal (list change_id_value)
                (List.map M.Change.id (c_q @ c_q2))
                q_entry.M.Change.Net.sources
          | entries ->
              failf "expected two entries, got %d" (List.length entries));
    ]

(* Revertability. *)

let revertability_group =
  group "revertability"
    [
      test "exact contiguous history is available" (fun () ->
          let st = state_exn [ base_history () ] in
          equal revertability_value M.Revertability.Available
            (M.State.revertability st M.Revert.Selection.all));
      test "a degraded conservative checkpoint does not degrade exact history"
        (fun () ->
          let cp =
            checkpoint ~capture:degraded
              (M.Checkpoint.Before_turn_tools (turn "t1"))
          in
          let st = state_exn [ M.Event.checkpoint cp; base_history () ] in
          equal revertability_value M.Revertability.Available
            (M.State.revertability st M.Revert.Selection.all));
      test "a non-contiguous entry is incomplete" (fun () ->
          let st = state_exn (gap_history ()) in
          equal revertability_value
            (M.Revertability.Incomplete
               [ M.Revertability.Non_contiguous { path = p1 } ])
            (M.State.revertability st M.Revert.Selection.all));
      test "an observed overlap on a resolved path is incomplete" (fun () ->
          let st =
            state_exn
              [ base_history (); observed_event ~turn:"t2" ~claim:"c3" [ p1 ] ]
          in
          equal revertability_value
            (M.Revertability.Incomplete
               [ M.Revertability.Observed { claim = claim "c3" } ])
            (M.State.revertability st
               (M.Revert.Selection.changes
                  (List.map M.Change.id
                     (M.State.changes st ~claim:(claim "c1"))))));
      test "an opaque-only claim still projects incomplete after a restart"
        (fun () ->
          let events = [ observed_event ~turn:"t1" ~claim:"c1" [ watched ] ] in
          let answer st =
            M.State.revertability st (M.Revert.Selection.turns [ turn "t1" ])
          in
          let st = state_exn events in
          equal revertability_value
            (M.Revertability.Incomplete
               [ M.Revertability.Observed { claim = claim "c1" } ])
            (answer st);
          let replayed =
            state_exn
              (List.map
                 (fun event ->
                   decode M.Event.jsont (encode M.Event.jsont event))
                 events)
          in
          equal revertability_value (answer st) (answer replayed));
      test "an uncertain stopping target degrades the selection" (fun () ->
          (* A commit-phase failure whose prefix touched p1 and whose stopping
             target is p2: a selection resolving either path — or naming p2 —
             answers Incomplete with the causal tie preserved. *)
          let error =
            apply_err ~fail_commit:(W.Path.equal p2)
              ~fs:[ (p1, "a1\n"); (p2, "a2\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"a1\n" ~after:"b1\n";
                Edit.rewrite ~path:p2 ~before:"a2\n" ~after:"b2\n";
              ]
          in
          let event =
            M.Event.of_attempt ~turn:(turn "t1") ~claim:(claim "c1") ~ordinal:0
              ~checkpoint:None
              ~applied:(Edit.Apply_error.applied error)
              ~uncertain:(Some p2)
          in
          let st = state_exn [ event ] in
          let uncertain =
            M.Revertability.Incomplete
              [ M.Revertability.Uncertain { claim = claim "c1"; path = p2 } ]
          in
          equal revertability_value uncertain
            (M.State.revertability st M.Revert.Selection.all);
          (* A paths scope naming only the uncertain target still answers
             Incomplete even though it resolves no change rows. *)
          equal revertability_value uncertain
            (M.State.revertability st (M.Revert.Selection.paths [ p2 ]));
          (* The answer survives an encode/decode restart. *)
          let replayed =
            state_exn [ decode M.Event.jsont (encode M.Event.jsont event) ]
          in
          equal revertability_value uncertain
            (M.State.revertability replayed M.Revert.Selection.all);
          (* The uncertain target is apply-level evidence, not an observed
             window: the confirmed prefix records p1 as an exact change, and p2
             surfaces only through the [Uncertain] revertability reason above. *)
          equal (list path_value) [] (M.State.observed st ~claim:(claim "c1"));
          equal (list path_value) [ p1 ]
            (List.map M.Change.path (M.State.changes st ~claim:(claim "c1"))));
      test "an unresolved revert is incomplete" (fun () ->
          let st = state_exn [ base_history () ] in
          let sel =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
          in
          let plan =
            prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st sel
          in
          let st' =
            state_exn
              [
                base_history (); before_revert plan; M.Event.revert_started plan;
              ]
          in
          equal revertability_value
            (M.Revertability.Incomplete
               [ M.Revertability.Unresolved_revert (rid "r1") ])
            (M.State.revertability st' sel));
      test "a superseded selection is unavailable" (fun () ->
          let e1 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2"
              ~fs:[ (p1, "b\n") ]
              [ Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n" ]
          in
          let st = state_exn [ e1; e2 ] in
          let by =
            match M.State.changes st ~claim:(claim "c2") with
            | [ change ] -> M.Change.id change
            | _ -> fail "expected one row"
          in
          equal revertability_value
            (M.Revertability.Unavailable
               [ M.Revertability.Superseded { path = p1; by } ])
            (M.State.revertability st
               (M.Revert.Selection.changes
                  (List.map M.Change.id
                     (M.State.changes st ~claim:(claim "c1"))))));
      test "unavailable outranks incomplete and reasons accumulate" (fun () ->
          (* p1 has a gap (incomplete grade) and its net-after is not the
             head (superseded proof): the answer is Unavailable carrying
             both reasons. Selecting only the first change makes the gap
             entry superseded by the second recorded change. *)
          let events = gap_history () in
          let st = state_exn events in
          let first_id =
            match M.State.changes st ~claim:(claim "c1") with
            | [ change ] -> M.Change.id change
            | _ -> fail "expected one row"
          in
          let by =
            match M.State.changes st ~claim:(claim "c2") with
            | [ change ] -> M.Change.id change
            | _ -> fail "expected one row"
          in
          equal revertability_value
            (M.Revertability.Unavailable
               [ M.Revertability.Superseded { path = p1; by } ])
            (M.State.revertability st (M.Revert.Selection.changes [ first_id ]));
          let full = M.State.revertability st M.Revert.Selection.all in
          equal revertability_value
            (M.Revertability.Incomplete
               [ M.Revertability.Non_contiguous { path = p1 } ])
            full);
      test "superseded is decided by image equality, not recency" (fun () ->
          (* Later changes return the path to the selection's net-after, so
             reverting the first change erases nothing recorded. *)
          let e1 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2"
              ~fs:[ (p1, "b\n") ]
              [ Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n" ]
          in
          let e3 =
            edit_event ~turn:"t3" ~claim:"c3"
              ~fs:[ (p1, "c\n") ]
              [ Edit.rewrite ~path:p1 ~before:"c\n" ~after:"b\n" ]
          in
          let st = state_exn [ e1; e2; e3 ] in
          equal revertability_value M.Revertability.Available
            (M.State.revertability st
               (M.Revert.Selection.changes
                  (List.map M.Change.id
                     (M.State.changes st ~claim:(claim "c1"))))));
      test "an empty resolution with no in-scope observation is available"
        (fun () ->
          let st =
            state_exn
              [
                base_history ();
                observed_event ~turn:"t2" ~claim:"c3" [ watched ];
              ]
          in
          equal revertability_value M.Revertability.Available
            (M.State.revertability st
               (M.Revert.Selection.changes
                  [ M.Change.Id.of_string "no-such-change" ])));
      test
        "a turn scope resolving nothing but containing an observation is \
         incomplete" (fun () ->
          let st =
            state_exn
              [
                base_history ();
                observed_event ~turn:"t7" ~claim:"c3" [ watched ];
              ]
          in
          equal revertability_value
            (M.Revertability.Incomplete
               [ M.Revertability.Observed { claim = claim "c3" } ])
            (M.State.revertability st (M.Revert.Selection.turns [ turn "t7" ])));
      test "a paths scope over an observed-only path is incomplete" (fun () ->
          (* An observation on a path in the selection's own paths is in
             scope even when the selection resolves no change rows: a paths
             scope over an observed-only span has something to protect and
             must not answer a vacuous Available. *)
          let st =
            state_exn [ observed_event ~turn:"t1" ~claim:"c1" [ watched ] ]
          in
          equal revertability_value
            (M.Revertability.Incomplete
               [ M.Revertability.Observed { claim = claim "c1" } ])
            (M.State.revertability st (M.Revert.Selection.paths [ watched ])));
      test "restoration rows update the ledger head" (fun () ->
          let events, plan, settled = rich_ledger () in
          let st = state_exn events in
          ignore settled;
          let original =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
          in
          let restoration_p1 =
            revert_derive
              (M.Revert.Id.to_string
                 (M.Revert.Plan.started plan).M.Revert.Started.id)
              p1
          in
          (match M.State.head st p1 with
          | Some head -> equal change_id_value restoration_p1 (M.Change.id head)
          | None -> fail "expected a head for p1");
          match M.State.revertability st original with
          | M.Revertability.Unavailable reasons ->
              is_true ~msg:"superseded by the restoration row"
                (List.exists
                   (function
                     | M.Revertability.Superseded { by; _ } ->
                         M.Change.Id.equal by restoration_p1
                     | _ -> false)
                   reasons)
          | other ->
              failf "expected Unavailable, got %a" M.Revertability.pp other);
    ]

(* Selection canonicalization. *)

let selection_group =
  group "selection"
    [
      test "order and duplication do not change equality" (fun () ->
          equal selection_value
            (M.Revert.Selection.turns [ turn "a"; turn "b" ])
            (M.Revert.Selection.turns [ turn "b"; turn "a"; turn "a" ]);
          equal selection_value
            (M.Revert.Selection.paths [ p1; p2 ])
            (M.Revert.Selection.paths [ p2; p1; p2 ]);
          equal selection_value
            (M.Revert.Selection.changes
               [ M.Change.Id.of_string "x"; M.Change.Id.of_string "y" ])
            (M.Revert.Selection.changes
               [
                 M.Change.Id.of_string "y";
                 M.Change.Id.of_string "x";
                 M.Change.Id.of_string "x";
               ]));
      test "empty selections are refused" (fun () ->
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.Selection.turns: selection must not be \
               empty"
            "turns" (fun () -> M.Revert.Selection.turns []);
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.Selection.changes: selection must not be \
               empty"
            "changes" (fun () -> M.Revert.Selection.changes []);
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.Selection.paths: selection must not be \
               empty"
            "paths" (fun () -> M.Revert.Selection.paths []));
      test "resolution does not depend on caller list order" (fun () ->
          let st = state_exn [ base_history () ] in
          let ids =
            List.map M.Change.id (M.State.changes st ~claim:(claim "c1"))
          in
          equal (list net_entry_value)
            (M.State.net st (M.Revert.Selection.changes ids))
            (M.State.net st (M.Revert.Selection.changes (List.rev ids))));
      test "selections round-trip and reject non-canonical payloads" (fun () ->
          let codec = M.Revert.Selection.jsont in
          List.iter
            (fun selection ->
              roundtrip "selection" codec M.Revert.Selection.equal selection)
            [
              M.Revert.Selection.all;
              M.Revert.Selection.turns [ turn "a"; turn "b" ];
              M.Revert.Selection.changes [ M.Change.Id.of_string "x" ];
              M.Revert.Selection.paths [ p1; p2 ];
            ];
          let unsorted =
            set_member "turns"
              (Json.list [ Json.string "b"; Json.string "a" ])
              (encode codec (M.Revert.Selection.turns [ turn "a"; turn "b" ]))
          in
          assert_decode_error ~contains:"deduplicated and sorted" "unsorted"
            codec unsorted;
          let duplicated =
            set_member "turns"
              (Json.list [ Json.string "a"; Json.string "a" ])
              (encode codec (M.Revert.Selection.turns [ turn "a"; turn "b" ]))
          in
          assert_decode_error ~contains:"deduplicated and sorted" "duplicated"
            codec duplicated;
          let empty =
            set_member "turns" (Json.list [])
              (encode codec (M.Revert.Selection.turns [ turn "a" ]))
          in
          assert_decode_error ~contains:"must not be empty" "empty" codec empty;
          assert_decode_error "unknown tag" codec
            (json_object [ ("type", Json.string "scope") ]);
          assert_decode_error "cross-arm member" codec
            (add_member "paths" (Json.list [])
               (encode codec M.Revert.Selection.all)));
    ]

(* Preparation. *)

let preparation_group =
  group "preparation"
    [
      test "a clean preparation freezes current reads and lowers the edit"
        (fun () ->
          let st = state_exn [ base_history () ] in
          let plan =
            prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st
              M.Revert.Selection.all
          in
          let started = M.Revert.Plan.started plan in
          equal selection_value M.Revert.Selection.all
            started.M.Revert.Started.selection;
          (match started.M.Revert.Started.targets with
          | [ t1'; t2' ] ->
              equal path_value p1 t1'.M.Revert.Target.path;
              equal image_value (text_image "b1\n") t1'.M.Revert.Target.expected;
              equal image_value (text_image "a1\n") t1'.M.Revert.Target.restore;
              equal path_value p2 t2'.M.Revert.Target.path;
              equal (list change_id_value)
                [ claim_derive "c1" p1 ]
                t1'.M.Revert.Target.sources
          | targets ->
              failf "expected two targets, got %d" (List.length targets));
          equal (list change_id_value)
            (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
            (M.Revert.Started.sources started);
          (* The lowered plan's preconditions are the frozen expected
             images: applying against the frozen texts succeeds and
             restores the net-before contents. *)
          let result =
            apply_plan_ok
              ~fs:[ (p1, "b1\n"); (p2, "b2\n") ]
              (M.Revert.Plan.edit plan)
          in
          equal int 2 (List.length (Edit.Result.entries result));
          (* Applying against anything else fails the precondition. *)
          let error =
            apply_plan_err
              ~fs:[ (p1, "tampered\n"); (p2, "b2\n") ]
              (M.Revert.Plan.edit plan)
          in
          check "stale apply stops in preflight"
            (Edit.Apply_error.phase error = Edit.Apply_error.Phase.Preflight));
      test "a stale read refuses with the recorded and observed images"
        (fun () ->
          let st = state_exn [ base_history () ] in
          let problems =
            prepare_err ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:
                     [
                       (p1, Edit.Observed.Text "drifted\n");
                       (p2, Edit.Observed.Text "b2\n");
                     ]
                   ~blobs:[ "a1\n"; "a2\n" ] ())
              st M.Revert.Selection.all
          in
          equal (list problem_value)
            [
              M.Revert.Problem.Stale
                {
                  path = p1;
                  expected = text_image "b1\n";
                  actual = text_image "drifted\n";
                };
            ]
            problems);
      test "a non-text read refuses as unreadable" (fun () ->
          let st = state_exn [ base_history () ] in
          let problems =
            prepare_err ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:
                     [
                       (p1, Edit.Observed.Other); (p2, Edit.Observed.Text "b2\n");
                     ]
                   ~blobs:[ "a1\n"; "a2\n" ] ())
              st M.Revert.Selection.all
          in
          equal (list problem_value)
            [ M.Revert.Problem.Unreadable { path = p1 } ]
            problems);
      test "a missing current read refuses" (fun () ->
          let st = state_exn [ base_history () ] in
          let problems =
            prepare_err ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p2, Edit.Observed.Text "b2\n") ]
                   ~blobs:[ "a1\n"; "a2\n" ] ())
              st M.Revert.Selection.all
          in
          equal (list problem_value)
            [ M.Revert.Problem.Missing_read p1 ]
            problems);
      test "a missing restore blob refuses and mints no partial plan" (fun () ->
          let st = state_exn [ base_history () ] in
          let problems =
            prepare_err ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:
                     [
                       (p1, Edit.Observed.Text "b1\n");
                       (p2, Edit.Observed.Text "b2\n");
                     ]
                   ~blobs:[ "a2\n" ] ())
              st M.Revert.Selection.all
          in
          equal (list problem_value)
            [
              M.Revert.Problem.Missing_blob { path = p1; content = cref "a1\n" };
            ]
            problems);
      test "problems accumulate across targets in one refusal" (fun () ->
          let st = state_exn [ base_history () ] in
          let problems =
            prepare_err ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:
                     [
                       (p1, Edit.Observed.Text "drifted\n");
                       (p2, Edit.Observed.Text "b2\n");
                     ]
                   ~blobs:[ "a1\n" ] ())
              st M.Revert.Selection.all
          in
          equal (list problem_value)
            [
              M.Revert.Problem.Stale
                {
                  path = p1;
                  expected = text_image "b1\n";
                  actual = text_image "drifted\n";
                };
              M.Revert.Problem.Missing_blob { path = p2; content = cref "a2\n" };
            ]
            problems);
      test "a selection resolving nothing refuses with nothing to revert"
        (fun () ->
          let st = state_exn [ base_history () ] in
          let problems =
            prepare_err ~id:(rid "r1") ~evidence:(evidence ()) st
              (M.Revert.Selection.changes
                 [ M.Change.Id.of_string "no-such-change" ])
          in
          equal (list problem_value)
            [ M.Revert.Problem.Nothing_to_revert ]
            problems);
      test "a superseded target refuses" (fun () ->
          let e1 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "a\n") ]
              [ Edit.rewrite ~path:p1 ~before:"a\n" ~after:"b\n" ]
          in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2"
              ~fs:[ (p1, "b\n") ]
              [ Edit.rewrite ~path:p1 ~before:"b\n" ~after:"c\n" ]
          in
          let st = state_exn [ e1; e2 ] in
          let by =
            match M.State.changes st ~claim:(claim "c2") with
            | [ change ] -> M.Change.id change
            | _ -> fail "expected one row"
          in
          let problems =
            prepare_err ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p1, Edit.Observed.Text "b\n") ]
                   ~blobs:[ "a\n" ] ())
              st
              (M.Revert.Selection.changes
                 (List.map M.Change.id (M.State.changes st ~claim:(claim "c1"))))
          in
          equal (list problem_value)
            [ M.Revert.Problem.Superseded { path = p1; by } ]
            problems);
      test "preparation refuses while a revert is unresolved" (fun () ->
          let st = state_exn [ base_history () ] in
          let sel =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
          in
          let plan =
            prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st sel
          in
          let st' =
            state_exn
              [
                base_history (); before_revert plan; M.Event.revert_started plan;
              ]
          in
          let problems =
            prepare_err ~id:(rid "r2") ~evidence:(base_evidence ()) st' sel
          in
          equal (list problem_value)
            [ M.Revert.Problem.Unresolved_revert (rid "r1") ]
            problems);
      test "a non-contiguous target needs an explicit override" (fun () ->
          let st = state_exn (gap_history ()) in
          let problems =
            prepare_err ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p1, Edit.Observed.Text "vd\n") ]
                   ~blobs:[ "va\n" ] ())
              st M.Revert.Selection.all
          in
          equal (list problem_value)
            [ M.Revert.Problem.Needs_override { path = p1 } ]
            problems);
      test
        "an exactly-covering override mints the plan with the plan-time read \
         frozen" (fun () ->
          let st = state_exn (gap_history ()) in
          let plan =
            prepare_ok
              ~override:(M.Revert.Override.accept_unrecorded_loss [ p1 ])
              ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p1, Edit.Observed.Text "unrecorded\n") ]
                   ~blobs:[ "va\n" ] ())
              st M.Revert.Selection.all
          in
          (match (M.Revert.Plan.started plan).M.Revert.Started.targets with
          | [ target ] ->
              equal image_value
                (text_image "unrecorded\n")
                target.M.Revert.Target.expected;
              equal image_value (text_image "va\n")
                target.M.Revert.Target.restore
          | _ -> fail "expected one target");
          (* The consent is frozen into the started fact. *)
          ignore
            (require_some (M.Revert.Plan.started plan).M.Revert.Started.override));
      test "an override naming a contiguous path is refused" (fun () ->
          let st =
            state_exn
              (gap_history ()
              @ [
                  edit_event ~turn:"t3" ~claim:"c5"
                    ~fs:[ (p2, "x\n") ]
                    [ Edit.rewrite ~path:p2 ~before:"x\n" ~after:"y\n" ];
                ])
          in
          let problems =
            prepare_err
              ~override:(M.Revert.Override.accept_unrecorded_loss [ p1; p2 ])
              ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:
                     [
                       (p1, Edit.Observed.Text "vd\n");
                       (p2, Edit.Observed.Text "y\n");
                     ]
                   ~blobs:[ "va\n"; "x\n" ] ())
              st M.Revert.Selection.all
          in
          equal (list problem_value)
            [ M.Revert.Problem.Unmatched_override { path = p2 } ]
            problems);
      test "an override naming an unresolved path is refused" (fun () ->
          let st = state_exn (gap_history ()) in
          let stray = wpath "stray.txt" in
          let problems =
            prepare_err
              ~override:(M.Revert.Override.accept_unrecorded_loss [ p1; stray ])
              ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p1, Edit.Observed.Text "vd\n") ]
                   ~blobs:[ "va\n" ] ())
              st M.Revert.Selection.all
          in
          equal (list problem_value)
            [ M.Revert.Problem.Unmatched_override { path = stray } ]
            problems);
      test "an overridden target whose read equals its restore image drops"
        (fun () ->
          let st = state_exn (gap_history ()) in
          let problems =
            prepare_err
              ~override:(M.Revert.Override.accept_unrecorded_loss [ p1 ])
              ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p1, Edit.Observed.Text "va\n") ]
                   ~blobs:[ "va\n" ] ())
              st M.Revert.Selection.all
          in
          equal (list problem_value)
            [ M.Revert.Problem.Nothing_to_revert ]
            problems);
      test "a dropped target leaves the remaining plan intact" (fun () ->
          let st =
            state_exn
              (gap_history ()
              @ [
                  edit_event ~turn:"t3" ~claim:"c5"
                    ~fs:[ (p2, "x\n") ]
                    [ Edit.rewrite ~path:p2 ~before:"x\n" ~after:"y\n" ];
                ])
          in
          let plan =
            prepare_ok
              ~override:(M.Revert.Override.accept_unrecorded_loss [ p1 ])
              ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:
                     [
                       (p1, Edit.Observed.Text "va\n");
                       (p2, Edit.Observed.Text "y\n");
                     ]
                   ~blobs:[ "va\n"; "x\n" ] ())
              st M.Revert.Selection.all
          in
          (match (M.Revert.Plan.started plan).M.Revert.Started.targets with
          | [ target ] -> equal path_value p2 target.M.Revert.Target.path
          | targets -> failf "expected one target, got %d" (List.length targets));
          (* The frozen consent narrows to the surviving targets: the dropped
             p1 is untouched by the plan, and p2 is contiguous, so no consent
             is frozen at all — and the started invariant (override paths are
             target paths) holds. *)
          equal (option pass) None
            (M.Revert.Plan.started plan).M.Revert.Started.override);
      test "evidence refuses duplicate reads and mismatched blob bytes"
        (fun () ->
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.Evidence.make: duplicate current read \
               for a path"
            "duplicate read" (fun () ->
              M.Revert.Evidence.make
                ~current:
                  [ (p1, Edit.Observed.Missing); (p1, Edit.Observed.Missing) ]
                ~blobs:[]);
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.Evidence.make: blob bytes do not match \
               their reference" "blob mismatch" (fun () ->
              M.Revert.Evidence.make ~current:[] ~blobs:[ (cref "a\n", "b\n") ]));
      test "an empty override is refused" (fun () ->
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.Override.accept_unrecorded_loss: paths \
               must not be empty" "empty override" (fun () ->
              M.Revert.Override.accept_unrecorded_loss []));
    ]

(* Lifecycle classification. *)

(* A three-target plan over p1, p2, p3 with the current texts b*/restores a*. *)
let three_target_plan () =
  let event =
    edit_event ~turn:"t1" ~claim:"c1"
      ~fs:[ (p1, "a1\n"); (p2, "a2\n"); (p3, "a3\n") ]
      [
        Edit.rewrite ~path:p1 ~before:"a1\n" ~after:"b1\n";
        Edit.rewrite ~path:p2 ~before:"a2\n" ~after:"b2\n";
        Edit.rewrite ~path:p3 ~before:"a3\n" ~after:"b3\n";
      ]
  in
  let st = state_exn [ event ] in
  let plan =
    prepare_ok ~id:(rid "r1")
      ~evidence:
        (evidence
           ~current:
             [
               (p1, Edit.Observed.Text "b1\n");
               (p2, Edit.Observed.Text "b2\n");
               (p3, Edit.Observed.Text "b3\n");
             ]
           ~blobs:[ "a1\n"; "a2\n"; "a3\n" ] ())
      st M.Revert.Selection.all
  in
  (st, event, plan)

let outcomes_of (settled : M.Revert.Settled.t) =
  settled.M.Revert.Settled.outcomes

let outcome_tag = function
  | M.Revert.Settled.Confirmed _ -> "confirmed"
  | M.Revert.Settled.Ambiguous -> "ambiguous"
  | M.Revert.Settled.Not_attempted -> "not_attempted"

let lifecycle_group =
  group "lifecycle classification"
    [
      test "full success confirms every target with restoration rows" (fun () ->
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          let result =
            apply_plan_ok
              ~fs:[ (p1, "b1\n"); (p2, "b2\n"); (p3, "b3\n") ]
              (M.Revert.Plan.edit plan)
          in
          let settled = M.Revert.settle started (Ok result) in
          equal (list string)
            [ "confirmed"; "confirmed"; "confirmed" ]
            (List.map (fun (_, o) -> outcome_tag o) (outcomes_of settled));
          (* Restoration rows: before is the frozen expected image, after
             the restored image; ids derive from the revert id and path —
             the per-path derivation law. *)
          List.iter2
            (fun (target : M.Revert.Target.t) row ->
              equal path_value target.M.Revert.Target.path (M.Change.path row);
              equal image_value target.M.Revert.Target.expected
                (M.Change.before row);
              equal image_value target.M.Revert.Target.restore
                (M.Change.after row);
              equal change_id_value
                (revert_derive "r1" (M.Change.path row))
                (M.Change.id row))
            started.M.Revert.Started.targets settled.M.Revert.Settled.changes;
          (* Confirmed outcomes name their own path's row. *)
          List.iter
            (fun (path, outcome) ->
              match outcome with
              | M.Revert.Settled.Confirmed id ->
                  equal change_id_value (revert_derive "r1" path) id
              | _ -> fail "expected confirmed")
            (outcomes_of settled);
          check "disposition"
            (settled.M.Revert.Settled.disposition
            = M.Revert.Settled.Applied { failure = None }));
      test "a preflight failure marks every target not attempted" (fun () ->
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          let error =
            apply_plan_err
              ~fs:[ (p1, "tampered\n"); (p2, "b2\n"); (p3, "b3\n") ]
              (M.Revert.Plan.edit plan)
          in
          let settled = M.Revert.settle started (Error error) in
          equal (list string)
            [ "not_attempted"; "not_attempted"; "not_attempted" ]
            (List.map (fun (_, o) -> outcome_tag o) (outcomes_of settled));
          equal (list change_value) [] settled.M.Revert.Settled.changes;
          match settled.M.Revert.Settled.disposition with
          | M.Revert.Settled.Applied { failure = Some failure } ->
              check "phase"
                (failure.M.Revert.Settled.Failure.phase
               = M.Revert.Settled.Failure.Preflight);
              check "kind"
                (M.Revert.Edit_failure.kind
                   failure.M.Revert.Settled.Failure.error
                = M.Revert.Edit_failure.Conflict)
          | _ -> fail "expected an applied failure");
      test
        "a commit failure confirms the prefix, marks the stopping target \
         ambiguous, and leaves the suffix not attempted" (fun () ->
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          let error =
            apply_plan_err ~fail_commit:(W.Path.equal p2)
              ~fs:[ (p1, "b1\n"); (p2, "b2\n"); (p3, "b3\n") ]
              (M.Revert.Plan.edit plan)
          in
          let settled = M.Revert.settle started (Error error) in
          equal (list string)
            [ "confirmed"; "ambiguous"; "not_attempted" ]
            (List.map (fun (_, o) -> outcome_tag o) (outcomes_of settled));
          equal (list path_value) [ p1; p2; p3 ]
            (List.map fst (outcomes_of settled));
          (match settled.M.Revert.Settled.changes with
          | [ row ] ->
              equal path_value p1 (M.Change.path row);
              equal image_value (text_image "b1\n") (M.Change.before row);
              equal image_value (text_image "a1\n") (M.Change.after row)
          | rows ->
              failf "expected one restoration row, got %d" (List.length rows));
          match settled.M.Revert.Settled.disposition with
          | M.Revert.Settled.Applied { failure = Some failure } -> (
              match failure.M.Revert.Settled.Failure.phase with
              | M.Revert.Settled.Failure.Commit { target } ->
                  equal path_value p2 target;
                  check "kind"
                    (M.Revert.Edit_failure.kind
                       failure.M.Revert.Settled.Failure.error
                    = M.Revert.Edit_failure.Io)
              | M.Revert.Settled.Failure.Preflight ->
                  fail "expected a commit phase")
          | _ -> fail "expected an applied failure");
      test
        "an outcome from a different apply with coinciding paths is a contract \
         breach" (fun () ->
          (* The frozen-image law (B5): the result covers the same paths but
             its transitions are not the frozen expected -> restore images,
             so it cannot be this plan's apply evidence. *)
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          let other =
            apply_ok
              ~fs:[ (p1, "z1\n"); (p2, "z2\n"); (p3, "z3\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"z1\n" ~after:"a1\n";
                Edit.rewrite ~path:p2 ~before:"z2\n" ~after:"a2\n";
                Edit.rewrite ~path:p3 ~before:"z3\n" ~after:"a3\n";
              ]
          in
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.settle: apply result entry does not \
               match the frozen expected image" "wrong before images" (fun () ->
              M.Revert.settle started (Ok other));
          let wrong_restore =
            apply_ok
              ~fs:[ (p1, "b1\n"); (p2, "b2\n"); (p3, "b3\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"b1\n" ~after:"z1\n";
                Edit.rewrite ~path:p2 ~before:"b2\n" ~after:"a2\n";
                Edit.rewrite ~path:p3 ~before:"b3\n" ~after:"a3\n";
              ]
          in
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.settle: apply result entry does not \
               match the frozen restore image" "wrong after image" (fun () ->
              M.Revert.settle started (Ok wrong_restore));
          (* The commit-prefix arm runs the same image checks. *)
          let prefix_error =
            apply_err ~fail_commit:(W.Path.equal p2)
              ~fs:[ (p1, "z1\n"); (p2, "b2\n"); (p3, "b3\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"z1\n" ~after:"a1\n";
                Edit.rewrite ~path:p2 ~before:"b2\n" ~after:"a2\n";
                Edit.rewrite ~path:p3 ~before:"b3\n" ~after:"a3\n";
              ]
          in
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.settle: apply result entry does not \
               match the frozen expected image" "wrong prefix image" (fun () ->
              M.Revert.settle started (Error prefix_error)));
      test "a result that does not cover the targets is a contract breach"
        (fun () ->
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          let short =
            apply_ok
              ~fs:[ (p1, "b1\n") ]
              [ Edit.rewrite ~path:p1 ~before:"b1\n" ~after:"a1\n" ]
          in
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.settle: apply result does not cover the \
               started targets" "short result" (fun () ->
              M.Revert.settle started (Ok short)));
      test "positionally swapped result entries are a contract breach"
        (fun () ->
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          let swapped =
            apply_ok
              ~fs:[ (p1, "b1\n"); (p2, "b2\n"); (p3, "b3\n") ]
              [
                Edit.rewrite ~path:p2 ~before:"b2\n" ~after:"a2\n";
                Edit.rewrite ~path:p1 ~before:"b1\n" ~after:"a1\n";
                Edit.rewrite ~path:p3 ~before:"b3\n" ~after:"a3\n";
              ]
          in
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.settle: apply result entry path does not \
               match its started target" "swapped entries" (fun () ->
              M.Revert.settle started (Ok swapped)));
      test
        "a commit error whose stopping target is out of position is a contract \
         breach" (fun () ->
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          (* An error minted from a reordered plan: applied [p2], stopping
             p1 — but the first unconfirmed started target is p1 only if
             nothing confirmed, and here one entry confirmed. *)
          let error =
            apply_err ~fail_commit:(W.Path.equal p1)
              ~fs:[ (p1, "b1\n"); (p2, "b2\n"); (p3, "b3\n") ]
              [
                Edit.rewrite ~path:p2 ~before:"b2\n" ~after:"a2\n";
                Edit.rewrite ~path:p1 ~before:"b1\n" ~after:"a1\n";
                Edit.rewrite ~path:p3 ~before:"b3\n" ~after:"a3\n";
              ]
          in
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.settle: stopping target is not the first \
               unconfirmed started target" "stopping mismatch" (fun () ->
              M.Revert.settle started (Error error)));
      test
        "a positionally shuffled applied prefix fails the pairwise pairing \
         check" (fun () ->
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          (* applied [p2; p1], stopping p3: the stopping target lines up,
             but the confirmed prefix pairs p1 with p2's entry — the
             positional path check rejects the mispairing. *)
          let error =
            apply_err ~fail_commit:(W.Path.equal p3)
              ~fs:[ (p1, "b1\n"); (p2, "b2\n"); (p3, "b3\n") ]
              [
                Edit.rewrite ~path:p2 ~before:"b2\n" ~after:"a2\n";
                Edit.rewrite ~path:p1 ~before:"b1\n" ~after:"a1\n";
                Edit.rewrite ~path:p3 ~before:"b3\n" ~after:"a3\n";
              ]
          in
          expect_invalid_arg
            ~expected:
              "Mentat_mutation.Revert.settle: apply result entry path does not \
               match its started target" "shuffled prefix" (fun () ->
              M.Revert.settle started (Error error)));
      test
        "settle_ambiguous marks every frozen target ambiguous from the record \
         alone" (fun () ->
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          let settled = M.Revert.settle_ambiguous started in
          equal (list string)
            [ "ambiguous"; "ambiguous"; "ambiguous" ]
            (List.map (fun (_, o) -> outcome_tag o) (outcomes_of settled));
          equal (list path_value) [ p1; p2; p3 ]
            (List.map fst (outcomes_of settled));
          equal (list change_value) [] settled.M.Revert.Settled.changes;
          check "recovered"
            (settled.M.Revert.Settled.disposition = M.Revert.Settled.Recovered);
          (* A pure function of the started record. *)
          equal settled_value settled (M.Revert.settle_ambiguous started));
      test
        "the crash path: an unresolved start settles ambiguous and unblocks \
         the ledger" (fun () ->
          let st = state_exn [ base_history () ] in
          let sel =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
          in
          let plan =
            prepare_ok ~id:(rid "r1") ~evidence:(base_evidence ()) st sel
          in
          (* Crash after the started append: replay exposes the unresolved
             revert; recovery settles from the frozen record. *)
          let crashed =
            state_exn
              [
                base_history (); before_revert plan; M.Event.revert_started plan;
              ]
          in
          let unresolved = M.State.unresolved_reverts crashed in
          equal (list started_value) [ M.Revert.Plan.started plan ] unresolved;
          let settled = M.Revert.settle_ambiguous (List.hd unresolved) in
          let recovered =
            state_exn
              [
                base_history ();
                before_revert plan;
                M.Event.revert_started plan;
                M.Event.revert_settled settled;
              ]
          in
          equal (list started_value) [] (M.State.unresolved_reverts recovered);
          (* Ambiguous settlement records no restoration rows, so heads are
             unchanged and the original selection stays answerable. *)
          equal revertability_value M.Revertability.Available
            (M.State.revertability recovered sel));
      test "the full walk lands in a valid ledger with moved heads" (fun () ->
          let events, _, _ = rich_ledger () in
          let st = state_exn events in
          equal (list started_value) [] (M.State.unresolved_reverts st);
          (* The reverted paths net away; the untouched create and the
             observation remain. *)
          let entries = M.State.net st M.Revert.Selection.all in
          equal (list path_value) [ p3 ]
            (List.map
               (fun (e : M.Change.Net.entry) -> e.M.Change.Net.path)
               entries));
    ]

(* Totals and hunks. *)

let totals_group =
  group "totals and hunks"
    [
      test "totals count distinct row paths and sum recorded lines" (fun () ->
          let rows =
            changes_of
              (edit_event ~turn:"t1" ~claim:"c1"
                 ~fs:[ (p2, "a\nb\n") ]
                 [
                   Edit.create ~path:p1 ~contents:"one\ntwo\n";
                   Edit.rewrite ~path:p2 ~before:"a\nb\n" ~after:"a\nc\n";
                 ])
            @ changes_of
                (edit_event ~turn:"t2" ~claim:"c2"
                   ~fs:[ (p1, "one\ntwo\n") ]
                   [ Edit.rewrite ~path:p1 ~before:"one\ntwo\n" ~after:"one\n" ])
          in
          equal stats_value
            (Textdiff.Stats.v ~files:2 ~additions:3 ~deletions:2)
            (M.Change.of_changes rows));
      test "empty totals are zero" (fun () ->
          equal stats_value
            (Textdiff.Stats.v ~files:0 ~additions:0 ~deletions:0)
            (M.Change.of_changes []));
      test "hunks resolve recorded texts and report a missing blob" (fun () ->
          let change =
            match
              changes_of
                (edit_event ~turn:"t1" ~claim:"c1"
                   ~fs:[ (p1, "a\nb\n") ]
                   [ Edit.rewrite ~path:p1 ~before:"a\nb\n" ~after:"a\nc\n" ])
            with
            | [ change ] -> change
            | _ -> fail "expected one row"
          in
          let store =
            [ (cref "a\nb\n", "a\nb\n"); (cref "a\nc\n", "a\nc\n") ]
          in
          let blob reference =
            List.find_map
              (fun (r, contents) ->
                if Mentat_digest.Content_ref.equal r reference then
                  Some contents
                else None)
              store
          in
          ignore (require_ok (M.Change.hunks ~blob change));
          match M.Change.hunks ~blob:(fun _ -> None) change with
          | Error (M.Change.Hunks_error.Missing_blob reference) ->
              check "names the before image"
                (Mentat_digest.Content_ref.equal reference (cref "a\nb\n"))
          | Error M.Change.Hunks_error.Bound_exceeded -> fail "wrong error"
          | Ok _ -> fail "expected a missing blob");
    ]

(* Codecs. *)

(* The edit event JSON for tampering. *)
let edit_json () = event_json (base_history ())

let tamper_change f json =
  let changes = get_member "changes" json in
  set_member "changes" (set_element 0 (f (get_element 0 changes)) changes) json

let codec_group =
  group "codecs"
    [
      test "every event arm round-trips" (fun () ->
          let events, plan, settled = rich_ledger () in
          ignore plan;
          ignore settled;
          List.iter
            (fun event -> roundtrip "arm" M.Event.jsont M.Event.equal event)
            events;
          (* Degraded checkpoints and recovered settlements too. *)
          roundtrip "degraded" M.Event.jsont M.Event.equal
            (M.Event.checkpoint
               (checkpoint ~capture:degraded
                  (M.Checkpoint.After_turn (turn "t1"))));
          let st = state_exn [ base_history () ] in
          let plan =
            prepare_ok ~id:(rid "r9") ~evidence:(base_evidence ()) st
              M.Revert.Selection.all
          in
          roundtrip "recovered" M.Event.jsont M.Event.equal
            (M.Event.revert_settled
               (M.Revert.settle_ambiguous (M.Revert.Plan.started plan)));
          (* A commit-phase failure settlement. *)
          let error =
            apply_plan_err ~fail_commit:(W.Path.equal p1)
              ~fs:[ (p1, "b1\n"); (p2, "b2\n") ]
              (M.Revert.Plan.edit plan)
          in
          roundtrip "commit failure" M.Event.jsont M.Event.equal
            (M.Event.revert_settled
               (M.Revert.settle (M.Revert.Plan.started plan) (Error error)));
          (* A later-ordinal apply and an uncertain-target event. *)
          roundtrip "second apply" M.Event.jsont M.Event.equal
            (edit_event ~ordinal:1 ~turn:"t1" ~claim:"c1"
               ~fs:[ (p1, "b1\n") ]
               [ Edit.rewrite ~path:p1 ~before:"b1\n" ~after:"c1\n" ]);
          let uncertain_event =
            let error =
              apply_err ~fail_commit:(W.Path.equal p1)
                ~fs:[ (p1, "a1\n") ]
                [ Edit.rewrite ~path:p1 ~before:"a1\n" ~after:"b1\n" ]
            in
            M.Event.of_attempt ~turn:(turn "t1") ~claim:(claim "c9") ~ordinal:0
              ~checkpoint:None
              ~applied:(Edit.Apply_error.applied error)
              ~uncertain:(Some p1)
          in
          roundtrip "uncertain target" M.Event.jsont M.Event.equal
            uncertain_event);
      test "replaying a decoded ledger reproduces every projection" (fun () ->
          let events = rich_events () in
          let replayed =
            List.map
              (fun event -> decode M.Event.jsont (encode M.Event.jsont event))
              events
          in
          assert_state_equiv "restart" (state_exn events) (state_exn replayed));
      test "retired wire shapes fail loudly" (fun () ->
          (* Record-style tags the wire format does not define. *)
          assert_decode_error "record tag" M.Event.jsont
            (json_object [ ("type", Json.string "record") ]);
          assert_decode_error "checkpoint record tag" M.Event.jsont
            (json_object [ ("type", Json.string "Checkpoint") ]);
          (* The tool-authored move op is not a wire tag. *)
          assert_decode_error "move member" M.Event.jsont
            (tamper_change
               (fun change -> add_member "op" (Json.string "move") change)
               (edit_json ()));
          (* The stored revertability member is deleted. *)
          assert_decode_error "revertability member" M.Event.jsont
            (tamper_change
               (fun change ->
                 add_member "revertability" (Json.string "revertable") change)
               (edit_json ()));
          (* The reserved failed settlement outcome is deleted. *)
          let events, _, settled = rich_ledger () in
          ignore events;
          let settled_json = event_json (M.Event.revert_settled settled) in
          let body = get_member "settled" settled_json in
          let outcomes = get_member "outcomes" body in
          assert_decode_error "failed outcome tag" M.Event.jsont
            (set_member "settled"
               (set_member "outcomes"
                  (set_element 0
                     (set_member "outcome"
                        (json_object [ ("type", Json.string "failed") ])
                        (get_element 0 outcomes))
                     outcomes)
                  body)
               settled_json);
          (* The checkpoint root member is deleted: identity is the boundary
             and a capture covers the whole workspace. *)
          let cp = checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")) in
          assert_decode_error "checkpoint root member" M.Checkpoint.jsont
            (add_member "root" (Json.string "ws")
               (encode M.Checkpoint.jsont cp)));
      test "unknown tags and members are structured errors" (fun () ->
          assert_decode_error "unknown event tag" M.Event.jsont
            (json_object [ ("type", Json.string "mystery") ]);
          assert_decode_error "unknown event member" M.Event.jsont
            (add_member "session" (Json.string "s1") (edit_json ()));
          assert_decode_error "unknown image tag" M.Image.jsont
            (json_object [ ("type", Json.string "binary") ]);
          assert_decode_error "cross-arm image member" M.Image.jsont
            (add_member "identity"
               (get_member "identity" (encode M.Image.jsont (text_image "x\n")))
               (encode M.Image.jsont M.Image.Missing)));
      test "the edit event re-validates its local invariants" (fun () ->
          assert_decode_error ~contains:"changes must not be empty" "empty"
            M.Event.jsont
            (set_member "changes" (Json.list []) (edit_json ()));
          (* Duplicate paths within one event. *)
          let json = edit_json () in
          let changes = get_member "changes" json in
          let duplicated =
            set_member "changes"
              (set_element 1 (get_element 0 changes) changes)
              json
          in
          assert_decode_error ~contains:"duplicate" "duplicate paths"
            M.Event.jsont duplicated;
          (* A negative ordinal and a missing ordinal member are refused. *)
          assert_decode_error ~contains:"non-negative" "negative ordinal"
            M.Event.jsont
            (set_member "ordinal" (Json.int (-1)) json);
          (* An uncertain target duplicating a confirmed change path. *)
          let change0 = get_element 0 changes in
          assert_decode_error ~contains:"duplicates a confirmed change path"
            "uncertain collision" M.Event.jsont
            (add_member "uncertain" (get_member "path" change0) json));
      test "a tampered change id fails the claim derivation" (fun () ->
          let json =
            tamper_change
              (fun change ->
                set_member "id" (Json.string (String.make 64 'e')) change)
              (edit_json ())
          in
          assert_decode_error ~contains:"does not derive" "forged id"
            M.Event.jsont json);
      test "an empty id string is rejected" (fun () ->
          assert_decode_error ~contains:"must not be empty" "empty id"
            M.Event.jsont
            (tamper_change
               (fun change -> set_member "id" (Json.string "") change)
               (edit_json ())));
      test "a missing-to-missing transition is rejected at decode" (fun () ->
          let missing = encode M.Image.jsont M.Image.Missing in
          assert_decode_error ~contains:"missing-to-missing" "invalid pair"
            M.Event.jsont
            (tamper_change
               (fun change ->
                 set_member "before" missing (set_member "after" missing change))
               (edit_json ())));
      test "a byte-identical text transition is rejected at decode" (fun () ->
          assert_decode_error ~contains:"must alter the file" "unchanged text"
            M.Event.jsont
            (tamper_change
               (fun change ->
                 set_member "after" (get_member "before" change) change)
               (edit_json ())));
      test "negative line counts are rejected at decode" (fun () ->
          assert_decode_error ~contains:"non-negative" "negative" M.Event.jsont
            (tamper_change
               (fun change -> set_member "additions" (Json.int (-1)) change)
               (edit_json ())));
      test "tool_observed re-validates canonical paths" (fun () ->
          let json =
            event_json (observed_event ~turn:"t1" ~claim:"c1" [ p1; p2 ])
          in
          let paths = get_member "paths" json in
          let swapped =
            set_member "paths"
              (set_element 0 (get_element 1 paths)
                 (set_element 1 (get_element 0 paths) paths))
              json
          in
          assert_decode_error ~contains:"deduplicated and sorted" "unsorted"
            M.Event.jsont swapped;
          let duplicated =
            set_member "paths" (set_element 1 (get_element 0 paths) paths) json
          in
          assert_decode_error ~contains:"deduplicated and sorted" "duplicated"
            M.Event.jsont duplicated;
          assert_decode_error ~contains:"must not be empty" "empty"
            M.Event.jsont
            (set_member "paths" (Json.list []) json));
      test "checkpoint ids are never serialized and re-derive on demand"
        (fun () ->
          let cp = checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")) in
          let json = encode M.Checkpoint.jsont cp in
          is_false ~msg:"no id member" (has_member "id" json);
          let event_json = encode M.Event.jsont (M.Event.checkpoint cp) in
          is_false ~msg:"no id member on the event"
            (has_member "id" (get_member "checkpoint" event_json));
          equal checkpoint_id_value (M.Checkpoint.id cp)
            (M.Checkpoint.id (decode M.Checkpoint.jsont json));
          (* Distinct boundaries derive distinct ids. *)
          not_equal checkpoint_id_value (M.Checkpoint.id cp)
            (M.Checkpoint.id (checkpoint (M.Checkpoint.After_turn (turn "t1")))));
      test "capture codecs reject negatives and unknown kinds" (fun () ->
          let cp = checkpoint (M.Checkpoint.Before_turn_tools (turn "t1")) in
          let json = encode M.Checkpoint.jsont cp in
          let capture = get_member "capture" json in
          assert_decode_error ~contains:"non-negative" "negative excluded"
            M.Checkpoint.jsont
            (set_member "capture"
               (set_member "excluded" (Json.int (-1)) capture)
               json);
          assert_decode_error "unknown capture tag" M.Checkpoint.jsont
            (set_member "capture"
               (set_member "type" (Json.string "partial") capture)
               json);
          let dcp =
            checkpoint ~capture:degraded
              (M.Checkpoint.Before_turn_tools (turn "t1"))
          in
          let djson = encode M.Checkpoint.jsont dcp in
          let failure = get_member "failure" (get_member "capture" djson) in
          assert_decode_error "unknown failure kind" M.Checkpoint.jsont
            (set_member "capture"
               (set_member "failure"
                  (set_member "kind" (Json.string "transient") failure)
                  (get_member "capture" djson))
               djson));
      test "started facts re-validate their invariants at decode" (fun () ->
          let st = state_exn (gap_history ()) in
          let plan =
            prepare_ok
              ~override:(M.Revert.Override.accept_unrecorded_loss [ p1 ])
              ~id:(rid "r1")
              ~evidence:
                (evidence
                   ~current:[ (p1, Edit.Observed.Text "vz\n") ]
                   ~blobs:[ "va\n" ] ())
              st M.Revert.Selection.all
          in
          let event = M.Event.revert_started plan in
          roundtrip "override round-trips inside started" M.Event.jsont
            M.Event.equal event;
          let json = event_json event in
          let started = get_member "started" json in
          assert_decode_error ~contains:"targets must not be empty"
            "empty targets" M.Event.jsont
            (set_member "started"
               (set_member "targets" (Json.list []) started)
               json);
          let targets = get_member "targets" started in
          let target = get_element 0 targets in
          assert_decode_error ~contains:"duplicate target paths"
            "duplicate targets" M.Event.jsont
            (set_member "started"
               (set_member "targets" (Json.list [ target; target ]) started)
               json);
          assert_decode_error ~contains:"must differ" "expected equals restore"
            M.Event.jsont
            (set_member "started"
               (set_member "targets"
                  (Json.list
                     [
                       set_member "expected"
                         (get_member "restore" target)
                         target;
                     ])
                  started)
               json);
          assert_decode_error ~contains:"sources must not be empty"
            "empty sources" M.Event.jsont
            (set_member "started"
               (set_member "targets"
                  (Json.list [ set_member "sources" (Json.list []) target ])
                  started)
               json);
          assert_decode_error ~contains:"deduplicated and sorted"
            "non-canonical override" M.Event.jsont
            (set_member "started"
               (set_member "override"
                  (json_object
                     [
                       ( "paths",
                         Json.list
                           [
                             get_element 0
                               (get_member "paths"
                                  (get_member "override" started));
                             get_element 0
                               (get_member "paths"
                                  (get_member "override" started));
                           ] );
                     ])
                  started)
               json);
          (* Every override path must be a target path. *)
          assert_decode_error ~contains:"outside the targets"
            "override outside targets" M.Event.jsont
            (set_member "started"
               (set_member "override"
                  (json_object
                     [
                       ( "paths",
                         Json.list [ encode W.Path.jsont (wpath "zz.txt") ] );
                     ])
                  started)
               json));
      test "settled facts re-validate outcome and row derivation at decode"
        (fun () ->
          let events, _, settled = rich_ledger () in
          ignore events;
          let json = event_json (M.Event.revert_settled settled) in
          let body = get_member "settled" json in
          let outcomes = get_member "outcomes" body in
          (* The regression pin: confirmed ids swapped across the two paths
             decode-fail on the per-path derivation. *)
          let outcome_of i = get_member "outcome" (get_element i outcomes) in
          let swap =
            set_member "outcomes"
              (set_element 0
                 (set_member "outcome" (outcome_of 1) (get_element 0 outcomes))
                 (set_element 1
                    (set_member "outcome" (outcome_of 0)
                       (get_element 1 outcomes))
                    outcomes))
              body
          in
          assert_decode_error ~contains:"confirmed outcome id does not derive"
            "swapped confirmed ids" M.Event.jsont
            (set_member "settled" swap json);
          (* A tampered restoration row id fails the row derivation. *)
          let rows = get_member "changes" body in
          let forged_row =
            set_member "id"
              (Json.string (String.make 64 'e'))
              (get_element 0 rows)
          in
          assert_decode_error ~contains:"restoration row id does not derive"
            "forged row id" M.Event.jsont
            (set_member "settled"
               (set_member "changes" (set_element 0 forged_row rows) body)
               json);
          (* Dropping a row breaks the confirmed/rows correspondence. *)
          assert_decode_error ~contains:"do not correspond" "missing row"
            M.Event.jsont
            (set_member "settled"
               (set_member "changes" (Json.list [ get_element 0 rows ]) body)
               json);
          (* Duplicate outcome paths and empty outcomes are refused. *)
          assert_decode_error ~contains:"duplicate outcome paths"
            "duplicate outcomes" M.Event.jsont
            (set_member "settled"
               (set_member "outcomes"
                  (set_element 1 (get_element 0 outcomes) outcomes)
                  body)
               json);
          assert_decode_error ~contains:"outcomes must not be empty"
            "empty outcomes" M.Event.jsont
            (set_member "settled"
               (set_member "outcomes" (Json.list [])
                  (set_member "changes" (Json.list []) body))
               json);
          (* Unknown outcome and disposition tags are refused. *)
          assert_decode_error "unknown outcome tag" M.Event.jsont
            (set_member "settled"
               (set_member "outcomes"
                  (set_element 0
                     (set_member "outcome"
                        (json_object [ ("type", Json.string "skipped") ])
                        (get_element 0 outcomes))
                     outcomes)
                  body)
               json);
          assert_decode_error "unknown disposition tag" M.Event.jsont
            (set_member "settled"
               (set_member "disposition"
                  (json_object [ ("type", Json.string "replayed") ])
                  body)
               json));
      test "settled facts re-validate disposition/outcome consistency at decode"
        (fun () ->
          let _, _, plan = three_target_plan () in
          let started = M.Revert.Plan.started plan in
          let fs = [ (p1, "b1\n"); (p2, "b2\n"); (p3, "b3\n") ] in
          let settled_json settled =
            event_json (M.Event.revert_settled settled)
          in
          (* Forged apply evidence: a clean apply's confirmed outcomes under
             a recovered disposition. *)
          let clean =
            settled_json
              (M.Revert.settle started
                 (Ok (apply_plan_ok ~fs (M.Revert.Plan.edit plan))))
          in
          assert_decode_error
            ~contains:"recovered settlement admits only ambiguous outcomes"
            "recovered with confirmed outcomes" M.Event.jsont
            (set_member "settled"
               (set_member "disposition"
                  (json_object [ ("type", Json.string "recovered") ])
                  (get_member "settled" clean))
               clean);
          (* Stripping the stopping failure forges a clean apply around an
             ambiguous outcome. *)
          let commit_failed =
            settled_json
              (M.Revert.settle started
                 (Error
                    (apply_plan_err ~fail_commit:(W.Path.equal p2) ~fs
                       (M.Revert.Plan.edit plan))))
          in
          assert_decode_error
            ~contains:"clean apply admits only confirmed outcomes"
            "stripped failure" M.Event.jsont
            (set_member "settled"
               (set_member "disposition"
                  (json_object [ ("type", Json.string "applied") ])
                  (get_member "settled" commit_failed))
               commit_failed);
          (* A commit failure's outcomes must be a confirmed prefix, one
             ambiguous outcome at the stopping target, and a not-attempted
             suffix: flattening the ambiguous outcome forges certainty. *)
          let commit_body = get_member "settled" commit_failed in
          let commit_outcomes = get_member "outcomes" commit_body in
          assert_decode_error
            ~contains:"requires the ambiguous outcome at the stopping target"
            "flattened ambiguous" M.Event.jsont
            (set_member "settled"
               (set_member "outcomes"
                  (set_element 1
                     (set_member "outcome"
                        (json_object [ ("type", Json.string "not_attempted") ])
                        (get_element 1 commit_outcomes))
                     commit_outcomes)
                  commit_body)
               commit_failed);
          (* A preflight failure never reaches a target. *)
          let preflight =
            settled_json
              (M.Revert.settle started
                 (Error
                    (apply_plan_err
                       ~fs:[ (p1, "tampered\n"); (p2, "b2\n"); (p3, "b3\n") ]
                       (M.Revert.Plan.edit plan))))
          in
          let preflight_body = get_member "settled" preflight in
          let preflight_outcomes = get_member "outcomes" preflight_body in
          assert_decode_error
            ~contains:"preflight failure admits only not-attempted outcomes"
            "ambiguous under preflight" M.Event.jsont
            (set_member "settled"
               (set_member "outcomes"
                  (set_element 0
                     (set_member "outcome"
                        (json_object [ ("type", Json.string "ambiguous") ])
                        (get_element 0 preflight_outcomes))
                     preflight_outcomes)
                  preflight_body)
               preflight));
      test "revertability answers round-trip and reject empty reasons"
        (fun () ->
          let codec = M.Revertability.jsont in
          List.iter
            (fun answer ->
              roundtrip "answer" codec M.Revertability.equal answer)
            [
              M.Revertability.Available;
              M.Revertability.Unavailable
                [
                  M.Revertability.Superseded
                    { path = p1; by = M.Change.Id.of_string "x" };
                ];
              M.Revertability.Incomplete
                [
                  M.Revertability.Non_contiguous { path = p1 };
                  M.Revertability.Observed { claim = claim "c1" };
                  M.Revertability.Unresolved_revert (rid "r1");
                ];
            ];
          assert_decode_error ~contains:"must not be empty" "empty reasons"
            codec
            (json_object
               [ ("type", Json.string "incomplete"); ("reasons", Json.list []) ]);
          assert_decode_error "unknown reason tag" codec
            (json_object
               [
                 ("type", Json.string "incomplete");
                 ( "reasons",
                   Json.list
                     [ json_object [ ("type", Json.string "degraded") ] ] );
               ]));
      test "edit failures round-trip and reject unknown kinds" (fun () ->
          let failure =
            M.Revert.Edit_failure.of_error (Edit.Error.io ~path:p1 "disk full")
          in
          let codec = M.Revert.Edit_failure.jsont in
          roundtrip "failure" codec M.Revert.Edit_failure.equal failure;
          check "kind survives"
            (M.Revert.Edit_failure.kind failure = M.Revert.Edit_failure.Io);
          let invalid_target =
            M.Revert.Edit_failure.of_error
              (Edit.Error.invalid_target ~path:p1
                 Edit.Error.Target_error.Symlink)
          in
          roundtrip "invalid target failure" codec M.Revert.Edit_failure.equal
            invalid_target;
          check "invalid target kind survives"
            (M.Revert.Edit_failure.kind invalid_target
            = M.Revert.Edit_failure.Invalid_target);
          let json = encode codec failure in
          assert_decode_error ~contains:"unknown edit failure kind"
            "unknown kind" codec
            (set_member "kind" (Json.string "panic") json);
          assert_decode_error ~contains:"must not be empty" "empty message"
            codec
            (set_member "message" (Json.string "") json));
    ]

(* Netted display diff (Diff.compute). *)

let operation_value =
  Testable.make
    ~pp:(fun ppf -> function
      | `Added -> Format.pp_print_string ppf "Added"
      | `Deleted -> Format.pp_print_string ppf "Deleted"
      | `Modified -> Format.pp_print_string ppf "Modified")
    ~equal:( = )

(* A blob reader over known texts: content-addressed, so a reference resolves to
   the text whose bytes it names. Bytes not in [texts] resolve to [None] — the
   missing-blob path. *)
let resolver texts reference =
  List.find_opt (Mentat_digest.Content_ref.matches reference) texts

let entry_paths (d : M.Diff.t) =
  List.map (fun (e : M.Diff.Entry.t) -> e.M.Diff.Entry.path) d.M.Diff.entries

let diff_group =
  group "netted display diff (Diff.compute)"
    [
      test "modified entries carry operations, contiguity, and summed counts"
        (fun () ->
          let state = state_exn [ base_history () ] in
          let d =
            M.Diff.compute ~state ~selection:M.Revert.Selection.all
              ~resolve:(resolver [ "a1\n"; "b1\n"; "a2\n"; "b2\n" ])
              ()
          in
          equal (list path_value)
            ~msg:"both edited paths appear, in ledger order" [ p1; p2 ]
            (entry_paths d);
          equal (list operation_value) ~msg:"each is a modification"
            [ `Modified; `Modified ]
            (List.map
               (fun (e : M.Diff.Entry.t) -> e.M.Diff.Entry.operation)
               d.M.Diff.entries);
          List.iter
            (fun (e : M.Diff.Entry.t) ->
              check "a contiguous history is not flagged"
                e.M.Diff.Entry.contiguous;
              check "resolvable images produce hunks"
                (Option.is_some e.M.Diff.Entry.hunks))
            d.M.Diff.entries;
          equal int ~msg:"one added line per rewrite" 2 d.M.Diff.additions;
          equal int ~msg:"one removed line per rewrite" 2 d.M.Diff.deletions;
          equal revertability_value ~msg:"a clean continuous selection is safe"
            M.Revertability.Available d.M.Diff.revertability);
      test "a discontinuous history is marked and diffed end to end" (fun () ->
          let state = state_exn (gap_history ()) in
          let d =
            M.Diff.compute ~state ~selection:M.Revert.Selection.all
              ~resolve:(resolver [ "va\n"; "vb\n"; "vc\n"; "vd\n" ])
              ()
          in
          match d.M.Diff.entries with
          | [ e ] ->
              equal path_value p1 e.M.Diff.Entry.path;
              check "the unrecorded gap flags the entry discontinuous"
                (not e.M.Diff.Entry.contiguous);
              check "the netted first-before to last-after diff is present"
                (Option.is_some e.M.Diff.Entry.hunks);
              equal int ~msg:"the end-to-end diff counts one added line" 1
                d.M.Diff.additions;
              equal int ~msg:"and one removed line" 1 d.M.Diff.deletions;
              check "discontinuity degrades the carried safety answer"
                (match d.M.Diff.revertability with
                | M.Revertability.Incomplete _ -> true
                | _ -> false)
          | entries ->
              failf "expected one netted entry, got %d" (List.length entries));
      test "an unresolvable image yields None hunks excluded from the counts"
        (fun () ->
          let state =
            state_exn
              [
                edit_event ~turn:"t1" ~claim:"c1" ~fs:[]
                  [ Edit.create ~path:p3 ~contents:"created\n" ];
              ]
          in
          let d =
            M.Diff.compute ~state ~selection:M.Revert.Selection.all
              ~resolve:(resolver []) ()
          in
          match d.M.Diff.entries with
          | [ e ] ->
              equal operation_value ~msg:"a creation is an addition" `Added
                e.M.Diff.Entry.operation;
              check "the missing after-image collapses the hunks to None"
                (Option.is_none e.M.Diff.Entry.hunks);
              equal int ~msg:"a None-hunk entry contributes no additions" 0
                d.M.Diff.additions;
              equal int ~msg:"and no deletions" 0 d.M.Diff.deletions
          | entries ->
              failf "expected one netted entry, got %d" (List.length entries));
      test "the selection scopes which paths the diff nets" (fun () ->
          let e1 =
            edit_on ~turn:"t1" ~claim:"c1" ~path:p1 ~before:"a\n" ~after:"b\n"
          in
          let e2 =
            edit_on ~turn:"t2" ~claim:"c2" ~path:p2 ~before:"x\n" ~after:"y\n"
          in
          let state = state_exn [ e1; e2 ] in
          let resolve = resolver [ "a\n"; "b\n"; "x\n"; "y\n" ] in
          equal (list path_value) ~msg:"the whole selection nets both paths"
            [ p1; p2 ]
            (entry_paths
               (M.Diff.compute ~state ~selection:M.Revert.Selection.all ~resolve
                  ()));
          equal (list path_value)
            ~msg:"a turn selection nets only that turn's path" [ p1 ]
            (entry_paths
               (M.Diff.compute ~state
                  ~selection:(M.Revert.Selection.turns [ turn "t1" ])
                  ~resolve ())));
      test "feasibility distinguishes a plain revert from a clean merge"
        (fun () ->
          let base = state_exn [ base_history () ] in
          is_true ~msg:"a non-superseded selection is trivially ready"
            (match
               M.Diff.feasibility
                 (M.Diff.compute ~state:base ~selection:M.Revert.Selection.all
                    ~resolve:(resolver [ "a1\n"; "b1\n"; "a2\n"; "b2\n" ])
                    ())
             with
            | M.Diff.Feasibility.Trivial -> true
            | _ -> false);
          let e1 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "A\nB\nC\nD\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"A\nB\nC\nD\n"
                  ~after:"A\nX\nC\nD\n";
              ]
          in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2"
              ~fs:[ (p1, "A\nX\nC\nD\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"A\nX\nC\nD\n"
                  ~after:"A\nX\nC\nY\n";
              ]
          in
          let state = state_exn [ e1; e2 ] in
          let selection =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes state ~claim:(claim "c1")))
          in
          let d =
            M.Diff.compute ~state ~selection
              ~current:(fun _ -> Some (Edit.Observed.Text "A\nX\nC\nY\n"))
              ~resolve:
                (resolver [ "A\nB\nC\nD\n"; "A\nX\nC\nD\n"; "A\nX\nC\nY\n" ])
              ()
          in
          is_true ~msg:"a superseded clean merge previews mergeable and ready"
            (match M.Diff.feasibility d with
            | M.Diff.Feasibility.Mergeable _ ->
                M.Diff.Feasibility.ready (M.Diff.feasibility d)
            | _ -> false));
    ]

let rung_2_group =
  group "rung 2 merge"
    [
      test
        "a clean merge applies a superseded selection; merge:false refuses it"
        (fun () ->
          let e1 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "A\nB\nC\nD\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"A\nB\nC\nD\n"
                  ~after:"A\nX\nC\nD\n";
              ]
          in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2"
              ~fs:[ (p1, "A\nX\nC\nD\n") ]
              [
                Edit.rewrite ~path:p1 ~before:"A\nX\nC\nD\n"
                  ~after:"A\nX\nC\nY\n";
              ]
          in
          let st = state_exn [ e1; e2 ] in
          let selection =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
          in
          let ev =
            evidence
              ~current:[ (p1, Edit.Observed.Text "A\nX\nC\nY\n") ]
              ~blobs:[ "A\nB\nC\nD\n"; "A\nX\nC\nD\n" ]
              ()
          in
          (match
             M.Revert.prepare ~merge:false ~id:(rid "r1") ~evidence:ev st
               selection
           with
          | Error problems ->
              is_true ~msg:"merge:false refuses the superseded selection"
                (List.exists
                   (function
                     | M.Revert.Problem.Superseded _ -> true | _ -> false)
                   problems)
          | Ok _ -> fail "merge:false must refuse a superseded selection");
          let plan = prepare_ok ~id:(rid "r1") ~evidence:ev st selection in
          (match M.Revert.Plan.merge_blobs plan with
          | [ (_, merged) ] ->
              equal string
                ~msg:
                  "the merged image keeps the later change and reverts line 2"
                "A\nB\nC\nY\n" merged
          | _ -> fail "expected exactly one merged restore blob");
          let (_ : M.State.t) =
            state_exn
              [ e1; e2; before_revert plan; M.Event.revert_started plan ]
          in
          ());
      test "an incompatible overlap surfaces as a conflict with markers"
        (fun () ->
          let e1 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "A\nB\nC\n") ]
              [ Edit.rewrite ~path:p1 ~before:"A\nB\nC\n" ~after:"A\nX\nC\n" ]
          in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2"
              ~fs:[ (p1, "A\nX\nC\n") ]
              [ Edit.rewrite ~path:p1 ~before:"A\nX\nC\n" ~after:"A\nZ\nC\n" ]
          in
          let st = state_exn [ e1; e2 ] in
          let selection =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
          in
          let ev =
            evidence
              ~current:[ (p1, Edit.Observed.Text "A\nZ\nC\n") ]
              ~blobs:[ "A\nB\nC\n"; "A\nX\nC\n" ]
              ()
          in
          match M.Revert.prepare ~id:(rid "r1") ~evidence:ev st selection with
          | Error [ M.Revert.Problem.Conflict { merge; _ } ] ->
              is_true ~msg:"the merge is not clean"
                (not (Textdiff.Merge.is_clean merge));
              is_true ~msg:"exactly one conflict region"
                (List.length (Textdiff.Merge.conflicts merge) = 1);
              let markers = Textdiff.Merge.render_markers merge in
              is_true ~msg:"the refusal message renders git-style markers"
                (String.length
                   (M.Revert.Problem.message
                      (M.Revert.Problem.Conflict { path = p1; merge }))
                > String.length markers)
          | Error problems ->
              failf "expected a single Conflict, got %a"
                (Format.pp_print_list M.Revert.Problem.pp)
                problems
          | Ok _ -> fail "a conflicting merge must not silently apply");
      test "a merge reproducing the current file reverts nothing" (fun () ->
          (* [e2] already undid [e1], so reverting [e1] merges back to the
             current file, drops the M-equals-current target, and finds nothing
             to revert. *)
          let e1 =
            edit_event ~turn:"t1" ~claim:"c1"
              ~fs:[ (p1, "A\n") ]
              [ Edit.rewrite ~path:p1 ~before:"A\n" ~after:"B\n" ]
          in
          let e2 =
            edit_event ~turn:"t2" ~claim:"c2"
              ~fs:[ (p1, "B\n") ]
              [ Edit.rewrite ~path:p1 ~before:"B\n" ~after:"A\n" ]
          in
          let st = state_exn [ e1; e2 ] in
          let selection =
            M.Revert.Selection.changes
              (List.map M.Change.id (M.State.changes st ~claim:(claim "c1")))
          in
          let ev =
            evidence
              ~current:[ (p1, Edit.Observed.Text "A\n") ]
              ~blobs:[ "A\n"; "B\n" ] ()
          in
          match M.Revert.prepare ~id:(rid "r1") ~evidence:ev st selection with
          | Error [ M.Revert.Problem.Nothing_to_revert ] -> ()
          | Error problems ->
              failf "expected Nothing_to_revert, got %a"
                (Format.pp_print_list M.Revert.Problem.pp)
                problems
          | Ok _ -> fail "a no-op merge must not mint a plan");
    ]

let () =
  run "mentat.mutation"
    [
      lowering_group;
      fold_group;
      start_validation_group;
      rung_2_group;
      append_group;
      at_claim_group;
      prefix_group;
      netting_group;
      revertability_group;
      selection_group;
      preparation_group;
      lifecycle_group;
      totals_group;
      codec_group;
      scope_outcome_group;
      diff_group;
    ]
