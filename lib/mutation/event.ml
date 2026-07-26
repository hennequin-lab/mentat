(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

type t =
  | Checkpoint of Checkpoint.t
  | Edit of {
      turn : Mentat_session.Turn.Id.t;
      claim : Mentat_session.Tool_claim.Id.t;
      ordinal : int;
      checkpoint : Checkpoint.Id.t option;
      changes : Change.t list;
      uncertain : Mentat_workspace.Path.t option;
    }
  | Tool_observed of {
      turn : Mentat_session.Turn.Id.t;
      claim : Mentat_session.Tool_claim.Id.t;
      paths : Mentat_workspace.Path.t list;
    }
  | Revert_started of Revert_data.Started.t
  | Revert_settled of Revert_data.Settled.t

let invalid fn message = invalid_arg' "Mentat_mutation.Event" fn message
let checkpoint checkpoint = Checkpoint checkpoint

(* Internal checked constructor shared by [of_edit], [of_attempt], and decode. *)
let edit ~turn ~claim ~ordinal ~checkpoint ~uncertain changes =
  if ordinal < 0 then invalid "edit" "ordinal must be non-negative";
  (match (changes, uncertain) with
  | [], None -> invalid "edit" "changes must not be empty"
  | _, _ -> ());
  if not (Canon.unique_paths (List.map Change.path changes)) then
    invalid "edit" "duplicate change paths within one event";
  (match uncertain with
  | Some path
    when List.exists
           (fun change -> Mentat_workspace.Path.equal path (Change.path change))
           changes ->
      invalid "edit" "uncertain target duplicates a confirmed change path"
  | Some _ | None -> ());
  List.iter
    (fun change ->
      let expected =
        Change.Id.of_string
          (Change_id.for_claim
             ~claim:(Mentat_session.Tool_claim.Id.to_string claim)
             ~ordinal (Change.path change))
      in
      if not (Change.Id.equal expected (Change.id change)) then
        invalid "edit"
          "change id does not derive from its claim, ordinal, and path")
    changes;
  Edit { turn; claim; ordinal; checkpoint; changes; uncertain }

let lower_entries ~claim ~ordinal entries =
  List.map
    (fun entry ->
      let path = Mentat_edit.Result.Entry.target_path entry in
      let id =
        Change_id.for_claim
          ~claim:(Mentat_session.Tool_claim.Id.to_string claim)
          ~ordinal path
      in
      Change.of_entry ~context:"Event.of_edit" ~id entry)
    entries

let of_edit ~turn ~claim ~ordinal ~checkpoint result =
  let entries = Mentat_edit.Result.entries result in
  (match entries with
  | [] -> invalid "of_edit" "result must not be empty"
  | _ :: _ -> ());
  edit ~turn ~claim ~ordinal ~checkpoint ~uncertain:None
    (lower_entries ~claim ~ordinal entries)

let of_attempt ~turn ~claim ~ordinal ~checkpoint ~applied ~uncertain =
  edit ~turn ~claim ~ordinal ~checkpoint ~uncertain
    (lower_entries ~claim ~ordinal applied)

(* Internal checked constructor shared by [tool_observed] and decode. *)
let observed ~turn ~claim paths =
  (match paths with
  | [] -> invalid "tool_observed" "paths must not be empty"
  | _ :: _ -> ());
  if not (Canon.strictly_sorted Mentat_workspace.Path.compare paths) then
    invalid "tool_observed" "paths must be deduplicated and sorted";
  Tool_observed { turn; claim; paths }

let tool_observed ~turn ~claim paths =
  observed ~turn ~claim (List.sort_uniq Mentat_workspace.Path.compare paths)

let revert_started plan = Revert_started (Revert_data.Plan.started plan)
let revert_settled settled = Revert_settled settled

let equal a b =
  match (a, b) with
  | Checkpoint a, Checkpoint b -> Checkpoint.equal a b
  | ( Edit
        {
          turn = turn_a;
          claim = claim_a;
          ordinal = ordinal_a;
          checkpoint = checkpoint_a;
          changes = changes_a;
          uncertain = uncertain_a;
        },
      Edit
        {
          turn = turn_b;
          claim = claim_b;
          ordinal = ordinal_b;
          checkpoint = checkpoint_b;
          changes = changes_b;
          uncertain = uncertain_b;
        } ) ->
      Mentat_session.Turn.Id.equal turn_a turn_b
      && Mentat_session.Tool_claim.Id.equal claim_a claim_b
      && Int.equal ordinal_a ordinal_b
      && Option.equal Checkpoint.Id.equal checkpoint_a checkpoint_b
      && List.equal Change.equal changes_a changes_b
      && Option.equal Mentat_workspace.Path.equal uncertain_a uncertain_b
  | ( Tool_observed { turn = turn_a; claim = claim_a; paths = paths_a },
      Tool_observed { turn = turn_b; claim = claim_b; paths = paths_b } ) ->
      Mentat_session.Turn.Id.equal turn_a turn_b
      && Mentat_session.Tool_claim.Id.equal claim_a claim_b
      && List.equal Mentat_workspace.Path.equal paths_a paths_b
  | Revert_started a, Revert_started b -> Revert_data.Started.equal a b
  | Revert_settled a, Revert_settled b -> Revert_data.Settled.equal a b
  | ( ( Checkpoint _ | Edit _ | Tool_observed _ | Revert_started _
      | Revert_settled _ ),
      _ ) ->
      false

let pp ppf = function
  | Checkpoint checkpoint -> Checkpoint.pp ppf checkpoint
  | Edit { claim; ordinal; changes; uncertain; _ } ->
      Format.fprintf ppf "edit(%a#%d, %d changes%s)"
        Mentat_session.Tool_claim.Id.pp claim ordinal (List.length changes)
        (match uncertain with None -> "" | Some _ -> ", uncertain target")
  | Tool_observed { claim; paths; _ } ->
      Format.fprintf ppf "tool_observed(%a, %d paths)"
        Mentat_session.Tool_claim.Id.pp claim (List.length paths)
  | Revert_started started ->
      Format.fprintf ppf "revert_started(%a)" Revert_data.Id.pp
        started.Revert_data.Started.id
  | Revert_settled settled ->
      Format.fprintf ppf "revert_settled(%a)" Revert_data.Id.pp
        settled.Revert_data.Settled.revert

let jsont =
  let checkpoint_case =
    Jsont.Object.map ~kind:"checkpoint event" (fun checkpoint ->
        Checkpoint checkpoint)
    |> Jsont.Object.mem "checkpoint" Checkpoint.jsont ~enc:(function
      | Checkpoint checkpoint -> checkpoint
      | Edit _ | Tool_observed _ | Revert_started _ | Revert_settled _ ->
          assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "checkpoint" ~dec:Fun.id
  in
  let edit_case =
    Jsont.Object.map ~kind:"edit event"
      (fun turn claim ordinal checkpoint changes uncertain ->
        decode_invalid_arg (fun () ->
            edit ~turn ~claim ~ordinal ~checkpoint ~uncertain changes))
    |> Jsont.Object.mem "turn" Mentat_session.Turn.Id.jsont ~enc:(function
      | Edit { turn; _ } -> turn
      | Checkpoint _ | Tool_observed _ | Revert_started _ | Revert_settled _ ->
          assert false)
    |> Jsont.Object.mem "claim" Mentat_session.Tool_claim.Id.jsont
         ~enc:(function
         | Edit { claim; _ } -> claim
         | Checkpoint _ | Tool_observed _ | Revert_started _ | Revert_settled _
           ->
             assert false)
    |> Jsont.Object.mem "ordinal" Jsont.int ~enc:(function
      | Edit { ordinal; _ } -> ordinal
      | Checkpoint _ | Tool_observed _ | Revert_started _ | Revert_settled _ ->
          assert false)
    |> Jsont.Object.opt_mem "checkpoint" Checkpoint.Id.jsont ~enc:(function
      | Edit { checkpoint; _ } -> checkpoint
      | Checkpoint _ | Tool_observed _ | Revert_started _ | Revert_settled _ ->
          assert false)
    |> Jsont.Object.mem "changes" (Jsont.list Change.jsont) ~enc:(function
      | Edit { changes; _ } -> changes
      | Checkpoint _ | Tool_observed _ | Revert_started _ | Revert_settled _ ->
          assert false)
    |> Jsont.Object.opt_mem "uncertain" Mentat_workspace.Path.jsont
         ~enc:(function
         | Edit { uncertain; _ } -> uncertain
         | Checkpoint _ | Tool_observed _ | Revert_started _ | Revert_settled _
           ->
             assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "edit" ~dec:Fun.id
  in
  let tool_observed_case =
    Jsont.Object.map ~kind:"tool observed event" (fun turn claim paths ->
        decode_invalid_arg (fun () -> observed ~turn ~claim paths))
    |> Jsont.Object.mem "turn" Mentat_session.Turn.Id.jsont ~enc:(function
      | Tool_observed { turn; _ } -> turn
      | Checkpoint _ | Edit _ | Revert_started _ | Revert_settled _ ->
          assert false)
    |> Jsont.Object.mem "claim" Mentat_session.Tool_claim.Id.jsont
         ~enc:(function
         | Tool_observed { claim; _ } -> claim
         | Checkpoint _ | Edit _ | Revert_started _ | Revert_settled _ ->
             assert false)
    |> Jsont.Object.mem "paths" (Jsont.list Mentat_workspace.Path.jsont)
         ~enc:(function
         | Tool_observed { paths; _ } -> paths
         | Checkpoint _ | Edit _ | Revert_started _ | Revert_settled _ ->
             assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "tool_observed" ~dec:Fun.id
  in
  let revert_started_case =
    Jsont.Object.map ~kind:"revert started event" (fun started ->
        Revert_started started)
    |> Jsont.Object.mem "started" Revert_data.Started.jsont ~enc:(function
      | Revert_started started -> started
      | Checkpoint _ | Edit _ | Tool_observed _ | Revert_settled _ ->
          assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "revert_started" ~dec:Fun.id
  in
  let revert_settled_case =
    Jsont.Object.map ~kind:"revert settled event" (fun settled ->
        Revert_settled settled)
    |> Jsont.Object.mem "settled" Revert_data.Settled.jsont ~enc:(function
      | Revert_settled settled -> settled
      | Checkpoint _ | Edit _ | Tool_observed _ | Revert_started _ ->
          assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "revert_settled" ~dec:Fun.id
  in
  let cases =
    List.map Jsont.Object.Case.make
      [
        checkpoint_case;
        edit_case;
        tool_observed_case;
        revert_started_case;
        revert_settled_case;
      ]
  in
  let enc_case = function
    | Checkpoint _ as event -> Jsont.Object.Case.value checkpoint_case event
    | Edit _ as event -> Jsont.Object.Case.value edit_case event
    | Tool_observed _ as event ->
        Jsont.Object.Case.value tool_observed_case event
    | Revert_started _ as event ->
        Jsont.Object.Case.value revert_started_case event
    | Revert_settled _ as event ->
        Jsont.Object.Case.value revert_settled_case event
  in
  Jsont.Object.map ~kind:"mutation event" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
