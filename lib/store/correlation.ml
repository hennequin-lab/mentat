(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Result.Syntax

type t =
  | Unknown_turn of Mentat_session.Turn.Id.t
  | Turn_without_tool_claim of Mentat_session.Turn.Id.t
  | Turn_not_settled of Mentat_session.Turn.Id.t
  | Unknown_claim of Mentat_session.Tool_claim.Id.t
  | Claim_turn_mismatch of {
      claim : Mentat_session.Tool_claim.Id.t;
      expected : Mentat_session.Turn.Id.t;
      actual : Mentat_session.Turn.Id.t;
    }
  | Claim_not_open of Mentat_session.Tool_claim.Id.t
  | Recovery_without_ambiguity of Mentat_session.Turn.Id.t
  | Before_tools_after_ambiguity of Mentat_session.Turn.Id.t

let message = function
  | Unknown_turn turn ->
      Format.asprintf "mutation references unknown turn %a"
        Mentat_session.Turn.Id.pp turn
  | Turn_without_tool_claim turn ->
      Format.asprintf "mutation boundary for turn %a has no tool claim"
        Mentat_session.Turn.Id.pp turn
  | Turn_not_settled turn ->
      Format.asprintf "after-turn boundary references unsettled turn %a"
        Mentat_session.Turn.Id.pp turn
  | Unknown_claim claim ->
      Format.asprintf "mutation references unknown tool claim %a"
        Mentat_session.Tool_claim.Id.pp claim
  | Claim_turn_mismatch { claim; expected; actual } ->
      Format.asprintf
        "mutation claim %a belongs to turn %a, not referenced turn %a"
        Mentat_session.Tool_claim.Id.pp claim Mentat_session.Turn.Id.pp actual
        Mentat_session.Turn.Id.pp expected
  | Claim_not_open claim ->
      Format.asprintf "mutation tool claim %a is not the open suspension"
        Mentat_session.Tool_claim.Id.pp claim
  | Recovery_without_ambiguity turn ->
      Format.asprintf
        "after-recovery boundary for turn %a has no prior ambiguous tool claim"
        Mentat_session.Turn.Id.pp turn
  | Before_tools_after_ambiguity turn ->
      Format.asprintf
        "before-tools boundary for turn %a follows an ambiguous tool claim"
        Mentat_session.Turn.Id.pp turn

let pp ppf t = Format.pp_print_string ppf (message t)

type mode = History | Live

let find_claim claims id =
  List.find_opt
    (fun (started, _) ->
      Mentat_session.Tool_claim.Id.equal
        (Mentat_session.Tool_claim.Started.id started)
        id)
    claims

let claim_for_turn claims turn =
  List.exists
    (fun (started, _) ->
      Mentat_session.Turn.Id.equal
        (Mentat_session.Tool_claim.Started.turn started)
        turn)
    claims

let ambiguous_for_turn claims turn =
  List.exists
    (fun (started, settled) ->
      Mentat_session.Turn.Id.equal
        (Mentat_session.Tool_claim.Started.turn started)
        turn
      &&
      match settled with
      | Some settled -> (
          match Mentat_session.Tool_claim.Settled.outcome settled with
          | Mentat_session.Tool_claim.Settled.Ambiguous -> true
          | Mentat_session.Tool_claim.Settled.Prepared _
          | Mentat_session.Tool_claim.Settled.Returned _ ->
              false)
      | None -> false)
    claims

let require_turn state turn =
  match Mentat_session.State.turn turn state with
  | Some _ -> Ok ()
  | None -> Error (Unknown_turn turn)

let require_open state ~turn ~claim =
  match Mentat_session.State.suspension state with
  | Some (Mentat_session.State.Tool started)
    when Mentat_session.Tool_claim.Id.equal
           (Mentat_session.Tool_claim.Started.id started)
           claim
         && Mentat_session.Turn.Id.equal
              (Mentat_session.Tool_claim.Started.turn started)
              turn ->
      Ok ()
  | Some (Mentat_session.State.Tool _)
  | Some (Mentat_session.State.Provider _)
  | Some (Mentat_session.State.Decision _)
  | None ->
      Error (Claim_not_open claim)

let require_claim state claims ~mode ~turn ~claim =
  match find_claim claims claim with
  | None -> Error (Unknown_claim claim)
  | Some (started, _) -> (
      let actual = Mentat_session.Tool_claim.Started.turn started in
      if not (Mentat_session.Turn.Id.equal actual turn) then
        Error (Claim_turn_mismatch { claim; expected = turn; actual })
      else
        match mode with
        | History -> Ok ()
        | Live -> require_open state ~turn ~claim)

let require_tool_boundary state claims ~mode ~recovery turn =
  let* () = require_turn state turn in
  if not (claim_for_turn claims turn) then Error (Turn_without_tool_claim turn)
  else if recovery && not (ambiguous_for_turn claims turn) then
    Error (Recovery_without_ambiguity turn)
  else
    match mode with
    | History -> Ok ()
    | Live -> (
        match Mentat_session.State.suspension state with
        | Some (Mentat_session.State.Tool started)
          when Mentat_session.Turn.Id.equal
                 (Mentat_session.Tool_claim.Started.turn started)
                 turn ->
            Ok ()
        | Some (Mentat_session.State.Tool started) ->
            Error
              (Claim_not_open (Mentat_session.Tool_claim.Started.id started))
        | Some (Mentat_session.State.Provider _)
        | Some (Mentat_session.State.Decision _)
        | None ->
            (* The boundary names a turn, while the only useful live diagnostic
               identity is the claim the turn previously established. *)
            let started, _ =
              List.find
                (fun (started, _) ->
                  Mentat_session.Turn.Id.equal
                    (Mentat_session.Tool_claim.Started.turn started)
                    turn)
                claims
            in
            Error
              (Claim_not_open (Mentat_session.Tool_claim.Started.id started)))

let check_event state claims ~mode = function
  | Mentat_mutation.Event.Checkpoint checkpoint -> (
      match Mentat_mutation.Checkpoint.boundary checkpoint with
      | Mentat_mutation.Checkpoint.Before_turn_tools turn -> (
          let* () =
            require_tool_boundary state claims ~mode ~recovery:false turn
          in
          match mode with
          | Live when ambiguous_for_turn claims turn ->
              Error (Before_tools_after_ambiguity turn)
          | History | Live -> Ok ())
      | Mentat_mutation.Checkpoint.After_recovery turn ->
          require_tool_boundary state claims ~mode ~recovery:true turn
      | Mentat_mutation.Checkpoint.After_turn turn ->
          let* () = require_turn state turn in
          if Option.is_some (Mentat_session.State.turn_outcome turn state) then
            Ok ()
          else Error (Turn_not_settled turn)
      | Mentat_mutation.Checkpoint.Before_revert _ -> Ok ())
  | Mentat_mutation.Event.Edit { turn; claim; _ }
  | Mentat_mutation.Event.Tool_observed { turn; claim; _ } ->
      require_claim state claims ~mode ~turn ~claim
  | Mentat_mutation.Event.Revert_started started -> (
      match started.Mentat_mutation.Revert.Started.selection with
      | Mentat_mutation.Revert.Selection.Turns turns ->
          List.fold_left
            (fun result turn ->
              let* () = result in
              require_turn state turn)
            (Ok ()) turns
      | Mentat_mutation.Revert.Selection.All
      | Mentat_mutation.Revert.Selection.Changes _
      | Mentat_mutation.Revert.Selection.Paths _ ->
          Ok ())
  | Mentat_mutation.Event.Revert_settled _ -> Ok ()

let check ~mode session events =
  let state = Mentat_session.state session in
  let claims = Mentat_session.State.tool_claims state in
  List.fold_left
    (fun result event ->
      let* () = result in
      check_event state claims ~mode event)
    (Ok ()) events
