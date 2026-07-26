(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

module Id =
  String_id.Make
    (struct
      let module_path = "Mentat_mutation.Checkpoint.Id"
      let kind = "checkpoint id"
    end)
    ()

let invalid fn message = invalid_arg' "Mentat_mutation.Checkpoint" fn message

type boundary =
  | Before_turn_tools of Mentat_session.Turn.Id.t
  | After_recovery of Mentat_session.Turn.Id.t
  | After_turn of Mentat_session.Turn.Id.t
  | Before_revert of Revert_data.Id.t

let boundary_equal a b =
  match (a, b) with
  | Before_turn_tools a, Before_turn_tools b -> Mentat_session.Turn.Id.equal a b
  | After_recovery a, After_recovery b -> Mentat_session.Turn.Id.equal a b
  | After_turn a, After_turn b -> Mentat_session.Turn.Id.equal a b
  | Before_revert a, Before_revert b -> Revert_data.Id.equal a b
  | (Before_turn_tools _ | After_recovery _ | After_turn _ | Before_revert _), _
    ->
      false

let boundary_pp ppf = function
  | Before_turn_tools turn ->
      Format.fprintf ppf "before_turn_tools:%a" Mentat_session.Turn.Id.pp turn
  | After_recovery turn ->
      Format.fprintf ppf "after_recovery:%a" Mentat_session.Turn.Id.pp turn
  | After_turn turn ->
      Format.fprintf ppf "after_turn:%a" Mentat_session.Turn.Id.pp turn
  | Before_revert revert ->
      Format.fprintf ppf "before_revert:%a" Revert_data.Id.pp revert

let boundary_jsont =
  let before_turn_tools_case =
    Jsont.Object.map ~kind:"before turn tools boundary" (fun turn ->
        Before_turn_tools turn)
    |> Jsont.Object.mem "turn" Mentat_session.Turn.Id.jsont ~enc:(function
      | Before_turn_tools turn -> turn
      | After_recovery _ | After_turn _ | Before_revert _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "before_turn_tools" ~dec:Fun.id
  in
  let after_recovery_case =
    Jsont.Object.map ~kind:"after recovery boundary" (fun turn ->
        After_recovery turn)
    |> Jsont.Object.mem "turn" Mentat_session.Turn.Id.jsont ~enc:(function
      | After_recovery turn -> turn
      | Before_turn_tools _ | After_turn _ | Before_revert _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "after_recovery" ~dec:Fun.id
  in
  let after_turn_case =
    Jsont.Object.map ~kind:"after turn boundary" (fun turn -> After_turn turn)
    |> Jsont.Object.mem "turn" Mentat_session.Turn.Id.jsont ~enc:(function
      | After_turn turn -> turn
      | Before_turn_tools _ | After_recovery _ | Before_revert _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "after_turn" ~dec:Fun.id
  in
  let before_revert_case =
    Jsont.Object.map ~kind:"before revert boundary" (fun revert ->
        Before_revert revert)
    |> Jsont.Object.mem "revert" Revert_data.Id.jsont ~enc:(function
      | Before_revert revert -> revert
      | Before_turn_tools _ | After_recovery _ | After_turn _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "before_revert" ~dec:Fun.id
  in
  let cases =
    List.map Jsont.Object.Case.make
      [
        before_turn_tools_case;
        after_recovery_case;
        after_turn_case;
        before_revert_case;
      ]
  in
  let enc_case = function
    | Before_turn_tools _ as boundary ->
        Jsont.Object.Case.value before_turn_tools_case boundary
    | After_recovery _ as boundary ->
        Jsont.Object.Case.value after_recovery_case boundary
    | After_turn _ as boundary ->
        Jsont.Object.Case.value after_turn_case boundary
    | Before_revert _ as boundary ->
        Jsont.Object.Case.value before_revert_case boundary
  in
  Jsont.Object.map ~kind:"checkpoint boundary" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

module Snapshot = struct
  type t = { backend : string; reference : string }

  let make ~backend ~reference =
    if String.is_empty backend then
      invalid "Snapshot.make" "backend must not be empty";
    if String.is_empty reference then
      invalid "Snapshot.make" "reference must not be empty";
    { backend; reference }

  let equal a b =
    String.equal a.backend b.backend && String.equal a.reference b.reference

  let pp ppf t = Format.fprintf ppf "snapshot(%s, %s)" t.backend t.reference

  let jsont =
    Jsont.Object.map ~kind:"snapshot" (fun backend reference ->
        decode_invalid_arg (fun () -> make ~backend ~reference))
    |> Jsont.Object.mem "backend" Jsont.string ~enc:(fun t -> t.backend)
    |> Jsont.Object.mem "reference" Jsont.string ~enc:(fun t -> t.reference)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Capture = struct
  module Failure = struct
    type t = { message : string }

    let make ~message =
      if String.is_empty message then
        invalid "Capture.Failure.make" "message must not be empty";
      { message }

    let message t = t.message
    let equal a b = String.equal a.message b.message
    let pp ppf t = Format.fprintf ppf "capture_failed(%s)" t.message

    (* The closed failure kind lives on the wire so a future arm widens the
       codec, never a free backend string. *)
    let jsont =
      let failed_case =
        Jsont.Object.map ~kind:"capture failure" (fun message ->
            decode_invalid_arg (fun () -> make ~message))
        |> Jsont.Object.mem "message" Jsont.string ~enc:message
        |> Jsont.Object.error_unknown |> Jsont.Object.finish
        |> Jsont.Object.Case.map "capture_failed" ~dec:Fun.id
      in
      let cases = [ Jsont.Object.Case.make failed_case ] in
      let enc_case failure = Jsont.Object.Case.value failed_case failure in
      Jsont.Object.map ~kind:"capture failure" Fun.id
      |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
  end

  type t =
    | Available of { snapshot : Snapshot.t; excluded : int }
    | Degraded of { failure : Failure.t }

  let equal a b =
    match (a, b) with
    | Available a, Available b ->
        Snapshot.equal a.snapshot b.snapshot && Int.equal a.excluded b.excluded
    | Degraded a, Degraded b -> Failure.equal a.failure b.failure
    | (Available _ | Degraded _), _ -> false

  let pp ppf = function
    | Available { snapshot; excluded } ->
        Format.fprintf ppf "available(%a, %d excluded)" Snapshot.pp snapshot
          excluded
    | Degraded { failure } ->
        Format.fprintf ppf "degraded(%a)" Failure.pp failure

  let jsont =
    let available_case =
      Jsont.Object.map ~kind:"available capture" (fun snapshot excluded ->
          if excluded < 0 then decode_error "excluded must be non-negative"
          else Available { snapshot; excluded })
      |> Jsont.Object.mem "snapshot" Snapshot.jsont ~enc:(function
        | Available { snapshot; _ } -> snapshot
        | Degraded _ -> assert false)
      |> Jsont.Object.mem "excluded" Jsont.int ~enc:(function
        | Available { excluded; _ } -> excluded
        | Degraded _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "available" ~dec:Fun.id
    in
    let degraded_case =
      Jsont.Object.map ~kind:"degraded capture" (fun failure ->
          Degraded { failure })
      |> Jsont.Object.mem "failure" Failure.jsont ~enc:(function
        | Degraded { failure } -> failure
        | Available _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "degraded" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make [ available_case; degraded_case ]
    in
    let enc_case = function
      | Available _ as capture -> Jsont.Object.Case.value available_case capture
      | Degraded _ as capture -> Jsont.Object.Case.value degraded_case capture
    in
    Jsont.Object.map ~kind:"capture" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

type t = { boundary : boundary; capture : Capture.t }

let make ~boundary ~capture =
  (match capture with
  | Capture.Available { excluded; _ } ->
      if excluded < 0 then invalid "make" "excluded must be non-negative"
  | Capture.Degraded _ -> ());
  { boundary; capture }

let boundary_parts = function
  | Before_turn_tools turn ->
      [ "before_turn_tools"; Mentat_session.Turn.Id.to_string turn ]
  | After_recovery turn ->
      [ "after_recovery"; Mentat_session.Turn.Id.to_string turn ]
  | After_turn turn -> [ "after_turn"; Mentat_session.Turn.Id.to_string turn ]
  | Before_revert revert -> [ "before_revert"; Revert_data.Id.to_string revert ]

let id_of_boundary boundary =
  Id.of_string
    (String_id.derive_id ~domain:"mentat.mutation.checkpoint.v1"
       (boundary_parts boundary))

let id t = id_of_boundary t.boundary
let boundary t = t.boundary
let capture t = t.capture

let snapshot t =
  match t.capture with
  | Capture.Available { snapshot; _ } -> Some snapshot
  | Capture.Degraded _ -> None

let equal a b =
  boundary_equal a.boundary b.boundary && Capture.equal a.capture b.capture

let pp ppf t =
  let status =
    match t.capture with
    | Capture.Available _ -> "available"
    | Capture.Degraded _ -> "degraded"
  in
  Format.fprintf ppf "checkpoint(%a, %s)" boundary_pp t.boundary status

let jsont =
  Jsont.Object.map ~kind:"checkpoint" (fun boundary capture ->
      decode_invalid_arg (fun () -> make ~boundary ~capture))
  |> Jsont.Object.mem "boundary" boundary_jsont ~enc:(fun t -> t.boundary)
  |> Jsont.Object.mem "capture" Capture.jsont ~enc:(fun t -> t.capture)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
