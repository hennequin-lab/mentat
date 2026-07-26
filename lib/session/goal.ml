(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_session.Goal" fn message

module Id =
  String_id.Make
    (struct
      let module_path = "Mentat_session.Goal.Id"
      let kind = "goal id"
    end)
    ()

let check_budget fn = function
  | Some budget when budget < 0 ->
      invalid fn "token_budget must not be negative"
  | Some _ | None -> ()

let check_present fn field = function
  | Some value when String.is_empty value ->
      invalid fn (field ^ " must not be empty")
  | Some _ | None -> ()

module Status = struct
  type t =
    | Active
    | Paused
    | Blocked of { reason : string option }
    | Budget_limited
    | Completed of { summary : string option }
    | Cleared

  let is_terminal = function
    | Completed _ | Cleared -> true
    | Active | Paused | Blocked _ | Budget_limited -> false

  let pausable = function
    | Active -> true
    | Paused | Blocked _ | Budget_limited | Completed _ | Cleared -> false

  let equal a b = a = b

  let pp ppf = function
    | Active -> Format.pp_print_string ppf "active"
    | Paused -> Format.pp_print_string ppf "paused"
    | Blocked { reason = None } -> Format.pp_print_string ppf "blocked"
    | Blocked { reason = Some reason } ->
        Format.fprintf ppf "blocked(%S)" reason
    | Budget_limited -> Format.pp_print_string ppf "budget-limited"
    | Completed { summary = None } -> Format.pp_print_string ppf "completed"
    | Completed { summary = Some summary } ->
        Format.fprintf ppf "completed(%S)" summary
    | Cleared -> Format.pp_print_string ppf "cleared"

  let jsont =
    let constant_case tag value =
      Jsont.Object.map ~kind:("goal status " ^ tag) value
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map tag ~dec:Fun.id
    in
    let active_case = constant_case "active" Active in
    let paused_case = constant_case "paused" Paused in
    let blocked_case =
      Jsont.Object.map ~kind:"goal status blocked" (fun reason ->
          decode_invalid_arg (fun () ->
              check_present "Status.jsont" "reason" reason;
              Blocked { reason }))
      |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(function
        | Blocked { reason } -> reason
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "blocked" ~dec:Fun.id
    in
    let budget_limited_case = constant_case "budget_limited" Budget_limited in
    let completed_case =
      Jsont.Object.map ~kind:"goal status completed" (fun summary ->
          decode_invalid_arg (fun () ->
              check_present "Status.jsont" "summary" summary;
              Completed { summary }))
      |> Jsont.Object.opt_mem "summary" Jsont.string ~enc:(function
        | Completed { summary } -> summary
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "completed" ~dec:Fun.id
    in
    let cleared_case = constant_case "cleared" Cleared in
    let cases =
      List.map Jsont.Object.Case.make
        [
          active_case;
          paused_case;
          blocked_case;
          budget_limited_case;
          completed_case;
          cleared_case;
        ]
    in
    let enc_case = function
      | Active as status -> Jsont.Object.Case.value active_case status
      | Paused as status -> Jsont.Object.Case.value paused_case status
      | Blocked _ as status -> Jsont.Object.Case.value blocked_case status
      | Budget_limited as status ->
          Jsont.Object.Case.value budget_limited_case status
      | Completed _ as status -> Jsont.Object.Case.value completed_case status
      | Cleared as status -> Jsont.Object.Case.value cleared_case status
    in
    Jsont.Object.map ~kind:"goal status" Fun.id
    |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Update = struct
  type t =
    | Declare of { id : Id.t; objective : string; token_budget : int option }
    | Pause of { id : Id.t }
    | Resume of { id : Id.t; token_budget : int option }
    | Edit of { id : Id.t; objective : string }
    | Clear of { id : Id.t }
    | Complete of { id : Id.t; summary : string option }
    | Block of { id : Id.t; reason : string option }
    | Budget_limited of { id : Id.t }

  let declare ~id ~objective ?token_budget () =
    if String.is_empty objective then
      invalid "Update.declare" "objective must not be empty";
    check_budget "Update.declare" token_budget;
    Declare { id; objective; token_budget }

  let pause ~id = Pause { id }

  let resume ~id ?token_budget () =
    check_budget "Update.resume" token_budget;
    Resume { id; token_budget }

  let edit ~id ~objective =
    if String.is_empty objective then
      invalid "Update.edit" "objective must not be empty";
    Edit { id; objective }

  let clear ~id = Clear { id }

  let complete ~id ?summary () =
    check_present "Update.complete" "summary" summary;
    Complete { id; summary }

  let block ~id ?reason () =
    check_present "Update.block" "reason" reason;
    Block { id; reason }

  let budget_limited ~id = Budget_limited { id }

  let id = function
    | Declare { id; _ }
    | Pause { id }
    | Resume { id; _ }
    | Edit { id; _ }
    | Clear { id }
    | Complete { id; _ }
    | Block { id; _ }
    | Budget_limited { id } ->
        id

  let equal a b = a = b

  let pp ppf = function
    | Declare { id; objective; _ } ->
        Format.fprintf ppf "declare(%a, %S)" Id.pp id objective
    | Pause { id } -> Format.fprintf ppf "pause(%a)" Id.pp id
    | Resume { id; _ } -> Format.fprintf ppf "resume(%a)" Id.pp id
    | Edit { id; objective } ->
        Format.fprintf ppf "edit(%a, %S)" Id.pp id objective
    | Clear { id } -> Format.fprintf ppf "clear(%a)" Id.pp id
    | Complete { id; _ } -> Format.fprintf ppf "complete(%a)" Id.pp id
    | Block { id; _ } -> Format.fprintf ppf "block(%a)" Id.pp id
    | Budget_limited { id } -> Format.fprintf ppf "budget-limited(%a)" Id.pp id

  let jsont =
    let declare_case =
      Jsont.Object.map ~kind:"goal declare" (fun id objective token_budget ->
          decode_invalid_arg (fun () -> declare ~id ~objective ?token_budget ()))
      |> Jsont.Object.mem "id" Id.jsont ~enc:(function
        | Declare { id; _ } -> id
        | _ -> assert false)
      |> Jsont.Object.mem "objective" Jsont.string ~enc:(function
        | Declare { objective; _ } -> objective
        | _ -> assert false)
      |> Jsont.Object.opt_mem "token_budget" Jsont.int ~enc:(function
        | Declare { token_budget; _ } -> token_budget
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "declare" ~dec:Fun.id
    in
    let pause_case =
      Jsont.Object.map ~kind:"goal pause" (fun id -> pause ~id)
      |> Jsont.Object.mem "id" Id.jsont ~enc:(function
        | Pause { id } -> id
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "pause" ~dec:Fun.id
    in
    let resume_case =
      Jsont.Object.map ~kind:"goal resume" (fun id token_budget ->
          decode_invalid_arg (fun () -> resume ~id ?token_budget ()))
      |> Jsont.Object.mem "id" Id.jsont ~enc:(function
        | Resume { id; _ } -> id
        | _ -> assert false)
      |> Jsont.Object.opt_mem "token_budget" Jsont.int ~enc:(function
        | Resume { token_budget; _ } -> token_budget
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "resume" ~dec:Fun.id
    in
    let edit_case =
      Jsont.Object.map ~kind:"goal edit" (fun id objective ->
          decode_invalid_arg (fun () -> edit ~id ~objective))
      |> Jsont.Object.mem "id" Id.jsont ~enc:(function
        | Edit { id; _ } -> id
        | _ -> assert false)
      |> Jsont.Object.mem "objective" Jsont.string ~enc:(function
        | Edit { objective; _ } -> objective
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "edit" ~dec:Fun.id
    in
    let clear_case =
      Jsont.Object.map ~kind:"goal clear" (fun id -> clear ~id)
      |> Jsont.Object.mem "id" Id.jsont ~enc:(function
        | Clear { id } -> id
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "clear" ~dec:Fun.id
    in
    let complete_case =
      Jsont.Object.map ~kind:"goal complete" (fun id summary ->
          decode_invalid_arg (fun () -> complete ~id ?summary ()))
      |> Jsont.Object.mem "id" Id.jsont ~enc:(function
        | Complete { id; _ } -> id
        | _ -> assert false)
      |> Jsont.Object.opt_mem "summary" Jsont.string ~enc:(function
        | Complete { summary; _ } -> summary
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "complete" ~dec:Fun.id
    in
    let block_case =
      Jsont.Object.map ~kind:"goal block" (fun id reason ->
          decode_invalid_arg (fun () -> block ~id ?reason ()))
      |> Jsont.Object.mem "id" Id.jsont ~enc:(function
        | Block { id; _ } -> id
        | _ -> assert false)
      |> Jsont.Object.opt_mem "reason" Jsont.string ~enc:(function
        | Block { reason; _ } -> reason
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "block" ~dec:Fun.id
    in
    let budget_limited_case =
      Jsont.Object.map ~kind:"goal budget-limited" (fun id ->
          budget_limited ~id)
      |> Jsont.Object.mem "id" Id.jsont ~enc:(function
        | Budget_limited { id } -> id
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "budget_limited" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make
        [
          declare_case;
          pause_case;
          resume_case;
          edit_case;
          clear_case;
          complete_case;
          block_case;
          budget_limited_case;
        ]
    in
    let enc_case = function
      | Declare _ as update -> Jsont.Object.Case.value declare_case update
      | Pause _ as update -> Jsont.Object.Case.value pause_case update
      | Resume _ as update -> Jsont.Object.Case.value resume_case update
      | Edit _ as update -> Jsont.Object.Case.value edit_case update
      | Clear _ as update -> Jsont.Object.Case.value clear_case update
      | Complete _ as update -> Jsont.Object.Case.value complete_case update
      | Block _ as update -> Jsont.Object.Case.value block_case update
      | Budget_limited _ as update ->
          Jsont.Object.Case.value budget_limited_case update
    in
    Jsont.Object.map ~kind:"goal update" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

type t = {
  id : Id.t;
  objective : string;
  status : Status.t;
  token_budget : int option;
  tokens_used : int;
  continuation_turns : int;
}

let make ~id ~objective ~status ~token_budget ~tokens_used ~continuation_turns =
  if String.is_empty objective then invalid "make" "objective must not be empty";
  check_budget "make" token_budget;
  if tokens_used < 0 then invalid "make" "tokens_used must not be negative";
  if continuation_turns < 0 then
    invalid "make" "continuation_turns must not be negative";
  if continuation_turns = 0 && tokens_used <> 0 then
    invalid "make" "tokens_used must be zero when continuation_turns is zero";
  (match status with
  | Status.Blocked { reason } -> check_present "make" "reason" reason
  | Status.Completed { summary } -> check_present "make" "summary" summary
  | Status.Active | Status.Paused | Status.Budget_limited | Status.Cleared -> ());
  { id; objective; status; token_budget; tokens_used; continuation_turns }

let id t = t.id
let objective t = t.objective
let status t = t.status
let token_budget t = t.token_budget
let tokens_used t = t.tokens_used
let continuation_turns t = t.continuation_turns

let remaining_tokens t =
  Option.map (fun budget -> Int.max 0 (budget - t.tokens_used)) t.token_budget

let equal a b =
  Id.equal a.id b.id
  && String.equal a.objective b.objective
  && Status.equal a.status b.status
  && Option.equal Int.equal a.token_budget b.token_budget
  && Int.equal a.tokens_used b.tokens_used
  && Int.equal a.continuation_turns b.continuation_turns

let pp ppf t =
  Format.fprintf ppf
    "@[<hov>{ id = %a; objective = %S; status = %a; tokens_used = %d }@]" Id.pp
    t.id t.objective Status.pp t.status t.tokens_used

let jsont =
  Jsont.Object.map ~kind:"session goal"
    (fun id objective status token_budget tokens_used continuation_turns ->
      decode_invalid_arg (fun () ->
          make ~id ~objective ~status ~token_budget ~tokens_used
            ~continuation_turns))
  |> Jsont.Object.mem "id" Id.jsont ~enc:id
  |> Jsont.Object.mem "objective" Jsont.string ~enc:objective
  |> Jsont.Object.mem "status" Status.jsont ~enc:status
  |> Jsont.Object.opt_mem "token_budget" Jsont.int ~enc:token_budget
  |> Jsont.Object.mem "tokens_used" Jsont.int ~enc:tokens_used
  |> Jsont.Object.mem "continuation_turns" Jsont.int ~enc:continuation_turns
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
