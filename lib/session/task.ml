(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

module Id =
  String_id.Make
    (struct
      let module_path = "Mentat_session.Task.Id"
      let kind = "task id"
    end)
    ()

module Owner = struct
  type t = Main

  let equal a b = match (a, b) with Main, Main -> true
  let pp ppf = function Main -> Format.pp_print_string ppf "main"

  let jsont =
    let main_case =
      Jsont.Object.map ~kind:"main owner" Main
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "main" ~dec:Fun.id
    in
    let cases = List.map Jsont.Object.Case.make [ main_case ] in
    let enc_case = function
      | Main as owner -> Jsont.Object.Case.value main_case owner
    in
    Jsont.Object.map ~kind:"task owner" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Status = struct
  type t = Pending | In_progress | Completed | Cancelled

  let equal a b = a = b

  let to_string = function
    | Pending -> "pending"
    | In_progress -> "in_progress"
    | Completed -> "completed"
    | Cancelled -> "cancelled"

  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum ~kind:"task status"
      [
        ("pending", Pending);
        ("in_progress", In_progress);
        ("completed", Completed);
        ("cancelled", Cancelled);
      ]
end

module Priority = struct
  type t = High | Medium | Low

  let equal a b = a = b
  let to_string = function High -> "high" | Medium -> "medium" | Low -> "low"
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum ~kind:"task priority"
      [ ("high", High); ("medium", Medium); ("low", Low) ]
end

module Error = struct
  type t =
    | Empty_content
    | Negative_position
    | Duplicate_id of Id.t
    | Non_contiguous_positions
    | Multiple_in_progress

  let message = function
    | Empty_content -> "content must not be empty"
    | Negative_position -> "position must not be negative"
    | Duplicate_id id -> "duplicate task id: " ^ Id.to_string id
    | Non_contiguous_positions -> "positions must be contiguous from zero"
    | Multiple_in_progress -> "an owner must have at most one in-progress task"

  let pp ppf t = Format.pp_print_string ppf (message t)
end

module Item = struct
  type t = {
    id : Id.t;
    owner : Owner.t;
    content : string;
    status : Status.t;
    priority : Priority.t;
    position : int;
  }

  let make ~id ~owner ~content ?(status = Status.Pending)
      ?(priority = Priority.Medium) ~position () =
    if String.is_empty content then Error Error.Empty_content
    else if position < 0 then Error Error.Negative_position
    else Ok { id; owner; content; status; priority; position }

  let equal a b =
    Id.equal a.id b.id
    && Owner.equal a.owner b.owner
    && String.equal a.content b.content
    && Status.equal a.status b.status
    && Priority.equal a.priority b.priority
    && Int.equal a.position b.position

  let pp ppf t =
    Format.fprintf ppf
      "@[<hov>{ id = %a; owner = %a; content = %S; status = %a; priority = %a; \
       position = %d }@]"
      Id.pp t.id Owner.pp t.owner t.content Status.pp t.status Priority.pp
      t.priority t.position

  let jsont =
    Jsont.Object.map ~kind:"task item"
      (fun id owner content status priority position ->
        match make ~id ~owner ~content ~status ~priority ~position () with
        | Ok item -> item
        | Error e -> decode_error (Error.message e))
    |> Jsont.Object.mem "id" Id.jsont ~enc:(fun t -> t.id)
    |> Jsont.Object.mem "owner" Owner.jsont ~enc:(fun t -> t.owner)
    |> Jsont.Object.mem "content" Jsont.string ~enc:(fun t -> t.content)
    |> Jsont.Object.mem "status" Status.jsont ~enc:(fun t -> t.status)
    |> Jsont.Object.mem "priority" Priority.jsont ~enc:(fun t -> t.priority)
    |> Jsont.Object.mem "position" Jsont.int ~enc:(fun t -> t.position)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Board = struct
  type t = Item.t list

  let check_unique_ids items =
    let rec loop seen = function
      | [] -> Ok ()
      | (item : Item.t) :: rest ->
          if List.exists (Id.equal item.Item.id) seen then
            Error (Error.Duplicate_id item.Item.id)
          else loop (item.Item.id :: seen) rest
    in
    loop [] items

  let check_owner owner_items =
    let positions =
      List.sort Int.compare
        (List.map (fun (item : Item.t) -> item.Item.position) owner_items)
    in
    let contiguous =
      List.equal Int.equal positions (List.init (List.length positions) Fun.id)
    in
    if not contiguous then Error Error.Non_contiguous_positions
    else
      let in_progress =
        List.length
          (List.filter
             (fun (item : Item.t) -> item.Item.status = Status.In_progress)
             owner_items)
      in
      if in_progress > 1 then Error Error.Multiple_in_progress else Ok ()

  let make items =
    match check_unique_ids items with
    | Error _ as e -> e
    | Ok () -> Result.map (fun () -> items) (check_owner items)

  let empty = []
  let items t = t
  let equal a b = List.equal Item.equal a b
  let pp ppf t = Format.fprintf ppf "board[%d task(s)]" (List.length t)

  let jsont =
    Jsont.map ~kind:"task board"
      ~dec:(fun items ->
        match make items with
        | Ok board -> board
        | Error e -> decode_error (Error.message e))
      ~enc:items (Jsont.list Item.jsont)
end
