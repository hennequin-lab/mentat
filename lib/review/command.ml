(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type review = Review.t

type t =
  | Mark_reviewed of Scope.t
  | Mark_unreviewed of Scope.t
  | Clear_mark of Scope.t
  | Approve
  | Set_pending
  | Set_cursor of Cursor.t
  | Move_cursor of { move : Cursor.move; wrap : bool }

let apply review = function
  | Mark_reviewed scope -> Review.mark_reviewed review scope
  | Mark_unreviewed scope -> Review.mark_unreviewed review scope
  | Clear_mark scope -> Ok (Review.clear_mark review scope)
  | Approve -> Ok (Review.approve review)
  | Set_pending -> Ok (Review.set_pending review)
  | Set_cursor cursor -> Review.set_cursor review cursor
  | Move_cursor { move; wrap } -> Ok (Review.move_cursor ~wrap review move)

let move_equal a b =
  match (a, b) with
  | Cursor.Next, Cursor.Next
  | Cursor.Previous, Cursor.Previous
  | Cursor.Next_file, Cursor.Next_file
  | Cursor.Previous_file, Cursor.Previous_file
  | Cursor.Next_cr, Cursor.Next_cr
  | Cursor.Previous_cr, Cursor.Previous_cr
  | Cursor.First, Cursor.First
  | Cursor.Last, Cursor.Last ->
      true
  | ( ( Cursor.Next | Cursor.Previous | Cursor.Next_file | Cursor.Previous_file
      | Cursor.Next_cr | Cursor.Previous_cr | Cursor.First | Cursor.Last ),
      _ ) ->
      false

let equal a b =
  match (a, b) with
  | Mark_reviewed a, Mark_reviewed b
  | Mark_unreviewed a, Mark_unreviewed b
  | Clear_mark a, Clear_mark b ->
      Scope.equal a b
  | Approve, Approve | Set_pending, Set_pending -> true
  | Set_cursor a, Set_cursor b -> Cursor.equal a b
  | Move_cursor a, Move_cursor b ->
      move_equal a.move b.move && Bool.equal a.wrap b.wrap
  | ( ( Mark_reviewed _ | Mark_unreviewed _ | Clear_mark _ | Approve
      | Set_pending | Set_cursor _ | Move_cursor _ ),
      _ ) ->
      false

let move_string = function
  | Cursor.Next -> "next"
  | Cursor.Previous -> "previous"
  | Cursor.Next_file -> "next_file"
  | Cursor.Previous_file -> "previous_file"
  | Cursor.Next_cr -> "next_cr"
  | Cursor.Previous_cr -> "previous_cr"
  | Cursor.First -> "first"
  | Cursor.Last -> "last"

let pp ppf = function
  | Mark_reviewed scope -> Format.fprintf ppf "mark_reviewed(%a)" Scope.pp scope
  | Mark_unreviewed scope ->
      Format.fprintf ppf "mark_unreviewed(%a)" Scope.pp scope
  | Clear_mark scope -> Format.fprintf ppf "clear_mark(%a)" Scope.pp scope
  | Approve -> Format.pp_print_string ppf "approve"
  | Set_pending -> Format.pp_print_string ppf "set_pending"
  | Set_cursor cursor -> Format.fprintf ppf "set_cursor(%a)" Cursor.pp cursor
  | Move_cursor { move; wrap } ->
      Format.fprintf ppf "move_cursor(%s, wrap=%b)" (move_string move) wrap

let move_jsont =
  Jsont.enum ~kind:"cursor move"
    [
      ("next", Cursor.Next);
      ("previous", Cursor.Previous);
      ("next_file", Cursor.Next_file);
      ("previous_file", Cursor.Previous_file);
      ("next_cr", Cursor.Next_cr);
      ("previous_cr", Cursor.Previous_cr);
      ("first", Cursor.First);
      ("last", Cursor.Last);
    ]

let jsont =
  let scope_command tag build project =
    Jsont.Object.map ~kind:(tag ^ " command") build
    |> Jsont.Object.mem "scope" Codec.scope ~enc:project
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map tag ~dec:Fun.id
  in
  let mark_reviewed_case =
    scope_command "mark_reviewed"
      (fun scope -> Mark_reviewed scope)
      (function Mark_reviewed scope -> scope | _ -> assert false)
  in
  let mark_unreviewed_case =
    scope_command "mark_unreviewed"
      (fun scope -> Mark_unreviewed scope)
      (function Mark_unreviewed scope -> scope | _ -> assert false)
  in
  let clear_mark_case =
    scope_command "clear_mark"
      (fun scope -> Clear_mark scope)
      (function Clear_mark scope -> scope | _ -> assert false)
  in
  let approve_case =
    Jsont.Object.map ~kind:"approve command" Approve
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "approve" ~dec:Fun.id
  in
  let set_pending_case =
    Jsont.Object.map ~kind:"set_pending command" Set_pending
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "set_pending" ~dec:Fun.id
  in
  let set_cursor_case =
    Jsont.Object.map ~kind:"set_cursor command" (fun cursor ->
        Set_cursor cursor)
    |> Jsont.Object.mem "cursor" Codec.cursor ~enc:(function
      | Set_cursor cursor -> cursor
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "set_cursor" ~dec:Fun.id
  in
  let move_cursor_case =
    Jsont.Object.map ~kind:"move_cursor command" (fun move wrap ->
        Move_cursor { move; wrap })
    |> Jsont.Object.mem "move" move_jsont ~enc:(function
      | Move_cursor { move; _ } -> move
      | _ -> assert false)
    |> Jsont.Object.mem "wrap" Jsont.bool ~enc:(function
      | Move_cursor { wrap; _ } -> wrap
      | _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "move_cursor" ~dec:Fun.id
  in
  let cases =
    List.map Jsont.Object.Case.make
      [
        mark_reviewed_case;
        mark_unreviewed_case;
        clear_mark_case;
        approve_case;
        set_pending_case;
        set_cursor_case;
        move_cursor_case;
      ]
  in
  let enc_case = function
    | Mark_reviewed _ as c -> Jsont.Object.Case.value mark_reviewed_case c
    | Mark_unreviewed _ as c -> Jsont.Object.Case.value mark_unreviewed_case c
    | Clear_mark _ as c -> Jsont.Object.Case.value clear_mark_case c
    | Approve as c -> Jsont.Object.Case.value approve_case c
    | Set_pending as c -> Jsont.Object.Case.value set_pending_case c
    | Set_cursor _ as c -> Jsont.Object.Case.value set_cursor_case c
    | Move_cursor _ as c -> Jsont.Object.Case.value move_cursor_case c
  in
  Jsont.Object.map ~kind:"review command" Fun.id
  |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
