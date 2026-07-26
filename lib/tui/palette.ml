(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module User_command = Mentat_protocol.User_command

type row = Builtin of Command.t | Custom of User_command.t
type t = { query : string; rows : row list; selected : int }
type activation = Run of Command.t | Insert of string | Expand of string
type msg = Select of int | Activate of int
type event = Stay | Activated of activation

let trim_horizontal text =
  let is_horizontal = function ' ' | '\t' -> true | _ -> false in
  let length = String.length text in
  let rec first index =
    if index < length && is_horizontal text.[index] then first (index + 1)
    else index
  in
  let rec last index =
    if index >= 0 && is_horizontal text.[index] then last (index - 1) else index
  in
  let first = first 0 in
  if first = length then ""
  else
    let last = last (length - 1) in
    String.sub text first (last - first + 1)

let canonical_query query = query |> trim_horizontal |> String.lowercase_ascii
let make = { query = ""; rows = []; selected = 0 }

let clamp lo hi value =
  if value < lo then lo else if value > hi then hi else value

let with_rows ~query rows t =
  let query = canonical_query query in
  let count = List.length rows in
  let selected =
    if String.equal query t.query then clamp 0 (max 0 (count - 1)) t.selected
    else 0
  in
  { query; rows; selected }

(* The intrinsic primary column: a builtin's slash spelling, or its title when
   the row has no slash (a key- or palette-only action), or a custom command's
   slash spelling. Slash-reachable rows keep their slash so completion measures a
   real prefix. *)
let row_slash = function
  | Builtin command -> (
      match Command.slash command with
      | Some slash -> slash
      | None -> Command.title command)
  | Custom command ->
      "/" ^ User_command.Name.to_string command.User_command.name

let row_argument_hint = function
  | Builtin command -> Command.argument_hint command
  | Custom command -> command.User_command.argument_hint

let selected_row t = List.nth_opt t.rows t.selected

let selected_command t =
  match selected_row t with
  | Some (Builtin command) -> Some command
  | Some (Custom _) | None -> None

let move direction t =
  let count = List.length t.rows in
  if count = 0 then t
  else
    let step = match direction with `Up -> -1 | `Down -> 1 in
    { t with selected = Option_list.wrap ~count t.selected step }

(* A hint present means the command takes a trailing argument, so activation
   inserts its slash spelling and a space instead of dispatching; a hintless
   builtin runs and a hintless custom command expands with empty arguments. *)
let activate t =
  match selected_row t with
  | None -> None
  | Some (Builtin command as row) -> (
      match (row_argument_hint row, Command.slash command) with
      | None, _ | Some _, None -> Some (Run command)
      | Some _, Some slash -> Some (Insert (slash ^ " ")))
  | Some (Custom command as row) -> (
      match row_argument_hint row with
      | None ->
          Some (Expand (User_command.Name.to_string command.User_command.name))
      | Some _ -> Some (Insert (row_slash row ^ " ")))

let select index t =
  let count = List.length t.rows in
  if count = 0 then t else { t with selected = clamp 0 (count - 1) index }

let update message t =
  match message with
  | Select index -> (select index t, Stay)
  | Activate index -> (
      let t = select index t in
      match activate t with
      | None -> (t, Stay)
      | Some activation -> (t, Activated activation))

(* Slash spellings are canonical lowercase ASCII, so their common prefix can be
   measured safely as bytes. *)
let common_prefix left right =
  let length = min (String.length left) (String.length right) in
  let rec loop index =
    if index < length && Char.equal left.[index] right.[index] then
      loop (index + 1)
    else index
  in
  String.sub left 0 (loop 0)

let complete t =
  match t.rows with
  | [] -> None
  | first :: rest ->
      let prefix =
        List.fold_left
          (fun prefix row -> common_prefix prefix (row_slash row))
          (row_slash first) rest
      in
      let token = "/" ^ t.query in
      if
        String.length prefix > String.length token
        && String.starts_with ~prefix:token prefix
      then Some prefix
      else None

let scope_tag = function
  | User_command.Project -> "project"
  | User_command.User -> "user"

let row_description = function
  | Builtin command -> Command.description command
  | Custom command -> (
      match command.User_command.description with
      | Some text -> text
      | None -> "")

(* The trailing faint column: a builtin's argument hint, or a custom command's
   argument hint (when present) followed by a parenthesized scope tag that marks
   it as a project or user command, never a builtin. *)
let row_hint = function
  | Builtin command -> Command.argument_hint command
  | Custom command ->
      let tag = "(" ^ scope_tag command.User_command.scope ^ ")" in
      Some
        (match command.User_command.argument_hint with
        | Some hint -> hint ^ " " ^ tag
        | None -> tag)

let row_view ~palette ~command_mode ~keybind ~selected row =
  let slash_style =
    if selected then Some (Theme.Palette.accent_style palette) else None
  in
  let primary = Completion_list.segment ?style:slash_style (row_slash row) in
  let detail =
    Completion_list.segment
      ~style:(Theme.Palette.muted_style palette)
      ("  " ^ row_description row)
  in
  (* In command-palette mode the trailing faint column is the current keybind,
     matching opencode's [title · category · keybind]; the slash palette keeps
     its argument hint and custom-command scope tag. *)
  let trailing = if command_mode then keybind row else row_hint row in
  let hint =
    Option.map
      (fun hint ->
        Completion_list.segment
          ~style:(Theme.Palette.faint_style palette)
          ("  " ^ hint))
      trailing
  in
  Completion_list.row ~detail ?hint primary

let view ~palette ?(command_mode = false) ?(keybind = fun _ -> None) t =
  match t.rows with
  | [] -> Completion_list.note ~palette "no matching commands"
  | rows ->
      rows
      |> List.mapi (fun index row ->
          row_view ~palette ~command_mode ~keybind
            ~selected:(index = t.selected) row)
      |> Completion_list.view ~palette ~selected:t.selected
           ~on_select:(fun index -> Select index)
           ~on_activate:(fun index -> Activate index)
