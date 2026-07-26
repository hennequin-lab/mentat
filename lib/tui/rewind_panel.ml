(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Turn_id = Mentat_session.Turn.Id

module Target = struct
  type t = { turn : Turn_id.t; text : string; at : float }

  let make ~turn ~text ~at = { turn; text; at }
  let turn t = t.turn
  let text t = t.text
  let at t = t.at
end

open Mosaic

type t = { targets : Target.t list; filter : string; selected : int }
type msg = Key of Panel.key
type event = Stay | Close | Arm of Turn_id.t

let make targets = { targets; filter = ""; selected = 0 }

(* The picker's own filter law over the target text, matched with the same
   case-insensitive substring rule the slash palette uses. *)
let matches ~query target =
  let query = String.lowercase_ascii query in
  if String.equal query "" then true
  else
    let haystack = String.lowercase_ascii (Target.text target) in
    let nl = String.length query and hl = String.length haystack in
    if nl > hl then false
    else
      let rec loop i =
        if i + nl > hl then false
        else if String.equal (String.sub haystack i nl) query then true
        else loop (i + 1)
      in
      loop 0

let visible t = List.filter (matches ~query:t.filter) t.targets

let clamp_selection t =
  let count = List.length (visible t) in
  if count = 0 then { t with selected = 0 }
  else { t with selected = max 0 (min t.selected (count - 1)) }

let drop_last_grapheme text =
  let last = ref None in
  Matrix.Text.iter_graphemes
    (fun ~offset ~len ->
      let end_offset = offset + len in
      if end_offset <= String.length text then last := Some offset)
    text;
  match !last with None -> text | Some offset -> String.sub text 0 offset

let table_page_key (event : Matrix.Input.Key.event) =
  match event.Matrix.Input.Key.key with
  | Matrix.Input.Key.Page_up | Matrix.Input.Key.Page_down -> true
  | _ -> false

let key event =
  if table_page_key event then None
  else
    match Panel.classify event with
    | Panel.Action Panel.Other -> None
    | message -> Some (Key message)

let move delta t =
  let count = List.length (visible t) in
  if count = 0 then t
  else
    let selected = Option_list.wrap ~count t.selected delta in
    { t with selected }

let narrow appended t =
  let filter = t.filter ^ Prims.normalize_inline appended in
  clamp_selection { t with filter; selected = 0 }

let backspace t =
  if String.equal t.filter "" then t
  else
    clamp_selection
      { t with filter = drop_last_grapheme t.filter; selected = 0 }

let arm_selected t =
  match List.nth_opt (visible t) t.selected with
  | None -> (t, Stay)
  | Some target -> (t, Arm (Target.turn target))

let jump number t =
  if number < 1 then (t, Stay)
  else
    match List.nth_opt (visible t) (number - 1) with
    | None -> (t, Stay)
    | Some target -> (t, Arm (Target.turn target))

let update (Key key) t =
  match key with
  | Panel.Action Panel.Escape -> (t, Close)
  | Panel.Action Panel.Enter -> arm_selected t
  | Panel.Action Panel.Up -> (move (-1) t, Stay)
  | Panel.Action Panel.Down -> (move 1 t, Stay)
  | Panel.Action Panel.Backspace -> (backspace t, Stay)
  | Panel.Printable text -> (narrow text t, Stay)
  | Panel.Digit number ->
      if String.equal t.filter "" then jump number t
      else (narrow (string_of_int number) t, Stay)
  | Panel.Action
      (Panel.Tab | Panel.Left | Panel.Right | Panel.Ctrl_d | Panel.Other) ->
      (t, Stay)

(* Relative age in the same vocabulary the session switcher uses, computed over
   plain second-valued fold instants rather than session timestamps. *)
let relative_age ~now at =
  let seconds = if now <= at then 0. else now -. at in
  let seconds = int_of_float seconds in
  if seconds < 60 then "just now"
  else if seconds < 3600 then Printf.sprintf "%dm ago" (seconds / 60)
  else if seconds < 86400 then Printf.sprintf "%dh ago" (seconds / 3600)
  else Printf.sprintf "%dd ago" (seconds / 86400)

let first_visible_line text =
  let text = Prims.normalize_inline text in
  match String.index_opt text '\n' with
  | None -> text
  | Some index -> String.sub text 0 index

let zero_size = { width = px 0; height = px 0 }
let fill_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = px 0 }

let paragraph ~palette ?(style = Theme.Palette.muted_style palette) value =
  text ~style ~wrap:`Word ~flex_shrink:0. ~size:fill_width value

let scroll ~key children =
  scroll_box ~key ~scroll_x:false ~scroll_y:true ~show_scrollbars:true
    ~focusable:false ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
    ~size:fill_remaining
    [
      box ~flex_direction:Flex_direction.Column ~padding:(padding_lrtb 2 2 0 0)
        ~flex_shrink:0. ~size:fill_width children;
    ]

let row ~palette ~now ~selected ~number target =
  let marker = if selected then Theme.cursor else " " in
  let ordinal = if number <= 9 then string_of_int number else "·" in
  let age = relative_age ~now (Target.at target) in
  box ~flex_direction:Flex_direction.Row ~align_items:Align.Flex_start
    ~flex_shrink:0.
    ~gap:{ width = px 1; height = px 0 }
    ~size:fill_width
    [
      text
        ~style:(Theme.Palette.accent_style palette)
        ~wrap:`None ~flex_shrink:0. marker;
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`None ~flex_shrink:0. ordinal;
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`None ~flex_shrink:0. age;
      text
        ~style:
          (if selected then Theme.Palette.user_style palette
           else Theme.Palette.muted_style palette)
        ~wrap:`None ~truncate:true ~flex_grow:1. ~flex_shrink:1.
        ~min_size:zero_size
        (first_visible_line (Target.text target));
    ]

let content ~palette ~now t =
  match visible t with
  | [] when t.targets = [] ->
      scroll ~key:"rewind.empty"
        [ paragraph ~palette "No earlier messages to rewind to." ]
  | [] ->
      scroll ~key:"rewind.no-match"
        [ paragraph ~palette "No matching messages." ]
  | rows ->
      scroll ~key:"rewind.rows"
        (List.mapi
           (fun index target ->
             row ~palette ~now ~selected:(index = t.selected)
               ~number:(index + 1) target)
           rows)

let hints t =
  match visible t with
  | [] -> [ "esc cancel" ]
  | _ -> [ "↵ edit & resubmit"; "type to filter"; "↑↓ choose"; "esc cancel" ]

let view ~palette ~now ~frame t =
  let filter = Prims.normalize_inline t.filter in
  Panel.view ~palette ~frame ~name:"rewind to message" ~filter ~hint:(hints t)
    ~content:(content ~palette ~now t)
