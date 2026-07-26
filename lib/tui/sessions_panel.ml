(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Summary = Mentat_session.Summary
module Id = Mentat_session.Id
module Time = Mentat_session.Time
module Id_set = Set.Make (Id)

let default_style = Ansi.Style.default
let replacement = Uchar.rep

let add_inline_uchar buffer uchar =
  let code = Uchar.to_int uchar in
  match code with
  | 0x09 | 0x0A | 0x0D | 0x2028 | 0x2029 -> Buffer.add_char buffer ' '
  | code when code < 0x20 || (code >= 0x7F && code <= 0x9F) ->
      Buffer.add_utf_8_uchar buffer replacement
  | _ -> Buffer.add_utf_8_uchar buffer uchar

(* Summary fields and query diagnostics cross the transport boundary. Decode
   them before Mosaic sees them so malformed bytes and terminal controls are
   inert even when a remote responder is faulty. *)
let normalize_inline text =
  let buffer = Buffer.create (String.length text) in
  let rec loop offset =
    if offset < String.length text then begin
      let decoded = String.get_utf_8_uchar text offset in
      let length = Uchar.utf_decode_length decoded in
      if Uchar.utf_decode_is_valid decoded then
        add_inline_uchar buffer (Uchar.utf_decode_uchar decoded)
      else Buffer.add_utf_8_uchar buffer replacement;
      loop (offset + length)
    end
  in
  loop 0;
  Buffer.contents buffer

let is_white_space uchar =
  match Uchar.to_int uchar with
  | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x20 | 0x85 | 0xA0 | 0x1680 | 0x2028
  | 0x2029 | 0x202F | 0x205F | 0x3000 ->
      true
  | code -> code >= 0x2000 && code <= 0x200A

let has_non_white_space text =
  let rec loop offset =
    if offset >= String.length text then false
    else
      let decoded = String.get_utf_8_uchar text offset in
      let uchar = Uchar.utf_decode_uchar decoded in
      if is_white_space uchar then
        loop (offset + Uchar.utf_decode_length decoded)
      else true
  in
  loop 0

let normalize_error message =
  let message = normalize_inline message in
  if has_non_white_space message then message else "session history unavailable"

let display_title summary =
  let title = Summary.display_title summary |> normalize_inline in
  if has_non_white_space title then title else Id.to_string (Summary.id summary)

let drop_last_grapheme text =
  let last = ref None in
  Matrix.Text.iter_graphemes
    (fun ~offset ~len ->
      let end_offset = offset + len in
      if end_offset <= String.length text then last := Some offset)
    text;
  match !last with None -> text | Some offset -> String.sub text 0 offset

type ready = { rows : Summary.t list; filter : string; selected : Id.t option }
type failure = { message : string; retained : ready option }
type t = Loading | Failed of failure | Ready of ready
type msg = Key of Panel.key | Select_index of int | Activate_index of int

type event =
  | Stay
  | Close
  | Resume of Id.t
  | Promote of { filter : string; select : Id.t option }

let loading = Loading

(* Summary order is product semantics, not viewport geometry. Normalize it at
   this boundary and keep one row per exact identifier, but retain every
   healthy summary: Mosaic decides which rows fit and reveals the selection. *)
let distinct_by_recency rows =
  let rec collect seen selected = function
    | [] -> List.rev selected
    | row :: rest ->
        let id = Summary.id row in
        if Id_set.mem id seen then collect seen selected rest
        else collect (Id_set.add id seen) (row :: selected) rest
  in
  rows |> List.stable_sort Summary.compare_recency |> collect Id_set.empty []

let visible ready =
  if String.equal ready.filter "" then ready.rows
  else List.filter (Summary.matches ~query:ready.filter) ready.rows

let first_id = function [] -> None | row :: _ -> Some (Summary.id row)

let contains_id id rows =
  List.exists (fun row -> Id.equal id (Summary.id row)) rows

let stabilize ready =
  let rows = visible ready in
  let selected =
    match ready.selected with
    | Some id when contains_id id rows -> Some id
    | Some _ | None -> first_id rows
  in
  { ready with selected }

let retained = function
  | Loading -> None
  | Ready ready -> Some ready
  | Failed failure -> failure.retained

let loaded summaries t =
  let rows = distinct_by_recency summaries in
  let filter, selected =
    match retained t with
    | None -> ("", None)
    | Some ready -> (ready.filter, ready.selected)
  in
  Ready (stabilize { rows; filter; selected })

let failed message t =
  Failed { message = normalize_error message; retained = retained t }

let table_page_key (event : Matrix.Input.Key.event) =
  let open Matrix.Input in
  match event.Key.key with Key.Page_up | Key.Page_down -> true | _ -> false

let key event =
  if table_page_key event then None
  else
    match Panel.classify event with
    | Panel.Action Panel.Other -> None
    | message -> Some (Key message)

let selected_row ready =
  match ready.selected with
  | None -> None
  | Some id ->
      List.find_opt (fun row -> Id.equal id (Summary.id row)) (visible ready)

let selected_index ready rows =
  match ready.selected with
  | None -> 0
  | Some id ->
      let rec find index = function
        | [] -> 0
        | row :: rest ->
            if Id.equal id (Summary.id row) then index
            else find (index + 1) rest
      in
      find 0 rows

let move delta ready =
  let rows = visible ready in
  match rows with
  | [] -> { ready with selected = None }
  | _ ->
      let count = List.length rows in
      let index = selected_index ready rows in
      let index = Option_list.wrap ~count index delta in
      let selected = Option.map Summary.id (List.nth_opt rows index) in
      { ready with selected }

let narrow appended ready =
  let appended = normalize_inline appended in
  stabilize { ready with filter = ready.filter ^ appended; selected = None }

let backspace ready =
  if String.equal ready.filter "" then ready
  else
    stabilize
      { ready with filter = drop_last_grapheme ready.filter; selected = None }

let jump number ready =
  if number < 1 then (ready, Stay)
  else
    match List.nth_opt (visible ready) (number - 1) with
    | None -> (ready, Stay)
    | Some row -> (ready, Resume (Summary.id row))

let select_index index ready =
  match List.nth_opt (visible ready) index with
  | None -> ready
  | Some row -> { ready with selected = Some (Summary.id row) }

let activate_index index ready =
  match List.nth_opt (visible ready) index with
  | None -> (ready, Stay)
  | Some row ->
      let ready = { ready with selected = Some (Summary.id row) } in
      (ready, Resume (Summary.id row))

let update_ready message ready =
  match message with
  | Select_index index -> (select_index index ready, Stay)
  | Activate_index index -> activate_index index ready
  | Key (Panel.Action Panel.Escape) -> (ready, Close)
  | Key (Panel.Action Panel.Enter) -> (
      match selected_row ready with
      | None -> (ready, Stay)
      | Some row -> (ready, Resume (Summary.id row)))
  | Key (Panel.Action Panel.Tab) ->
      let select = Option.map Summary.id (selected_row ready) in
      (ready, Promote { filter = ready.filter; select })
  | Key (Panel.Action Panel.Up) -> (move (-1) ready, Stay)
  | Key (Panel.Action Panel.Down) -> (move 1 ready, Stay)
  | Key (Panel.Action Panel.Backspace) -> (backspace ready, Stay)
  | Key (Panel.Printable text) -> (narrow text ready, Stay)
  | Key (Panel.Digit number) ->
      if String.equal ready.filter "" then jump number ready
      else (narrow (string_of_int number) ready, Stay)
  | Key (Panel.Action (Panel.Left | Panel.Right | Panel.Ctrl_d | Panel.Other))
    ->
      (ready, Stay)

let update message = function
  | Loading as t -> (
      match message with
      | Key (Panel.Action Panel.Escape) -> (t, Close)
      | Select_index _ | Activate_index _
      | Key (Panel.Printable _)
      | Key (Panel.Digit _)
      | Key
          (Panel.Action
             ( Panel.Enter | Panel.Tab | Panel.Left | Panel.Right | Panel.Up
             | Panel.Down | Panel.Backspace | Panel.Ctrl_d | Panel.Other )) ->
          (t, Stay))
  | Failed ({ retained = None; _ } as failure) -> (
      match message with
      | Key (Panel.Action Panel.Escape) -> (Failed failure, Close)
      | Select_index _ | Activate_index _
      | Key (Panel.Printable _)
      | Key (Panel.Digit _)
      | Key
          (Panel.Action
             ( Panel.Enter | Panel.Tab | Panel.Left | Panel.Right | Panel.Up
             | Panel.Down | Panel.Backspace | Panel.Ctrl_d | Panel.Other )) ->
          (Failed failure, Stay))
  | Ready ready ->
      let ready, event = update_ready message ready in
      (Ready ready, event)
  | Failed ({ retained = Some ready; _ } as failure) ->
      let ready, event = update_ready message ready in
      (Failed { failure with retained = Some ready }, event)

let relative_age ~now updated_at =
  let now_ms = Time.to_unix_ms now in
  let updated_ms = Time.to_unix_ms updated_at in
  let seconds =
    if Int64.compare updated_ms now_ms >= 0 then 0L
    else Int64.div (Int64.sub now_ms updated_ms) 1_000L
  in
  let minute = 60L in
  let hour = 3_600L in
  let day = 86_400L in
  let week = 604_800L in
  let month = 2_592_000L in
  let year = 31_536_000L in
  if Int64.compare seconds minute < 0 then "just now"
  else if Int64.compare seconds hour < 0 then
    Printf.sprintf "%Ldm ago" (Int64.div seconds minute)
  else if Int64.compare seconds day < 0 then
    Printf.sprintf "%Ldh ago" (Int64.div seconds hour)
  else if Int64.compare seconds week < 0 then
    Printf.sprintf "%Ldd ago" (Int64.div seconds day)
  else if Int64.compare seconds month < 0 then
    Printf.sprintf "%Ldw ago" (Int64.div seconds week)
  else if Int64.compare seconds year < 0 then
    Printf.sprintf "%Ldmo ago" (Int64.div seconds month)
  else Printf.sprintf "%Ldy ago" (Int64.div seconds year)

let zero_size = { width = px 0; height = px 0 }
let fill_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = px 0 }
let vertical_gap = { width = px 0; height = px 1 }

let paragraph ?(style = default_style) value =
  text ~style ~wrap:`Word ~flex_shrink:0. ~size:fill_width value

let error ~palette message =
  box ~flex_direction:Flex_direction.Row ~flex_wrap:Flex_wrap.Wrap
    ~align_items:Align.Flex_start
    ~gap:{ width = px 1; height = px 0 }
    ~flex_shrink:0. ~size:fill_width
    [
      text
        ~style:(Theme.Palette.error_style palette)
        ~wrap:`None ~flex_shrink:0. "!";
      text
        ~style:(Theme.Palette.error_style palette)
        ~wrap:`Word ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size message;
    ]

let scroll_state ~key children =
  scroll_box ~key ~scroll_x:false ~scroll_y:true ~show_scrollbars:true
    ~focusable:false ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
    ~size:fill_remaining
    [
      box ~flex_direction:Flex_direction.Column ~gap:vertical_gap
        ~padding:(padding_lrtb 2 2 0 0) ~flex_shrink:0. ~size:fill_width
        children;
    ]

let refresh_error ~palette message =
  scroll_box ~key:"sessions.refresh-error" ~scroll_x:false ~scroll_y:true
    ~show_scrollbars:false ~focusable:false ~flex_shrink:1. ~min_size:zero_size
    ~size:fill_width
    [
      box ~padding:(padding_lrtb 2 2 0 0) ~flex_shrink:0. ~size:fill_width
        [ error ~palette message ];
    ]

let phase_style ~palette = function
  | Summary.Phase.Idle -> Theme.Palette.muted_style palette
  | Summary.Phase.Working -> Theme.Palette.running_style palette
  | Summary.Phase.Waiting -> Theme.Palette.warning_style palette

let table_columns =
  [
    Table.column ~width:`Auto ~overflow:`Crop "";
    Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis "session";
    Table.column ~width:`Auto ~overflow:`Ellipsis "status";
    Table.column ~width:`Auto ~alignment:`Right ~overflow:`Ellipsis "updated";
  ]

let table_row ~palette ~now ~selected summary =
  let phase = Summary.phase summary in
  [|
    Table.cell
      ~style:(Theme.Palette.accent_style palette)
      (if selected then "❯" else "");
    Table.cell (display_title summary);
    Table.cell
      ~style:(phase_style ~palette phase)
      (Summary.Phase.to_string phase);
    Table.cell
      ~style:(Theme.Palette.muted_style palette)
      (relative_age ~now (Summary.updated_at summary));
  |]

let selected_table_index ready rows =
  match ready.selected with
  | None -> 0
  | Some id ->
      let rec find index = function
        | [] -> 0
        | summary :: rest ->
            if Id.equal id (Summary.id summary) then index
            else find (index + 1) rest
      in
      find 0 rows

let widget_owns_key (event : Matrix.Input.Key.event) =
  let open Matrix.Input in
  let modifier = event.Key.modifier in
  let plain =
    not
      (modifier.Modifier.shift || modifier.Modifier.ctrl
     || modifier.Modifier.alt || modifier.Modifier.super)
  in
  match event.Key.key with
  | Key.Up | Key.Down -> plain
  | Key.Page_up | Key.Page_down -> true
  | Key.Enter | Key.KP_enter -> true
  | _ -> false

let table_key event =
  let data = Event.Key.data event in
  if widget_owns_key data then None
  else
    let message = key data in
    Event.Key.prevent_default event;
    message

let sessions_table ~palette ~now ready rows =
  let selected = selected_table_index ready rows in
  let table_rows =
    List.mapi
      (fun index summary ->
        table_row ~palette ~now ~selected:(index = selected) summary)
      rows
  in
  table ~key:"sessions.table" ~columns:table_columns ~rows:table_rows
    ~selected_row:selected ~border:false ~show_header:false
    ~show_column_separator:false ~show_row_separator:false ~cell_padding:1
    ~text_color:Ansi.Color.default ~background:Ansi.Color.default
    ~selected_text_color:(Theme.Palette.selection_fg palette)
    ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
    ~selected_background:(Theme.Palette.selection_bg palette)
    ~focused_selected_background:(Theme.Palette.selection_bg palette)
    ~wrap_selection:true ~show_scroll_indicator:true ~autofocus:true
    ~on_key:table_key ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
    ~size:fill_remaining ~activate_on_click:true
    ~on_change:(fun index -> Some (Select_index index))
    ~on_activate:(fun index -> Some (Activate_index index))
    ()

let ready_content ~palette ~now ?refresh ready =
  match ready.rows with
  | [] ->
      let children =
        Option.fold ~none:[]
          ~some:(fun message -> [ error ~palette message ])
          refresh
        @ [
            paragraph
              ~style:(Theme.Palette.muted_style palette)
              "No recent sessions in this workspace.";
          ]
      in
      scroll_state ~key:"sessions.empty" children
  | _ -> (
      match visible ready with
      | [] ->
          let children =
            Option.fold ~none:[]
              ~some:(fun message -> [ error ~palette message ])
              refresh
            @ [
                paragraph
                  ~style:(Theme.Palette.muted_style palette)
                  "No matching sessions.";
              ]
          in
          scroll_state ~key:"sessions.no-match" children
      | rows -> (
          let table = sessions_table ~palette ~now ready rows in
          match refresh with
          | None -> table
          | Some message ->
              box ~key:"sessions.retained" ~flex_direction:Flex_direction.Column
                ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
                ~size:fill_remaining
                [ refresh_error ~palette message; table ]))

let content ~palette ~now = function
  | Loading ->
      scroll_state ~key:"sessions.loading"
        [
          paragraph
            ~style:(Theme.Palette.muted_style palette)
            "⠋ loading sessions…";
        ]
  | Ready ready -> ready_content ~palette ~now ready
  | Failed { message; retained = None } ->
      scroll_state ~key:"sessions.error" [ error ~palette message ]
  | Failed { message; retained = Some ready } ->
      ready_content ~palette ~now ~refresh:message ready

let filter = function
  | Loading -> ""
  | Ready ready -> ready.filter
  | Failed { retained = None; _ } -> ""
  | Failed { retained = Some ready; _ } -> ready.filter

let has_selection = function
  | Loading | Failed { retained = None; _ } -> false
  | Ready ready | Failed { retained = Some ready; _ } ->
      Option.is_some (selected_row ready)

let interactive = function
  | Loading | Failed { retained = None; _ } -> false
  | Ready _ | Failed { retained = Some _; _ } -> true

let hints t =
  if not (interactive t) then [ "esc close" ]
  else
    (if has_selection t then [ "↵ resume" ] else [])
    @ [ "tab browse"; "type to filter"; "↑↓ select"; "esc close" ]

let view ~palette ~now ~frame t =
  let filter = filter t |> normalize_inline in
  let hints = hints t in
  Panel.view ~palette ~frame ~name:"sessions" ~filter ~hint:hints
    ~content:(content ~palette ~now t)
