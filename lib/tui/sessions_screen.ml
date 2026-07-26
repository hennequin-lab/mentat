(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Summary = Mentat_session.Summary
module Id = Mentat_session.Id
module Time = Mentat_session.Time
module Status = Mentat_session.Metadata.Status
module Forked_from = Mentat_session.Metadata.Forked_from

module Id_set = Set.Make (struct
  type t = Id.t

  let compare = Id.compare
end)

let default_style = Ansi.Style.default

(* Every string reaching Mosaic is inert one-line UTF-8. Summary values are
   owner-validated, but this boundary remains defensive against faulty remote
   responders and hostile query diagnostics. *)
let normalize_inline = Prims.normalize_inline

let is_white_space uchar =
  match Uchar.to_int uchar with
  | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x20 | 0x85 | 0xA0 | 0x1680 | 0x2028
  | 0x2029 | 0x202F | 0x205F | 0x3000 ->
      true
  | code -> code >= 0x2000 && code <= 0x200A

let has_non_white_space text =
  let found = ref false in
  let rec loop offset =
    if offset < String.length text && not !found then begin
      let decoded = String.get_utf_8_uchar text offset in
      let uchar = Uchar.utf_decode_uchar decoded in
      if not (is_white_space uchar) then found := true;
      loop (offset + Uchar.utf_decode_length decoded)
    end
  in
  loop 0;
  !found

let trim_unicode text =
  let first = ref None in
  let last = ref 0 in
  let rec loop offset =
    if offset < String.length text then begin
      let decoded = String.get_utf_8_uchar text offset in
      let length = Uchar.utf_decode_length decoded in
      if not (is_white_space (Uchar.utf_decode_uchar decoded)) then begin
        if Option.is_none !first then first := Some offset;
        last := offset + length
      end;
      loop (offset + length)
    end
  in
  loop 0;
  match !first with
  | None -> ""
  | Some offset -> String.sub text offset (!last - offset)

let drop_last_grapheme text =
  let last = ref None in
  Matrix.Text.iter_graphemes
    (fun ~offset ~len ->
      if offset + len <= String.length text then last := Some offset)
    text;
  match !last with None -> text | Some offset -> String.sub text 0 offset

let normalize_error message =
  let message = normalize_inline message in
  if has_non_white_space message then message else "session history unavailable"

let display_title summary =
  let title = Summary.display_title summary |> normalize_inline in
  if has_non_white_space title then title
  else Summary.id summary |> Id.to_string |> normalize_inline

type filter = Closed | Open of string
type rename_issue = Blank_title
type rename = { target : Id.t; text : string; issue : rename_issue option }

type mutation =
  | Pending_rename of { target : Id.t; title : string }
  | Pending_archive of Id.t
  | Pending_restore of Id.t
  | Pending_delete of Id.t

type editing =
  | Browsing
  | Renaming of rename
  | Confirming_delete of Id.t
  | Mutating of mutation

type ready = {
  rows : Summary.t list;
  filter : filter;
  selected : Id.t option;
  editing : editing;
}

type pending = { query : string; select : Id.t option }

type failure = {
  message : string;
  pending : pending option;
  retained : ready option;
}

type t = Loading of pending | Failed of failure | Ready of ready
type verb = [ `Fork | `Rename | `Archive | `Restore | `Delete ]

type msg =
  | Key of Screen.key
  | Verb of verb
  | Select_session of Id.t
  | Activate_session of Id.t

let verb v = Verb v

type event =
  | Stay
  | Close
  | Resume of Id.t
  | Fork of Id.t
  | Rename of { id : Id.t; title : string }
  | Archive of Id.t
  | Restore of Id.t
  | Delete of Id.t

let loading = Loading { query = ""; select = None }

let promoted ~filter ~select =
  Loading { query = normalize_inline filter; select }

let sorted_distinct rows =
  let rec distinct seen selected = function
    | [] -> List.rev selected
    | row :: rest ->
        let id = Summary.id row in
        if Id_set.mem id seen then distinct seen selected rest
        else distinct (Id_set.add id seen) (row :: selected) rest
  in
  rows |> List.sort Summary.compare_recency |> distinct Id_set.empty []

let filter_query = function Closed -> "" | Open query -> query

let visible (ready : ready) =
  match ready.filter with
  | Closed | Open "" -> ready.rows
  | Open query -> List.filter (Summary.matches ~query) ready.rows

let first_id = function [] -> None | row :: _ -> Some (Summary.id row)

let contains_id id rows =
  List.exists (fun row -> Id.equal id (Summary.id row)) rows

let selected_row (ready : ready) =
  match ready.selected with
  | None -> None
  | Some id ->
      List.find_opt (fun row -> Id.equal id (Summary.id row)) (visible ready)

let can_rename summary = not (Status.is_deleted (Summary.lifecycle summary))

let can_delete summary =
  (not (Status.is_deleted (Summary.lifecycle summary)))
  && Summary.Phase.equal (Summary.phase summary) Summary.Phase.Idle

let is_selected (ready : ready) id =
  match ready.selected with
  | Some selected -> Id.equal selected id
  | None -> false

let mutation_target = function
  | Pending_rename { target; _ } -> target
  | Pending_archive target | Pending_restore target | Pending_delete target ->
      target

let clear_mutation (ready : ready) =
  match ready.editing with
  | Mutating _ -> { ready with editing = Browsing }
  | Browsing | Renaming _ | Confirming_delete _ -> ready

let stabilize_edit (ready : ready) =
  match ready.editing with
  | Browsing -> Browsing
  | Renaming rename -> (
      match
        List.find_opt
          (fun row -> Id.equal rename.target (Summary.id row))
          (visible ready)
      with
      | Some row when is_selected ready rename.target && can_rename row ->
          Renaming rename
      | Some _ | None -> Browsing)
  | Confirming_delete id -> (
      match
        List.find_opt (fun row -> Id.equal id (Summary.id row)) (visible ready)
      with
      | Some row when is_selected ready id && can_delete row ->
          Confirming_delete id
      | Some _ | None -> Browsing)
  | Mutating mutation ->
      let target = mutation_target mutation in
      if is_selected ready target && contains_id target (visible ready) then
        Mutating mutation
      else Browsing

let stabilize (ready : ready) =
  let rows = visible ready in
  let selected =
    match ready.selected with
    | Some id when contains_id id rows -> Some id
    | Some _ | None -> first_id rows
  in
  let ready = { ready with selected } in
  { ready with editing = stabilize_edit ready }

let initial_ready (pending : pending) rows =
  let filter =
    if String.equal pending.query "" then Closed else Open pending.query
  in
  stabilize { rows; filter; selected = pending.select; editing = Browsing }

let retained = function
  | Loading _ -> None
  | Ready ready -> Some ready
  | Failed failure -> failure.retained

let pending = function
  | Loading pending -> Some pending
  | Ready _ -> None
  | Failed failure -> failure.pending

let loaded summaries t =
  let rows = sorted_distinct summaries in
  match retained t with
  | Some ready -> Ready (stabilize { (clear_mutation ready) with rows })
  | None ->
      let pending =
        match pending t with
        | Some pending -> pending
        | None -> { query = ""; select = None }
      in
      Ready (initial_ready pending rows)

let failed message t =
  Failed
    {
      message = normalize_error message;
      pending = pending t;
      retained = Option.map clear_mutation (retained t);
    }

let mutation_failed = failed

let key event =
  match Screen.classify event with
  | Screen.Action Screen.Other -> None
  | message -> Some (Key message)

let selected_index (ready : ready) rows =
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

let move delta (ready : ready) =
  let rows = visible ready in
  match rows with
  | [] -> { ready with selected = None; editing = Browsing }
  | _ ->
      let count = List.length rows in
      let index = selected_index ready rows in
      let index = Option_list.wrap ~count index delta in
      let selected = Option.map Summary.id (List.nth_opt rows index) in
      { ready with selected; editing = Browsing }

let narrow appended (ready : ready) =
  let appended = normalize_inline appended in
  stabilize
    {
      ready with
      filter = Open (filter_query ready.filter ^ appended);
      selected = None;
      editing = Browsing;
    }

let backspace_filter (ready : ready) =
  let query = filter_query ready.filter in
  stabilize
    {
      ready with
      filter = Open (drop_last_grapheme query);
      selected = None;
      editing = Browsing;
    }

let lifecycle_is lifecycle expected = Status.equal lifecycle expected
let can_resume summary = lifecycle_is (Summary.lifecycle summary) Status.Active

let can_fork summary =
  can_resume summary
  && Summary.Phase.equal (Summary.phase summary) Summary.Phase.Idle

let can_archive summary = can_fork summary

let can_restore summary =
  lifecycle_is (Summary.lifecycle summary) Status.Archived

let resume (ready : ready) =
  match selected_row ready with
  | Some row when can_resume row -> (ready, Resume (Summary.id row))
  | Some _ | None -> (ready, Stay)

let update_renaming (ready : ready) (rename : rename) message =
  match message with
  | Screen.Action Screen.Escape -> ({ ready with editing = Browsing }, Stay)
  | Screen.Action Screen.Enter ->
      let title = rename.text |> normalize_inline |> trim_unicode in
      if has_non_white_space title then
        ( {
            ready with
            editing =
              Mutating (Pending_rename { target = rename.target; title });
          },
          Rename { id = rename.target; title } )
      else
        ( {
            ready with
            editing = Renaming { rename with issue = Some Blank_title };
          },
          Stay )
  | Screen.Action Screen.Backspace ->
      ( {
          ready with
          editing =
            Renaming
              {
                rename with
                text = drop_last_grapheme rename.text;
                issue = None;
              };
        },
        Stay )
  | Screen.Char text ->
      ( {
          ready with
          editing =
            Renaming
              {
                rename with
                text = rename.text ^ normalize_inline text;
                issue = None;
              };
        },
        Stay )
  | Screen.Action
      ( Screen.Tab | Screen.Up | Screen.Down | Screen.Left | Screen.Right
      | Screen.Page_up | Screen.Page_down | Screen.Other ) ->
      (ready, Stay)

let update_confirmation (ready : ready) id message =
  match message with
  | Screen.Char "d" -> (
      match selected_row ready with
      | Some row when Id.equal id (Summary.id row) && can_delete row ->
          ({ ready with editing = Mutating (Pending_delete id) }, Delete id)
      | Some _ | None -> ({ ready with editing = Browsing }, Stay))
  | Screen.Char _
  | Screen.Action
      ( Screen.Enter | Screen.Escape | Screen.Tab | Screen.Up | Screen.Down
      | Screen.Left | Screen.Right | Screen.Backspace | Screen.Page_up
      | Screen.Page_down | Screen.Other ) ->
      ({ ready with editing = Browsing }, Stay)

let update_filter (ready : ready) message =
  match message with
  | Screen.Action Screen.Escape ->
      (stabilize { ready with filter = Closed; editing = Browsing }, Stay)
  | Screen.Action Screen.Enter -> resume ready
  | Screen.Action Screen.Up -> (move (-1) ready, Stay)
  | Screen.Action Screen.Down -> (move 1 ready, Stay)
  | Screen.Action (Screen.Page_up | Screen.Page_down) -> (ready, Stay)
  | Screen.Action Screen.Backspace -> (backspace_filter ready, Stay)
  | Screen.Char text -> (narrow text ready, Stay)
  | Screen.Action (Screen.Tab | Screen.Left | Screen.Right | Screen.Other) ->
      (ready, Stay)

let rename_text row =
  match Summary.title row with
  | None -> ""
  | Some title -> normalize_inline title

let update_browsing (ready : ready) message =
  match message with
  | Screen.Action Screen.Escape -> (ready, Close)
  | Screen.Action Screen.Enter -> resume ready
  | Screen.Action Screen.Up -> (move (-1) ready, Stay)
  | Screen.Action Screen.Down -> (move 1 ready, Stay)
  | Screen.Action (Screen.Page_up | Screen.Page_down) -> (ready, Stay)
  | Screen.Char "/" ->
      ({ ready with filter = Open ""; editing = Browsing }, Stay)
  (* [f r a u d] are registry verbs now, resolved through the keymap and folded
     as {!Verb} messages; every other character is inert while browsing. *)
  | Screen.Char _
  | Screen.Action
      (Screen.Tab | Screen.Left | Screen.Right | Screen.Backspace | Screen.Other)
    ->
      (ready, Stay)

(* A resolved screen verb acts on the selected row when its precondition holds,
   otherwise it is inert — exactly the browsing letters it replaced. *)
let update_verb (ready : ready) = function
  | `Fork -> (
      match selected_row ready with
      | Some row when can_fork row -> (ready, Fork (Summary.id row))
      | Some _ | None -> (ready, Stay))
  | `Rename -> (
      match selected_row ready with
      | Some row when can_rename row ->
          let id = Summary.id row in
          ( {
              ready with
              editing =
                Renaming { target = id; text = rename_text row; issue = None };
            },
            Stay )
      | Some _ | None -> (ready, Stay))
  | `Archive -> (
      match selected_row ready with
      | Some row when can_archive row ->
          let id = Summary.id row in
          ({ ready with editing = Mutating (Pending_archive id) }, Archive id)
      | Some _ | None -> (ready, Stay))
  | `Restore -> (
      match selected_row ready with
      | Some row when can_restore row ->
          let id = Summary.id row in
          ({ ready with editing = Mutating (Pending_restore id) }, Restore id)
      | Some _ | None -> (ready, Stay))
  | `Delete -> (
      match selected_row ready with
      | Some row when can_delete row ->
          ({ ready with editing = Confirming_delete (Summary.id row) }, Stay)
      | Some _ | None -> (ready, Stay))

let update_ready message (ready : ready) =
  match ready.editing with
  | Renaming rename -> update_renaming ready rename message
  | Confirming_delete id -> update_confirmation ready id message
  | Mutating _ -> (ready, Stay)
  | Browsing -> (
      match ready.filter with
      | Closed -> update_browsing ready message
      | Open _ -> update_filter ready message)

let map_retained failure ready = Failed { failure with retained = Some ready }

let update_key message = function
  | Loading _ as state -> (
      match message with
      | Screen.Action Screen.Escape -> (state, Close)
      | Screen.Char _
      | Screen.Action
          ( Screen.Enter | Screen.Tab | Screen.Up | Screen.Down | Screen.Left
          | Screen.Right | Screen.Backspace | Screen.Page_up | Screen.Page_down
          | Screen.Other ) ->
          (state, Stay))
  | Failed ({ retained = None; _ } as failure) -> (
      match message with
      | Screen.Action Screen.Escape -> (Failed failure, Close)
      | Screen.Char _
      | Screen.Action
          ( Screen.Enter | Screen.Tab | Screen.Up | Screen.Down | Screen.Left
          | Screen.Right | Screen.Backspace | Screen.Page_up | Screen.Page_down
          | Screen.Other ) ->
          (Failed failure, Stay))
  | Ready ready ->
      let ready, event = update_ready message ready in
      (Ready ready, event)
  | Failed ({ retained = Some ready; _ } as failure) ->
      let ready, event = update_ready message ready in
      (map_retained failure ready, event)

let update_verb_message verb = function
  | Ready ready ->
      let ready, event = update_verb ready verb in
      (Ready ready, event)
  | Failed ({ retained = Some ready; _ } as failure) ->
      let ready, event = update_verb ready verb in
      (map_retained failure ready, event)
  | (Loading _ | Failed { retained = None; _ }) as state -> (state, Stay)

(* The browser accepts registry verbs only while browsing with the filter
   closed; the shell resolves them there and leaves every other state to the
   screen's own vocabulary. *)
let browsing = function
  | Ready { editing = Browsing; filter = Closed; _ }
  | Failed { retained = Some { editing = Browsing; filter = Closed; _ }; _ } ->
      true
  | Ready _ | Loading _ | Failed _ -> false

let select_session id (ready : ready) =
  if contains_id id (visible ready) then
    { ready with selected = Some id; editing = Browsing }
  else ready

let update_selection ~activate id = function
  | Loading _ as state -> (state, Stay)
  | Failed ({ retained = None; _ } as failure) -> (Failed failure, Stay)
  | Ready ({ editing = Mutating _; _ } as ready) -> (Ready ready, Stay)
  | Ready ready ->
      let ready = select_session id ready in
      if activate then
        let ready, event = resume ready in
        (Ready ready, event)
      else (Ready ready, Stay)
  | Failed
      ({ retained = Some ({ editing = Mutating _; _ } as ready); _ } as failure)
    ->
      (map_retained failure ready, Stay)
  | Failed ({ retained = Some ready; _ } as failure) ->
      let ready = select_session id ready in
      if activate then
        let ready, event = resume ready in
        (map_retained failure ready, event)
      else (map_retained failure ready, Stay)

let update message state =
  match message with
  | Key key -> update_key key state
  | Verb verb -> update_verb_message verb state
  | Select_session id -> update_selection ~activate:false id state
  | Activate_session id -> update_selection ~activate:true id state

(* Rendering. *)

let relative_seconds ~now time =
  let now_ms = Time.to_unix_ms now in
  let time_ms = Time.to_unix_ms time in
  if Int64.compare time_ms now_ms >= 0 then 0L
  else Int64.div (Int64.sub now_ms time_ms) 1_000L

let relative_age ~now updated_at =
  let seconds = relative_seconds ~now updated_at in
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

type group = Today | This_week | Older

let group ~now summary =
  let seconds = relative_seconds ~now (Summary.updated_at summary) in
  if Int64.compare seconds 86_400L < 0 then Today
  else if Int64.compare seconds 604_800L < 0 then This_week
  else Older

let group_name = function
  | Today -> "today"
  | This_week -> "this week"
  | Older -> "older"

let phase_style ~palette = function
  | Summary.Phase.Idle -> Theme.Palette.muted_style palette
  | Summary.Phase.Working -> Theme.Palette.running_style palette
  | Summary.Phase.Waiting -> Theme.Palette.warning_style palette

let plural count singular = if count = 1 then singular else singular ^ "s"
let zero_size = { width = px 0; height = px 0 }
let full_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = pct 100 }

let prose ?key ?(style = default_style) ?(wrap = `Word) value =
  text ?key ~style ~wrap ~truncate:false ~selectable:true ~flex_shrink:0.
    ~min_size:zero_size ~size:full_width value

let content_column ?key children =
  box ?key ~flex_direction:Flex_direction.Column
    ~gap:{ width = px 0; height = px 1 }
    ~padding:(padding_lrtb 2 2 0 0) ~flex_grow:1. ~flex_shrink:1.
    ~min_size:zero_size ~size:full_width children

let scrollable ?(sticky_scroll = false) ?(flex_grow = 1.) ~key children =
  scroll_box ~key ~scroll_x:false ~scroll_y:true ~sticky_scroll
    ~sticky_start:`Bottom ~show_scrollbars:true ~focusable:false ~flex_grow
    ~flex_shrink:1. ~min_size:zero_size ~size:fill_remaining
    [ content_column children ]

let table_cell ?style value = Table.cell ?style (normalize_inline value)

let session_columns =
  [
    Table.column ~width:(`Fixed 2) ~overflow:`Crop "";
    Table.column ~width:(`Flex 4.) ~overflow:`Ellipsis "session";
    Table.column ~width:`Auto ~overflow:`Crop "lifecycle";
    Table.column ~width:`Auto ~overflow:`Crop "phase";
    Table.column ~width:`Auto ~alignment:`Right ~overflow:`Crop "turns";
    Table.column ~width:`Auto ~overflow:`Crop "updated";
    Table.column ~width:`Auto ~overflow:`Crop "recency";
  ]

let row_identity (ready : ready) summary =
  let id = Summary.id summary in
  match ready.editing with
  | Renaming rename when Id.equal rename.target id ->
      "rename: " ^ normalize_inline rename.text ^ "▏"
  | Confirming_delete target when Id.equal target id -> "DELETE? press d again"
  | Mutating (Pending_rename { target; title }) when Id.equal target id ->
      "saving: " ^ normalize_inline title
  | Mutating (Pending_archive target) when Id.equal target id -> "archiving…"
  | Mutating (Pending_restore target) when Id.equal target id -> "restoring…"
  | Mutating (Pending_delete target) when Id.equal target id -> "deleting…"
  | Browsing | Renaming _ | Confirming_delete _ | Mutating _ ->
      display_title summary

let session_table_row ~palette ~now (ready : ready) summary =
  let selected = is_selected ready (Summary.id summary) in
  let identity_style =
    match ready.editing with
    | Renaming rename
      when selected && Id.equal rename.target (Summary.id summary) ->
        Theme.Palette.accent_style palette
    | Confirming_delete target
      when selected && Id.equal target (Summary.id summary) ->
        Theme.Palette.error_style palette
    | Mutating mutation
      when selected && Id.equal (mutation_target mutation) (Summary.id summary)
      ->
        Theme.Palette.warning_style palette
    | Browsing | Renaming _ | Confirming_delete _ | Mutating _ -> default_style
  in
  let phase = Summary.phase summary in
  let turns = Summary.turns summary in
  [|
    table_cell
      ~style:(Theme.Palette.accent_style palette)
      (if selected then "❯" else "");
    table_cell ~style:identity_style (row_identity ready summary);
    table_cell
      ~style:(Theme.Palette.muted_style palette)
      (Status.to_string (Summary.lifecycle summary));
    table_cell
      ~style:(phase_style ~palette phase)
      (Summary.Phase.to_string phase);
    table_cell
      ~style:(Theme.Palette.muted_style palette)
      (Printf.sprintf "%d %s" turns (plural turns "turn"));
    table_cell
      ~style:(Theme.Palette.muted_style palette)
      (relative_age ~now (Summary.updated_at summary));
    table_cell
      ~style:(Theme.Palette.muted_style palette)
      (group_name (group ~now summary));
  |]

let table_focusable (ready : ready) =
  match (ready.editing, ready.filter) with
  | Browsing, (Closed | Open _) -> true
  | Renaming _, _ | Confirming_delete _, _ | Mutating _, _ -> false

let message_for_row constructor rows index =
  Option.map
    (fun summary -> constructor (Summary.id summary))
    (List.nth_opt rows index)

let results_table ~palette ~now (ready : ready) rows =
  let focusable = table_focusable ready in
  let rendered_rows = List.map (session_table_row ~palette ~now ready) rows in
  table ~key:"sessions.results" ~columns:session_columns ~rows:rendered_rows
    ~selected_row:(selected_index ready rows)
    ~border:false ~show_header:true ~show_column_separator:false
    ~show_row_separator:false ~cell_padding:0
    ~selected_text_color:(Theme.Palette.selection_fg palette)
    ~selected_background:(Theme.Palette.selection_bg palette)
    ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
    ~focused_selected_background:(Theme.Palette.selection_bg palette)
    ~wrap_selection:true ~focusable ~autofocus:focusable ~flex_grow:2.
    ~flex_shrink:1. ~min_size:zero_size ~size:fill_remaining
    ~activate_on_click:true
    ~on_change:(message_for_row (fun id -> Select_session id) rows)
    ~on_activate:(message_for_row (fun id -> Activate_session id) rows)
    ()

let preview_detail ~palette summary =
  match Summary.preview summary with
  | None -> []
  | Some preview ->
      let preview = normalize_inline preview in
      if has_non_white_space preview then
        [ prose ~style:(Theme.Palette.faint_style palette) preview ]
      else []

let cwd_detail ~palette ~home summary =
  let cwd =
    Summary.cwd summary |> Path_display.home_relative ~home |> normalize_inline
  in
  let cwd = if has_non_white_space cwd then cwd else "[workspace]" in
  prose ~style:(Theme.Palette.muted_style palette) ("cwd " ^ cwd)

let lineage_detail ~palette summary =
  match Summary.forked_from summary with
  | None -> []
  | Some lineage ->
      let parent =
        lineage |> Forked_from.parent |> Id.to_string |> normalize_inline
      in
      let copied = Forked_from.copied_events lineage in
      [
        prose
          ~style:(Theme.Palette.muted_style palette)
          (Printf.sprintf "fork of %s%s%d copied %s" parent Theme.separator
             copied (plural copied "event"));
      ]

let browsing_detail ~palette ~home summary =
  preview_detail ~palette summary
  @ [ cwd_detail ~palette ~home summary ]
  @ lineage_detail ~palette summary

let rename_detail ~palette (rename : rename) =
  [
    prose
      ~style:(Theme.Palette.accent_style palette)
      ~wrap:`Char
      ("rename: " ^ normalize_inline rename.text ^ "▏");
  ]
  @
  match rename.issue with
  | None -> []
  | Some Blank_title ->
      [
        prose
          ~style:(Theme.Palette.warning_style palette)
          "Title must contain a non-whitespace character.";
      ]

let delete_detail ~palette summary =
  [
    prose
      ~style:(Theme.Palette.error_style palette)
      (Printf.sprintf "Permanently delete “%s”?" (display_title summary));
    prose
      ~style:(Theme.Palette.warning_style palette)
      "Press d again to DELETE. Any other key cancels.";
  ]

let mutation_detail summary = function
  | Pending_rename { title; _ } ->
      Printf.sprintf "⠋ saving “%s”…" (normalize_inline title)
  | Pending_archive _ ->
      Printf.sprintf "⠋ archiving “%s”…" (display_title summary)
  | Pending_restore _ ->
      Printf.sprintf "⠋ restoring “%s”…" (display_title summary)
  | Pending_delete _ ->
      Printf.sprintf "⠋ permanently deleting “%s”…" (display_title summary)

let selected_detail ~palette ~home (ready : ready) =
  match selected_row ready with
  | None -> None
  | Some summary ->
      let lines, sticky_scroll =
        match ready.editing with
        | Renaming rename -> (rename_detail ~palette rename, true)
        | Confirming_delete _ -> (delete_detail ~palette summary, false)
        | Mutating mutation ->
            ( [
                prose
                  ~style:(Theme.Palette.warning_style palette)
                  (mutation_detail summary mutation);
              ],
              false )
        | Browsing -> (browsing_detail ~palette ~home summary, false)
      in
      Some
        (scrollable ~key:"sessions.detail" ~sticky_scroll ~flex_grow:1. lines)

let status_view ~key ~style message = scrollable ~key [ prose ~style message ]

let retained_error ~palette message =
  scroll_box ~key:"sessions.refresh-error" ~scroll_x:false ~scroll_y:true
    ~show_scrollbars:true ~focusable:false ~flex_grow:0. ~flex_shrink:1.
    ~min_size:zero_size ~size:full_width
    [
      content_column
        [
          prose
            ~style:(Theme.Palette.error_style palette)
            (Theme.problem ^ message);
        ];
    ]

let ready_content ~palette ~home ~now ?error (ready : ready) =
  let diagnostics =
    Option.to_list (Option.map (retained_error ~palette) error)
  in
  let result =
    match (ready.rows, visible ready) with
    | [], _ ->
        status_view ~key:"sessions.empty"
          ~style:(Theme.Palette.muted_style palette)
          "No sessions in this workspace."
    | _ :: _, [] ->
        status_view ~key:"sessions.no-match"
          ~style:(Theme.Palette.muted_style palette)
          "No matching sessions."
    | _ :: _, rows -> results_table ~palette ~now ready rows
  in
  box ~key:"sessions.content" ~flex_direction:Flex_direction.Column
    ~gap:{ width = px 0; height = px 1 }
    ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size ~size:fill_remaining
    (diagnostics
    @ (result :: Option.to_list (selected_detail ~palette ~home ready)))

let content ~palette ~home ~now = function
  | Loading _ ->
      status_view ~key:"sessions.loading"
        ~style:(Theme.Palette.muted_style palette)
        "⠋ loading sessions…"
  | Failed { retained = None; message; _ } ->
      status_view ~key:"sessions.failure"
        ~style:(Theme.Palette.error_style palette)
        (Theme.problem ^ message)
  | Ready ready -> ready_content ~palette ~home ~now ready
  | Failed { retained = Some ready; message; _ } ->
      ready_content ~palette ~home ~now ~error:message ready

let filter_line = function
  | Loading _ | Failed { retained = None; _ } -> None
  | Ready ready | Failed { retained = Some ready; _ } -> (
      match ready.filter with
      | Closed -> None
      | Open query ->
          Some
            {
              Screen.query = normalize_inline query;
              matches = List.length (visible ready);
            })

let session_count = function
  | Loading _ | Failed { retained = None; _ } -> None
  | Ready ready | Failed { retained = Some ready; _ } ->
      Some (List.length ready.rows)

let selected_capabilities (ready : ready) =
  match selected_row ready with
  | None -> []
  | Some row ->
      (if can_resume row then [ "↵ resume" ] else [])
      @ (if can_fork row then [ "f fork" ] else [])
      @ (if can_rename row then [ "r rename" ] else [])
      @ (if can_archive row then [ "a archive" ] else [])
      @ (if can_restore row then [ "u restore" ] else [])
      @ if can_delete row then [ "d delete" ] else []

let hints = function
  | Loading _ | Failed { retained = None; _ } -> [ "esc back" ]
  | Ready ready | Failed { retained = Some ready; _ } -> (
      match ready.editing with
      | Renaming _ -> [ "↵ save"; "esc cancel" ]
      | Confirming_delete _ -> [ "d DELETE"; "any key cancel" ]
      | Mutating _ -> [ "updating session…" ]
      | Browsing -> (
          match ready.filter with
          | Open _ ->
              (match selected_row ready with
                | Some row when can_resume row -> [ "↵ resume" ]
                | Some _ | None -> [])
              @ [ "↑↓ select"; "esc clear filter" ]
          | Closed -> selected_capabilities ready @ [ "/ filter"; "esc back" ]))

let fact t =
  match session_count t with
  | None -> ""
  | Some count -> Printf.sprintf "%d %s" count (plural count "session")

let view ~palette ~home ~now ~frame t =
  Screen.view ~palette ~frame ~name:"sessions" ~fact:(fact t)
    ~filter:(filter_line t) ~hint:(hints t)
    ~content:(content ~palette ~home ~now t)
