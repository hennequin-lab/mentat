(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

module Input_mode = struct
  type t = Plain | Shell | History_search
end

module Mode = Mentat_session.Contract.Mode

type draft_mode = Prompt | Shell_command

(* The history walk is a zipper. [initial_draft] and [initial_mode] are the
   exact editable state that paging forward beyond the newest entry restores. *)
type history_cursor = {
  older : Draft.History_entry.t list;
  newer : Draft.History_entry.t list;
  initial_draft : Draft.t;
  initial_mode : draft_mode;
}

type t = {
  draft : Draft.t;
  mode : draft_mode;
  history : Draft.History_entry.t list;
  history_cursor : history_cursor option;
  searching : bool;
  shell_enabled : bool;
  widget_desync : bool;
  (* A controlled value push does not cancel an in-flight Mosaic cursor report.
     While this is set, the first such report refers to the previous buffer and
     is discarded. *)
  shell_trigger_pending : bool;
      (* Mosaic may batch another input callback before the controlled textarea has
     rendered the consumed [!] away. Such a callback still includes that one
     widget-only byte and must consume it again. *)
}

let shell_history_entry entry =
  entry |> Draft.of_history_entry |> Draft.with_cursor 0
  |> Draft.insert_text "!" |> Draft.history_entry

(* A shell-history marker inserted by [shell_history_entry] is a plain run. Do
   not reinterpret the first byte of a structured atom as composer syntax. *)
let has_plain_shell_trigger draft =
  String.starts_with ~prefix:"!" (Draft.text draft)
  &&
  match Draft.runs draft with
  | (span, kind) :: _ ->
      Draft.Span.first span = 0
      && Draft.Span.last span >= 1
      && kind = Draft.Plain
  | [] -> false

let consume_shell_trigger draft =
  Draft.replace_range (Draft.Span.make ~first:0 ~last:1) "" draft

let classify_draft ~shell_enabled draft =
  if shell_enabled && has_plain_shell_trigger draft then
    (Shell_command, consume_shell_trigger draft)
  else (Prompt, draft)

let mode_and_draft_of_history_entry ~shell_enabled entry =
  Draft.of_history_entry entry |> classify_draft ~shell_enabled

let init ~shell_enabled ?draft () =
  let draft =
    match draft with Some text -> Draft.of_text text | None -> Draft.empty
  in
  let mode, draft = classify_draft ~shell_enabled draft in
  {
    draft;
    mode;
    history = [];
    history_cursor = None;
    searching = false;
    shell_enabled;
    widget_desync = false;
    shell_trigger_pending = false;
  }

let draft t = t.draft
let draft_text t = Draft.text t.draft
let is_blank t = Draft.is_blank t.draft

let history_entry t =
  let entry = Draft.history_entry t.draft in
  match t.mode with
  | Prompt -> entry
  | Shell_command -> shell_history_entry entry

let input_mode t =
  if t.searching then Input_mode.History_search
  else
    match t.mode with
    | Prompt -> Input_mode.Plain
    | Shell_command -> Input_mode.Shell

let active_file_ref_token t =
  Draft.active_file_ref_token_span t.draft
  |> Option.map (fun span ->
      String.sub (Draft.text t.draft) (Draft.Span.first span)
        (Draft.Span.length span))

(* Mosaic cursor offsets count extended grapheme clusters; Draft offsets count
   UTF-8 bytes. Both directions use Matrix's segmentation, clamp out-of-range
   widget values, and operate only over Draft's normalized text. *)
let byte_of_grapheme text grapheme =
  if grapheme <= 0 then 0
  else begin
    let remaining = ref grapheme in
    let result = ref 0 in
    (try
       Matrix.Text.iter_graphemes
         (fun ~offset ~len ->
           if !remaining = 0 then raise Exit;
           result := offset + len;
           decr remaining)
         text
     with Exit -> ());
    !result
  end

let grapheme_of_byte text byte =
  let byte = max 0 (min byte (String.length text)) in
  let count = ref 0 in
  (try
     Matrix.Text.iter_graphemes
       (fun ~offset ~len ->
         if offset >= byte then raise Exit;
         incr count;
         if offset + len >= byte then raise Exit)
       text
   with Exit -> ());
  !count

let desync_after draft t =
  t.widget_desync || not (String.equal (Draft.text draft) (Draft.text t.draft))

(* [reported] is the widget's complete buffer. Draft infers its one contiguous
   edit so untouched atoms survive. Shell classification happens only after
   Draft has repaired malformed UTF-8 and reconciled atomic ranges. *)
let with_reported reported t =
  let updated = Draft.replace_visible_text reported t.draft in
  match t.mode with
  | Prompt when t.shell_enabled && has_plain_shell_trigger updated ->
      let draft = consume_shell_trigger updated in
      {
        t with
        draft;
        mode = Shell_command;
        widget_desync = true;
        shell_trigger_pending = true;
        history_cursor = None;
      }
  | Shell_command
    when t.shell_trigger_pending && has_plain_shell_trigger updated ->
      let draft = consume_shell_trigger updated in
      { t with draft; widget_desync = true; history_cursor = None }
  | Prompt | Shell_command ->
      {
        t with
        draft = updated;
        widget_desync = not (String.equal (Draft.text updated) reported);
        shell_trigger_pending = false;
        history_cursor = None;
      }

(* Paste, completion, history, and clearing update the model before Mosaic's
   controlled value catches up. *)
let with_draft draft t =
  { t with widget_desync = desync_after draft t; draft; history_cursor = None }

let with_paste text t =
  let draft = Draft.insert_paste text t.draft in
  if t.mode = Prompt && t.shell_enabled && has_plain_shell_trigger draft then
    let draft = consume_shell_trigger draft in
    with_draft draft
      { t with mode = Shell_command; shell_trigger_pending = false }
  else with_draft draft t

let cursor_moved grapheme t =
  if t.widget_desync then
    (* A terminal read may contain several edits. Their input and cursor
       callbacks are folded before the controlled value renders, so an
       intermediate cursor still belongs to the widget-only buffer. Only the
       controlled Draft caret acknowledges reconciliation; clearing on the
       first stale cursor would lose the pending shell trigger before a
       cumulative [!command] report can consume it. *)
    let controlled =
      grapheme_of_byte (Draft.text t.draft) (Draft.cursor t.draft)
    in
    if grapheme = controlled then
      { t with widget_desync = false; shell_trigger_pending = false }
    else t
  else
    let byte = byte_of_grapheme (Draft.text t.draft) grapheme in
    if byte = Draft.cursor t.draft then t
    else { t with draft = Draft.with_cursor byte t.draft }

let prepend_history entry entries =
  entry
  :: List.filter
       (fun other -> not (Draft.History_entry.equal entry other))
       entries

let append_unique entries additions =
  let module Seen = Hashtbl.Make (struct
    type t = Draft.History_entry.t

    let equal = Draft.History_entry.equal

    (* Exact equality includes all structured metadata and implies equal visible
       text. Text is therefore a valid hash even though unequal structured
       entries may intentionally collide. *)
    let hash entry = Hashtbl.hash (Draft.History_entry.text entry)
  end) in
  let seen = Seen.create (List.length entries + List.length additions) in
  List.iter (fun entry -> Seen.replace seen entry ()) entries;
  let rec collect accepted = function
    | [] -> entries @ List.rev accepted
    | entry :: remaining ->
        if Seen.mem seen entry then collect accepted remaining
        else begin
          Seen.add seen entry ();
          collect (entry :: accepted) remaining
        end
  in
  collect [] additions

let remember entry t =
  { t with history = prepend_history entry t.history; history_cursor = None }

let with_history entries t =
  { t with history = append_unique t.history entries; history_cursor = None }

let has_following_separator draft =
  let text = Draft.text draft in
  let cursor = Draft.cursor draft in
  cursor < String.length text
  &&
  match String.get text cursor with
  | ' ' | '\n' | '\r' | '\t' | '\011' | '\012' -> true
  | _ -> false

let quoted_file_ref path =
  let buffer = Buffer.create (String.length path + 3) in
  Buffer.add_string buffer "@\"";
  String.iter
    (function
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '"' -> Buffer.add_string buffer "\\\""
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | byte -> Buffer.add_char buffer byte)
    path;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let file_ref_label path =
  if String.exists Char.Ascii.is_white path || String.contains path '"' then
    quoted_file_ref path
  else "@" ^ path

let complete_file_ref path draft =
  let draft =
    Draft.replace_active_file_ref_token ~label:(file_ref_label path) ~path draft
  in
  if has_following_separator draft then draft else Draft.insert_text " " draft

let clear_file_ref_token draft =
  match Draft.active_file_ref_token_span draft with
  | Some span -> Draft.replace_range span "" draft
  | None -> draft

let set_file_ref_token text draft =
  match Draft.active_file_ref_token_span draft with
  | Some span -> Draft.replace_range span ("@" ^ text) draft
  | None -> draft

let previous_history t =
  match (t.history_cursor, t.history) with
  | None, [] -> t
  | None, entry :: older ->
      let mode, draft =
        mode_and_draft_of_history_entry ~shell_enabled:t.shell_enabled entry
      in
      {
        t with
        draft;
        mode;
        widget_desync = desync_after draft t;
        shell_trigger_pending = false;
        history_cursor =
          Some
            {
              older;
              newer = [];
              initial_draft = t.draft;
              initial_mode = t.mode;
            };
      }
  | Some { older = []; _ }, _ -> t
  | Some ({ older = entry :: older; _ } as cursor), _ ->
      let mode, draft =
        mode_and_draft_of_history_entry ~shell_enabled:t.shell_enabled entry
      in
      {
        t with
        draft;
        mode;
        widget_desync = desync_after draft t;
        shell_trigger_pending = false;
        history_cursor =
          Some { cursor with older; newer = history_entry t :: cursor.newer };
      }

let next_history t =
  match t.history_cursor with
  | None -> t
  | Some { newer = []; initial_draft; initial_mode; _ } ->
      with_draft initial_draft
        { t with mode = initial_mode; shell_trigger_pending = false }
  | Some ({ newer = entry :: newer; _ } as cursor) ->
      let mode, draft =
        mode_and_draft_of_history_entry ~shell_enabled:t.shell_enabled entry
      in
      {
        t with
        draft;
        mode;
        widget_desync = desync_after draft t;
        shell_trigger_pending = false;
        history_cursor =
          Some { cursor with older = history_entry t :: cursor.older; newer };
      }

type msg =
  | Edited of string
  | Cursor_moved of int
  | Paste of string
  | Submit of string
  | Help_key
  | Complete_file_ref of string
  | Set_file_ref_token of string
  | Clear_file_ref_token
  | Insert_image of Draft.Image_ref.t
  | Restore_history of Draft.History_entry.t
  | History_previous
  | History_next
  | Begin_history_search
  | End_history_search
  | Clear_to_history
  | Exit_shell
  | List_key of [ `Up | `Down | `Tab ]

type event =
  | Submitted of {
      text : string;
      media : Mentat_llm.Content.t list;
      entry : Draft.History_entry.t;
    }
  | Shell_submitted of { command : string; entry : Draft.History_entry.t }
  | Blank_submitted
  | Draft_discarded of Draft.History_entry.t
  | Help_requested

let update ?(submit_enabled = true) msg t =
  match msg with
  | Edited reported -> (with_reported reported t, [])
  | Cursor_moved grapheme -> (cursor_moved grapheme t, [])
  | Paste text -> (with_paste text t, [])
  | Complete_file_ref path -> (with_draft (complete_file_ref path t.draft) t, [])
  | Set_file_ref_token text ->
      (with_draft (set_file_ref_token text t.draft) t, [])
  | Clear_file_ref_token -> (with_draft (clear_file_ref_token t.draft) t, [])
  | Insert_image image -> (with_draft (Draft.insert_image image t.draft) t, [])
  | Restore_history entry ->
      let mode, draft =
        mode_and_draft_of_history_entry ~shell_enabled:t.shell_enabled entry
      in
      (with_draft draft { t with mode; shell_trigger_pending = false }, [])
  | History_previous -> (previous_history t, [])
  | History_next -> (next_history t, [])
  | Begin_history_search -> ({ t with searching = true }, [])
  | End_history_search -> ({ t with searching = false }, [])
  | Help_key ->
      if t.mode = Prompt && (not t.searching) && Draft.is_blank t.draft then
        (t, [ Help_requested ])
      else (t, [])
  | List_key _ -> (t, [])
  | Exit_shell -> (
      match t.mode with
      | Shell_command when Draft.is_blank t.draft ->
          ( with_draft Draft.empty
              {
                t with
                mode = Prompt;
                searching = false;
                shell_trigger_pending = false;
              },
            [] )
      | Prompt | Shell_command -> (t, []))
  | Clear_to_history ->
      if Draft.is_blank t.draft then (t, [])
      else
        let entry = history_entry t in
        let t = remember entry (with_draft Draft.empty t) in
        ( {
            t with
            mode = Prompt;
            searching = false;
            shell_trigger_pending = false;
          },
          [ Draft_discarded entry ] )
  | Submit value -> (
      if not submit_enabled then (t, [])
      else
        (* Reconcile the supplied full widget value before classifying the
           submission. This covers a final input callback batched with Enter,
           including a just-typed shell trigger. *)
        let t = with_reported value t in
        match Draft.submit t.draft with
        | None ->
            if t.mode = Shell_command then (t, [])
            else (with_draft Draft.empty t, [ Blank_submitted ])
        | Some (submitted, cleared) ->
            let entry =
              match t.mode with
              | Prompt -> submitted.Draft.submitted_history_entry
              | Shell_command ->
                  shell_history_entry submitted.Draft.submitted_history_entry
            in
            let event =
              match t.mode with
              | Prompt ->
                  Submitted
                    {
                      text = submitted.Draft.submitted_text;
                      media = submitted.Draft.submitted_media;
                      entry;
                    }
              | Shell_command ->
                  Shell_submitted
                    { command = submitted.Draft.submitted_text; entry }
            in
            ( {
                draft = cleared;
                mode = Prompt;
                history = prepend_history entry t.history;
                history_cursor = None;
                searching = false;
                shell_enabled = t.shell_enabled;
                widget_desync =
                  not (String.equal (Draft.text cleared) (Draft.text t.draft));
                shell_trigger_pending = false;
              },
              [ event ] ))

(* Styled runs concatenate to exactly the controlled value, as Mosaic's spans
   contract requires. *)
let spans_of_draft ~palette draft =
  let text = Draft.text draft in
  List.map
    (fun (span, kind) ->
      let first = Draft.Span.first span in
      let piece = String.sub text first (Draft.Span.length span) in
      let style =
        match kind with
        | Draft.Plain -> Ansi.Style.default
        | Draft.Atom -> Theme.Palette.atom_style palette
      in
      { Mosaic.text = piece; style })
    (Draft.runs draft)

let max_input_rows = 6

let submit_key_bindings =
  let binding = Mosaic_ui.Textarea.key_binding in
  [
    binding "return" Mosaic_ui.Textarea.Submit;
    binding "enter" Mosaic_ui.Textarea.Submit;
    binding "linefeed" Mosaic_ui.Textarea.Newline;
    binding ~shift:true "return" Mosaic_ui.Textarea.Newline;
    binding ~shift:true "linefeed" Mosaic_ui.Textarea.Newline;
    binding ~shift:true "enter" Mosaic_ui.Textarea.Newline;
  ]

let normalize_single_line text =
  let text = Draft.of_text text |> Draft.text in
  String.map
    (function '\n' | '\r' | '\t' | '\011' | '\012' -> ' ' | byte -> byte)
    text

let normalize_agent = function
  | None -> None
  | Some agent ->
      let agent = normalize_single_line agent |> String.trim in
      if String.equal agent "" then None else Some agent

let frame_rule_color ~palette ~mode ~agent =
  match mode with
  | Mode.Plan -> Theme.Palette.mode_plan palette
  | Mode.Review -> Theme.Palette.mode_review palette
  | Mode.Build -> (
      match agent with
      | Some _ -> Theme.Palette.accent palette
      | None -> Theme.Palette.rule palette)

let frame_marker_color ~palette = function
  | Mode.Build -> Theme.Palette.accent palette
  | Mode.Plan -> Theme.Palette.mode_plan palette
  | Mode.Review -> Theme.Palette.mode_review palette

let mode_tag = function
  | Mode.Build -> "build"
  | Mode.Plan -> "plan"
  | Mode.Review -> "review"

let mode_label = function
  | Mode.Build -> None
  | Mode.Plan -> Some (Theme.mode_plan ^ " plan")
  | Mode.Review -> Some (Theme.mode_review ^ " review")

let hidden_overflow = { x = Overflow.Hidden; y = Overflow.Hidden }

(* A border-box with only its top side is an intrinsic horizontal rule. In the
   top row it flexes between the two intrinsic chips; as the bottom row it owns
   the parent's full width. No terminal-column measurement crosses this module
   boundary. *)
let border_rule ~key ~grow ~rule_color =
  box ~key ~box_sizing:Box_sizing.Border_box ~overflow:hidden_overflow
    ~flex_grow:(if grow then 1. else 0.)
    ~flex_shrink:1. ~border:true ~border_style:Border.single
    ~border_sides:[ `Top ] ~border_color:rule_color ~fill:false
    ~size:{ width = (if grow then auto else pct 100); height = px 1 }
    []

let top_rule ~palette ~rule_color ~mode ~agent =
  let mode_label = mode_label mode in
  let agent_label = Option.map (fun name -> "@" ^ name) agent in
  let key =
    "composer.top_rule." ^ mode_tag mode
    ^ if Option.is_some agent then ".agent" else ".root"
  in
  let mode_chip =
    match mode_label with
    | Some label -> [ Theme.chip ~palette ~color:rule_color label ]
    | None -> []
  in
  let agent_chip =
    match agent_label with
    | Some label -> [ Theme.chip ~palette ~color:rule_color label ]
    | None -> []
  in
  box ~key ~flex_direction:Flex_direction.Row ~box_sizing:Box_sizing.Border_box
    ~overflow:hidden_overflow ~flex_shrink:0.
    ~size:{ width = pct 100; height = px 1 }
    (mode_chip
    @ [ border_rule ~key:"composer.top_rule.fill" ~grow:true ~rule_color ]
    @ agent_chip)

let bottom_rule ~rule_color =
  border_rule ~key:"composer.bottom_rule" ~grow:false ~rule_color

let marker_of ~palette ~mode = function
  | Input_mode.Plain ->
      ( Theme.cursor,
        Ansi.Style.make ~fg:(frame_marker_color ~palette mode) ~bold:true () )
  | Input_mode.Shell ->
      (Theme.shell_marker ^ " ", Theme.Palette.warning_style palette)
  | Input_mode.History_search ->
      ( Theme.history_marker ^ " ",
        Ansi.Style.make ~fg:(Theme.Palette.history palette) () )

let placeholder_for ~agent ~turn_running = function
  | Input_mode.History_search -> "search history"
  | Input_mode.Shell -> "shell command"
  | Input_mode.Plain -> (
      match agent with
      | Some agent -> "message @" ^ agent
      | None ->
          if turn_running then "queue a message — sends after this turn"
          else "message mentat")

let render ?(submit_enabled = true) ?(list_open = false) ?(mode = Mode.Build)
    ?agent ?(turn_running = false) ?(top_margin = 1) ?inspect_latest_change
    ~palette ~on_msg t =
  if top_margin < 0 then
    invalid_arg "Mentat_tui.Composer.render: top_margin must be non-negative";
  let agent = normalize_agent agent in
  let stored_value = Draft.text t.draft in
  let input = input_mode t in
  let marker_glyph, marker_style = marker_of ~palette ~mode input in
  let rule_color = frame_rule_color ~palette ~mode ~agent in
  let textarea_placeholder = placeholder_for ~agent ~turn_running input in
  let single_line =
    not (String.contains stored_value '\n' || String.contains stored_value '\r')
  in
  (* This widget-local hook runs before Textarea's defaults. The application
     subscription is too late to suppress insertion or cursor movement. *)
  let on_key ev =
    let data = Event.Key.data ev in
    let modifier = data.Matrix.Input.Key.modifier in
    let plain_modifiers =
      (not modifier.Matrix.Input.Modifier.ctrl)
      && (not modifier.Matrix.Input.Modifier.alt)
      && not modifier.Matrix.Input.Modifier.super
    in
    let ctrl_char char =
      modifier.Matrix.Input.Modifier.ctrl
      &&
      match data.Matrix.Input.Key.key with
      | Matrix.Input.Key.Char scalar -> Uchar.equal scalar (Uchar.of_char char)
      | _ -> false
    in
    let list_key key =
      Event.Key.prevent_default ev;
      Some (on_msg (List_key key))
    in
    match data.Matrix.Input.Key.key with
    | Matrix.Input.Key.Char scalar
      when input = Input_mode.Plain && Draft.is_blank t.draft && plain_modifiers
           && Uchar.equal scalar (Uchar.of_char '?') ->
        Event.Key.prevent_default ev;
        Some (on_msg Help_key)
    | Matrix.Input.Key.Up when plain_modifiers && (list_open || single_line) ->
        list_key `Up
    | Matrix.Input.Key.Down when plain_modifiers && (list_open || single_line)
      ->
        list_key `Down
    | Matrix.Input.Key.Tab when list_open && plain_modifiers -> list_key `Tab
    | Matrix.Input.Key.Tab
      when single_line && plain_modifiers
           && not modifier.Matrix.Input.Modifier.shift ->
        list_key `Tab
    | _ when list_open && ctrl_char 'p' -> list_key `Up
    | _ when list_open && ctrl_char 'n' -> list_key `Down
    | _ when (not list_open) && ctrl_char 'd' -> (
        match inspect_latest_change with
        | None -> None
        | Some message ->
            Event.Key.prevent_default ev;
            Some message)
    | _ -> None
  in
  let on_submit =
    if submit_enabled then fun value -> Some (on_msg (Submit value))
    else Fun.const None
  in
  box ~key:"composer" ~flex_direction:Flex_direction.Column ~flex_shrink:0.
    ~margin:(margin_lrtb 0 0 top_margin 0)
    ~size:{ width = pct 100; height = auto }
    [
      top_rule ~palette ~rule_color ~mode ~agent;
      box ~key:"composer.input" ~flex_direction:Flex_direction.Row
        ~size:{ width = pct 100; height = auto }
        [
          text ~style:marker_style ~wrap:`None ~flex_shrink:0.
            ~align_self:Align.Flex_start marker_glyph;
          (* Input mode changes marker and placeholder semantics, not editor
             identity. Mosaic reconciles the controlled value and cursor; a
             remount would only discard its selection and scroll viewport. *)
          textarea ~key:"composer.textarea" ~id:"mentat-tui-composer"
            ~autofocus:true ~value:stored_value ~on_key
            ~cursor:(grapheme_of_byte stored_value (Draft.cursor t.draft))
            ~spans:(spans_of_draft ~palette t.draft)
            ~placeholder:textarea_placeholder
            ~placeholder_color:(Theme.Palette.faint palette)
            ~wrap:`Word ~key_bindings:submit_key_bindings ~flex_grow:1.
            ~size:{ width = auto; height = auto }
            ~min_size:{ width = auto; height = px 1 }
            ~max_size:{ width = auto; height = px max_input_rows }
            ~on_input:(fun value -> Some (on_msg (Edited value)))
              (* Draft owns one caret, not a selection range. While Mosaic is
               extending a selection, keep Draft's last caret stable; the
               subsequent full-value [Edited] callback reconciles the selected
               replacement atomically. *)
            ~on_cursor:(fun ~cursor ~selection ->
              if Option.is_none selection then
                Some (on_msg (Cursor_moved cursor))
              else None)
            ~on_paste:(fun ev ->
              Event.Paste.prevent_default ev;
              Some (on_msg (Paste (Event.Paste.text ev))))
            ~on_submit ();
        ];
      bottom_rule ~rule_color;
    ]
