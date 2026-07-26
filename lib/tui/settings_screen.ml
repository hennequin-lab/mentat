(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Config = Mentat_config
module View = Config.Resolved.View
module Entry = View.Entry
module Config_value = View.Value
module Provider = Mentat_llm.Provider
module Account = Mentat_provider.Account
module Account_phase = Account.Phase
module Discovery = Account.Discovery
module Credential_error = Mentat_provider.Credential_error
module Session = Mentat_session
module Session_view = Session.Session_view
module Summary = Session.Summary

let default_style = Ansi.Style.default

let is_white_space uchar =
  match Uchar.to_int uchar with
  | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x20 | 0x85 | 0xA0 | 0x1680 | 0x2028
  | 0x2029 | 0x202F | 0x205F | 0x3000 ->
      true
  | code -> code >= 0x2000 && code <= 0x200A

let add_multiline_uchar buffer uchar =
  let code = Uchar.to_int uchar in
  match code with
  | 0x09 | 0x0A | 0x0D | 0x2028 | 0x2029 -> Buffer.add_char buffer ' '
  | code when code < 0x20 || (code >= 0x7F && code <= 0x9F) ->
      Buffer.add_utf_8_uchar buffer Uchar.rep
  | _ -> Buffer.add_utf_8_uchar buffer uchar

(* Every string that did not originate in this module crosses this boundary
   before Mosaic sees it. This makes terminal controls inert and malformed UTF-8
   visible rather than geometry-changing. *)
let normalize_inline = Prims.normalize_inline

(* Diagnostics are the one external value whose line structure is semantic.
   Preserve line feeds while applying the same terminal-safety boundary as
   [normalize_inline]; Mosaic performs wrapping and viewport clipping. *)
let normalize_multiline text =
  let buffer = Buffer.create (String.length text) in
  let rec loop offset =
    if offset < String.length text then begin
      let decoded = String.get_utf_8_uchar text offset in
      let length = Uchar.utf_decode_length decoded in
      if Uchar.utf_decode_is_valid decoded then
        let uchar = Uchar.utf_decode_uchar decoded in
        if Uchar.to_int uchar = 0x0A then Buffer.add_char buffer '\n'
        else add_multiline_uchar buffer uchar
      else Buffer.add_utf_8_uchar buffer Uchar.rep;
      loop (offset + length)
    end
  in
  loop 0;
  Buffer.contents buffer

let visibly_nonblank text =
  let visible = ref false in
  (try
     Matrix.Text.iter_graphemes
       (fun ~offset ~len ->
         let grapheme = String.sub text offset len in
         let contains_non_space = ref false in
         let rec inspect position =
           if position < String.length grapheme then begin
             let decoded = String.get_utf_8_uchar grapheme position in
             let uchar = Uchar.utf_decode_uchar decoded in
             if not (is_white_space uchar) then contains_non_space := true;
             inspect (position + Uchar.utf_decode_length decoded)
           end
         in
         inspect 0;
         if !contains_non_space then begin
           visible := true;
           raise Exit
         end)
       text
   with Exit -> ());
  !visible

let drop_last_grapheme text =
  let last = ref None in
  Matrix.Text.iter_graphemes
    (fun ~offset ~len ->
      if offset + len <= String.length text then last := Some offset)
    text;
  match !last with None -> text | Some offset -> String.sub text 0 offset

let contains ~needle haystack =
  String.includes
    ~affix:(String.lowercase_ascii needle)
    (String.lowercase_ascii haystack)

let format_int value =
  let text = string_of_int value in
  let length = String.length text in
  if length <= 3 then text
  else
    let buffer = Buffer.create (length + (length / 3)) in
    String.iteri
      (fun index char ->
        if index > 0 && (length - index) mod 3 = 0 then
          Buffer.add_char buffer ',';
        Buffer.add_char buffer char)
      text;
    Buffer.contents buffer

let formatted pp value = Format.asprintf "%a" pp value |> normalize_inline

let normalize_error ~fallback message =
  let message = normalize_inline message in
  if visibly_nonblank message then message else fallback

type tab = Command.settings_tab = Config | Status | Usage

type 'a remote =
  | Loading
  | Unavailable of string
  | Available of { value : 'a; refresh_error : string option }

type control = Model_control | Permission_control

type config_item =
  | Control of control
  | Warning of int * Config.Warning.t
  | Effective of View.Entry.t

type config_identity =
  | Model_identity
  | Permission_identity
  | Warning_identity of int
  | Entry_identity of string

type issue = No_active_session

type config_diagnostic =
  | Diagnostic of Ansi.Style.t * string
  | Diagnostic_error of string

type mutation = { nonce : unit ref; session_id : Session.Id.t }

type pending = {
  request : mutation;
  review : Mentat_permission.Review_behavior.t;
}

type mutation_feedback =
  | Accepted of Mentat_permission.Review_behavior.t
  | Rejected of Mentat_permission.Review_behavior.t * Mentat_diagnostic.t

type t = {
  snapshot : Snapshot.t;
  session : Session_view.t option;
  configuration : View.t remote;
  readiness : Discovery.t list remote;
  tab : tab;
  filter : string option;
  selected : config_identity;
  review_choice : Mentat_permission.Review_behavior.t option;
  issue : issue option;
  pending : pending option;
  feedback : mutation_feedback option;
}

type msg = Key of Screen.key | Select_config of int | Activate_config of int

type event =
  | Stay
  | Close
  | Open_model_panel
  | Set_permission_review of {
      request : mutation;
      review : Mentat_permission.Review_behavior.t;
    }

let loading ~tab ~snapshot ~session =
  {
    snapshot;
    session;
    configuration = Loading;
    readiness = Loading;
    tab;
    filter = None;
    selected = Model_identity;
    review_choice = None;
    issue = None;
    pending = None;
    feedback = None;
  }

let session_id session = Session_view.summary session |> Summary.id

let same_session left right =
  match (left, right) with
  | None, None -> true
  | Some left, Some right ->
      Session.Id.equal (session_id left) (session_id right)
  | None, Some _ | Some _, None -> false

let set_session session (t : t) =
  if same_session t.session session then { t with session; issue = None }
  else
    {
      t with
      session;
      review_choice = None;
      issue = None;
      pending = None;
      feedback = None;
    }

let mutation_equal (left : mutation) (right : mutation) =
  left.nonce == right.nonce && Session.Id.equal left.session_id right.session_id

let request_matches_current_session (request : mutation) (t : t) =
  match t.session with
  | Some session -> Session.Id.equal request.session_id (session_id session)
  | None -> false

let clear_accepted_review review t =
  let review_choice =
    match t.review_choice with
    | Some candidate
      when Mentat_permission.Review_behavior.equal candidate review ->
        None
    | choice -> choice
  in
  { t with review_choice }

let mutation_finished request result t =
  match t.pending with
  | Some pending
    when mutation_equal request pending.request
         && request_matches_current_session request t -> (
      match result with
      | Ok () ->
          let t = clear_accepted_review pending.review t in
          {
            t with
            pending = None;
            feedback = Some (Accepted pending.review);
            issue = None;
          }
      | Error error ->
          {
            t with
            pending = None;
            feedback =
              Some
                (Rejected
                   (pending.review, Mentat_protocol.Error.diagnostic error));
            issue = None;
          })
  | Some _ | None -> t

let config_identity = function
  | Control Model_control -> Model_identity
  | Control Permission_control -> Permission_identity
  | Warning (index, _) -> Warning_identity index
  | Effective entry -> Entry_identity (Entry.key entry)

let origin_text entry = formatted Config.Origin.pp (Entry.origin entry)

let warning_text warning =
  let source = formatted Config.Source.pp (Config.Warning.source warning) in
  let kind = Config.Warning.kind_string warning |> normalize_inline in
  let message = Config.Warning.message warning |> normalize_inline in
  Printf.sprintf "%s · %s · %s" kind message source

let entry_value_text entry =
  match Entry.value entry with
  | Config_value.Shown { text; json = _ } -> normalize_inline text
  | Config_value.Redacted -> "[REDACTED]"

let control_search_text = function
  | Model_control -> "model selector current model"
  | Permission_control -> "permission review"

let item_matches query = function
  | Control control -> contains ~needle:query (control_search_text control)
  | Warning (_, warning) -> contains ~needle:query (warning_text warning)
  | Effective entry ->
      contains ~needle:query
        (String.concat " "
           [
             normalize_inline (Entry.key entry);
             entry_value_text entry;
             origin_text entry;
           ])

let configuration_entries t =
  match t.configuration with
  | Loading | Unavailable _ -> []
  | Available { value; refresh_error = _ } -> View.entries value

let configuration_warnings t =
  match t.configuration with
  | Loading | Unavailable _ -> []
  | Available { value; refresh_error = _ } -> View.warnings value

let config_items t =
  let items =
    [ Control Model_control; Control Permission_control ]
    @ List.mapi
        (fun index warning -> Warning (index, warning))
        (configuration_warnings t)
    @ List.map (fun entry -> Effective entry) (configuration_entries t)
  in
  match t.filter with
  | None | Some "" -> items
  | Some query -> List.filter (item_matches query) items

let has_identity identity items =
  List.exists (fun item -> config_identity item = identity) items

let repair_selection t =
  let items = config_items t in
  if has_identity t.selected items then t
  else
    match items with
    | [] -> t
    | first :: _ -> { t with selected = config_identity first }

let configuration_loaded value t =
  repair_selection
    { t with configuration = Available { value; refresh_error = None } }

let configuration_failed message t =
  let message =
    normalize_error ~fallback:"effective configuration unavailable" message
  in
  let configuration =
    match t.configuration with
    | Available available ->
        Available { available with refresh_error = Some message }
    | Loading | Unavailable _ -> Unavailable message
  in
  { t with configuration }

let compare_readiness left right =
  Provider.compare (Discovery.provider left) (Discovery.provider right)

let readiness_loaded value t =
  let value = List.stable_sort compare_readiness value in
  { t with readiness = Available { value; refresh_error = None } }

let readiness_failed message t =
  let message =
    normalize_error ~fallback:"account readiness unavailable" message
  in
  let readiness =
    match t.readiness with
    | Available available ->
        Available { available with refresh_error = Some message }
    | Loading | Unavailable _ -> Unavailable message
  in
  { t with readiness }

let key event =
  match Screen.classify event with
  | Screen.Action Screen.Other -> None
  | message -> Some (Key message)

let append_to_active text t =
  let text = normalize_inline text in
  match (t.tab, t.filter) with
  | Config, Some query ->
      repair_selection { t with filter = Some (query ^ text); issue = None }
  | Config, None | Status, _ | Usage, _ -> t

let paste = append_to_active
let tabs = [ Config; Status; Usage ]
let tab_index tab = match tab with Config -> 0 | Status -> 1 | Usage -> 2

let switch_tab direction t =
  let count = List.length tabs in
  let index = Option_list.wrap ~count (tab_index t.tab) direction in
  {
    t with
    tab = List.nth tabs index;
    filter = None;
    issue = None;
    feedback = None;
  }

let move_selection direction t =
  let items = config_items t in
  let count = List.length items in
  if count = 0 then t
  else
    let current =
      match
        List.find_index (fun item -> config_identity item = t.selected) items
      with
      | Some index -> index
      | None -> 0
    in
    let index = Option_list.wrap ~count current direction in
    {
      t with
      selected = config_identity (List.nth items index);
      issue = None;
      feedback = None;
    }

let selected_item t =
  List.find_opt (fun item -> config_identity item = t.selected) (config_items t)

let index_of equal value values =
  Option.value (List.find_index (equal value) values) ~default:0

let cycle values ~equal ~current ~direction =
  match values with
  | [] -> invalid_arg "Settings_screen.cycle: empty vocabulary"
  | _ ->
      let count = List.length values in
      let index =
        match current with
        | Some current -> index_of equal current values
        | None -> if direction < 0 then 0 else -1
      in
      List.nth values (Option_list.wrap ~count index direction)

let require_session t event =
  match t.session with
  | Some _ -> ({ t with issue = None }, event)
  | None -> ({ t with issue = Some No_active_session }, Stay)

let start_mutation review event t =
  match (t.pending, t.session) with
  | Some _, _ -> (t, Stay)
  | None, None -> ({ t with issue = Some No_active_session }, Stay)
  | None, Some session ->
      let request = { nonce = ref (); session_id = session_id session } in
      ( {
          t with
          pending = Some { request; review };
          feedback = None;
          issue = None;
        },
        event request )

let open_model t =
  match t.pending with
  | Some _ -> (t, Stay)
  | None -> require_session t Open_model_panel

let review_values =
  [
    Mentat_permission.Review_behavior.Enforce;
    Mentat_permission.Review_behavior.Bypass;
  ]

(* Left and Right choose the permission review value as a local, visibly
   unconfirmed candidate. No request reaches the engine here, so cycling never
   round-trips and never reverts under a correlated echo. *)
let pick_review direction t =
  match t.pending with
  | Some _ -> (t, Stay)
  | None ->
      let choice =
        cycle review_values ~equal:Mentat_permission.Review_behavior.equal
          ~current:t.review_choice ~direction
      in
      ( { t with review_choice = Some choice; feedback = None; issue = None },
        Stay )

(* Enter applies the chosen candidate for the next turn. With nothing chosen
   yet it selects the leading value, so a first Enter starts the choice and a
   second Enter applies it. *)
let commit_review t =
  match (t.pending, t.review_choice) with
  | Some _, _ -> (t, Stay)
  | None, Some review ->
      start_mutation review
        (fun request -> Set_permission_review { request; review })
        t
  | None, None -> pick_review 1 t

let activate_config t =
  match selected_item t with
  | Some (Control Model_control) -> open_model t
  | Some (Control Permission_control) -> commit_review t
  | Some (Warning _ | Effective _) | None -> (t, Stay)

(* Left and Right change the focused setting's value. Only the permission
   control carries an editable value; every other row has none, so the keys are
   inert there rather than meaning something different. *)
let change_value direction t =
  match selected_item t with
  | Some (Control Permission_control) -> pick_review direction t
  | Some (Control Model_control) | Some (Warning _ | Effective _) | None ->
      (t, Stay)

let update_config_closed message t =
  match message with
  | Screen.Action Screen.Up -> (move_selection (-1) t, Stay)
  | Screen.Action Screen.Down -> (move_selection 1 t, Stay)
  | Screen.Char "/" ->
      (repair_selection { t with filter = Some ""; feedback = None }, Stay)
  | Screen.Action Screen.Enter -> activate_config t
  | Screen.Action Screen.Left -> change_value (-1) t
  | Screen.Action Screen.Right -> change_value 1 t
  | Screen.Action Screen.Tab -> (switch_tab 1 t, Stay)
  | Screen.Action Screen.Escape -> (t, Close)
  | Screen.Char _
  | Screen.Action
      (Screen.Backspace | Screen.Page_up | Screen.Page_down | Screen.Other) ->
      (t, Stay)

let update_config_filter query message t =
  match message with
  | Screen.Action Screen.Escape ->
      (repair_selection { t with filter = None }, Stay)
  | Screen.Action Screen.Backspace ->
      ( repair_selection
          { t with filter = Some (drop_last_grapheme query); issue = None },
        Stay )
  | Screen.Char text -> (append_to_active text t, Stay)
  | Screen.Action Screen.Up -> (move_selection (-1) t, Stay)
  | Screen.Action Screen.Down -> (move_selection 1 t, Stay)
  | Screen.Action Screen.Enter -> activate_config t
  | Screen.Action Screen.Tab -> (switch_tab 1 t, Stay)
  | Screen.Action
      ( Screen.Left | Screen.Right | Screen.Page_up | Screen.Page_down
      | Screen.Other ) ->
      (t, Stay)

let update_read_only message t =
  match message with
  | Screen.Action Screen.Tab -> (switch_tab 1 t, Stay)
  | Screen.Action Screen.Escape -> (t, Close)
  | Screen.Char _
  | Screen.Action
      ( Screen.Enter | Screen.Up | Screen.Down | Screen.Left | Screen.Right
      | Screen.Backspace | Screen.Page_up | Screen.Page_down | Screen.Other ) ->
      (t, Stay)

let update_surface message t =
  match (t.tab, t.filter) with
  | Config, Some query -> update_config_filter query message t
  | Config, None -> update_config_closed message t
  | (Status | Usage), _ -> update_read_only message t

let update_config_widget ~activate index t =
  match List.nth_opt (config_items t) index with
  | None -> (t, Stay)
  | Some item ->
      let t = { t with selected = config_identity item; issue = None } in
      if activate then activate_config t else (t, Stay)

let update message t =
  match message with
  | Key key -> update_surface key t
  | Select_config index -> update_config_widget ~activate:false index t
  | Activate_config index -> update_config_widget ~activate:true index t

(* Rendering. *)

let zero_size = { width = px 0; height = px 0 }
let fill_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = px 0 }
let horizontal_gap = { width = px 2; height = px 0 }
let vertical_gap = { width = px 0; height = px 1 }

let paragraph ?(style = default_style) ?(wrap = `Word) text_value =
  text ~style ~wrap ~flex_shrink:0. ~size:fill_width
    (normalize_inline text_value)

let multiline ?(style = default_style) text_value =
  text ~style ~wrap:`Word ~flex_shrink:0. ~size:fill_width
    (normalize_multiline text_value)

let callout ~style ~marker text_value =
  box ~flex_direction:Flex_direction.Row ~flex_wrap:Flex_wrap.Wrap
    ~align_items:Align.Flex_start ~gap:horizontal_gap ~flex_shrink:0.
    ~size:fill_width
    [
      text ~style ~wrap:`None ~flex_shrink:0. marker;
      text ~style ~wrap:`Word ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
        (normalize_inline text_value);
    ]

let error ~palette text_value =
  callout ~style:(Theme.Palette.error_style palette) ~marker:"!" text_value

let warning ~palette text_value =
  callout ~style:(Theme.Palette.warning_style palette) ~marker:"⚠" text_value

(* A read-only fact sheet is a stack of grouped sections. Each section is a bold
   heading over tight key-value rows; the label sits in a muted, fixed-width
   column so a group's values line up, and the value follows in the foreground.
   A value wider than the remaining width wraps and hangs at the value column
   rather than folding back under the label. Sections are separated by one blank
   row (the scroll region's gap); rows within a section are adjacent. *)
type row =
  | Pair of { label : string; value : string; value_style : Ansi.Style.t }
  | Line of Ansi.Style.t * string
  | Problem of string

let pair ?(value_style = default_style) label value =
  Pair { label; value = normalize_inline value; value_style }

let row_indent = "  "

let pad_right width text =
  let length = String.length text in
  if length >= width then text else text ^ String.make (width - length) ' '

let label_column rows =
  List.fold_left
    (fun width -> function
      | Pair { label; _ } -> max width (String.length label)
      | Line _ | Problem _ -> width)
    0 rows

let pair_view ~palette ~column label value value_style =
  box ~flex_direction:Flex_direction.Row ~flex_shrink:0. ~size:fill_width
    [
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`None ~flex_shrink:0.
        (row_indent ^ pad_right (column + 2) label);
      text ~style:value_style ~wrap:`Word ~flex_grow:1. ~flex_shrink:1.
        ~min_size:zero_size value;
    ]

let row_view ~palette ~column = function
  | Pair { label; value; value_style } ->
      pair_view ~palette ~column label value value_style
  | Line (style, value) -> paragraph ~style (row_indent ^ value)
  | Problem message -> error ~palette message

let section ~palette title rows =
  let column = label_column rows in
  box ~flex_direction:Flex_direction.Column ~flex_shrink:0. ~size:fill_width
    (paragraph ~style:Theme.bold title
    :: List.map (row_view ~palette ~column) rows)

let page_key (event : Matrix.Input.Key.event) =
  match event.Matrix.Input.Key.key with
  | Matrix.Input.Key.Page_up | Matrix.Input.Key.Page_down -> true
  | _ -> false

let route_screen_key event data =
  match key data with
  | None -> None
  | Some message ->
      Event.Key.prevent_default event;
      Some message

let route_non_page_key event =
  let data = Event.Key.data event in
  if page_key data then None else route_screen_key event data

let scroll_region ~key children =
  scroll_box ~key ~scroll_x:false ~scroll_y:true ~show_scrollbars:true
    ~focusable:true ~autofocus:true ~on_key:route_non_page_key ~flex_grow:1.
    ~flex_shrink:1. ~min_size:zero_size ~size:fill_remaining
    [
      box ~flex_direction:Flex_direction.Column ~gap:vertical_gap
        ~flex_shrink:0. ~size:fill_width children;
    ]

let mutation_target_text review =
  "permission review " ^ formatted Mentat_permission.Review_behavior.pp review

let mutation_feedback ~palette t =
  match (t.pending, t.feedback) with
  | Some pending, _ ->
      Some
        (warning ~palette
           ("Request in flight: "
           ^ mutation_target_text pending.review
           ^ " for the next turn."))
  | None, Some (Accepted target) ->
      Some
        (callout
           ~style:(Theme.Palette.success_style palette)
           ~marker:"✓"
           ("Client accepted "
           ^ mutation_target_text target
           ^ " for the next turn."))
  | None, Some (Rejected (target, diagnostic)) ->
      Some
        (scroll_box ~key:"settings.mutation-feedback" ~scroll_x:false
           ~scroll_y:true ~show_scrollbars:true ~focusable:false ~flex_shrink:1.
           ~min_size:zero_size
           ~max_size:{ width = pct 100; height = pct 40 }
           ~size:fill_width
           [
             box ~flex_direction:Flex_direction.Column ~gap:vertical_gap
               ~flex_shrink:0. ~size:fill_width
               [
                 error ~palette
                   ("Client rejected " ^ mutation_target_text target ^ ".");
                 multiline
                   ~style:(Theme.Palette.error_style palette)
                   (Mentat_diagnostic.to_string diagnostic);
               ];
           ])
  | None, None -> None

let tab_label = function
  | Config -> "config"
  | Status -> "status"
  | Usage -> "usage"

let tab_row ~palette active =
  box ~key:"settings.tabs" ~flex_direction:Flex_direction.Row
    ~flex_wrap:Flex_wrap.Wrap ~gap:horizontal_gap ~flex_shrink:0.
    ~size:fill_width
    (List.map
       (fun tab ->
         let style =
           if tab = active then Theme.Palette.accent_style palette
           else Theme.Palette.muted_style palette
         in
         text ~style ~wrap:`None ~flex_shrink:0. (tab_label tab))
       tabs)

let config_control_value control t =
  match control with
  | Model_control -> (
      match t.session with
      | Some session -> (
          match Session_view.active_model session with
          | Some model ->
              Provider.id (Mentat_llm.Model.provider model)
              ^ "/" ^ Mentat_llm.Model.id model
          | None -> Snapshot.model_line t.snapshot)
      | None -> Snapshot.model_line t.snapshot)
  | Permission_control -> (
      match t.pending with
      | Some { review; _ } ->
          "requesting: " ^ formatted Mentat_permission.Review_behavior.pp review
      | None -> (
          match t.review_choice with
          | Some review ->
              "candidate: "
              ^ formatted Mentat_permission.Review_behavior.pp review
          | None -> "choose a next-turn request"))

let control_label = function
  | Model_control -> "model"
  | Permission_control -> "permission review"

let control_style ~palette control t =
  match (control, t.session, t.review_choice) with
  | _, None, _ -> Theme.Palette.muted_style palette
  | Permission_control, Some _, Some Mentat_permission.Review_behavior.Bypass ->
      Theme.Palette.warning_style palette
  | _ -> Theme.Palette.accent_style palette

let selected_index t items =
  Option.value
    (List.find_index (fun item -> config_identity item = t.selected) items)
    ~default:0

let configuration_diagnostics ~palette t =
  let next_turn =
    Diagnostic
      ( Theme.Palette.faint_style palette,
        "Session controls below apply to the next turn only." )
  in
  let issue =
    match t.issue with
    | Some No_active_session ->
        [
          Diagnostic_error
            "No active session. Settings changes require an active session.";
        ]
    | None -> []
  in
  match t.configuration with
  | Loading ->
      (next_turn :: issue)
      @ [
          Diagnostic
            ( Theme.Palette.muted_style palette,
              "⠋ loading effective configuration…" );
        ]
  | Unavailable message -> (next_turn :: issue) @ [ Diagnostic_error message ]
  | Available { value = _; refresh_error } ->
      let refresh =
        match refresh_error with
        | None -> []
        | Some message ->
            [ Diagnostic_error (message ^ " · showing retained values") ]
      in
      (next_turn :: issue) @ refresh

let diagnostic_view ~palette = function
  | Diagnostic (style, text_value) -> paragraph ~style text_value
  | Diagnostic_error text_value -> error ~palette text_value

let table_cell ?style text_value =
  Table.cell ?style (normalize_inline text_value)

let config_table_row ~palette ~selected item t =
  let marker = if selected then "›" else "" in
  match item with
  | Control control ->
      [|
        table_cell ~style:(Theme.Palette.accent_style palette) marker;
        table_cell (control_label control);
        table_cell
          ~style:(control_style ~palette control t)
          (config_control_value control t);
        table_cell ~style:(Theme.Palette.faint_style palette) "next turn";
      |]
  | Warning (_, warning_value) ->
      [|
        table_cell
          ~style:(Theme.Palette.warning_style palette)
          (if selected then "›" else "!");
        table_cell
          ~style:(Theme.Palette.warning_style palette)
          (Config.Warning.kind_string warning_value);
        table_cell
          ~style:(Theme.Palette.warning_style palette)
          (Config.Warning.message warning_value);
        table_cell
          ~style:(Theme.Palette.muted_style palette)
          (formatted Config.Source.pp (Config.Warning.source warning_value));
      |]
  | Effective entry ->
      let value_style =
        match Entry.value entry with
        | Config_value.Redacted -> Theme.Palette.warning_style palette
        | Config_value.Shown _ -> default_style
      in
      [|
        table_cell ~style:(Theme.Palette.accent_style palette) marker;
        table_cell (Entry.key entry);
        table_cell ~style:value_style (entry_value_text entry);
        table_cell
          ~style:(Theme.Palette.muted_style palette)
          (origin_text entry);
      |]

let config_columns =
  [
    Table.column ~width:`Auto ~overflow:`Crop "";
    Table.column ~width:(`Flex 2.) ~overflow:`Ellipsis "setting";
    Table.column ~width:(`Flex 3.) ~overflow:`Ellipsis "value";
    Table.column ~width:(`Flex 2.) ~overflow:`Ellipsis "source";
  ]

let config_widget_key event =
  let data = Event.Key.data event in
  match data.Matrix.Input.Key.key with
  | Matrix.Input.Key.Up | Matrix.Input.Key.Down | Matrix.Input.Key.Page_up
  | Matrix.Input.Key.Page_down | Matrix.Input.Key.Enter
  | Matrix.Input.Key.KP_enter ->
      None
  | _ -> route_screen_key event data

let config_body ~palette t =
  let diagnostics =
    configuration_diagnostics ~palette t |> List.map (diagnostic_view ~palette)
  in
  let items = config_items t in
  let selected = selected_index t items in
  let rows =
    List.mapi
      (fun index item ->
        config_table_row ~palette ~selected:(index = selected) item t)
      items
  in
  let content =
    match rows with
    | [] ->
        paragraph
          ~style:(Theme.Palette.muted_style palette)
          "No matching settings."
    | _ ->
        table ~key:"settings.config-table" ~columns:config_columns ~rows
          ~selected_row:selected ~border:false ~show_header:true
          ~show_column_separator:false ~show_row_separator:false ~cell_padding:1
          ~selected_text_color:(Theme.Palette.selection_fg palette)
          ~selected_background:(Theme.Palette.selection_bg palette)
          ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
          ~focused_selected_background:(Theme.Palette.selection_bg palette)
          ~wrap_selection:true ~show_scroll_indicator:true ~focusable:true
          ~autofocus:true ~on_key:config_widget_key ~flex_grow:1.
          ~flex_shrink:1. ~min_size:zero_size ~size:fill_remaining
          ~activate_on_click:true
          ~on_change:(fun index -> Some (Select_config index))
          ~on_activate:(fun index -> Some (Activate_config index))
          ()
  in
  box ~key:"settings.config" ~flex_direction:Flex_direction.Column
    ~gap:vertical_gap ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
    ~size:fill_remaining
    (diagnostics @ [ content ])

let lifecycle_text status = formatted Session.Metadata.Status.pp status
let workflow_text mode = formatted Session.Contract.Mode.pp mode
let outcome_text outcome = formatted Session.Turn.Outcome.pp outcome

let model_text model =
  Provider.id (Mentat_llm.Model.provider model)
  ^ "/" ^ Mentat_llm.Model.id model

let snapshot_status_rows snapshot =
  let context =
    match Snapshot.context_window snapshot with
    | None -> "—"
    | Some tokens -> format_int tokens ^ " tokens"
  in
  let sandbox = Option.value (Snapshot.sandbox snapshot) ~default:"—" in
  [
    pair "version" (Snapshot.version snapshot);
    pair "current model" (Snapshot.model_line snapshot);
    pair "workspace"
      (Path_display.home_relative ~home:(Snapshot.home snapshot)
         (Snapshot.cwd snapshot));
    pair "context window" context;
    pair "launch sandbox" sandbox;
  ]

let session_status_rows ~palette = function
  | None -> [ Line (Theme.Palette.muted_style palette, "No active session.") ]
  | Some session ->
      let summary = Session_view.summary session in
      let active_model =
        match Session_view.active_model session with
        | Some model -> model_text model
        | None -> "none"
      in
      let waiting =
        match Session_view.waiting session with
        | Some waiting -> Session_view.Waiting.to_string waiting
        | None -> "none"
      in
      let workflow =
        match Session_view.workflow_mode session with
        | Some mode -> workflow_text mode
        | None -> "none"
      in
      let outcome =
        match Session_view.last_outcome session with
        | Some outcome -> outcome_text outcome
        | None -> "none"
      in
      [
        pair "id" (Summary.id summary |> Session.Id.to_string);
        pair "lifecycle" (Summary.lifecycle summary |> lifecycle_text);
        pair "phase" (Summary.phase summary |> Summary.Phase.to_string);
        pair "active model" active_model;
        pair "waiting" waiting;
        pair "workflow" workflow;
        pair "last outcome" outcome;
      ]

let phase_style ~palette = function
  | Account_phase.Ready -> Theme.Palette.success_style palette
  | Account_phase.Unchecked | Account_phase.Degraded ->
      Theme.Palette.warning_style palette
  | Account_phase.Missing -> Theme.Palette.muted_style palette
  | Account_phase.Blocked -> Theme.Palette.error_style palette

let readiness_row ~palette = function
  | Discovery.Known account ->
      let phase = Account.phase account in
      Pair
        {
          label = Provider.id (Account.provider account);
          value = Account_phase.to_string phase;
          value_style = phase_style ~palette phase;
        }
  | Discovery.Resolution_failed { provider; error } ->
      Pair
        {
          label = Provider.id provider;
          value = Credential_error.message error |> normalize_inline;
          value_style = Theme.Palette.error_style palette;
        }

let readiness_rows ~palette = function
  | Loading ->
      [
        Line (Theme.Palette.muted_style palette, "⠋ loading account readiness…");
      ]
  | Unavailable message -> [ Problem message ]
  | Available { value; refresh_error } ->
      let refresh =
        match refresh_error with
        | None -> []
        | Some message -> [ Problem (message ^ " · showing retained phases") ]
      in
      let rows =
        match value with
        | [] ->
            [
              Line
                ( Theme.Palette.muted_style palette,
                  "No account discoveries reported." );
            ]
        | value -> List.map (readiness_row ~palette) value
      in
      refresh @ rows

let status_body ~palette t =
  scroll_region ~key:"settings.status-scroll"
    [
      section ~palette "Runtime" (snapshot_status_rows t.snapshot);
      section ~palette "Session" (session_status_rows ~palette t.session);
      section ~palette "Providers" (readiness_rows ~palette t.readiness);
    ]

let usage_sections ~palette session =
  let metrics = Session_view.metrics session in
  let usage = metrics.Session.Metrics.usage in
  let count label value = pair label (format_int value) in
  let model =
    match Session_view.active_model session with
    | Some model -> model_text model
    | None -> "none"
  in
  let total =
    Mentat_llm.Usage.input_total usage + Mentat_llm.Usage.output_total usage
  in
  let tokens =
    [
      Pair
        { label = "total"; value = format_int total; value_style = Theme.bold };
      count "input" usage.Mentat_llm.Usage.input;
      count "cache read" usage.Mentat_llm.Usage.cache_read;
      count "cache write" usage.Mentat_llm.Usage.cache_write;
      count "output" usage.Mentat_llm.Usage.output;
      count "reasoning" usage.Mentat_llm.Usage.reasoning;
    ]
  in
  let activity =
    [
      count "turns" metrics.Session.Metrics.turns;
      count "responses" metrics.Session.Metrics.responses;
      count "tool calls" metrics.Session.Metrics.tool_calls;
      count "tool failures" metrics.Session.Metrics.tool_failures;
      count "tool rejections" metrics.Session.Metrics.tool_rejections;
      count "permission denials" metrics.Session.Metrics.permission_denials;
    ]
  in
  let tools =
    match metrics.Session.Metrics.tool_calls_by_name with
    | [] -> []
    | tools ->
        [
          section ~palette "Tool calls"
            (List.map (fun (name, calls) -> count name calls) tools);
        ]
  in
  [
    section ~palette "Model" [ Line (default_style, model) ];
    section ~palette "Tokens" tokens;
    section ~palette "Activity" activity;
  ]
  @ tools

let usage_body ~palette t =
  match t.session with
  | None ->
      scroll_region ~key:"settings.usage-scroll"
        [
          paragraph
            ~style:(Theme.Palette.muted_style palette)
            "No active session usage.";
        ]
  | Some session ->
      scroll_region ~key:"settings.usage-scroll"
        (usage_sections ~palette session)

let content ~palette t =
  let body =
    match t.tab with
    | Config -> config_body ~palette t
    | Status -> status_body ~palette t
    | Usage -> usage_body ~palette t
  in
  let feedback = Option.to_list (mutation_feedback ~palette t) in
  box ~key:"settings.content" ~flex_direction:Flex_direction.Column
    ~gap:vertical_gap ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
    ~size:fill_remaining
    ((tab_row ~palette t.tab :: feedback) @ [ body ])

let active_matches t =
  match t.tab with
  | Config -> List.length (config_items t)
  | Status | Usage -> 0

let filter t =
  match (t.tab, t.filter) with
  | Config, Some query ->
      Some { Screen.query = normalize_inline query; matches = active_matches t }
  | Config, None | Status, _ | Usage, _ -> None

let fact t =
  match t.pending with
  | Some _ -> "requesting"
  | None -> (
      match t.feedback with
      | Some (Accepted _) -> "accepted"
      | Some (Rejected _) -> "error"
      | None -> (
          match t.tab with
          | Config -> (
              match t.configuration with
              | Loading -> "loading"
              | Unavailable _ -> "unavailable"
              | Available { value; refresh_error = _ } ->
                  Printf.sprintf "%d values" (List.length (View.entries value)))
          | Status -> (
              match t.readiness with
              | Loading -> "loading"
              | Unavailable _ -> "unavailable"
              | Available { value; refresh_error = _ } ->
                  Printf.sprintf "%d providers" (List.length value))
          | Usage ->
              if Option.is_some t.session then "this session" else "no session")
      )

let hint t =
  match t.pending with
  | Some _ -> [ "request in flight"; "tab page"; "esc back" ]
  | None -> (
      let surface =
        match (t.tab, t.filter, t.issue) with
        | Config, Some _, _ when active_matches t = 0 ->
            [ "type to filter"; "esc clear filter" ]
        | Config, Some _, _ -> [ "↑↓ select"; "↵ action"; "esc clear filter" ]
        | Config, None, _ ->
            [
              "↑↓ move";
              "←→ change";
              "↵ apply";
              "tab page";
              "/ filter";
              "esc back";
            ]
        | (Status | Usage), _, _ -> [ "tab page"; "esc back" ]
      in
      match t.feedback with
      | Some (Rejected _) -> "scroll error" :: surface
      | Some (Accepted _) | None -> surface)

let view ~palette ~frame t =
  Screen.view ~palette ~frame ~name:"settings" ~fact:(fact t) ~filter:(filter t)
    ~hint:(hint t) ~content:(content ~palette t)
