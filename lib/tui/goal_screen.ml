(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Goal = Mentat_session.Goal

type choice = Pause_choice | Edit_choice | Resume_choice | Clear_choice

type command =
  | Pause of Goal.Id.t
  | Edit of { goal : Goal.Id.t; objective : string }
  | Resume of { goal : Goal.Id.t; budget : int option }
  | Clear of Goal.Id.t

type mutation = { nonce : unit ref; goal : Goal.Id.t }
type pending = { mutation : mutation; command : command; acknowledged : bool }
type feedback = Rejected of command * Mentat_diagnostic.t

type projection =
  | Loading
  | Unavailable of Mentat_diagnostic.t
  | Available of {
      goal : Goal.t option;
      refresh_error : Mentat_diagnostic.t option;
    }

type editor =
  | Browsing
  | Editing_objective of Inline_input.t
  | Editing_budget of Inline_input.t
  | Confirming_clear

type issue = Empty_objective | Unchanged_objective | Invalid_budget

type t = {
  projection : projection;
  detail_generation : int;
  detail_page : (int * [ `Up | `Down ]) option;
  next_detail_page : int;
  selected : choice option;
  editor : editor;
  issue : issue option;
  pending : pending option;
  feedback : feedback option;
}

type msg =
  | Key of Screen.key
  | Detail_page_applied of int
  | Select_action of int
  | Activate_action of int
  | Input_msg of Inline_input.msg

type event =
  | Stay
  | Close
  | Request of { mutation : mutation; command : command }

let actions = function
  | None -> []
  | Some goal -> (
      match Goal.status goal with
      | Goal.Status.Active -> [ Pause_choice; Edit_choice; Clear_choice ]
      | Goal.Status.Paused | Goal.Status.Blocked _ | Goal.Status.Budget_limited
        ->
          [ Resume_choice; Edit_choice; Clear_choice ]
      | Goal.Status.Completed _ | Goal.Status.Cleared -> [])

let first_action goal =
  match actions goal with [] -> None | action :: _ -> Some action

let available goal = Available { goal; refresh_error = None }

let loading =
  {
    projection = Loading;
    detail_generation = 0;
    detail_page = None;
    next_detail_page = 0;
    selected = None;
    editor = Browsing;
    issue = None;
    pending = None;
    feedback = None;
  }

let make goal =
  {
    projection = available goal;
    detail_generation = 0;
    detail_page = None;
    next_detail_page = 0;
    selected = first_action goal;
    editor = Browsing;
    issue = None;
    pending = None;
    feedback = None;
  }

let current_goal t =
  match t.projection with
  | Available { goal; _ } -> goal
  | Loading | Unavailable _ -> None

let current_actions t = actions (current_goal t)

let set_goal goal t =
  match t.projection with
  | Available { goal = previous; refresh_error }
    when Option.equal Goal.equal goal previous ->
      if Option.is_none refresh_error then t
      else
        {
          t with
          projection = available goal;
          detail_generation = t.detail_generation + 1;
          detail_page = None;
        }
  | Loading | Unavailable _ | Available _ ->
      {
        projection = available goal;
        detail_generation = t.detail_generation + 1;
        detail_page = None;
        next_detail_page = t.next_detail_page;
        selected = first_action goal;
        editor = Browsing;
        issue = None;
        pending = None;
        feedback = None;
      }

let load_failed error t =
  let diagnostic = Mentat_protocol.Error.diagnostic error in
  match t.projection with
  | Loading | Unavailable _ ->
      {
        loading with
        projection = Unavailable diagnostic;
        detail_generation = t.detail_generation + 1;
        feedback = None;
      }
  | Available { goal; _ } ->
      {
        t with
        projection = Available { goal; refresh_error = Some diagnostic };
        detail_generation = t.detail_generation + 1;
        detail_page = None;
        editor = Browsing;
        issue = None;
        pending = None;
        feedback = None;
      }

let mutation_equal left right =
  left.nonce == right.nonce && Goal.Id.equal left.goal right.goal

let mutation_finished request result t =
  match t.pending with
  | Some pending when mutation_equal request pending.mutation -> (
      match result with
      | Ok () ->
          {
            t with
            pending = Some { pending with acknowledged = true };
            feedback = None;
          }
      | Error error ->
          {
            t with
            pending = None;
            feedback =
              Some
                (Rejected
                   (pending.command, Mentat_protocol.Error.diagnostic error));
          })
  | Some _ | None -> t

let key event =
  match Screen.classify event with
  | Screen.Action Screen.Other -> None
  | message -> Some (Key message)

let editing t =
  match t.editor with
  | Editing_objective _ | Editing_budget _ -> true
  | Browsing | Confirming_clear -> false

let choice_equal left right = left = right

let selected_index t available =
  match t.selected with
  | None -> 0
  | Some selected ->
      Option.value
        (List.find_index (choice_equal selected) available)
        ~default:0

let select_index index t =
  let available = current_actions t in
  match List.nth_opt available index with
  | None -> t
  | Some selected -> { t with selected = Some selected; issue = None }

let move_selection delta t =
  let available = current_actions t in
  match available with
  | [] -> { t with selected = None }
  | _ ->
      let count = List.length available in
      let index = selected_index t available in
      let index = Option_list.wrap ~count index delta in
      select_index index t

let command_goal = function
  | Pause goal | Clear goal -> goal
  | Edit { goal; _ } | Resume { goal; _ } -> goal

let start command t =
  let mutation = { nonce = ref (); goal = command_goal command } in
  ( {
      t with
      editor = Browsing;
      issue = None;
      pending = Some { mutation; command; acknowledged = false };
      feedback = None;
    },
    Request { mutation; command } )

let open_choice choice t =
  match (choice, current_goal t) with
  | Pause_choice, Some goal -> start (Pause (Goal.id goal)) t
  | Edit_choice, Some goal ->
      ( {
          t with
          editor =
            Editing_objective (Inline_input.of_text (Goal.objective goal));
          issue = None;
        },
        Stay )
  | Resume_choice, Some _ ->
      ({ t with editor = Editing_budget Inline_input.empty; issue = None }, Stay)
  | Clear_choice, Some _ ->
      ({ t with editor = Confirming_clear; issue = None }, Stay)
  | (Pause_choice | Edit_choice | Resume_choice | Clear_choice), None ->
      (t, Stay)

let activate_selected t =
  match t.selected with
  | None -> (t, Stay)
  | Some choice -> open_choice choice t

let is_white_space uchar =
  match Uchar.to_int uchar with
  | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D | 0x20 | 0x85 | 0xA0 | 0x1680 | 0x2028
  | 0x2029 | 0x202F | 0x205F | 0x3000 ->
      true
  | code -> code >= 0x2000 && code <= 0x200A

(* [Inline_input] has already normalized malformed input, so this scan can
   trim semantic Unicode whitespace without consulting display geometry. *)
let trim_white_space text =
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

let editable_objective goal =
  Inline_input.of_text (Goal.objective goal)
  |> Inline_input.value |> trim_white_space

let submit_objective input t =
  let objective = Inline_input.value input |> trim_white_space in
  match current_goal t with
  | None -> ({ t with editor = Browsing; issue = None }, Stay)
  | Some _ when String.equal objective "" ->
      ({ t with issue = Some Empty_objective }, Stay)
  | Some goal when String.equal objective (editable_objective goal) ->
      ({ t with issue = Some Unchanged_objective }, Stay)
  | Some goal -> start (Edit { goal = Goal.id goal; objective }) t

let submit_budget input t =
  let budget = Inline_input.value input |> trim_white_space in
  match current_goal t with
  | None -> ({ t with editor = Browsing; issue = None }, Stay)
  | Some goal when String.equal budget "" ->
      start (Resume { goal = Goal.id goal; budget = None }) t
  | Some goal -> (
      match int_of_string_opt budget with
      | Some budget when budget >= 0 ->
          start (Resume { goal = Goal.id goal; budget = Some budget }) t
      | Some _ | None -> ({ t with issue = Some Invalid_budget }, Stay))

let input_outcome input_kind outcome t =
  match outcome with
  | Inline_input.Stay input ->
      let editor =
        match input_kind with
        | `Objective -> Editing_objective input
        | `Budget -> Editing_budget input
      in
      ({ t with editor; issue = None }, Stay)
  | Inline_input.Cancel -> ({ t with editor = Browsing; issue = None }, Stay)
  | Inline_input.Submit _ -> (
      match input_kind with
      | `Objective -> (
          match t.editor with
          | Editing_objective input -> submit_objective input t
          | Browsing | Editing_budget _ | Confirming_clear -> (t, Stay))
      | `Budget -> (
          match t.editor with
          | Editing_budget input -> submit_budget input t
          | Browsing | Editing_objective _ | Confirming_clear -> (t, Stay)))

let confirm_clear t =
  match current_goal t with
  | None -> ({ t with editor = Browsing }, Stay)
  | Some goal -> start (Clear (Goal.id goal)) t

let update_browsing message t =
  match message with
  | Screen.Action Screen.Escape -> (t, Close)
  | Screen.Action Screen.Up -> (move_selection (-1) t, Stay)
  | Screen.Action Screen.Down -> (move_selection 1 t, Stay)
  | Screen.Action Screen.Enter -> activate_selected t
  | Screen.Char digit -> (
      match int_of_string_opt digit with
      | Some number
        when number >= 1 && number <= List.length (current_actions t) ->
          (select_index (number - 1) t, Stay)
      | Some _ | None -> (t, Stay))
  | Screen.Action
      ( Screen.Tab | Screen.Left | Screen.Right | Screen.Backspace
      | Screen.Page_up | Screen.Page_down | Screen.Other ) ->
      (t, Stay)

let update_editor message t =
  match message with
  | Screen.Action Screen.Escape ->
      ({ t with editor = Browsing; issue = None }, Stay)
  | Screen.Char _
  | Screen.Action
      ( Screen.Enter | Screen.Tab | Screen.Up | Screen.Down | Screen.Left
      | Screen.Right | Screen.Backspace | Screen.Page_up | Screen.Page_down
      | Screen.Other ) ->
      (* The focused Mosaic input owns every non-Escape key. *)
      (t, Stay)

let update_confirmation message t =
  match message with
  | Screen.Action Screen.Enter -> confirm_clear t
  | Screen.Action Screen.Escape ->
      ({ t with editor = Browsing; issue = None }, Stay)
  | Screen.Char _
  | Screen.Action
      ( Screen.Tab | Screen.Up | Screen.Down | Screen.Left | Screen.Right
      | Screen.Backspace | Screen.Page_up | Screen.Page_down | Screen.Other ) ->
      ({ t with editor = Browsing; issue = None }, Stay)

let page_detail direction t =
  let serial = t.next_detail_page in
  {
    t with
    detail_page = Some (serial, direction);
    next_detail_page = serial + 1;
  }

let update_key message t =
  match message with
  | Screen.Action Screen.Page_up -> (page_detail `Up t, Stay)
  | Screen.Action Screen.Page_down -> (page_detail `Down t, Stay)
  | Screen.Char _
  | Screen.Action
      ( Screen.Escape | Screen.Enter | Screen.Tab | Screen.Up | Screen.Down
      | Screen.Left | Screen.Right | Screen.Backspace | Screen.Other ) -> (
      match t.pending with
      | Some _ -> (
          match message with
          | Screen.Action Screen.Escape -> (t, Close)
          | Screen.Char _
          | Screen.Action
              ( Screen.Enter | Screen.Tab | Screen.Up | Screen.Down
              | Screen.Left | Screen.Right | Screen.Backspace | Screen.Page_up
              | Screen.Page_down | Screen.Other ) ->
              (t, Stay))
      | None -> (
          match t.editor with
          | Browsing -> update_browsing message t
          | Editing_objective _ | Editing_budget _ -> update_editor message t
          | Confirming_clear -> update_confirmation message t))

let detail_page_applied serial t =
  match t.detail_page with
  | Some (current, _) when Int.equal current serial ->
      { t with detail_page = None }
  | Some _ | None -> t

let update_input message t =
  match (t.pending, t.editor) with
  | Some _, _ -> (t, Stay)
  | None, Editing_objective input ->
      input_outcome `Objective (Inline_input.update message input) t
  | None, Editing_budget input ->
      input_outcome `Budget (Inline_input.update message input) t
  | None, (Browsing | Confirming_clear) -> (t, Stay)

let update_action ~activate index t =
  match t.pending with
  | Some _ -> (t, Stay)
  | None ->
      let t = select_index index t in
      if activate then activate_selected t else (t, Stay)

let update message t =
  match message with
  | Key message -> update_key message t
  | Detail_page_applied serial -> (detail_page_applied serial t, Stay)
  | Select_action index -> update_action ~activate:false index t
  | Activate_action index -> update_action ~activate:true index t
  | Input_msg message -> update_input message t

let replacement = Uchar.rep

let normalize_multiline text =
  let buffer = Buffer.create (String.length text) in
  let rec loop offset =
    if offset < String.length text then begin
      let decoded = String.get_utf_8_uchar text offset in
      let length = max 1 (Uchar.utf_decode_length decoded) in
      let uchar =
        if Uchar.utf_decode_is_valid decoded then Uchar.utf_decode_uchar decoded
        else replacement
      in
      let code = Uchar.to_int uchar in
      if code = 0x0A || code = 0x2028 || code = 0x2029 then
        Buffer.add_char buffer '\n'
      else if code = 0x09 then Buffer.add_char buffer ' '
      else if code = 0x0D then begin
        Buffer.add_char buffer '\n';
        let next = offset + length in
        if next < String.length text && Char.equal text.[next] '\n' then
          loop (next + 1)
        else loop next
      end
      else begin
        if code < 0x20 || (code >= 0x7F && code <= 0x9F) then
          Buffer.add_utf_8_uchar buffer replacement
        else Buffer.add_utf_8_uchar buffer uchar;
        loop (offset + length)
      end
    end
  in
  loop 0;
  Buffer.contents buffer

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

let zero_size = { width = px 0; height = px 0 }
let full_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = px 0 }
let vertical_gap = { width = px 0; height = px 1 }
let horizontal_gap = { width = px 2; height = px 0 }
let indent = padding_lrtb 2 2 0 0

let line ?(style = Ansi.Style.default) value =
  text ~style ~wrap:`Word ~truncate:false ~selectable:true ~flex_shrink:0.
    ~size:full_width
    (Prims.normalize_inline value)

let multiline ?(style = Ansi.Style.default) value =
  text ~style ~wrap:`Word ~truncate:false ~selectable:true ~flex_shrink:0.
    ~size:full_width
    (normalize_multiline value)

let atom ?(style = Ansi.Style.default) value =
  text ~style ~wrap:`None ~truncate:false ~selectable:true ~flex_shrink:0.
    (Prims.normalize_inline value)

let field ~palette label value =
  box ~flex_direction:Flex_direction.Row ~flex_wrap:Flex_wrap.Wrap
    ~align_items:Align.Flex_start ~gap:horizontal_gap ~flex_shrink:0.
    ~size:full_width
    [
      atom label;
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`Word ~truncate:false ~selectable:true ~flex_grow:1.
        ~flex_shrink:1. ~min_size:zero_size
        (Prims.normalize_inline value);
    ]

let status_label = function
  | Goal.Status.Active -> "active"
  | Goal.Status.Paused -> "paused"
  | Goal.Status.Blocked _ -> "blocked"
  | Goal.Status.Budget_limited -> "budget limited"
  | Goal.Status.Completed _ -> "completed"
  | Goal.Status.Cleared -> "cleared"

let status_style ~palette = function
  | Goal.Status.Active -> Theme.Palette.running_style palette
  | Goal.Status.Paused | Goal.Status.Blocked _ | Goal.Status.Budget_limited ->
      Theme.Palette.warning_style palette
  | Goal.Status.Completed _ -> Theme.Palette.success_style palette
  | Goal.Status.Cleared -> Theme.Palette.muted_style palette

let status_detail ~palette = function
  | Goal.Status.Blocked { reason = Some reason } ->
      [
        multiline
          ~style:(Theme.Palette.warning_style palette)
          ("blocked: " ^ reason);
      ]
  | Goal.Status.Completed { summary = Some summary } ->
      [
        multiline
          ~style:(Theme.Palette.success_style palette)
          ("completed: " ^ summary);
      ]
  | Goal.Status.Active | Goal.Status.Paused | Goal.Status.Budget_limited
  | Goal.Status.Blocked { reason = None }
  | Goal.Status.Completed { summary = None }
  | Goal.Status.Cleared ->
      []

let accounting ~palette goal =
  let turns = Goal.continuation_turns goal in
  let turn_label =
    if turns = 1 then "continuation turn" else "continuation turns"
  in
  let tokens = Goal.tokens_used goal in
  let budget =
    match Goal.token_budget goal with
    | None -> format_int tokens ^ " used · no token budget"
    | Some budget ->
        let remaining = Option.value (Goal.remaining_tokens goal) ~default:0 in
        Printf.sprintf "%s / %s used · %s remaining" (format_int tokens)
          (format_int budget) (format_int remaining)
  in
  [
    field ~palette "activity"
      (Printf.sprintf "%s %s" (format_int turns) turn_label);
    field ~palette "tokens" budget;
  ]

let goal_details ~palette goal =
  let status = Goal.status goal in
  [
    box ~flex_direction:Flex_direction.Row ~flex_wrap:Flex_wrap.Wrap
      ~gap:horizontal_gap ~flex_shrink:0. ~size:full_width
      [
        atom "status";
        atom ~style:(status_style ~palette status) (status_label status);
      ];
    field ~palette "goal id" (Goal.Id.to_string (Goal.id goal));
    line ~style:(Theme.Palette.muted_style palette) "objective";
    box ~padding:(padding_lrtb 2 0 0 0) ~flex_shrink:0. ~size:full_width
      [ multiline (Goal.objective goal) ];
  ]
  @ status_detail ~palette status
  @ accounting ~palette goal

let load_error ~palette heading diagnostic =
  [
    line ~style:(Theme.Palette.error_style palette) (Theme.problem ^ heading);
    multiline
      ~style:(Theme.Palette.error_style palette)
      (Mentat_diagnostic.to_string diagnostic);
  ]

let detail_rows ~palette = function
  | Loading ->
      [ line ~style:(Theme.Palette.muted_style palette) "⠋ loading goal…" ]
  | Unavailable diagnostic -> load_error ~palette "goal unavailable" diagnostic
  | Available { goal; refresh_error } ->
      let failure =
        match refresh_error with
        | None -> []
        | Some diagnostic ->
            load_error ~palette "could not refresh goal; showing retained state"
              diagnostic
      in
      let goal =
        match goal with
        | None ->
            [
              line
                ~style:(Theme.Palette.muted_style palette)
                "No goal is set for this session.";
              line
                ~style:(Theme.Palette.faint_style palette)
                "Declare one with /goal <objective> from the prompt — it rides \
                 your next turn. A model may also declare a goal while \
                 working.";
            ]
        | Some goal -> goal_details ~palette goal
      in
      failure @ goal

let detail_view ~palette t =
  let key = "goal.detail." ^ string_of_int t.detail_generation in
  let owns_focus = current_actions t = [] in
  scroll_box ~key ~scroll_x:false ~scroll_y:true ~show_scrollbars:true
    ~focusable:owns_focus ~autofocus:owns_focus ~flex_grow:1. ~flex_shrink:1.
    ~min_size:zero_size ~size:fill_remaining
    ?scroll_by:
      (Option.map
         (fun (serial, direction) ->
           {
             Mosaic.Scroll_box.key = key ^ ".page." ^ string_of_int serial;
             x = None;
             y = Some (match direction with `Up -> -1. | `Down -> 1.);
             unit = `Viewport;
           })
         t.detail_page)
    ?on_scroll_by_applied:
      (Option.map
         (fun (serial, _) ~key:_ -> Some (Detail_page_applied serial))
         t.detail_page)
    [
      box ~flex_direction:Flex_direction.Column ~gap:vertical_gap
        ~padding:indent ~flex_shrink:0. ~size:full_width
        (detail_rows ~palette t.projection);
    ]

let choice_label = function
  | Pause_choice -> "pause goal"
  | Edit_choice -> "edit objective"
  | Resume_choice -> "resume goal"
  | Clear_choice -> "clear goal"

let action_table_key event =
  let data = Event.Key.data event in
  match data.Matrix.Input.Key.key with
  | Matrix.Input.Key.Page_up | Matrix.Input.Key.Page_down ->
      Event.Key.prevent_default event;
      key data
  | _ -> None

let actions_view ~palette t =
  let available = current_actions t in
  let rows =
    List.mapi
      (fun index choice ->
        [|
          Table.cell
            (Printf.sprintf "%s%d. %s"
               (if Some choice = t.selected then Theme.cursor else "  ")
               (index + 1) (choice_label choice));
        |])
      available
  in
  table ~key:"goal.actions"
    ~columns:[ Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis "" ]
    ~rows
    ~selected_row:(selected_index t available)
    ~border:false ~show_header:false ~show_column_separator:false
    ~show_row_separator:false ~cell_padding:0
    ~text_color:(Theme.Palette.muted palette)
    ~background:Ansi.Color.default
    ~selected_text_color:(Theme.Palette.selection_fg palette)
    ~selected_background:(Theme.Palette.selection_bg palette)
    ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
    ~focused_selected_background:(Theme.Palette.selection_bg palette)
    ~wrap_selection:true ~focusable:true ~autofocus:true
    ~on_key:action_table_key ~flex_shrink:1. ~min_size:zero_size
    ~size:full_width ~activate_on_click:true
    ~on_change:(fun index -> Some (Select_action index))
    ~on_activate:(fun index -> Some (Activate_action index))
    ()

let issue_text = function
  | Empty_objective -> "enter a non-empty objective"
  | Unchanged_objective -> "the objective is unchanged"
  | Invalid_budget ->
      "enter a nonnegative whole-token budget, or leave it blank"

let issue_view ~palette = function
  | None -> []
  | Some issue ->
      [
        line
          ~style:(Theme.Palette.warning_style palette)
          (Theme.problem ^ issue_text issue);
      ]

let editor_view ~palette prompt input =
  box ~key:"goal.editor" ~flex_direction:Flex_direction.Column ~gap:vertical_gap
    ~flex_shrink:0. ~size:full_width
    [
      line ~style:(Theme.Palette.muted_style palette) prompt;
      Inline_input.view ~palette input
      |> Mosaic.map (fun message -> Input_msg message);
    ]

let command_label = function
  | Pause _ -> "pause goal"
  | Edit _ -> "edit objective"
  | Resume _ -> "resume goal"
  | Clear _ -> "clear goal"

let pending_view ~palette pending =
  let message =
    if pending.acknowledged then
      "✓ client accepted "
      ^ command_label pending.command
      ^ "; waiting for the goal fact…"
    else "⠋ requesting " ^ command_label pending.command ^ "…"
  in
  line
    ~style:
      (if pending.acknowledged then Theme.Palette.success_style palette
       else Theme.Palette.warning_style palette)
    message

let feedback_view ~palette = function
  | None -> []
  | Some (Rejected (command, diagnostic)) ->
      [
        scroll_box ~key:"goal.error" ~scroll_x:false ~scroll_y:true
          ~show_scrollbars:true ~focusable:false ~flex_shrink:1.
          ~min_size:zero_size
          ~max_size:{ width = pct 100; height = pct 40 }
          ~size:full_width
          [
            box ~flex_direction:Flex_direction.Column ~gap:vertical_gap
              ~flex_shrink:0. ~size:full_width
              [
                line
                  ~style:(Theme.Palette.error_style palette)
                  (Theme.problem ^ "could not " ^ command_label command);
                multiline
                  ~style:(Theme.Palette.error_style palette)
                  (Mentat_diagnostic.to_string diagnostic);
              ];
          ];
      ]

let controls ~palette t =
  let body =
    match t.pending with
    | Some pending -> [ pending_view ~palette pending ]
    | None -> (
        match t.editor with
        | Browsing ->
            if current_actions t = [] then [] else [ actions_view ~palette t ]
        | Editing_objective input ->
            editor_view ~palette "new objective" input
            :: issue_view ~palette t.issue
        | Editing_budget input ->
            editor_view ~palette "token budget (blank keeps the current budget)"
              input
            :: issue_view ~palette t.issue
        | Confirming_clear ->
            [
              line
                ~style:(Theme.Palette.error_style palette)
                "Permanently clear this goal?";
              line
                ~style:(Theme.Palette.warning_style palette)
                "Press Enter again to CLEAR. Escape cancels.";
            ])
  in
  feedback_view ~palette t.feedback @ body

let hints t =
  let paging =
    match t.projection with
    | Loading -> []
    | Unavailable _ | Available _ -> [ "pgup/pgdn details" ]
  in
  match t.pending with
  | Some _ -> paging @ [ "request in flight"; "esc back" ]
  | None -> (
      match t.editor with
      | Editing_objective _ ->
          [ "type objective"; "↵ save"; "esc cancel"; "paste works" ]
      | Editing_budget _ ->
          [ "type budget or leave blank"; "↵ resume"; "esc cancel" ]
      | Confirming_clear -> paging @ [ "↵ CLEAR"; "esc cancel" ]
      | Browsing ->
          if current_actions t = [] then paging @ [ "esc back" ]
          else paging @ [ "↑↓ select"; "1-3 choose"; "↵ action"; "esc back" ])

let fact = function
  | Loading -> "loading"
  | Unavailable _ -> "unavailable"
  | Available { goal = None; _ } -> "no goal"
  | Available { goal = Some goal; _ } -> status_label (Goal.status goal)

let view ~palette ~frame t =
  let content =
    box ~key:"goal.content" ~flex_direction:Flex_direction.Column
      ~gap:vertical_gap ~flex_grow:1. ~flex_shrink:1. ~min_size:zero_size
      ~size:fill_remaining
      (detail_view ~palette t :: controls ~palette t)
  in
  Screen.view ~palette ~frame ~name:"goal" ~fact:(fact t.projection)
    ~filter:None ~hint:(hints t) ~content
