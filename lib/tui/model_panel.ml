(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Provider = Mentat_llm.Provider
module Selector = Mentat_provider.Selector
module Model = Mentat_provider.Model
module Effort = Mentat_llm.Request.Options.Reasoning_effort
module Readiness = Mentat_provider.Model_readiness
module Route = Readiness.Route
module Entry = Readiness.Entry
module Eligibility = Readiness.Eligibility
module Actionability = Readiness.Actionability
module Account = Mentat_provider.Account
module Phase = Account.Phase
module Problem = Account.Problem
module Credential_error = Mentat_provider.Credential_error

let replacement = Uchar.rep

let add_inline_uchar buffer uchar =
  let code = Uchar.to_int uchar in
  match code with
  | 0x09 | 0x0A | 0x0D | 0x2028 | 0x2029 -> Buffer.add_char buffer ' '
  | code when code < 0x20 || (code >= 0x7F && code <= 0x9F) ->
      Buffer.add_utf_8_uchar buffer replacement
  | _ -> Buffer.add_utf_8_uchar buffer uchar

(* Snapshot labels, query diagnostics, and paste all cross a boundary before
   reaching Mosaic. Decode them once so malformed bytes and controls cannot
   emit terminal instructions or create extra lines. *)
let normalize_inline text =
  let buffer = Buffer.create (String.length text) in
  let rec loop offset =
    if offset < String.length text then begin
      let decoded = String.get_utf_8_uchar text offset in
      let length = max 1 (Uchar.utf_decode_length decoded) in
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

let nonblank text =
  let found = ref false in
  let rec loop offset =
    if (not !found) && offset < String.length text then begin
      let decoded = String.get_utf_8_uchar text offset in
      let length = max 1 (Uchar.utf_decode_length decoded) in
      if Uchar.utf_decode_is_valid decoded then
        found := not (is_white_space (Uchar.utf_decode_uchar decoded))
      else found := true;
      loop (offset + length)
    end
  in
  loop 0;
  !found

let drop_last_grapheme text =
  let last = ref None in
  Matrix.Text.iter_graphemes
    (fun ~offset ~len ->
      if offset + len <= String.length text then last := Some offset)
    text;
  match !last with None -> text | Some offset -> String.sub text 0 offset

let grapheme_count text =
  let count = ref 0 in
  Matrix.Text.iter_graphemes (fun ~offset:_ ~len:_ -> incr count) text;
  !count

let normalize_error message =
  let message = normalize_inline message in
  if nonblank message then message else "model catalog unavailable"

type readiness =
  | Loading
  | Unavailable of string
  | Available of {
      value : Readiness.t;
      refreshing : bool;
      refresh_error : string option;
    }

type selection_block =
  | Lifecycle of Model.status
  | Provider_blocked of Account.Discovery.t
  | Model_unavailable

type issue =
  | Invalid_selector of Selector.Error.t
  | Selection_blocked of { selector : Selector.t; reason : selection_block }
  | Refused of string
(* A shell-supplied refusal for a selection the panel parsed but the
         context cannot apply — for example an activation with no session to
         bind. Cleared by any further input like the other issues. *)

type t = {
  snapshot : Snapshot.t;
  readiness : readiness;
  input : string;
  selected : int;
  effort : Effort.t option;
      (* The live reasoning-effort override [←]/[→] adjusts. [None] follows the
         selected model's own default; a [Some] value is applied to a model only
         when the model supports it (see {!applied_effort}). *)
  issue : issue option;
}

type msg =
  | Key of Panel.key
  | Reload_key
  | Select_index of int
  | Activate_index of int

type event =
  | Stay
  | Close
  | Reload
  | Set_model of { selector : Selector.t; reasoning_effort : Effort.t option }

let loading snapshot =
  {
    snapshot;
    readiness = Loading;
    input = "";
    selected = 0;
    effort = None;
    issue = None;
  }

let current_selector snapshot =
  match Selector.of_string (Snapshot.model snapshot) with
  | Ok selector -> Some selector
  | Error _ -> None

let failed message t =
  let message = normalize_error message in
  let readiness =
    match t.readiness with
    | Available available ->
        Available
          { available with refreshing = false; refresh_error = Some message }
    | Loading | Unavailable _ -> Unavailable message
  in
  { t with readiness }

let reload_chord (event : Matrix.Input.Key.event) =
  let open Matrix.Input in
  event.Key.modifier.Modifier.ctrl
  &&
  match event.Key.key with
  | Key.Char uchar -> Uchar.equal uchar (Uchar.of_char 'r')
  | _ -> false

let key event =
  if reload_chord event then Some Reload_key
  else
    match Panel.classify event with
    | Panel.Action Panel.Other -> None
    | classified -> Some (Key classified)

let reload t =
  let readiness =
    match t.readiness with
    | Available available ->
        Available { available with refreshing = true; refresh_error = None }
    | Loading | Unavailable _ -> Loading
  in
  { t with readiness; issue = None }

let append text t =
  let text = normalize_inline text in
  { t with input = t.input ^ text; selected = 0; issue = None }

let paste = append

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  if needle_length = 0 then true
  else
    let rec loop offset =
      if offset + needle_length > haystack_length then false
      else if String.equal (String.sub haystack offset needle_length) needle
      then true
      else loop (offset + 1)
    in
    loop 0

let provider_label route =
  match Route.display_name route with
  | Some name -> normalize_inline name
  | None -> Provider.id (Route.provider route) |> normalize_inline

let model_label entry =
  let model = Entry.model entry in
  match Model.display_name model with
  | Some name -> normalize_inline name
  | None -> Model.id model |> normalize_inline

(* A structurally unavailable model — [Model.visible] false, meaning the
   provider API can never route it (a chat-only alias under a Responses route,
   a retired id) — is not a pick, so it never reaches the table. This is the
   static never-routable lifecycle signal, deliberately distinct from an
   actionable block (sign-in required) or a transient account gap (the checked
   account did not list the id): those stay visible and keep their status
   clause. *)
let entries readiness =
  Readiness.routes readiness
  |> List.concat_map (fun route ->
      Route.models route
      |> List.filter (fun entry -> Model.visible (Entry.model entry))
      |> List.map (fun entry -> (route, entry)))

let entry_matches ~input (route, entry) =
  let needle = String.trim input |> String.lowercase_ascii in
  String.is_empty needle
  ||
  let model = Entry.model entry in
  let fields =
    [
      Provider.id (Route.provider route);
      Option.value ~default:"" (Route.display_name route);
      Model.id model;
      Option.value ~default:"" (Model.display_name model);
      Selector.to_string (Model.selector model);
    ]
  in
  List.exists
    (fun field ->
      contains ~needle (normalize_inline field |> String.lowercase_ascii))
    fields

let matching_entries input readiness =
  entries readiness |> List.filter (entry_matches ~input)

let visible_rows t =
  match t.readiness with
  | Available { value; _ } -> matching_entries t.input value
  | Loading | Unavailable _ -> []

let selector_of_row (_, entry) = Model.selector (Entry.model entry)

let selected_selector t =
  List.nth_opt (visible_rows t) t.selected |> Option.map selector_of_row

let selected_model t =
  List.nth_opt (visible_rows t) t.selected
  |> Option.map (fun (_, entry) -> Entry.model entry)

(* The effort a pick persists for [model]: the live override when the model
   supports it, else [None] to follow the model's own default. Recording [None]
   rather than the default level keeps "follow the default" true across a later
   change to that default. *)
let applied_effort t model =
  match t.effort with
  | Some effort when List.mem effort (Model.supported_reasoning model) ->
      Some effort
  | Some _ | None -> None

(* The concrete level shown for [model]: the live override when supported, else
   the model default, else [Disabled] (no effort). *)
let effort_level t model =
  match applied_effort t model with
  | Some effort -> effort
  | None -> (
      match Model.default_reasoning model with
      | Some default -> default
      | None -> Effort.Disabled)

let index_of value values =
  let rec loop i = function
    | [] -> 0
    | x :: rest -> if x = value then i else loop (i + 1) rest
  in
  loop 0 values

(* [←]/[→] walk the selected row model's supported levels and wrap at the ends.
   Landing on the model default records [None] (follow-default) rather than an
   explicit pin, so the default level reads as such and never duplicates a
   separate row. A model that supports no effort is left unchanged. *)
let adjust delta t =
  match selected_model t with
  | None -> t
  | Some model -> (
      match Model.supported_reasoning model with
      | [] -> t
      | supported ->
          let count = List.length supported in
          let index = index_of (effort_level t model) supported in
          let index = Option_list.wrap ~count index delta in
          let level = List.nth supported index in
          let effort =
            if Option.equal ( = ) (Model.default_reasoning model) (Some level)
            then None
            else Some level
          in
          { t with effort })

let find_selector selector rows =
  List.find_index
    (fun row -> Selector.equal selector (selector_of_row row))
    rows

let preferred_selection t rows previous =
  let find =
    Option.bind previous (fun selector -> find_selector selector rows)
  in
  match find with
  | Some index -> index
  | None -> (
      match current_selector t.snapshot with
      | Some selector -> Option.value ~default:0 (find_selector selector rows)
      | None -> 0)

let loaded value t =
  let previous = selected_selector t in
  let readiness =
    Available { value; refreshing = false; refresh_error = None }
  in
  let issue =
    match t.issue with
    | Some (Invalid_selector _) as issue -> issue
    | Some (Selection_blocked _ | Refused _) | None -> None
  in
  let next = { t with readiness; issue } in
  let selected = preferred_selection next (visible_rows next) previous in
  { next with selected }

let select_index index t =
  let last = List.length (visible_rows t) - 1 in
  let selected = if last < 0 then 0 else max 0 (min last index) in
  { t with selected }

let refuse_selection message t = { t with issue = Some (Refused message) }

let submit t =
  match Selector.of_string t.input with
  | Ok selector ->
      ({ t with issue = None }, Set_model { selector; reasoning_effort = None })
  | Error issue -> ({ t with issue = Some (Invalid_selector issue) }, Stay)

let selection_block entry =
  match Entry.eligibility entry with
  | Eligibility.Ineligible status -> Some (Lifecycle status)
  | Eligibility.Eligible -> (
      match Entry.actionability entry with
      | Actionability.Actionable -> None
      | Actionability.Provider_blocked discovery ->
          Some (Provider_blocked discovery)
      | Actionability.Model_unavailable -> Some Model_unavailable)

let activate index t =
  let t = select_index index t in
  match List.nth_opt (visible_rows t) t.selected with
  | None -> submit t
  | Some ((_, entry) as row) -> (
      let selector = selector_of_row row in
      match selection_block entry with
      | None ->
          ( { t with issue = None },
            Set_model
              {
                selector;
                reasoning_effort = applied_effort t (Entry.model entry);
              } )
      | Some reason ->
          ( { t with issue = Some (Selection_blocked { selector; reason }) },
            Stay ))

let update message t =
  match message with
  | Select_index index -> (select_index index t, Stay)
  | Activate_index index -> activate index t
  | Reload_key -> (reload t, Reload)
  | Key (Panel.Action Panel.Escape) -> (t, Close)
  | Key (Panel.Action Panel.Enter) ->
      if List.is_empty (visible_rows t) then submit t else activate t.selected t
  | Key (Panel.Action Panel.Backspace) ->
      ( { t with input = drop_last_grapheme t.input; selected = 0; issue = None },
        Stay )
  | Key (Panel.Printable text) -> (append text t, Stay)
  | Key (Panel.Digit digit) -> (append (string_of_int digit) t, Stay)
  | Key (Panel.Action Panel.Left) -> (adjust (-1) t, Stay)
  | Key (Panel.Action Panel.Right) -> (adjust 1 t, Stay)
  | Key
      (Panel.Action
         (Panel.Tab | Panel.Up | Panel.Down | Panel.Ctrl_d | Panel.Other)) ->
      (t, Stay)

let problem_label problem = Problem.to_string problem |> normalize_inline

let blocked_provider_status = function
  | Account.Discovery.Resolution_failed { error; _ } ->
      "credential · " ^ (Credential_error.message error |> normalize_inline)
  | Account.Discovery.Known account -> (
      match Account.phase account with
      | Phase.Missing -> "sign-in required"
      | Phase.Blocked -> (
          match Account.problems account with
          | [] -> "provider blocked"
          | problems ->
              "blocked · "
              ^ String.concat ", " (List.map problem_label problems))
      | phase -> "provider blocked · " ^ Phase.to_string phase)

(* The single quiet status clause a table row carries: [None] when the row is
   ready to pick — the row is its own affordance — else one muted phrase naming
   the blocker. It folds the three readiness facets (lifecycle eligibility,
   checked availability, provider-route actionability) that once filled three
   loud columns into one, so the list reads as a picker, not a status grid. *)
let row_status entry =
  match Entry.eligibility entry with
  | Eligibility.Ineligible Model.Deprecated -> Some "deprecated"
  | Eligibility.Ineligible (Model.Unavailable reason) ->
      Some ("unavailable · " ^ normalize_inline reason)
  | Eligibility.Ineligible (Model.Stable | Model.Preview) -> Some "not eligible"
  | Eligibility.Eligible -> (
      match Entry.actionability entry with
      | Actionability.Actionable -> (
          match Model.status (Entry.model entry) with
          | Model.Preview -> Some "preview"
          | Model.Stable | Model.Deprecated | Model.Unavailable _ -> None)
      | Actionability.Model_unavailable -> Some "unavailable"
      | Actionability.Provider_blocked discovery ->
          Some (blocked_provider_status discovery))

(* The one catalog number worth scanning while choosing a model: its context
   window, compact and right-aligned ([1.05M], [400K], smaller exact). It is the
   single spec kept from the old status grid because it changes the pick, and it
   reads muted beside the name rather than competing with it. Empty when the
   owner declares no window. *)
let context_window_label model =
  match Model.context_window model with
  | None -> ""
  | Some tokens when tokens >= 1_000_000 ->
      Printf.sprintf "%gM" (Float.of_int tokens /. 1_000_000.)
  | Some tokens when tokens >= 1_000 ->
      Printf.sprintf "%gK" (Float.of_int tokens /. 1_000.)
  | Some tokens -> string_of_int tokens

let minimum_size = { width = px 0; height = px 0 }
let full_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = px 0 }

let prose ?key ?(style = Ansi.Style.default) ?(wrap = `Word) value =
  text ?key ~style ~wrap ~truncate:false ~selectable:false ~flex_shrink:0.
    ~min_size:minimum_size ~size:full_width value

(* A stack of panel blocks, always one row apart. The separation is structural
   rather than air: a block the panel has squeezed below a whole row is still
   drawn on one, and a neighbour placed flush against it then starts on that same
   row, so the two texts interleave. One row of clearance makes that impossible,
   and it costs the panel a row it can afford to lose. *)
let content_column ?key ?(grow = true) children =
  box ?key ~flex_direction:Flex_direction.Column
    ~flex_grow:(if grow then 1. else 0.)
    ~flex_shrink:1. ~min_size:minimum_size
    ~size:(if grow then fill_remaining else full_width)
    ~gap:{ width = px 0; height = px 1 }
    children

(* A diagnostic fills the room the panel leaves it. Its basis is zero rather
   than the panel's full height, so it takes only what is left over instead of
   claiming the rows its neighbouring blocks stand on. *)
let diagnostic_scroll ~key ~style message =
  scroll_box ~key ~scroll_x:false ~scroll_y:true ~focusable:false ~flex_grow:1.
    ~flex_shrink:1. ~min_size:minimum_size ~size:fill_remaining
    [ content_column ~grow:false [ prose ~style message ] ]

let current_line ~palette snapshot =
  let summary = Snapshot.model_line snapshot in
  box ~key:"model.current" ~flex_direction:Flex_direction.Row ~flex_shrink:0.
    ~min_size:minimum_size ~size:full_width
    ~gap:{ width = px 2; height = px 0 }
    [
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`None ~truncate:true ~flex_shrink:0. "Current";
      text ~wrap:`None ~truncate:true ~flex_grow:1. ~flex_shrink:1.
        ~min_size:minimum_size summary;
    ]

(* The reasoning-effort intensity ramp: ○ minimal/low, ◐ medium, ● high,
   ◉ extra-high/max. Disabled reads as the low glyph — the level word carries
   the distinction. *)
let effort_low = "○"
let effort_medium = "◐"
let effort_high = "●"
let effort_max = "◉"

let effort_glyph = function
  | Effort.Disabled | Effort.Minimal | Effort.Low -> effort_low
  | Effort.Medium -> effort_medium
  | Effort.High -> effort_high
  | Effort.Extra_high | Effort.Max -> effort_max

let effort_word = function
  | Effort.Disabled -> "No"
  | Effort.Minimal -> "Minimal"
  | Effort.Low -> "Low"
  | Effort.Medium -> "Medium"
  | Effort.High -> "High"
  | Effort.Extra_high -> "Extra high"
  | Effort.Max -> "Max"

let top_tier = function
  | Effort.Extra_high | Effort.Max -> true
  | Effort.Disabled | Effort.Minimal | Effort.Low | Effort.Medium | Effort.High
    ->
      false

(* The inline effort line under the table, for the selected row's model: the
   intensity glyph, the level word, a [(default)] tag, then either an adjust
   affordance or a top-tier caution. A model that supports no effort states so
   instead of offering the control. *)
let effort_line ~palette t model =
  match Model.supported_reasoning model with
  | [] ->
      box ~key:"model.effort" ~flex_direction:Flex_direction.Row ~flex_shrink:0.
        ~min_size:minimum_size ~size:full_width
        [
          text
            ~style:(Theme.Palette.muted_style palette)
            ~wrap:`None ~truncate:true ~flex_shrink:1. ~min_size:minimum_size
            (effort_low ^ " Reasoning effort is not adjustable for this model");
        ]
  | _ ->
      let level = effort_level t model in
      let is_default =
        Option.equal ( = ) (Model.default_reasoning model) (Some level)
      in
      let glyph = effort_glyph level in
      let glyph_style =
        if String.equal glyph effort_low then Theme.Palette.muted_style palette
        else Theme.Palette.accent_style palette
      in
      let label =
        effort_word level ^ " effort" ^ if is_default then " (default)" else ""
      in
      let tail_style, tail =
        if top_tier level then
          (Theme.Palette.warning_style palette, "⚠ highest tier")
        else (Theme.Palette.faint_style palette, "← → adjust")
      in
      box ~key:"model.effort" ~flex_direction:Flex_direction.Row ~flex_shrink:0.
        ~min_size:minimum_size ~size:full_width
        ~gap:{ width = px 1; height = px 0 }
        [
          text ~style:glyph_style ~wrap:`None ~flex_shrink:0. glyph;
          text ~wrap:`None ~truncate:true ~flex_shrink:1. ~min_size:minimum_size
            label;
          text ~style:tail_style ~wrap:`None ~truncate:true ~flex_shrink:1.
            ~min_size:minimum_size tail;
          box ~flex_grow:1. ~flex_shrink:1. ~min_size:minimum_size [];
        ]

(* The picker anatomy: a selection cursor, the provider shown once per
   contiguous run (standing in for a group
   header, which the flat table cannot intersperse), the model name, one muted
   status clause, and the current model's trailing ✓. Selection is marker and
   accent only — never a fill — routed through {!Theme.selection_fg}/[_bg]. A
   blocked or locked row is muted whole so the eye reads ready models as the
   bright, pickable ones. *)
let readiness_table ~palette t rows =
  let current = current_selector t.snapshot in
  let selected =
    if List.is_empty rows then 0 else min t.selected (List.length rows - 1)
  in
  let columns =
    [
      Table.column ~width:`Auto ~overflow:`Crop ~min_width:1 "";
      Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis ~min_width:1 "provider";
      Table.column ~width:(`Flex 1.5) ~overflow:`Ellipsis ~min_width:1 "model";
      Table.column ~width:(`Flex 1.5) ~overflow:`Ellipsis ~min_width:1 "status";
      Table.column ~width:`Auto ~alignment:`Right ~overflow:`Crop ~min_width:1
        "context";
      Table.column ~width:`Auto ~alignment:`Right ~overflow:`Crop "";
    ]
  in
  let _, table_rows =
    List.fold_left
      (fun (previous_provider, acc) (route, entry) ->
        let index = List.length acc in
        let selector = selector_of_row (route, entry) in
        let is_current =
          Option.fold ~none:false ~some:(Selector.equal selector) current
        in
        let is_blocked = Option.is_some (selection_block entry) in
        let provider = provider_label route in
        let provider_cell =
          if Option.equal String.equal previous_provider (Some provider) then ""
          else provider
        in
        let model_style =
          if is_blocked then Theme.Palette.muted_style palette
          else Ansi.Style.default
        in
        let cells =
          [|
            Table.cell
              ~style:(Theme.Palette.accent_style palette)
              (if index = selected then "❯" else "");
            Table.cell ~style:(Theme.Palette.muted_style palette) provider_cell;
            (* A pickable name stays unstyled so the table's selected-row
               foreground can take it; only blocked rows pin the muted style. *)
            (if is_blocked then
               Table.cell ~style:model_style (model_label entry)
             else Table.cell (model_label entry));
            Table.cell
              ~style:(Theme.Palette.muted_style palette)
              (Option.value ~default:"" (row_status entry));
            Table.cell
              ~style:(Theme.Palette.muted_style palette)
              (context_window_label (Entry.model entry));
            Table.cell
              ~style:(Theme.Palette.success_style palette)
              (if is_current then "✓" else "");
          |]
        in
        (Some provider, cells :: acc))
      (None, []) rows
  in
  let table_rows = List.rev table_rows in
  table ~key:"model.catalog.table" ~columns ~rows:table_rows
    ~selected_row:selected ~border:false ~show_header:false
    ~show_column_separator:false ~show_row_separator:false ~cell_padding:1
    ~background:Ansi.Color.default ~text_color:Ansi.Color.default
    ~selected_background:(Theme.Palette.selection_bg palette)
    ~selected_text_color:(Theme.Palette.selection_fg palette)
    ~focused_selected_background:(Theme.Palette.selection_bg palette)
    ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
    ~wrap_selection:true ~show_scroll_indicator:true ~focusable:true
    ~autofocus:true ~flex_grow:1. ~flex_shrink:1. ~min_size:minimum_size
    ~size:fill_remaining ~activate_on_click:true
    ~on_change:(fun index -> Some (Select_index index))
    ~on_activate:(fun index -> Some (Activate_index index))
    ()

(* The catalog's share of the panel, as a flat run of blocks in the panel's own
   stack rather than a column nested inside it. A nested column is what a short
   panel squeezes: it keeps the height its own lines need while shrinking to
   less, and those lines are then drawn below it, over the set-by-name field.
   Flat, the panel shortens the run instead, and every block keeps its row. *)
let readiness_blocks ~palette t =
  match t.readiness with
  | Loading ->
      [
        diagnostic_scroll ~key:"model.catalog.loading"
          ~style:(Theme.Palette.muted_style palette)
          "⠋ loading the live model catalog… You can set a model by name below.";
      ]
  | Unavailable message ->
      [
        diagnostic_scroll ~key:"model.catalog.unavailable"
          ~style:(Theme.Palette.error_style palette)
          (Theme.problem ^ message ^ " · set by name or press ctrl+r to retry");
      ]
  | Available { value; refreshing; refresh_error } ->
      let rows = matching_entries t.input value in
      let route_count = List.length (Readiness.routes value) in
      let model_count = List.length (entries value) in
      let visible_count = List.length rows in
      let summary =
        if String.is_empty t.input then
          Printf.sprintf "%d providers · %d models" route_count model_count
        else Printf.sprintf "%d of %d models" visible_count model_count
      in
      let summary = prose ~style:(Theme.Palette.muted_style palette) summary in
      let refreshing =
        if refreshing then
          [
            prose
              ~style:(Theme.Palette.muted_style palette)
              "⠋ refreshing the live model catalog…";
          ]
        else []
      in
      if List.is_empty rows then
        let message =
          if String.is_empty t.input then
            "No models are currently reported by the catalog."
          else "No model matches the filter."
        in
        (summary :: refreshing)
        @ [
            diagnostic_scroll ~key:"model.catalog.empty"
              ~style:(Theme.Palette.muted_style palette)
              message;
          ]
      else
        let failure =
          match refresh_error with
          | None -> []
          | Some message ->
              [
                scroll_box ~key:"model.catalog.refresh-error" ~scroll_x:false
                  ~scroll_y:true ~focusable:false ~flex_shrink:1.
                  ~min_size:minimum_size
                  ~max_size:{ width = pct 100; height = pct 30 }
                  ~size:full_width
                  [
                    prose
                      ~style:(Theme.Palette.error_style palette)
                      (Theme.problem ^ message);
                  ];
              ]
        in
        let effort =
          match List.nth_opt rows (min t.selected (List.length rows - 1)) with
          | Some (_, entry) -> [ effort_line ~palette t (Entry.model entry) ]
          | None -> []
        in
        (summary :: refreshing) @ failure
        @ [ readiness_table ~palette t rows ]
        @ effort

let selector_input ~palette t =
  let value = normalize_inline t.input in
  content_column ~key:"model.selector" ~grow:false
    [
      prose ~style:(Theme.Palette.muted_style palette) "Filter or set by name";
      box ~flex_direction:Flex_direction.Row ~flex_shrink:0.
        ~min_size:minimum_size
        ~size:{ width = pct 100; height = px 1 }
        [
          text
            ~style:(Theme.Palette.accent_style palette)
            ~wrap:`None ~flex_shrink:0. Theme.cursor;
          Mosaic.input ~key:"model.selector.input" ~value
            ~cursor:(grapheme_count value) ~placeholder:"provider/model"
            ~text_color:Ansi.Color.default ~background_color:Ansi.Color.default
            ~placeholder_color:(Theme.Palette.faint palette)
            ~focusable:false ~show_cursor:false ~selectable:false ~flex_grow:1.
            ~flex_shrink:1. ~min_size:minimum_size
            ~size:{ width = pct 100; height = px 1 }
            ();
        ];
    ]

let selection_issue_message selector = function
  | Lifecycle Model.Deprecated ->
      Selector.to_string selector ^ " is deprecated and cannot be selected"
  | Lifecycle (Model.Unavailable reason) ->
      Selector.to_string selector
      ^ " is unavailable · " ^ normalize_inline reason
  | Lifecycle (Model.Stable | Model.Preview) ->
      Selector.to_string selector ^ " is not eligible for selection"
  | Provider_blocked discovery ->
      Selector.to_string selector
      ^ " cannot be selected · "
      ^ blocked_provider_status discovery
  | Model_unavailable ->
      Selector.to_string selector ^ " is not available to this account"

let issue_message = function
  | Invalid_selector issue -> Selector.Error.message issue |> normalize_error
  | Selection_blocked { selector; reason } ->
      selection_issue_message selector reason
  | Refused message -> normalize_error message

let issue_content ~palette = function
  | None -> []
  | Some issue ->
      [
        scroll_box ~key:"model.selector.issue" ~scroll_x:false ~scroll_y:true
          ~focusable:false ~flex_shrink:1. ~min_size:minimum_size
          ~max_size:{ width = pct 100; height = pct 25 }
          ~size:full_width
          [
            prose
              ~style:(Theme.Palette.error_style palette)
              (issue_message issue);
          ];
      ]

(* Below [compact_rows] screen rows the panel drops the Current line — the
   footer carries the live model and effort — so the picker table keeps enough
   rows to browse. The panel's share of a 32-row screen otherwise leaves the
   flex table a single scrolling row. *)
let compact_rows = 36

let content ~palette ~rows t =
  let compact = rows < compact_rows in
  let current = if compact then [] else [ current_line ~palette t.snapshot ] in
  content_column ~key:"model.content"
    (current
    @ readiness_blocks ~palette t
    @ issue_content ~palette t.issue
    @ [ selector_input ~palette t ])

let hints t =
  (if List.is_empty (visible_rows t) then []
   else [ "↑↓/pgup/pgdn browse"; "←→ effort" ])
  @ [
      "↵ choose/set";
      "ctrl+r refresh";
      "type filter or provider/model";
      "esc close";
    ]

let view ~palette ~frame ~rows t =
  Panel.view ~palette ~frame ~name:"model" ~filter:(normalize_inline t.input)
    ~hint:(hints t) ~content:(content ~palette ~rows t)
