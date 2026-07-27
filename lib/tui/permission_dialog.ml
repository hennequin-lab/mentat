(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Access = Mentat_permission.Access
module Request = Mentat_permission.Request
module Review = Mentat_permission.Policy.Review
module Answer = Mentat_permission.Answer

type t = {
  review : Review.t;
  nav : Option_list.t;
  expanded : bool;
  guidance : Inline_input.t option;
}

type outcome = Stay | Answer of { answer : Answer.t; message : string option }
type line_breaks = Preserve | Flatten

type evidence =
  | Fixed of { indent : int; style : Ansi.Style.t; text : string }
  | Prose of { indent : int; style : Ansi.Style.t; text : string }
  | Diff of string

let option_count = 4
let replacement = Uchar.rep

let add_line_break ~line_breaks buffer =
  Buffer.add_string buffer
    (match line_breaks with Preserve -> "\n" | Flatten -> " ")

let add_uchar ~line_breaks buffer uchar =
  let code = Uchar.to_int uchar in
  if code = 0x09 then Buffer.add_string buffer "  "
  else if code = 0x0a || code = 0x0d || code = 0x2028 || code = 0x2029 then
    add_line_break ~line_breaks buffer
  else if code < 0x20 || (code >= 0x7f && code <= 0x9f) then
    Buffer.add_utf_8_uchar buffer replacement
  else Buffer.add_utf_8_uchar buffer uchar

let normalize ~line_breaks value =
  let buffer = Buffer.create (String.length value) in
  let rec loop offset =
    if offset < String.length value then
      match value.[offset] with
      | '\r' ->
          let next = offset + 1 in
          let next =
            if next < String.length value && Char.equal value.[next] '\n' then
              next + 1
            else next
          in
          add_line_break ~line_breaks buffer;
          loop next
      | '\n' ->
          add_line_break ~line_breaks buffer;
          loop (offset + 1)
      | _ ->
          let decoded = String.get_utf_8_uchar value offset in
          let length = max 1 (Uchar.utf_decode_length decoded) in
          let uchar =
            if Uchar.utf_decode_is_valid decoded then
              Uchar.utf_decode_uchar decoded
            else replacement
          in
          add_uchar ~line_breaks buffer uchar;
          loop (offset + length)
  in
  loop 0;
  Buffer.contents buffer

let normalize_lines = normalize ~line_breaks:Preserve

let normalize_one_line value =
  normalize ~line_breaks:Flatten value |> String.trim

let is_white_space uchar =
  match Uchar.to_int uchar with
  | 0x09 | 0x0a | 0x0b | 0x0c | 0x0d | 0x20 | 0x85 | 0xa0 | 0x1680 | 0x2028
  | 0x2029 | 0x202f | 0x205f | 0x3000 ->
      true
  | code -> code >= 0x2000 && code <= 0x200a

let without_white_space grapheme =
  let buffer = Buffer.create (String.length grapheme) in
  let rec loop offset =
    if offset < String.length grapheme then begin
      let decoded = String.get_utf_8_uchar grapheme offset in
      let length = max 1 (Uchar.utf_decode_length decoded) in
      let uchar =
        if Uchar.utf_decode_is_valid decoded then Uchar.utf_decode_uchar decoded
        else replacement
      in
      if not (is_white_space uchar) then Buffer.add_utf_8_uchar buffer uchar;
      loop (offset + length)
    end
  in
  loop 0;
  Buffer.contents buffer

let has_visible_text value =
  let visible = ref false in
  (try
     Matrix.Text.iter_graphemes
       (fun ~offset ~len ->
         let grapheme = String.sub value offset len |> without_white_space in
         Matrix.Text.iter_grapheme_info ~width_method:`Unicode ~tab_width:2
           (fun ~offset:_ ~len:_ ~width ->
             if width > 0 then begin
               visible := true;
               raise Exit
             end)
           grapheme)
       value
   with Exit -> ());
  !visible

let visible_or ~fallback value =
  if has_visible_text value then value else fallback

let one_line_or ~fallback value =
  normalize_one_line value |> visible_or ~fallback

let lines_or ~fallback value = normalize_lines value |> visible_or ~fallback

let make review =
  {
    review;
    nav = Option_list.make ~count:option_count;
    expanded = false;
    guidance = None;
  }

type msg =
  | Select_choice of int
  | Activate_choice of int
  | Input_msg of Inline_input.msg

let bare_letter (event : Matrix.Input.Key.event) expected =
  let modifier = event.Matrix.Input.Key.modifier in
  (not modifier.Matrix.Input.Modifier.ctrl)
  && (not modifier.Matrix.Input.Modifier.alt)
  && (not modifier.Matrix.Input.Modifier.super)
  &&
  match event.Matrix.Input.Key.key with
  | Matrix.Input.Key.Char uchar ->
      let code = Uchar.to_int uchar in
      let code = if code >= 0x41 && code <= 0x5a then code + 0x20 else code in
      code = Char.code expected
  | _ -> false

let ctrl_o (event : Matrix.Input.Key.event) =
  let modifier = event.Matrix.Input.Key.modifier in
  modifier.Matrix.Input.Modifier.ctrl
  && (not modifier.Matrix.Input.Modifier.alt)
  && (not modifier.Matrix.Input.Modifier.super)
  &&
  match event.Matrix.Input.Key.key with
  | Matrix.Input.Key.Char uchar ->
      let code = Uchar.to_int uchar in
      code = Char.code 'o' || code = Char.code 'O'
  | _ -> false

let deny_answer = Answer { answer = Answer.deny; message = None }

let selected_answer t =
  match Option_list.selected t.nav with
  | 0 -> Answer.once
  | 1 -> Answer.exact_for_conversation
  | 2 -> Answer.deny
  | selected ->
      invalid_arg
        (Printf.sprintf
           "Mentat_tui.Permission_dialog: invalid selected option %d" selected)

let open_guidance t = ({ t with guidance = Some Inline_input.empty }, Stay)

(* Option 4 opens the free-text editor; every other option answers immediately.
   Index 3 never reaches [selected_answer], so its [Invalid_argument] edge is
   unreachable. *)
let confirm t =
  if Option_list.selected t.nav = 3 then open_guidance t
  else (t, Answer { answer = selected_answer t; message = None })

let input_outcome t = function
  | Inline_input.Stay input -> ({ t with guidance = Some input }, Stay)
  | Inline_input.Cancel -> ({ t with guidance = None }, Stay)
  | Inline_input.Submit text when not (has_visible_text text) ->
      (* Blank guidance is a valid bare deny, so an empty note answers rather
         than flashing — the one deliberate divergence from plan/question. *)
      ({ t with guidance = None }, deny_answer)
  | Inline_input.Submit text ->
      ( { t with guidance = None },
        Answer { answer = Answer.deny; message = Some text } )

let edit_key event input t = input_outcome t (Inline_input.key event input)

let select_choice index t =
  if index < 0 || index >= option_count then t
  else { t with nav = Option_list.jump (index + 1) t.nav }

let key event t =
  match t.guidance with
  | Some input -> edit_key event input t
  | None -> (
      if ctrl_o event then ({ t with expanded = not t.expanded }, Stay)
      else if bare_letter event 'y' then
        (t, Answer { answer = Answer.once; message = None })
      else if bare_letter event 'a' then
        (t, Answer { answer = Answer.exact_for_conversation; message = None })
      else if bare_letter event 'd' || bare_letter event 'n' then
        (t, deny_answer)
      else
        match Panel.classify event with
        | Panel.Digit digit ->
            ({ t with nav = Option_list.jump digit t.nav }, Stay)
        | Panel.Printable _ -> (t, Stay)
        | Panel.Action Panel.Up -> ({ t with nav = Option_list.up t.nav }, Stay)
        | Panel.Action Panel.Down ->
            ({ t with nav = Option_list.down t.nav }, Stay)
        | Panel.Action Panel.Enter -> confirm t
        | Panel.Action Panel.Escape -> (t, deny_answer)
        | Panel.Action
            ( Panel.Tab | Panel.Left | Panel.Right | Panel.Backspace
            | Panel.Ctrl_d | Panel.Other ) ->
            (t, Stay))

let editing t = Option.is_some t.guidance

let update message t =
  match message with
  | Select_choice index -> (select_choice index t, Stay)
  | Activate_choice index ->
      let t = select_choice index t in
      confirm t
  | Input_msg message -> (
      match t.guidance with
      | None -> (t, Stay)
      | Some input -> input_outcome t (Inline_input.update message input))

let path_op = function
  | `Read -> "read"
  | `Create -> "create"
  | `Modify -> "modify"
  | `Delete -> "delete"

let path_action = function
  | `Read -> "Read"
  | `Create -> "Create"
  | `Modify -> "Edit"
  | `Delete -> "Delete"

let path_scope scope =
  Access.Path_scope.to_string scope |> one_line_or ~fallback:"[blank path]"

let protocol = function
  | `Http -> "http"
  | `Https -> "https"
  | `Ssh -> "ssh"
  | `Tcp -> "tcp"
  | `Udp -> "udp"
  | `Other name -> one_line_or ~fallback:"[blank protocol]" name

let host value = one_line_or ~fallback:"[blank host]" value
let command_text value = one_line_or ~fallback:"[blank command]" value
let program value = one_line_or ~fallback:"[blank program]" value
let language value = one_line_or ~fallback:"[blank language]" value
let code_source value = lines_or ~fallback:"[blank code]" value
let custom_name value = one_line_or ~fallback:"[blank custom access]" value
let subject value = one_line_or ~fallback:"[blank subject]" value
let source value = one_line_or ~fallback:"[blank source]" value
let item_display value = lines_or ~fallback:"[blank item label]" value
let request_display value = lines_or ~fallback:"[blank action]" value

let command_cwd = function
  | Access.Command.Shell { cwd; _ }
  | Access.Command.Argv { cwd; _ }
  | Access.Command.Code { cwd; _ } ->
      path_scope cwd

let command_execution command =
  Access.Command.execution command
  |> Access.Command.execution_to_string
  |> one_line_or ~fallback:"[blank execution route]"

let shell_word value = Filename.quote (normalize_one_line value)

let command_inline = function
  | Access.Command.Shell { text; _ } -> command_text text
  | Access.Command.Argv { program = executable; args; _ } ->
      String.concat " "
        (shell_word (program executable) :: List.map shell_word args)
  | Access.Command.Code { language = name; _ } ->
      "evaluate " ^ language name ^ " code"

let network_target ~protocol:scheme ~host:name ~port =
  protocol scheme ^ "://" ^ host name
  ^ Option.fold ~none:"" ~some:(fun number -> ":" ^ string_of_int number) port

let access_inline ?(omit_command = false) = function
  | Access.Path { op; scope } -> path_op op ^ " " ^ path_scope scope
  | Access.Command command ->
      if omit_command then
        match command with
        | Access.Command.Code _ -> "code evaluation"
        | Access.Command.Shell _ | Access.Command.Argv _ -> "command"
      else "command " ^ command_inline command
  | Access.Network { protocol = scheme; host = name; port } ->
      "network " ^ network_target ~protocol:scheme ~host:name ~port
  (* The two sandbox widenings are spelled out rather than shown as their
     identifiers. A reviewer approving `shell.escalate` is being asked for the
     whole posture, and the identifier says none of that; what they need is what
     changes and what does not. Every other custom access keeps its raw name —
     the host that minted it knows what it means and this dialog does not. *)
  | Access.Custom { name = "shell.escalate"; _ } ->
      "run outside the sandbox — reads, writes and network open, except \
       Mentat's own directories"
  | Access.Custom { name = "shell.grant"; subject = detail } ->
      "make writable for this command only"
      ^ Option.fold ~none:"" ~some:(fun value -> ": " ^ subject value) detail
  | Access.Custom { name; subject = detail } ->
      "custom " ^ custom_name name
      ^ Option.fold ~none:""
          ~some:(fun value -> " " ^ shell_word (subject value))
          detail

let is_path = function
  | Access.Path _ -> true
  | Access.Command _ | Access.Network _ | Access.Custom _ -> false

let is_command = function
  | Access.Command _ -> true
  | Access.Path _ | Access.Network _ | Access.Custom _ -> false

let is_code = function
  | Access.Command (Access.Command.Code _) -> true
  | Access.Command (Access.Command.Shell _ | Access.Command.Argv _)
  | Access.Path _ | Access.Network _ | Access.Custom _ ->
      false

let headline request accesses changes =
  let has_action = Option.is_some (Request.display request) in
  match accesses with
  | [ Access.Command _ ] when has_action && List.exists is_code accesses ->
      "Evaluate this code?"
  | [ Access.Command _ ] -> "Run this command?"
  | _ :: _ :: _ when List.for_all is_command accesses -> "Run these commands?"
  | [ Access.Path { op; scope } ] when List.is_empty changes ->
      path_action op ^ " " ^ path_scope scope ^ "?"
  | [ Access.Path _ ] -> "Apply this change?"
  | _ :: _ :: _ when List.for_all is_path accesses -> "Apply these changes?"
  | [ Access.Network { protocol = scheme; host = name; port } ] ->
      "Connect to " ^ network_target ~protocol:scheme ~host:name ~port ^ "?"
  | [ Access.Custom { name; _ } ] -> "Allow " ^ custom_name name ^ "?"
  | _ -> "Allow these accesses?"

let exact_scope = function
  | [ Access.Path { op; scope } ] ->
      let prefix =
        match op with
        | `Read -> "reads of "
        | `Create | `Modify -> "edits to "
        | `Delete -> "deletes of "
      in
      prefix ^ path_scope scope
  | [ Access.Command _ ] -> "this exact command"
  | [ Access.Network { protocol = scheme; host = name; port } ] ->
      "connections to " ^ network_target ~protocol:scheme ~host:name ~port
  | [ Access.Custom { name; subject = detail } ] ->
      "the exact " ^ custom_name name
      ^ Option.fold ~none:" access"
          ~some:(fun value -> " access to " ^ subject value)
          detail
  | _ -> "these exact accesses"

let allow_once_label = function
  | [ Access.Command _ ] -> "run once"
  | [ Access.Path { op = `Create | `Modify | `Delete; _ } ] ->
      "apply this change"
  | accesses when List.length accesses > 1 && List.for_all is_path accesses ->
      "apply these changes"
  | [ Access.Network _ ] -> "connect once"
  | _ -> "allow once"

let request_action_matches_access request access =
  match (Request.display request, access) with
  | Some display, Access.Command (Access.Command.Shell { text; _ }) ->
      String.equal (normalize_one_line display) (command_text text)
  | Some display, Access.Command (Access.Command.Argv _ as command) ->
      String.equal (normalize_one_line display) (command_inline command)
  | Some display, Access.Command (Access.Command.Code { source; _ }) ->
      String.equal (normalize_lines display) (code_source source)
  | None, _ | Some _, (Access.Path _ | Access.Network _ | Access.Custom _) ->
      false

let add_decimal left right =
  let left_length = String.length left in
  let right_length = String.length right in
  let length = max left_length right_length in
  let result = Bytes.make (length + 1) '0' in
  let carry = ref 0 in
  for offset = 0 to length - 1 do
    let digit value value_length =
      let index = value_length - offset - 1 in
      if index < 0 then 0 else Char.code value.[index] - Char.code '0'
    in
    let sum = digit left left_length + digit right right_length + !carry in
    Bytes.set result (length - offset) (Char.chr (Char.code '0' + (sum mod 10)));
    carry := sum / 10
  done;
  Bytes.set result 0 (Char.chr (Char.code '0' + !carry));
  if !carry = 0 then Bytes.sub_string result 1 length
  else Bytes.to_string result

let exact_total field changes =
  let rec loop total = function
    | [] -> Some total
    | change :: rest -> (
        match field change with
        | None -> None
        | Some count -> loop (add_decimal total (string_of_int count)) rest)
  in
  loop "0" changes

let count_value = Option.value ~default:"?"

let changes_label changes =
  let count = List.length changes in
  let noun = if count = 1 then "change" else "changes" in
  let additions = exact_total Request.Change.additions changes |> count_value in
  let removals = exact_total Request.Change.removals changes |> count_value in
  Printf.sprintf "%d %s \194\183 +%s \226\136\146%s" count noun additions
    removals

let change_label change =
  let additions =
    Request.Change.additions change |> Option.map string_of_int |> count_value
  in
  let removals =
    Request.Change.removals change |> Option.map string_of_int |> count_value
  in
  "+" ^ additions ^ " \226\136\146" ^ removals

let request_evidence ~palette request accesses =
  match Request.display request with
  | None -> []
  | Some value ->
      let value = request_display value in
      if List.exists is_code accesses then
        String.split_on_char '\n' value
        |> List.map (fun text ->
            Fixed
              { indent = 2; style = Theme.Palette.warning_style palette; text })
      else if List.exists is_command accesses then
        [
          Fixed
            {
              indent = 2;
              style = Theme.Palette.warning_style palette;
              text = "$ " ^ normalize_one_line value;
            };
        ]
      else
        [
          Prose
            {
              indent = 2;
              style = Theme.Palette.muted_style palette;
              text = value;
            };
        ]

let access_evidence ~palette request access_count index access =
  let prefix =
    if access_count = 1 then "access \194\183 "
    else Printf.sprintf "access %d/%d \194\183 " (index + 1) access_count
  in
  let action_is_identity = request_action_matches_access request access in
  let identity =
    Fixed
      {
        indent = 2;
        style = Theme.Palette.muted_style palette;
        text = prefix ^ access_inline ~omit_command:action_is_identity access;
      }
  in
  match access with
  | Access.Command command ->
      let code =
        match command with
        | Access.Command.Code { source; _ } when not action_is_identity ->
            code_source source |> String.split_on_char '\n'
            |> List.map (fun text ->
                Fixed
                  {
                    indent = 4;
                    style = Theme.Palette.warning_style palette;
                    text;
                  })
        | Access.Command.Code _ | Access.Command.Shell _ | Access.Command.Argv _
          ->
            []
      in
      identity
      :: Fixed
           {
             indent = 4;
             style = Theme.Palette.faint_style palette;
             text = "in " ^ command_cwd command;
           }
      :: Fixed
           {
             indent = 4;
             style = Theme.Palette.faint_style palette;
             text = "execution " ^ command_execution command;
           }
      :: code
  | Access.Path _ | Access.Network _ | Access.Custom _ -> [ identity ]

let item_summary ~palette item_count index item =
  let display = Request.Item.display item in
  let change = Request.Item.change item in
  if Option.is_none display && Option.is_none change then []
  else
    let owner = access_inline (Request.Item.access item) in
    let label =
      match display with None -> owner | Some value -> item_display value
    in
    let counts =
      Option.fold ~none:""
        ~some:(fun value -> " \194\183 " ^ change_label value)
        change
    in
    [
      Prose
        {
          indent = 2;
          style = Theme.Palette.muted_style palette;
          text =
            Printf.sprintf "item %d/%d \194\183 %s%s" (index + 1) item_count
              label counts;
        };
    ]

let evidence ~palette review =
  let request = Review.request review in
  let accesses = Review.accesses review in
  let items = Review.items review in
  let access_count = List.length accesses in
  let item_count = List.length items in
  let item_summaries =
    List.mapi (item_summary ~palette item_count) items |> List.concat
  in
  let diffs =
    List.filter_map
      (fun item -> Option.bind (Request.Item.change item) Request.Change.diff)
      items
    |> List.map (fun diff -> Diff diff)
  in
  let access_details =
    List.mapi (access_evidence ~palette request access_count) accesses
    |> List.concat
  in
  let provenance =
    match Request.source request with
    | None -> []
    | Some value ->
        [
          Fixed
            {
              indent = 2;
              style = Theme.Palette.faint_style palette;
              text = "requested by " ^ source value;
            };
        ]
  in
  request_evidence ~palette request accesses
  @ item_summaries @ diffs @ access_details @ provenance

let minimum_size = { width = px 0; height = px 0 }
let full_width = { width = pct 100; height = auto }
let fill_remaining = { width = pct 100; height = px 0 }

let fixed_row ~indent ~style value =
  box ~flex_shrink:0. ~min_size:minimum_size ~size:full_width
    ~padding:(padding_lrtb indent 2 0 0)
    [
      text ~style ~wrap:`None ~truncate:true ~flex_shrink:1.
        ~min_size:minimum_size value;
    ]

let prose_row ~indent ~style value =
  box ~flex_shrink:0. ~min_size:minimum_size ~size:full_width
    ~padding:(padding_lrtb indent 2 0 0)
    [
      text ~style ~wrap:`Word ~truncate:false ~flex_shrink:1.
        ~min_size:minimum_size ~size:full_width value;
    ]

(* One diff node over the shared renderer, unified always: the dialog is not
   width-responsive, and the enclosing evidence scroll_box owns height capping
   and scrolling. *)
let diff_node ~palette diff =
  Diff_view.render ~layout:Mosaic.Diff.Unified
    ~theme:(Theme.Palette.diff_theme palette)
    ~text_style:(Ansi.Style.make ~fg:(Theme.Palette.muted palette) ())
    ~added_emphasis:(Theme.Palette.diff_added_emphasis palette)
    ~removed_emphasis:(Theme.Palette.diff_removed_emphasis palette)
    ~palette ~emphasis:false ~wrap:`Word ~size:full_width
    (Diff_view.of_unified diff)

let render_evidence ~palette = function
  | Fixed { indent; style; text } -> fixed_row ~indent ~style text
  | Prose { indent; style; text } -> prose_row ~indent ~style text
  | Diff diff -> diff_node ~palette diff

let evidence_view ~palette t entries =
  let content =
    box ~key:"permission.evidence.content" ~flex_direction:Flex_direction.Column
      ~flex_shrink:0. ~min_size:minimum_size ~size:full_width
      (List.map (render_evidence ~palette) entries)
  in
  scroll_box ~key:"permission.evidence" ~scroll_x:false ~scroll_y:true
    ~focusable:true
    ~flex_grow:(if t.expanded then 1. else 0.)
    ~flex_shrink:1. ~min_size:minimum_size
    ~max_size:
      (if t.expanded then { width = pct 100; height = pct 100 }
       else { width = pct 100; height = px 8 })
    ~size:(if t.expanded then fill_remaining else full_width)
    [ content ]

let options_view ~palette t accesses =
  let once = allow_once_label accesses in
  let scope = exact_scope accesses in
  let selected = Option_list.selected t.nav in
  let columns =
    [
      Table.column ~width:(`Flex 1.) ~overflow:`Ellipsis ~min_width:1 "decision";
    ]
  in
  let row index label =
    let cursor = if selected = index then Theme.cursor else "  " in
    [| Table.cell (cursor ^ string_of_int (index + 1) ^ ". " ^ label) |]
  in
  let rows =
    [
      row 0 ("Yes, " ^ once);
      row 1 ("Yes, allow " ^ scope ^ " for this conversation");
      row 2 "No, deny";
      row 3 "No, deny and tell the model what to do differently";
    ]
  in
  table ~key:"permission.options" ~columns ~rows ~selected_row:selected
    ~border:false ~show_header:false ~show_column_separator:false
    ~show_row_separator:false ~cell_padding:0 ~background:Ansi.Color.default
    ~text_color:(Theme.Palette.muted palette)
    ~selected_background:(Theme.Palette.selection_bg palette)
    ~selected_text_color:(Theme.Palette.selection_fg palette)
    ~focused_selected_background:(Theme.Palette.selection_bg palette)
    ~focused_selected_text_color:(Theme.Palette.selection_fg palette)
    ~wrap_selection:true ~focusable:true ~autofocus:true ~flex_shrink:0.
    ~min_size:minimum_size ~size:full_width
    ~on_change:(fun index -> Some (Select_choice index))
    ~on_activate:(fun index -> Some (Activate_choice index))
    ()

let view ~palette (t : t) : msg Mosaic.t =
  let request = Review.request t.review in
  let accesses = Review.accesses t.review in
  let changes = Review.changes t.review in
  let all_evidence = evidence ~palette t.review in
  let count_rows =
    if List.is_empty changes then []
    else
      [
        fixed_row ~indent:2
          ~style:(Theme.Palette.muted_style palette)
          (changes_label changes);
      ]
  in
  let hint =
    match t.guidance with
    | Some _ -> [ "type a message"; "↵ deny with message"; "esc cancel" ]
    | None ->
        [ "1-4 select"; "↵ choose" ]
        @
        if not (List.is_empty all_evidence) then
          [ (if t.expanded then "^O less" else "^O more") ]
        else []
  in
  let summary_rows =
    if t.expanded then []
    else
      fixed_row ~indent:2
        ~style:(Theme.Palette.accent_style palette)
        (headline request accesses changes)
      :: count_rows
  in
  let decisions =
    match t.guidance with
    | None -> [ options_view ~palette t accesses ]
    | Some input ->
        [
          Inline_input.view ~palette input
          |> Mosaic.map (fun message -> Input_msg message);
        ]
  in
  let content =
    summary_rows @ [ evidence_view ~palette t all_evidence ] @ decisions
  in
  Panel.view ~palette
    ~frame:(Theme.Palette.accent palette)
    ~name:"permission" ~filter:"" ~hint ~content:(fragment content)
