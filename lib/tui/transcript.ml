(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Task = Mentat_session.Task

type reasoning = { duration_s : int; title : string option; body : string }

type block =
  | Banner of Snapshot.t
  | User of string
  | Assistant of string
  | Reasoning of reasoning
  | Tool of Tool_block.t
  | Board of Task.Board.t
  | Notice of Notice.t

(* Blocks stay newest-first so the common append path does not traverse an
   increasingly large conversation. *)
type t = block list
type text_shape = Inline | Block

let is_control code = code < 0x20 || (code >= 0x7f && code <= 0x9f)

let normalize shape text =
  let buffer = Buffer.create (String.length text) in
  let newline () =
    Buffer.add_char buffer (match shape with Inline -> ' ' | Block -> '\n')
  in
  let rec loop offset =
    if offset < String.length text then
      let decoded = String.get_utf_8_uchar text offset in
      let length = Uchar.utf_decode_length decoded in
      if not (Uchar.utf_decode_is_valid decoded) then (
        Buffer.add_utf_8_uchar buffer Uchar.rep;
        loop (offset + length))
      else
        let uchar = Uchar.utf_decode_uchar decoded in
        let code = Uchar.to_int uchar in
        if code = 0x0d then (
          newline ();
          let next = offset + length in
          let next =
            if next < String.length text && Char.equal text.[next] '\n' then
              next + 1
            else next
          in
          loop next)
        else if code = 0x0a || code = 0x2028 || code = 0x2029 then (
          newline ();
          loop (offset + length))
        else if code = 0x09 then (
          Buffer.add_string buffer "  ";
          loop (offset + length))
        else if is_control code then (
          Buffer.add_utf_8_uchar buffer Uchar.rep;
          loop (offset + length))
        else (
          Buffer.add_utf_8_uchar buffer uchar;
          loop (offset + length))
  in
  loop 0;
  Buffer.contents buffer

let is_unicode_space code =
  code = 0x20 || code = 0x85 || code = 0xa0 || code = 0x1680
  || (code >= 0x2000 && code <= 0x200a)
  || code = 0x2028 || code = 0x2029 || code = 0x202f || code = 0x205f
  || code = 0x3000

let grapheme_is_space grapheme =
  let rec loop offset =
    if offset >= String.length grapheme then true
    else
      let decoded = String.get_utf_8_uchar grapheme offset in
      let uchar = Uchar.utf_decode_uchar decoded in
      is_unicode_space (Uchar.to_int uchar)
      && loop (offset + Uchar.utf_decode_length decoded)
  in
  loop 0

let has_visible_text text =
  try
    Matrix.Text.iter_graphemes
      (fun ~offset ~len ->
        let grapheme = String.sub text offset len in
        (* Width is part of the semantic definition of visible input here: a
           string made only of combining marks cannot label a document block.
           This measurement never participates in presentation allocation. *)
        if
          Matrix.Text.measure ~width_method:`Unicode ~tab_width:2 grapheme > 0
          && not (grapheme_is_space grapheme)
        then raise Exit)
      text;
    false
  with Exit -> true

let trim_unicode_space text =
  let first = ref None in
  let last = ref 0 in
  let rec loop offset =
    if offset < String.length text then (
      let decoded = String.get_utf_8_uchar text offset in
      let length = Uchar.utf_decode_length decoded in
      if not (is_unicode_space (Uchar.to_int (Uchar.utf_decode_uchar decoded)))
      then (
        if Option.is_none !first then first := Some offset;
        last := offset + length);
      loop (offset + length))
  in
  loop 0;
  match !first with
  | None -> ""
  | Some first -> String.sub text first (!last - first)

let normalized_required ~field shape text =
  let text = normalize shape text in
  if has_visible_text text then text
  else invalid_arg ("Transcript." ^ field ^ ": text must be visible")

let normalized_heading ~field text =
  normalized_required ~field Inline text |> trim_unicode_space

let normalized_optional shape text =
  let text = normalize shape text in
  if has_visible_text text then Some text else None

let banner snapshot = Banner snapshot
let user text = User (normalized_required ~field:"user" Block text)

let assistant markdown =
  Assistant (normalized_required ~field:"assistant" Block markdown)

let reasoning ~duration_s ?title ~body () =
  if duration_s < 0 then
    invalid_arg "Transcript.reasoning: duration_s must be non-negative";
  let title =
    Option.bind title (fun title ->
        normalized_optional Inline title |> Option.map trim_unicode_space)
  in
  let body = Option.value ~default:"" (normalized_optional Block body) in
  Reasoning { duration_s; title; body }

let tool tool_block = Tool tool_block
let board task_board = Board task_board

let normalize_fact = function
  | Notice.Fact text ->
      normalized_optional Inline text
      |> Option.map (fun text -> Notice.Fact (trim_unicode_space text))
  | Notice.Change { added; removed } ->
      if added < 0 || removed < 0 then
        invalid_arg "Transcript.notice: change counts must be non-negative";
      Some (Notice.Change { added; removed })
  | Notice.Errors count ->
      if count < 0 then
        invalid_arg "Transcript.notice: error count must be non-negative";
      Some (Notice.Errors count)

let normalize_notice = function
  | Notice.Event message ->
      Notice.Event (normalized_required ~field:"notice" Block message)
  | Notice.Echo { command; result } ->
      Notice.Echo
        {
          command = normalized_heading ~field:"notice" command;
          result = Option.bind result (normalized_optional Block);
        }
  | Notice.Interrupt -> Notice.Interrupt
  | Notice.Failure { message; next_step; count } ->
      if count < 1 then
        invalid_arg "Transcript.notice: failure count must be positive";
      Notice.Failure
        {
          message = normalized_required ~field:"notice" Block message;
          next_step = normalized_required ~field:"notice" Block next_step;
          count;
        }
  | Notice.Seam label -> Notice.Seam (normalized_heading ~field:"notice" label)
  | Notice.Data { source; facts; atom } ->
      Notice.Data
        {
          source = normalized_heading ~field:"notice" source;
          facts = List.filter_map normalize_fact facts;
          atom =
            Option.bind atom (fun atom ->
                normalized_optional Inline atom |> Option.map trim_unicode_space);
        }
  | Notice.Workspace { level; source; title; body } ->
      Notice.Workspace
        {
          level;
          source = normalized_heading ~field:"notice" source;
          title = normalized_heading ~field:"notice" title;
          body = Option.bind body (normalized_optional Block);
        }

let notice value = Notice (normalize_notice value)
let empty = []
let length = List.length

(* [t] is stored newest-first: the [n] oldest blocks to keep are its trailing
   [n] elements, and the dropped blocks are the [length - n] newest ones at the
   head. Peel those newest blocks off the head into [dropped]; the remaining tail
   is [kept]. Both results keep [t]'s newest-first order. *)
let split n t =
  if n < 0 then invalid_arg "Transcript.split: length must be non-negative";
  let total = List.length t in
  let drop = if n >= total then 0 else total - n in
  let rec loop index acc = function
    | rest when index = drop -> (List.rev acc, rest)
    | block :: rest -> loop (index + 1) (block :: acc) rest
    | [] -> (List.rev acc, [])
  in
  let dropped, kept = loop 0 [] t in
  (kept, dropped)

let succ_saturating count = if count = max_int then max_int else count + 1

let append t block =
  match (block, t) with
  | Banner _, _ :: _ ->
      invalid_arg "Transcript.append: a banner must be the first block"
  | ( Notice (Notice.Failure { message; next_step; count = _ }),
      Notice
        (Notice.Failure
           { message = previous_message; next_step = previous_next; count })
      :: rest )
    when String.equal message previous_message
         && String.equal next_step previous_next ->
      Notice
        (Notice.Failure
           {
             message = previous_message;
             next_step = previous_next;
             count = succ_saturating count;
           })
      :: rest
  | ( Notice (Notice.Data { source; _ }),
      Notice (Notice.Data { source = previous_source; _ }) :: rest )
    when String.equal source previous_source ->
      block :: rest
  | Board _, Board _ :: rest -> block :: rest
  | _ -> block :: t

let blank_row = box ~size:{ width = pct 100; height = px 1 } []
let zero_size = { width = px 0; height = px 0 }
let hidden_overflow = { x = Overflow.Hidden; y = Overflow.Hidden }

let shrinking_space columns =
  box ~flex_shrink:100. ~min_size:zero_size
    ~size:{ width = px columns; height = auto }
    []

let render_user ~palette value =
  let marker_style =
    Ansi.Style.make
      ~fg:(Theme.Palette.muted palette)
      ~bg:(Theme.Palette.user_bg palette)
      ()
  in
  box ~flex_direction:Flex_direction.Row ~min_size:zero_size
    ~size:{ width = pct 100; height = auto }
    ~background:(Theme.Palette.user_bg palette)
    [
      text ~style:marker_style ~wrap:`None ~flex_shrink:0. Theme.cursor;
      box ~flex_direction:Flex_direction.Column ~flex_grow:1. ~flex_shrink:1.
        ~min_size:{ width = px 0; height = auto }
        [ text ~style:(Theme.Palette.user_style palette) ~wrap:`Word value ];
      shrinking_space 1;
    ]

let user_block ~palette text =
  let text = normalized_required ~field:"user_block" Block text in
  render_user ~palette text

let render_assistant ~palette value =
  box ~flex_direction:Flex_direction.Row ~min_size:zero_size
    ~size:{ width = pct 100; height = auto }
    [
      text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`None ~flex_shrink:0. (Theme.tool ^ " ");
      box ~flex_direction:Flex_direction.Column ~flex_grow:1. ~flex_shrink:1.
        ~min_size:{ width = px 0; height = auto }
        [ Prose.view ~palette value ];
    ]

let reasoning_head ~palette { duration_s; title; body } =
  let label = Printf.sprintf "%s Thought for %ds" Theme.thought duration_s in
  let title =
    match title with
    | None -> []
    | Some title ->
        [
          text
            ~style:(Theme.Palette.thinking_style palette)
            ~wrap:`None ~truncate:true ~flex_shrink:100.
            ~min_size:{ width = px 0; height = auto }
            (Theme.separator ^ title);
        ]
  in
  let hint =
    if has_visible_text body then
      [
        text
          ~style:(Theme.Palette.faint_style palette)
          ~wrap:`None ~truncate:true ~flex_shrink:10.
          ~min_size:{ width = px 0; height = auto }
          "  (ctrl+o)";
      ]
    else []
  in
  box ~flex_direction:Flex_direction.Row ~flex_shrink:0.
    ~overflow:hidden_overflow ~min_size:zero_size
    ~size:{ width = pct 100; height = px 1 }
    (text
       ~style:(Theme.Palette.thinking_style palette)
       ~wrap:`None ~truncate:true ~flex_shrink:1.
       ~min_size:{ width = px 0; height = auto }
       label
     :: title
    @ hint)

let render_reasoning ~palette ~expanded reasoning =
  let head = reasoning_head ~palette reasoning in
  if expanded && has_visible_text reasoning.body then
    box ~flex_direction:Flex_direction.Column ~min_size:zero_size
      ~size:{ width = pct 100; height = auto }
      [
        head;
        box ~flex_direction:Flex_direction.Row ~min_size:zero_size
          ~size:{ width = pct 100; height = auto }
          [
            shrinking_space 2;
            box ~flex_direction:Flex_direction.Column ~flex_grow:1.
              ~flex_shrink:1.
              ~min_size:{ width = px 0; height = auto }
              [ Prose.thinking ~palette reasoning.body ];
          ];
      ]
  else head

type board_counts = { total : int; running : int; done_ : int }

let board_counts board =
  let add counts (item : Task.Item.t) =
    match item.Task.Item.status with
    | Task.Status.In_progress -> { counts with running = counts.running + 1 }
    | Task.Status.Pending -> counts
    | Task.Status.Completed | Task.Status.Cancelled ->
        { counts with done_ = counts.done_ + 1 }
  in
  let items = Task.Board.items board in
  List.fold_left add { total = List.length items; running = 0; done_ = 0 } items

let render_board ~palette board =
  let counts = board_counts board in
  let argument =
    Printf.sprintf "%d tasks%s%d done%s%d running" counts.total Theme.separator
      counts.done_ Theme.separator counts.running
  in
  Tool_block.header ~palette Tool_block.Todo ~argument ~dot:Tool_block.Succeeded

let render ~palette ~expanded = function
  | Banner snapshot -> Banner.record ~palette snapshot
  | User value -> render_user ~palette value
  | Assistant value -> render_assistant ~palette value
  | Reasoning reasoning -> render_reasoning ~palette ~expanded reasoning
  | Tool tool_block -> Tool_block.view ~palette tool_block
  | Board board -> render_board ~palette board
  | Notice notice -> Notice.view ~palette notice

let view ?(expanded = false) ~palette t =
  let add (previous, children) block =
    let rendered = render ~palette ~expanded block in
    let children =
      match previous with
      | None | Some (Banner _) -> rendered :: children
      | Some _ -> rendered :: blank_row :: children
    in
    (Some block, children)
  in
  let _, children = List.fold_left add (None, []) (List.rev t) in
  box ~flex_direction:Flex_direction.Column
    ~size:{ width = pct 100; height = auto }
    (List.rev children)
