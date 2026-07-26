(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Summary = Mentat_session.Summary
module Id = Mentat_session.Id
module Time = Mentat_session.Time

module Motion = struct
  type t = Static | Pouring of int

  let pour_len = Array.length Theme.pour_frames
  let hold_frames = 4
  let cycle_len = pour_len + hold_frames

  let () =
    if pour_len = 0 then
      invalid_arg "Home.Motion: Theme.pour_frames must not be empty"

  let init ~reduced = if reduced then Static else Pouring 0
  let freeze _ = Static
  let animating = function Static -> false | Pouring _ -> true

  let tick = function
    | Static -> Static
    | Pouring index -> Pouring ((index + 1) mod cycle_len)

  let row1, row2 =
    match Theme.lockup with
    | [ row1; row2 ] -> (row1, row2)
    | _ -> invalid_arg "Home.Motion: Theme.lockup must contain exactly two rows"

  let row1_suffix = "  " ^ Theme.grain_aloft

  let without_suffix ~name row suffix =
    if String.ends_with ~suffix row then
      String.sub row 0 (String.length row - String.length suffix)
    else invalid_arg ("Home.Motion: Theme.lockup " ^ name ^ " suffix changed")

  let row1_prefix = without_suffix ~name:"first-row" row1 row1_suffix
  let row2_prefix = without_suffix ~name:"second-row" row2 Theme.heap

  let frame_rows (frame : Theme.pour_frame) =
    [ row1_prefix ^ frame.Theme.grain; row2_prefix ^ frame.Theme.mound ]

  let lockup_rows = function
    | Static -> Theme.lockup
    | Pouring index ->
        let frame = if index < pour_len then index else pour_len - 1 in
        frame_rows Theme.pour_frames.(frame)
end

let replacement = Uchar.rep

let add_inline_uchar buffer uchar =
  let code = Uchar.to_int uchar in
  match code with
  | 0x09 | 0x0A | 0x0D | 0x2028 | 0x2029 -> Buffer.add_char buffer ' '
  | code when code < 0x20 || (code >= 0x7F && code <= 0x9F) ->
      Buffer.add_utf_8_uchar buffer replacement
  | _ -> Buffer.add_utf_8_uchar buffer uchar

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

let visibly_nonblank text =
  let rec loop offset =
    if offset >= String.length text then false
    else
      let decoded = String.get_utf_8_uchar text offset in
      if is_white_space (Uchar.utf_decode_uchar decoded) then
        loop (offset + Uchar.utf_decode_length decoded)
      else true
  in
  loop 0

let normalize_error message =
  let message = normalize_inline message in
  if visibly_nonblank message then message else "session history unavailable"

let eligible summary =
  Mentat_session.Metadata.Status.is_active (Summary.lifecycle summary)
  && Option.is_none (Summary.forked_from summary)
  && Option.is_none (Summary.delegated_from summary)

let normalize_summaries summaries =
  let seen = Hashtbl.create (List.length summaries) in
  summaries
  |> List.sort Summary.compare_recency
  |> List.filter (fun summary ->
      let identifier = Summary.id summary |> Id.to_string in
      if (not (eligible summary)) || Hashtbl.mem seen identifier then false
      else begin
        Hashtbl.add seen identifier ();
        true
      end)

module Recents = struct
  type rows = Summary.t list

  type t =
    | Loading
    | Ready of rows
    | Refreshing of rows
    | Failed of { message : string; retained : rows option }

  let loading = Loading

  let retained = function
    | Loading -> None
    | Ready rows | Refreshing rows -> Some rows
    | Failed failure -> failure.retained

  let loaded summaries = Ready (normalize_summaries summaries)

  let refreshing t =
    match retained t with None -> Loading | Some rows -> Refreshing rows

  let failed message t =
    Failed { message = normalize_error message; retained = retained t }

  let most_recent t =
    match retained t with
    | Some (summary :: _) -> Some (Summary.id summary)
    | Some [] | None -> None
end

let hidden_overflow = { x = Overflow.Hidden; y = Overflow.Hidden }

let grow_spacer =
  box ~flex_grow:1. ~flex_shrink:1. ~size:{ width = pct 100; height = px 0 } []

let composer_inset composer =
  box ~flex_shrink:0.
    ~size:{ width = pct 100; height = auto }
    ~max_size:{ width = px 100; height = auto }
    [ composer ]

let inline ?(shrink = 0.) ?(truncate = false) style content =
  text ~style ~wrap:`None ~truncate ~flex_shrink:shrink content

let centered_row children =
  box ~flex_direction:Flex_direction.Row ~justify_content:Justify.Center
    ~overflow:hidden_overflow ~flex_shrink:0.
    ~size:{ width = pct 100; height = px 1 }
    children

let status_row ~style ~marker message =
  centered_row
    [ inline ~shrink:1. style (marker ^ " " ^ normalize_inline message) ]

let account_row ~palette =
  centered_row
    [
      inline (Theme.Palette.error_style palette) "! ";
      inline (Theme.Palette.atom_style palette) "/login";
      inline ~shrink:1.
        (Theme.Palette.muted_style palette)
        " — no connected account";
    ]

(* Styled runs for the notice's lead line: each occurrence of the product name
   is an unbolded-accent atom inside an otherwise default-fg line. *)
let notice_lead_runs ~palette content =
  let needle = "mentat" in
  let nlen = String.length needle in
  let atom = Theme.Palette.atom_style palette in
  let rec go acc start i =
    if i + nlen > String.length content then
      let tail = String.sub content start (String.length content - start) in
      List.rev
        (if tail = "" then acc else inline Ansi.Style.default tail :: acc)
    else if String.sub content i nlen = needle then
      let before = String.sub content start (i - start) in
      let acc =
        if before = "" then acc else inline Ansi.Style.default before :: acc
      in
      go (inline atom needle :: acc) (i + nlen) (i + nlen)
    else go acc start (i + 1)
  in
  go [] 0 0

(* The notice slot: release/host announcements between the facts line and the
   composer. The [▎] bar is accent — a notice is
   mentat speaking to its user, so it reads warm, not like chrome. The lead
   line is default foreground with the product name an accent atom; every
   supporting line is muted. The block sizes to its widest line, so the stage
   centers it on its visible text and the bars stay registered, lines
   left-aligned within the block. *)
let notice_block ~palette lines =
  let lines =
    lines |> List.map normalize_inline |> List.filter visibly_nonblank
  in
  let line index content =
    let runs =
      if index = 0 then notice_lead_runs ~palette content
      else [ inline (Theme.Palette.muted_style palette) content ]
    in
    box ~flex_direction:Flex_direction.Row ~overflow:hidden_overflow
      ~flex_shrink:1.
      ~size:{ width = auto; height = px 1 }
      (inline (Theme.Palette.accent_style palette) "▎ " :: runs)
  in
  match lines with
  | [] -> None
  | lines ->
      Some
        (box ~flex_direction:Flex_direction.Row ~justify_content:Justify.Center
           ~overflow:hidden_overflow ~flex_shrink:1.
           ~size:{ width = pct 100; height = auto }
           [
             box ~flex_direction:Flex_direction.Column ~flex_shrink:1.
               ~overflow:hidden_overflow
               ~size:{ width = auto; height = auto }
               (List.mapi line lines);
           ])

let brand ~palette snapshot motion =
  Banner.home ~palette snapshot ~rows:(Motion.lockup_rows motion)

let turns_label turns =
  Printf.sprintf "%d turn%s" turns (if turns = 1 then "" else "s")

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

let summary_title summary =
  let usable = function
    | None -> None
    | Some text ->
        let text = normalize_inline text in
        if visibly_nonblank text then Some text else None
  in
  match usable (Summary.title summary) with
  | Some title -> title
  | None -> (
      match usable (Summary.preview summary) with
      | Some preview -> preview
      | None -> Summary.id summary |> Id.to_string |> normalize_inline)

let phase_piece ~palette summary =
  match Summary.phase summary with
  | Summary.Phase.Idle -> None
  | Summary.Phase.Working ->
      Some (inline (Theme.Palette.running_style palette) "working")
  | Summary.Phase.Waiting ->
      Some (inline (Theme.Palette.warning_style palette) "waiting")

let with_separators ~palette pieces =
  let rec loop = function
    | [] -> []
    | [ piece ] -> [ piece ]
    | piece :: remaining ->
        piece
        :: inline (Theme.Palette.muted_style palette) Theme.separator
        :: loop remaining
  in
  loop pieces

let recent_row ~palette ~now summary =
  let metadata =
    Option.to_list (phase_piece ~palette summary)
    @ [
        inline
          (Theme.Palette.muted_style palette)
          (relative_age ~now (Summary.updated_at summary));
        inline
          (Theme.Palette.muted_style palette)
          (turns_label (Summary.turns summary));
      ]
    |> with_separators ~palette
  in
  centered_row
    ([
       inline (Theme.Palette.atom_style palette) "↵ ";
       inline ~shrink:1. ~truncate:true Ansi.Style.default
         ("\"" ^ summary_title summary ^ "\"");
       inline (Theme.Palette.muted_style palette) Theme.separator;
     ]
    @ metadata)

let ready_rows ~palette ~now = function
  | [] ->
      [
        status_row
          ~style:(Theme.Palette.muted_style palette)
          ~marker:"∅" "no recent sessions";
      ]
  | summary :: _ -> [ recent_row ~palette ~now summary ]

let recents_rows ~palette ~now = function
  | Recents.Loading ->
      [
        status_row
          ~style:(Theme.Palette.muted_style palette)
          ~marker:"⠋" "loading sessions…";
      ]
  | Recents.Ready rows -> ready_rows ~palette ~now rows
  | Recents.Refreshing rows ->
      status_row
        ~style:(Theme.Palette.muted_style palette)
        ~marker:"⠋" "refreshing sessions…"
      :: ready_rows ~palette ~now rows
  | Recents.Failed { message; retained = None } ->
      [
        status_row
          ~style:(Theme.Palette.error_style palette)
          ~marker:"!" message;
      ]
  | Recents.Failed { message; retained = Some rows } ->
      status_row ~style:(Theme.Palette.error_style palette) ~marker:"!" message
      :: ready_rows ~palette ~now rows

let facts_block ~palette ~now ~account_absent recents =
  let rows =
    (if account_absent then [ account_row ~palette ] else [])
    @ recents_rows ~palette ~now recents
  in
  box ~flex_direction:Flex_direction.Column ~flex_shrink:1.
    ~overflow:hidden_overflow
    ~size:{ width = pct 100; height = auto }
    rows

let permission_warning ~palette =
  status_row
    ~style:(Theme.Palette.error_style palette)
    ~marker:"!" "permission bypass — change in /settings"

let shrinkable priority node =
  box ~flex_shrink:priority ~overflow:hidden_overflow
    ~min_size:{ width = auto; height = px 0 }
    ~size:{ width = pct 100; height = auto }
    [ node ]

let stage ~palette ~snapshot ~recents ~now ~account_absent ~permission_review
    ~notice ~motion ~composer =
  let idle = Option.is_some composer in
  let warning =
    match (idle, permission_review) with
    | true, Mentat_permission.Review_behavior.Bypass ->
        Some (permission_warning ~palette)
    | true, Mentat_permission.Review_behavior.Enforce | false, _ -> None
  in
  let facts =
    if idle then Some (facts_block ~palette ~now ~account_absent recents)
    else None
  in
  let content =
    [
      Some (shrinkable 25. (brand ~palette snapshot motion));
      Option.map (shrinkable 100.) (notice_block ~palette notice);
      Option.map composer_inset composer;
      Option.map (shrinkable 50.) facts;
      warning;
    ]
    |> List.filter_map Fun.id
  in
  let content =
    box ~flex_direction:Flex_direction.Column ~align_items:Align.Center
      ~overflow:hidden_overflow ~flex_shrink:1.
      ~min_size:{ width = auto; height = px 0 }
      ~gap:{ width = px 0; height = px 1 }
      ~size:{ width = pct 100; height = auto }
      content
  in
  box ~key:"stage" ~flex_direction:Flex_direction.Column
    ~overflow:hidden_overflow ~flex_grow:1. ~flex_shrink:1.
    ~min_size:{ width = auto; height = px 0 }
    ~size:{ width = pct 100; height = auto }
    [ grow_spacer; content; grow_spacer ]
