(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Label = struct
  type t = string

  let invalid_char = function '\n' | '\r' | '\000' -> true | _ -> false
  let has_invalid_char = String.exists invalid_char

  let of_string label =
    if String.is_empty label then invalid_arg "diff label must not be empty";
    if has_invalid_char label then invalid_arg "diff label is malformed";
    label

  let escaped label =
    let label =
      if String.is_empty label then "<empty>" else String.escaped label
    in
    of_string label

  let to_string t = t
  let pp ppf t = Format.pp_print_string ppf t
end

module File_change = struct
  type t =
    | Add of { label : Label.t; contents : string }
    | Delete of { label : Label.t; contents : string }
    | Modify of { label : Label.t; before : string; after : string }

  let of_states ~label ~before ~after =
    match (before, after) with
    | None, None -> None
    | None, Some contents -> Some (Add { label; contents })
    | Some contents, None -> Some (Delete { label; contents })
    | Some before, Some after -> Some (Modify { label; before; after })

  let create ~label ~contents = Add { label; contents }
  let delete ~label ~contents = Delete { label; contents }
  let modify ~label ~before ~after = Modify { label; before; after }

  let label = function
    | Add file -> file.label
    | Delete file -> file.label
    | Modify file -> file.label

  let before = function
    | Add _ -> None
    | Delete file -> Some file.contents
    | Modify file -> Some file.before

  let after = function
    | Add file -> Some file.contents
    | Delete _ -> None
    | Modify file -> Some file.after
end

type stats = { files : int; additions : int; deletions : int }

module Limits = struct
  type t = {
    max_files : int;
    max_file_bytes : int;
    max_lines : int;
    max_edit_distance : int option;
  }

  let make ~max_files ~max_file_bytes ~max_lines ?max_edit_distance () =
    if max_files < 0 then invalid_arg "max_files must be non-negative";
    if max_file_bytes < 0 then invalid_arg "max_file_bytes must be non-negative";
    if max_lines < 0 then invalid_arg "max_lines must be non-negative";
    Option.iter
      (fun max_edit_distance ->
        if max_edit_distance < 0 then
          invalid_arg "max_edit_distance must be non-negative")
      max_edit_distance;
    { max_files; max_file_bytes; max_lines; max_edit_distance }
end

type t = { text : string; stats : stats; omitted : int }
type line = { content : string; newline : bool }
type op = Delete_line of line | Insert_line of line | Keep_line of line

type indexed_op = {
  op : op;
  indexed_before_start : int;
  indexed_before_len : int;
  indexed_after_start : int;
  indexed_after_len : int;
}

type file_text = {
  before_state : string option;
  after_state : string option;
  before_text : string;
  after_text : string;
}

type hunk = {
  before_start : int;
  before_len : int;
  after_start : int;
  after_len : int;
  lines : (char * line) list;
}

let empty_stats = { files = 0; additions = 0; deletions = 0 }

let validate_context context =
  if context < 0 then invalid_arg "context must be non-negative";
  context

let file_text file =
  let before_state = File_change.before file in
  let after_state = File_change.after file in
  {
    before_state;
    after_state;
    before_text = Option.value before_state ~default:"";
    after_text = Option.value after_state ~default:"";
  }

let is_noop = function
  | File_change.Modify { before; after; _ } -> String.equal before after
  | File_change.Add _ | File_change.Delete _ -> false

let split_lines text =
  let len = String.length text in
  let rec count acc i =
    if i = len then
      if len > 0 && not (Char.equal text.[len - 1] '\n') then acc + 1 else acc
    else if Char.equal text.[i] '\n' then count (acc + 1) (i + 1)
    else count acc (i + 1)
  in
  let count = count 0 0 in
  if count = 0 then [||]
  else
    (* The first pass counted the exact number of output lines, so the unsafe
       writes below stay within [lines]. *)
    let lines = Array.make count { content = ""; newline = true } in
    let rec fill line start i =
      if i = len then
        if start < len then
          Array.unsafe_set lines line
            { content = String.sub text start (len - start); newline = false }
        else ()
      else if Char.equal text.[i] '\n' then begin
        Array.unsafe_set lines line
          { content = String.sub text start (i - start); newline = true };
        fill (line + 1) (i + 1) (i + 1)
      end
      else fill line start (i + 1)
    in
    fill 0 0 0;
    lines

let line_equal a b =
  String.equal a.content b.content && Bool.equal a.newline b.newline

let edit_script ?max_distance before after =
  let exception Found of int in
  let n = Array.length before in
  let m = Array.length after in
  let max_d = n + m in
  if max_d = 0 then Some []
  else
    let search_limit = min max_d (Option.value max_distance ~default:max_d) in
    let offset = search_limit in
    let v = Array.make ((2 * search_limit) + 3) (-1) in
    let traces = Array.make (search_limit + 1) [||] in
    v.(offset + 1) <- 0;
    try
      for d = 0 to search_limit do
        for k = -d to d do
          if (k + d) mod 2 = 0 then begin
            let x =
              if k = -d || (k <> d && v.(offset + k - 1) < v.(offset + k + 1))
              then v.(offset + k + 1)
              else v.(offset + k - 1) + 1
            in
            let x = ref x in
            let y = ref (!x - k) in
            while !x < n && !y < m && line_equal before.(!x) after.(!y) do
              incr x;
              incr y
            done;
            v.(offset + k) <- !x;
            if !x >= n && !y >= m then begin
              traces.(d) <- Array.copy v;
              raise_notrace (Found d)
            end
          end
        done;
        traces.(d) <- Array.copy v
      done;
      if search_limit < max_d then None else Some []
    with Found d_final ->
      let x = ref n in
      let y = ref m in
      let ops = ref [] in
      for d = d_final downto 1 do
        let k = !x - !y in
        let previous = traces.(d - 1) in
        let previous_k =
          if
            k = -d
            || (k <> d && previous.(offset + k - 1) < previous.(offset + k + 1))
          then k + 1
          else k - 1
        in
        let previous_x = previous.(offset + previous_k) in
        let previous_y = previous_x - previous_k in
        while !x > previous_x && !y > previous_y do
          ops := Keep_line before.(!x - 1) :: !ops;
          decr x;
          decr y
        done;
        if previous_k = k + 1 then ops := Insert_line after.(previous_y) :: !ops
        else ops := Delete_line before.(previous_x) :: !ops;
        x := previous_x;
        y := previous_y
      done;
      while !x > 0 && !y > 0 do
        ops := Keep_line before.(!x - 1) :: !ops;
        decr x;
        decr y
      done;
      while !x > 0 do
        ops := Delete_line before.(!x - 1) :: !ops;
        decr x
      done;
      while !y > 0 do
        ops := Insert_line after.(!y - 1) :: !ops;
        decr y
      done;
      Some !ops

let edit_stats before after =
  let exception Found of int in
  let n = Array.length before in
  let m = Array.length after in
  let max_d = n + m in
  if max_d = 0 then { files = 1; additions = 0; deletions = 0 }
  else
    let offset = max_d in
    let v = Array.make ((2 * max_d) + 3) (-1) in
    v.(offset + 1) <- 0;
    let distance =
      try
        for d = 0 to max_d do
          for k = -d to d do
            if (k + d) mod 2 = 0 then begin
              let x =
                if k = -d || (k <> d && v.(offset + k - 1) < v.(offset + k + 1))
                then v.(offset + k + 1)
                else v.(offset + k - 1) + 1
              in
              let x = ref x in
              let y = ref (!x - k) in
              while !x < n && !y < m && line_equal before.(!x) after.(!y) do
                incr x;
                incr y
              done;
              v.(offset + k) <- !x;
              if !x >= n && !y >= m then raise_notrace (Found d)
            end
          done
        done;
        max_d
      with Found d -> d
    in
    {
      files = 1;
      additions = (distance + m - n) / 2;
      deletions = (distance + n - m) / 2;
    }

(* Word-level emphasis. Each line is tokenized on codepoint boundaries into
   maximal runs of one class — whitespace, an ASCII word character, or a single
   "other" codepoint — so a run of spaces stays one recoverable token while
   punctuation and space-free scripts never collapse into one whole-line token.
   The two token sequences diff through the shared Myers pass; the changed
   tokens' byte ranges are the emphasis spans. *)

type char_class = Space | Word | Other

let class_equal a b =
  match (a, b) with
  | Space, Space | Word, Word | Other, Other -> true
  | (Space | Word | Other), _ -> false

let class_of_uchar u =
  let c = Uchar.to_int u in
  if c = 0x20 || c = 0x09 || c = 0x0A || c = 0x0B || c = 0x0C || c = 0x0D then
    Space
  else if
    (c >= Char.code 'a' && c <= Char.code 'z')
    || (c >= Char.code 'A' && c <= Char.code 'Z')
    || (c >= Char.code '0' && c <= Char.code '9')
    || c = Char.code '_'
  then Word
  else Other

type token = { token_start : int; token_stop : int; token_text : string }

let decode_class text i =
  let decoded = String.get_utf_8_uchar text i in
  let width = max 1 (Uchar.utf_decode_length decoded) in
  let cls =
    if Uchar.utf_decode_is_valid decoded then
      class_of_uchar (Uchar.utf_decode_uchar decoded)
    else Other
  in
  (cls, width)

let tokenize text =
  let len = String.length text in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < len do
    let start = !i in
    let cls, width = decode_class text !i in
    i := !i + width;
    (match cls with
    | Other -> ()
    | Space | Word ->
        let continue = ref true in
        while !continue && !i < len do
          let cls', width' = decode_class text !i in
          if class_equal cls' cls then i := !i + width' else continue := false
        done);
    tokens :=
      {
        token_start = start;
        token_stop = !i;
        token_text = String.sub text start (!i - start);
      }
      :: !tokens
  done;
  Array.of_list (List.rev !tokens)

let token_lines tokens =
  Array.map (fun t -> { content = t.token_text; newline = false }) tokens

let word_spans ?max_edit_distance ~before ~after () =
  let before_tokens = tokenize before in
  let after_tokens = tokenize after in
  match
    edit_script ?max_distance:max_edit_distance
      (token_lines before_tokens)
      (token_lines after_tokens)
  with
  | None -> ([], [])
  | Some ops ->
      let deletions = ref 0 in
      let insertions = ref 0 in
      let keeps = ref 0 in
      let before_index = ref 0 in
      let after_index = ref 0 in
      let removed = ref [] in
      let added = ref [] in
      (* Adjacent changed tokens share a byte boundary, so extend the running
         span rather than emit a second one; a kept token between two changes
         breaks the run because its bytes sit in the gap. *)
      let push acc token =
        match !acc with
        | (start, stop) :: rest when stop = token.token_start ->
            acc := (start, token.token_stop) :: rest
        | spans -> acc := (token.token_start, token.token_stop) :: spans
      in
      List.iter
        (function
          | Keep_line _ ->
              incr keeps;
              incr before_index;
              incr after_index
          | Delete_line _ ->
              incr deletions;
              push removed before_tokens.(!before_index);
              incr before_index
          | Insert_line _ ->
              incr insertions;
              push added after_tokens.(!after_index);
              incr after_index)
        ops;
      (* A changed-token ratio above one half means the two lines share too few
         words to read as an intra-line edit, so suppress emphasis and let the
         line render as a plain add/remove pair. *)
      if !deletions + !insertions > 2 * !keeps then ([], [])
      else (List.rev !removed, List.rev !added)

let indexed_ops ops =
  let before_line = ref 1 in
  let after_line = ref 1 in
  let entry op before_len after_len =
    let entry : indexed_op =
      {
        op;
        indexed_before_start = !before_line;
        indexed_before_len = before_len;
        indexed_after_start = !after_line;
        indexed_after_len = after_len;
      }
    in
    before_line := !before_line + before_len;
    after_line := !after_line + after_len;
    entry
  in
  Array.of_list
    (List.map
       (function
         | Keep_line _ as op -> entry op 1 1
         | Delete_line _ as op -> entry op 1 0
         | Insert_line _ as op -> entry op 0 1)
       ops)

let is_change entry =
  match entry.op with
  | Keep_line _ -> false
  | Delete_line _ | Insert_line _ -> true

let changed_blocks entries =
  let len = Array.length entries in
  let rec loop blocks i =
    if i >= len then List.rev blocks
    else if not (is_change entries.(i)) then loop blocks (i + 1)
    else
      let start = i in
      let rec finish i =
        if i >= len || not (is_change entries.(i)) then i - 1 else finish (i + 1)
      in
      let last = finish i in
      loop ((start, last) :: blocks) (last + 1)
  in
  loop [] 0

let expand_block entries ~context (start, last) =
  let len = Array.length entries in
  let first = if context >= start then 0 else start - context in
  let after_last = len - 1 - last in
  let last = if context >= after_last then len - 1 else last + context in
  (first, last)

let merge_ranges ranges =
  let rec loop merged = function
    | [] -> List.rev merged
    | (start, last) :: ranges -> (
        match merged with
        | (merged_start, merged_last) :: rest when start <= merged_last + 1 ->
            loop ((merged_start, max merged_last last) :: rest) ranges
        | _ -> loop ((start, last) :: merged) ranges)
  in
  loop [] ranges

let line_of_op = function
  | Keep_line line | Delete_line line | Insert_line line -> line

let prefix_of_op = function
  | Keep_line _ -> ' '
  | Delete_line _ -> '-'
  | Insert_line _ -> '+'

let hunk_of_range (entries : indexed_op array) (start, last) =
  let before_start = ref None in
  let after_start = ref None in
  let before_len = ref 0 in
  let after_len = ref 0 in
  let lines = ref [] in
  for i = start to last do
    let entry : indexed_op = entries.(i) in
    if entry.indexed_before_len > 0 then begin
      if Option.is_none !before_start then
        before_start := Some entry.indexed_before_start;
      before_len := !before_len + entry.indexed_before_len
    end;
    if entry.indexed_after_len > 0 then begin
      if Option.is_none !after_start then
        after_start := Some entry.indexed_after_start;
      after_len := !after_len + entry.indexed_after_len
    end;
    lines := (prefix_of_op entry.op, line_of_op entry.op) :: !lines
  done;
  ({
     before_start =
       Option.value !before_start ~default:entries.(start).indexed_before_start;
     before_len = !before_len;
     after_start =
       Option.value !after_start ~default:entries.(start).indexed_after_start;
     after_len = !after_len;
     lines = List.rev !lines;
   }
    : hunk)

let hunks_of_ops ~context ops =
  let entries = indexed_ops ops in
  changed_blocks entries
  |> List.map (expand_block entries ~context)
  |> merge_ranges
  |> List.map (hunk_of_range entries)

let starts_with_at text ~prefix ~at =
  let len = String.length text in
  let prefix_len = String.length prefix in
  at + prefix_len <= len
  &&
  let rec loop i =
    i = prefix_len || (Char.equal text.[at + i] prefix.[i] && loop (i + 1))
  in
  loop 0

let bidi_escapes =
  [|
    ("\216\156", "\\u{061c}");
    ("\226\128\142", "\\u{200e}");
    ("\226\128\143", "\\u{200f}");
    ("\226\128\170", "\\u{202a}");
    ("\226\128\171", "\\u{202b}");
    ("\226\128\172", "\\u{202c}");
    ("\226\128\173", "\\u{202d}");
    ("\226\128\174", "\\u{202e}");
    ("\226\129\166", "\\u{2066}");
    ("\226\129\167", "\\u{2067}");
    ("\226\129\168", "\\u{2068}");
    ("\226\129\169", "\\u{2069}");
  |]

let bidi_escape text i =
  let rec loop j =
    if j = Array.length bidi_escapes then None
    else
      let prefix, escape = bidi_escapes.(j) in
      if starts_with_at text ~prefix ~at:i then
        Some (escape, String.length prefix)
      else loop (j + 1)
  in
  loop 0

let add_escaped_byte buffer byte =
  Buffer.add_string buffer "\\x";
  let hex = "0123456789ABCDEF" in
  Buffer.add_char buffer hex.[byte lsr 4];
  Buffer.add_char buffer hex.[byte land 0xF]

let add_display_string buffer text =
  let len = String.length text in
  let rec loop i =
    if i < len then
      match bidi_escape text i with
      | Some (escape, width) ->
          Buffer.add_string buffer escape;
          loop (i + width)
      | None ->
          let char = text.[i] in
          let code = Char.code char in
          if
            (code < 0x20 && not (Char.equal char '\t'))
            || code = 0x7F
            || (code >= 0x80 && code <= 0x9F)
          then add_escaped_byte buffer code
          else Buffer.add_char buffer char;
          loop (i + 1)
  in
  loop 0

module Hunk = struct
  module Line = struct
    type kind = Context | Added | Removed

    type t = {
      kind : kind;
      text : string;
      newline : bool;
      old_line : int option;
      new_line : int option;
    }

    let kind (line : t) = line.kind
    let text (line : t) = line.text
    let newline (line : t) = line.newline
    let old_line (line : t) = line.old_line
    let new_line (line : t) = line.new_line
    let kind_rank = function Context -> 0 | Removed -> 1 | Added -> 2

    let equal a b =
      Int.equal (kind_rank a.kind) (kind_rank b.kind)
      && String.equal a.text b.text
      && Bool.equal a.newline b.newline
      && Option.equal Int.equal a.old_line b.old_line
      && Option.equal Int.equal a.new_line b.new_line

    let prefix_char = function Context -> ' ' | Removed -> '-' | Added -> '+'

    let pp ppf line =
      Format.fprintf ppf "%c%s" (prefix_char line.kind) line.text
  end

  type t = {
    old_start : int;
    old_count : int;
    new_start : int;
    new_count : int;
    lines : Line.t list;
  }

  let old_start (hunk : t) = hunk.old_start
  let old_count (hunk : t) = hunk.old_count
  let new_start (hunk : t) = hunk.new_start
  let new_count (hunk : t) = hunk.new_count
  let lines (hunk : t) = hunk.lines

  let line_counts hunks =
    List.fold_left
      (fun (adds, dels) hunk ->
        List.fold_left
          (fun (adds, dels) line ->
            match Line.kind line with
            | Line.Added -> (adds + 1, dels)
            | Line.Removed -> (adds, dels + 1)
            | Line.Context -> (adds, dels))
          (adds, dels) (lines hunk))
      (0, 0) hunks

  let equal a b =
    Int.equal a.old_start b.old_start
    && Int.equal a.old_count b.old_count
    && Int.equal a.new_start b.new_start
    && Int.equal a.new_count b.new_count
    && List.equal Line.equal a.lines b.lines

  let pp ppf hunk =
    let header start count = if count = 0 then start - 1 else start in
    Format.fprintf ppf "@@@@ -%d,%d +%d,%d @@@@"
      (header hunk.old_start hunk.old_count)
      hunk.old_count
      (header hunk.new_start hunk.new_count)
      hunk.new_count;
    List.iter (fun line -> Format.fprintf ppf "@\n%a" Line.pp line) hunk.lines

  let render hunk =
    let start s count = if count = 0 then s - 1 else s in
    let buffer = Buffer.create 256 in
    Buffer.add_string buffer
      (Printf.sprintf "@@ -%d,%d +%d,%d @@\n"
         (start hunk.old_start hunk.old_count)
         hunk.old_count
         (start hunk.new_start hunk.new_count)
         hunk.new_count);
    List.iter
      (fun line ->
        Buffer.add_char buffer (Line.prefix_char (Line.kind line));
        add_display_string buffer (Line.text line);
        Buffer.add_char buffer '\n';
        if not (Line.newline line) then
          Buffer.add_string buffer "\\ No newline at end of file\n")
      hunk.lines;
    Buffer.contents buffer

  (* One line's generating set: its kind, raw content, and newline flag. Both the
     engine's regrouping and the wire decoder produce these, and {!build} walks
     them into a hunk — the single pass that assigns per-side line numbers and
     counts. Those derived fields are computed one way and never carried on the
     wire, so no derivable field is trusted from it. *)
  type wire_line = { wire_kind : Line.kind; content : string; newline : bool }

  let build ~old_start ~new_start wire_lines =
    let old_line = ref old_start in
    let new_line = ref new_start in
    let old_count = ref 0 in
    let new_count = ref 0 in
    let derive { wire_kind; content; newline } : Line.t =
      match wire_kind with
      | Line.Context ->
          let old_no = !old_line and new_no = !new_line in
          incr old_line;
          incr new_line;
          incr old_count;
          incr new_count;
          {
            Line.kind = Line.Context;
            text = content;
            newline;
            old_line = Some old_no;
            new_line = Some new_no;
          }
      | Line.Removed ->
          let old_no = !old_line in
          incr old_line;
          incr old_count;
          {
            Line.kind = Line.Removed;
            text = content;
            newline;
            old_line = Some old_no;
            new_line = None;
          }
      | Line.Added ->
          let new_no = !new_line in
          incr new_line;
          incr new_count;
          {
            Line.kind = Line.Added;
            text = content;
            newline;
            old_line = None;
            new_line = Some new_no;
          }
    in
    let lines = List.map derive wire_lines in
    {
      old_start;
      old_count = !old_count;
      new_start;
      new_count = !new_count;
      lines;
    }

  let kind_jsont =
    Jsont.enum ~kind:"diff line kind"
      [
        ("context", Line.Context);
        ("added", Line.Added);
        ("removed", Line.Removed);
      ]

  (* On the wire a line's content is exactly one of two members: a readable
     [text] string when the raw bytes are valid UTF-8, a lowercase-hex [hex]
     string otherwise. The UTF-8 test runs once, when a line is projected for
     encoding ({!coded_of_line}), and is read back for free from the present
     member on decode. This classification is confined to the codec: the
     structured {!Textdiff.hunks} path builds from plain {!wire_line}
     values and never runs it. *)
  type wire_content = Text of string | Hex of string

  type coded_line = {
    coded_kind : Line.kind;
    coded_content : wire_content;
    coded_newline : bool;
  }

  let coded_of_line (line : Line.t) =
    let text = Line.text line in
    let coded_content =
      if String.is_valid_utf_8 text then Text text else Hex text
    in
    {
      coded_kind = Line.kind line;
      coded_content;
      coded_newline = Line.newline line;
    }

  let wire_line_of_coded { coded_kind; coded_content; coded_newline } =
    let content = match coded_content with Text s | Hex s -> s in
    { wire_kind = coded_kind; content; newline = coded_newline }

  let coded_line_jsont =
    Jsont.Object.map ~kind:"diff hunk line"
      (fun coded_kind text hex coded_newline ->
        let coded_content =
          match (text, hex) with
          | Some text, None -> Text text
          | None, Some hex -> Hex hex
          | Some _, Some _ ->
              Jsont.Error.msg Jsont.Meta.none
                {|diff hunk line has both "text" and "hex"|}
          | None, None ->
              Jsont.Error.msg Jsont.Meta.none
                {|diff hunk line has neither "text" nor "hex"|}
        in
        { coded_kind; coded_content; coded_newline })
    |> Jsont.Object.mem "kind" kind_jsont ~enc:(fun l -> l.coded_kind)
    |> Jsont.Object.opt_mem "text" Jsont.string ~enc:(fun l ->
        match l.coded_content with Text s -> Some s | Hex _ -> None)
    |> Jsont.Object.opt_mem "hex" Jsont.binary_string ~enc:(fun l ->
        match l.coded_content with Hex s -> Some s | Text _ -> None)
    |> Jsont.Object.mem "newline" Jsont.bool ~enc:(fun l -> l.coded_newline)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let jsont =
    Jsont.Object.map ~kind:"diff hunk" (fun old_start new_start coded_lines ->
        build ~old_start ~new_start (List.map wire_line_of_coded coded_lines))
    |> Jsont.Object.mem "old_start" Jsont.int ~enc:old_start
    |> Jsont.Object.mem "new_start" Jsont.int ~enc:new_start
    |> Jsont.Object.mem "lines" (Jsont.list coded_line_jsont) ~enc:(fun hunk ->
        List.map coded_of_line (lines hunk))
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Merge = struct
  module Region = struct
    type t =
      | Stable of string
      | Resolved of string
      | Conflict of { base : string; ours : string; theirs : string }

    let equal a b =
      match (a, b) with
      | Stable x, Stable y | Resolved x, Resolved y -> String.equal x y
      | Conflict a, Conflict b ->
          String.equal a.base b.base && String.equal a.ours b.ours
          && String.equal a.theirs b.theirs
      | (Stable _ | Resolved _ | Conflict _), _ -> false

    (* A byte-exact string member, the same encoding {!Hunk.jsont} gives a
       line's content: a readable [text] when the bytes are valid UTF-8, a
       lowercase-hex [hex] otherwise, exactly one present. *)
    let content_jsont =
      Jsont.Object.map ~kind:"merge content" (fun text hex ->
          match (text, hex) with
          | Some text, None -> text
          | None, Some hex -> hex
          | Some _, Some _ ->
              Jsont.Error.msg Jsont.Meta.none
                {|merge content has both "text" and "hex"|}
          | None, None ->
              Jsont.Error.msg Jsont.Meta.none
                {|merge content has neither "text" nor "hex"|})
      |> Jsont.Object.opt_mem "text" Jsont.string ~enc:(fun s ->
          if String.is_valid_utf_8 s then Some s else None)
      |> Jsont.Object.opt_mem "hex" Jsont.binary_string ~enc:(fun s ->
          if String.is_valid_utf_8 s then None else Some s)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish

    let stable_case =
      Jsont.Object.map ~kind:"stable region" (fun content -> Stable content)
      |> Jsont.Object.mem "content" content_jsont ~enc:(function
        | Stable content -> content
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "stable" ~dec:Fun.id

    let resolved_case =
      Jsont.Object.map ~kind:"resolved region" (fun content -> Resolved content)
      |> Jsont.Object.mem "content" content_jsont ~enc:(function
        | Resolved content -> content
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "resolved" ~dec:Fun.id

    let conflict_case =
      Jsont.Object.map ~kind:"conflict region" (fun base ours theirs ->
          Conflict { base; ours; theirs })
      |> Jsont.Object.mem "base" content_jsont ~enc:(function
        | Conflict c -> c.base
        | _ -> assert false)
      |> Jsont.Object.mem "ours" content_jsont ~enc:(function
        | Conflict c -> c.ours
        | _ -> assert false)
      |> Jsont.Object.mem "theirs" content_jsont ~enc:(function
        | Conflict c -> c.theirs
        | _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "conflict" ~dec:Fun.id

    let jsont =
      let cases =
        List.map Jsont.Object.Case.make
          [ stable_case; resolved_case; conflict_case ]
      in
      let enc_case = function
        | Stable _ as region -> Jsont.Object.Case.value stable_case region
        | Resolved _ as region -> Jsont.Object.Case.value resolved_case region
        | Conflict _ as region -> Jsont.Object.Case.value conflict_case region
      in
      Jsont.Object.map ~kind:"merge region" Fun.id
      |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
  end

  type t = { regions : Region.t list }

  (* The byte-exact text of [lines.(lo)] through [lines.(hi - 1)], newline flags
     preserved (a final line without its newline stays so). *)
  let slice_text lines lo hi =
    let buffer = Buffer.create 256 in
    for i = lo to hi - 1 do
      let line = lines.(i) in
      Buffer.add_string buffer line.content;
      if line.newline then Buffer.add_char buffer '\n'
    done;
    Buffer.contents buffer

  (* The maximal change blocks of an edit script relative to the base, each as
     [(base_start, base_len, derived_start, derived_len)]. A block groups the
     deletions and insertions between two kept lines; consecutive keeps carry no
     block. *)
  let change_blocks ops =
    let base = ref 0
    and derived = ref 0
    and blocks = ref []
    and cur = ref None in
    let flush () =
      match !cur with
      | Some block ->
          blocks := block :: !blocks;
          cur := None
      | None -> ()
    in
    List.iter
      (function
        | Keep_line _ ->
            flush ();
            incr base;
            incr derived
        | Delete_line _ ->
            (match !cur with
            | None -> cur := Some (!base, 1, !derived, 0)
            | Some (bs, bl, ds, dl) -> cur := Some (bs, bl + 1, ds, dl));
            incr base
        | Insert_line _ ->
            (match !cur with
            | None -> cur := Some (!base, 0, !derived, 1)
            | Some (bs, bl, ds, dl) -> cur := Some (bs, bl, ds, dl + 1));
            incr derived)
      ops;
    flush ();
    List.rev !blocks

  let v ?max_edit_distance ~base ~ours ~theirs () =
    let base_l = split_lines base in
    let ours_l = split_lines ours in
    let theirs_l = split_lines theirs in
    match
      ( edit_script ?max_distance:max_edit_distance base_l ours_l,
        edit_script ?max_distance:max_edit_distance base_l theirs_l )
    with
    | Some ours_ops, Some theirs_ops ->
        let tag ours blocks = List.map (fun block -> (ours, block)) blocks in
        let combined =
          tag true (change_blocks ours_ops)
          @ tag false (change_blocks theirs_ops)
          |> List.sort (fun (_, (b1, l1, _, _)) (_, (b2, l2, _, _)) ->
              match Int.compare b1 b2 with 0 -> Int.compare l1 l2 | c -> c)
        in
        let base_len = Array.length base_l in
        let classify base_txt ours_txt theirs_txt =
          let ours_changed = not (String.equal ours_txt base_txt) in
          let theirs_changed = not (String.equal theirs_txt base_txt) in
          match (ours_changed, theirs_changed) with
          | false, false -> Region.Stable base_txt
          | true, false -> Region.Resolved ours_txt
          | false, true -> Region.Resolved theirs_txt
          | true, true ->
              if String.equal ours_txt theirs_txt then Region.Resolved ours_txt
              else
                Region.Conflict
                  { base = base_txt; ours = ours_txt; theirs = theirs_txt }
        in
        let rec gather members region_end = function
          | (side, (bs, bl, ds, dl)) :: more when bs <= region_end ->
              gather
                ((side, (bs, bl, ds, dl)) :: members)
                (max region_end (bs + bl))
                more
          | rest -> (members, region_end, rest)
        in
        let rec sweep ~curr ~ours_der ~theirs_der acc = function
          | [] ->
              let acc =
                if curr < base_len then
                  Region.Stable (slice_text base_l curr base_len) :: acc
                else acc
              in
              List.rev acc
          | (_, (region_start, _, _, _)) :: _ as blocks ->
              let members, region_end, rest = gather [] region_start blocks in
              let acc, ours_der, theirs_der =
                if region_start > curr then
                  let stable = region_start - curr in
                  ( Region.Stable (slice_text base_l curr region_start) :: acc,
                    ours_der + stable,
                    theirs_der + stable )
                else (acc, ours_der, theirs_der)
              in
              let region_base_len = region_end - region_start in
              let delta ours =
                List.fold_left
                  (fun total (side, (_, bl, _, dl)) ->
                    if Bool.equal side ours then total + dl - bl else total)
                  0 members
              in
              let ours_delta = delta true and theirs_delta = delta false in
              let base_txt = slice_text base_l region_start region_end in
              let ours_txt =
                slice_text ours_l ours_der
                  (ours_der + region_base_len + ours_delta)
              in
              let theirs_txt =
                slice_text theirs_l theirs_der
                  (theirs_der + region_base_len + theirs_delta)
              in
              sweep ~curr:region_end
                ~ours_der:(ours_der + region_base_len + ours_delta)
                ~theirs_der:(theirs_der + region_base_len + theirs_delta)
                (classify base_txt ours_txt theirs_txt :: acc)
                rest
        in
        Some { regions = sweep ~curr:0 ~ours_der:0 ~theirs_der:0 [] combined }
    | Some _, None | None, _ -> None

  let regions t = t.regions

  let is_clean t =
    List.for_all
      (function
        | Region.Conflict _ -> false
        | Region.Stable _ | Region.Resolved _ -> true)
      t.regions

  let conflicts t =
    List.filter
      (function
        | Region.Conflict _ -> true
        | Region.Stable _ | Region.Resolved _ -> false)
      t.regions

  let resolved t =
    if is_clean t then
      Some
        (String.concat ""
           (List.map
              (function
                | Region.Stable content | Region.Resolved content -> content
                | Region.Conflict _ -> assert false)
              t.regions))
    else None

  let equal a b = List.equal Region.equal a.regions b.regions

  let render_markers t =
    let buffer = Buffer.create 256 in
    let ensure_newline () =
      let length = Buffer.length buffer in
      if length > 0 && not (Char.equal (Buffer.nth buffer (length - 1)) '\n')
      then Buffer.add_char buffer '\n'
    in
    List.iter
      (function
        | Region.Stable content | Region.Resolved content ->
            Buffer.add_string buffer content
        | Region.Conflict { ours; theirs; _ } ->
            ensure_newline ();
            Buffer.add_string buffer "<<<<<<< ours\n";
            Buffer.add_string buffer ours;
            ensure_newline ();
            Buffer.add_string buffer "=======\n";
            Buffer.add_string buffer theirs;
            ensure_newline ();
            Buffer.add_string buffer ">>>>>>> theirs\n")
      t.regions;
    Buffer.contents buffer

  let jsont =
    Jsont.Object.map ~kind:"merge" (fun regions -> { regions })
    |> Jsont.Object.mem "regions" (Jsont.list Region.jsont) ~enc:(fun t ->
        t.regions)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

let hunk_wire_lines (hunk : hunk) =
  (* Regroup each change block so removals precede additions, matching rendered
     unified output; relative order within each side is preserved. Line numbering
     and counts are {!Hunk.build}'s single responsibility, shared with the wire
     decoder, so this produces only the unnumbered generating lines. *)
  let out = ref [] in
  let deletions = ref [] in
  let insertions = ref [] in
  let wire kind (l : line) : Hunk.wire_line =
    { Hunk.wire_kind = kind; content = l.content; newline = l.newline }
  in
  let flush () =
    List.iter
      (fun l -> out := wire Hunk.Line.Removed l :: !out)
      (List.rev !deletions);
    List.iter
      (fun l -> out := wire Hunk.Line.Added l :: !out)
      (List.rev !insertions);
    deletions := [];
    insertions := []
  in
  List.iter
    (fun (prefix, l) ->
      match prefix with
      | '-' -> deletions := l :: !deletions
      | '+' -> insertions := l :: !insertions
      | _ ->
          flush ();
          out := wire Hunk.Line.Context l :: !out)
    hunk.lines;
  flush ();
  List.rev !out

let hunks ?(context = 3) ?max_edit_distance ~before ~after () =
  let context = validate_context context in
  Option.iter
    (fun max_edit_distance ->
      if max_edit_distance < 0 then
        invalid_arg "max_edit_distance must be non-negative")
    max_edit_distance;
  match
    edit_script ?max_distance:max_edit_distance (split_lines before)
      (split_lines after)
  with
  | None -> None
  | Some ops ->
      Some
        (List.map
           (fun (hunk : hunk) ->
             Hunk.build ~old_start:hunk.before_start ~new_start:hunk.after_start
               (hunk_wire_lines hunk))
           (hunks_of_ops ~context ops))

let hunk_start start len = if len = 0 then start - 1 else start
let add_int buffer n = Buffer.add_string buffer (string_of_int n)

let add_line buffer prefix line =
  Buffer.add_char buffer prefix;
  add_display_string buffer line.content;
  Buffer.add_char buffer '\n';
  if not line.newline then
    Buffer.add_string buffer "\\ No newline at end of file\n"

let render_hunk buffer (hunk : hunk) =
  Buffer.add_string buffer "@@ -";
  add_int buffer (hunk_start hunk.before_start hunk.before_len);
  Buffer.add_char buffer ',';
  add_int buffer hunk.before_len;
  Buffer.add_string buffer " +";
  add_int buffer (hunk_start hunk.after_start hunk.after_len);
  Buffer.add_char buffer ',';
  add_int buffer hunk.after_len;
  Buffer.add_string buffer " @@\n";
  let deletions = ref [] in
  let insertions = ref [] in
  let flush_changes () =
    List.iter (fun line -> add_line buffer '-' line) (List.rev !deletions);
    List.iter (fun line -> add_line buffer '+' line) (List.rev !insertions);
    deletions := [];
    insertions := []
  in
  List.iter
    (fun (prefix, line) ->
      match prefix with
      | '-' -> deletions := line :: !deletions
      | '+' -> insertions := line :: !insertions
      | _ ->
          flush_changes ();
          add_line buffer ' ' line)
    hunk.lines;
  flush_changes ()

let add_file_header buffer ~before_label ~after_label =
  Buffer.add_string buffer "--- ";
  add_display_string buffer before_label;
  Buffer.add_char buffer '\n';
  Buffer.add_string buffer "+++ ";
  add_display_string buffer after_label;
  Buffer.add_char buffer '\n'

let stats_for_ops ops =
  let additions = ref 0 in
  let deletions = ref 0 in
  List.iter
    (function
      | Insert_line _ -> incr additions
      | Delete_line _ -> incr deletions
      | Keep_line _ -> ())
    ops;
  { files = 1; additions = !additions; deletions = !deletions }

let line_count text =
  let len = String.length text in
  let rec loop count i =
    if i = len then
      if len > 0 && not (Char.equal text.[len - 1] '\n') then count + 1
      else count
    else if Char.equal text.[i] '\n' then loop (count + 1) (i + 1)
    else loop count (i + 1)
  in
  loop 0 0

let ops_of_lines op lines =
  let ops = ref [] in
  for i = Array.length lines - 1 downto 0 do
    ops := op lines.(i) :: !ops
  done;
  !ops

let pure_change_ops ?max_distance op contents =
  let distance = line_count contents in
  match max_distance with
  | Some max_distance when distance > max_distance -> None
  | None | Some _ -> Some (ops_of_lines op (split_lines contents))

let ops_for_file_text ?max_distance text =
  match (text.before_state, text.after_state) with
  | None, None -> Some []
  | None, Some after ->
      pure_change_ops ?max_distance (fun l -> Insert_line l) after
  | Some before, None ->
      pure_change_ops ?max_distance (fun l -> Delete_line l) before
  | Some _, Some _ ->
      edit_script ?max_distance
        (split_lines text.before_text)
        (split_lines text.after_text)

let diff_label label = function None -> "/dev/null" | Some _ -> label

let stats_for_file file =
  match file with
  | File_change.Add { contents; _ } ->
      { files = 1; additions = line_count contents; deletions = 0 }
  | File_change.Delete { contents; _ } ->
      { files = 1; additions = 0; deletions = line_count contents }
  | File_change.Modify { before; after; _ } ->
      if String.equal before after then empty_stats
      else edit_stats (split_lines before) (split_lines after)

let add_omission buffer reason =
  Buffer.add_string buffer "[diff omitted: ";
  Buffer.add_string buffer reason;
  Buffer.add_string buffer "]\n"

let max_text_bytes text =
  max (String.length text.before_text) (String.length text.after_text)

let max_text_lines text =
  max (line_count text.before_text) (line_count text.after_text)

let render_omitted_file ~omitted buffer file text reason =
  let label = Label.to_string (File_change.label file) in
  let before_label = diff_label label text.before_state in
  let after_label = diff_label label text.after_state in
  add_file_header buffer ~before_label ~after_label;
  add_omission buffer reason;
  incr omitted;
  { files = 1; additions = 0; deletions = 0 }

let render_file_text_into ?limits ~context ~omitted buffer file text =
  let label = Label.to_string (File_change.label file) in
  match limits with
  | Some limits when max_text_bytes text > limits.Limits.max_file_bytes ->
      render_omitted_file ~omitted buffer file text
        (Printf.sprintf "file exceeds %d byte display limit"
           limits.Limits.max_file_bytes)
  | Some limits when max_text_lines text > limits.Limits.max_lines ->
      render_omitted_file ~omitted buffer file text
        (Printf.sprintf "file exceeds %d line display limit"
           limits.Limits.max_lines)
  | _ -> (
      let max_distance =
        Option.bind limits (fun limits -> limits.Limits.max_edit_distance)
      in
      match ops_for_file_text ?max_distance text with
      | None -> (
          match max_distance with
          | Some max_distance ->
              render_omitted_file ~omitted buffer file text
                (Printf.sprintf "edit distance exceeds %d display limit"
                   max_distance)
          | None -> assert false)
      | Some ops ->
          let file_stats = stats_for_ops ops in
          let before_label = diff_label label text.before_state in
          let after_label = diff_label label text.after_state in
          add_file_header buffer ~before_label ~after_label;
          List.iter (render_hunk buffer) (hunks_of_ops ~context ops);
          file_stats)

let render_file_into ?limits ~context ~omitted buffer file =
  if is_noop file then empty_stats
  else
    render_file_text_into ?limits ~context ~omitted buffer file (file_text file)

let add_stats a b =
  {
    files = a.files + b.files;
    additions = a.additions + b.additions;
    deletions = a.deletions + b.deletions;
  }

let count_non_noop files =
  List.fold_left
    (fun count file -> if is_noop file then count else count + 1)
    0 files

let render ?limits ?(context = 3) files =
  let context = validate_context context in
  let buffer = Buffer.create 2048 in
  let omitted = ref 0 in
  let max_files = Option.map (fun limits -> limits.Limits.max_files) limits in
  let rec loop rendered stats = function
    | [] -> stats
    | file :: files when is_noop file -> loop rendered stats files
    | file :: files -> (
        match max_files with
        | Some max_files when rendered >= max_files ->
            let remaining = 1 + count_non_noop files in
            add_omission buffer
              (if remaining = 1 then "1 file exceeds max_files display limit"
               else
                 Printf.sprintf "%d files exceed max_files display limit"
                   remaining);
            omitted := !omitted + remaining;
            add_stats stats { files = remaining; additions = 0; deletions = 0 }
        | None | Some _ ->
            let file_stats =
              render_file_into ?limits ~context ~omitted buffer file
            in
            loop (rendered + 1) (add_stats stats file_stats) files)
  in
  let stats = loop 0 empty_stats files in
  { text = Buffer.contents buffer; stats; omitted = !omitted }

let stats t = t.stats
let omitted t = t.omitted
let to_string t = t.text

module Stats = struct
  type t = stats = { files : int; additions : int; deletions : int }

  let v ~files ~additions ~deletions =
    if files < 0 || additions < 0 || deletions < 0 then
      invalid_arg "Textdiff.Stats.v: counts must be non-negative";
    { files; additions; deletions }

  let jsont =
    let dec files additions deletions =
      if files < 0 || additions < 0 || deletions < 0 then
        Jsont.Error.msg Jsont.Meta.none "diff stats counts must be non-negative"
      else { files; additions; deletions }
    in
    Jsont.Object.map ~kind:"diff stats" dec
    |> Jsont.Object.mem "files" Jsont.int ~enc:(fun s -> s.files)
    |> Jsont.Object.mem "additions" Jsont.int ~enc:(fun s -> s.additions)
    |> Jsont.Object.mem "deletions" Jsont.int ~enc:(fun s -> s.deletions)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let equal a b =
    Int.equal a.files b.files
    && Int.equal a.additions b.additions
    && Int.equal a.deletions b.deletions

  let of_changes files =
    List.fold_left
      (fun stats file -> add_stats stats (stats_for_file file))
      empty_stats files
end
