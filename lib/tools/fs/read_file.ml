(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let name = "read_file"

let json_object fields =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       fields)

module Output = struct
  let max_file_bytes = 1_048_576
  let max_directory_entries = 1_000
  let display_line_bytes = 2_000
  let invalid fn message = invalid_arg ("read_file " ^ fn ^ ": " ^ message)

  module Continuation = struct
    type t = {
      path : string;
      offset : int;
      limit : int;
      max_bytes : int option;
    }

    let make ~path ~offset ~limit ?max_bytes () =
      if String.is_empty path then invalid "continuation" "empty path";
      if offset < 1 then invalid "continuation" "offset must be at least 1";
      if limit < 1 || limit > max_directory_entries then
        invalid "continuation"
          (Printf.sprintf "limit must be between 1 and %d" max_directory_entries);
      (match max_bytes with
      | Some max_bytes when max_bytes < 0 || max_bytes > max_file_bytes ->
          invalid "continuation"
            (Printf.sprintf "max_bytes must be between 0 and %d" max_file_bytes)
      | Some _ | None -> ());
      { path; offset; limit; max_bytes }

    let to_json continuation =
      let fields =
        [
          ("path", Jsont.Json.string continuation.path);
          ("offset", Jsont.Json.int continuation.offset);
          ("limit", Jsont.Json.int continuation.limit);
        ]
      in
      let fields =
        match continuation.max_bytes with
        | None -> fields
        | Some max_bytes -> fields @ [ ("max_bytes", Jsont.Json.int max_bytes) ]
      in
      json_object fields
  end

  type line_count = Exact of int | Lower_bound of int
  type partial_reason = Ranged | Byte_capped | Ranged_and_byte_capped

  type status =
    | Complete of Mentat_digest.Content_ref.t
    | Partial of { reason : partial_reason; next : Continuation.t option }

  type entry_kind = Regular_file | Directory | Symlink | Other

  type entry = {
    entry_path : Mentat_workspace.Path.t;
    entry_name : string;
    entry_kind : entry_kind;
  }

  type t = {
    text : string;
    semantic : Mentat_tools_output.Read.t;
    media : Mentat_llm.Content.t list;
    truncated : bool;
  }

  let validate_line_count = function
    | (Exact count | Lower_bound count) when count < 0 ->
        invalid "file" "total line count must be non-negative"
    | Exact _ | Lower_bound _ -> ()

  let partial_reason_string = function
    | Ranged -> "ranged"
    | Byte_capped -> "byte_capped"
    | Ranged_and_byte_capped -> "ranged_and_byte_capped"

  let byte_capped = function
    | Complete _ -> false
    | Partial { reason = Byte_capped | Ranged_and_byte_capped; _ } -> true
    | Partial { reason = Ranged; _ } -> false

  let complete = function Complete _ -> true | Partial _ -> false

  let line_count_text = function
    | Exact count -> string_of_int count
    | Lower_bound count -> ">=" ^ string_of_int count

  let logical_lines text =
    if String.is_empty text then []
    else
      let lines = String.split_on_char '\n' text in
      if Char.equal text.[String.length text - 1] '\n' then
        List.rev (List.tl (List.rev lines))
      else lines

  let display_line line =
    if String.length line <= display_line_bytes then (line, false)
    else
      ( Text_helpers.valid_utf8_prefix line display_line_bytes
        ^ " [line truncated]",
        true )

  let rendered_lines ~start_line contents =
    let buffer = Buffer.create (String.length contents + 32) in
    let truncated = ref false in
    let lines = logical_lines contents in
    List.iteri
      (fun index line ->
        let line = Text_helpers.strip_trailing_cr line in
        let displayed, line_truncated = display_line line in
        if line_truncated then truncated := true;
        Buffer.add_string buffer (string_of_int (start_line + index));
        Buffer.add_char buffer '\t';
        Buffer.add_string buffer displayed;
        Buffer.add_char buffer '\n')
      lines;
    (Buffer.contents buffer, List.length lines, !truncated)

  let line_range_text ~start_line ~returned_lines ~total_lines =
    if returned_lines = 0 then
      let empty = "empty at " ^ string_of_int start_line in
      match total_lines with
      | Exact total when start_line > total ->
          Printf.sprintf "%s (past EOF; file has %d lines)" empty total
      | Exact _ | Lower_bound _ -> empty
    else Printf.sprintf "%d-%d" start_line (start_line + returned_lines - 1)

  let encode_json json =
    match Jsont_bytesrw.encode_string Jsont.json json with
    | Ok text -> text
    | Error message -> invalid "continuation" message

  let continuation_text = function
    | None -> ""
    | Some continuation ->
        "next: " ^ name ^ " " ^ encode_json (Continuation.to_json continuation)

  let file ~path ~contents ~start_line ~total_lines ~status =
    if String.length contents > max_file_bytes then
      invalid "file"
        (Printf.sprintf "contents exceed the %d-byte bound" max_file_bytes);
    if not (String.is_valid_utf_8 contents) then
      invalid "file" "contents must be valid UTF-8";
    if start_line < 1 then invalid "file" "start_line must be at least 1";
    validate_line_count total_lines;
    let lines, returned_lines, display_truncated =
      rendered_lines ~start_line contents
    in
    (match total_lines with
    | Exact total
      when returned_lines > 0 && start_line + returned_lines - 1 > total ->
        invalid "file" "exact total is smaller than the returned line range"
    | Exact _ | Lower_bound _ -> ());
    (match status with
    | Complete identity ->
        if start_line <> 1 then
          invalid "file" "a complete file must start at line 1";
        (match total_lines with
        | Exact total when total = returned_lines -> ()
        | Exact _ | Lower_bound _ ->
            invalid "file"
              "a complete file must have an exact total equal to its returned \
               line count");
        if
          not
            (Mentat_digest.Content_ref.equal identity
               (Mentat_digest.Content_ref.of_contents contents))
        then invalid "file" "complete identity does not match contents"
    | Partial _ -> ());
    let reason =
      match status with
      | Complete _ -> ""
      | Partial { reason; _ } -> " reason=" ^ partial_reason_string reason
    in
    let identity =
      match status with
      | Complete identity ->
          " identity=" ^ Mentat_digest.Content_ref.to_token identity
      | Partial _ -> ""
    in
    let header =
      Printf.sprintf "%s lines=%s returned=%d/%s status=%s%s%s"
        (Mentat_workspace.Path.display path)
        (line_range_text ~start_line ~returned_lines ~total_lines)
        returned_lines
        (line_count_text total_lines)
        (if complete status then "complete" else "partial")
        reason identity
    in
    let next =
      match status with
      | Complete _ -> ""
      | Partial { next; _ } -> continuation_text next
    in
    let next = if String.is_empty next then "" else next ^ "\n" in
    {
      text = header ^ "\n" ^ lines ^ next;
      semantic = Mentat_tools_output.Read.file ~lines:returned_lines;
      media = [];
      truncated = byte_capped status || display_truncated;
    }

  let unchanged ~path ~identity =
    let text =
      Printf.sprintf "%s unchanged identity=%s\n"
        (Mentat_workspace.Path.display path)
        (Mentat_digest.Content_ref.to_token identity)
    in
    {
      text;
      semantic = Mentat_tools_output.Read.unchanged;
      media = [];
      truncated = false;
    }

  (* An image file returned to a vision model: a short text description plus the
     bytes as base64 media. The read semantic is [Read.Image] — the model content
     is the media, not text lines, and the fact says so honestly. The engine
     strips the media when the active model or channel cannot carry it. *)
  let image ~path ~format ~bytes ~dimension_text =
    let media_type = Mentat_llm.Image.Format.media_type format in
    {
      text =
        Printf.sprintf "%s %s image%s (%d bytes)"
          (Mentat_workspace.Path.display path)
          media_type dimension_text (String.length bytes);
      semantic = Mentat_tools_output.Read.image;
      media =
        [
          Mentat_llm.Content.media ~media_type
            (`Base64 (Base64.encode_string bytes));
        ];
      truncated = false;
    }

  let entry_suffix = function
    | Regular_file -> ""
    | Directory -> "/"
    | Symlink -> "@"
    | Other -> "?"

  let listing ~path ~entries ~offset ~limit ~total_entries ~complete ~next =
    let returned_entries = List.length entries in
    if returned_entries > max_directory_entries then
      invalid "listing"
        (Printf.sprintf "entries exceed the %d-entry bound"
           max_directory_entries);
    if offset < 1 then invalid "listing" "offset must be at least 1";
    if limit < 1 || limit > max_directory_entries then
      invalid "listing"
        (Printf.sprintf "limit must be between 1 and %d" max_directory_entries);
    if returned_entries > limit then
      invalid "listing" "returned entries exceed the page limit";
    if total_entries < 0 || total_entries < returned_entries then
      invalid "listing" "total_entries is smaller than the returned page";
    (match (complete, next) with
    | true, None | false, Some _ -> ()
    | true, Some _ ->
        invalid "listing" "a complete page cannot have a next call"
    | false, None ->
        invalid "listing" "an incomplete page must have a next call");
    let buffer = Buffer.create 256 in
    Printf.bprintf buffer "%s entries=%d/%d offset=%d limit=%d status=%s\n"
      (Mentat_workspace.Path.display path)
      returned_entries total_entries offset limit
      (if complete then "complete" else "partial");
    List.iter
      (fun entry ->
        Buffer.add_string buffer
          (Mentat_workspace.Path.display entry.entry_path);
        Buffer.add_string buffer (entry_suffix entry.entry_kind);
        Buffer.add_char buffer '\n')
      entries;
    (match next with
    | None -> ()
    | Some continuation ->
        Buffer.add_string buffer (continuation_text (Some continuation));
        Buffer.add_char buffer '\n');
    {
      text = Buffer.contents buffer;
      semantic = Mentat_tools_output.Read.listing ~entries:returned_entries;
      media = [];
      truncated = not complete;
    }

  let encode output =
    Mentat_tools_output.Codec.encode Mentat_tools_output.Read.jsont
      ~text:output.text ~media:output.media ~truncated:output.truncated
      output.semantic
end

let max_file_bytes = Output.max_file_bytes
let default_max_bytes = 64 * 1024
let max_directory_limit = Output.max_directory_entries
let default_directory_limit = 200
let read_chunk_bytes = 16_384

(* Image caps default to the [image.*] config defaults; the executable overrides
   them from resolved config at construction. Unlike attach, a read has no
   process-spawn boundary, so it cannot downscale — an over-cap image is rejected
   with an instructive message rather than shrunk. *)
let default_image_max_bytes = 5 * 1024 * 1024
let default_image_max_dimension = 8000

module Input = struct
  type t = {
    path : string;
    offset : int option;
    limit : int option;
    max_bytes : int option;
    if_identity : Mentat_digest.Content_ref.t option;
  }

  let make ~path ~offset ~limit ~max_bytes ~if_identity =
    if String.is_empty path then invalid_arg "path must not be empty";
    (match offset with
    | Some offset when offset < 1 -> invalid_arg "offset must be at least 1"
    | Some _ | None -> ());
    (match limit with
    | Some limit when limit < 1 -> invalid_arg "limit must be positive"
    | Some limit when limit > max_directory_limit ->
        invalid_arg
          ("limit must be at most " ^ string_of_int max_directory_limit)
    | Some _ | None -> ());
    (match max_bytes with
    | Some max_bytes when max_bytes < 0 ->
        invalid_arg "max_bytes must be non-negative"
    | Some max_bytes when max_bytes > max_file_bytes ->
        invalid_arg ("max_bytes must be at most " ^ string_of_int max_file_bytes)
    | Some _ | None -> ());
    (match if_identity with
    | Some _
      when Option.is_some offset || Option.is_some limit
           || Option.is_some max_bytes ->
        invalid_arg
          "if_identity can only be used with an unwindowed complete-file read"
    | Some _ | None -> ());
    let offset =
      match (offset, limit) with
      | None, Some _ -> Some 1
      | Some _, (Some _ | None) | None, None -> offset
    in
    { path; offset; limit; max_bytes; if_identity }

  let identity_of_string = function
    | None -> None
    | Some token -> (
        match Mentat_digest.Content_ref.of_token token with
        | Ok identity -> Some identity
        | Error error ->
            invalid_arg
              ("if_identity is not a file identity: "
              ^ Mentat_digest.Error.message error))

  let max_input_integer =
    Float.min 9_007_199_254_740_991. (float_of_int Int.max_int)

  (* [Jsont.int] also accepts numeric strings and truncates fractional numbers.
     Tool inputs instead follow JSON Schema's integer semantics and stay within
     both OCaml's integer range and the range in which a JSON number represents
     every integer exactly. *)
  let exact_integer =
    let decode = function
      | Jsont.Number (value, _)
        when Float.is_integer value && Float.abs value <= max_input_integer ->
          int_of_float value
      | Jsont.Number _ | Jsont.Null _ | Jsont.Bool _ | Jsont.String _
      | Jsont.Array _ | Jsont.Object _ ->
          Jsont.Error.msg Jsont.Meta.none
            "expected an integer in JSON's safe integer range"
    in
    Jsont.map ~kind:"integer" ~dec:decode
      ~enc:(fun value -> Jsont.Json.int value)
      Jsont.json

  let codec ~conditional_read =
    let object_codec =
      if conditional_read then
        Jsont.Object.map ~kind:"read_file input"
          (fun path offset limit max_bytes if_identity ->
            Mentat_tool.Codec.decode_invalid_arg (fun () ->
                make ~path ~offset ~limit ~max_bytes
                  ~if_identity:(identity_of_string if_identity)))
        |> Jsont.Object.mem "path" Jsont.string ~enc:(fun input -> input.path)
        |> Jsont.Object.opt_mem "offset" exact_integer ~enc:(fun input ->
            input.offset)
        |> Jsont.Object.opt_mem "limit" exact_integer ~enc:(fun input ->
            input.limit)
        |> Jsont.Object.opt_mem "max_bytes" exact_integer ~enc:(fun input ->
            input.max_bytes)
        |> Jsont.Object.opt_mem "if_identity" Jsont.string ~enc:(fun input ->
            Option.map Mentat_digest.Content_ref.to_token input.if_identity)
        |> Jsont.Object.error_unknown |> Jsont.Object.finish
      else
        Jsont.Object.map ~kind:"read_file input"
          (fun path offset limit max_bytes ->
            Mentat_tool.Codec.decode_invalid_arg (fun () ->
                make ~path ~offset ~limit ~max_bytes ~if_identity:None))
        |> Jsont.Object.mem "path" Jsont.string ~enc:(fun input -> input.path)
        |> Jsont.Object.opt_mem "offset" exact_integer ~enc:(fun input ->
            input.offset)
        |> Jsont.Object.opt_mem "limit" exact_integer ~enc:(fun input ->
            input.limit)
        |> Jsont.Object.opt_mem "max_bytes" exact_integer ~enc:(fun input ->
            input.max_bytes)
        |> Jsont.Object.error_unknown |> Jsont.Object.finish
    in
    Codec.strict_object ~kind:"strict read_file input" object_codec

  let schema_property kind description fields =
    json_object
      (("type", Jsont.Json.string kind)
      :: ("description", Jsont.Json.string description)
      :: fields)

  let schema ~conditional_read =
    let properties =
      [
        ( "path",
          schema_property "string"
            "Workspace-relative or workspace-contained absolute path to read."
            [] );
        ( "offset",
          schema_property "integer"
            "One-based first file line or directory entry. Defaults to 1."
            [
              ("minimum", Jsont.Json.int 1);
              ("maximum", Jsont.Json.number max_input_integer);
            ] );
        ( "limit",
          schema_property "integer"
            "Maximum file lines or directory entries to return."
            [
              ("minimum", Jsont.Json.int 1);
              ("maximum", Jsont.Json.int max_directory_limit);
            ] );
        ( "max_bytes",
          schema_property "integer" "Maximum UTF-8 bytes returned for a file."
            [
              ("minimum", Jsont.Json.int 0);
              ("maximum", Jsont.Json.int max_file_bytes);
            ] );
      ]
    in
    let properties =
      if conditional_read then
        properties
        @ [
            ( "if_identity",
              schema_property "string"
                "Complete-file identity from a previous read. Valid only \
                 without offset, limit, or max_bytes."
                [] );
          ]
      else properties
    in
    json_object
      [
        ("type", Jsont.Json.string "object");
        ("properties", json_object properties);
        ("required", Jsont.Json.list [ Jsont.Json.string "path" ]);
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let contract ~conditional_read =
    Mentat_tool.Input.make (codec ~conditional_read)
      ~schema:(schema ~conditional_read)

  let start_line input = Option.value input.offset ~default:1

  let effective_max_bytes input =
    Option.value input.max_bytes ~default:default_max_bytes

  let effective_directory_limit input =
    Option.value input.limit ~default:default_directory_limit
end

type read_error =
  | File_error of Mentat_workspace_io.File_error.t
  | Binary_file of Mentat_workspace.Path.t
  | Invalid_utf8 of Mentat_workspace.Path.t

let utf8_sequence_length byte =
  let byte = Char.code byte in
  if byte >= 0xC2 && byte <= 0xDF then Some 2
  else if byte >= 0xE0 && byte <= 0xEF then Some 3
  else if byte >= 0xF0 && byte <= 0xF4 then Some 4
  else None

(* A byte cap may cut a well-formed scalar after its leading byte. It must not
   turn a malformed suffix into valid text merely by dropping that suffix. An
   invalid decode consumes the whole available suffix only when every present
   continuation byte was valid and the next byte was missing. *)
let incomplete_utf8_suffix text start =
  let present = String.length text - start in
  match utf8_sequence_length text.[start] with
  | Some expected when present < expected ->
      let decoded = String.get_utf_8_uchar text start in
      (not (Uchar.utf_decode_is_valid decoded))
      && Uchar.utf_decode_length decoded = present
  | Some _ | None -> false

let incomplete_utf8_prefix text =
  let length = String.length text in
  let lower_bound = max 0 (length - 3) in
  let rec find start =
    if start < lower_bound then None
    else
      let prefix = String.sub text 0 start in
      if String.is_valid_utf_8 prefix && incomplete_utf8_suffix text start then
        Some prefix
      else find (start - 1)
  in
  find (length - 1)

let valid_returned_text ~path ~byte_capped text =
  if String.is_valid_utf_8 text then Ok text
  else if byte_capped then
    incomplete_utf8_prefix text |> Option.to_result ~none:(Invalid_utf8 path)
  else Error (Invalid_utf8 path)

let total_lines ~bytes_seen ~newlines ~last_char =
  if bytes_seen = 0 then 0
  else
    match last_char with
    | Some '\n' -> newlines
    | Some _ -> newlines + 1
    | None -> 0

let next_file_input input path ~start_line ~returned_lines ~range_has_more =
  match input.Input.limit with
  | Some limit when returned_lines > 0 && range_has_more ->
      Some
        (Output.Continuation.make ?max_bytes:input.Input.max_bytes
           ~path:(Mentat_workspace.Path.display path)
           ~offset:(start_line + returned_lines)
           ~limit ())
  | Some _ | None -> None

let read_file workspace_io path input ~cancelled =
  let start_line = Input.start_line input in
  let ranged =
    Option.is_some input.Input.offset || Option.is_some input.Input.limit
  in
  let max_bytes = Input.effective_max_bytes input in
  let contents = Buffer.create (min max_bytes 4096) in
  let bytes_seen = ref 0 in
  let newlines = ref 0 in
  let last_char = ref None in
  let current_line = ref 1 in
  let file_offset = ref 0 in
  let byte_capped = ref false in
  let eof = ref false in
  let range_has_more = ref false in
  let first_chunk = ref true in
  let selected line =
    line >= start_line
    &&
    match input.Input.limit with
    | None -> true
    | Some limit -> line - start_line < limit
  in
  let passed_range line =
    match input.Input.limit with
    | None -> false
    | Some limit -> line >= start_line && line - start_line >= limit
  in
  let rec scan () =
    if cancelled () then `Cancelled
    else if !byte_capped || !range_has_more || !eof then `Done
    else
      match
        Mentat_workspace_io.File.read workspace_io path ~offset:!file_offset
          ~max_bytes:read_chunk_bytes
      with
      | Error error -> `Error (File_error error)
      | Ok slice ->
          let chunk = slice.Mentat_workspace_io.File.contents in
          if String.is_empty chunk then begin
            eof := true;
            `Done
          end
          else if !first_chunk && Text_helpers.looks_binary chunk then
            `Error (Binary_file path)
          else begin
            first_chunk := false;
            file_offset := !file_offset + String.length chunk;
            let index = ref 0 in
            while
              !index < String.length chunk
              && (not !byte_capped) && not !range_has_more
            do
              if cancelled () then raise Exit;
              if passed_range !current_line && ranged then
                range_has_more := true
              else begin
                let byte = chunk.[!index] in
                incr bytes_seen;
                last_char := Some byte;
                if selected !current_line then
                  if Buffer.length contents < max_bytes then
                    Buffer.add_char contents byte
                  else byte_capped := true;
                if Char.equal byte '\n' then begin
                  incr newlines;
                  incr current_line
                end;
                incr index
              end
            done;
            eof :=
              slice.Mentat_workspace_io.File.eof
              && !index = String.length chunk
              && (not !byte_capped) && not !range_has_more;
            if
              (not !range_has_more) && passed_range !current_line && ranged
              && not !eof
            then scan ()
            else if !byte_capped || !range_has_more || !eof then `Done
            else scan ()
          end
  in
  match scan () with
  | exception Exit -> `Cancelled
  | `Cancelled -> `Cancelled
  | `Error error -> `Error error
  | `Done -> (
      match
        valid_returned_text ~path ~byte_capped:!byte_capped
          (Buffer.contents contents)
      with
      | Error error -> `Error error
      | Ok contents -> (
          let complete =
            start_line = 1 && (not !byte_capped) && (not !range_has_more)
            && !eof
          in
          let total =
            total_lines ~bytes_seen:!bytes_seen ~newlines:!newlines
              ~last_char:!last_char
          in
          let total_lines =
            if !eof || complete then Output.Exact total
            else Output.Lower_bound total
          in
          let returned_lines = Text_helpers.logical_line_count contents in
          let status =
            if complete then
              Output.Complete (Mentat_digest.Content_ref.of_contents contents)
            else
              let reason =
                match (ranged, !byte_capped) with
                | true, true -> Output.Ranged_and_byte_capped
                | true, false -> Output.Ranged
                | false, true -> Output.Byte_capped
                | false, false -> Output.Ranged
              in
              let next =
                next_file_input input path ~start_line ~returned_lines
                  ~range_has_more:!range_has_more
              in
              Output.Partial { reason; next }
          in
          match (input.Input.if_identity, status) with
          | Some requested, Output.Complete identity
            when Mentat_digest.Content_ref.equal requested identity ->
              `Output (Output.unchanged ~path ~identity)
          | Some _, _ | None, _ ->
              `Output
                (Output.file ~path ~contents ~start_line ~total_lines ~status)))

let is_vcs_metadata name =
  List.exists (String.equal name) Glob.vcs_metadata_dirs

let entry_kind = function
  | `Regular_file -> Output.Regular_file
  | `Directory -> Output.Directory
  | `Symbolic_link -> Output.Symlink
  | `Unknown | `Fifo | `Character_special | `Block_device | `Socket ->
      Output.Other

let entry_rank = function
  | Output.Directory -> 0
  | Output.Regular_file -> 1
  | Output.Symlink -> 2
  | Output.Other -> 3

let compare_entries (left : Output.entry) (right : Output.entry) =
  match
    Int.compare
      (entry_rank left.Output.entry_kind)
      (entry_rank right.Output.entry_kind)
  with
  | 0 -> String.compare left.Output.entry_name right.Output.entry_name
  | order -> order

let list_directory workspace_io path input ~cancelled =
  if Option.is_some input.Input.if_identity then
    `Invalid_input
      (Mentat_workspace.Path.display path
      ^ ": if_identity cannot be used with a directory")
  else
    match Mentat_workspace_io.File.read_dir_entries workspace_io path with
    | Error error -> `File_error error
    | Ok paths -> (
        let rec classify entries = function
          | [] -> `Entries (List.sort compare_entries entries)
          | (kind, child) :: children ->
              if cancelled () then `Cancelled
              else
                let child_name =
                  match Mentat_workspace.Path.basename child with
                  | Some name -> name
                  | None -> assert false
                in
                if is_vcs_metadata child_name then classify entries children
                else
                  classify
                    ({
                       Output.entry_path = child;
                       entry_name = child_name;
                       entry_kind = entry_kind kind;
                     }
                    :: entries)
                    children
        in
        match classify [] paths with
        | (`Cancelled | `File_error _) as result -> result
        | `Entries entries ->
            let offset = Input.start_line input in
            let limit = Input.effective_directory_limit input in
            let total_entries = List.length entries in
            let returned =
              entries |> List.drop (offset - 1) |> List.take limit
            in
            let first_unreturned = offset + List.length returned in
            let complete =
              offset > total_entries || first_unreturned > total_entries
            in
            let next =
              if complete then None
              else
                Some
                  (Output.Continuation.make
                     ~path:(Mentat_workspace.Path.display path)
                     ~offset:first_unreturned ~limit ())
            in
            `Output
              (Output.listing ~path ~entries:returned ~offset ~limit
                 ~total_entries ~complete ~next))

let max_suggestion_distance text =
  let length = String.length text in
  if length <= 4 then 1 else if length <= 8 then 2 else 3

let suggestions workspace_io path =
  match
    (Mentat_workspace.Path.parent path, Mentat_workspace.Path.basename path)
  with
  | None, _ | _, None -> []
  | Some parent, Some wanted -> (
      match Mentat_workspace_io.File.read_dir workspace_io parent with
      | Error _ -> []
      | Ok children ->
          children
          |> List.filter_map (fun child ->
              match Mentat_workspace.Path.basename child with
              | None -> None
              | Some name ->
                  let distance = String.edit_distance wanted name in
                  if
                    String.equal wanted name
                    || distance > max_suggestion_distance wanted
                  then None
                  else Some (distance, name, child))
          |> List.sort
               (fun
                 (left_distance, left_name, _)
                 (right_distance, right_name, _)
               ->
                 match Int.compare left_distance right_distance with
                 | 0 -> String.compare left_name right_name
                 | order -> order)
          |> List.map (fun (_, _, child) -> child)
          |> List.to_seq |> Seq.take 3 |> List.of_seq)

let failed_file_error workspace_io error =
  match error with
  | Mentat_workspace_io.File_error.Not_found path ->
      let candidates = suggestions workspace_io path in
      if List.is_empty candidates then Fs_error.failed error
      else
        Fs_error.failed error
          ~message:
            (Fs_error.message error ^ ". Did you mean: "
            ^ String.concat ", "
                (List.map Mentat_workspace.Path.display candidates)
            ^ "?")
  | error -> Fs_error.failed error

(* Re-read a file's raw bytes for the image branch, bounded by [max_bytes] (the
   image byte cap). A file exceeding the cap yields [`Too_large prefix], keeping
   the read prefix so the caller can still sniff its format and tailor the
   message. *)
let read_image_bytes workspace_io path ~cancelled ~max_bytes =
  let buffer = Buffer.create 65536 in
  let rec loop offset =
    if cancelled () then `Cancelled
    else
      match
        Mentat_workspace_io.File.read workspace_io path ~offset
          ~max_bytes:read_chunk_bytes
      with
      | Error error -> `Error error
      | Ok slice ->
          let chunk = slice.Mentat_workspace_io.File.contents in
          if String.is_empty chunk then `Bytes (Buffer.contents buffer)
          else if Buffer.length buffer + String.length chunk > max_bytes then
            `Too_large (Buffer.contents buffer)
          else begin
            Buffer.add_string buffer chunk;
            loop (offset + String.length chunk)
          end
  in
  loop 0

(* A binary file that sniffs as a supported image is returned to the model as
   media plus a text description — the model's own way to pull an image into
   context. [None] falls back to the plain binary rejection when the bytes are
   not an image. An image over the byte or dimension cap is a text rejection: a
   read has no downscale spawn boundary, so the message tells the user to resize
   it. *)
let image_result workspace_io path ~cancelled ~image_max_bytes
    ~image_max_dimension =
  match
    read_image_bytes workspace_io path ~cancelled ~max_bytes:image_max_bytes
  with
  | `Cancelled -> Some (Mentat_tool.Result.cancelled ())
  | `Error error -> Some (failed_file_error workspace_io error)
  | `Too_large prefix -> (
      match Mentat_llm.Image.sniff prefix with
      | None -> None
      | Some _ ->
          Some
            (Mentat_tool.Result.failed `Invalid_input
               (Printf.sprintf
                  "%s: image exceeds the %d-byte cap; resize it (read does not \
                   downscale)"
                  (Mentat_workspace.Path.display path)
                  image_max_bytes)))
  | `Bytes bytes -> (
      match Mentat_llm.Image.sniff bytes with
      | None -> None
      | Some format -> (
          let dimensions = Mentat_llm.Image.dimensions bytes in
          match dimensions with
          | Some { Mentat_llm.Image.width; height }
            when width > image_max_dimension || height > image_max_dimension ->
              Some
                (Mentat_tool.Result.failed `Invalid_input
                   (Printf.sprintf
                      "%s: image dimensions %dx%d exceed the %d-pixel cap"
                      (Mentat_workspace.Path.display path)
                      width height image_max_dimension))
          | _ ->
              let dimension_text =
                match dimensions with
                | Some { Mentat_llm.Image.width; height } ->
                    Printf.sprintf " %dx%d" width height
                | None -> ""
              in
              Some
                (Mentat_tool.Result.completed
                   ~output:(Output.image ~path ~format ~bytes ~dimension_text)
                   ())))

let run workspace_io input ~cancelled ~image_max_bytes ~image_max_dimension =
  if cancelled () then Mentat_tool.Result.cancelled ()
  else
    Permissions.with_resolved_run workspace_io input.Input.path (fun path ->
        match Mentat_workspace_io.File.stat workspace_io path with
        | Error error -> failed_file_error workspace_io error
        | Ok stat -> (
            match stat.Eio.File.Stat.kind with
            | `Regular_file -> (
                match read_file workspace_io path input ~cancelled with
                | `Cancelled -> Mentat_tool.Result.cancelled ()
                | `Error (Binary_file path) -> (
                    match
                      image_result workspace_io path ~cancelled ~image_max_bytes
                        ~image_max_dimension
                    with
                    | Some result -> result
                    | None ->
                        Mentat_tool.Result.failed `Invalid_input
                          (Mentat_workspace.Path.display path ^ ": binary file")
                    )
                | `Error (Invalid_utf8 path) ->
                    Mentat_tool.Result.failed `Invalid_input
                      (Mentat_workspace.Path.display path
                      ^ ": not valid UTF-8 text")
                | `Error (File_error error) ->
                    failed_file_error workspace_io error
                | `Output output -> Mentat_tool.Result.completed ~output ())
            | `Directory -> (
                match list_directory workspace_io path input ~cancelled with
                | `Cancelled -> Mentat_tool.Result.cancelled ()
                | `Invalid_input message ->
                    Mentat_tool.Result.failed `Invalid_input message
                | `File_error error -> failed_file_error workspace_io error
                | `Output output -> Mentat_tool.Result.completed ~output ())
            | `Symbolic_link | `Unknown | `Fifo | `Character_special
            | `Block_device | `Socket ->
                Mentat_tool.Result.failed `Invalid_input
                  (Mentat_workspace.Path.display path
                  ^ ": not a readable file or directory")))

let permissions workspace_io input =
  Permissions.with_resolved workspace_io input.Input.path (fun path ->
      [
        Mentat_permission.Request.of_accesses ~source:name
          [ Mentat_permission.Access.path ~op:`Read path ];
      ])

let make ?(conditional_read = false)
    ?(image_max_bytes = default_image_max_bytes)
    ?(image_max_dimension = default_image_max_dimension) workspace_io =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.read_file
    ~input:(Input.contract ~conditional_read)
    ~output:Output.encode ~permissions:(permissions workspace_io)
    ~run:(fun ~cancelled input ->
      run workspace_io input ~cancelled ~image_max_bytes ~image_max_dimension)
    ()
