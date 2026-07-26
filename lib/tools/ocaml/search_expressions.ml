(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let max_source_bytes = 8 * 1024 * 1024

module Syntax = Mentat_ocaml
module Grep = Mentat_ocaml_grep

let name = "ocaml_search_expressions"
let default_limit = 100
let max_limit = 1_000
let max_line_bytes = 2_000
let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_bool value = Jsont.Json.bool value

let json_to_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok text -> text
  | Error message -> invalid_arg ("could not encode JSON: " ^ message)

module Input = struct
  type t = {
    pattern : string;
    paths : string list option;
    offset : int option;
    limit : int option;
  }

  let validate_path path =
    if String.is_empty path then
      invalid_arg "paths must not contain empty paths";
    if String.contains path '\x00' then invalid_arg "paths must not contain NUL"

  let validate_paths = function
    | None -> ()
    | Some [] -> invalid_arg "paths must not be empty"
    | Some paths -> List.iter validate_path paths

  let validate_pagination offset limit =
    (match offset with
    | Some offset when offset < 1 -> invalid_arg "offset must be at least 1"
    | Some _ | None -> ());
    match limit with
    | Some limit when limit < 1 -> invalid_arg "limit must be positive"
    | Some limit when limit > max_limit ->
        invalid_arg ("limit must be at most " ^ string_of_int max_limit)
    | Some _ | None -> ()

  let make pattern paths offset limit =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    if String.is_empty pattern then invalid_arg "pattern must not be empty";
    if String.contains pattern '\x00' then
      invalid_arg "pattern must not contain NUL";
    validate_paths paths;
    validate_pagination offset limit;
    { pattern; paths; offset; limit }

  let max_input_integer =
    Float.min 9_007_199_254_740_991. (float_of_int Int.max_int)

  let exact_integer =
    let decode = function
      | Jsont.Number (value, _)
        when Float.is_integer value && Float.abs value <= max_input_integer ->
          int_of_float value
      | Jsont.Number _ | Jsont.Null _ | Jsont.Bool _ | Jsont.String _
      | Jsont.Array _ | Jsont.Object _ ->
          Codec.decode_error "expected an integer in JSON's safe integer range"
    in
    Jsont.map ~kind:"integer" ~dec:decode
      ~enc:(fun value -> json_int value)
      Jsont.json

  let object_codec =
    Jsont.Object.map ~kind:"ocaml_search_expressions input" make
    |> Jsont.Object.mem "pattern" Jsont.string ~enc:(fun input -> input.pattern)
    |> Jsont.Object.opt_mem "paths" (Jsont.list Jsont.string) ~enc:(fun input ->
        input.paths)
    |> Jsont.Object.opt_mem "offset" exact_integer ~enc:(fun input ->
        input.offset)
    |> Jsont.Object.opt_mem "limit" exact_integer ~enc:(fun input ->
        input.limit)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec =
    Codec.strict_object ~kind:"strict ocaml_search_expressions input"
      object_codec

  let schema_property kind description fields =
    Codec.obj
      (("type", json_string kind)
      :: ("description", json_string description)
      :: fields)

  let schema =
    Codec.obj
      [
        ("type", json_string "object");
        ( "properties",
          Codec.obj
            [
              ( "pattern",
                schema_property "string"
                  "One complete OCaml expression pattern. __ replaces any \
                   expression but does not relax OCaml grammar; a match still \
                   needs complete PATTERN -> EXPR clauses. __1/__2 are \
                   unification metavariables, f ?arg:PRESENT / f ?arg:MISSING \
                   constrain optional arguments, and match/record clauses \
                   match as sets."
                  [ ("minLength", json_int 1) ] );
              ( "paths",
                schema_property "array"
                  "Workspace-relative or workspace-contained absolute file or \
                   directory roots. Defaults to the workspace current \
                   directory."
                  [
                    ( "items",
                      Codec.obj
                        [
                          ("type", json_string "string");
                          ("minLength", json_int 1);
                        ] );
                    ("minItems", json_int 1);
                  ] );
              ( "offset",
                schema_property "integer"
                  "One-based first finding. Defaults to 1."
                  [
                    ("minimum", json_int 1);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
              ( "limit",
                schema_property "integer"
                  "Maximum findings returned. Defaults to 100."
                  [ ("minimum", json_int 1); ("maximum", json_int max_limit) ]
              );
            ] );
        ("required", Jsont.Json.list [ json_string "pattern" ]);
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
  let effective_paths input = Option.value input.paths ~default:[ "." ]
  let effective_offset input = Option.value input.offset ~default:1
  let effective_limit input = Option.value input.limit ~default:default_limit

  let to_json input =
    let fields = [ ("pattern", json_string input.pattern) ] in
    let fields =
      match input.paths with
      | None -> fields
      | Some paths ->
          fields @ [ ("paths", Jsont.Json.list (List.map json_string paths)) ]
    in
    let fields =
      match input.offset with
      | None -> fields
      | Some offset -> fields @ [ ("offset", json_int offset) ]
    in
    let fields =
      match input.limit with
      | None -> fields
      | Some limit -> fields @ [ ("limit", json_int limit) ]
    in
    Codec.obj fields

  let continuation input ~paths ~offset ~limit =
    {
      pattern = input.pattern;
      paths = Some paths;
      offset = Some offset;
      limit = Some limit;
    }
end

let pattern_error_message error =
  let diagnostic = Format.asprintf "%a" Grep.Pattern.pp_error error in
  match error with
  | Grep.Pattern.Syntax _ ->
      diagnostic
      ^ ". A pattern must be one complete OCaml expression; `__` replaces an \
         expression but does not relax OCaml grammar. Match clauses require \
         `PATTERN -> EXPR`, for example: `match __ with Some x -> __ | None -> \
         __`."
  | Grep.Pattern.Unsupported _ -> diagnostic

type root_kind = Regular_file | Directory
type root = { raw : string; path : Mentat_workspace.Path.t; kind : root_kind }

type search_error =
  | File_error of Mentat_workspace_io.File_error.t
  | Invalid_root of Mentat_workspace.Path.t * Eio.File.Stat.kind
  | Enumerate of string
  | Cancelled

let interrupted () = Mentat_tool.Result.cancelled ()

let error_kind = function
  | File_error error -> Fs_error.failure error
  | Invalid_root _ -> `Invalid_input
  | Enumerate _ -> `Failed
  | Cancelled -> `Failed

let error_message = function
  | File_error error -> Fs_error.message error
  | Invalid_root (path, `Symbolic_link) ->
      Mentat_workspace.Path.display path
      ^ ": symlink search roots are not supported"
  | Invalid_root (path, (`Regular_file | `Directory)) ->
      Mentat_workspace.Path.display path ^ ": invalid search root"
  | Invalid_root
      (path, (`Unknown | `Fifo | `Character_special | `Block_device | `Socket))
    ->
      Mentat_workspace.Path.display path
      ^ ": expected a regular file or directory"
  | Enumerate message -> "file enumeration failed: " ^ message
  | Cancelled -> "tool call cancelled"

let failed error =
  Mentat_tool.Result.failed (error_kind error) (error_message error)

let resolve_roots workspace_io input =
  let rec loop seen roots = function
    | [] -> Ok (List.rev roots)
    | raw :: raws -> (
        match Mentat_workspace_io.resolve_path workspace_io raw with
        | Error error ->
            Error
              (Mentat_tool.Result.failed `Invalid_input
                 (Mentat_workspace.Resolve_error.message error))
        | Ok path -> (
            if Mentat_workspace.Path.Set.mem path seen then loop seen roots raws
            else
              match Mentat_workspace_io.File.lstat workspace_io path with
              | Error error -> Error (failed (File_error error))
              | Ok stat -> (
                  match stat.Eio.File.Stat.kind with
                  | `Regular_file ->
                      loop
                        (Mentat_workspace.Path.Set.add path seen)
                        ({ raw; path; kind = Regular_file } :: roots)
                        raws
                  | `Directory ->
                      loop
                        (Mentat_workspace.Path.Set.add path seen)
                        ({ raw; path; kind = Directory } :: roots)
                        raws
                  | kind -> Error (failed (Invalid_root (path, kind))))))
  in
  loop Mentat_workspace.Path.Set.empty [] (Input.effective_paths input)

let enumerate_directory workspace_io ~cancelled root =
  match
    Fs.Glob.Enumeration.paths workspace_io ~cancelled ~root:root.path
      ~pattern:"**/*.ml"
  with
  | Ok paths -> Ok paths
  | Error `Cancelled -> Error Cancelled
  | Error (`File_error error) -> Error (File_error error)
  | Error (`Invalid_pattern message) -> Error (Enumerate message)

let enumerate_candidates workspace_io ~cancelled roots =
  let rec loop seen acc = function
    | [] -> Ok (List.rev acc)
    | root :: roots -> (
        let files =
          match root.kind with
          | Regular_file -> Ok [ root.path ]
          | Directory -> enumerate_directory workspace_io ~cancelled root
        in
        match files with
        | Error _ as error -> error
        | Ok files ->
            let seen, acc =
              List.fold_left
                (fun (seen, acc) file ->
                  if Mentat_workspace.Path.Set.mem file seen then (seen, acc)
                  else (Mentat_workspace.Path.Set.add file seen, file :: acc))
                (seen, acc) files
            in
            loop seen acc roots)
  in
  loop Mentat_workspace.Path.Set.empty [] roots

module Output = struct
  type partial_reason = Limit
  type status = Complete | Partial of partial_reason

  type skipped_reason =
    | Binary
    | Invalid_utf8
    | Too_large
    | Syntax_error of string
    | Read_error of string

  type skipped = { skipped_address : string; reason : skipped_reason }
  type line = { number : int; text : string; truncated : bool }

  type finding = {
    location : Syntax.Location.t;
    address : string;
    lines : line list;
  }

  type t = {
    pattern : string;
    offset : int;
    limit : int;
    findings : finding list;
    total_results : int;
    matching_files : int;
    status : status;
    next : Input.t option;
    skipped : skipped list;
    searched_files : int;
  }

  let has_more output =
    match output.status with Complete -> false | Partial Limit -> true

  let status_to_string = function
    | Complete -> "complete"
    | Partial Limit -> "partial"

  let skipped_reason_label = function
    | Binary -> "binary"
    | Invalid_utf8 -> "invalid_utf8"
    | Too_large -> "too_large"
    | Syntax_error _ -> "syntax_error"
    | Read_error _ -> "read_error"

  let skipped_reason_message = function
    | Binary | Invalid_utf8 | Too_large -> None
    | Syntax_error message | Read_error message -> Some message

  let add_header buffer output =
    Printf.bprintf buffer
      "ocaml_search_expressions pattern=%s results=%d/%d offset=%d limit=%d \
       status=%s searched_files=%d\n"
      (json_to_string (json_string output.pattern))
      (List.length output.findings)
      output.total_results output.offset output.limit
      (status_to_string output.status)
      output.searched_files

  let add_line buffer line =
    Printf.bprintf buffer "  %d: %s" line.number line.text;
    if line.truncated then Buffer.add_string buffer " [truncated]";
    Buffer.add_char buffer '\n'

  let add_finding buffer (finding : finding) =
    Printf.bprintf buffer "%s:%s" finding.address
      (Format.asprintf "%a" Syntax.Range.pp
         (Syntax.Location.range finding.location));
    Buffer.add_char buffer '\n';
    List.iter (add_line buffer) finding.lines

  let add_skipped buffer output =
    match output.skipped with
    | [] -> ()
    | skipped ->
        Buffer.add_string buffer "skipped:\n";
        List.iter
          (fun skipped ->
            Printf.bprintf buffer "  %s reason=%s" skipped.skipped_address
              (skipped_reason_label skipped.reason);
            (match skipped_reason_message skipped.reason with
            | None -> ()
            | Some message ->
                Buffer.add_char buffer ' ';
                Buffer.add_string buffer message);
            Buffer.add_char buffer '\n')
          skipped

  let add_next buffer output =
    match output.next with
    | None -> ()
    | Some next ->
        Buffer.add_string buffer "next: ocaml_search_expressions ";
        Buffer.add_string buffer (json_to_string (Input.to_json next));
        Buffer.add_char buffer '\n'

  let text output =
    let buffer = Buffer.create 512 in
    add_header buffer output;
    (match output.findings with
    | [] -> Buffer.add_string buffer "No matches\n"
    | findings -> List.iter (add_finding buffer) findings);
    add_skipped buffer output;
    add_next buffer output;
    Buffer.contents buffer

  let encode output =
    let truncated =
      has_more output
      || List.exists
           (fun finding ->
             List.exists (fun line -> line.truncated) finding.lines)
           output.findings
      || not (List.is_empty output.skipped)
    in
    let semantic =
      Mentat_tools_output.Search.matches ~total:output.total_results
        ~files:output.matching_files
    in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Search.jsont
      ~text:(text output) ~truncated semantic
end

let read_source workspace_io path =
  match
    Mentat_workspace_io.File.load workspace_io path ~max_bytes:max_source_bytes
  with
  | Error (Mentat_workspace_io.File_error.Too_large _) -> Error Output.Too_large
  | Error error -> Error (Output.Read_error (Fs_error.message error))
  | Ok source ->
      if Text_helpers.looks_binary source then Error Output.Binary
      else if not (String.is_valid_utf_8 source) then Error Output.Invalid_utf8
      else Ok source

let bounded_line raw =
  if String.length raw <= max_line_bytes then (raw, false)
  else (Text_helpers.valid_utf8_prefix raw max_line_bytes, true)

let finding_of_location ~address source_lines location =
  let range = Syntax.Location.range location in
  let start_line = Syntax.Position.line (Syntax.Range.start range) in
  let end_line = Syntax.Position.line (Syntax.Range.end_ range) in
  let count = Array.length source_lines in
  let first = max 1 (min count start_line) in
  let last = max first (min count end_line) in
  let line number =
    let raw = Text_helpers.strip_trailing_cr source_lines.(number - 1) in
    let text, truncated = bounded_line raw in
    { Output.number; text; truncated }
  in
  {
    Output.location;
    address;
    lines = List.init (last - first + 1) (fun index -> line (first + index));
  }

let search_file workspace_io pattern path =
  let address = Address.display workspace_io path in
  match read_source workspace_io path with
  | Error reason -> Error { Output.skipped_address = address; reason }
  | Ok source -> (
      let filename = Mentat_workspace.Path.display path in
      match Grep.parse_implementation ~filename source with
      | Error error ->
          Error
            {
              Output.skipped_address = address;
              reason = Output.Syntax_error (Grep.Parse_error.to_string error);
            }
      | Ok structure ->
          let locations = Grep.search pattern ~path structure in
          let source_lines = Array.of_list (String.split_on_char '\n' source) in
          Ok (List.map (finding_of_location ~address source_lines) locations))

let compare_findings left right =
  Syntax.Location.compare left.Output.location right.Output.location

let take count values =
  let rec loop acc count = function
    | _ when count = 0 -> List.rev acc
    | [] -> List.rev acc
    | value :: values -> loop (value :: acc) (count - 1) values
  in
  loop [] count values

let drop count values =
  let rec loop count values =
    if count = 0 then values
    else match values with [] -> [] | _ :: values -> loop (count - 1) values
  in
  loop count values

let assemble input roots ~findings ~skipped ~searched_files =
  let findings = List.sort compare_findings findings in
  let total_results = List.length findings in
  let matching_files =
    findings
    |> List.map (fun finding -> finding.Output.address)
    |> List.sort_uniq String.compare
    |> List.length
  in
  let offset = Input.effective_offset input in
  let limit = Input.effective_limit input in
  let findings = findings |> drop (offset - 1) |> take limit in
  let returned = List.length findings in
  let has_more =
    offset <= total_results && offset + returned <= total_results
  in
  let status =
    if has_more then Output.Partial Output.Limit else Output.Complete
  in
  let next =
    if has_more then
      Some
        (Input.continuation input
           ~paths:(List.map (fun root -> root.raw) roots)
           ~offset:(offset + returned) ~limit)
    else None
  in
  {
    Output.pattern = input.Input.pattern;
    offset;
    limit;
    findings;
    total_results;
    matching_files;
    status;
    next;
    skipped;
    searched_files;
  }

let run workspace_io input ~cancelled =
  if cancelled () then interrupted ()
  else
    match Grep.Pattern.parse input.Input.pattern with
    | Error error ->
        Mentat_tool.Result.failed `Invalid_input (pattern_error_message error)
    | Ok pattern -> (
        match resolve_roots workspace_io input with
        | Error result -> result
        | Ok roots -> (
            match enumerate_candidates workspace_io ~cancelled roots with
            | Error Cancelled -> interrupted ()
            | Error ((File_error _ | Invalid_root _ | Enumerate _) as error) ->
                failed error
            | Ok candidates ->
                let rec loop findings skipped searched_files = function
                  | [] ->
                      Mentat_tool.Result.completed
                        ~output:
                          (assemble input roots ~findings
                             ~skipped:(List.rev skipped) ~searched_files)
                        ()
                  | path :: paths -> (
                      if cancelled () then interrupted ()
                      else
                        match search_file workspace_io pattern path with
                        | Error skipped_file ->
                            loop findings (skipped_file :: skipped)
                              searched_files paths
                        | Ok file_findings ->
                            loop
                              (List.rev_append file_findings findings)
                              skipped (searched_files + 1) paths)
                in
                loop [] [] 0 candidates))

let permissions workspace_io input =
  List.concat_map
    (fun raw ->
      Permissions.with_resolved workspace_io raw (fun path ->
          [
            Mentat_permission.Request.of_accesses ~source:name
              [ Mentat_permission.Access.path ~op:`Read path ];
          ]))
    (Input.effective_paths input)

let make workspace_io =
  Mentat_tool.make ~name
    ~description:Mentat_prompts.Tools.ocaml_search_expressions
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io)
    ~run:(fun ~cancelled input -> run workspace_io input ~cancelled)
    ()
