(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let name = "search_text"
let default_limit = 100
let max_limit = 1_000
let max_context_lines = 5
let max_line_bytes = 2_000
let max_rg_output_bytes = 16 * 1024 * 1024
let max_rg_stderr_bytes = 64 * 1024
let max_rg_timeout_seconds = 60.
let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_bool value = Jsont.Json.bool value

let json_to_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok text -> text
  | Error message -> invalid_arg ("could not encode JSON: " ^ message)

module Input = struct
  type case = Sensitive | Insensitive
  type mode = Files | Count | Matches

  type t = {
    pattern : string;
    paths : string list option;
    glob : string option;
    mode : mode;
    case : case;
    context_lines : int option;
    offset : int option;
    limit : int option;
  }

  let mode_to_string = function
    | Files -> "files"
    | Count -> "count"
    | Matches -> "matches"

  let mode_of_string = function
    | "files" -> Files
    | "count" -> Count
    | "matches" -> Matches
    | value -> invalid_arg ("unknown mode: " ^ value)

  let validate_path path =
    if String.is_empty path then
      invalid_arg "paths must not contain empty paths";
    if String.contains path '\000' then invalid_arg "paths must not contain NUL"

  let validate_paths = function
    | None -> ()
    | Some [] -> invalid_arg "paths must not be empty"
    | Some paths -> List.iter validate_path paths

  let validate_glob = function
    | None -> ()
    | Some glob ->
        if String.is_empty glob then invalid_arg "glob must not be empty";
        if String.contains glob '\000' then
          invalid_arg "glob must not contain NUL"

  let validate_context mode = function
    | None -> ()
    | Some context_lines ->
        if mode <> Matches then
          invalid_arg "context_lines is valid only in matches mode";
        if context_lines < 0 then
          invalid_arg "context_lines must be non-negative";
        if context_lines > max_context_lines then
          invalid_arg
            ("context_lines must be at most " ^ string_of_int max_context_lines)

  let validate_page offset limit =
    (match offset with
    | Some offset when offset < 1 -> invalid_arg "offset must be at least 1"
    | Some _ | None -> ());
    match limit with
    | Some limit when limit < 1 -> invalid_arg "limit must be positive"
    | Some limit when limit > max_limit ->
        invalid_arg ("limit must be at most " ^ string_of_int max_limit)
    | Some _ | None -> ()

  let make pattern paths glob mode case_insensitive context_lines offset limit =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    if String.is_empty pattern then invalid_arg "pattern must not be empty";
    if String.contains pattern '\000' then
      invalid_arg "pattern must not contain NUL";
    let mode = Option.fold ~none:Files ~some:mode_of_string mode in
    let case =
      match case_insensitive with
      | Some true -> Insensitive
      | Some false | None -> Sensitive
    in
    validate_paths paths;
    validate_glob glob;
    validate_context mode context_lines;
    validate_page offset limit;
    { pattern; paths; glob; mode; case; context_lines; offset; limit }

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
    Jsont.Object.map ~kind:"search_text input" make
    |> Jsont.Object.mem "pattern" Jsont.string ~enc:(fun input -> input.pattern)
    |> Jsont.Object.opt_mem "paths" (Jsont.list Jsont.string) ~enc:(fun input ->
        input.paths)
    |> Jsont.Object.opt_mem "glob" Jsont.string ~enc:(fun input -> input.glob)
    |> Jsont.Object.opt_mem "mode" Jsont.string ~enc:(fun input ->
        Some (mode_to_string input.mode))
    |> Jsont.Object.opt_mem "case_insensitive" Jsont.bool ~enc:(fun input ->
        Some (input.case = Insensitive))
    |> Jsont.Object.opt_mem "context_lines" exact_integer ~enc:(fun input ->
        input.context_lines)
    |> Jsont.Object.opt_mem "offset" exact_integer ~enc:(fun input ->
        input.offset)
    |> Jsont.Object.opt_mem "limit" exact_integer ~enc:(fun input ->
        input.limit)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict search_text input" object_codec

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
                  "Ripgrep/Rust regular expression to search for in UTF-8 text \
                   files."
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
              ( "glob",
                schema_property "string"
                  "Optional file glob, for example *.ml or **/*.ts."
                  [ ("minLength", json_int 1) ] );
              ( "mode",
                schema_property "string" "Result mode. Defaults to files."
                  [
                    ( "enum",
                      Jsont.Json.list
                        (List.map json_string [ "files"; "count"; "matches" ])
                    );
                  ] );
              ( "case_insensitive",
                schema_property "boolean"
                  "Use case-insensitive regular-expression matching." [] );
              ( "context_lines",
                schema_property "integer"
                  "Symmetric context lines around matches; valid only in \
                   matches mode."
                  [
                    ("minimum", json_int 0);
                    ("maximum", json_int max_context_lines);
                  ] );
              ( "offset",
                schema_property "integer"
                  "One-based first result entry. Defaults to 1."
                  [
                    ("minimum", json_int 1);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
              ( "limit",
                schema_property "integer"
                  "Maximum result entries. Defaults to 100."
                  [ ("minimum", json_int 1); ("maximum", json_int max_limit) ]
              );
            ] );
        ("required", Jsont.Json.list [ json_string "pattern" ]);
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
  let effective_paths input = Option.value input.paths ~default:[ "." ]

  let effective_context input =
    match input.mode with
    | Matches -> Option.value input.context_lines ~default:0
    | Files | Count -> 0

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
      match input.glob with
      | None -> fields
      | Some glob -> fields @ [ ("glob", json_string glob) ]
    in
    let fields =
      fields @ [ ("mode", json_string (mode_to_string input.mode)) ]
    in
    let fields =
      fields @ [ ("case_insensitive", json_bool (input.case = Insensitive)) ]
    in
    let fields =
      match input.context_lines with
      | None -> fields
      | Some context -> fields @ [ ("context_lines", json_int context) ]
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

  let continuation input ~offset ~limit =
    {
      input with
      paths = Some (effective_paths input);
      context_lines =
        (match input.mode with
        | Matches -> Some (effective_context input)
        | Files | Count -> None);
      offset = Some offset;
      limit = Some limit;
    }
end

module Output = struct
  type total = Exact of int
  type line_kind = Match | Context
  type skipped_reason = Binary | Invalid_utf8

  type skipped = {
    skipped_path : Mentat_workspace.Path.t;
    reason : skipped_reason;
  }

  type line = {
    number : int;
    text : string;
    kind : line_kind;
    truncated : bool;
  }

  type span = { span_path : Mentat_workspace.Path.t; lines : line list }
  type count = { count_path : Mentat_workspace.Path.t; matching_lines : int }

  type result =
    | Files of Mentat_workspace.Path.t list
    | Count of { files : count list; total_matching_lines : total }
    | Matches of span list

  type page = {
    offset : int;
    limit : int;
    returned : int;
    total : total;
    next : Input.t option;
  }

  type t = {
    pattern : string;
    mode : Input.mode;
    result : result;
    matching_files : int;
    page : page;
    skipped : skipped list;
  }

  let total_value (Exact value) = value
  let total_text total = string_of_int (total_value total)
  let has_more output = Option.is_some output.page.next

  let skipped_reason_to_string = function
    | Binary -> "binary"
    | Invalid_utf8 -> "invalid_utf8"

  let status_to_string output =
    if has_more output then "partial" else "complete"

  let lines_truncated = function
    | Files _ | Count _ -> false
    | Matches spans ->
        List.exists
          (fun span -> List.exists (fun line -> line.truncated) span.lines)
          spans

  let truncated output =
    has_more output
    || lines_truncated output.result
    || not (List.is_empty output.skipped)

  let add_header buffer output =
    Buffer.add_string buffer "pattern=";
    Buffer.add_string buffer (json_to_string (json_string output.pattern));
    Buffer.add_string buffer " mode=";
    Buffer.add_string buffer (Input.mode_to_string output.mode);
    Buffer.add_string buffer " results=";
    Buffer.add_string buffer (string_of_int output.page.returned);
    Buffer.add_char buffer '/';
    Buffer.add_string buffer (total_text output.page.total);
    Buffer.add_string buffer " offset=";
    Buffer.add_string buffer (string_of_int output.page.offset);
    Buffer.add_string buffer " limit=";
    Buffer.add_string buffer (string_of_int output.page.limit);
    Buffer.add_string buffer " status=";
    Buffer.add_string buffer (status_to_string output);
    Buffer.add_char buffer '\n'

  let add_result buffer = function
    | Files [] | Count { files = []; _ } | Matches [] ->
        Buffer.add_string buffer "No matches\n"
    | Files paths ->
        List.iter
          (fun path ->
            Buffer.add_string buffer (Mentat_workspace.Path.display path);
            Buffer.add_char buffer '\n')
          paths
    | Count { files; _ } ->
        List.iter
          (fun count ->
            Buffer.add_string buffer
              (Mentat_workspace.Path.display count.count_path);
            Buffer.add_string buffer " matching_lines=";
            Buffer.add_string buffer (string_of_int count.matching_lines);
            Buffer.add_char buffer '\n')
          files
    | Matches spans ->
        List.iter
          (fun span ->
            Buffer.add_string buffer
              (Mentat_workspace.Path.display span.span_path);
            Buffer.add_char buffer '\n';
            List.iter
              (fun line ->
                Buffer.add_string buffer "  ";
                Buffer.add_string buffer (string_of_int line.number);
                Buffer.add_char buffer
                  (match line.kind with Match -> ':' | Context -> '-');
                Buffer.add_char buffer ' ';
                Buffer.add_string buffer line.text;
                if line.truncated then Buffer.add_string buffer " [truncated]";
                Buffer.add_char buffer '\n')
              span.lines)
          spans

  let add_skipped buffer skipped =
    if not (List.is_empty skipped) then begin
      Buffer.add_string buffer "skipped:\n";
      List.iter
        (fun skipped ->
          Buffer.add_string buffer "  ";
          Buffer.add_string buffer
            (Mentat_workspace.Path.display skipped.skipped_path);
          Buffer.add_string buffer " reason=";
          Buffer.add_string buffer (skipped_reason_to_string skipped.reason);
          Buffer.add_char buffer '\n')
        skipped
    end

  let add_next buffer = function
    | None -> ()
    | Some input ->
        Buffer.add_string buffer "next: ";
        Buffer.add_string buffer name;
        Buffer.add_char buffer ' ';
        Buffer.add_string buffer (json_to_string (Input.to_json input));
        Buffer.add_char buffer '\n'

  let text output =
    let buffer = Buffer.create 512 in
    add_header buffer output;
    add_result buffer output.result;
    add_skipped buffer output.skipped;
    add_next buffer output.page.next;
    Buffer.contents buffer

  let encode output =
    let total = total_value output.page.total in
    let semantic =
      match output.result with
      | Files _ -> Mentat_tools_output.Search.files ~total
      | Count { total_matching_lines = Exact total; _ } ->
          Mentat_tools_output.Search.matching_lines ~total
      | Matches _ ->
          Mentat_tools_output.Search.matches ~total ~files:output.matching_files
    in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Search.jsont
      ~text:(text output) ~truncated:(truncated output) semantic
end

type search_error =
  | File_error of Mentat_workspace_io.File_error.t
  | Invalid_root of Mentat_workspace.Path.t * [ `Symlink | `Other ]
  | Invalid_search of string
  | Command_refused of Mentat_workspace_io.Command.Error.t
  | Timed_out
  | Output_limit of Mentat_workspace_io.Command.stream
  | Incomplete_output
  | Command_failed of string
  | Cancelled

type root = { argument : string }

let protected_vcs_globs =
  List.concat_map
    (fun directory -> [ "!" ^ directory ^ "/**"; "!**/" ^ directory ^ "/**" ])
    Glob.vcs_metadata_dirs

let failure = function
  | File_error error -> Fs_error.failure error
  | Invalid_root _ | Invalid_search _ -> `Invalid_input
  | Command_refused (Mentat_workspace_io.Command.Error.Sandbox _) ->
      `Unavailable
  | Command_refused (Mentat_workspace_io.Command.Error.Unknown_cwd_root _) ->
      `Invalid_input
  | Command_refused
      ( Mentat_workspace_io.Command.Error.Spawn _
      | Mentat_workspace_io.Command.Error.Io _ )
  | Output_limit _ | Incomplete_output | Command_failed _ ->
      `Failed
  | Timed_out -> `Timed_out
  | Cancelled -> `Failed

let error_message = function
  | File_error error -> Fs_error.message error
  | Invalid_root (path, `Symlink) ->
      Mentat_workspace.Path.display path
      ^ ": symlink search roots are not supported"
  | Invalid_root (path, _) ->
      Mentat_workspace.Path.display path
      ^ ": expected a regular file or directory"
  | Invalid_search message -> message
  | Command_refused
      (Mentat_workspace_io.Command.Error.Spawn
         (Eio.Process.Executable_not_found _)) ->
      "ripgrep executable not found; search_text requires rg in PATH"
  | Command_refused error -> Mentat_workspace_io.Command.Error.message error
  | Timed_out -> "ripgrep timed out after 60000ms"
  | Output_limit Mentat_workspace_io.Command.Stdout ->
      "ripgrep stdout exceeded internal output limit"
  | Output_limit Mentat_workspace_io.Command.Stderr ->
      "ripgrep stderr exceeded internal output limit"
  | Incomplete_output -> "ripgrep output was incomplete"
  | Command_failed message -> message
  | Cancelled -> "tool call cancelled"

let failed error =
  Mentat_tool.Result.failed (failure error) (error_message error)

let resolve_roots workspace_io ~cancelled input =
  let rec loop seen roots = function
    | [] -> Ok (List.rev roots)
    | _ when cancelled () -> Error Cancelled
    | argument :: arguments -> (
        match Mentat_workspace_io.resolve_path workspace_io argument with
        | Error error ->
            Error
              (Invalid_search (Mentat_workspace.Resolve_error.message error))
        | Ok path -> (
            if Mentat_workspace.Path.Set.mem path seen then
              loop seen roots arguments
            else
              match Mentat_workspace_io.File.lstat workspace_io path with
              | Error error -> Error (File_error error)
              | Ok stat -> (
                  match stat.Eio.File.Stat.kind with
                  | `Regular_file | `Directory ->
                      loop
                        (Mentat_workspace.Path.Set.add path seen)
                        ({ argument } :: roots) arguments
                  | `Symbolic_link -> Error (Invalid_root (path, `Symlink))
                  | `Unknown | `Fifo | `Character_special | `Block_device
                  | `Socket ->
                      Error (Invalid_root (path, `Other)))))
  in
  loop Mentat_workspace.Path.Set.empty [] (Input.effective_paths input)

module Rg_event = struct
  type kind = Match | Context

  type t = {
    kind : kind;
    path : Mentat_workspace.Path.t;
    line_number : int;
    text : string option;
  }
end

type rg_json_line = Event of Rg_event.t | Skipped of Output.skipped | Ignore

let json_field name = function
  | Jsont.Object (fields, _) -> Option.map snd (Jsont.Json.find_mem name fields)
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      None

type unique_json_field = Missing | One of Jsont.json | Repeated

let json_fields name = function
  | Jsont.Object (fields, _) ->
      List.filter_map
        (fun ((field, _), value) ->
          if String.equal field name then Some value else None)
        fields
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      []

let unique_json_field name json =
  match json_fields name json with
  | [] -> Missing
  | [ value ] -> One value
  | _ :: _ :: _ -> Repeated

type rg_text_payload = Utf8 of string | Bytes | Malformed

let rg_text_payload = function
  | Jsont.Object (fields, _) as json -> (
      let only_payload_members =
        List.for_all
          (fun ((name, _), _) ->
            String.equal name "text" || String.equal name "bytes")
          fields
      in
      if not only_payload_members then Malformed
      else
        match
          (unique_json_field "text" json, unique_json_field "bytes" json)
        with
        | One (Jsont.String (text, _)), Missing -> Utf8 text
        | Missing, One (Jsont.String _) -> Bytes
        | (Missing | One _ | Repeated), (Missing | One _ | Repeated) ->
            Malformed)
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      Malformed

let malformed_rg_event kind detail =
  Error
    (Command_failed
       (Printf.sprintf "rg JSON: malformed %s event (%s)" kind detail))

let rg_path workspace_io ~event_type path_json =
  match rg_text_payload path_json with
  | Bytes -> Ok None
  | Malformed -> malformed_rg_event event_type "invalid path payload"
  | Utf8 text -> (
      match Mentat_workspace_io.resolve_path workspace_io text with
      | Ok path -> Ok (Some path)
      | Error error ->
          Error (Invalid_search (Mentat_workspace.Resolve_error.message error)))

let event_type = function
  | Jsont.String ((("match" | "context") as event_type), _) -> Some event_type
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Object _ | Jsont.Array _ ->
      None

let json_is_not_null = function
  | Jsont.Null _ -> false
  | Jsont.Bool _ | Jsont.Number _ | Jsont.String _ | Jsont.Object _
  | Jsont.Array _ ->
      true

let rg_event_of_json workspace_io json =
  let parse_line_event event_type =
    match unique_json_field "data" json with
    | Missing -> malformed_rg_event event_type "missing data"
    | Repeated -> malformed_rg_event event_type "duplicate data member"
    | One (Jsont.Object _ as data) -> (
        let kind =
          if String.equal event_type "match" then Rg_event.Match
          else Rg_event.Context
        in
        match
          (unique_json_field "path" data, unique_json_field "lines" data)
        with
        | Repeated, _ -> malformed_rg_event event_type "duplicate path member"
        | _, Repeated -> malformed_rg_event event_type "duplicate lines member"
        | Missing, _ | _, Missing ->
            malformed_rg_event event_type "missing path or lines"
        | One path_json, One lines_json -> (
            let text =
              match rg_text_payload lines_json with
              | Utf8 text -> Ok (Some text)
              | Bytes -> Ok None
              | Malformed ->
                  malformed_rg_event event_type "invalid lines payload"
            in
            let line_number =
              match unique_json_field "line_number" data with
              | Missing -> malformed_rg_event event_type "missing line_number"
              | Repeated ->
                  malformed_rg_event event_type "duplicate line_number member"
              | One (Jsont.Number (value, _))
                when Float.is_integer value && value >= 1.
                     && value <= float_of_int Int.max_int ->
                  Ok (int_of_float value)
              | One _ -> malformed_rg_event event_type "invalid line_number"
            in
            match (text, line_number) with
            | (Error _ as error), _ | _, (Error _ as error) -> error
            | Ok text, Ok line_number -> (
                match rg_path workspace_io ~event_type path_json with
                | Error _ as error -> error
                | Ok None -> Ok Ignore
                | Ok (Some path) ->
                    Ok (Event { Rg_event.kind; path; line_number; text }))))
    | One _ -> malformed_rg_event event_type "data must be an object"
  in
  match json_fields "type" json with
  | [ Jsont.String ((("match" | "context") as event_type), _) ] ->
      parse_line_event event_type
  | [ Jsont.String ("end", _) ] -> (
      match json_field "data" json with
      | None -> Ok Ignore
      | Some data -> (
          match (json_field "path" data, json_field "binary_offset" data) with
          | Some path_json, Some binary_offset
            when json_is_not_null binary_offset -> (
              match rg_path workspace_io ~event_type:"end" path_json with
              | Error _ as error -> error
              | Ok None -> Ok Ignore
              | Ok (Some path) ->
                  Ok
                    (Skipped
                       { Output.skipped_path = path; reason = Output.Binary }))
          | Some _, Some _ | Some _, None | None, (None | Some _) -> Ok Ignore))
  | type_values -> (
      match List.find_map event_type type_values with
      | Some event_type ->
          malformed_rg_event event_type "duplicate or invalid type member"
      | None -> Ok Ignore)

let parse_rg_json_line workspace_io line =
  match Jsont_bytesrw.decode_string Jsont.json line with
  (* The decode message is model-facing tool text, so strip jsont's SGR escapes
     (its global styler emits them under an interactive TERM) at this decode
     boundary rather than mutating the global styler. *)
  | Error message ->
      Error (Command_failed ("rg JSON: " ^ Text_helpers.strip_ansi message))
  | Ok json -> rg_event_of_json workspace_io json

let parse_rg_events workspace_io ~cancelled stdout =
  let add_skipped skipped skipped_paths =
    Mentat_workspace.Path.Map.add skipped.Output.skipped_path skipped
      skipped_paths
  in
  let rec loop skipped_paths events = function
    | _ when cancelled () -> Error Cancelled
    | [] ->
        let skipped =
          Mentat_workspace.Path.Map.bindings skipped_paths
          |> List.map (fun (_, skipped) -> skipped)
        in
        Ok (skipped, List.rev events)
    | "" :: lines -> loop skipped_paths events lines
    | line :: lines -> (
        match parse_rg_json_line workspace_io line with
        | Error _ as error -> error
        | Ok Ignore -> loop skipped_paths events lines
        | Ok (Skipped skipped) ->
            loop (add_skipped skipped skipped_paths) events lines
        | Ok (Event event) -> (
            match event.Rg_event.text with
            | None ->
                let skipped =
                  {
                    Output.skipped_path = event.Rg_event.path;
                    reason = Output.Invalid_utf8;
                  }
                in
                loop (add_skipped skipped skipped_paths) events lines
            | Some text when String.contains text '\000' ->
                let skipped =
                  {
                    Output.skipped_path = event.Rg_event.path;
                    reason = Output.Invalid_utf8;
                  }
                in
                loop (add_skipped skipped skipped_paths) events lines
            | Some _ -> loop skipped_paths (event :: events) lines))
  in
  loop Mentat_workspace.Path.Map.empty [] (String.split_on_char '\n' stdout)

let rg_args input roots =
  let args =
    [
      "rg";
      "--json";
      "--hidden";
      "--no-config";
      "--no-require-git";
      "--no-messages";
      "--color";
      "never";
      "--line-number";
      "--with-filename";
    ]
  in
  let args =
    match input.Input.case with
    | Input.Sensitive -> args
    | Input.Insensitive -> args @ [ "--ignore-case" ]
  in
  let context = Input.effective_context input in
  let args =
    if context = 0 then args else args @ [ "--context"; string_of_int context ]
  in
  let args =
    match input.Input.glob with
    | None -> args
    | Some glob -> args @ [ "--glob"; glob ]
  in
  let args =
    List.fold_left
      (fun args glob -> args @ [ "--glob"; glob ])
      args protected_vcs_globs
  in
  args
  @ [ "--"; input.Input.pattern ]
  @ List.map (fun root -> root.argument) roots

let diagnostic_stderr stderr =
  let raw_truncated = String.length stderr > max_rg_stderr_bytes in
  let raw =
    if raw_truncated then String.sub stderr 0 max_rg_stderr_bytes else stderr
  in
  let repaired = String.trim (Text_helpers.utf8_lossy raw) in
  let marker = "\n... ripgrep stderr truncated ..." in
  if (not raw_truncated) && String.length repaired <= max_rg_stderr_bytes then
    repaired
  else
    let prefix_budget = max_rg_stderr_bytes - String.length marker in
    let prefix =
      Text_helpers.valid_utf8_prefix repaired prefix_budget |> String.trim
    in
    if String.is_empty prefix then String.trim marker else prefix ^ marker

let command_failure code stderr =
  let message = diagnostic_stderr stderr in
  if code = 2 then
    Invalid_search
      (if String.is_empty message then "ripgrep rejected the search request"
       else message)
  else
    Command_failed
      (if String.is_empty message then
         "ripgrep exited with status " ^ string_of_int code
       else "ripgrep exited with status " ^ string_of_int code ^ ": " ^ message)

let run_rg workspace_io ~clock ~cancelled input roots =
  let timeout = Eio.Time.Timeout.seconds clock max_rg_timeout_seconds in
  match
    Mentat_workspace_io.Command.run workspace_io
      ~capture:(Mentat_workspace_io.Command.Limit max_rg_output_bytes) ~timeout
      ~cancelled (rg_args input roots)
  with
  | Error error -> Error (Command_refused error)
  | Ok outcome -> (
      let stdout =
        Mentat_workspace_io.Command.Captured.render
          outcome.Mentat_workspace_io.Command.stdout
      in
      let stderr =
        Mentat_workspace_io.Command.Captured.render
          outcome.Mentat_workspace_io.Command.stderr
      in
      match outcome.Mentat_workspace_io.Command.termination with
      | Mentat_workspace_io.Command.Stopped -> Error Cancelled
      | Mentat_workspace_io.Command.Timed_out -> Error Timed_out
      | Mentat_workspace_io.Command.Output_limit { stream; _ } ->
          Error (Output_limit stream)
      | Mentat_workspace_io.Command.Supervision_failed error ->
          Error
            (Command_failed
               (Format.asprintf "ripgrep supervision failed: %a" Eio.Exn.pp_err
                  error))
      | Mentat_workspace_io.Command.Exited (`Signaled signal) ->
          Error
            (Command_failed
               ("ripgrep terminated by signal " ^ string_of_int signal))
      | Mentat_workspace_io.Command.Exited (`Exited (0 | 1)) ->
          if
            Mentat_workspace_io.Command.Captured.is_complete
              outcome.Mentat_workspace_io.Command.stdout
            && Mentat_workspace_io.Command.Captured.is_complete
                 outcome.Mentat_workspace_io.Command.stderr
          then parse_rg_events workspace_io ~cancelled stdout
          else Error Incomplete_output
      | Mentat_workspace_io.Command.Exited (`Exited code) ->
          Error (command_failure code stderr))

let skipped_path_set skipped =
  List.fold_left
    (fun paths skipped ->
      Mentat_workspace.Path.Set.add skipped.Output.skipped_path paths)
    Mentat_workspace.Path.Set.empty skipped

let filtered_events skipped events =
  let skipped_paths = skipped_path_set skipped in
  List.filter
    (fun event ->
      not (Mentat_workspace.Path.Set.mem event.Rg_event.path skipped_paths))
    events

let event_order left right =
  let path =
    Mentat_workspace.Path.compare left.Rg_event.path right.Rg_event.path
  in
  if path <> 0 then path
  else
    let line =
      Int.compare left.Rg_event.line_number right.Rg_event.line_number
    in
    if line <> 0 then line
    else
      match (left.Rg_event.kind, right.Rg_event.kind) with
      | Rg_event.Match, Rg_event.Context -> -1
      | Rg_event.Context, Rg_event.Match -> 1
      | Rg_event.Match, Rg_event.Match | Rg_event.Context, Rg_event.Context -> 0

let sorted_unique_paths paths =
  paths
  |> List.fold_left
       (fun set path -> Mentat_workspace.Path.Set.add path set)
       Mentat_workspace.Path.Set.empty
  |> Mentat_workspace.Path.Set.elements

let search_files skipped events =
  events |> filtered_events skipped
  |> List.filter_map (fun event ->
      match event.Rg_event.kind with
      | Rg_event.Match -> Some event.Rg_event.path
      | Rg_event.Context -> None)
  |> sorted_unique_paths

module Match_key = struct
  type t = Mentat_workspace.Path.t * int

  let compare (left_path, left_line) (right_path, right_line) =
    let path = Mentat_workspace.Path.compare left_path right_path in
    if path <> 0 then path else Int.compare left_line right_line
end

module Match_key_set = Set.Make (Match_key)

let unique_match_events events =
  let add (seen, matches) event =
    match event.Rg_event.kind with
    | Rg_event.Context -> (seen, matches)
    | Rg_event.Match ->
        let key = (event.Rg_event.path, event.Rg_event.line_number) in
        if Match_key_set.mem key seen then (seen, matches)
        else (Match_key_set.add key seen, event :: matches)
  in
  let _, matches = List.fold_left add (Match_key_set.empty, []) events in
  List.rev matches

let search_counts skipped events =
  let skipped_paths = skipped_path_set skipped in
  let add counts event =
    if Mentat_workspace.Path.Set.mem event.Rg_event.path skipped_paths then
      counts
    else
      let current =
        Option.value
          (Mentat_workspace.Path.Map.find_opt event.Rg_event.path counts)
          ~default:0
      in
      Mentat_workspace.Path.Map.add event.Rg_event.path (current + 1) counts
  in
  events |> unique_match_events
  |> List.fold_left add Mentat_workspace.Path.Map.empty
  |> Mentat_workspace.Path.Map.bindings
  |> List.map (fun (count_path, matching_lines) ->
      { Output.count_path; matching_lines })

let matching_events skipped events =
  events |> filtered_events skipped |> unique_match_events
  |> List.sort event_order

let strip_trailing_newline text =
  let length = String.length text in
  if length > 0 && Char.equal text.[length - 1] '\n' then
    Text_helpers.strip_trailing_cr (String.sub text 0 (length - 1))
  else Text_helpers.strip_trailing_cr text

let output_line kind event =
  let text =
    strip_trailing_newline (Option.value event.Rg_event.text ~default:"")
  in
  let truncated = String.length text > max_line_bytes in
  let text =
    if truncated then Text_helpers.valid_utf8_prefix text max_line_bytes
    else text
  in
  { Output.number = event.Rg_event.line_number; text; kind; truncated }

module Int_map = Map.Make (Int)

let add_span_line lines line =
  match Int_map.find_opt line.Output.number lines with
  | Some existing when existing.Output.kind = Output.Match -> lines
  | Some _ | None -> Int_map.add line.Output.number line lines

let spans_of_lines path lines =
  let finish spans current =
    match current with
    | [] -> spans
    | lines ->
        let lines = List.rev lines in
        if List.exists (fun line -> line.Output.kind = Output.Match) lines then
          { Output.span_path = path; lines } :: spans
        else spans
  in
  let rec loop spans current previous = function
    | [] -> List.rev (finish spans current)
    | (number, line) :: bindings ->
        if previous + 1 = number then
          loop spans (line :: current) number bindings
        else loop (finish spans current) [ line ] number bindings
  in
  match Int_map.bindings lines with
  | [] -> []
  | (number, line) :: bindings -> loop [] [ line ] number bindings

let context_for_page ~context page_matches event =
  context > 0
  && event.Rg_event.kind = Rg_event.Context
  && List.exists
       (fun matched ->
         Mentat_workspace.Path.equal event.Rg_event.path matched.Rg_event.path
         && abs (event.Rg_event.line_number - matched.Rg_event.line_number)
            <= context)
       page_matches

let search_matches skipped events ~context ~offset ~limit =
  let matches = matching_events skipped events in
  let total = List.length matches in
  let page_matches = matches |> List.drop (offset - 1) |> List.take limit in
  let is_page_match event =
    List.exists
      (fun matched ->
        Mentat_workspace.Path.equal event.Rg_event.path matched.Rg_event.path
        && event.Rg_event.line_number = matched.Rg_event.line_number)
      page_matches
  in
  let add spans event =
    if not (is_page_match event || context_for_page ~context page_matches event)
    then spans
    else
      let kind =
        match event.Rg_event.kind with
        | Rg_event.Match -> Output.Match
        | Rg_event.Context -> Output.Context
      in
      let lines =
        Option.value
          (Mentat_workspace.Path.Map.find_opt event.Rg_event.path spans)
          ~default:Int_map.empty
      in
      Mentat_workspace.Path.Map.add event.Rg_event.path
        (add_span_line lines (output_line kind event))
        spans
  in
  let spans =
    events |> filtered_events skipped |> List.sort event_order
    |> List.fold_left add Mentat_workspace.Path.Map.empty
    |> Mentat_workspace.Path.Map.bindings
    |> List.concat_map (fun (path, lines) -> spans_of_lines path lines)
  in
  (spans, total, List.length page_matches)

let page input ~returned ~total =
  let offset = Input.effective_offset input in
  let limit = Input.effective_limit input in
  let next =
    if returned > 0 && offset + returned <= total then
      Some (Input.continuation input ~offset:(offset + returned) ~limit)
    else None
  in
  { Output.offset; limit; returned; total = Output.Exact total; next }

let output input skipped events =
  let offset = Input.effective_offset input in
  let limit = Input.effective_limit input in
  let make result returned total =
    {
      Output.pattern = input.Input.pattern;
      mode = input.Input.mode;
      result;
      matching_files = List.length (search_files skipped events);
      page = page input ~returned ~total;
      skipped;
    }
  in
  match input.Input.mode with
  | Input.Files ->
      let files = search_files skipped events in
      let total = List.length files in
      let returned = files |> List.drop (offset - 1) |> List.take limit in
      make (Output.Files returned) (List.length returned) total
  | Input.Count ->
      let counts = search_counts skipped events in
      let total = List.length counts in
      let total_matching_lines =
        List.fold_left
          (fun total count -> total + count.Output.matching_lines)
          0 counts
      in
      let returned = counts |> List.drop (offset - 1) |> List.take limit in
      make
        (Output.Count
           {
             files = returned;
             total_matching_lines = Output.Exact total_matching_lines;
           })
        (List.length returned) total
  | Input.Matches ->
      let spans, total, returned =
        search_matches skipped events
          ~context:(Input.effective_context input)
          ~offset ~limit
      in
      make (Output.Matches spans) returned total

let run workspace_io ~clock ~cancelled input =
  if cancelled () then Mentat_tool.Result.cancelled ()
  else
    match resolve_roots workspace_io ~cancelled input with
    | Error Cancelled -> Mentat_tool.Result.cancelled ()
    | Error error -> failed error
    | Ok roots -> (
        match run_rg workspace_io ~clock ~cancelled input roots with
        | Error Cancelled -> Mentat_tool.Result.cancelled ()
        | Error error -> failed error
        | Ok (skipped, events) ->
            Mentat_tool.Result.completed
              ~output:(output input skipped events)
              ())

let permissions workspace_io input =
  let rec loop seen requests = function
    | [] -> List.rev requests
    | raw :: raws -> (
        match Mentat_workspace_io.resolve_path workspace_io raw with
        | Error _ -> loop seen requests raws
        | Ok path ->
            if Mentat_workspace.Path.Set.mem path seen then
              loop seen requests raws
            else
              let request =
                Mentat_permission.Request.of_accesses ~source:name
                  [ Mentat_permission.Access.path ~op:`Read path ]
              in
              loop
                (Mentat_workspace.Path.Set.add path seen)
                (request :: requests) raws)
  in
  loop Mentat_workspace.Path.Set.empty [] (Input.effective_paths input)

let make workspace_io ~clock =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.search_text
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io)
    ~run:(fun ~cancelled input -> run workspace_io ~clock ~cancelled input)
    ()
