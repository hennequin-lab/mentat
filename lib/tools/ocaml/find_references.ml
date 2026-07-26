(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let max_source_bytes = 8 * 1024 * 1024

module Syntax = Mentat_ocaml

let name = "ocaml_find_references"
let default_limit = 200
let max_limit = 1_000
let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_bool value = Jsont.Json.bool value

let json_to_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok text -> text
  | Error diagnostic -> invalid_arg ("could not encode JSON: " ^ diagnostic)

module Scope = struct
  type t = Buffer | Project | Renaming

  let of_string = function
    | "buffer" -> Buffer
    | "project" -> Project
    | "renaming" -> Renaming
    | _ -> invalid_arg "scope must be one of buffer, project, or renaming"

  let to_string = function
    | Buffer -> "buffer"
    | Project -> "project"
    | Renaming -> "renaming"
end

module Input = struct
  type t = {
    path : string;
    position : Syntax.Position.t;
    scope : Scope.t;
    include_stale : bool;
    offset : int option;
    limit : int option;
  }

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
    Jsont.map ~kind:"integer" ~dec:decode ~enc:json_int Jsont.json

  let make path line column requested_scope requested_include_stale offset limit
      =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    if String.is_empty path then invalid_arg "path must not be empty";
    if String.contains path '\x00' then invalid_arg "path must not contain NUL";
    if line < 1 then invalid_arg "line must be at least 1";
    if column < 0 then invalid_arg "column must be non-negative";
    (match offset with
    | Some offset when offset < 1 -> invalid_arg "offset must be at least 1"
    | Some _ | None -> ());
    (match limit with
    | Some limit when limit < 1 -> invalid_arg "limit must be positive"
    | Some limit when limit > max_limit ->
        invalid_arg ("limit must be at most " ^ string_of_int max_limit)
    | Some _ | None -> ());
    let scope =
      match requested_scope with
      | None -> Scope.Project
      | Some requested_scope -> Scope.of_string requested_scope
    in
    let include_stale = Option.value requested_include_stale ~default:false in
    {
      path;
      position = Syntax.Position.make ~line ~column;
      scope;
      include_stale;
      offset;
      limit;
    }

  let object_codec =
    Jsont.Object.map ~kind:"ocaml_find_references input" make
    |> Jsont.Object.mem "path" Jsont.string ~enc:(fun input -> input.path)
    |> Jsont.Object.mem "line" exact_integer ~enc:(fun input ->
        Syntax.Position.line input.position)
    |> Jsont.Object.mem "column" exact_integer ~enc:(fun input ->
        Syntax.Position.column input.position)
    |> Jsont.Object.opt_mem "scope" Jsont.string ~enc:(fun input ->
        match input.scope with
        | Scope.Project -> None
        | scope -> Some (Scope.to_string scope))
    |> Jsont.Object.opt_mem "include_stale" Jsont.bool ~enc:(fun input ->
        if input.include_stale then Some true else None)
    |> Jsont.Object.opt_mem "offset" exact_integer ~enc:(fun input ->
        input.offset)
    |> Jsont.Object.opt_mem "limit" exact_integer ~enc:(fun input ->
        input.limit)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec =
    Codec.strict_object ~kind:"strict ocaml_find_references input" object_codec

  let schema_integer description ~minimum ~maximum =
    Codec.obj
      [
        ("type", json_string "integer");
        ("description", json_string description);
        ("minimum", json_int minimum);
        ("maximum", maximum);
      ]

  let schema =
    Codec.obj
      [
        ("type", json_string "object");
        ( "properties",
          Codec.obj
            [
              ( "path",
                Codec.obj
                  [
                    ("type", json_string "string");
                    ( "description",
                      json_string
                        "Workspace-relative or workspace-contained absolute \
                         OCaml source file path." );
                    ("minLength", json_int 1);
                  ] );
              ( "line",
                schema_integer "One-based source line of the identifier cursor."
                  ~minimum:1
                  ~maximum:(Jsont.Json.number max_input_integer) );
              ( "column",
                schema_integer
                  "Zero-based byte column in the source line, matching OCaml \
                   and Merlin locations."
                  ~minimum:0
                  ~maximum:(Jsont.Json.number max_input_integer) );
              ( "scope",
                Codec.obj
                  [
                    ("type", json_string "string");
                    ( "enum",
                      Jsont.Json.list
                        [
                          json_string "buffer";
                          json_string "project";
                          json_string "renaming";
                        ] );
                    ( "description",
                      json_string
                        "Merlin occurrence scope. buffer searches only the \
                         current source; project and renaming consult project \
                         occurrence indexes. Defaults to project." );
                  ] );
              ( "include_stale",
                Codec.obj
                  [
                    ("type", json_string "boolean");
                    ( "description",
                      json_string
                        "Include occurrences Merlin marks stale. Defaults to \
                         false." );
                  ] );
              ( "offset",
                schema_integer
                  "One-based first reference after stale filtering and \
                   canonical deduplication. Defaults to 1."
                  ~minimum:1
                  ~maximum:(Jsont.Json.number max_input_integer) );
              ( "limit",
                schema_integer "Maximum references returned. Defaults to 200."
                  ~minimum:1 ~maximum:(json_int max_limit) );
            ] );
        ( "required",
          Jsont.Json.list
            [ json_string "path"; json_string "line"; json_string "column" ] );
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
  let effective_offset input = Option.value input.offset ~default:1
  let effective_limit input = Option.value input.limit ~default:default_limit

  let json ~path ~offset input =
    Codec.obj
      [
        ("path", json_string path);
        ("line", json_int (Syntax.Position.line input.position));
        ("column", json_int (Syntax.Position.column input.position));
        ("scope", json_string (Scope.to_string input.scope));
        ("include_stale", json_bool input.include_stale);
        ("offset", json_int offset);
        ("limit", json_int (effective_limit input));
      ]
end

let named_values name fields =
  List.filter_map
    (fun ((member_name, _), value) ->
      if String.equal member_name name then Some value else None)
    fields

let required_member name fields =
  match named_values name fields with
  | [ value ] -> Ok value
  | [] -> Error ("missing " ^ name)
  | _ :: _ :: _ -> Error ("duplicate " ^ name)

let exact_member_names expected fields =
  let actual = List.map (fun ((member_name, _), _) -> member_name) fields in
  let sort = List.sort String.compare in
  List.equal String.equal (sort expected) (sort actual)

let safe_integer = function
  | Jsont.Number (value, _)
    when Float.is_integer value && Float.abs value <= Input.max_input_integer ->
      Some (int_of_float value)
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ | Jsont.Object _ ->
      None

let parse_position = function
  | Jsont.Object (fields, _) when exact_member_names [ "line"; "col" ] fields
    -> (
      match (required_member "line" fields, required_member "col" fields) with
      | Ok line, Ok column -> (
          match (safe_integer line, safe_integer column) with
          | Some line, Some column -> (
              try Ok (Syntax.Position.make ~line ~column)
              with Invalid_argument diagnostic -> Error diagnostic)
          | None, _ | _, None -> Error "position members must be safe integers")
      | Error diagnostic, _ | _, Error diagnostic -> Error diagnostic)
  | Jsont.Object _ -> Error "position has unexpected or duplicate members"
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      Error "position is not an object"

type raw_occurrence = {
  raw_file : string option;
  raw_range : Syntax.Range.t;
  raw_stale : bool;
}

let parse_file = function
  | Jsont.String (file, _) ->
      if String.contains file '\x00' then Error "file contains NUL"
      else Ok (Some file)
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.Array _
  | Jsont.Object _ ->
      Error "file is not a string"

let parse_stale = function
  | Jsont.Bool (stale, _) -> Ok stale
  | Jsont.Null _ | Jsont.Number _ | Jsont.String _ | Jsont.Array _
  | Jsont.Object _ ->
      Error "stale is not a boolean"

let parse_occurrence scope = function
  | Jsont.Object (fields, _)
    when exact_member_names
           (match scope with
           | Scope.Buffer -> [ "start"; "end"; "stale" ]
           | Scope.Project | Scope.Renaming ->
               [ "file"; "start"; "end"; "stale" ])
           fields -> (
      match
        ( required_member "start" fields,
          required_member "end" fields,
          required_member "stale" fields )
      with
      | Ok start, Ok end_, Ok stale -> (
          let file =
            match scope with
            | Scope.Buffer -> Ok None
            | Scope.Project | Scope.Renaming -> (
                match required_member "file" fields with
                | Error _ as error -> error
                | Ok file -> parse_file file)
          in
          match
            (parse_position start, parse_position end_, file, parse_stale stale)
          with
          | Ok start, Ok end_, Ok raw_file, Ok raw_stale -> (
              try
                Ok
                  {
                    raw_file;
                    raw_range = Syntax.Range.make ~start ~end_;
                    raw_stale;
                  }
              with Invalid_argument diagnostic -> Error diagnostic)
          | Error diagnostic, _, _, _
          | _, Error diagnostic, _, _
          | _, _, Error diagnostic, _
          | _, _, _, Error diagnostic ->
              Error diagnostic)
      | Error diagnostic, _, _ | _, Error diagnostic, _ | _, _, Error diagnostic
        ->
          Error diagnostic)
  | Jsont.Object _ -> Error "occurrence has unexpected or duplicate members"
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      Error "occurrence is not an object"

let parse_occurrences scope = function
  | Jsont.Array (items, _) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | item :: items -> (
            match parse_occurrence scope item with
            | Ok occurrence -> loop (occurrence :: acc) items
            | Error _ as error -> error)
      in
      loop [] items
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Object _ ->
      Error "occurrences value is not an array"

type reference = { address : string; range : Syntax.Range.t; stale : bool }

let compare_reference left right =
  match String.compare left.address right.address with
  | 0 -> Syntax.Range.compare left.range right.range
  | order -> order

let normalize_occurrence workspace_io ~source_path ~source_root ~cwd occurrence
    =
  let normalized_path =
    match occurrence.raw_file with
    | None | Some "" | Some "*buffer*" -> Ok source_path
    | Some file -> (
        match Lpath.Abs.resolve_any ~base:cwd file with
        | Error error -> Error (Lpath.Error.message error)
        | Ok absolute -> (
            match
              Merlin_support.workspace_path_of_absolute workspace_io
                ~source_root ~cwd absolute
            with
            | Some path -> Ok path
            | None ->
                Error
                  ("path is outside the workspace: "
                  ^ Lpath.Abs.to_string absolute)))
  in
  match normalized_path with
  | Error diagnostic ->
      Error ("could not normalize ocamlmerlin occurrence: " ^ diagnostic)
  | Ok path ->
      Result.map
        (fun address ->
          {
            address;
            range = occurrence.raw_range;
            stale = occurrence.raw_stale;
          })
        (Address.provider workspace_io path)

let normalize_occurrences workspace_io ~source_path ~source_root ~cwd
    occurrences =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | occurrence :: occurrences -> (
        match
          normalize_occurrence workspace_io ~source_path ~source_root ~cwd
            occurrence
        with
        | Ok reference -> loop (reference :: acc) occurrences
        | Error _ as error -> error)
  in
  loop [] occurrences

(* Equal locations are one semantic occurrence. If Merlin reports conflicting
   freshness bits, a fresh observation dominates so default filtering cannot
   discard a location the backend also reported as current. *)
let sort_and_deduplicate references =
  let sorted = List.sort compare_reference references in
  let rec loop acc = function
    | [] -> List.rev acc
    | reference :: references -> (
        match acc with
        | previous :: acc when compare_reference previous reference = 0 ->
            let merged =
              { previous with stale = previous.stale && reference.stale }
            in
            loop (merged :: acc) references
        | [] | _ :: _ -> loop (reference :: acc) references)
  in
  loop [] sorted

let occurrences_args ~filename input =
  [
    "-identifier-at";
    Format.asprintf "%a" Syntax.Position.pp input.Input.position;
    "-scope";
    Scope.to_string input.Input.scope;
    "-filename";
    filename;
  ]

type index_status = Not_applicable | Unknown
type status = Complete | Partial

type output = {
  query_address : string;
  input : Input.t;
  references : reference list;
  reported_count : int;
  eligible_count : int;
  eligible_files : int;
  stale_skipped : int;
  duplicate_skipped : int;
  offset : int;
  status : status;
  next : Jsont.json option;
  index_status : index_status;
}

module Output = struct
  let index_status_to_string = function
    | Not_applicable -> "not_applicable"
    | Unknown -> "unknown"

  let status_to_string = function
    | Complete -> "complete"
    | Partial -> "partial"

  let has_more output = output.status = Partial

  let reference_line reference =
    Printf.sprintf "- %s:%s%s" reference.address
      (Format.asprintf "%a" Syntax.Range.pp reference.range)
      (if reference.stale then " stale" else "")

  let text output =
    let position = output.input.Input.position in
    let buffer = Buffer.create 512 in
    Printf.bprintf buffer "OCaml references for %s:%d:%d\n" output.query_address
      (Syntax.Position.line position)
      (Syntax.Position.column position);
    Printf.bprintf buffer "scope: %s\n"
      (Scope.to_string output.input.Input.scope);
    Printf.bprintf buffer "references: %d returned of %d"
      (List.length output.references)
      output.reported_count;
    if output.stale_skipped > 0 then
      Printf.bprintf buffer ", %d stale skipped" output.stale_skipped;
    if output.duplicate_skipped > 0 then
      Printf.bprintf buffer ", %d duplicate skipped" output.duplicate_skipped;
    Printf.bprintf buffer ", offset %d, status %s\n" output.offset
      (status_to_string output.status);
    Printf.bprintf buffer "index_status: %s\n"
      (index_status_to_string output.index_status);
    Buffer.add_string buffer "backend: ocamlmerlin";
    List.iter
      (fun reference ->
        Buffer.add_char buffer '\n';
        Buffer.add_string buffer (reference_line reference))
      output.references;
    (match output.next with
    | None -> ()
    | Some next ->
        Buffer.add_char buffer '\n';
        Buffer.add_string buffer "next: ocaml_find_references ";
        Buffer.add_string buffer (json_to_string next));
    if output.stale_skipped > 0 && not output.input.Input.include_stale then begin
      Buffer.add_char buffer '\n';
      Buffer.add_string buffer
        "stale note: rebuild the project index, for Dune usually `dune build \
         @ocaml-index`."
    end;
    String.trim (Buffer.contents buffer)

  let encode output =
    let semantic =
      Mentat_tools_output.Ocaml.References.make
        ~references:output.eligible_count ~files:output.eligible_files
    in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Ocaml.References.jsont
      ~text:(text output) ~truncated:(has_more output) semantic
end

let interrupted () = Mentat_tool.Result.cancelled ()

let take count values =
  let rec loop acc remaining = function
    | _ when remaining = 0 -> List.rev acc
    | [] -> List.rev acc
    | value :: values -> loop (value :: acc) (remaining - 1) values
  in
  loop [] count values

let drop count values =
  let rec loop remaining values =
    if remaining = 0 then values
    else
      match values with [] -> [] | _ :: values -> loop (remaining - 1) values
  in
  loop count values

let index_status_of_scope = function
  | Scope.Buffer -> Not_applicable
  | Scope.Project | Scope.Renaming -> Unknown

let assemble ~query_address input references ~reported_count =
  let unique_count = List.length references in
  let duplicate_skipped = reported_count - unique_count in
  let references, stale_skipped =
    if input.Input.include_stale then (references, 0)
    else
      let fresh =
        List.filter (fun reference -> not reference.stale) references
      in
      (fresh, unique_count - List.length fresh)
  in
  let eligible_count = List.length references in
  let eligible_files =
    references
    |> List.map (fun reference -> reference.address)
    |> List.sort_uniq String.compare
    |> List.length
  in
  let offset = Input.effective_offset input in
  let limit = Input.effective_limit input in
  let page = references |> drop (offset - 1) |> take limit in
  let returned = List.length page in
  let has_more =
    offset <= eligible_count && returned <= eligible_count - offset
  in
  let status = if has_more then Partial else Complete in
  let next =
    if has_more then
      Some (Input.json ~path:query_address ~offset:(offset + returned) input)
    else None
  in
  {
    query_address;
    input;
    references = page;
    reported_count;
    eligible_count;
    eligible_files;
    stale_skipped;
    duplicate_skipped;
    offset;
    status;
    next;
    index_status = index_status_of_scope input.Input.scope;
  }

let run workspace_io ~clock ~program ~cancelled input =
  if cancelled () then interrupted ()
  else
    match
      Merlin_support.resolve_source workspace_io ~path:input.Input.path
        ~max_bytes:max_source_bytes
    with
    | Error result -> result
    | Ok (source_path, source) -> (
        if cancelled () then interrupted ()
        else
          let source_root = Mentat_workspace.Path.root_of source_path in
          match
            ( Address.provider workspace_io source_path,
              Mentat_workspace_io.to_abs workspace_io source_path,
              Mentat_workspace_io.to_abs workspace_io source_root )
          with
          | Error diagnostic, _, _ ->
              Mentat_tool.Result.failed `Failed diagnostic
          | _, Error error, _ | _, _, Error error ->
              Mentat_tool.Result.failed `Failed
                (Mentat_workspace.Resolve_error.message error)
          | Ok query_address, Ok filename, Ok cwd -> (
              let filename = Lpath.Abs.to_string filename in
              match
                Merlin.run workspace_io ~clock ~program ~cwd:source_root
                  ~command:"occurrences"
                  ~args:(occurrences_args ~filename input)
                  ~source ~cancelled
              with
              | Error Merlin.Cancelled -> interrupted ()
              | Error error -> Merlin_support.merlin_failure error
              | Ok value -> (
                  if cancelled () then interrupted ()
                  else
                    match parse_occurrences input.Input.scope value with
                    | Error diagnostic ->
                        Mentat_tool.Result.failed `Failed
                          ("could not decode ocamlmerlin occurrences result: "
                          ^ Text_helpers.bounded_diagnostic
                              ~max_bytes:Merlin.max_detail_bytes diagnostic)
                    | Ok occurrences -> (
                        match
                          normalize_occurrences workspace_io ~source_path
                            ~source_root ~cwd occurrences
                        with
                        | Error diagnostic ->
                            Mentat_tool.Result.failed `Failed
                              (Text_helpers.bounded_diagnostic
                                 ~max_bytes:Merlin.max_detail_bytes diagnostic)
                        | Ok references ->
                            let reported_count = List.length references in
                            let references = sort_and_deduplicate references in
                            Mentat_tool.Result.completed
                              ~output:
                                (assemble ~query_address input references
                                   ~reported_count)
                              ()))))

let permissions workspace_io ~execution input =
  Permissions.with_resolved workspace_io input.Input.path (fun path ->
      [
        Mentat_permission.Request.of_accesses ~source:name
          [
            Mentat_permission.Access.path ~op:`Read path;
            Confinement.custom_access execution;
          ];
      ])

let make workspace_io ~clock ~program =
  if List.is_empty program then
    invalid_arg "Ocaml.Find_references.make: program prefix must not be empty";
  let execution = Confinement.confined workspace_io in
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.ocaml_find_references
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io ~execution)
    ~run:(fun ~cancelled input ->
      run workspace_io ~clock ~program ~cancelled input)
    ()
