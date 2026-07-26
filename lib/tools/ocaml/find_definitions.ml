(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_bool value = Jsont.Json.bool value
let max_source_bytes = 8 * 1024 * 1024

module Syntax = Mentat_ocaml

let name = "ocaml_find_definitions"

module Kind = struct
  type t = Definition | Declaration | Type_definition

  let of_string = function
    | "definition" -> Definition
    | "declaration" -> Declaration
    | "type-definition" -> Type_definition
    | value -> invalid_arg ("unknown kind: " ^ value)

  let to_string = function
    | Definition -> "definition"
    | Declaration -> "declaration"
    | Type_definition -> "type-definition"
end

module Input = struct
  type t = {
    path : string;
    position : Syntax.Position.t;
    identifier : string option;
    kind : Kind.t;
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

  let validate_identifier kind = function
    | None -> ()
    | Some identifier when String.is_empty identifier ->
        invalid_arg "identifier must not be empty"
    | Some identifier when String.contains identifier '\x00' ->
        invalid_arg "identifier must not contain NUL"
    | Some _ when kind = Kind.Type_definition ->
        invalid_arg "identifier cannot be used with type-definition lookups"
    | Some _ -> ()

  let make path line column identifier requested_kind =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    if String.is_empty path then invalid_arg "path must not be empty";
    if String.contains path '\x00' then invalid_arg "path must not contain NUL";
    if line < 1 then invalid_arg "line must be at least 1";
    if column < 0 then invalid_arg "column must be non-negative";
    let kind =
      match requested_kind with
      | None -> Kind.Definition
      | Some requested_kind -> Kind.of_string requested_kind
    in
    validate_identifier kind identifier;
    { path; position = Syntax.Position.make ~line ~column; identifier; kind }

  let object_codec =
    Jsont.Object.map ~kind:"ocaml_find_definitions input" make
    |> Jsont.Object.mem "path" Jsont.string ~enc:(fun input -> input.path)
    |> Jsont.Object.mem "line" exact_integer ~enc:(fun input ->
        Syntax.Position.line input.position)
    |> Jsont.Object.mem "column" exact_integer ~enc:(fun input ->
        Syntax.Position.column input.position)
    |> Jsont.Object.opt_mem "identifier" Jsont.string ~enc:(fun input ->
        input.identifier)
    |> Jsont.Object.opt_mem "kind" Jsont.string ~enc:(fun input ->
        match input.kind with
        | Kind.Definition -> None
        | kind -> Some (Kind.to_string kind))
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec =
    Codec.strict_object ~kind:"strict ocaml_find_definitions input" object_codec

  let schema_integer description ~minimum =
    Codec.obj
      [
        ("type", json_string "integer");
        ("description", json_string description);
        ("minimum", json_int minimum);
        ("maximum", Jsont.Json.number max_input_integer);
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
                schema_integer "One-based source line of the lookup cursor."
                  ~minimum:1 );
              ( "column",
                schema_integer
                  "Zero-based byte column in the source line, matching OCaml \
                   and Merlin locations."
                  ~minimum:0 );
              ( "identifier",
                Codec.obj
                  [
                    ("type", json_string "string");
                    ( "description",
                      json_string
                        "Optional Merlin locate prefix. Omit it to locate the \
                         identifier under the cursor." );
                    ("minLength", json_int 1);
                  ] );
              ( "kind",
                Codec.obj
                  [
                    ("type", json_string "string");
                    ( "enum",
                      Jsont.Json.list
                        [
                          json_string "definition";
                          json_string "declaration";
                          json_string "type-definition";
                        ] );
                    ( "description",
                      json_string
                        "Lookup kind. Defaults to definition. type-definition \
                         cannot be combined with identifier." );
                  ] );
            ] );
        ( "required",
          Jsont.Json.list
            [ json_string "path"; json_string "line"; json_string "column" ] );
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

type merlin_found = { file : string option; found_position : Syntax.Position.t }
type merlin_response = Found of merlin_found | At_origin | Not_found of string

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

let parse_found = function
  | Jsont.Object (fields, _) when exact_member_names [ "pos" ] fields -> (
      match required_member "pos" fields with
      | Error _ as error -> error
      | Ok position ->
          Result.map
            (fun found_position -> Found { file = None; found_position })
            (parse_position position))
  | Jsont.Object (fields, _) when exact_member_names [ "file"; "pos" ] fields
    -> (
      match (required_member "file" fields, required_member "pos" fields) with
      | Ok (Jsont.String (file, _)), Ok position ->
          if String.is_empty file then Error "target file is empty"
          else if String.contains file '\x00' then
            Error "target file contains NUL"
          else
            Result.map
              (fun found_position -> Found { file = Some file; found_position })
              (parse_position position)
      | Ok _, Ok _ -> Error "target file is not a string"
      | Error diagnostic, _ | _, Error diagnostic -> Error diagnostic)
  | Jsont.Object _ -> Error "target has unexpected or duplicate members"
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      Error "target is not an object"

let not_found_detail message =
  match
    Text_helpers.bounded_diagnostic ~max_bytes:Merlin.max_detail_bytes message
  with
  | "" -> "ocamlmerlin could not locate a definition"
  | detail -> detail

let parse_merlin_response = function
  | Jsont.String ("Already at definition point", _) -> Ok At_origin
  | Jsont.String (message, _) ->
      if String.is_empty message then Error "empty locate response"
      else if String.contains message '\n' || String.contains message '\r' then
        Error "multi-line locate response"
      else Ok (Not_found (not_found_detail message))
  | Jsont.Object _ as value -> parse_found value
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.Array _ ->
      Error "locate value is neither a string nor an object"

type target_kind = Workspace | External

type target = {
  target_kind : target_kind;
  address : string;
  position : Syntax.Position.t;
}

let compare_target_kind left right =
  match (left, right) with
  | Workspace, Workspace | External, External -> 0
  | Workspace, External -> -1
  | External, Workspace -> 1

let compare_target left right =
  match compare_target_kind left.target_kind right.target_kind with
  | 0 -> (
      match String.compare left.address right.address with
      | 0 -> Syntax.Position.compare left.position right.position
      | order -> order)
  | order -> order

let normalize_target workspace_io ~source_path ~source_root ~cwd
    (found : merlin_found) =
  let target_file =
    match found.file with
    | None | Some "*buffer*" -> Ok (`Workspace source_path)
    | Some file -> (
        match Lpath.Abs.resolve_any ~base:cwd file with
        | Error error -> Error (Lpath.Error.message error)
        | Ok absolute -> (
            match
              Merlin_support.workspace_path_of_absolute workspace_io
                ~source_root ~cwd absolute
            with
            | Some path -> Ok (`Workspace path)
            | None -> Ok (`External (Lpath.Abs.to_string absolute))))
  in
  match target_file with
  | Error diagnostic ->
      Error ("could not normalize ocamlmerlin target: " ^ diagnostic)
  | Ok (`Workspace path) ->
      Result.map
        (fun address ->
          { target_kind = Workspace; address; position = found.found_position })
        (Address.provider workspace_io path)
  | Ok (`External address) ->
      Ok { target_kind = External; address; position = found.found_position }

let locate_args ~filename input =
  let position = Format.asprintf "%a" Syntax.Position.pp input.Input.position in
  match input.Input.kind with
  | Kind.Type_definition ->
      ("locate-type", [ "-position"; position; "-filename"; filename ])
  | Kind.Definition | Kind.Declaration ->
      let look_for =
        match input.Input.kind with
        | Kind.Definition -> "implementation"
        | Kind.Declaration -> "interface"
        | Kind.Type_definition -> assert false
      in
      let args =
        [ "-position"; position; "-look-for"; look_for; "-filename"; filename ]
      in
      let args =
        match input.Input.identifier with
        | None -> args
        | Some identifier -> args @ [ "-prefix"; identifier ]
      in
      ("locate", args)

type index_status = Not_applicable | Unknown
type output = { definitions : target list; index_status : index_status }

module Output = struct
  let index_status_string = function
    | Not_applicable -> "not_applicable"
    | Unknown -> "unknown"

  let target_text (target : target) =
    let line = Syntax.Position.line target.position in
    let column = Syntax.Position.column target.position in
    match target.target_kind with
    | Workspace ->
        Printf.sprintf "%s:%d:%d-%d:%d" target.address line column line column
    | External -> Printf.sprintf "%s:%d:%d" target.address line column

  let text (output : output) =
    let buffer = Buffer.create 160 in
    (match output.definitions with
    | [] -> Buffer.add_string buffer "OCaml definitions: none\n"
    | definitions ->
        Printf.bprintf buffer "OCaml definitions: %d\n"
          (List.length definitions);
        List.iter
          (fun target ->
            Buffer.add_string buffer "- ";
            Buffer.add_string buffer (target_text target);
            Buffer.add_char buffer '\n')
          definitions);
    Buffer.add_string buffer
      ("index_status: " ^ index_status_string output.index_status);
    Buffer.contents buffer

  let encode output =
    let semantic =
      match output.definitions with
      | [ target ] ->
          Mentat_tools_output.Ocaml.Definition.make ~path:target.address
            ~line:(Syntax.Position.line target.position)
      | [] ->
          invalid_arg "ocaml_find_definitions completed without a definition"
      | _ :: _ :: _ ->
          invalid_arg
            "ocaml_find_definitions completed with multiple definitions"
    in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Ocaml.Definition.jsont
      ~text:(text output) semantic
end

let interrupted () = Mentat_tool.Result.cancelled ()

let completed_output workspace_io ~source_path ~source_root ~cwd ~query_address
    input response =
  let target, index_status =
    match response with
    | At_origin ->
        ( Ok
            {
              target_kind = Workspace;
              address = query_address;
              position = input.Input.position;
            },
          Not_applicable )
    | Found found ->
        ( normalize_target workspace_io ~source_path ~source_root ~cwd found,
          Unknown )
    | Not_found _ -> assert false
  in
  match target with
  | Error diagnostic -> Mentat_tool.Result.failed `Failed diagnostic
  | Ok target ->
      let definitions = List.sort_uniq compare_target [ target ] in
      Mentat_tool.Result.completed ~output:{ definitions; index_status } ()

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
          | Ok query_address, Ok filename, Ok cwd_absolute -> (
              let filename = Lpath.Abs.to_string filename in
              let command, args = locate_args ~filename input in
              match
                Merlin.run workspace_io ~clock ~program ~cwd:source_root
                  ~command ~args ~source ~cancelled
              with
              | Error Merlin.Cancelled -> interrupted ()
              | Error error -> Merlin_support.merlin_failure error
              | Ok value -> (
                  if cancelled () then interrupted ()
                  else
                    match parse_merlin_response value with
                    | Error diagnostic ->
                        Mentat_tool.Result.failed `Failed
                          ("could not decode ocamlmerlin locate result: "
                          ^ Text_helpers.bounded_diagnostic
                              ~max_bytes:Merlin.max_detail_bytes diagnostic)
                    | Ok (Not_found diagnostic) ->
                        Mentat_tool.Result.failed `Not_found diagnostic
                    | Ok ((At_origin | Found _) as response) ->
                        completed_output workspace_io ~source_path ~source_root
                          ~cwd:cwd_absolute ~query_address input response)))

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
    invalid_arg "Ocaml.Find_definitions.make: program prefix must not be empty";
  let execution = Confinement.confined workspace_io in
  Mentat_tool.make ~name
    ~description:Mentat_prompts.Tools.ocaml_find_definitions
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io ~execution)
    ~run:(fun ~cancelled input ->
      run workspace_io ~clock ~program ~cancelled input)
    ()
