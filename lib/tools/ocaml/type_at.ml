(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let max_source_bytes = 8 * 1024 * 1024

module Syntax = Mentat_ocaml

let name = "ocaml_type_at"
let default_max_enclosings = 1
let max_enclosings = 8
let max_verbosity = 3
let max_type_bytes = 4 * 1024
let max_documentation_bytes = 8 * 1024
let printer_width = 80
let max_enclosings_bound = max_enclosings
let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_bool value = Jsont.Json.bool value

module Input = struct
  type t = {
    path : string;
    position : Syntax.Position.t;
    max_enclosings : int;
    verbosity : int;
    documentation : bool;
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

  let make path line column requested_enclosings requested_verbosity
      requested_documentation =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    if String.is_empty path then invalid_arg "path must not be empty";
    if String.contains path '\x00' then invalid_arg "path must not contain NUL";
    if line < 1 then invalid_arg "line must be at least 1";
    if column < 0 then invalid_arg "column must be non-negative";
    let max_enclosings =
      Option.value requested_enclosings ~default:default_max_enclosings
    in
    if max_enclosings < 1 then invalid_arg "max_enclosings must be at least 1";
    if max_enclosings > max_enclosings_bound then
      invalid_arg
        ("max_enclosings must be at most " ^ string_of_int max_enclosings_bound);
    let verbosity = Option.value requested_verbosity ~default:0 in
    if verbosity < 0 then invalid_arg "verbosity must be non-negative";
    if verbosity > max_verbosity then
      invalid_arg ("verbosity must be at most " ^ string_of_int max_verbosity);
    let documentation = Option.value requested_documentation ~default:false in
    {
      path;
      position = Syntax.Position.make ~line ~column;
      max_enclosings;
      verbosity;
      documentation;
    }

  let object_codec =
    Jsont.Object.map ~kind:"ocaml_type_at input" make
    |> Jsont.Object.mem "path" Jsont.string ~enc:(fun input -> input.path)
    |> Jsont.Object.mem "line" exact_integer ~enc:(fun input ->
        Syntax.Position.line input.position)
    |> Jsont.Object.mem "column" exact_integer ~enc:(fun input ->
        Syntax.Position.column input.position)
    |> Jsont.Object.opt_mem "max_enclosings" exact_integer ~enc:(fun input ->
        if input.max_enclosings = default_max_enclosings then None
        else Some input.max_enclosings)
    |> Jsont.Object.opt_mem "verbosity" exact_integer ~enc:(fun input ->
        if input.verbosity = 0 then None else Some input.verbosity)
    |> Jsont.Object.opt_mem "documentation" Jsont.bool ~enc:(fun input ->
        if input.documentation then Some true else None)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec =
    Codec.strict_object ~kind:"strict ocaml_type_at input" object_codec

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
                schema_integer "One-based source line." ~minimum:1
                  ~maximum:(Jsont.Json.number max_input_integer) );
              ( "column",
                schema_integer
                  "Zero-based byte column in the source line, matching OCaml \
                   and Merlin locations."
                  ~minimum:0
                  ~maximum:(Jsont.Json.number max_input_integer) );
              ( "max_enclosings",
                schema_integer
                  "Maximum enclosing type frames, innermost first. Every \
                   returned frame costs one Merlin type query. Defaults to 1."
                  ~minimum:1 ~maximum:(json_int max_enclosings) );
              ( "verbosity",
                schema_integer
                  "Merlin alias and module-type expansion depth. Zero uses \
                   Merlin's default and omits the flag. Defaults to 0."
                  ~minimum:0 ~maximum:(json_int max_verbosity) );
              ( "documentation",
                Codec.obj
                  [
                    ("type", json_string "boolean");
                    ( "description",
                      json_string
                        "Fetch the entity's odoc documentation with one \
                         additional Merlin query. Defaults to false." );
                  ] );
            ] );
        ( "required",
          Jsont.Json.list
            [ json_string "path"; json_string "line"; json_string "column" ] );
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

type type_field = Printed of string | Index_ref | Absent
type raw_frame = { raw_range : Syntax.Range.t; type_field : type_field }

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

let parse_type_field = function
  | Jsont.String (value, _) -> Printed value
  | Jsont.Number _ as value -> (
      match safe_integer value with
      | Some index when index >= 0 -> Index_ref
      | Some _ | None -> Absent)
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Array _ | Jsont.Object _ -> Absent

let valid_tail = function
  | Jsont.String (("no" | "position" | "call"), _) -> true
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ | Jsont.Object _ ->
      false

let parse_frame = function
  | Jsont.Object (fields, _)
    when exact_member_names [ "start"; "end"; "type"; "tail" ] fields -> (
      match
        ( required_member "start" fields,
          required_member "end" fields,
          required_member "type" fields,
          required_member "tail" fields )
      with
      | Ok start, Ok end_, Ok type_json, Ok tail when valid_tail tail -> (
          match (parse_position start, parse_position end_) with
          | Ok start, Ok end_ -> (
              try
                Ok
                  {
                    raw_range = Syntax.Range.make ~start ~end_;
                    type_field = parse_type_field type_json;
                  }
              with Invalid_argument diagnostic -> Error diagnostic)
          | Error diagnostic, _ | _, Error diagnostic -> Error diagnostic)
      | Ok _, Ok _, Ok _, Ok _ -> Error "frame tail is invalid"
      | Error diagnostic, _, _, _
      | _, Error diagnostic, _, _
      | _, _, Error diagnostic, _
      | _, _, _, Error diagnostic ->
          Error diagnostic)
  | Jsont.Object _ -> Error "frame has unexpected or duplicate members"
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      Error "frame is not an object"

let parse_frames = function
  | Jsont.Array (frames, _) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | frame :: frames -> (
            match parse_frame frame with
            | Ok frame -> loop (frame :: acc) frames
            | Error _ as error -> error)
      in
      loop [] frames
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Object _ ->
      Error "type-enclosing value is not an array"

let dedup_adjacent (frames : raw_frame list) =
  let rec loop previous_range index acc = function
    | [] -> List.rev acc
    | (frame : raw_frame) :: frames ->
        if
          Option.exists
            (fun previous -> Syntax.Range.equal previous frame.raw_range)
            previous_range
        then loop previous_range (index + 1) acc frames
        else
          loop (Some frame.raw_range) (index + 1) ((index, frame) :: acc) frames
  in
  loop None 0 [] frames

let type_enclosing_args ~filename ~position ~index ~verbosity =
  let args =
    [
      "-position";
      Format.asprintf "%a" Syntax.Position.pp position;
      "-index";
      string_of_int index;
      "-printer-width";
      string_of_int printer_width;
      "-filename";
      filename;
    ]
  in
  if verbosity = 0 then args
  else args @ [ "-verbosity"; string_of_int verbosity ]

let document_args ~filename ~position =
  [
    "-position";
    Format.asprintf "%a" Syntax.Position.pp position;
    "-filename";
    filename;
  ]

type documentation =
  | Not_requested
  | Not_available of { reason : string; truncated : bool }
  | Available of { text : string; truncated : bool }

type frame = { range : Syntax.Range.t; type_text : string; truncated : bool }

type output = {
  address : string;
  input : Input.t;
  frames : frame list;
  documentation : documentation;
}

let truncate_utf8 ~max_bytes text =
  if String.length text <= max_bytes then (text, false)
  else (Text_helpers.valid_utf8_prefix text max_bytes, true)

let document_sentinel text =
  String.equal text "No documentation available"
  || String.equal text "Not a valid identifier"
  || String.starts_with ~prefix:"didn't manage to find" text
  || String.starts_with ~prefix:"Not in environment" text
  || String.ends_with ~suffix:"is a builtin, no documentation is available" text
  || String.ends_with ~suffix:" but could not be found" text

let interrupted () = Mentat_tool.Result.cancelled ()

let run_type_enclosing workspace_io ~clock ~program ~cwd ~filename ~position
    ~verbosity ~source ~cancelled ~index =
  match
    Merlin.run workspace_io ~clock ~program ~cwd ~command:"type-enclosing"
      ~args:(type_enclosing_args ~filename ~position ~index ~verbosity)
      ~source ~cancelled
  with
  | Error _ as error -> error
  | Ok value ->
      Result.map_error
        (fun diagnostic -> Merlin.Malformed diagnostic)
        (parse_frames value)

let printed_type ~index ~expected_range (frames : raw_frame list) =
  match List.nth_opt frames index with
  | None ->
      Error (Merlin.Malformed ("frame " ^ string_of_int index ^ " is missing"))
  | Some frame when not (Syntax.Range.equal frame.raw_range expected_range) ->
      Error
        (Merlin.Malformed ("frame " ^ string_of_int index ^ " changed range"))
  | Some { type_field = Printed type_text; _ }
    when not (String.is_empty (String.trim type_text)) ->
      Ok type_text
  | Some { type_field = Printed _; _ } ->
      Error
        (Merlin.Malformed
           ("frame " ^ string_of_int index ^ " has an empty printed type"))
  | Some { type_field = Index_ref | Absent; _ } ->
      Error
        (Merlin.Malformed
           ("frame " ^ string_of_int index ^ " has no printed type"))

let build_frame ~range type_text =
  let type_text, truncated =
    truncate_utf8 ~max_bytes:max_type_bytes type_text
  in
  { range; type_text; truncated }

let fetch_documentation workspace_io ~clock ~program ~cwd ~filename ~position
    ~source ~cancelled =
  match
    Merlin.run workspace_io ~clock ~program ~cwd ~command:"document"
      ~args:(document_args ~filename ~position)
      ~source ~cancelled
  with
  | Error Merlin.Cancelled -> `Cancelled
  | Error _ ->
      `Documentation
        (Not_available
           { reason = "documentation lookup failed"; truncated = false })
  | Ok (Jsont.String (text, _)) ->
      let absent = document_sentinel text in
      let text, truncated =
        truncate_utf8 ~max_bytes:max_documentation_bytes text
      in
      if absent then `Documentation (Not_available { reason = text; truncated })
      else `Documentation (Available { text; truncated })
  | Ok
      ( Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.Array _
      | Jsont.Object _ ) ->
      `Documentation
        (Not_available
           { reason = "documentation lookup failed"; truncated = false })

module Output = struct
  let is_truncated (output : output) =
    List.exists (fun frame -> frame.truncated) output.frames
    ||
    match output.documentation with
    | Not_requested -> false
    | Not_available { truncated; _ } | Available { truncated; _ } -> truncated

  let add_frame buffer address (frame : frame) =
    let start = Syntax.Range.start frame.range in
    Printf.bprintf buffer "- %s:%d:%d  %s%s\n" address
      (Syntax.Position.line start)
      (Syntax.Position.column start)
      frame.type_text
      (if frame.truncated then " (truncated)" else "")

  let add_documentation buffer = function
    | Not_requested -> ()
    | Not_available { reason; truncated } ->
        Printf.bprintf buffer "documentation: unavailable (%s)%s\n" reason
          (if truncated then " (truncated)" else "")
    | Available { text; truncated } ->
        Printf.bprintf buffer "documentation: %s%s\n" text
          (if truncated then " (truncated)" else "")

  let text (output : output) =
    let input = output.input in
    let buffer = Buffer.create 256 in
    Printf.bprintf buffer "OCaml type at %s:%d:%d\n" output.address
      (Syntax.Position.line input.Input.position)
      (Syntax.Position.column input.Input.position);
    List.iter (add_frame buffer output.address) output.frames;
    add_documentation buffer output.documentation;
    Buffer.add_string buffer "backend: ocamlmerlin";
    Buffer.contents buffer

  let first_line text =
    text
    |> String.map (fun char -> if Char.equal char '\r' then '\n' else char)
    |> String.split_on_char '\n' |> List.map String.trim
    |> List.find (Fun.negate String.is_empty)
    |> truncate_utf8 ~max_bytes:Mentat_tools_output.Ocaml.Type_at.max_head_bytes
    |> fst

  let encode output =
    let semantic =
      match output.frames with
      | [] -> invalid_arg "ocaml_type_at completed without a type frame"
      | frame :: additional ->
          Mentat_tools_output.Ocaml.Type_at.make
            ~head:(first_line frame.type_text)
            ~more:(List.length additional)
    in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Ocaml.Type_at.jsont
      ~text:(text output) ~truncated:(is_truncated output) semantic
end

let build_frames workspace_io ~clock ~program ~cwd ~filename ~position
    ~verbosity ~source ~cancelled raw_frames selected =
  let rec loop frames = function
    | [] -> Ok (List.rev frames)
    | (merlin_index, (raw_frame : raw_frame)) :: selected -> (
        if cancelled () then Error Merlin.Cancelled
        else
          let type_text =
            if merlin_index = 0 then
              printed_type ~index:0 ~expected_range:raw_frame.raw_range
                raw_frames
            else
              match
                run_type_enclosing workspace_io ~clock ~program ~cwd ~filename
                  ~position ~verbosity ~source ~cancelled ~index:merlin_index
              with
              | Error _ as error -> error
              | Ok targeted ->
                  printed_type ~index:merlin_index
                    ~expected_range:raw_frame.raw_range targeted
          in
          match type_text with
          | Error _ as error -> error
          | Ok type_text ->
              loop
                (build_frame ~range:raw_frame.raw_range type_text :: frames)
                selected)
  in
  loop [] selected

let take count values =
  let rec loop acc remaining = function
    | _ when remaining = 0 -> List.rev acc
    | [] -> List.rev acc
    | value :: values -> loop (value :: acc) (remaining - 1) values
  in
  loop [] count values

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
          let cwd = Mentat_workspace.Path.root_of source_path in
          match
            ( Address.provider workspace_io source_path,
              Mentat_workspace_io.to_abs workspace_io source_path )
          with
          | Error diagnostic, _ -> Mentat_tool.Result.failed `Failed diagnostic
          | _, Error error ->
              Mentat_tool.Result.failed `Failed
                (Mentat_workspace.Resolve_error.message error)
          | Ok address, Ok absolute -> (
              let filename = Lpath.Abs.to_string absolute in
              let position = input.Input.position in
              let verbosity = input.Input.verbosity in
              match
                run_type_enclosing workspace_io ~clock ~program ~cwd ~filename
                  ~position ~verbosity ~source ~cancelled ~index:0
              with
              | Error Merlin.Cancelled -> interrupted ()
              | Error error -> Merlin_support.merlin_failure error
              | Ok raw_frames -> (
                  let selected =
                    raw_frames |> dedup_adjacent
                    |> take input.Input.max_enclosings
                  in
                  if List.is_empty selected then
                    Mentat_tool.Result.failed `Not_found
                      (Printf.sprintf "no type at position %d:%d"
                         (Syntax.Position.line position)
                         (Syntax.Position.column position))
                  else
                    match
                      build_frames workspace_io ~clock ~program ~cwd ~filename
                        ~position ~verbosity ~source ~cancelled raw_frames
                        selected
                    with
                    | Error Merlin.Cancelled -> interrupted ()
                    | Error error -> Merlin_support.merlin_failure error
                    | Ok frames -> (
                        let documentation =
                          if not input.Input.documentation then
                            `Documentation Not_requested
                          else if cancelled () then `Cancelled
                          else
                            fetch_documentation workspace_io ~clock ~program
                              ~cwd ~filename ~position ~source ~cancelled
                        in
                        match documentation with
                        | `Cancelled -> interrupted ()
                        | `Documentation documentation ->
                            Mentat_tool.Result.completed
                              ~output:{ address; input; frames; documentation }
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
    invalid_arg "Ocaml.Type_at.make: program prefix must not be empty";
  let execution = Confinement.confined workspace_io in
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.ocaml_type_at
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io ~execution)
    ~run:(fun ~cancelled input ->
      run workspace_io ~clock ~program ~cancelled input)
    ()
