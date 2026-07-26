(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Syntax = Mentat_ocaml
module Content_ref = Mentat_digest.Content_ref

let name = "ocaml_rename"
let default_max_occurrences = 200
let max_occurrences = 1_000
let max_file_bytes = 1024 * 1024
let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_bool value = Jsont.Json.bool value

(* Identifier validation deliberately matches the textual rename contract. A
   syntax-aware occurrence query does not make replacing an operator, keyword,
   or differently-cased namespace sound. *)
let is_identifier_character = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let is_identifier_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

type name_class = Lowercase | Uppercase | Other

let classify_name value =
  if String.is_empty value then Other
  else
    match value.[0] with
    | 'a' .. 'z' | '_' -> Lowercase
    | 'A' .. 'Z' -> Uppercase
    | _ -> Other

let name_class_description = function
  | Lowercase -> "value"
  | Uppercase -> "constructor or module"
  | Other -> "operator"

let is_identifier value =
  (not (String.is_empty value))
  && is_identifier_start value.[0]
  && String.for_all is_identifier_character value

let keywords =
  [
    "and";
    "as";
    "asr";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "effect";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "perform";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let is_keyword value = List.mem value keywords

let validate_names ~old_name ~new_name =
  let old_class = classify_name old_name in
  let new_class = classify_name new_name in
  if old_class = Other then
    Error
      (Printf.sprintf
         "the entity under the cursor (%S) is an operator or unsupported \
          identifier; rename supports value and constructor/module names only"
         old_name)
  else if not (is_identifier new_name) then
    Error
      (Printf.sprintf "new name %S is not a valid OCaml identifier" new_name)
  else if is_keyword new_name then
    Error (Printf.sprintf "new name %S is an OCaml keyword" new_name)
  else if new_class <> old_class then
    Error
      (Printf.sprintf
         "new name %S is a %s identifier but the entity is a %s identifier"
         new_name
         (name_class_description new_class)
         (name_class_description old_class))
  else if String.equal new_name old_name then
    Error
      (Printf.sprintf "new name %S is the same as the current name" new_name)
  else Ok ()

module Input = struct
  type t = {
    path : string;
    position : Syntax.Position.t;
    new_name : string;
    dry_run : bool;
    max_occurrences : int;
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

  let make path line column new_name requested_dry_run requested_max_occurrences
      =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    if String.is_empty path then invalid_arg "path must not be empty";
    if String.contains path '\x00' then invalid_arg "path must not contain NUL";
    if String.is_empty new_name then invalid_arg "new_name must not be empty";
    if String.contains new_name '\x00' then
      invalid_arg "new_name must not contain NUL";
    if line < 1 then invalid_arg "line must be at least 1";
    if column < 0 then invalid_arg "column must be non-negative";
    let occurrence_cap =
      Option.value requested_max_occurrences ~default:default_max_occurrences
    in
    if occurrence_cap < 1 then invalid_arg "max_occurrences must be positive";
    if occurrence_cap > max_occurrences then
      invalid_arg "max_occurrences must be at most 1000";
    {
      path;
      position = Syntax.Position.make ~line ~column;
      new_name;
      dry_run = Option.value requested_dry_run ~default:false;
      max_occurrences = occurrence_cap;
    }

  let object_codec =
    Jsont.Object.map ~kind:"ocaml_rename input" make
    |> Jsont.Object.mem "path" Jsont.string ~enc:(fun input -> input.path)
    |> Jsont.Object.mem "line" exact_integer ~enc:(fun input ->
        Syntax.Position.line input.position)
    |> Jsont.Object.mem "column" exact_integer ~enc:(fun input ->
        Syntax.Position.column input.position)
    |> Jsont.Object.mem "new_name" Jsont.string ~enc:(fun input ->
        input.new_name)
    |> Jsont.Object.opt_mem "dry_run" Jsont.bool ~enc:(fun input ->
        if input.dry_run then Some true else None)
    |> Jsont.Object.opt_mem "max_occurrences" exact_integer ~enc:(fun input ->
        if input.max_occurrences = default_max_occurrences then None
        else Some input.max_occurrences)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict ocaml_rename input" object_codec

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
              ( "new_name",
                Codec.obj
                  [
                    ("type", json_string "string");
                    ( "description",
                      json_string
                        "Replacement identifier. Its lowercase-value or \
                         uppercase-constructor/module class must match the \
                         entity under the cursor." );
                    ("minLength", json_int 1);
                  ] );
              ( "dry_run",
                Codec.obj
                  [
                    ("type", json_string "boolean");
                    ( "description",
                      json_string
                        "Validate and report the complete rename without \
                         writing. Defaults to false." );
                  ] );
              ( "max_occurrences",
                schema_integer
                  "Safety cap on deduplicated occurrences. Exceeding it \
                   refuses the rename. Defaults to 200."
                  ~minimum:1 ~maximum:(json_int max_occurrences) );
            ] );
        ( "required",
          Jsont.Json.list
            [
              json_string "path";
              json_string "line";
              json_string "column";
              json_string "new_name";
            ] );
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

type tool_error =
  | Domain of { kind : Mentat_tool.Result.failure; message : string }
  | Cancelled

let error kind message = Error (Domain { kind; message })
let invalid message = error `Invalid_input message
let stale message = error `Stale message
let failed message = error `Failed message

let result_of_error = function
  | Domain { kind; message } -> Mentat_tool.Result.failed kind message
  | Cancelled -> Mentat_tool.Result.cancelled ()

let interrupted () = Mentat_tool.Result.cancelled ()

(* Source coordinates are bytes. Converting through line starts keeps Merlin,
   compiler locations, and replacement slices in one coordinate system. *)
let line_starts text =
  let starts = ref [ 0 ] in
  String.iteri
    (fun index character ->
      if Char.equal character '\n' then starts := (index + 1) :: !starts)
    text;
  Array.of_list (List.rev !starts)

let offset_of starts text position =
  let line = Syntax.Position.line position in
  let column = Syntax.Position.column position in
  if line > Array.length starts then None
  else
    let start = starts.(line - 1) in
    let limit =
      if line = Array.length starts then String.length text
      else starts.(line) - 1
    in
    let offset = start + column in
    if offset > limit then None else Some offset

let identifier_at ~contents ~position =
  let starts = line_starts contents in
  match offset_of starts contents position with
  | None -> None
  | Some offset ->
      let length = String.length contents in
      let anchor =
        if offset < length && is_identifier_character contents.[offset] then
          Some offset
        else if offset > 0 && is_identifier_character contents.[offset - 1] then
          Some (offset - 1)
        else None
      in
      Option.map
        (fun anchor ->
          let start = ref anchor in
          while !start > 0 && is_identifier_character contents.[!start - 1] do
            decr start
          done;
          let stop = ref anchor in
          while !stop < length && is_identifier_character contents.[!stop] do
            incr stop
          done;
          String.sub contents !start (!stop - !start))
        anchor

type parsed =
  | Implementation of Parsetree.structure
  | Interface of Parsetree.signature

let lexbuf ~filename source =
  let lexbuf = Lexing.from_string source in
  lexbuf.Lexing.lex_curr_p <-
    {
      Lexing.pos_fname = filename;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0;
    };
  lexbuf

let parse ~filename ~interface source =
  try
    if interface then Ok (Interface (Parse.interface (lexbuf ~filename source)))
    else Ok (Implementation (Parse.implementation (lexbuf ~filename source)))
  with exn -> Error (Printexc.to_string exn)

let syntax_position (position : Lexing.position) =
  Syntax.Position.make ~line:position.Lexing.pos_lnum
    ~column:(position.Lexing.pos_cnum - position.Lexing.pos_bol)

let location_range ~allow_ghost (location : Location.t) =
  if
    ((not allow_ghost) && location.Location.loc_ghost)
    || location.Location.loc_start.Lexing.pos_cnum
       > location.Location.loc_end.Lexing.pos_cnum
  then None
  else
    try
      Some
        (Syntax.Range.make
           ~start:(syntax_position location.Location.loc_start)
           ~end_:(syntax_position location.Location.loc_end))
    with Invalid_argument _ -> None

let longident_last_range (identifier : Longident.t Location.loc) =
  match identifier.Location.txt with
  | Longident.Lident _ ->
      location_range ~allow_ghost:true identifier.Location.loc
  | Longident.Ldot (_, last) ->
      location_range ~allow_ghost:true last.Location.loc
  | Longident.Lapply _ -> None

(* A single Merlin range is ambiguous at syntactic puns: changing the bytes can
   rename both a label and a value. Refusing those sites is safer than guessing
   which namespace Merlin intended. *)
let refused_ranges parsed =
  let ranges = ref [] in
  let add = function Some range -> ranges := range :: !ranges | None -> () in
  let record_pun identifier value_location =
    if identifier.Location.loc.Location.loc_ghost then begin
      add (location_range ~allow_ghost:false value_location);
      add (longident_last_range identifier)
    end
  in
  let parameter_pun (parameter : Parsetree.function_param) =
    match parameter.Parsetree.pparam_desc with
    | Parsetree.Pparam_val
        ((Asttypes.Labelled label | Asttypes.Optional label), _, pattern) -> (
        match pattern.Parsetree.ppat_desc with
        | Parsetree.Ppat_var variable
          when String.equal variable.Location.txt label ->
            add (location_range ~allow_ghost:false variable.Location.loc)
        | _ -> ())
    | Parsetree.Pparam_val (Asttypes.Nolabel, _, _) | Parsetree.Pparam_newtype _
      ->
        ()
  in
  let iterator =
    {
      Ast_iterator.default_iterator with
      Ast_iterator.expr =
        (fun self expression ->
          (match expression.Parsetree.pexp_desc with
          | Parsetree.Pexp_record (fields, _) ->
              List.iter
                (fun (identifier, value) ->
                  record_pun identifier value.Parsetree.pexp_loc)
                fields
          | Parsetree.Pexp_function (parameters, _, _) ->
              List.iter parameter_pun parameters
          | _ -> ());
          Ast_iterator.default_iterator.Ast_iterator.expr self expression);
      Ast_iterator.pat =
        (fun self pattern ->
          (match pattern.Parsetree.ppat_desc with
          | Parsetree.Ppat_record (fields, _) ->
              List.iter
                (fun (identifier, value) ->
                  record_pun identifier value.Parsetree.ppat_loc)
                fields
          | _ -> ());
          Ast_iterator.default_iterator.Ast_iterator.pat self pattern);
    }
  in
  (match parsed with
  | Implementation structure ->
      iterator.Ast_iterator.structure iterator structure
  | Interface signature -> iterator.Ast_iterator.signature iterator signature);
  !ranges

(* Merlin occurrence decoding is strict because a best-effort prefix would turn
   malformed or stale index data into an incomplete project-wide mutation. *)
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
  raw_file : string;
  raw_range : Syntax.Range.t;
  raw_stale : bool;
}

let parse_occurrence_file = function
  | Jsont.String (file, _) ->
      if String.contains file '\x00' then Error "file contains NUL" else Ok file
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.Array _
  | Jsont.Object _ ->
      Error "file is not a string"

let parse_occurrence_stale = function
  | Jsont.Bool (stale, _) -> Ok stale
  | Jsont.Null _ | Jsont.Number _ | Jsont.String _ | Jsont.Array _
  | Jsont.Object _ ->
      Error "stale is not a boolean"

let parse_occurrence = function
  | Jsont.Object (fields, _)
    when exact_member_names [ "file"; "start"; "end"; "stale" ] fields -> (
      match
        ( required_member "file" fields,
          required_member "start" fields,
          required_member "end" fields,
          required_member "stale" fields )
      with
      | Ok file, Ok start, Ok end_, Ok stale -> (
          match
            ( parse_occurrence_file file,
              parse_position start,
              parse_position end_,
              parse_occurrence_stale stale )
          with
          | Ok raw_file, Ok start, Ok end_, Ok raw_stale -> (
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
      | Error diagnostic, _, _, _
      | _, Error diagnostic, _, _
      | _, _, Error diagnostic, _
      | _, _, _, Error diagnostic ->
          Error diagnostic)
  | Jsont.Object _ -> Error "occurrence has unexpected or duplicate members"
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      Error "occurrence is not an object"

let parse_occurrences = function
  | Jsont.Array (items, _) ->
      let rec loop occurrences = function
        | [] -> Ok (List.rev occurrences)
        | item :: items -> (
            match parse_occurrence item with
            | Ok occurrence -> loop (occurrence :: occurrences) items
            | Error _ as error -> error)
      in
      loop [] items
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Object _ ->
      Error "occurrences value is not an array"

type occurrence = { path : Mentat_workspace.Path.t; range : Syntax.Range.t }

let compare_occurrence left right =
  match Mentat_workspace.Path.compare left.path right.path with
  | 0 -> Syntax.Range.compare left.range right.range
  | order -> order

let normalize_occurrence workspace_io ~source_path ~source_root ~cwd occurrence
    =
  let path =
    match occurrence.raw_file with
    | "" | "*buffer*" -> Ok source_path
    | file -> (
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
  Result.map (fun path -> { path; range = occurrence.raw_range }) path

let normalize_occurrences workspace_io ~source_path ~source_root ~cwd
    occurrences =
  let rec loop normalized = function
    | [] -> Ok (List.rev normalized)
    | occurrence :: occurrences -> (
        match
          normalize_occurrence workspace_io ~source_path ~source_root ~cwd
            occurrence
        with
        | Ok occurrence -> loop (occurrence :: normalized) occurrences
        | Error _ as error -> error)
  in
  loop [] occurrences

(* Equal path/range pairs name one semantic occurrence. Staleness is checked
   before this function, so duplicates cannot conceal a stale backend report. *)
let sort_and_deduplicate occurrences =
  let sorted = List.sort compare_occurrence occurrences in
  let rec loop unique = function
    | [] -> List.rev unique
    | occurrence :: occurrences -> (
        match unique with
        | previous :: _ when compare_occurrence previous occurrence = 0 ->
            loop unique occurrences
        | [] | _ :: _ -> loop (occurrence :: unique) occurrences)
  in
  loop [] sorted

let group_by_path occurrences =
  let rec loop groups current_path ranges = function
    | [] -> (
        match current_path with
        | None -> List.rev groups
        | Some path -> List.rev ((path, List.rev ranges) :: groups))
    | occurrence :: occurrences -> (
        match current_path with
        | Some path when Mentat_workspace.Path.equal path occurrence.path ->
            loop groups current_path (occurrence.range :: ranges) occurrences
        | None ->
            loop groups (Some occurrence.path) [ occurrence.range ] occurrences
        | Some path ->
            loop
              ((path, List.rev ranges) :: groups)
              (Some occurrence.path) [ occurrence.range ] occurrences)
  in
  loop [] None [] occurrences

module Plan = struct
  type target = {
    path : Mentat_workspace.Path.t;
    address : string;
    before : Content_ref.t;
    after : Content_ref.t;
    spans : Syntax.Range.t list;
  }

  type t = { old_name : string; new_name : string; targets : target list }

  let invalid message = invalid_arg ("Ocaml.Rename.Plan: " ^ message)

  let strictly_sorted compare values =
    let rec loop = function
      | left :: (right :: _ as rest) -> compare left right < 0 && loop rest
      | [] | [ _ ] -> true
    in
    loop values

  let make_target path address before after spans =
    if String.is_empty address then invalid "target address must not be empty";
    if String.contains address '\x00' then
      invalid "target address must not contain NUL";
    if List.is_empty spans then invalid "target spans must not be empty";
    if not (strictly_sorted Syntax.Range.compare spans) then
      invalid "target spans must be strictly ordered";
    if Content_ref.equal before after then
      invalid "target before and after identities must differ";
    if Content_ref.length before > max_file_bytes then
      invalid "target before identity exceeds the file bound";
    if Content_ref.length after > max_file_bytes then
      invalid "target after identity exceeds the file bound";
    { path; address; before; after; spans }

  let make old_name new_name targets =
    (match validate_names ~old_name ~new_name with
    | Ok () -> ()
    | Error message -> invalid message);
    if List.is_empty targets then invalid "targets must not be empty";
    if
      not
        (strictly_sorted
           (fun (left : target) (right : target) ->
             Mentat_workspace.Path.compare left.path right.path)
           targets)
    then invalid "targets must be strictly ordered by path";
    let occurrences =
      List.fold_left
        (fun count (target : target) -> count + List.length target.spans)
        0 targets
    in
    if occurrences > max_occurrences then
      invalid "total occurrences exceed the rename limit";
    { old_name; new_name; targets }

  let position_jsont =
    let make line column =
      Mentat_tool.Codec.decode_invalid_arg (fun () ->
          Syntax.Position.make ~line ~column)
    in
    Jsont.Object.map ~kind:"OCaml rename position" make
    |> Jsont.Object.mem "line" Input.exact_integer ~enc:Syntax.Position.line
    |> Jsont.Object.mem "column" Input.exact_integer ~enc:Syntax.Position.column
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let range_jsont =
    let make start end_ =
      Mentat_tool.Codec.decode_invalid_arg (fun () ->
          Syntax.Range.make ~start ~end_)
    in
    Jsont.Object.map ~kind:"OCaml rename range" make
    |> Jsont.Object.mem "start" position_jsont ~enc:Syntax.Range.start
    |> Jsont.Object.mem "end" position_jsont ~enc:Syntax.Range.end_
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let target_jsont =
    let make path address before after spans =
      Mentat_tool.Codec.decode_invalid_arg (fun () ->
          make_target path address before after spans)
    in
    Jsont.Object.map ~kind:"OCaml rename target" make
    |> Jsont.Object.mem "path" Mentat_workspace.Path.jsont
         ~enc:(fun (target : target) -> target.path)
    |> Jsont.Object.mem "address" Jsont.string ~enc:(fun (target : target) ->
        target.address)
    |> Jsont.Object.mem "before" Content_ref.jsont
         ~enc:(fun (target : target) -> target.before)
    |> Jsont.Object.mem "after" Content_ref.jsont ~enc:(fun (target : target) ->
        target.after)
    |> Jsont.Object.mem "spans" (Jsont.list range_jsont)
         ~enc:(fun (target : target) -> target.spans)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let jsont =
    let decode version old_name new_name targets =
      Mentat_tool.Codec.decode_invalid_arg (fun () ->
          if version <> 1 then invalid "unsupported version";
          make old_name new_name targets)
    in
    Jsont.Object.map ~kind:"OCaml rename plan" decode
    |> Jsont.Object.mem "version" Input.exact_integer ~enc:(fun _ -> 1)
    |> Jsont.Object.mem "old_name" Jsont.string ~enc:(fun plan -> plan.old_name)
    |> Jsont.Object.mem "new_name" Jsont.string ~enc:(fun plan -> plan.new_name)
    |> Jsont.Object.mem "targets" (Jsont.list target_jsont) ~enc:(fun plan ->
        plan.targets)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let occurrences plan =
    List.fold_left
      (fun count (target : target) -> count + List.length target.spans)
      0 plan.targets

  let files plan = List.length plan.targets

  let describe plan =
    Printf.sprintf "Rename %s to %s at %d occurrence(s) in %d file(s)"
      plan.old_name plan.new_name (occurrences plan) (files plan)
end

type replacement = { start : int; stop : int }

let classify_occurrence ~contents ~starts ~old_name ~refused ~address range =
  let start_position = Syntax.Range.start range in
  let start_line = Syntax.Position.line start_position in
  let start_column = Syntax.Position.column start_position in
  match
    ( offset_of starts contents start_position,
      offset_of starts contents (Syntax.Range.end_ range) )
  with
  | Some start, Some stop when stop >= start && stop <= String.length contents
    ->
      let found = String.sub contents start (stop - start) in
      if not (String.equal found old_name) then
        stale
          (Printf.sprintf
             "%s:%d:%d no longer holds %S (found %S); rebuild the project \
              index with `dune build @ocaml-index` and retry"
             address start_line start_column old_name found)
      else
        let before_is_boundary =
          start = 0 || not (is_identifier_character contents.[start - 1])
        in
        let after_is_boundary =
          stop = String.length contents
          || not (is_identifier_character contents.[stop])
        in
        if not (before_is_boundary && after_is_boundary) then
          invalid
            (Printf.sprintf
               "%s:%d:%d is not a standalone identifier; the local parse \
                cannot corroborate the rename here, edit it manually"
               address start_line start_column)
        else if
          start > 0
          && (Char.equal contents.[start - 1] '~'
             || Char.equal contents.[start - 1] '?')
        then
          invalid
            (Printf.sprintf
               "%s:%d:%d is a labelled-argument occurrence (~/?); this tool \
                does not rewrite label or pun sites, edit it manually"
               address start_line start_column)
        else if List.exists (Syntax.Range.equal range) refused then
          invalid
            (Printf.sprintf
               "%s:%d:%d is a record-field or labelled-argument pun; this tool \
                does not rewrite label or pun sites, edit it manually"
               address start_line start_column)
        else Ok { start; stop }
  | _ ->
      invalid
        (Printf.sprintf
           "%s:%d:%d is outside the current source; the local parse cannot \
            corroborate the rename here"
           address start_line start_column)

let replacements_overlap replacements =
  let sorted =
    List.sort
      (fun left right -> Int.compare left.start right.start)
      replacements
  in
  let rec loop = function
    | left :: (right :: _ as rest) -> left.stop > right.start || loop rest
    | [] | [ _ ] -> false
  in
  loop sorted

let apply_replacements source new_name replacements =
  let descending =
    List.sort
      (fun left right -> Int.compare right.start left.start)
      replacements
  in
  List.fold_left
    (fun source replacement ->
      String.sub source 0 replacement.start
      ^ new_name
      ^ String.sub source replacement.stop
          (String.length source - replacement.stop))
    source descending

let source_kind address =
  if Filename.check_suffix address ".mli" then Ok true
  else if Filename.check_suffix address ".ml" then Ok false
  else invalid (address ^ ": not an OCaml source file, cannot rename here")

let plan_contents ~cancelled ~address ~old_name ~new_name ~spans source =
  match source_kind address with
  | Error _ as error -> error
  | Ok interface -> (
      match parse ~filename:address ~interface source with
      | Error diagnostic ->
          failed
            (address ^ ": could not parse source: "
            ^ Text_helpers.bounded_diagnostic ~max_bytes:Merlin.max_detail_bytes
                diagnostic)
      | Ok parsed -> (
          let refused = refused_ranges parsed in
          let starts = line_starts source in
          let rec collect replacements = function
            | [] -> Ok (List.rev replacements)
            | _ when cancelled () -> Error Cancelled
            | span :: spans -> (
                match
                  classify_occurrence ~contents:source ~starts ~old_name
                    ~refused ~address span
                with
                | Ok replacement -> collect (replacement :: replacements) spans
                | Error _ as error -> error)
          in
          match collect [] spans with
          | Error _ as error -> error
          | Ok replacements -> (
              if replacements_overlap replacements then
                invalid
                  (address
                 ^ ": occurrences overlap; the index may be stale or \
                    ppx-generated, edit it manually")
              else
                let after = apply_replacements source new_name replacements in
                if String.length after > max_file_bytes then
                  invalid
                    (Printf.sprintf
                       "%s: renamed source is too large (%d bytes, max %d)"
                       address (String.length after) max_file_bytes)
                else
                  match parse ~filename:address ~interface after with
                  | Error diagnostic ->
                      failed
                        (address ^ ": rename produced unparseable source: "
                        ^ Text_helpers.bounded_diagnostic
                            ~max_bytes:Merlin.max_detail_bytes diagnostic)
                  | Ok _ -> Ok after)))

let edit_observation_error path expected actual =
  let error = Mentat_edit.Error.state_mismatch ~path ~expected ~actual in
  Domain { kind = Edit_error.failure error; message = Edit_error.message error }

let observe_text workspace_io path =
  match Mentat_workspace_io.Edit.observe workspace_io path with
  | Ok (Mentat_edit.Observed.Text source) -> Ok source
  | Ok Mentat_edit.Observed.Missing ->
      Error (edit_observation_error path `Text `Missing)
  | Ok Mentat_edit.Observed.Other ->
      Error (edit_observation_error path `Text `Other)
  | Error error ->
      Error
        (Domain
           {
             kind = Edit_error.failure error;
             message = Edit_error.message error;
           })

let prepare_target workspace_io ~cancelled ~old_name ~new_name (path, spans) =
  match Address.provider workspace_io path with
  | Error diagnostic -> failed diagnostic
  | Ok address -> (
      match observe_text workspace_io path with
      | Error _ as error -> error
      | Ok before -> (
          match
            plan_contents ~cancelled ~address ~old_name ~new_name ~spans before
          with
          | Error _ as error -> error
          | Ok after -> (
              try
                Ok
                  (Plan.make_target path address
                     (Content_ref.of_contents before)
                     (Content_ref.of_contents after)
                     spans)
              with Invalid_argument diagnostic -> failed diagnostic)))

let prepare_targets workspace_io ~cancelled ~old_name ~new_name groups =
  let rec loop targets = function
    | [] -> Ok (List.rev targets)
    | _ when cancelled () -> Error Cancelled
    | group :: groups -> (
        match
          prepare_target workspace_io ~cancelled ~old_name ~new_name group
        with
        | Ok target -> loop (target :: targets) groups
        | Error _ as error -> error)
  in
  loop [] groups

module Output = struct
  type t = { applied : bool; plan : Plan.t }

  let text output =
    let plan = output.plan in
    let buffer = Buffer.create 384 in
    Printf.bprintf buffer "OCaml rename: %s -> %s\n" plan.Plan.old_name
      plan.Plan.new_name;
    Printf.bprintf buffer "applied: %b\n" output.applied;
    Printf.bprintf buffer "occurrences: %d in %d file(s)\n"
      (Plan.occurrences plan) (Plan.files plan);
    Buffer.add_string buffer "index_status: unknown\nbackend: ocamlmerlin";
    List.iter
      (fun target ->
        Printf.bprintf buffer "\n- %s: %d occurrence(s)" target.Plan.address
          (List.length target.Plan.spans))
      plan.Plan.targets;
    Buffer.contents buffer

  let encode output =
    let disposition =
      if output.applied then Mentat_tools_output.Ocaml.Rename.Applied
      else Mentat_tools_output.Ocaml.Rename.Previewed
    in
    let semantic =
      Mentat_tools_output.Ocaml.Rename.make ~disposition
        ~occurrences:(Plan.occurrences output.plan)
        ~files:(Plan.files output.plan)
    in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Ocaml.Rename.jsont
      ~text:(text output) semantic
end

let completed ~applied plan =
  Mentat_tool.Result.completed ~output:{ Output.applied; plan } ()

let file_error error =
  Domain { kind = Fs_error.failure error; message = Fs_error.message error }

let load_source workspace_io input =
  match Mentat_workspace_io.resolve_path workspace_io input.Input.path with
  | Error error -> invalid (Mentat_workspace.Resolve_error.message error)
  | Ok path -> (
      match Mentat_workspace_io.File.stat workspace_io path with
      | Error error -> Error (file_error error)
      | Ok stat -> (
          match stat.Eio.File.Stat.kind with
          | `Regular_file -> (
              match
                Mentat_workspace_io.File.load workspace_io path
                  ~max_bytes:max_file_bytes
              with
              | Ok source -> Ok (path, source)
              | Error error -> Error (file_error error))
          | kind ->
              invalid
                (Mentat_workspace.Path.display path
                ^ ": expected a regular file, found " ^ Stat_kind.kind_name kind
                )))

let occurrences_arguments ~filename position =
  [
    "-identifier-at";
    Format.asprintf "%a" Syntax.Position.pp position;
    "-scope";
    "renaming";
    "-filename";
    filename;
  ]

let prepare_occurrences workspace_io ~clock ~program ~cancelled input
    ~source_path ~source_root ~source =
  match
    ( Mentat_workspace_io.to_abs workspace_io source_path,
      Mentat_workspace_io.to_abs workspace_io source_root )
  with
  | Error error, _ | _, Error error ->
      `Finished
        (Mentat_tool.Result.failed `Failed
           (Mentat_workspace.Resolve_error.message error))
  | Ok filename, Ok cwd -> (
      let filename = Lpath.Abs.to_string filename in
      let arguments = occurrences_arguments ~filename input.Input.position in
      match
        Merlin.run workspace_io ~clock ~program ~cwd:source_root
          ~command:"occurrences" ~args:arguments ~source ~cancelled
      with
      | Error Merlin.Cancelled -> `Finished (interrupted ())
      | Error error -> `Finished (Merlin_support.merlin_failure error)
      | Ok value -> (
          if cancelled () then `Finished (interrupted ())
          else
            match parse_occurrences value with
            | Error diagnostic ->
                `Finished
                  (Mentat_tool.Result.failed `Failed
                     ("could not decode ocamlmerlin occurrences result: "
                     ^ Text_helpers.bounded_diagnostic
                         ~max_bytes:Merlin.max_detail_bytes diagnostic))
            | Ok raw_occurrences -> (
                let stale_count =
                  List.fold_left
                    (fun count occurrence ->
                      if occurrence.raw_stale then count + 1 else count)
                    0 raw_occurrences
                in
                if stale_count > 0 then
                  `Finished
                    (Mentat_tool.Result.failed `Stale
                       (Printf.sprintf
                          "index appears stale: %d occurrence(s) reported \
                           stale; rebuild with `dune build @ocaml-index` and \
                           retry"
                          stale_count))
                else
                  match
                    normalize_occurrences workspace_io ~source_path ~source_root
                      ~cwd raw_occurrences
                  with
                  | Error diagnostic ->
                      `Finished
                        (Mentat_tool.Result.failed `Failed
                           ("could not normalize ocamlmerlin occurrence: "
                           ^ Text_helpers.bounded_diagnostic
                               ~max_bytes:Merlin.max_detail_bytes diagnostic))
                  | Ok occurrences ->
                      let occurrences = sort_and_deduplicate occurrences in
                      let occurrence_count = List.length occurrences in
                      if occurrence_count > input.Input.max_occurrences then
                        `Finished
                          (Mentat_tool.Result.failed `Failed
                             (Printf.sprintf
                                "%d occurrences exceed the rename cap %d; the \
                                 rename is too large to apply safely as one \
                                 edit"
                                occurrence_count input.Input.max_occurrences))
                      else if List.is_empty occurrences then
                        `Finished
                          (Mentat_tool.Result.failed `Invalid_input
                             (Printf.sprintf
                                "no renameable binding at %s:%d:%d; the cursor \
                                 may not be on an identifier, or the project \
                                 index is missing (`dune build @ocaml-index`)"
                                input.Input.path
                                (Syntax.Position.line input.Input.position)
                                (Syntax.Position.column input.Input.position)))
                      else `Occurrences (group_by_path occurrences))))

let prepare workspace_io ~clock ~program ~cancelled input =
  if cancelled () then `Finished (interrupted ())
  else
    match load_source workspace_io input with
    | Error error -> `Finished (result_of_error error)
    | Ok (source_path, source) -> (
        if cancelled () then `Finished (interrupted ())
        else
          match
            identifier_at ~contents:source ~position:input.Input.position
          with
          | None ->
              `Finished
                (Mentat_tool.Result.failed `Invalid_input
                   (Printf.sprintf
                      "no identifier at %s:%d:%d; place the cursor on the \
                       binding to rename"
                      (Mentat_workspace.Path.display source_path)
                      (Syntax.Position.line input.Input.position)
                      (Syntax.Position.column input.Input.position)))
          | Some old_name -> (
              match validate_names ~old_name ~new_name:input.Input.new_name with
              | Error message ->
                  `Finished (Mentat_tool.Result.failed `Invalid_input message)
              | Ok () -> (
                  let source_root = Mentat_workspace.Path.root_of source_path in
                  match
                    prepare_occurrences workspace_io ~clock ~program ~cancelled
                      input ~source_path ~source_root ~source
                  with
                  | `Finished result -> `Finished result
                  | `Occurrences groups -> (
                      match
                        prepare_targets workspace_io ~cancelled ~old_name
                          ~new_name:input.Input.new_name groups
                      with
                      | Error error -> `Finished (result_of_error error)
                      | Ok targets ->
                          let plan =
                            Plan.make old_name input.Input.new_name targets
                          in
                          if cancelled () then `Finished (interrupted ())
                          else if input.Input.dry_run then
                            `Finished (completed ~applied:false plan)
                          else `Prepared plan))))

let rebuild_target workspace_io ~cancelled plan (target : Plan.target) =
  match observe_text workspace_io target.Plan.path with
  | Error _ as error -> error
  | Ok before -> (
      if not (Content_ref.matches target.Plan.before before) then
        stale
          (target.Plan.address
         ^ ": source changed after rename preparation; prepare the rename again"
          )
      else
        match
          plan_contents ~cancelled ~address:target.Plan.address
            ~old_name:plan.Plan.old_name ~new_name:plan.Plan.new_name
            ~spans:target.Plan.spans before
        with
        | Error _ as error -> error
        | Ok after -> (
            if not (Content_ref.matches target.Plan.after after) then
              failed
                (target.Plan.address
               ^ ": prepared rename no longer reconstructs its validated after \
                  identity")
            else
              match
                Mentat_edit.rewrite ~path:target.Plan.path ~before ~after
              with
              | Ok edit -> Ok edit
              | Error error ->
                  Error
                    (Domain
                       {
                         kind = Edit_error.failure error;
                         message = Edit_error.message error;
                       })))

let rebuild_edit workspace_io ~cancelled plan =
  let rec loop edits = function
    | [] -> (
        match Mentat_edit.concat (List.rev edits) with
        | Ok edit -> Ok edit
        | Error error ->
            Error
              (Domain
                 {
                   kind = Edit_error.failure error;
                   message = Edit_error.message error;
                 }))
    | _ when cancelled () -> Error Cancelled
    | target :: targets -> (
        match rebuild_target workspace_io ~cancelled plan target with
        | Ok edit -> loop (edit :: edits) targets
        | Error _ as error -> error)
  in
  loop [] plan.Plan.targets

let run workspace_io ~cancelled plan =
  if cancelled () then interrupted ()
  else
    match rebuild_edit workspace_io ~cancelled plan with
    | Error error -> result_of_error error
    | Ok edit when Mentat_edit.is_empty edit ->
        Mentat_tool.Result.failed `Failed
          "prepared rename unexpectedly reconstructed an empty edit"
    | Ok edit -> (
        if cancelled () then interrupted ()
        else
          match Mentat_workspace_io.Edit.apply workspace_io edit with
          | Error error -> Edit_error.failed_apply error
          | Ok authoritative_result ->
              (* Claim_scope is the sole mutation-evidence owner. *)
              ignore authoritative_result;
              completed ~applied:true plan)

let prepare_permissions workspace_io ~execution input =
  match Mentat_workspace_io.resolve_path workspace_io input.Input.path with
  | Error _ -> []
  | Ok source ->
      let root = Mentat_workspace.Path.root_of source in
      [
        Mentat_permission.Request.of_accesses ~source:name
          [
            Mentat_permission.Access.path ~op:`Read root;
            Confinement.custom_access execution;
          ];
      ]

let permissions plan =
  let items =
    List.map
      (fun (target : Plan.target) ->
        Mentat_permission.Request.Item.make ~display:target.Plan.address
          (Mentat_permission.Access.path ~op:`Modify target.Plan.path))
      plan.Plan.targets
  in
  [
    Mentat_permission.Request.make ~source:name ~display:(Plan.describe plan)
      items;
  ]

let make workspace_io ~clock ~program =
  if List.is_empty program then
    invalid_arg "Ocaml.Rename.make: program prefix must not be empty";
  let execution = Confinement.confined workspace_io in
  Mentat_tool.make_staged ~name ~description:Mentat_prompts.Tools.ocaml_rename
    ~input:Input.contract ~prepared:Plan.jsont ~describe:Plan.describe
    ~output:Output.encode
    ~prepare_permissions:(prepare_permissions workspace_io ~execution)
    ~prepare:(fun ~cancelled input ->
      prepare workspace_io ~clock ~program ~cancelled input)
    ~permissions
    ~run:(fun ~cancelled plan -> run workspace_io ~cancelled plan)
    ()
