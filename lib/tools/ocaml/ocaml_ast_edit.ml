(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let name = "ocaml_ast_edit"
let max_file_bytes = 1024 * 1024

type file_kind = Implementation | Interface

let file_kind_string = function
  | Implementation -> "implementation"
  | Interface -> "interface"

let file_kind_of_string = function
  | "implementation" | "ml" -> Ok Implementation
  | "interface" | "mli" -> Ok Interface
  | value ->
      Error
        ("file_kind must be implementation, interface, ml, or mli; got " ^ value)

let infer_file_kind path =
  if Filename.check_suffix path ".mli" then Ok Interface
  else if Filename.check_suffix path ".ml" then Ok Implementation
  else Error "file_kind is required for paths that do not end in .ml or .mli"

module Item_kind = struct
  type t =
    | Value
    | Type
    | Module
    | Module_type
    | Exception
    | External
    | Open
    | Include
    | Class
    | Class_type
    | Extension
    | Eval

  let equal (left : t) right = left = right

  let to_string = function
    | Value -> "value"
    | Type -> "type"
    | Module -> "module"
    | Module_type -> "module_type"
    | Exception -> "exception"
    | External -> "external"
    | Open -> "open"
    | Include -> "include"
    | Class -> "class"
    | Class_type -> "class_type"
    | Extension -> "extension"
    | Eval -> "eval"

  let pp formatter kind = Format.pp_print_string formatter (to_string kind)
end

let item_kind_of_string = function
  | "value" -> Ok Item_kind.Value
  | "type" -> Ok Item_kind.Type
  | "module" -> Ok Item_kind.Module
  | "module_type" -> Ok Item_kind.Module_type
  | "exception" -> Ok Item_kind.Exception
  | "external" -> Ok Item_kind.External
  | "open" -> Ok Item_kind.Open
  | "include" -> Ok Item_kind.Include
  | "class" -> Ok Item_kind.Class
  | "class_type" -> Ok Item_kind.Class_type
  | "extension" -> Ok Item_kind.Extension
  | "eval" -> Ok Item_kind.Eval
  | value -> Error ("unknown item_kind: " ^ value)

module Node_kind = struct
  type t = Item of Item_kind.t option | Expression | Type

  let matches requested actual =
    match (requested, actual) with
    | Item None, Item _ -> true
    | Item (Some requested), Item (Some actual) ->
        Item_kind.equal requested actual
    | Expression, Expression | Type, Type -> true
    | (Item _ | Expression | Type), _ -> false

  let pp formatter = function
    | Item None -> Format.pp_print_string formatter "item"
    | Item (Some kind) -> Format.fprintf formatter "%a item" Item_kind.pp kind
    | Expression -> Format.pp_print_string formatter "expression"
    | Type -> Format.pp_print_string formatter "type"
end

let node_kind_of_fields kind item_kind =
  match (kind, item_kind) with
  | "expression", None -> Ok Node_kind.Expression
  | "type", None -> Ok Node_kind.Type
  | "item", None -> Ok (Node_kind.Item None)
  | "item", Some item_kind ->
      Result.map
        (fun item_kind -> Node_kind.Item (Some item_kind))
        (item_kind_of_string item_kind)
  | ("expression" | "type"), Some _ ->
      Error "item_kind is only valid when kind is item"
  | value, _ -> Error ("kind must be item, expression, or type; got " ^ value)

module Selector = struct
  type t =
    | Item of {
        path : string list;
        kind : Item_kind.t option;
        occurrence : int;
      }
    | Enclosing of { kind : Node_kind.t; position : Mentat_ocaml.Position.t }
    | Exact of { kind : Node_kind.t; range : Mentat_ocaml.Range.t }

  let item ?kind ?(occurrence = 1) path =
    if List.is_empty path then invalid_arg "selector.path must not be empty";
    List.iter
      (fun component ->
        if String.is_empty component then
          invalid_arg "selector.path components must not be empty")
      path;
    if occurrence < 1 then invalid_arg "selector.occurrence must be at least 1";
    Item { path; kind; occurrence }

  let enclosing ~kind ~position = Enclosing { kind; position }
  let exact ~kind ~range = Exact { kind; range }

  let pp_path formatter path =
    Format.pp_print_string formatter (String.concat "." path)

  let pp formatter = function
    | Item { path; kind = None; occurrence } ->
        Format.fprintf formatter "item %a occurrence %d" pp_path path occurrence
    | Item { path; kind = Some kind; occurrence } ->
        Format.fprintf formatter "%a %a occurrence %d" Item_kind.pp kind pp_path
          path occurrence
    | Enclosing { kind; position } ->
        Format.fprintf formatter "enclosing %a at %a" Node_kind.pp kind
          Mentat_ocaml.Position.pp position
    | Exact { kind; range } ->
        Format.fprintf formatter "exact %a at %a" Node_kind.pp kind
          Mentat_ocaml.Range.pp range
end

module Edit = struct
  type operation = Replace | Insert_before | Insert_after | Delete

  type t = {
    operation : operation;
    selector : Selector.t;
    text : string option;
  }

  let make ~operation ~selector ?text () =
    Option.iter
      (fun text ->
        if not (String.is_valid_utf_8 text) then
          invalid_arg "edit.text must be valid UTF-8")
      text;
    begin match (operation, text) with
    | Delete, _ -> ()
    | (Replace | Insert_before | Insert_after), Some text
      when not (String.is_empty text) ->
        ()
    | Replace, _ -> invalid_arg "replace requires non-empty edit.text"
    | (Insert_before | Insert_after), _ ->
        invalid_arg "insertion requires non-empty edit.text"
    end;
    let text = match operation with Delete -> None | _ -> text in
    { operation; selector; text }

  let operation edit = edit.operation
  let selector edit = edit.selector
  let text edit = edit.text
end

let operation_string = function
  | Edit.Replace -> "replace"
  | Edit.Insert_before -> "insert_before"
  | Edit.Insert_after -> "insert_after"
  | Edit.Delete -> "delete"

let operation_of_string = function
  | "replace" -> Ok Edit.Replace
  | "insert_before" -> Ok Edit.Insert_before
  | "insert_after" -> Ok Edit.Insert_after
  | "delete" -> Ok Edit.Delete
  | value ->
      Error
        ("op must be replace, insert_before, insert_after, or delete; got "
       ^ value)

module Input = struct
  type t = {
    path : string;
    file_kind : file_kind;
    edits : Edit.t list;
    if_identity : Mentat_digest.Content_ref.t option;
  }

  let path input = input.path
  let file_kind input = input.file_kind
  let edits input = input.edits
  let if_identity input = input.if_identity

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
      ~enc:(fun value -> Jsont.Json.int value)
      Jsont.json

  let position line column =
    if line < 1 then invalid_arg "line must be at least 1";
    if column < 0 then invalid_arg "column must be non-negative";
    Mentat_ocaml.Position.make ~line ~column

  let range start_line start_column end_line end_column =
    Mentat_ocaml.Range.make
      ~start:(position start_line start_column)
      ~end_:(position end_line end_column)

  let required name = function
    | Some value -> value
    | None -> invalid_arg (name ^ " is required")

  let selector mode path item_kind occurrence kind node_item_kind line column
      start_line start_column end_line end_column =
    match mode with
    | "item" ->
        let path = required "selector.path when mode is item" path in
        let kind =
          match item_kind with
          | None -> None
          | Some value -> (
              match item_kind_of_string value with
              | Ok kind -> Some kind
              | Error message -> invalid_arg message)
        in
        Selector.item ?kind ?occurrence path
    | "enclosing" ->
        let kind = required "selector.kind when mode is enclosing" kind in
        let kind =
          match node_kind_of_fields kind node_item_kind with
          | Ok kind -> kind
          | Error message -> invalid_arg message
        in
        let line = required "selector.line when mode is enclosing" line in
        let column = required "selector.column when mode is enclosing" column in
        Selector.enclosing ~kind ~position:(position line column)
    | "exact" ->
        let kind = required "selector.kind when mode is exact" kind in
        let kind =
          match node_kind_of_fields kind node_item_kind with
          | Ok kind -> kind
          | Error message -> invalid_arg message
        in
        let start_line =
          required "selector.start_line when mode is exact" start_line
        in
        let start_column =
          required "selector.start_column when mode is exact" start_column
        in
        let end_line =
          required "selector.end_line when mode is exact" end_line
        in
        let end_column =
          required "selector.end_column when mode is exact" end_column
        in
        Selector.exact ~kind
          ~range:(range start_line start_column end_line end_column)
    | value ->
        invalid_arg
          ("selector.mode must be item, enclosing, or exact; got " ^ value)

  let selector_object_codec =
    Jsont.Object.map ~kind:"ocaml_ast_edit selector"
      (fun
        mode
        path
        item_kind
        occurrence
        kind
        node_item_kind
        line
        column
        start_line
        start_column
        end_line
        end_column
      ->
        Mentat_tool.Codec.decode_invalid_arg (fun () ->
            selector mode path item_kind occurrence kind node_item_kind line
              column start_line start_column end_line end_column))
    |> Jsont.Object.mem "mode" Jsont.string ~enc:(function
      | Selector.Item _ -> "item"
      | Selector.Enclosing _ -> "enclosing"
      | Selector.Exact _ -> "exact")
    |> Jsont.Object.opt_mem "path" (Jsont.list Jsont.string) ~enc:(function
      | Selector.Item { path; _ } -> Some path
      | Selector.Enclosing _ | Selector.Exact _ -> None)
    |> Jsont.Object.opt_mem "item_kind" Jsont.string ~enc:(function
      | Selector.Item { kind = Some kind; _ } -> Some (Item_kind.to_string kind)
      | Selector.Item { kind = None; _ }
      | Selector.Enclosing _ | Selector.Exact _ ->
          None)
    |> Jsont.Object.opt_mem "occurrence" exact_integer ~enc:(function
      | Selector.Item { occurrence; _ } when occurrence <> 1 -> Some occurrence
      | Selector.Item _ | Selector.Enclosing _ | Selector.Exact _ -> None)
    |> Jsont.Object.opt_mem "kind" Jsont.string ~enc:(function
      | Selector.Enclosing { kind; _ } | Selector.Exact { kind; _ } ->
          Some
            (match kind with
            | Node_kind.Item _ -> "item"
            | Node_kind.Expression -> "expression"
            | Node_kind.Type -> "type")
      | Selector.Item _ -> None)
    |> Jsont.Object.opt_mem "node_item_kind" Jsont.string ~enc:(function
      | Selector.Enclosing { kind = Node_kind.Item (Some kind); _ }
      | Selector.Exact { kind = Node_kind.Item (Some kind); _ } ->
          Some (Item_kind.to_string kind)
      | Selector.Item _ | Selector.Enclosing _ | Selector.Exact _ -> None)
    |> Jsont.Object.opt_mem "line" exact_integer ~enc:(function
      | Selector.Enclosing { position; _ } ->
          Some (Mentat_ocaml.Position.line position)
      | Selector.Item _ | Selector.Exact _ -> None)
    |> Jsont.Object.opt_mem "column" exact_integer ~enc:(function
      | Selector.Enclosing { position; _ } ->
          Some (Mentat_ocaml.Position.column position)
      | Selector.Item _ | Selector.Exact _ -> None)
    |> Jsont.Object.opt_mem "start_line" exact_integer ~enc:(function
      | Selector.Exact { range; _ } ->
          Some (Mentat_ocaml.Position.line (Mentat_ocaml.Range.start range))
      | Selector.Item _ | Selector.Enclosing _ -> None)
    |> Jsont.Object.opt_mem "start_column" exact_integer ~enc:(function
      | Selector.Exact { range; _ } ->
          Some (Mentat_ocaml.Position.column (Mentat_ocaml.Range.start range))
      | Selector.Item _ | Selector.Enclosing _ -> None)
    |> Jsont.Object.opt_mem "end_line" exact_integer ~enc:(function
      | Selector.Exact { range; _ } ->
          Some (Mentat_ocaml.Position.line (Mentat_ocaml.Range.end_ range))
      | Selector.Item _ | Selector.Enclosing _ -> None)
    |> Jsont.Object.opt_mem "end_column" exact_integer ~enc:(function
      | Selector.Exact { range; _ } ->
          Some (Mentat_ocaml.Position.column (Mentat_ocaml.Range.end_ range))
      | Selector.Item _ | Selector.Enclosing _ -> None)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let selector_codec =
    Codec.strict_object ~kind:"strict ocaml_ast_edit selector"
      selector_object_codec

  let edit_object_codec =
    Jsont.Object.map ~kind:"ocaml_ast_edit edit" (fun operation selector text ->
        Mentat_tool.Codec.decode_invalid_arg (fun () ->
            let operation =
              match operation_of_string operation with
              | Ok operation -> operation
              | Error message -> invalid_arg message
            in
            Edit.make ~operation ~selector ?text ()))
    |> Jsont.Object.mem "op" Jsont.string ~enc:(fun edit ->
        operation_string (Edit.operation edit))
    |> Jsont.Object.mem "selector" selector_codec ~enc:Edit.selector
    |> Jsont.Object.opt_mem "text" Jsont.string ~enc:Edit.text
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let edit_codec =
    Codec.strict_object ~kind:"strict ocaml_ast_edit edit" edit_object_codec

  let make path file_kind if_identity edits =
    if String.is_empty path then invalid_arg "path must not be empty";
    let file_kind =
      match file_kind with
      | Some value -> (
          match file_kind_of_string value with
          | Ok file_kind -> file_kind
          | Error message -> invalid_arg message)
      | None -> (
          match infer_file_kind path with
          | Ok file_kind -> file_kind
          | Error message -> invalid_arg message)
    in
    let if_identity =
      match if_identity with
      | None -> None
      | Some "" -> invalid_arg "if_identity must not be empty"
      | Some token -> (
          match Mentat_digest.Content_ref.of_token token with
          | Ok identity -> Some identity
          | Error error ->
              invalid_arg
                ("if_identity is not a file identity: "
                ^ Mentat_digest.Error.message error))
    in
    if List.is_empty edits then invalid_arg "edits must not be empty";
    { path; file_kind; edits; if_identity }

  let object_codec =
    Jsont.Object.map ~kind:"ocaml_ast_edit input"
      (fun path file_kind if_identity edits ->
        Mentat_tool.Codec.decode_invalid_arg (fun () ->
            make path file_kind if_identity edits))
    |> Jsont.Object.mem "path" Jsont.string ~enc:path
    |> Jsont.Object.opt_mem "file_kind" Jsont.string ~enc:(fun input ->
        Some (file_kind_string input.file_kind))
    |> Jsont.Object.opt_mem "if_identity" Jsont.string ~enc:(fun input ->
        Option.map Mentat_digest.Content_ref.to_token input.if_identity)
    |> Jsont.Object.mem "edits" (Jsont.list edit_codec) ~enc:edits
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec =
    Codec.strict_object ~kind:"strict ocaml_ast_edit input" object_codec

  let string_enum values =
    Codec.obj
      [
        ("type", Jsont.Json.string "string");
        ( "enum",
          Jsont.Json.list
            (List.map (fun value -> Jsont.Json.string value) values) );
      ]

  let item_kind_schema =
    string_enum
      [
        "value";
        "type";
        "module";
        "module_type";
        "exception";
        "external";
        "open";
        "include";
        "class";
        "class_type";
        "extension";
        "eval";
      ]

  let property ?description fields =
    let fields =
      match description with
      | None -> fields
      | Some description ->
          ("description", Jsont.Json.string description) :: fields
    in
    Codec.obj fields

  let selector_schema =
    Codec.obj
      [
        ("type", Jsont.Json.string "object");
        ( "properties",
          Codec.obj
            [
              ( "mode",
                property
                  [
                    ("type", Jsont.Json.string "string");
                    ( "enum",
                      Jsont.Json.list
                        (List.map
                           (fun value -> Jsont.Json.string value)
                           [ "item"; "enclosing"; "exact" ]) );
                  ] );
              ( "path",
                property
                  ~description:
                    "Qualified item path. Required when mode is item."
                  [
                    ("type", Jsont.Json.string "array");
                    ("items", property [ ("type", Jsont.Json.string "string") ]);
                    ("minItems", Jsont.Json.int 1);
                  ] );
              ( "item_kind",
                property ~description:"Optional item namespace filter."
                  [ ("allOf", Jsont.Json.list [ item_kind_schema ]) ] );
              ( "occurrence",
                property
                  ~description:
                    "One-based matching declaration occurrence. Defaults to 1."
                  [
                    ("type", Jsont.Json.string "integer");
                    ("minimum", Jsont.Json.int 1);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
              ( "kind",
                property
                  ~description:"Node kind for enclosing and exact selectors."
                  [
                    ("type", Jsont.Json.string "string");
                    ( "enum",
                      Jsont.Json.list
                        (List.map
                           (fun value -> Jsont.Json.string value)
                           [ "item"; "expression"; "type" ]) );
                  ] );
              ( "node_item_kind",
                property
                  ~description:"Optional namespace filter when kind is item."
                  [ ("allOf", Jsont.Json.list [ item_kind_schema ]) ] );
              ( "line",
                property ~description:"One-based enclosing cursor line."
                  [
                    ("type", Jsont.Json.string "integer");
                    ("minimum", Jsont.Json.int 1);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
              ( "column",
                property ~description:"Zero-based byte cursor column."
                  [
                    ("type", Jsont.Json.string "integer");
                    ("minimum", Jsont.Json.int 0);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
              ( "start_line",
                property ~description:"One-based exact range start line."
                  [
                    ("type", Jsont.Json.string "integer");
                    ("minimum", Jsont.Json.int 1);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
              ( "start_column",
                property ~description:"Zero-based exact range start column."
                  [
                    ("type", Jsont.Json.string "integer");
                    ("minimum", Jsont.Json.int 0);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
              ( "end_line",
                property ~description:"One-based exact range end line."
                  [
                    ("type", Jsont.Json.string "integer");
                    ("minimum", Jsont.Json.int 1);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
              ( "end_column",
                property ~description:"Zero-based exact range end column."
                  [
                    ("type", Jsont.Json.string "integer");
                    ("minimum", Jsont.Json.int 0);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
            ] );
        ("required", Jsont.Json.list [ Jsont.Json.string "mode" ]);
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let edit_schema =
    Codec.obj
      [
        ("type", Jsont.Json.string "object");
        ( "properties",
          Codec.obj
            [
              ( "op",
                property
                  [
                    ("type", Jsont.Json.string "string");
                    ( "enum",
                      Jsont.Json.list
                        (List.map
                           (fun value -> Jsont.Json.string value)
                           [
                             "replace";
                             "insert_before";
                             "insert_after";
                             "delete";
                           ]) );
                  ] );
              ("selector", selector_schema);
              ( "text",
                property
                  ~description:
                    "Non-empty replacement or insertion OCaml fragment."
                  [
                    ("type", Jsont.Json.string "string");
                    ("minLength", Jsont.Json.int 1);
                  ] );
            ] );
        ( "required",
          Jsont.Json.list
            [ Jsont.Json.string "op"; Jsont.Json.string "selector" ] );
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let schema =
    Codec.obj
      [
        ("type", Jsont.Json.string "object");
        ( "properties",
          Codec.obj
            [
              ( "path",
                property
                  ~description:"Workspace OCaml source path to edit atomically."
                  [
                    ("type", Jsont.Json.string "string");
                    ("minLength", Jsont.Json.int 1);
                  ] );
              ( "file_kind",
                property
                  ~description:
                    "Source grammar; defaults from a .ml or .mli suffix."
                  [
                    ("type", Jsont.Json.string "string");
                    ( "enum",
                      Jsont.Json.list
                        (List.map
                           (fun value -> Jsont.Json.string value)
                           [ "implementation"; "interface"; "ml"; "mli" ]) );
                  ] );
              ( "if_identity",
                property
                  ~description:
                    "Complete-file identity from a previous complete read."
                  [
                    ("type", Jsont.Json.string "string");
                    ("minLength", Jsont.Json.int 1);
                  ] );
              ( "edits",
                property
                  [
                    ("type", Jsont.Json.string "array");
                    ("items", edit_schema);
                    ("minItems", Jsont.Json.int 1);
                  ] );
            ] );
        ( "required",
          Jsont.Json.list
            [ Jsont.Json.string "path"; Jsont.Json.string "edits" ] );
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

module Error = struct
  type t =
    | Invalid_text of string
    | Invalid_range of string
    | Parse_error of {
        phase : string;
        message : string;
        range : Mentat_ocaml.Range.t option;
      }
    | Selection_not_found of Selector.t
    | Ambiguous_selection of {
        selector : Selector.t;
        matches : Mentat_ocaml.Range.t list;
      }
    | Invalid_operation of string
    | Overlapping_edits of Mentat_ocaml.Range.t * Mentat_ocaml.Range.t
    | Edit_error of Mentat_edit.Error.t

  let message = function
    | Invalid_text reason -> "invalid UTF-8 text: " ^ reason
    | Invalid_range reason -> "invalid source range: " ^ reason
    | Parse_error { phase; message; range = None } ->
        phase ^ " parse error: " ^ message
    | Parse_error { phase; message; range = Some range } ->
        Format.asprintf "%s parse error at %a: %s" phase Mentat_ocaml.Range.pp
          range message
    | Selection_not_found selector ->
        Format.asprintf "AST selection not found: %a" Selector.pp selector
    | Ambiguous_selection { selector; matches } ->
        Format.asprintf "AST selection is ambiguous: %a matched %d ranges"
          Selector.pp selector (List.length matches)
    | Invalid_operation reason -> "invalid AST edit operation: " ^ reason
    | Overlapping_edits (left, right) ->
        Format.asprintf "AST edits overlap: %a and %a" Mentat_ocaml.Range.pp
          left Mentat_ocaml.Range.pp right
    | Edit_error error -> Edit_error.message error
end

type parsed =
  | Implementation_ast of Parsetree.structure
  | Interface_ast of Parsetree.signature

type node = {
  kind : Node_kind.t;
  path : string list option;
  location : Location.t;
}

type span = {
  start_offset : int;
  end_offset : int;
  range : Mentat_ocaml.Range.t;
}

type selected = { node : node; span : span }

let position_of_lexing position =
  Mentat_ocaml.Position.make ~line:position.Lexing.pos_lnum
    ~column:(position.Lexing.pos_cnum - position.Lexing.pos_bol)

let range_of_location location =
  Mentat_ocaml.Range.make
    ~start:(position_of_lexing location.Location.loc_start)
    ~end_:(position_of_lexing location.Location.loc_end)

let lexbuf ?(filename = "") text =
  let lexbuf = Lexing.from_string text in
  lexbuf.Lexing.lex_curr_p <-
    {
      Lexing.pos_fname = filename;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0;
    };
  lexbuf

let parse_error phase exception_ =
  let range =
    match exception_ with
    | Syntaxerr.Error error ->
        Some (range_of_location (Syntaxerr.location_of_error error))
    | _ -> None
  in
  let message =
    match Location.error_of_exn exception_ with
    | Some (`Ok report) ->
        Format.asprintf "%a" Format_doc.Doc.format
          report.Location.main.Location.txt
    | Some `Already_displayed | None -> Printexc.to_string exception_
  in
  Error.Parse_error { phase; message; range }

let parse_source ~path ~file_kind contents =
  let filename = Mentat_workspace.Path.display path in
  try
    match file_kind with
    | Implementation ->
        Ok
          (Implementation_ast (Parse.implementation (lexbuf ~filename contents)))
    | Interface ->
        Ok (Interface_ast (Parse.interface (lexbuf ~filename contents)))
  with exception_ -> Error (parse_error "source" exception_)

let parse_items ~file_kind text =
  try
    match file_kind with
    | Implementation ->
        ignore (Parse.implementation (lexbuf text) : Parsetree.structure);
        Ok ()
    | Interface ->
        ignore (Parse.interface (lexbuf text) : Parsetree.signature);
        Ok ()
  with exception_ -> Error (parse_error "replacement item" exception_)

let parse_expression text =
  try
    ignore (Parse.expression (lexbuf text) : Parsetree.expression);
    Ok ()
  with exception_ -> Error (parse_error "replacement expression" exception_)

let parse_type text =
  try
    ignore (Parse.core_type (lexbuf text) : Parsetree.core_type);
    Ok ()
  with exception_ -> Error (parse_error "replacement type" exception_)

let line_starts text =
  let starts = ref [ 0 ] in
  String.iteri
    (fun index character ->
      if Char.equal character '\n' then starts := (index + 1) :: !starts)
    text;
  Array.of_list (List.rev !starts)

let offset_of_position starts text position =
  let line = Mentat_ocaml.Position.line position in
  let column = Mentat_ocaml.Position.column position in
  if line < 1 || line > Array.length starts then
    Error
      (Error.Invalid_range (Printf.sprintf "line %d is outside the file" line))
  else
    let start = starts.(line - 1) in
    let limit =
      if line = Array.length starts then String.length text
      else starts.(line) - 1
    in
    let offset = start + column in
    if offset > limit then
      Error
        (Error.Invalid_range
           (Printf.sprintf "column %d is outside line %d" column line))
    else Ok offset

let span_of_location location =
  {
    start_offset = location.Location.loc_start.Lexing.pos_cnum;
    end_offset = location.Location.loc_end.Lexing.pos_cnum;
    range = range_of_location location;
  }

let contains_offset span offset =
  span.start_offset <= offset && offset < span.end_offset

let span_size span = span.end_offset - span.start_offset

let location_is_real location =
  (not location.Location.loc_ghost)
  && location.Location.loc_start.Lexing.pos_cnum
     <= location.Location.loc_end.Lexing.pos_cnum

let last_identifier (identifier : Longident.t Location.loc) =
  match identifier.Location.txt with
  | Longident.Lident name -> Some name
  | Longident.Ldot (_, name) -> Some name.Location.txt
  | Longident.Lapply _ -> None

let module_expression_name expression =
  match expression.Parsetree.pmod_desc with
  | Parsetree.Pmod_ident identifier -> last_identifier identifier
  | Parsetree.Pmod_structure _ | Parsetree.Pmod_functor _
  | Parsetree.Pmod_apply _ | Parsetree.Pmod_constraint _
  | Parsetree.Pmod_apply_unit _ | Parsetree.Pmod_unpack _
  | Parsetree.Pmod_extension _ ->
      None

let pattern_names pattern =
  let names = ref [] in
  let iterator =
    {
      Ast_iterator.default_iterator with
      Ast_iterator.pat =
        (fun self pattern ->
          begin match pattern.Parsetree.ppat_desc with
          | Parsetree.Ppat_var { Location.txt = name; Location.loc }
            when not loc.Location.loc_ghost ->
              names := name :: !names
          | _ -> ()
          end;
          Ast_iterator.default_iterator.Ast_iterator.pat self pattern);
    }
  in
  iterator.Ast_iterator.pat iterator pattern;
  List.rev !names

let add_node nodes kind path location =
  if location_is_real location then { kind; path; location } :: nodes else nodes

let add_item nodes kind path location =
  add_node nodes (Node_kind.Item (Some kind)) (Some path) location

let rec structure_nodes path nodes structure =
  List.fold_left (structure_item_nodes path) nodes structure

and structure_item_nodes path nodes item =
  let nodes =
    match item.Parsetree.pstr_desc with
    | Parsetree.Pstr_value (_, bindings) ->
        List.fold_left
          (fun nodes binding ->
            List.fold_left
              (fun nodes name ->
                add_item nodes Item_kind.Value (path @ [ name ])
                  item.Parsetree.pstr_loc)
              nodes
              (pattern_names binding.Parsetree.pvb_pat))
          nodes bindings
    | Parsetree.Pstr_type (_, declarations) ->
        List.fold_left
          (fun nodes declaration ->
            add_item nodes Item_kind.Type
              (path @ [ declaration.Parsetree.ptype_name.Location.txt ])
              item.Parsetree.pstr_loc)
          nodes declarations
    | Parsetree.Pstr_typext extension ->
        add_item nodes Item_kind.Type
          (path @ [ "type_extension" ])
          extension.Parsetree.ptyext_path.Location.loc
    | Parsetree.Pstr_exception extension ->
        add_item nodes Item_kind.Exception
          (path
          @ [
              extension.Parsetree.ptyexn_constructor.Parsetree.pext_name
                .Location.txt;
            ])
          item.Parsetree.pstr_loc
    | Parsetree.Pstr_module binding ->
        module_binding_node path nodes item.Parsetree.pstr_loc binding
    | Parsetree.Pstr_recmodule bindings ->
        List.fold_left
          (fun nodes binding ->
            module_binding_node path nodes item.Parsetree.pstr_loc binding)
          nodes bindings
    | Parsetree.Pstr_modtype declaration ->
        add_item nodes Item_kind.Module_type
          (path @ [ declaration.Parsetree.pmtd_name.Location.txt ])
          item.Parsetree.pstr_loc
    | Parsetree.Pstr_open declaration -> (
        match module_expression_name declaration.Parsetree.popen_expr with
        | Some name ->
            add_item nodes Item_kind.Open (path @ [ name ])
              item.Parsetree.pstr_loc
        | None ->
            add_item nodes Item_kind.Open (path @ [ "open" ])
              item.Parsetree.pstr_loc)
    | Parsetree.Pstr_include _ ->
        add_item nodes Item_kind.Include (path @ [ "include" ])
          item.Parsetree.pstr_loc
    | Parsetree.Pstr_primitive value ->
        add_item nodes Item_kind.External
          (path @ [ value.Parsetree.pval_name.Location.txt ])
          item.Parsetree.pstr_loc
    | Parsetree.Pstr_class declarations ->
        List.fold_left
          (fun nodes declaration ->
            add_item nodes Item_kind.Class
              (path @ [ declaration.Parsetree.pci_name.Location.txt ])
              item.Parsetree.pstr_loc)
          nodes declarations
    | Parsetree.Pstr_class_type declarations ->
        List.fold_left
          (fun nodes declaration ->
            add_item nodes Item_kind.Class_type
              (path @ [ declaration.Parsetree.pci_name.Location.txt ])
              item.Parsetree.pstr_loc)
          nodes declarations
    | Parsetree.Pstr_extension _ ->
        add_item nodes Item_kind.Extension (path @ [ "extension" ])
          item.Parsetree.pstr_loc
    | Parsetree.Pstr_eval _ ->
        add_item nodes Item_kind.Eval (path @ [ "eval" ])
          item.Parsetree.pstr_loc
    | Parsetree.Pstr_attribute _ -> nodes
  in
  let nodes_ref = ref nodes in
  let iterator =
    {
      Ast_iterator.default_iterator with
      Ast_iterator.expr =
        (fun self expression ->
          nodes_ref :=
            add_node !nodes_ref Node_kind.Expression None
              expression.Parsetree.pexp_loc;
          Ast_iterator.default_iterator.Ast_iterator.expr self expression);
      Ast_iterator.typ =
        (fun self core_type ->
          nodes_ref :=
            add_node !nodes_ref Node_kind.Type None core_type.Parsetree.ptyp_loc;
          Ast_iterator.default_iterator.Ast_iterator.typ self core_type);
    }
  in
  iterator.Ast_iterator.structure_item iterator item;
  !nodes_ref

and module_binding_node path nodes item_location binding =
  let name = binding.Parsetree.pmb_name.Location.txt in
  let nodes =
    match name with
    | Some name ->
        add_item nodes Item_kind.Module (path @ [ name ]) item_location
    | None -> nodes
  in
  match (name, binding.Parsetree.pmb_expr.Parsetree.pmod_desc) with
  | Some name, Parsetree.Pmod_structure structure ->
      structure_nodes (path @ [ name ]) nodes structure
  | Some _, _ | None, _ -> nodes

let rec signature_nodes path nodes signature =
  List.fold_left (signature_item_nodes path) nodes signature

and signature_item_nodes path nodes item =
  let nodes =
    match item.Parsetree.psig_desc with
    | Parsetree.Psig_value value ->
        add_item nodes Item_kind.Value
          (path @ [ value.Parsetree.pval_name.Location.txt ])
          item.Parsetree.psig_loc
    | Parsetree.Psig_type (_, declarations)
    | Parsetree.Psig_typesubst declarations ->
        List.fold_left
          (fun nodes declaration ->
            add_item nodes Item_kind.Type
              (path @ [ declaration.Parsetree.ptype_name.Location.txt ])
              item.Parsetree.psig_loc)
          nodes declarations
    | Parsetree.Psig_typext extension ->
        add_item nodes Item_kind.Type
          (path @ [ "type_extension" ])
          extension.Parsetree.ptyext_path.Location.loc
    | Parsetree.Psig_exception extension ->
        add_item nodes Item_kind.Exception
          (path
          @ [
              extension.Parsetree.ptyexn_constructor.Parsetree.pext_name
                .Location.txt;
            ])
          item.Parsetree.psig_loc
    | Parsetree.Psig_module declaration ->
        module_declaration_node path nodes item.Parsetree.psig_loc declaration
    | Parsetree.Psig_recmodule declarations ->
        List.fold_left
          (fun nodes declaration ->
            module_declaration_node path nodes item.Parsetree.psig_loc
              declaration)
          nodes declarations
    | Parsetree.Psig_modtype declaration
    | Parsetree.Psig_modtypesubst declaration ->
        add_item nodes Item_kind.Module_type
          (path @ [ declaration.Parsetree.pmtd_name.Location.txt ])
          item.Parsetree.psig_loc
    | Parsetree.Psig_modsubst substitution ->
        add_item nodes Item_kind.Module
          (path @ [ substitution.Parsetree.pms_name.Location.txt ])
          item.Parsetree.psig_loc
    | Parsetree.Psig_open declaration -> (
        match last_identifier declaration.Parsetree.popen_expr with
        | Some name ->
            add_item nodes Item_kind.Open (path @ [ name ])
              item.Parsetree.psig_loc
        | None ->
            add_item nodes Item_kind.Open (path @ [ "open" ])
              item.Parsetree.psig_loc)
    | Parsetree.Psig_include _ ->
        add_item nodes Item_kind.Include (path @ [ "include" ])
          item.Parsetree.psig_loc
    | Parsetree.Psig_class descriptions ->
        List.fold_left
          (fun nodes description ->
            add_item nodes Item_kind.Class
              (path @ [ description.Parsetree.pci_name.Location.txt ])
              item.Parsetree.psig_loc)
          nodes descriptions
    | Parsetree.Psig_class_type descriptions ->
        List.fold_left
          (fun nodes description ->
            add_item nodes Item_kind.Class_type
              (path @ [ description.Parsetree.pci_name.Location.txt ])
              item.Parsetree.psig_loc)
          nodes descriptions
    | Parsetree.Psig_extension _ ->
        add_item nodes Item_kind.Extension (path @ [ "extension" ])
          item.Parsetree.psig_loc
    | Parsetree.Psig_attribute _ -> nodes
  in
  let nodes_ref = ref nodes in
  let iterator =
    {
      Ast_iterator.default_iterator with
      Ast_iterator.typ =
        (fun self core_type ->
          nodes_ref :=
            add_node !nodes_ref Node_kind.Type None core_type.Parsetree.ptyp_loc;
          Ast_iterator.default_iterator.Ast_iterator.typ self core_type);
    }
  in
  iterator.Ast_iterator.signature_item iterator item;
  !nodes_ref

and module_declaration_node path nodes item_location declaration =
  let name = declaration.Parsetree.pmd_name.Location.txt in
  let nodes =
    match name with
    | Some name ->
        add_item nodes Item_kind.Module (path @ [ name ]) item_location
    | None -> nodes
  in
  match (name, declaration.Parsetree.pmd_type.Parsetree.pmty_desc) with
  | Some name, Parsetree.Pmty_signature signature ->
      signature_nodes (path @ [ name ]) nodes signature
  | Some _, _ | None, _ -> nodes

let nodes_of_parsed = function
  | Implementation_ast structure -> List.rev (structure_nodes [] [] structure)
  | Interface_ast signature -> List.rev (signature_nodes [] [] signature)

let select_item nodes path kind occurrence =
  let matches =
    List.filter
      (fun node ->
        match node.path with
        | Some node_path ->
            List.equal String.equal path node_path
            && Node_kind.matches (Node_kind.Item kind) node.kind
        | None -> false)
      nodes
  in
  let rec drop count values =
    if count = 0 then values
    else match values with [] -> [] | _ :: rest -> drop (count - 1) rest
  in
  match drop (occurrence - 1) matches with
  | node :: _ -> Ok node
  | [] -> Error `Not_found

let select_enclosing nodes contents kind position =
  let starts = line_starts contents in
  match offset_of_position starts contents position with
  | Error error -> Error (`Range error)
  | Ok offset ->
      let matches =
        nodes
        |> List.filter (fun node ->
            Node_kind.matches kind node.kind
            && contains_offset (span_of_location node.location) offset)
        |> List.sort (fun left right ->
            Int.compare
              (span_size (span_of_location left.location))
              (span_size (span_of_location right.location)))
      in
      begin match matches with node :: _ -> Ok node | [] -> Error `Not_found
      end

let select_exact nodes kind range =
  let matches =
    List.filter
      (fun node ->
        Node_kind.matches kind node.kind
        && Mentat_ocaml.Range.equal range (range_of_location node.location))
      nodes
  in
  match matches with
  | [ node ] -> Ok node
  | [] -> Error `Not_found
  | node :: rest
    when List.for_all
           (fun other ->
             Mentat_ocaml.Range.equal
               (range_of_location node.location)
               (range_of_location other.location))
           rest ->
      Ok node
  | matches ->
      Error
        (`Ambiguous
           (List.map (fun node -> range_of_location node.location) matches))

let resolve_selector nodes contents selector =
  match selector with
  | Selector.Item { path; kind; occurrence } -> (
      match select_item nodes path kind occurrence with
      | Ok node -> Ok node
      | Error `Not_found -> Error (Error.Selection_not_found selector))
  | Selector.Enclosing { kind; position } -> (
      match select_enclosing nodes contents kind position with
      | Ok node -> Ok node
      | Error (`Range error) -> Error error
      | Error `Not_found -> Error (Error.Selection_not_found selector))
  | Selector.Exact { kind; range } -> (
      match select_exact nodes kind range with
      | Ok node -> Ok node
      | Error `Not_found -> Error (Error.Selection_not_found selector)
      | Error (`Ambiguous matches) ->
          Error (Error.Ambiguous_selection { selector; matches }))

let validate_replacement ~file_kind selected edit =
  match (Edit.operation edit, selected.node.kind, Edit.text edit) with
  | Edit.Delete, _, None -> Ok ""
  | Edit.Replace, Node_kind.Item _, Some text ->
      Result.map (fun () -> text) (parse_items ~file_kind text)
  | Edit.Replace, Node_kind.Expression, Some text ->
      Result.map (fun () -> text) (parse_expression text)
  | Edit.Replace, Node_kind.Type, Some text ->
      Result.map (fun () -> text) (parse_type text)
  | (Edit.Insert_before | Edit.Insert_after), Node_kind.Item _, Some text ->
      Result.map (fun () -> text) (parse_items ~file_kind text)
  | ( (Edit.Insert_before | Edit.Insert_after),
      (Node_kind.Expression | Node_kind.Type),
      Some _ ) ->
      Error
        (Error.Invalid_operation
           "insert_before and insert_after are only valid around item \
            selections")
  | (Edit.Replace | Edit.Insert_before | Edit.Insert_after), _, None ->
      Error (Error.Invalid_operation "operation is missing replacement text")
  | Edit.Delete, _, Some _ -> Ok ""

type patch = {
  selected : selected;
  replace_start : int;
  replace_end : int;
  replacement : string;
}

let resolve_edit nodes contents ~file_kind edit =
  match resolve_selector nodes contents (Edit.selector edit) with
  | Error error -> Error error
  | Ok node ->
      let span = span_of_location node.location in
      let selected = { node; span } in
      begin match validate_replacement ~file_kind selected edit with
      | Error error -> Error error
      | Ok replacement ->
          let replace_start, replace_end =
            match Edit.operation edit with
            | Edit.Replace | Edit.Delete -> (span.start_offset, span.end_offset)
            | Edit.Insert_before -> (span.start_offset, span.start_offset)
            | Edit.Insert_after -> (span.end_offset, span.end_offset)
          in
          Ok { selected; replace_start; replace_end; replacement }
      end

let patch_order left right =
  match Int.compare left.replace_start right.replace_start with
  | 0 -> Int.compare left.replace_end right.replace_end
  | order -> order

let check_overlaps patches =
  let patches = List.sort patch_order patches in
  let rec loop = function
    | first :: (second :: _ as rest) ->
        if
          first.replace_end > second.replace_start
          || first.replace_start = second.replace_start
             && (first.replace_end > first.replace_start
                || second.replace_end > second.replace_start)
        then
          Error
            (Error.Overlapping_edits
               (first.selected.span.range, second.selected.span.range))
        else loop rest
    | [] | [ _ ] -> Ok ()
  in
  loop patches

let rewritten_size contents patches =
  List.fold_left
    (fun size patch ->
      let removed = patch.replace_end - patch.replace_start in
      Int64.add size (Int64.of_int (String.length patch.replacement - removed)))
    (Int64.of_int (String.length contents))
    patches

let apply_patches ~final_size contents patches =
  let patches =
    List.sort
      (fun left right ->
        match Int.compare right.replace_start left.replace_start with
        | 0 -> Int.compare right.replace_end left.replace_end
        | order -> order)
      patches
  in
  let cursor = ref (String.length contents) in
  let pieces = ref [] in
  List.iter
    (fun patch ->
      let unchanged =
        String.sub contents patch.replace_end (!cursor - patch.replace_end)
      in
      pieces := patch.replacement :: unchanged :: !pieces;
      cursor := patch.replace_start)
    patches;
  let output = Buffer.create final_size in
  Buffer.add_substring output contents 0 !cursor;
  List.iter (Buffer.add_string output) !pieces;
  Buffer.contents output

let rec resolve_edits nodes contents file_kind resolved = function
  | [] -> Ok (List.rev resolved)
  | edit :: edits -> (
      match resolve_edit nodes contents ~file_kind edit with
      | Error error -> Error error
      | Ok patch ->
          resolve_edits nodes contents file_kind (patch :: resolved) edits)

type plan = { edit : Mentat_edit.t }

let plan ~path ~file_kind ~contents edits =
  if not (String.is_valid_utf_8 contents) then
    Error (Error.Invalid_text "source contents must be valid UTF-8")
  else if Text_helpers.looks_binary contents then
    Error (Error.Invalid_text "source contents must be UTF-8 text")
  else if List.is_empty edits then
    Error (Error.Invalid_operation "at least one AST edit is required")
  else
    match parse_source ~path ~file_kind contents with
    | Error error -> Error error
    | Ok parsed -> (
        let nodes = nodes_of_parsed parsed in
        match resolve_edits nodes contents file_kind [] edits with
        | Error error -> Error error
        | Ok patches -> (
            match check_overlaps patches with
            | Error error -> Error error
            | Ok () -> (
                let after_size = rewritten_size contents patches in
                if Int64.compare after_size (Int64.of_int max_file_bytes) > 0
                then
                  Error
                    (Error.Edit_error
                       (Mentat_edit.Error.too_large ~path ~size:after_size
                          ~max_size:(Int64.of_int max_file_bytes)))
                else
                  let after_contents =
                    apply_patches ~final_size:(Int64.to_int after_size) contents
                      patches
                  in
                  match parse_source ~path ~file_kind after_contents with
                  | Error (Error.Parse_error { message; range; phase = _ }) ->
                      Error
                        (Error.Parse_error
                           { phase = "edited source"; message; range })
                  | Error error -> Error error
                  | Ok _ -> (
                      match
                        Mentat_edit.rewrite ~path ~before:contents
                          ~after:after_contents
                      with
                      | Error error -> Error (Error.Edit_error error)
                      | Ok edit -> Ok { edit }))))

let planned_change edits =
  let additions =
    List.fold_left
      (fun count edit ->
        match Edit.text edit with
        | Some text -> count + Text_helpers.logical_line_count text
        | None -> count)
      0 edits
  in
  let removals =
    if
      List.for_all
        (fun edit ->
          match Edit.operation edit with
          | Edit.Insert_before | Edit.Insert_after -> true
          | Edit.Replace | Edit.Delete -> false)
        edits
    then Some 0
    else None
  in
  Mentat_permission.Request.Change.make ~additions ?removals ()

module Output = struct
  module Update = Mentat_tools_output.Update

  type operation = Modify | Unchanged

  type t = {
    path : string;
    operation : operation;
    added : int;
    removed : int option;
  }

  let modified ~path change =
    {
      path;
      operation = Modify;
      added =
        Option.value
          (Mentat_permission.Request.Change.additions change)
          ~default:0;
      removed = Mentat_permission.Request.Change.removals change;
    }

  let unchanged ~path =
    { path; operation = Unchanged; added = 0; removed = Some 0 }

  let operation_string = function
    | Modify -> "modify"
    | Unchanged -> "unchanged"

  let text (output : t) =
    let removed =
      match output.removed with
      | Some count -> string_of_int count
      | None -> "unknown"
    in
    Printf.sprintf "%s: %s added=%d removed=%s\n"
      (operation_string output.operation)
      output.path output.added removed

  let encode output =
    let files = match output.operation with Modify -> 1 | Unchanged -> 0 in
    let update =
      Update.make ~disposition:Update.Applied ~files
        ~additions:(Some output.added) ~deletions:output.removed ~skipped:0
    in
    Mentat_tools_output.Codec.encode Update.jsont ~text:(text output) update
end

let interrupted () = Mentat_tool.Result.cancelled ()

let stale path =
  Mentat_tool.Result.failed `Stale
    (Mentat_workspace.Path.display path ^ ": stale file identity")

let failed_plan error =
  Mentat_tool.Result.failed `Invalid_input (Error.message error)

let apply workspace_io plan output =
  Edit_error.applied workspace_io plan.edit ~output

let run workspace_io input ~cancelled =
  if cancelled () then interrupted ()
  else
    Permissions.with_resolved_run workspace_io (Input.path input) (fun path ->
        match
          Merlin_support.load_source workspace_io path ~max_bytes:max_file_bytes
        with
        | Error result -> result
        | Ok contents -> (
            match Input.if_identity input with
            | Some expected
              when not (Mentat_digest.Content_ref.matches expected contents) ->
                stale path
            | Some _ | None -> (
                match
                  plan ~path ~file_kind:(Input.file_kind input) ~contents
                    (Input.edits input)
                with
                | Error error -> failed_plan error
                | Ok plan ->
                    if cancelled () then interrupted ()
                    else if Mentat_edit.is_empty plan.edit then
                      Mentat_tool.Result.completed
                        ~output:
                          (Output.unchanged
                             ~path:(Address.display workspace_io path))
                        ()
                    else
                      apply workspace_io plan
                        (Output.modified
                           ~path:(Address.display workspace_io path)
                           (planned_change (Input.edits input))))))

let permissions workspace_io input =
  Permissions.with_resolved workspace_io (Input.path input) (fun path ->
      let item =
        Mentat_permission.Request.Item.make
          ~change:(planned_change (Input.edits input))
          (Mentat_permission.Access.path ~op:`Modify path)
      in
      [ Mentat_permission.Request.make ~source:name [ item ] ])

let make workspace_io =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.ocaml_ast_edit
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io)
    ~run:(fun ~cancelled input -> run workspace_io input ~cancelled)
    ()
