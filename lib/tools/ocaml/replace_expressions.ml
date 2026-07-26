(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let max_source_bytes = 8 * 1024 * 1024

open Parsetree
open Longident
open Asttypes
open Ast_iterator
module Syntax = Mentat_ocaml
module Grep = Mentat_ocaml_grep

let name = "ocaml_replace_expressions"
let default_max_sites = 200
let max_max_sites = 1_000
let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value
let json_bool value = Jsont.Json.bool value

module Input = struct
  type t = {
    pattern : string;
    template : string;
    paths : string list option;
    max_sites : int option;
    dry_run : bool;
  }

  let validate_text name text =
    if String.is_empty text then invalid_arg (name ^ " must not be empty");
    if String.contains text '\x00' then
      invalid_arg (name ^ " must not contain NUL")

  let validate_path path =
    if String.is_empty path then
      invalid_arg "paths must not contain empty paths";
    if String.contains path '\x00' then invalid_arg "paths must not contain NUL"

  let validate_paths = function
    | None -> ()
    | Some [] -> invalid_arg "paths must not be empty"
    | Some paths -> List.iter validate_path paths

  let validate_max_sites = function
    | None -> ()
    | Some value when value < 1 -> invalid_arg "max_sites must be at least 1"
    | Some value when value > max_max_sites ->
        invalid_arg ("max_sites must be at most " ^ string_of_int max_max_sites)
    | Some _ -> ()

  let make pattern template paths max_sites dry_run =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    validate_text "pattern" pattern;
    validate_text "template" template;
    validate_paths paths;
    validate_max_sites max_sites;
    {
      pattern;
      template;
      paths;
      max_sites;
      dry_run = Option.value dry_run ~default:false;
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

  let object_codec =
    Jsont.Object.map ~kind:"ocaml_replace_expressions input" make
    |> Jsont.Object.mem "pattern" Jsont.string ~enc:(fun input -> input.pattern)
    |> Jsont.Object.mem "template" Jsont.string ~enc:(fun input ->
        input.template)
    |> Jsont.Object.opt_mem "paths" (Jsont.list Jsont.string) ~enc:(fun input ->
        input.paths)
    |> Jsont.Object.opt_mem "max_sites" exact_integer ~enc:(fun input ->
        input.max_sites)
    |> Jsont.Object.opt_mem "dry_run" Jsont.bool ~enc:(fun input ->
        if input.dry_run then Some true else None)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec =
    Codec.strict_object ~kind:"strict ocaml_replace_expressions input"
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
                  "One complete OCaml expression pattern. __ is an anonymous \
                   expression wildcard; __1/__2 are unification metavariables; \
                   optional-argument markers and set-matched clauses follow \
                   ocaml_search_expressions."
                  [ ("minLength", json_int 1) ] );
              ( "template",
                schema_property "string"
                  "One OCaml expression whose numbered holes are bound by \
                   pattern. Captured source is spliced exactly and verified \
                   structurally; capture-prone binder scopes are rejected."
                  [ ("minLength", json_int 1) ] );
              ( "paths",
                schema_property "array"
                  "Workspace-relative or workspace-contained absolute regular \
                   file or directory roots. Defaults to the logical current \
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
              ( "max_sites",
                schema_property "integer"
                  "Maximum matched sites across all files. Exceeding it writes \
                   nothing. Defaults to 200."
                  [
                    ("minimum", json_int 1); ("maximum", json_int max_max_sites);
                  ] );
              ( "dry_run",
                schema_property "boolean"
                  "Validate and summarize rewrites without applying them. \
                   Defaults to false."
                  [] );
            ] );
        ( "required",
          Jsont.Json.list [ json_string "pattern"; json_string "template" ] );
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
  let effective_paths input = Option.value input.paths ~default:[ "." ]

  let effective_max_sites input =
    Option.value input.max_sites ~default:default_max_sites
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

let parse_expr text =
  match Parse.expression (Lexing.from_string text) with
  | expression -> Some expression
  | exception _ -> None

let parse_expr_diag text =
  match Parse.expression (Lexing.from_string text) with
  | expression -> Ok expression
  | exception exn ->
      let position =
        match exn with
        | Syntaxerr.Error error ->
            let location = Syntaxerr.location_of_error error in
            let start = location.Location.loc_start in
            Printf.sprintf " at line %d, column %d" start.Lexing.pos_lnum
              (start.Lexing.pos_cnum - start.Lexing.pos_bol)
        | _ -> ""
      in
      Error (Printf.sprintf "not a single OCaml expression%s" position)

let is_metavar = Grep.Pattern.is_metavariable_name

let collect_metavars_with ~expression_holes ast =
  let metavars = ref [] in
  let add name =
    if is_metavar name && not (List.mem name !metavars) then
      metavars := name :: !metavars
  in
  let rec longident = function
    | Lident name -> add name
    | Ldot (prefix, name) ->
        longident prefix.Location.txt;
        add name.Location.txt
    | Lapply (left, right) ->
        longident left.Location.txt;
        longident right.Location.txt
  in
  let super = Ast_iterator.default_iterator in
  let expr self expression =
    (match expression.pexp_desc with
    | Pexp_ident { txt = Lident name; _ } when is_metavar name ->
        if expression_holes then add name
    | Pexp_ident { txt; _ } | Pexp_new { txt; _ } -> longident txt
    | Pexp_construct ({ txt; _ }, _) -> longident txt
    | Pexp_field (_, { txt; _ }) | Pexp_setfield (_, { txt; _ }, _) ->
        longident txt
    | _ -> ());
    super.expr self expression
  in
  let pat self pattern =
    (match pattern.ppat_desc with
    | Ppat_var { txt; _ } -> add txt
    | Ppat_construct ({ txt; _ }, _) -> longident txt
    | _ -> ());
    super.pat self pattern
  in
  let iterator = { super with expr; pat } in
  iterator.expr iterator ast;
  List.sort String.compare !metavars

let collect_metavars = collect_metavars_with ~expression_holes:true

let template_holes ast =
  let holes = ref [] in
  let super = Ast_iterator.default_iterator in
  let expr self expression =
    match expression.pexp_desc with
    | Pexp_ident { txt = Lident name; _ } when is_metavar name ->
        holes := (name, expression.pexp_loc) :: !holes
    | _ -> super.expr self expression
  in
  let iterator = { super with expr } in
  iterator.expr iterator ast;
  List.rev !holes

let non_hole_metavars = collect_metavars_with ~expression_holes:false

let forbidden_vocabulary ast =
  let found = ref None in
  let set message = if Option.is_none !found then found := Some message in
  let rec longident = function
    | Lident "__" ->
        set
          "the anonymous wildcard __ is pattern-only; a template needs a \
           numbered hole like __1"
    | Lident _ -> ()
    | Ldot (prefix, name) ->
        longident prefix.Location.txt;
        if String.equal name.Location.txt "__" then
          set "the anonymous wildcard __ is pattern-only in a template"
    | Lapply (left, right) ->
        longident left.Location.txt;
        longident right.Location.txt
  in
  let super = Ast_iterator.default_iterator in
  let expr self expression =
    (match expression.pexp_desc with
    | Pexp_ident { txt; _ } -> longident txt
    | Pexp_construct
        ({ txt = Lident (("PRESENT" | "MISSING") as marker); _ }, None) ->
        set
          (marker
         ^ " is a pattern-only optional-argument marker and has no meaning in \
            a template")
    | _ -> ());
    super.expr self expression
  in
  let pat self pattern =
    (match pattern.ppat_desc with
    | Ppat_var { txt = "__"; _ } -> set "the wildcard __ is pattern-only"
    | _ -> ());
    super.pat self pattern
  in
  let iterator = { super with expr; pat } in
  iterator.expr iterator ast;
  !found

let pattern_vars pattern =
  let variables = ref [] in
  let super = Ast_iterator.default_iterator in
  let pat self pattern =
    (match pattern.ppat_desc with
    | Ppat_var { txt; _ } -> variables := txt :: !variables
    | Ppat_alias (_, { txt; _ }) -> variables := txt :: !variables
    | _ -> ());
    super.pat self pattern
  in
  let iterator = { super with pat } in
  iterator.pat iterator pattern;
  List.rev !variables

let param_vars (parameter : function_param) =
  match parameter.pparam_desc with
  | Pparam_val (_, _, pattern) -> pattern_vars pattern
  | Pparam_newtype _ -> []

type scope = { location : Location.t; kind : string; binders : string list }

let template_scopes ast =
  let scopes = ref [] in
  let add location kind binders =
    if not (List.is_empty binders) then
      scopes := { location; kind; binders } :: !scopes
  in
  let add_cases kind cases =
    List.iter
      (fun case ->
        let binders = pattern_vars case.pc_lhs in
        add case.pc_rhs.pexp_loc kind binders;
        Option.iter (fun guard -> add guard.pexp_loc kind binders) case.pc_guard)
      cases
  in
  let rec add_parameter_defaults binders = function
    | [] -> ()
    | parameter :: parameters -> (
        match parameter.pparam_desc with
        | Pparam_newtype _ -> add_parameter_defaults binders parameters
        | Pparam_val (_, default, pattern) ->
            Option.iter
              (fun expression ->
                add expression.pexp_loc "fun parameter default" binders)
              default;
            add_parameter_defaults (binders @ pattern_vars pattern) parameters)
  in
  let super = Ast_iterator.default_iterator in
  let expr self expression =
    (match expression.pexp_desc with
    | Pexp_let (recursive, bindings, body) ->
        let binders =
          List.concat_map (fun binding -> pattern_vars binding.pvb_pat) bindings
        in
        add body.pexp_loc "let" binders;
        if recursive = Recursive then
          List.iter
            (fun binding -> add binding.pvb_expr.pexp_loc "let rec" binders)
            bindings
    | Pexp_function (parameters, _, body) -> (
        add_parameter_defaults [] parameters;
        let binders = List.concat_map param_vars parameters in
        let body_location =
          match body with
          | Pfunction_body body -> body.pexp_loc
          | Pfunction_cases (_, location, _) -> location
        in
        add body_location "fun" binders;
        match body with
        | Pfunction_body _ -> ()
        | Pfunction_cases (cases, _, _) -> add_cases "function case" cases)
    | Pexp_match (_, cases) | Pexp_try (_, cases) ->
        add_cases "match/case" cases
    | Pexp_for (pattern, _, _, _, body) ->
        add body.pexp_loc "for" (pattern_vars pattern)
    | Pexp_struct_item (_, body) ->
        scopes :=
          {
            location = body.pexp_loc;
            kind = "local definition";
            binders = [ "local binding" ];
          }
          :: !scopes
    | Pexp_letop { let_; ands; body } ->
        let binders =
          pattern_vars let_.pbop_pat
          @ List.concat_map (fun binding -> pattern_vars binding.pbop_pat) ands
        in
        add body.pexp_loc "binding operator" binders
    | _ -> ());
    super.expr self expression
  in
  let iterator = { super with expr } in
  iterator.expr iterator ast;
  !scopes

let capture_offenders ast =
  let scopes = template_scopes ast in
  let contains outer inner =
    outer.Location.loc_start.Lexing.pos_cnum
    <= inner.Location.loc_start.Lexing.pos_cnum
    && inner.Location.loc_end.Lexing.pos_cnum
       <= outer.Location.loc_end.Lexing.pos_cnum
  in
  template_holes ast
  |> List.filter_map (fun (hole, location) ->
      match
        List.find_opt
          (fun (scope : scope) -> contains scope.location location)
          scopes
      with
      | Some scope -> Some (hole, scope)
      | None -> None)

let validate_template ~pattern_metavars raw =
  let template = String.trim raw in
  match parse_expr_diag template with
  | Error message -> Error ("template is " ^ message)
  | Ok ast -> (
      match forbidden_vocabulary ast with
      | Some message -> Error message
      | None -> (
          let holes = template_holes ast in
          let template_metavars = collect_metavars ast in
          let non_holes = non_hole_metavars ast in
          if not (List.is_empty non_holes) then
            Error
              (Printf.sprintf
                 "template uses metavariable(s) %s outside expression-hole \
                  positions"
                 (String.concat ", " non_holes))
          else
            let missing =
              List.filter
                (fun name -> not (List.mem name pattern_metavars))
                template_metavars
            in
            if not (List.is_empty missing) then
              Error
                (Printf.sprintf
                   "template uses metavariable(s) %s that the pattern does not \
                    bind"
                   (String.concat ", " missing))
            else
              match capture_offenders ast with
              | (hole, scope) :: _ ->
                  Error
                    (Printf.sprintf
                       "template hole %s is in the scope of a %s binder (%s), \
                        which risks variable capture; keep template holes \
                        outside any binder the template introduces"
                       hole scope.kind
                       (String.concat ", " scope.binders))
              | [] -> Ok (template, ast, holes)))

let line_starts source =
  let starts = ref [ 0 ] in
  String.iteri
    (fun index char ->
      if Char.equal char '\n' then starts := (index + 1) :: !starts)
    source;
  Array.of_list (List.rev !starts)

let byte_offset starts position =
  starts.(Syntax.Position.line position - 1) + Syntax.Position.column position

let range_bytes starts range =
  ( byte_offset starts (Syntax.Range.start range),
    byte_offset starts (Syntax.Range.end_ range) )

let slice source start_offset end_offset =
  String.sub source start_offset (end_offset - start_offset)

let splice text substitutions =
  let substitutions =
    List.sort
      (fun (left, _, _) (right, _, _) -> Int.compare right left)
      substitutions
  in
  List.fold_left
    (fun text (start_offset, end_offset, replacement) ->
      slice text 0 start_offset ^ replacement
      ^ slice text end_offset (String.length text))
    text substitutions

type render_mode = Minimal | Widened
type fragment = { raw : string; ast : Parsetree.expression; atomic : bool }

let is_atomic raw ast =
  match ast.pexp_desc with
  | Pexp_ident _ -> true
  | Pexp_constant _ ->
      let trimmed = String.trim raw in
      not
        (String.length trimmed > 0
        && (Char.equal trimmed.[0] '-' || Char.equal trimmed.[0] '+'))
  | _ -> false

let render template holes fragments mode =
  let substitutions =
    List.map
      (fun (hole, location) ->
        let fragment = List.assoc hole fragments in
        let wrap =
          String.includes ~affix:"[@" fragment.raw
          || match mode with Widened -> not fragment.atomic | Minimal -> false
        in
        let text = if wrap then "(" ^ fragment.raw ^ ")" else fragment.raw in
        ( location.Location.loc_start.Lexing.pos_cnum,
          location.Location.loc_end.Lexing.pos_cnum,
          text ))
      holes
  in
  splice template substitutions

let expected_ast template_ast fragments =
  let super = Ast_mapper.default_mapper in
  let expr self expression =
    match expression.pexp_desc with
    | Pexp_ident { txt = Lident name; _ } when is_metavar name -> (
        match List.assoc_opt name fragments with
        | Some fragment -> fragment.ast
        | None -> super.Ast_mapper.expr self expression)
    | _ -> super.Ast_mapper.expr self expression
  in
  let mapper = { super with Ast_mapper.expr } in
  mapper.Ast_mapper.expr mapper template_ast

let isolation_ok replacement expected =
  match parse_expr replacement with
  | Some expression -> Grep.structurally_equal_expr expression expected
  | None -> false

let expr_spans structure =
  let spans = ref [] in
  let super = Ast_iterator.default_iterator in
  let expr self expression =
    spans :=
      ( expression.pexp_loc.Location.loc_start.Lexing.pos_cnum,
        expression.pexp_loc.Location.loc_end.Lexing.pos_cnum,
        expression )
      :: !spans;
    super.expr self expression
  in
  let iterator = { super with expr } in
  iterator.structure iterator structure;
  !spans

let node_at spans start_offset end_offset =
  List.find_map
    (fun (start, end_, node) ->
      if start = start_offset && end_ = end_offset then Some node else None)
    spans

type site = {
  start_offset : int;
  end_offset : int;
  expected : Parsetree.expression;
  location : Syntax.Location.t;
  mutable text : string;
  mutable wrapped : bool;
}

let apply_sites source sites =
  splice source
    (List.map
       (fun site -> (site.start_offset, site.end_offset, site.text))
       sites)

let final_ranges sites =
  let rec loop delta = function
    | [] -> []
    | site :: sites ->
        let start_offset = site.start_offset + delta in
        let end_offset = start_offset + String.length site.text in
        (site, start_offset, end_offset)
        :: loop
             (delta + String.length site.text
             - (site.end_offset - site.start_offset))
             sites
  in
  loop 0 sites

let build_fragments site_texts =
  List.fold_left
    (fun fragments (hole, raw) ->
      match fragments with
      | Error _ -> fragments
      | Ok fragments -> (
          match parse_expr raw with
          | Some ast ->
              Ok ((hole, { raw; ast; atomic = is_atomic raw ast }) :: fragments)
          | None -> Error hole))
    (Ok []) site_texts

let site_text_of source starts binding =
  match Grep.Binding.captured binding with
  | Grep.Binding.Source range ->
      let start_offset, end_offset = range_bytes starts range in
      slice source start_offset end_offset
  | Grep.Binding.Ident identifier -> identifier

let build_site ~template ~template_ast ~holes source starts (location, bindings)
    =
  let range = Syntax.Location.range location in
  let start_offset, end_offset = range_bytes starts range in
  let site_texts =
    List.map
      (fun binding ->
        (Grep.Binding.name binding, site_text_of source starts binding))
      bindings
  in
  match build_fragments site_texts with
  | Error hole ->
      Error
        (Printf.sprintf "captured fragment for %s did not parse in isolation"
           hole)
  | Ok fragments ->
      let expected = expected_ast template_ast fragments in
      let rendered =
        match render template holes fragments Minimal with
        | minimal when isolation_ok minimal expected -> Some minimal
        | _ ->
            let widened = render template holes fragments Widened in
            if isolation_ok widened expected then Some widened else None
      in
      begin match rendered with
      | None ->
          Error
            (Printf.sprintf
               "site at %d:%d cannot be parenthesized to the template's \
                structure"
               (Syntax.Position.line (Syntax.Range.start range))
               (Syntax.Position.column (Syntax.Range.start range)))
      | Some text ->
          Ok
            {
              start_offset;
              end_offset;
              expected;
              location;
              text;
              wrapped = false;
            }
      end

let rec verify_file ~cancelled ~address source sites =
  if cancelled () then Error `Cancelled
  else
    let after = apply_sites source sites in
    match Grep.parse_implementation ~filename:address after with
    | Error error ->
        Error (`Rewrite_unparsable (Grep.Parse_error.to_string error))
    | Ok structure ->
        if cancelled () then Error `Cancelled
        else
          let spans = expr_spans structure in
          let failing =
            List.find_opt
              (fun (site, start_offset, end_offset) ->
                match node_at spans start_offset end_offset with
                | Some node ->
                    not (Grep.structurally_equal_expr node site.expected)
                | None -> true)
              (final_ranges sites)
          in
          begin match failing with
          | None -> Ok after
          | Some (site, _, _) ->
              if site.wrapped then
                let range = Syntax.Location.range site.location in
                Error
                  (`Unrenderable
                     (Printf.sprintf
                        "site at %d.%d could not be reconciled with the \
                         template after parenthesization"
                        (Syntax.Position.line (Syntax.Range.start range))
                        (Syntax.Position.column (Syntax.Range.start range))))
              else (
                site.text <- "(" ^ site.text ^ ")";
                site.wrapped <- true;
                verify_file ~cancelled ~address source sites)
          end

type root_kind = Regular_file | Directory
type root = { path : Mentat_workspace.Path.t; kind : root_kind }

type search_error =
  | File_error of Mentat_workspace_io.File_error.t
  | Invalid_root of Mentat_workspace.Path.t * Eio.File.Stat.kind
  | Enumerate of string
  | Cancelled

let interrupted () = Mentat_tool.Result.cancelled ()

let error_kind = function
  | File_error error -> Fs_error.failure error
  | Invalid_root _ -> `Invalid_input
  | Enumerate _ | Cancelled -> `Failed

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

let resolve_roots workspace_io ~cancelled input =
  let rec loop seen roots = function
    | [] -> Ok (List.rev roots)
    | raw :: raws -> (
        if cancelled () then Error (interrupted ())
        else
          match Mentat_workspace_io.resolve_path workspace_io raw with
          | Error error ->
              Error
                (Mentat_tool.Result.failed `Invalid_input
                   (Mentat_workspace.Resolve_error.message error))
          | Ok path -> (
              if Mentat_workspace.Path.Set.mem path seen then
                loop seen roots raws
              else
                match Mentat_workspace_io.File.lstat workspace_io path with
                | Error error -> Error (failed (File_error error))
                | Ok stat ->
                    let kind =
                      match stat.Eio.File.Stat.kind with
                      | `Regular_file -> Ok Regular_file
                      | `Directory -> Ok Directory
                      | kind -> Error kind
                    in
                    begin match kind with
                    | Error kind -> Error (failed (Invalid_root (path, kind)))
                    | Ok kind ->
                        loop
                          (Mentat_workspace.Path.Set.add path seen)
                          ({ path; kind } :: roots) raws
                    end))
  in
  loop Mentat_workspace.Path.Set.empty [] (Input.effective_paths input)

let enumerate_directory workspace_io ~cancelled (root : root) =
  match
    Fs.Glob.Enumeration.paths workspace_io ~cancelled ~root:root.path
      ~pattern:"**/*.ml"
  with
  | Ok paths -> Ok paths
  | Error `Cancelled -> Error Cancelled
  | Error (`File_error error) -> Error (File_error error)
  | Error (`Invalid_pattern message) -> Error (Enumerate message)

let enumerate_candidates workspace_io ~cancelled roots =
  let rec loop seen candidates = function
    | [] -> Ok (List.rev candidates)
    | (root : root) :: roots -> (
        if cancelled () then Error Cancelled
        else
          let files =
            match root.kind with
            | Regular_file -> Ok [ root.path ]
            | Directory -> enumerate_directory workspace_io ~cancelled root
          in
          match files with
          | Error _ as error -> error
          | Ok files ->
              let seen, candidates =
                List.fold_left
                  (fun (seen, candidates) file ->
                    if Mentat_workspace.Path.Set.mem file seen then
                      (seen, candidates)
                    else
                      ( Mentat_workspace.Path.Set.add file seen,
                        file :: candidates ))
                  (seen, candidates) files
              in
              loop seen candidates roots)
  in
  loop Mentat_workspace.Path.Set.empty [] roots

type skipped_reason =
  | Binary
  | Invalid_utf8
  | Too_large
  | Syntax_error of string
  | Read_error of string
  | Unrenderable of string
  | Rewrite_unparsable of string

type skipped = { path : Mentat_workspace.Path.t; reason : skipped_reason }

let skipped_reason_label = function
  | Binary -> "binary"
  | Invalid_utf8 -> "invalid_utf8"
  | Too_large -> "too_large"
  | Syntax_error _ -> "syntax_error"
  | Read_error _ -> "read_error"
  | Unrenderable _ -> "unrenderable"
  | Rewrite_unparsable _ -> "rewrite_unparsable"

module Output = struct
  type status = Applied | Previewed
  type operation = Modify | Unchanged

  type file = {
    file_address : string;
    operation : operation;
    added : int option;
    removed : int option;
  }

  type skipped_file = {
    skipped_address : string;
    skipped_reason : skipped_reason;
  }

  type t = { status : status; files : file list; skipped : skipped_file list }

  let unchanged address =
    {
      file_address = address;
      operation = Unchanged;
      added = Some 0;
      removed = Some 0;
    }

  let modified address change =
    {
      file_address = address;
      operation = Modify;
      added = Mentat_permission.Request.Change.additions change;
      removed = Mentat_permission.Request.Change.removals change;
    }

  let skipped address reason =
    { skipped_address = address; skipped_reason = reason }

  let status_string = function Applied -> "applied" | Previewed -> "previewed"

  let operation_string = function
    | Modify -> "modify"
    | Unchanged -> "unchanged"

  let count_text = function
    | Some count -> string_of_int count
    | None -> "unknown"

  let text output =
    let buffer = Buffer.create 256 in
    Printf.bprintf buffer "%s status=%s files=%d skipped=%d\n" name
      (status_string output.status)
      (List.length output.files)
      (List.length output.skipped);
    (match output.files with
    | [] -> Buffer.add_string buffer "No rewrites\n"
    | files ->
        List.iter
          (fun (file : file) ->
            Printf.bprintf buffer "%s: %s added=%s removed=%s\n"
              (operation_string file.operation)
              file.file_address (count_text file.added)
              (count_text file.removed))
          files);
    (match output.skipped with
    | [] -> ()
    | skipped ->
        Buffer.add_string buffer "skipped:\n";
        List.iter
          (fun (skipped : skipped_file) ->
            Printf.bprintf buffer "  %s reason=%s\n" skipped.skipped_address
              (skipped_reason_label skipped.skipped_reason))
          skipped);
    Buffer.contents buffer

  let encode output =
    let changed =
      List.filter
        (fun (file : file) ->
          match file.operation with Modify -> true | Unchanged -> false)
        output.files
    in
    let sum field =
      List.fold_left
        (fun total file ->
          match (total, field file) with
          | Some total, Some count -> Some (total + count)
          | Some _, None | None, Some _ | None, None -> None)
        (Some 0) changed
    in
    let disposition =
      match output.status with
      | Applied -> Mentat_tools_output.Update.Applied
      | Previewed -> Mentat_tools_output.Update.Previewed
    in
    let semantic =
      Mentat_tools_output.Update.make ~disposition ~files:(List.length changed)
        ~additions:(sum (fun file -> file.added))
        ~deletions:(sum (fun file -> file.removed))
        ~skipped:(List.length output.skipped)
    in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Update.jsont
      ~text:(text output) semantic
end

let read_source workspace_io path =
  match
    Mentat_workspace_io.File.load workspace_io path ~max_bytes:max_source_bytes
  with
  | Error (Mentat_workspace_io.File_error.Too_large _) -> Error Too_large
  | Error error -> Error (Read_error (Fs_error.message error))
  | Ok source ->
      if Text_helpers.looks_binary source then Error Binary
      else if not (String.is_valid_utf_8 source) then Error Invalid_utf8
      else Ok source

type matched = {
  matched_path : Mentat_workspace.Path.t;
  source : string;
  sites : (Syntax.Location.t * Grep.Binding.t list) list;
}

let search_file workspace_io pattern path =
  match read_source workspace_io path with
  | Error reason -> `Skipped { path; reason }
  | Ok source -> (
      let filename = Address.display workspace_io path in
      match Grep.parse_implementation ~filename source with
      | Error error ->
          `Skipped
            { path; reason = Syntax_error (Grep.Parse_error.to_string error) }
      | Ok structure -> (
          match Grep.search_with_bindings pattern ~path structure with
          | [] -> `Searched
          | sites -> `Matched { matched_path = path; source; sites }))

let render_matched workspace_io ~cancelled ~template ~template_ast ~holes
    matched =
  let starts = line_starts matched.source in
  let rec build sites = function
    | [] -> Ok (List.rev sites)
    | binding :: bindings -> (
        if cancelled () then Error `Cancelled
        else
          match
            build_site ~template ~template_ast ~holes matched.source starts
              binding
          with
          | Error message -> Error (`Unrenderable message)
          | Ok site -> build (site :: sites) bindings)
  in
  match build [] matched.sites with
  | Error _ as error -> error
  | Ok sites -> (
      let address = Address.display workspace_io matched.matched_path in
      match verify_file ~cancelled ~address matched.source sites with
      | Error _ as error -> error
      | Ok after -> (
          match
            Mentat_edit.rewrite ~path:matched.matched_path
              ~before:matched.source ~after
          with
          | Error error ->
              Error (`Unrenderable (Mentat_edit.Error.message error))
          | Ok edit -> Ok edit))

let planned_change path input =
  Change_evidence.modify ~path ~before:input.Input.pattern
    ~after:input.Input.template

let summary_file workspace_io input path edit =
  if Mentat_edit.is_empty edit then
    Output.unchanged (Address.display workspace_io path)
  else
    let change = planned_change path input in
    Output.modified (Address.display workspace_io path) change

let output_skipped workspace_io (skipped : skipped) =
  Output.skipped (Address.display workspace_io skipped.path) skipped.reason

let finish status files skipped =
  Mentat_tool.Result.completed ~output:{ Output.status; files; skipped } ()

let run workspace_io input ~cancelled =
  if cancelled () then interrupted ()
  else
    match Grep.Pattern.parse input.Input.pattern with
    | Error error ->
        Mentat_tool.Result.failed `Invalid_input (pattern_error_message error)
    | Ok pattern -> (
        if cancelled () then interrupted ()
        else
          let pattern_metavars = Grep.Pattern.metavariables pattern in
          match validate_template ~pattern_metavars input.Input.template with
          | Error message -> Mentat_tool.Result.failed `Invalid_input message
          | Ok (template, template_ast, holes) -> (
              if cancelled () then interrupted ()
              else
                match resolve_roots workspace_io ~cancelled input with
                | Error result -> result
                | Ok roots -> (
                    match
                      enumerate_candidates workspace_io ~cancelled roots
                    with
                    | Error Cancelled -> interrupted ()
                    | Error
                        ((File_error _ | Invalid_root _ | Enumerate _) as error)
                      ->
                        failed error
                    | Ok candidates ->
                        let rec search matched skipped = function
                          | [] -> `Done (List.rev matched, List.rev skipped)
                          | path :: paths -> (
                              if cancelled () then `Cancelled
                              else
                                let outcome =
                                  search_file workspace_io pattern path
                                in
                                if cancelled () then `Cancelled
                                else
                                  match outcome with
                                  | `Skipped skip ->
                                      search matched (skip :: skipped) paths
                                  | `Searched -> search matched skipped paths
                                  | `Matched matched_file ->
                                      search (matched_file :: matched) skipped
                                        paths)
                        in
                        begin match search [] [] candidates with
                        | `Cancelled -> interrupted ()
                        | `Done (matched, read_skips) ->
                            let matched =
                              List.sort
                                (fun left right ->
                                  Mentat_workspace.Path.compare
                                    left.matched_path right.matched_path)
                                matched
                            in
                            let total_sites =
                              List.fold_left
                                (fun total matched ->
                                  total + List.length matched.sites)
                                0 matched
                            in
                            let limit = Input.effective_max_sites input in
                            if total_sites > limit then
                              Mentat_tool.Result.failed `Failed
                                (Printf.sprintf
                                   "found %d matching site(s), which exceeds \
                                    max_sites=%d; narrow paths or raise \
                                    max_sites (nothing was written)"
                                   total_sites limit)
                            else
                              let rec render files plans skipped = function
                                | [] ->
                                    `Done
                                      ( List.rev files,
                                        List.rev plans,
                                        List.rev skipped )
                                | matched_file :: matched_files -> (
                                    if cancelled () then `Cancelled
                                    else
                                      match
                                        render_matched workspace_io ~cancelled
                                          ~template ~template_ast ~holes
                                          matched_file
                                      with
                                      | Error `Cancelled -> `Cancelled
                                      | Error (`Unrenderable message) ->
                                          render files plans
                                            ({
                                               path = matched_file.matched_path;
                                               reason = Unrenderable message;
                                             }
                                            :: skipped)
                                            matched_files
                                      | Error (`Rewrite_unparsable message) ->
                                          render files plans
                                            ({
                                               path = matched_file.matched_path;
                                               reason =
                                                 Rewrite_unparsable message;
                                             }
                                            :: skipped)
                                            matched_files
                                      | Ok edit ->
                                          let file =
                                            summary_file workspace_io input
                                              matched_file.matched_path edit
                                          in
                                          render (file :: files) (edit :: plans)
                                            skipped matched_files)
                              in
                              begin match render [] [] [] matched with
                              | `Cancelled -> interrupted ()
                              | `Done (files, plans, render_skips) -> (
                                  let skipped =
                                    List.sort
                                      (fun (left : skipped) (right : skipped) ->
                                        Mentat_workspace.Path.compare left.path
                                          right.path)
                                      (read_skips @ render_skips)
                                    |> List.map (output_skipped workspace_io)
                                  in
                                  if cancelled () then interrupted ()
                                  else if input.Input.dry_run then
                                    finish Output.Previewed files skipped
                                  else
                                    match Mentat_edit.concat plans with
                                    | Error error -> Edit_error.failed error
                                    | Ok _ when cancelled () -> interrupted ()
                                    | Ok plan when Mentat_edit.is_empty plan ->
                                        finish Output.Applied files skipped
                                    | Ok plan -> (
                                        match
                                          Mentat_workspace_io.Edit.apply
                                            workspace_io plan
                                        with
                                        | Error error ->
                                            Edit_error.failed_apply error
                                        | Ok _ ->
                                            finish Output.Applied files skipped)
                                  )
                              end
                        end)))

let permissions workspace_io input =
  let request_for path =
    if input.Input.dry_run then
      Mentat_permission.Request.of_accesses ~source:name
        [ Mentat_permission.Access.path ~op:`Read path ]
    else
      let item =
        Mentat_permission.Request.Item.make
          ~change:(planned_change path input)
          (Mentat_permission.Access.path ~op:`Modify path)
      in
      Mentat_permission.Request.make ~source:name [ item ]
  in
  (* First-occurrence-wins dedup on the resolved path: correct only because
     [List.concat_map] applies its callback left to right, so [seen] fills in
     input order. *)
  let seen = ref Mentat_workspace.Path.Set.empty in
  List.concat_map
    (fun raw ->
      Permissions.with_resolved workspace_io raw (fun path ->
          if Mentat_workspace.Path.Set.mem path !seen then []
          else begin
            seen := Mentat_workspace.Path.Set.add path !seen;
            [ request_for path ]
          end))
    (Input.effective_paths input)

let make workspace_io =
  Mentat_tool.make ~name
    ~description:Mentat_prompts.Tools.ocaml_replace_expressions
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io)
    ~run:(fun ~cancelled input -> run workspace_io input ~cancelled)
    ()
