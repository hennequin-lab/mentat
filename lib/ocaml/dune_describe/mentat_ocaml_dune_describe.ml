(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Error = Error
module Workspace = Mentat_workspace

let ( let* ) = Result.bind

type prepare =
  cwd:Lpath.Abs.t ->
  argv:string list ->
  (string list * string array, string) result

type component_acc = {
  component : Mentat_ocaml.Project.Component.t;
  uid : string option;
  requires : string list option;
}

let workspace_args ?(with_deps = true) ?(recursive = true) () =
  let args = [ "dune"; "describe"; "workspace"; "--root"; "." ] in
  let args = if with_deps then args @ [ "--with-deps" ] else args in
  if recursive then args else args @ [ "--no-recursive" ]

let tests_args ?context () =
  let args = [ "dune"; "describe"; "tests"; "--root"; "." ] in
  match context with None -> args | Some context -> args @ [ context ]

let path_error path error =
  Error (Error.Path_error { path; message = Lpath.Error.message error })

let workspace_error path error =
  Error
    (Error.Path_error { path; message = Workspace.Resolve_error.message error })

let parse_error source message =
  Error (Error.Parse_error { source; offset = None; message })

let construct ~source f =
  try Ok (f ()) with Invalid_argument message -> parse_error source message

let module_name ~source name =
  construct ~source (fun () -> Mentat_ocaml.Module_name.make name)

let path_of_string workspace text =
  if String.equal text "" then
    Error (Error.Path_error { path = text; message = "path must not be empty" })
  else if String.starts_with ~prefix:"/" text then
    match Lpath.Abs.of_string text with
    | Error error -> path_error text error
    | Ok abs -> (
        match Workspace.import_abs workspace abs with
        | Error error -> workspace_error text error
        | Ok path -> Ok path)
  else
    match Lpath.Rel.of_string text with
    | Error error -> path_error text error
    | Ok rel -> Ok (Workspace.Path.append (Workspace.root_path workspace) rel)

let workspace_path_of_string workspace text =
  if String.equal text "" then
    Error (Error.Path_error { path = text; message = "path must not be empty" })
  else if String.starts_with ~prefix:"/" text then
    match Lpath.Abs.of_string text with
    | Error error -> path_error text error
    | Ok abs -> (
        match Workspace.import_abs workspace abs with
        | Ok path -> Ok (Some path)
        | Error (Workspace.Resolve_error.Outside_workspace _) -> Ok None
        | Error error -> workspace_error text error)
  else Result.map Option.some (path_of_string workspace text)

let opt_path ?(external_paths = false) workspace = function
  | Sexp.List [] -> Ok None
  | Sexp.List [ Sexp.Atom path ] ->
      if external_paths then workspace_path_of_string workspace path
      else Result.map Option.some (path_of_string workspace path)
  | sexp -> (
      match Sexp.atom sexp with
      | None ->
          Error
            (Error.Parse_error
               {
                 source = Error.Workspace_describe;
                 offset = None;
                 message = "expected path atom or empty option";
               })
      | Some path ->
          if external_paths then workspace_path_of_string workspace path
          else Result.map Option.some (path_of_string workspace path))

let record_fields ~source = function
  | Sexp.List fields ->
      let field = function
        | Sexp.List [ Sexp.Atom name; value ] -> Ok (name, value)
        | other ->
            Error
              (Error.Parse_error
                 {
                   source;
                   offset = None;
                   message =
                     "expected record field, got " ^ Sexp.to_string other;
                 })
      in
      let rec loop names acc = function
        | [] -> Ok (List.rev acc)
        | sexp :: fields -> (
            match field sexp with
            | Error _ as error -> error
            | Ok ((name, _) as field) ->
                if List.exists (String.equal name) names then
                  Error
                    (Error.Parse_error
                       {
                         source;
                         offset = None;
                         message = "duplicate record field " ^ name;
                       })
                else loop (name :: names) (field :: acc) fields)
      in
      loop [] [] fields
  | other ->
      Error
        (Error.Parse_error
           {
             source;
             offset = None;
             message = "expected record, got " ^ Sexp.to_string other;
           })

let field fields name = List.assoc_opt name fields

let required ~source fields name =
  match field fields name with
  | Some value -> Ok value
  | None ->
      Error
        (Error.Parse_error
           { source; offset = None; message = "missing field " ^ name })

let atom_field ~source fields name =
  let* value = required ~source fields name in
  match Sexp.atom value with
  | Some value -> Ok value
  | None ->
      Error
        (Error.Parse_error
           {
             source;
             offset = None;
             message = "field " ^ name ^ " must be an atom";
           })

let bool_field ~source fields name =
  let* value = atom_field ~source fields name in
  match value with
  | "true" -> Ok true
  | "false" -> Ok false
  | _ ->
      Error
        (Error.Parse_error
           {
             source;
             offset = None;
             message = "field " ^ name ^ " must be a bool";
           })

let atoms_field ~source fields name =
  let* value = required ~source fields name in
  match Sexp.list value with
  | Some values ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | value :: values -> (
            match Sexp.atom value with
            | Some value -> loop (value :: acc) values
            | None ->
                Error
                  (Error.Parse_error
                     {
                       source;
                       offset = None;
                       message = "field " ^ name ^ " must be an atom list";
                     }))
      in
      loop [] values
  | None ->
      Error
        (Error.Parse_error
           {
             source;
             offset = None;
             message = "field " ^ name ^ " must be a list";
           })

let deps_field ~source fields name make =
  match field fields name with
  | None -> Ok Mentat_ocaml.Project.Deps.Unknown
  | Some value -> (
      match Sexp.list value with
      | None ->
          Error
            (Error.Parse_error
               {
                 source;
                 offset = None;
                 message = "field " ^ name ^ " must be a list";
               })
      | Some values ->
          let rec loop acc = function
            | [] -> Ok (Mentat_ocaml.Project.Deps.Known (List.rev acc))
            | value :: values -> (
                match Sexp.atom value with
                | Some value ->
                    let* value = make ~source value in
                    loop (value :: acc) values
                | None ->
                    Error
                      (Error.Parse_error
                         {
                           source;
                           offset = None;
                           message = "field " ^ name ^ " must be an atom list";
                         }))
          in
          loop [] values)

let module_deps fields =
  match field fields "module_deps" with
  | None ->
      Ok (Mentat_ocaml.Project.Deps.Unknown, Mentat_ocaml.Project.Deps.Unknown)
  | Some sexp ->
      let* fields = record_fields ~source:Error.Workspace_describe sexp in
      let* for_intf =
        deps_field ~source:Error.Workspace_describe fields "for_intf"
          module_name
      in
      let* for_impl =
        deps_field ~source:Error.Workspace_describe fields "for_impl"
          module_name
      in
      Ok (for_intf, for_impl)

let compilation_unit ?(external_paths = false) workspace sexp =
  let* fields = record_fields ~source:Error.Workspace_describe sexp in
  let* name = atom_field ~source:Error.Workspace_describe fields "name" in
  let* impl = required ~source:Error.Workspace_describe fields "impl" in
  let* intf = required ~source:Error.Workspace_describe fields "intf" in
  let* impl = opt_path ~external_paths workspace impl in
  let* intf = opt_path ~external_paths workspace intf in
  let* interface_deps, implementation_deps = module_deps fields in
  let* name = module_name ~source:Error.Workspace_describe name in
  construct ~source:Error.Workspace_describe (fun () ->
      Mentat_ocaml.Project.Compilation_unit.make ?impl ?intf ~interface_deps
        ~implementation_deps name)

let compilation_units ?(external_paths = false) workspace fields =
  let* modules = required ~source:Error.Workspace_describe fields "modules" in
  match Sexp.list modules with
  | None ->
      Error
        (Error.Parse_error
           {
             source = Error.Workspace_describe;
             offset = None;
             message = "modules must be a list";
           })
  | Some modules ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | sexp :: rest -> (
            match compilation_unit ~external_paths workspace sexp with
            | Ok unit_ -> loop (unit_ :: acc) rest
            | Error _ as error -> error)
      in
      loop [] modules

let library_component workspace fields =
  let* name = atom_field ~source:Error.Workspace_describe fields "name" in
  let* uid = atom_field ~source:Error.Workspace_describe fields "uid" in
  let* local = bool_field ~source:Error.Workspace_describe fields "local" in
  let* source_dir =
    atom_field ~source:Error.Workspace_describe fields "source_dir"
  in
  let* source_dir, units =
    if local then
      let* source_dir = path_of_string workspace source_dir in
      let* units = compilation_units workspace fields in
      Ok (Some source_dir, units)
    else
      let* source_dir = workspace_path_of_string workspace source_dir in
      let* units = compilation_units ~external_paths:true workspace fields in
      Ok (source_dir, units)
  in
  let* requires =
    atoms_field ~source:Error.Workspace_describe fields "requires"
  in
  let* component =
    construct ~source:Error.Workspace_describe (fun () ->
        if local then
          Mentat_ocaml.Project.Component.local_library ?source_dir ~name ~units
            ()
        else
          Mentat_ocaml.Project.Component.external_library ?source_dir ~name
            ~units ())
  in
  Ok { component; uid = Some uid; requires = Some requires }

let executable_components workspace fields =
  let* names = atoms_field ~source:Error.Workspace_describe fields "names" in
  let* requires =
    atoms_field ~source:Error.Workspace_describe fields "requires"
  in
  let* units = compilation_units workspace fields in
  let source_dir =
    List.find_map
      (fun unit_ ->
        match Mentat_ocaml.Project.Compilation_unit.impl unit_ with
        | Some path -> Workspace.Path.parent path
        | None ->
            Option.bind
              (Mentat_ocaml.Project.Compilation_unit.intf unit_)
              Workspace.Path.parent)
      units
  in
  let source_dir = Option.value source_dir ~default:(Workspace.cwd workspace) in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | name :: names ->
        let* component =
          construct ~source:Error.Workspace_describe (fun () ->
              Mentat_ocaml.Project.Component.executable ~dir:source_dir ~name
                ~units ())
        in
        loop ({ component; uid = None; requires = Some requires } :: acc) names
  in
  loop [] names

let item workspace = function
  | Sexp.List [ Sexp.Atom "root"; Sexp.Atom root ] ->
      let* root = path_of_string workspace root in
      Ok (`Root root)
  | Sexp.List [ Sexp.Atom "build_context"; Sexp.Atom build_context ] ->
      Ok (`Build_context build_context)
  | Sexp.List [ Sexp.Atom "library"; record ] ->
      let* fields = record_fields ~source:Error.Workspace_describe record in
      Result.map
        (fun component -> `Components [ component ])
        (library_component workspace fields)
  | Sexp.List [ Sexp.Atom "executables"; record ] ->
      let* fields = record_fields ~source:Error.Workspace_describe record in
      Result.map
        (fun components -> `Components components)
        (executable_components workspace fields)
  | sexp ->
      Error
        (Error.Parse_error
           {
             source = Error.Workspace_describe;
             offset = None;
             message = "unknown workspace item " ^ Sexp.to_string sexp;
           })

let resolve_requires components =
  let uid_table = Hashtbl.create 17 in
  let add_uid acc =
    match acc.uid with
    | None -> Ok ()
    | Some uid -> (
        match Hashtbl.find_opt uid_table uid with
        | None ->
            Hashtbl.add uid_table uid
              (Mentat_ocaml.Project.Component.id acc.component);
            Ok ()
        | Some _ -> Error (Error.Duplicate_library_uid uid))
  in
  let rec add_all = function
    | [] -> Ok ()
    | acc :: rest -> (
        match add_uid acc with
        | Ok () -> add_all rest
        | Error _ as error -> error)
  in
  let resolve uid =
    match Hashtbl.find_opt uid_table uid with
    | Some id -> Ok id
    | None -> Error (Error.Unknown_library_uid uid)
  in
  let rec map_requires acc = function
    | [] -> Ok (List.rev acc)
    | uid :: uids -> (
        match resolve uid with
        | Ok id -> map_requires (id :: acc) uids
        | Error _ as error -> error)
  in
  let component acc =
    match acc.requires with
    | None -> Ok acc.component
    | Some uids ->
        let* requires = map_requires [] uids in
        construct ~source:Error.Workspace_describe (fun () ->
            Mentat_ocaml.Project.Component.with_requires
              (Mentat_ocaml.Project.Deps.Known requires) acc.component)
  in
  let rec components_loop acc = function
    | [] -> Ok (List.rev acc)
    | item :: items -> (
        match component item with
        | Ok component -> components_loop (component :: acc) items
        | Error _ as error -> error)
  in
  let* () = add_all components in
  components_loop [] components

let of_workspace_output ~workspace output =
  let* sexp = Sexp.parse ~source:Error.Workspace_describe output in
  let values =
    match sexp with
    | Sexp.List values -> Ok values
    | Sexp.Atom _ ->
        Error
          (Error.Parse_error
             {
               source = Error.Workspace_describe;
               offset = None;
               message = "expected item list";
             })
  in
  let* values = values in
  let rec loop root build_context components = function
    | [] ->
        let* components = resolve_requires (List.rev components) in
        construct ~source:Error.Workspace_describe (fun () ->
            Mentat_ocaml.Project.make ?root ?build_context components)
    | sexp :: rest -> (
        match item workspace sexp with
        | Error _ as error -> error
        | Ok (`Root parsed_root) -> (
            match root with
            | None -> loop (Some parsed_root) build_context components rest
            | Some _ ->
                parse_error Error.Workspace_describe
                  "duplicate workspace item root")
        | Ok (`Build_context parsed_build_context) -> (
            match build_context with
            | None -> loop root (Some parsed_build_context) components rest
            | Some _ ->
                parse_error Error.Workspace_describe
                  "duplicate workspace item build_context")
        | Ok (`Components new_components) ->
            loop root build_context
              (List.rev_append new_components components)
              rest)
  in
  loop None None [] values

let location_of_string workspace text =
  let split =
    match String.rindex_opt text ':' with
    | None -> None
    | Some col_sep -> (
        match String.rindex_from_opt text (col_sep - 1) ':' with
        | None -> None
        | Some line_sep -> Some (line_sep, col_sep))
  in
  match split with
  | None -> None
  | Some (line_sep, col_sep) -> (
      let path = String.sub text 0 line_sep in
      let line =
        String.sub text (line_sep + 1) (col_sep - line_sep - 1)
        |> int_of_string_opt
      in
      let column = String.drop_first (col_sep + 1) text |> int_of_string_opt in
      match (line, column, path_of_string workspace path) with
      | Some line, Some column, Ok path ->
          begin try
            let position = Mentat_ocaml.Position.make ~line ~column in
            Some
              (Mentat_ocaml.Location.make ~path
                 ~range:(Mentat_ocaml.Range.point position))
          with Invalid_argument _ -> None
          end
      | _ -> None)

let component_for_test project source_dir =
  Mentat_ocaml.Project.components project
  |> List.find_opt (fun component ->
      match Mentat_ocaml.Project.Component.source_dir component with
      | None -> false
      | Some dir -> Workspace.Path.equal dir source_dir)
  |> Option.map Mentat_ocaml.Project.Component.id

let test_of_sexp workspace project sexp =
  let* fields = record_fields ~source:Error.Tests_describe sexp in
  let* name = atom_field ~source:Error.Tests_describe fields "name" in
  let* source_dir =
    atom_field ~source:Error.Tests_describe fields "source_dir"
  in
  let* target = atom_field ~source:Error.Tests_describe fields "target" in
  let* enabled = bool_field ~source:Error.Tests_describe fields "enabled" in
  let* source_dir = path_of_string workspace source_dir in
  let package =
    match field fields "package" with
    | Some (Sexp.Atom package) -> Some package
    | Some (Sexp.List []) | None -> None
    | Some _ -> None
  in
  let location =
    match field fields "location" with
    | Some (Sexp.Atom location) -> location_of_string workspace location
    | Some _ | None -> None
  in
  let component = component_for_test project source_dir in
  construct ~source:Error.Tests_describe (fun () ->
      Mentat_ocaml.Project.Test.make ?component ?package ?location ~name
        ~source_dir ~target ~enabled ())

let of_tests_output ~workspace project output =
  let* sexp = Sexp.parse ~source:Error.Tests_describe output in
  let tests =
    match sexp with
    | Sexp.List values ->
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | value :: values -> (
              match test_of_sexp workspace project value with
              | Ok test -> loop (test :: acc) values
              | Error _ as error -> error)
        in
        loop [] values
    | Sexp.Atom _ ->
        Error
          (Error.Parse_error
             {
               source = Error.Tests_describe;
               offset = None;
               message = "expected test list";
             })
  in
  let* tests = tests in
  construct ~source:Error.Tests_describe (fun () ->
      Mentat_ocaml.Project.make
        ?root:(Mentat_ocaml.Project.root project)
        ?build_context:(Mentat_ocaml.Project.build_context project)
        ~tests
        (Mentat_ocaml.Project.components project))

let of_outputs ~workspace ~workspace_output ~tests_output =
  let* project = of_workspace_output ~workspace workspace_output in
  of_tests_output ~workspace project tests_output
