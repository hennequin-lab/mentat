(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let has_duplicates compare values =
  let sorted = List.sort compare values in
  let rec loop = function
    | first :: (second :: _ as rest) -> compare first second = 0 || loop rest
    | [] | [ _ ] -> false
  in
  loop sorted

let plain_label label =
  (not (String.is_empty label)) && not (String.contains label '\000')

module Deps = struct
  type 'a t = Unknown | Known of 'a list

  let unknown = Unknown
  let known values = Known values
end

module Compilation_unit = struct
  type t = {
    name : Module_name.t;
    impl : Mentat_workspace.Path.t option;
    intf : Mentat_workspace.Path.t option;
    interface_deps : Module_name.t Deps.t;
    implementation_deps : Module_name.t Deps.t;
  }

  let check_unique_deps fn field deps =
    match deps with
    | Deps.Unknown -> ()
    | Deps.Known deps ->
        if has_duplicates Module_name.compare deps then
          Import.invalid_arg' "Mentat_ocaml.Project.Compilation_unit" fn
            (field ^ " must not contain duplicates")

  let make ?impl ?intf ?(interface_deps = Deps.Unknown)
      ?(implementation_deps = Deps.Unknown) name =
    check_unique_deps "make" "interface_deps" interface_deps;
    check_unique_deps "make" "implementation_deps" implementation_deps;
    { name; impl; intf; interface_deps; implementation_deps }

  let name t = t.name
  let impl t = t.impl
  let intf t = t.intf
  let interface_deps t = t.interface_deps
  let implementation_deps t = t.implementation_deps
  let pp ppf t = Module_name.pp ppf t.name
end

module Component = struct
  module Id = struct
    type t = string

    let check_name fn field value =
      if not (plain_label value) then
        Import.invalid_arg' "Mentat_ocaml.Project.Component.Id" fn
          (field ^ " must not be empty or contain NUL")

    let library name =
      check_name "library" "name" name;
      "library:" ^ name

    let external_library name =
      check_name "external_library" "name" name;
      "external-library:" ^ name

    let executable ~dir ~name =
      check_name "executable" "name" name;
      Printf.sprintf "executable:%S:%S:%s"
        (Mentat_workspace.Root.Key.to_string
           (Mentat_workspace.Path.root_key dir))
        (Mentat_workspace.Path.display dir)
        name

    let to_string t = t
    let compare = String.compare
    let equal = String.equal
    let pp ppf t = Format.pp_print_string ppf t
  end

  module Kind = struct
    type t = Local_library | External_library | Executable

    let pp ppf = function
      | Local_library -> Format.pp_print_string ppf "library"
      | External_library -> Format.pp_print_string ppf "external-library"
      | Executable -> Format.pp_print_string ppf "executable"
  end

  type t = {
    id : Id.t;
    name : string;
    kind : Kind.t;
    source_dir : Mentat_workspace.Path.t option;
    location : Location.t option;
    units : Compilation_unit.t list;
    requires : Id.t Deps.t;
  }

  let check_requires fn = function
    | Deps.Unknown -> ()
    | Deps.Known requires ->
        if has_duplicates Id.compare requires then
          Import.invalid_arg' "Mentat_ocaml.Project.Component" fn
            "requires must not contain duplicate ids"

  let make ?source_dir ?location ?(units = []) ?(requires = Deps.Unknown) ~id
      ~name ~kind () =
    Import.require_non_empty "Mentat_ocaml.Project.Component" "make" "name" name;
    check_requires "make" requires;
    { id; name; kind; source_dir; location; units; requires }

  let local_library ?source_dir ?location ?units ?requires ~name () =
    make ?source_dir ?location ?units ?requires ~id:(Id.library name) ~name
      ~kind:Kind.Local_library ()

  let external_library ?source_dir ?location ?units ?requires ~name () =
    make ?source_dir ?location ?units ?requires ~id:(Id.external_library name)
      ~name ~kind:Kind.External_library ()

  let executable ~dir ?location ?units ?requires ~name () =
    make ~source_dir:dir ?location ?units ?requires
      ~id:(Id.executable ~dir ~name) ~name ~kind:Kind.Executable ()

  let with_requires requires t =
    check_requires "with_requires" requires;
    { t with requires }

  let id t = t.id
  let name t = t.name
  let kind t = t.kind
  let source_dir t = t.source_dir
  let location t = t.location
  let units t = t.units
  let requires t = t.requires
  let pp ppf t = Format.fprintf ppf "%a %s" Kind.pp t.kind t.name
end

module Test = struct
  type t = {
    component : Component.Id.t option;
    name : string;
    source_dir : Mentat_workspace.Path.t;
    package : string option;
    location : Location.t option;
    target : string;
    enabled : bool;
  }

  let make ?component ?package ?location ~name ~source_dir ~target ~enabled () =
    Import.require_non_empty "Mentat_ocaml.Project.Test" "make" "name" name;
    Option.iter
      (Import.require_non_empty "Mentat_ocaml.Project.Test" "make" "package")
      package;
    Import.require_non_empty "Mentat_ocaml.Project.Test" "make" "target" target;
    { component; name; source_dir; package; location; target; enabled }

  let component t = t.component
  let name t = t.name
  let source_dir t = t.source_dir
  let package t = t.package
  let location t = t.location
  let target t = t.target
  let enabled t = t.enabled
  let pp ppf t = Format.fprintf ppf "%s -> %s" t.name t.target
end

type t = {
  root : Mentat_workspace.Path.t option;
  build_context : string option;
  components : Component.t list;
  tests : Test.t list;
}

let make ?root ?build_context ?(tests = []) components =
  Option.iter
    (Import.require_non_empty "Mentat_ocaml.Project" "make" "build_context")
    build_context;
  let component_ids = List.map Component.id components in
  if has_duplicates Component.Id.compare component_ids then
    Import.invalid_arg' "Mentat_ocaml.Project" "make"
      "components must not contain duplicate ids";
  let has_component id =
    List.exists
      (fun component -> Component.Id.equal id (Component.id component))
      components
  in
  List.iter
    (fun component ->
      match Component.requires component with
      | Deps.Unknown -> ()
      | Deps.Known requires ->
          List.iter
            (fun id ->
              if not (has_component id) then
                Import.invalid_arg' "Mentat_ocaml.Project" "make"
                  "component requires unknown component id")
            requires)
    components;
  List.iter
    (fun test ->
      match Test.component test with
      | None -> ()
      | Some id ->
          if not (has_component id) then
            Import.invalid_arg' "Mentat_ocaml.Project" "make"
              "test references unknown component id")
    tests;
  { root; build_context; components; tests }

let root t = t.root
let build_context t = t.build_context
let components t = t.components
let tests t = t.tests

let component t id =
  List.find_opt
    (fun component -> Component.Id.equal id (Component.id component))
    t.components

let filter_known_components t ids = List.filter_map (component t) ids

let dependencies t id =
  match component t id with
  | None -> None
  | Some component -> (
      match Component.requires component with
      | Deps.Unknown -> Some Deps.Unknown
      | Deps.Known ids -> Some (Deps.Known (filter_known_components t ids)))

let local_components t =
  List.filter
    (fun component ->
      match Component.kind component with
      | Component.Kind.Local_library | Component.Kind.Executable -> true
      | Component.Kind.External_library -> false)
    t.components

let external_components t =
  List.filter
    (fun component ->
      match Component.kind component with
      | Component.Kind.External_library -> true
      | Component.Kind.Local_library | Component.Kind.Executable -> false)
    t.components

let pp ppf t =
  Format.fprintf ppf "%d components, %d tests" (List.length t.components)
    (List.length t.tests)
