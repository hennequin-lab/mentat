(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Normalized project descriptions for agent-facing tools.

    A {!type:t} groups the project's {!Component.t} nodes and {!Test.t} entries.
    Build them with {!make}, which validates that component dependencies and
    tests refer to known components. *)

module Deps : sig
  (** Optionally-computed dependency information. *)

  type 'a t =
    | Unknown
    | Known of 'a list
        (** Dependency information that may not have been requested from the
            backend.

            [Unknown] means the producer did not compute this dependency set.
            [Known []] means it computed the set and found no dependencies. *)

  val unknown : 'a t
  (** [unknown] is [Unknown], the absence of computed dependency information. *)

  val known : 'a list -> 'a t
  (** [known values] is [Known values]. *)
end

module Compilation_unit : sig
  (** OCaml compilation units belonging to a project component. *)

  type t
  (** An OCaml compilation unit that belongs to a project component. *)

  val make :
    ?impl:Mentat_workspace.Path.t ->
    ?intf:Mentat_workspace.Path.t ->
    ?interface_deps:Module_name.t Deps.t ->
    ?implementation_deps:Module_name.t Deps.t ->
    Module_name.t ->
    t
  (** [make ... name] is a compilation unit.

      [impl] and [intf] are source files when Dune reports them inside the
      workspace. Dependency names are direct OCaml module dependencies as
      reported by the backend.

      Dependencies default to {!Deps.Unknown}. Raises [Invalid_argument] if a
      known interface or implementation dependency list contains duplicate
      module names. *)

  val name : t -> Module_name.t
  (** [name t] is [t]'s module name. *)

  val impl : t -> Mentat_workspace.Path.t option
  (** [impl t] is the path to [t]'s implementation file, or [None] when Dune did
      not report one inside the workspace. *)

  val intf : t -> Mentat_workspace.Path.t option
  (** [intf t] is the path to [t]'s interface file, or [None] when Dune did not
      report one inside the workspace. *)

  val interface_deps : t -> Module_name.t Deps.t
  (** [interface_deps t] is the direct module dependencies of [t]'s interface.
  *)

  val implementation_deps : t -> Module_name.t Deps.t
  (** [implementation_deps t] is the direct module dependencies of [t]'s
      implementation. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as its module name. *)
end

module Component : sig
  (** Project component nodes: local libraries, executables, and external
      dependencies. *)

  module Id : sig
    (** Component identities. *)

    type t
    (** Stable component identity inside one project description. *)

    val library : string -> t
    (** [library name] is the id for a local library.

        Raises [Invalid_argument] if [name] is empty or contains NUL. *)

    val external_library : string -> t
    (** [external_library name] is the id for an external library.

        Raises [Invalid_argument] if [name] is empty or contains NUL. *)

    val executable : dir:Mentat_workspace.Path.t -> name:string -> t
    (** [executable ~dir ~name] is the id for an executable declared in [dir].

        Raises [Invalid_argument] if [name] is empty or contains NUL. *)

    val to_string : t -> string
    (** [to_string t] is [t]'s stable string form. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] formats [t] as its string form. *)
  end

  module Kind : sig
    (** Component kinds. *)

    type t =
      | Local_library  (** A library defined in the workspace. *)
      | External_library  (** A library outside the workspace. *)
      | Executable  (** An executable defined in the workspace. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] formats [t] as ["library"], ["external-library"], or
        ["executable"]. *)
  end

  type t
  (** A library, executable, or external dependency node.

      Components should be built with {!local_library}, {!external_library}, or
      {!executable} so the id, kind, and source directory agree. [requires]
      stores component ids. Adapters are responsible for resolving Dune-specific
      ids, digests, and external library names into this project-local id space
      before constructing the final {!type:Project.t}. *)

  val local_library :
    ?source_dir:Mentat_workspace.Path.t ->
    ?location:Location.t ->
    ?units:Compilation_unit.t list ->
    ?requires:Id.t Deps.t ->
    name:string ->
    unit ->
    t
  (** [local_library ?source_dir ?location ?units ?requires ~name ()] is a local
      library component named [name]. [source_dir] is the workspace directory
      containing the library stanza, when known.

      [requires] defaults to {!Deps.Unknown}. Raises [Invalid_argument] if
      [name] is empty, [name] contains NUL, or [requires] contains duplicate
      ids. *)

  val external_library :
    ?source_dir:Mentat_workspace.Path.t ->
    ?location:Location.t ->
    ?units:Compilation_unit.t list ->
    ?requires:Id.t Deps.t ->
    name:string ->
    unit ->
    t
  (** [external_library ?source_dir ?location ?units ?requires ~name ()] is an
      external library component named [name].

      [source_dir] is present only when the producer can map the external
      library's source directory into the current workspace. [requires] defaults
      to {!Deps.Unknown}. Raises [Invalid_argument] if [name] is empty, [name]
      contains NUL, or [requires] contains duplicate ids. *)

  val executable :
    dir:Mentat_workspace.Path.t ->
    ?location:Location.t ->
    ?units:Compilation_unit.t list ->
    ?requires:Id.t Deps.t ->
    name:string ->
    unit ->
    t
  (** [executable ~dir ?location ?units ?requires ~name ()] is an executable
      component named [name] declared in [dir]. [dir] is part of the
      executable's stable component id.

      [requires] defaults to {!Deps.Unknown}. Raises [Invalid_argument] if
      [name] is empty, [name] contains NUL, or [requires] contains duplicate
      ids. *)

  val with_requires : Id.t Deps.t -> t -> t
  (** [with_requires requires t] is [t] with [requires].

      Raises [Invalid_argument] if [requires] contains duplicate ids. *)

  val id : t -> Id.t
  (** [id t] is [t]'s stable component id. *)

  val name : t -> string
  (** [name t] is [t]'s name. *)

  val kind : t -> Kind.t
  (** [kind t] is [t]'s kind. *)

  val source_dir : t -> Mentat_workspace.Path.t option
  (** [source_dir t] is the workspace directory of [t]'s stanza, or [None] when
      it is unknown. *)

  val location : t -> Location.t option
  (** [location t] is [t]'s declaration location, or [None] when unknown. *)

  val units : t -> Compilation_unit.t list
  (** [units t] is [t]'s compilation units. *)

  val requires : t -> Id.t Deps.t
  (** [requires t] is [t]'s direct component dependencies, as component ids. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as its kind followed by its name. *)
end

module Test : sig
  (** Runnable test entries reported by the build system. *)

  type t
  (** A runnable test entry reported by the build system. *)

  val make :
    ?component:Component.Id.t ->
    ?package:string ->
    ?location:Location.t ->
    name:string ->
    source_dir:Mentat_workspace.Path.t ->
    target:string ->
    enabled:bool ->
    unit ->
    t
  (** [make ...] is a test description.

      [target] is the Dune target or alias to run. [component] is the project
      component this test exercises when known. Raises [Invalid_argument] if
      [name], [package], or [target] is empty. *)

  val component : t -> Component.Id.t option
  (** [component t] is the id of the component this test exercises, or [None]
      when unknown. *)

  val name : t -> string
  (** [name t] is [t]'s name. *)

  val source_dir : t -> Mentat_workspace.Path.t
  (** [source_dir t] is the workspace directory [t] runs in. *)

  val package : t -> string option
  (** [package t] is [t]'s package, or [None] when it has none. *)

  val location : t -> Location.t option
  (** [location t] is [t]'s declaration location, or [None] when unknown. *)

  val target : t -> string
  (** [target t] is the Dune target or alias that runs [t]. *)

  val enabled : t -> bool
  (** [enabled t] is [true] iff [t] is enabled in the current configuration. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] as ["name -> target"]. *)
end

type t
(** A normalized project description for agent-facing tools. *)

val make :
  ?root:Mentat_workspace.Path.t ->
  ?build_context:string ->
  ?tests:Test.t list ->
  Component.t list ->
  t
(** [make components] is a project description.

    [build_context] is the Dune build context name or path. Raises
    [Invalid_argument] if [build_context] is empty when present, or if
    [components] contains duplicate ids, any component dependency refers to a
    missing component id, or any test refers to a missing component id. *)

val root : t -> Mentat_workspace.Path.t option
(** [root t] is the workspace root of the project, or [None] when unknown. *)

val build_context : t -> string option
(** [build_context t] is the Dune build context name or path, or [None] when
    unknown. *)

val components : t -> Component.t list
(** [components t] is all of the project's components. *)

val tests : t -> Test.t list
(** [tests t] is all of the project's tests. *)

val component : t -> Component.Id.t -> Component.t option
(** [component t id] is [Some c] if [c] is the component with id [id] in [t],
    and [None] otherwise. *)

val dependencies : t -> Component.Id.t -> Component.t Deps.t option
(** [dependencies t id] is [Some deps] if [id] is a component in [t] and [None]
    otherwise.

    [Some Deps.Unknown] means the producer did not compute direct dependencies
    for [id]. [Some (Deps.Known [])] means the producer computed the direct
    dependencies and found none. *)

val local_components : t -> Component.t list
(** [local_components t] is the project's local libraries and executables. *)

val external_components : t -> Component.t list
(** [external_components t] is the project's external-library components. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] as a count of its components and tests. *)
