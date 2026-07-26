(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Executable tool definitions — internal implementation.

    The facade {!Mentat_tool} owns the public contract of {!make},
    {!make_staged}, and {!declaration}; this module exposes the definition
    variant concretely so {!Call} can decode and run a tool's callbacks. The
    facade keeps its [t] transparently equal to this type but does not re-export
    this module, so the constructors below are unreachable to a public caller.
*)

(** The type for executable tool definitions. *)
type t =
  | Tool : {
      declaration : Mentat_llm.Tool.t;
      input : 'input Input.t;
      output : 'output Output.encoder;
      permissions : 'input -> Mentat_permission.Request.t list;
      run : cancelled:(unit -> bool) -> 'input -> 'output Result.t;
    }
      -> t  (** An ordinary tool: one permission-guarded callback. *)
  | Staged : {
      declaration : Mentat_llm.Tool.t;
      input : 'input Input.t;
      prepared : 'prepared Jsont.t;
      describe : 'prepared -> string;
      output : 'output Output.encoder;
      prepare_permissions : 'input -> Mentat_permission.Request.t list;
      prepare :
        cancelled:(unit -> bool) ->
        'input ->
        [ `Finished of 'output Result.t | `Prepared of 'prepared ];
      permissions : 'prepared -> Mentat_permission.Request.t list;
      run : cancelled:(unit -> bool) -> 'prepared -> 'output Result.t;
    }
      -> t
      (** A staged tool: an inspect-guarded preparation, then a callback guarded
          by the final permissions its plan needs. *)

val make :
  name:string ->
  description:string ->
  input:'input Input.t ->
  output:'output Output.encoder ->
  ?permissions:('input -> Mentat_permission.Request.t list) ->
  run:(cancelled:(unit -> bool) -> 'input -> 'output Result.t) ->
  unit ->
  t
(** See {!Mentat_tool.make}. *)

val make_staged :
  name:string ->
  description:string ->
  input:'input Input.t ->
  prepared:'prepared Jsont.t ->
  describe:('prepared -> string) ->
  output:'output Output.encoder ->
  ?prepare_permissions:('input -> Mentat_permission.Request.t list) ->
  prepare:
    (cancelled:(unit -> bool) ->
    'input ->
    [ `Finished of 'output Result.t | `Prepared of 'prepared ]) ->
  ?permissions:('prepared -> Mentat_permission.Request.t list) ->
  run:(cancelled:(unit -> bool) -> 'prepared -> 'output Result.t) ->
  unit ->
  t
(** See {!Mentat_tool.make_staged}. *)

val declaration : t -> Mentat_llm.Tool.t
(** [declaration t] is [t]'s model-visible declaration and durable identity. *)
