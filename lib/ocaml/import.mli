(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** House helpers — the codec-free subset of [lib/*/import.mli].

    The other copies also carry jsont decode helpers. This library has no jsont
    dependency, so only the argument-validation pair lives here; the smart
    constructors across the value model raise through it so every
    [Invalid_argument] diagnostic names the same [module.fn: message] shape. *)

val invalid_arg' : string -> string -> string -> 'a
(** [invalid_arg' module_path fn message] raises [Invalid_argument] with a
    ["module_path.fn: message"] diagnostic. *)

val require_non_empty : string -> string -> string -> string -> unit
(** [require_non_empty module_path fn field value] raises [Invalid_argument]
    through {!invalid_arg'} when [value] is empty, naming [field]. *)
