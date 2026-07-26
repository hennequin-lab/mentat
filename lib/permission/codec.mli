(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Library-private structural-enum codec shared by {!Access} and {!Policy}.

    The [kind], [path_op], and [network_protocol] enums are owned by {!Access};
    {!Policy}'s matchers mirror them. This module holds the single stable_text
    and {!Jsont} rendering of those enums so the persisted access form and the
    matcher rule form cannot disagree about how an enum encodes. Not part of the
    public {!Mentat_permission} surface. Depends only on {!Import} and {!Jsont},
    keeping a clean nominal DAG below {!Access}. Enum validity (a non-empty
    [`Other] protocol name) is enforced by the per-module construction
    validators ([Access.network], [Rule.network_host]) that run on every decode
    path; this codec only translates the vocabulary. *)

val stable_field : string -> string
(** [stable_field s] is [s] length-framed as ["<len>:<s>"], the digest-input
    primitive for embedding arbitrary text unambiguously. *)

val stable_option : ('a -> string) -> 'a option -> string
(** [stable_option f o] is ["none"] or ["some:" ^ f v]. *)

val stable_kind : [ `Read | `Write | `Command | `Network | `Custom ] -> string
(** [stable_kind k] is [k]'s canonical digest-input spelling. *)

val stable_path_op : [ `Read | `Create | `Modify | `Delete ] -> string
(** [stable_path_op op] is [op]'s canonical digest-input spelling. *)

val stable_protocol :
  [ `Http | `Https | `Ssh | `Tcp | `Udp | `Other of string ] -> string
(** [stable_protocol p] is [p]'s canonical digest-input spelling; [`Other name]
    frames [name] with {!stable_field}. *)

val kind_jsont : [ `Read | `Write | `Command | `Network | `Custom ] Jsont.t
(** [kind_jsont] maps an access kind to and from its lowercase JSON string. *)

val path_op_jsont : [ `Read | `Create | `Modify | `Delete ] Jsont.t
(** [path_op_jsont] maps a path operation to and from its lowercase JSON string.
*)

val network_protocol_jsont :
  [ `Http | `Https | `Ssh | `Tcp | `Udp | `Other of string ] Jsont.t
(** [network_protocol_jsont] encodes the built-in protocols as lowercase strings
    and [`Other name] as the object [{ "type": "other"; "name" }]. A non-empty
    [`Other] name is enforced by the construction validators, not here. *)
