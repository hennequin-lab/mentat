(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** A user-invoked command as a frontend sees it for completion.

    A custom command is a markdown prompt template a user invokes as [/name].
    This value is the thin, serializable summary a frontend renders in its [/]
    completion menu; the command body and its expansion stay server-side and
    never cross this boundary. A frontend that cannot link the discovery library
    ([mentat.tui]) consumes commands only through this type. *)

module Name : sig
  (** A validated command name.

      A name is the [:]-joined path a command is invoked by; a namespaced name
      like [git:commit] comes from [git/commit.md]. Validation on the wire
      mirrors the discovery library's grammar so a malformed name is
      unrepresentable here, keeping the boundary free of free-string input. *)

  type t
  (** A command name: one or more [:]-joined segments, each 1 to 64 bytes of
      lowercase ASCII letters, digits, and hyphens with a leading letter or
      digit. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is [Ok name] if [s] is a non-empty [:]-joined sequence of
      well-formed segments, and [Error message] otherwise. *)

  val to_string : t -> string
  (** [to_string name] is [name]'s [:]-joined bytes. *)

  val equal : t -> t -> bool
  (** [equal a b] is byte equality of {!to_string}. *)

  val compare : t -> t -> int
  (** [compare a b] is the total byte order of {!to_string}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf name] prints [name]'s bytes. *)

  val jsont : t Jsont.t
  (** [jsont] maps a name to and from its [:]-joined JSON string, rejecting a
      malformed name on decode. *)
end

(** The presentational origin of a command. The compat roots ([.agents],
    [.claude]) fold into {!Project}. *)
type scope = Project | User

type t = {
  name : Name.t;  (** [git:commit]. *)
  description : string option;  (** Shown in completion, if present. *)
  argument_hint : string option;
      (** Shown after the name in completion, if present. *)
  scope : scope;  (** Presentational origin. *)
}
(** A completion summary: only what the palette renders. *)

val jsont : t Jsont.t
(** [jsont] maps a summary to and from a JSON object with [name], [scope], and
    optional [description] and [argument_hint], rejecting unknown members and a
    malformed name or scope. *)
