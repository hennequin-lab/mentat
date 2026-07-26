(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable workspace observations.

    A notice is the session's own record of a workspace runtime observation — a
    filesystem-watcher batch or a dune build-health change — recorded against
    the turn that saw it through {!Event.Workspace_notice}. It is the minimal,
    session-owned payload the durable journal controls: the workspace runtime's
    live {!Mentat_workspace.Notice.t} carries a producer-side coalescing key and
    rides the ephemeral progress lane, but only its model-visible content —
    severity, source, one-line title, optional multi-line body — is journaled,
    converted at the drain boundary. Keeping the durable core here, rather than
    embedding the workspace type, keeps the journal format independent of the
    workspace library's evolution.

    A notice is an external observation, not a projection of the transcript, so
    storing it does not violate the derived-facts-are-never-stored law, which
    binds only genuinely-derived facts. *)

(** Notice severities. *)
module Severity : sig
  type t = Info | Warning | Error  (** The type for notice severities. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same severity. *)

  val to_string : t -> string
  (** [to_string s] is [s]'s stable lowercase tag: ["info"], ["warning"], or
      ["error"]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf severity] formats [severity] for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps severities to their stable tags, rejecting unknown tags. *)
end

type t
(** A durable, model-visible workspace observation.

    Invariant: [source] and [title] are non-empty, and [body] is non-empty when
    present. *)

val make :
  source:string ->
  severity:Severity.t ->
  title:string ->
  ?body:string ->
  unit ->
  t
(** [make ~source ~severity ~title ?body ()] is a notice from the producer named
    [source] with a one-line [title] and optional multi-line [body] (default:
    none).

    Raises [Invalid_argument] if [source] or [title] is empty, or if [body] is
    present and empty. *)

val source : t -> string
(** [source t] names the producer (for example ["dune"] or ["fswatch"]). *)

val severity : t -> Severity.t
(** [severity t] is the notice's severity. *)

val title : t -> string
(** [title t] is the one-line summary. *)

val body : t -> string option
(** [body t] is the optional detail text. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] agree on every field. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf notice] formats [notice] for diagnostics. The output is not stable
    storage syntax. *)

val jsont : t Jsont.t
(** [jsont] maps notices to the strict JSON object with string members
    ["source"], ["severity"] (["info"], ["warning"], or ["error"]), ["title"],
    and the optional string member ["body"] (omitted when absent). Decoding
    rejects missing required members, unknown members, unknown severities, and
    any value violating {!make}'s non-empty invariants. *)
