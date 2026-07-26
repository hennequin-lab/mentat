(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** A minimal, escaping-by-construction HTML combinator surface.

    This module is the entire cross-site-scripting boundary of {!Mentat_web}.
    Every byte of model output, tool output, or file content reaches a rendered
    page only through {!El.txt}, {!At.v}, or {!At.data}, each of which
    HTML-escapes the five characters ampersand, less-than, greater-than,
    double-quote, and apostrophe by construction. The one deliberate hole,
    {!El.unsafe_raw}, is reserved for the static page chrome an author writes; a
    fact payload must never reach it.

    We build this surface rather than depend on [htmlit] or [tyxml] to keep the
    escaping under direct review and off the lock file — the inventory needs
    only the small combinator set below. *)

type t
(** The type for a rendered HTML node. *)

(** {1:attributes Attributes} *)

module At : sig
  (** HTML element attributes. *)

  type t
  (** The type for one attribute. *)

  val v : string -> string -> t
  (** [v name value] is the attribute [name="value"]. [value] is HTML-escaped by
      construction, so it is a safe sink for untrusted bytes that name a value
      rather than markup. *)

  val bool : string -> bool -> t
  (** [bool name present] is the boolean attribute [name] when [present] is
      [true], and a no-op attribute when [false]. A boolean attribute renders
      with no value. *)

  val id : string -> t
  (** [id s] is [id="s"]. *)

  val class_ : string -> t
  (** [class_ s] is [class="s"]. *)

  val href : string -> t
  (** [href s] is [href="s"]. The value is escaped, but escaping does not
      neutralise a dangerous URL scheme (for example [javascript:]); scheme
      safety is the caller's concern (see {!Mentat_web.Render}). *)

  val type' : string -> t
  (** [type' s] is [type="s"]. *)

  val name : string -> t
  (** [name s] is [name="s"]. *)

  val value : string -> t
  (** [value s] is [value="s"]. *)

  val action : string -> t
  (** [action s] is [action="s"]. *)

  val method' : string -> t
  (** [method' s] is [method="s"]. *)

  val data : string -> string -> t
  (** [data key value] is the custom attribute [data-key="value"], the carrier
      for the swap vocabulary the SSE dispatcher reads. [value] is escaped by
      construction. *)
end

(** {1:elements Elements} *)

module El : sig
  (** HTML elements and text nodes. *)

  val txt : string -> t
  (** [txt s] is a text node carrying [s]. This is the one untrusted-bytes sink:
      [s] is HTML-escaped by construction. *)

  val v : ?at:At.t list -> string -> t list -> t
  (** [v ~at tag children] is the element [tag] with attributes [at] (default
      none) and [children]. A void tag (for example [br], [hr], [input])
      self-closes and ignores [children]. *)

  val unsafe_raw : string -> t
  (** [unsafe_raw s] is [s] emitted verbatim, unescaped. {b The one XSS hole:}
      reserve it for static author-controlled chrome; never pass a fact,
      progress, or any other attacker-influenceable payload through it. *)

  val splice : t list -> t
  (** [splice ns] concatenates [ns] with no wrapping element. *)

  val void : t
  (** [void] is the empty node: it renders to nothing. *)

  val section : ?at:At.t list -> t list -> t
  (** [section] is {!v} for the [section] tag. *)

  val article : ?at:At.t list -> t list -> t
  (** [article] is {!v} for the [article] tag. *)

  val aside : ?at:At.t list -> t list -> t
  (** [aside] is {!v} for the [aside] tag. *)

  val div : ?at:At.t list -> t list -> t
  (** [div] is {!v} for the [div] tag. *)

  val span : ?at:At.t list -> t list -> t
  (** [span] is {!v} for the [span] tag. *)

  val p : ?at:At.t list -> t list -> t
  (** [p] is {!v} for the [p] tag. *)

  val pre : ?at:At.t list -> t list -> t
  (** [pre] is {!v} for the [pre] tag. *)

  val code : ?at:At.t list -> t list -> t
  (** [code] is {!v} for the [code] tag. *)

  val ul : ?at:At.t list -> t list -> t
  (** [ul] is {!v} for the [ul] tag. *)

  val ol : ?at:At.t list -> t list -> t
  (** [ol] is {!v} for the [ol] tag. *)

  val li : ?at:At.t list -> t list -> t
  (** [li] is {!v} for the [li] tag. *)

  val a : ?at:At.t list -> t list -> t
  (** [a] is {!v} for the [a] tag. *)

  val form : ?at:At.t list -> t list -> t
  (** [form] is {!v} for the [form] tag. *)

  val button : ?at:At.t list -> t list -> t
  (** [button] is {!v} for the [button] tag. *)

  val textarea : ?at:At.t list -> t list -> t
  (** [textarea] is {!v} for the [textarea] tag. *)

  val input : ?at:At.t list -> unit -> t
  (** [input ~at ()] is the void [input] element. *)

  val label : ?at:At.t list -> t list -> t
  (** [label] is {!v} for the [label] tag. *)

  val details : ?at:At.t list -> t list -> t
  (** [details] is {!v} for the [details] tag. *)

  val summary : ?at:At.t list -> t list -> t
  (** [summary] is {!v} for the [summary] tag. *)

  val time : ?at:At.t list -> t list -> t
  (** [time] is {!v} for the [time] tag. *)

  val blockquote : ?at:At.t list -> t list -> t
  (** [blockquote] is {!v} for the [blockquote] tag. *)

  val em : ?at:At.t list -> t list -> t
  (** [em] is {!v} for the [em] tag. *)

  val strong : ?at:At.t list -> t list -> t
  (** [strong] is {!v} for the [strong] tag. *)

  val br : ?at:At.t list -> unit -> t
  (** [br ~at ()] is the void [br] element. *)

  val hr : ?at:At.t list -> unit -> t
  (** [hr ~at ()] is the void [hr] element, the seam boundary. *)

  val progress : ?at:At.t list -> t list -> t
  (** [progress] is {!v} for the [progress] tag. *)

  val h : int -> ?at:At.t list -> t list -> t
  (** [h level ~at children] is a heading element [h<level>], with [level]
      clamped to the range [1..6]. *)
end

(** {1:rendering Rendering} *)

val to_string : t -> string
(** [to_string n] is the HTML serialisation of [n]. Void elements self-close;
    text nodes and escaped attribute values carry no unescaped ampersand,
    less-than, greater-than, double-quote, or apostrophe outside an
    {!El.unsafe_raw} node. *)
