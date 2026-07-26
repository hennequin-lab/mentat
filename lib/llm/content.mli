(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Model-visible content blocks.

    Content is the shared payload language for user messages and tool results.
    It is intentionally small: visible text and opaque media. Content values are
    inert; provider adapters decide which media types and source forms they can
    encode for a given model API. *)

type media_source =
  [ `Uri of string | `Base64 of string | `Ref of Mentat_digest.Content_ref.t ]
(** The type for model-visible media sources.

    [`Uri uri] is an adapter-interpreted URI or URL. [`Base64 data] is inline
    base64 payload text. [`Ref r] is a content-addressed reference (digest and
    byte length) to media bytes held elsewhere (an attachment store); it is
    durable and stable across replays without inlining large payloads into the
    transcript.

    A [`Ref] is not directly wire-sendable: an effectful request-preparation
    pass above [mentat.llm] resolves it to [`Base64]/[`Uri] before the request
    reaches a {!Client.t}, and strictly after {!Request.digest} is taken. An
    adapter that receives an unresolved [`Ref] returns {!Error.Invalid_request}.
    {!media} accepts only non-empty [`Uri]/[`Base64] strings; a [`Ref] is always
    accepted. MIME, URI, and base64 validation belong to adapters. *)

type t = private
  | Text of string
  | Media of { media_type : string; source : media_source }
      (** The type for model-visible content blocks.

          Text and media strings are non-empty. *)

val text : string -> t
(** [text s] is text content [s].

    Raises [Invalid_argument] if [s] is empty. *)

val media : media_type:string -> media_source -> t
(** [media ~media_type source] is media content with MIME type [media_type].

    Raises [Invalid_argument] if [media_type] is empty, or a [`Uri]/[`Base64]
    source string is empty. A [`Ref] source is always accepted. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same content block: equal
    text, or equal media type and source (a [`Ref] compares by content
    reference). *)

val jsont : t Jsont.t
(** [jsont] maps content blocks to and from tagged JSON objects.

    Decoding errors if the object violates content invariants. *)
