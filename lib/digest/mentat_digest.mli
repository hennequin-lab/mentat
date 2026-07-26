(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Stable SHA-256 digests and canonical content references.

    {!type:t} is the SHA-256 digest of an exact byte sequence. Construct one
    with {!string}; serialize it with {!to_hex}, {!to_raw_string}, or {!jsont};
    and reload hexadecimal text with {!of_hex}. {!Content_ref} pairs a digest
    with its byte length for content addressing and staleness checks. {!derive}
    and {!key} derive namespaced digests and short identifiers from
    length-framed parts.

    {b Hashing model.} Hashing is over bytes, not text: it applies no Unicode,
    line-ending, NUL, or character-set normalization, and defines no canonical
    form for its input. A caller that folds several fields into one digest is
    responsible for its own framing; use {!derive} for length-framed,
    domain-separated derived digests and {!key} for their truncated hexadecimal
    identifiers. The module is deterministic and pure.

    {b One neutral type.} This library provides one digest type {!t} and one
    structural refinement, {!Content_ref.t}. A consumer that needs type-level
    separation between semantic roles wraps either type in its own abstraction.

    {b Not an authentication boundary.} This module provides no keyed
    authentication, and {!equal} is not constant-time. Digests here are
    integrity and identity tokens, not secrets. *)

(** {1:errors Errors} *)

module Error : sig
  (** Codec parse failures. Each is a recoverable failure on external or durable
      input; a caller reports it and rejects the input. *)

  type t =
    | Not_hex
        (** {!of_hex} (or {!jsont}): the input is not exactly 64 lowercase
            hexadecimal characters. *)
    | Malformed_content_ref
        (** {!Content_ref.of_token} (or {!Content_ref.jsont}): the input is not
            a canonical [sha256:<hex>:<length>] token. The grammar is strict and
            the only recovery is rejection, so violations are not
            sub-classified. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. It is stable enough for logs,
      not for programmatic matching. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats {!message} output. *)
end

(** {1:digests Digests} *)

type t
(** A SHA-256 digest: exactly 32 raw bytes. The representation is opaque;
    serialize with {!to_hex}, {!to_raw_string}, or {!jsont}. *)

val string : string -> t
(** [string s] is the SHA-256 digest of [s]'s exact bytes. [s] may contain
    arbitrary bytes, including invalid UTF-8 and NUL. The name follows
    {!Stdlib.Digest.string}: this is the digest {e of} the string, not a
    rendering of a digest {e to} text — that is {!to_hex}. *)

(** {2:digest_codecs Digest codecs} *)

val to_hex : t -> string
(** [to_hex t] is [t] as 64 lowercase hexadecimal characters (['0']–['9'],
    ['a']–['f']). Use for stable text identifiers and diagnostics. *)

val of_hex : string -> (t, Error.t) result
(** [of_hex s] is the digest whose {!to_hex} is [s], or [Error Error.Not_hex] if
    [s] is not exactly 64 lowercase hexadecimal characters. *)

val to_raw_string : t -> string
(** [to_raw_string t] is [t]'s 32 raw digest bytes as a binary string — not
    hexadecimal text, and possibly containing NUL. Use for byte-oriented
    protocols such as the RFC 7636 PKCE [S256] code challenge. This is a
    one-directional escape hatch: there is no inverse. *)

val jsont : t Jsont.t
(** [jsont] maps a digest to and from its {!to_hex} JSON string, rejecting a
    non-hex string with an {!Error.Not_hex} diagnostic. *)

(** {2:digest_std Comparing and printing} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same digest. Not
    constant-time; digests here are not secrets. *)

val compare : t -> t -> int
(** [compare a b] orders [a] and [b] by a total order consistent with {!equal}.
    It lets a consumer key a [Map] or [Set] on digests without observing the
    representation. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats {!to_hex} output. *)

(** {1:deriving Derived identifiers} *)

val frame : Buffer.t -> string -> unit
(** [frame buffer text] appends [text] to [buffer] length-framed as
    [<decimal length>:<bytes>] — the injective primitive {!derive} and {!key}
    compose. Framing is injective over arbitrary byte strings: two framed values
    never share a byte sequence across their boundary, even with an embedded
    [':'], NUL, or newline, so a reader cannot confuse where one field ends and
    the next begins.

    Use it directly to assemble a domain-separated digest input when the caller
    interleaves its own unframed markers between framed values and so cannot use
    {!derive}; feed the resulting {!Buffer.contents} to {!string}. When every
    field is framed uniformly, prefer {!derive}. *)

val derive : domain:string -> string list -> t
(** [derive ~domain parts] is the SHA-256 of [domain] and each of [parts], in
    order, each encoded length-framed as [<decimal length>:<bytes>].

    {b Framing law.} Every field — the domain first, then each part — is
    prefixed by its decimal byte length and [':'], so the framed input is
    injective over arbitrary byte strings, including empty parts and embedded
    NUL: no two distinct [(domain, parts)] concatenate to the same bytes, and
    distinct [domain] values separate namespaces.

    [domain] should be a stable, versioned namespace name, for example
    ["mentat.sandbox.identity.v1"]. The result is a full digest; {!key} is the
    same derivation truncated to a hexadecimal identifier.

    Raises [Invalid_argument] if [domain] is empty. *)

val key : length:int -> domain:string -> string list -> string
(** [key ~length ~domain parts] is the first [length] lowercase hexadecimal
    characters of [to_hex (derive ~domain parts)]: the SHA-256 of [domain] and
    each of [parts], each encoded length-framed as [<decimal length>:<bytes>].
    The framing is injective over arbitrary byte strings, including embedded
    NUL, so distinct [(domain, parts)] never share an input, and distinct
    [domain] values separate namespaces.

    [domain] should be a stable, versioned namespace name, for example
    ["mentat.permission.rule.v1"]. [length] is required so the
    collision-versus-display tradeoff stays explicit at each call site: the
    result is a truncated identifier, {e not} a full {!t}, and short prefixes
    suit local generated ids only where the caller accepts the collision risk
    for the chosen length.

    Raises [Invalid_argument] if [length] is not in the range \[[0];[64]\] or if
    [domain] is empty. *)

(** {1:incremental Incremental hashing} *)

module Fold : sig
  (** A single-owner incremental SHA-256 context: fold a byte stream through
      {!add} in any chunking, then {!digest} once for the result. This produces
      the same digest as {!string} of the chunks' concatenation without holding
      the whole input in memory. *)

  type digest := t
  (** [digest] is the enclosing {!Mentat_digest.t}. *)

  type t
  (** A mutable hashing context. It is single-owner: one holder folds one
      stream; there is no thread-safety and no sharing. *)

  val create : unit -> t
  (** [create ()] is a fresh context over the empty byte stream. *)

  val add : t -> string -> unit
  (** [add ctx chunk] appends [chunk]'s exact bytes to [ctx]'s stream. Chunking
      is invisible to the result: [add ctx a; add ctx b] hashes the same bytes
      as one [add ctx (a ^ b)], and folding a sequence of chunks yields
      {!string} of their concatenation.

      Raises [Invalid_argument] if [ctx] was finalized by {!digest}. *)

  val digest : t -> digest
  (** [digest ctx] is the SHA-256 of the bytes folded into [ctx]. It finalizes
      [ctx]: any later {!add} or {!digest} raises [Invalid_argument]. *)
end

(** {1:refs Content references} *)

module Content_ref : sig
  (** The neutral immutable identity of a complete byte string: its SHA-256
      {!digest} together with its byte {!length}, serialized as the canonical
      [sha256:<hex>:<length>] token. Content references identify stored bytes,
      carry review and mutation evidence, and guard optimistic file updates.

      A content reference is {e not} a security boundary. *)

  type digest := t
  (** [digest] is the enclosing {!Mentat_digest.t}. *)

  type t
  (** A content reference. Opaque; serialize with {!to_token} or {!jsont}. Two
      references are equal iff both digest and length are equal. *)

  val of_contents : string -> t
  (** [of_contents s] references [s]: its SHA-256 digest and its byte length,
      computed with one hashing pass. *)

  val digest : t -> digest
  (** [digest r] is [r]'s SHA-256 digest. Its hex form is [to_hex (digest r)].
  *)

  val length : t -> int
  (** [length r] is the byte length of the content [r] references. *)

  val matches : t -> string -> bool
  (** [matches r s] is [true] iff [s] is content that [r] references: its byte
      length equals {!length} and its SHA-256 equals {!digest}. The length is
      checked first, so a wrong-sized [s] returns [false] without hashing. Use
      to validate stored or received bytes and for optimistic-concurrency
      checks. *)

  val verify : t -> string -> (unit, t) result
  (** [verify r s] is [Ok ()] iff [s] is content that [r] references (same byte
      length and SHA-256), and otherwise [Error actual] where [actual] is
      [of_contents s] — the reference [s] {e does} carry. It hashes [s] exactly
      once. Prefer it over {!matches} at a mismatch site that must report or
      record the actual reference, so the bytes are not hashed a second time to
      recover it. *)

  (** {2:ref_codecs Content-reference codecs} *)

  val to_token : t -> string
  (** [to_token r] is the canonical token [sha256:<hex>:<length>]: 64 lowercase
      hexadecimal digest characters, then the decimal byte length with no
      leading zero (except ["0"]). Stable across versions; this is both the
      display and the JSON string form. *)

  val of_token : string -> (t, Error.t) result
  (** [of_token s] parses a canonical {!to_token} token, or
      [Error Error.Malformed_content_ref]. The accepted grammar is exactly
      [sha256:<64 lowercase hex>:<decimal length>]; the parser rejects empty
      input, a wrong tag, uppercase or non-hex or wrong-length digests, missing
      or extra fields, non-decimal, signed, or leading-zero lengths, and lengths
      greater than [max_int]. *)

  val jsont : t Jsont.t
  (** [jsont] maps a reference to and from its {!to_token} JSON string,
      validating the same grammar as {!of_token} and reporting {!Error.message}
      diagnostics. *)

  (** {2:ref_std Comparing and printing} *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same digest and length. *)

  val compare : t -> t -> int
  (** [compare a b] orders [a] and [b] by a total order consistent with
      {!equal}, so a content-addressed store can key a [Map] or [Set] on
      references. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf r] formats {!to_token} output. *)
end
