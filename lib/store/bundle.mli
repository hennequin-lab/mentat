(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The export-bundle wire format.

    Bundle owns the stream's shape — the tag, the format version, and the header
    and line codecs — and {!Export} encodes through these codecs, so the writer
    and the decoder cannot drift. The decoder exists so the library's round-trip
    tests can prove that "export is complete" is a fact, not a claim; it is
    deliberately not a public import surface, which would additionally need a
    staged directory, fsyncs, and atomic installation and is minted only when a
    consumer exists. *)

val tag : string
(** The bundle's [export] header tag, ["mentat.session"]. *)

val format_version : int
(** The bundle stream format version carried in the header line. *)

val header_jsont : (string * int * int) Jsont.t
(** [header_jsont] is the header line: the export {!tag}, the {!format_version},
    and the embedded document's own version. *)

val document_version_jsont : int Jsont.t
(** [document_version_jsont] reads the [version] member of a document section
    line's embedded document — the writer derives the header's document version
    from it, and {!decode} checks the two agree. *)

module Line : sig
  (** The section lines that follow the header, in stream order: document,
      events, blobs, end. *)

  type t =
    | Document of Mentat_session.t
    | Event of Mentat_mutation.Event.t
    | Blob of { reference : Mentat_digest.Content_ref.t; bytes : string }
        (** [bytes] is base64-encoded. *)
    | End of { events : int; blobs : int; digest : string }
        (** [digest] is ["sha256:..."] over the exact bytes of every preceding
            line, trailing newlines included. *)

  val jsont : t Jsont.t
  (** [jsont] is one [\n]-terminated section line, discriminated by its
      [section] member. *)
end

val referenced_blobs :
  Mentat_mutation.Event.t list -> Mentat_digest.Content_ref.t list
(** [referenced_blobs events] is every content reference the events' images
    carry, deduplicated, in first-reference order. This is the mutation half;
    {!referenced_blobs_all} adds the document's media. *)

val referenced_blobs_all :
  Mentat_session.t ->
  Mentat_mutation.Event.t list ->
  Mentat_digest.Content_ref.t list
(** [referenced_blobs_all document events] is the complete blob set a bundle
    closes over: {!referenced_blobs} of [events] unioned with [document]'s
    {!Mentat_session.media_refs}, deduplicated with the mutation-event refs
    first then any new document-media refs — the exact blob section
    {!Export.write} emits and {!decode} requires. *)

(** {1:decoding Decoding} *)

module Error : sig
  (** Bundle decode failures. *)

  type t =
    | Truncated
        (** The bundle does not prove completeness: a torn final line, a missing
            terminal manifest, counts that do not match, or a digest that does
            not cover the received bytes. *)
    | Corrupt of string
        (** A line that will not decode, an unsupported format or document
            version, out-of-order sections, blob bytes that do not match their
            reference, a blob section that is not exactly the events' referenced
            images (a duplicate line, an unreferenced blob, or a referenced blob
            absent from the bundle), or events that do not replay. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic for [e]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf e] formats [e] for diagnostics. *)
end

type t = {
  document_version : int;  (** The header's document version. *)
  session : Mentat_session.t;  (** The decoded, replay-valid document. *)
  events : Mentat_mutation.Event.t list;  (** Events in append order. *)
  blobs : (Mentat_digest.Content_ref.t * string) list;
      (** Referenced images, verified against their references. *)
}
(** The type for a decoded bundle. *)

val decode : string -> (t, Error.t) result
(** [decode input] decodes a complete {!Export.write} stream, verifying the
    header, section order, counts, the manifest digest over the exact preceding
    bytes, and every blob's content reference. Beyond the wire checks it proves
    the bundle installable: the blob section must be exactly {!referenced_blobs}
    of the events — no duplicate line, no unreferenced blob, no reference left
    unclosed — and the events must replay through
    {!Mentat_mutation.State.of_events}; it also proves every mutation turn and
    tool claim belongs to the embedded session document. Each failure is a loud
    [Corrupt], never a repaired value. *)
