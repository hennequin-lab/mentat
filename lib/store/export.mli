(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The export bridge — one streamed, versioned, self-describing bundle per
    session.

    Export composes the session and mutation handles; it is a bridge over the
    two domains, not an op of either. An archive must prove completeness — a
    torn archive cannot — so the stream ends with a terminal manifest whose
    digest covers the exact bytes of every preceding line: a bundle without its
    end line, with counts that do not match, or with a digest that does not
    cover the received bytes is truncated, never a repaired tail.

    The wire format is owned by {!Bundle}: export encodes every line through the
    bundle codecs, so the writer and the decoder cannot drift. There is no
    public import: the decoder stays internal to the library and its round-trip
    tests, so nothing public can half-install a session. *)

val write :
  root:Handle.t ->
  fence:Run_lock.guard ->
  write:(string -> unit) ->
  (unit, [ `Session of Session.Error.t | `Mutation of Mutation.Error.t ]) result
(** [write ~root ~fence ~write] streams the fenced session's complete bundle to
    [write], line by line. The document and correlated mutation state are first
    snapshotted under their shared session-document lock; the lock is released
    before [write] is called, so a callback cannot prolong the write critical
    section. This remains coherent even when two fibers share one guard. Every
    blob is re-verified against its {!Mentat_digest.Content_ref.t} as it is read
    ([`Mutation (Blob_mismatch _)] otherwise). The stream:

    - a header line
      [{"export":"mentat.session","format_version":N,"document_version":M}];
    - one [{"section":"document","document":...}] line carrying the whole
      {!Mentat_session.jsont} encoding;
    - [{"section":"event","event":...}] lines per {!Mentat_mutation.Event.t};
    - [{"section":"blob","ref":...,"bytes":<base64>}] lines per referenced
      image;
    - a terminal manifest line
      [{"section":"end","events":N,"blobs":M,"digest":"sha256:..."}] whose
      digest covers the exact bytes of every preceding line, trailing newlines
      included.

    This is the value the protocol [Export] query streams; the frontend owns
    where the bytes land.

    A referenced blob that is missing entirely — possible only through external
    damage, since blobs are reclaimed only with their whole session and every
    referenced image's bytes enter the store through a composite append op
    before the event that references them lands — is
    [`Mutation (Missing_blob _)].

    Raises [Invalid_argument] if [fence] is released or does not hold the run
    lock of [root] — liveness is re-checked at every read boundary of the
    stream, so a fence falling mid-export raises rather than letting an
    incoherent capture reach its terminal manifest — or if a value the store
    itself validated — the loaded document, a replayed event — fails to
    re-encode, which is impossible-by-construction and a programmer error, never
    corruption. *)
