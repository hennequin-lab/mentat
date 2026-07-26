(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Canonical JSON for durable tool values.

    Canonicalization gives a JSON value a deterministic member order so that two
    semantically equal values serialize identically. It is the single owner of
    the shape stored as a tool claim's input and a prepared payload; {!Call} and
    {!Prepared} both route their durable JSON through it.

    {b Why not {!Jsont.Json}?} [Jsont.Json.equal] and [compare] already sort
    object members {e for comparison} and ignore parse metadata, so a plain
    equality between two decoded values is order-insensitive. But [Jsont.Json]
    has no operation that {e produces} a canonically-ordered value: the stored
    form must have a fixed member order for its serialization — and therefore
    its content digest — to be stable across processes. {!of_json} produces that
    form, and {!equal} compares two values with member order significant, so a
    payload that is not already canonical is detectable. *)

val of_json : Jsont.json -> Jsont.json
(** [of_json j] is [j] with every object's members sorted by name (byte-wise,
    stable on equal names) and the sort applied recursively to array elements
    and member values. Scalars are returned unchanged. Parse metadata is
    preserved but does not participate in the order. *)

val equal : Jsont.json -> Jsont.json -> bool
(** [equal a b] is [true] iff [a] and [b] have the same structure with
    {e member order significant}, ignoring parse metadata. Unlike
    {!Jsont.Json.equal} it does not reorder object members, so it distinguishes
    a canonical value from a reordering of it. *)
