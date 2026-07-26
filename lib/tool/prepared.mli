(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Durable staged payloads — internal implementation.

    The facade re-exports only the query surface ({!tool}, {!description},
    {!requests}, {!equal}, {!jsont}). {!of_payload}, {!input}, and
    {!reconstruct} serve {!Call}'s run and resume paths and are
    library-internal, so no public caller mints a prepared value from a live
    plan outside a staged run and none reads the resume-only payload.

    A prepared value is the one tool-owned value that survives Mentat's
    process-exiting permission wait: the erased, serializable envelope of a
    staged tool's plan — tool name, canonical provider input, canonical plan
    payload, final permission requests, and a presentation description. It
    carries no callback, output, workspace evidence, or attempt count. A stored
    envelope decodes through {!jsont} (the session reloads it) and becomes
    authority only by passing {!reconstruct}, the revalidation
    {!Mentat_tool.Call.resume} performs. *)

type t
(** The type for a durable prepared payload. *)

val of_payload :
  tool:string ->
  input:Jsont.json ->
  'a Jsont.t ->
  describe:('a -> string) ->
  requests:('a -> Mentat_permission.Request.t list) ->
  'a ->
  (t, string) result
(** [of_payload ~tool ~input jsont ~describe ~requests plan] is the prepared
    envelope for [plan].

    It encodes [plan] with [jsont], canonicalizes the payload, decodes it back,
    and derives the final [requests] and the [describe] description from the
    {e reconstructed} plan — never the original heap value — so the live path
    and the process-resumed path consume the same plan. [input] is the canonical
    provider input, sourced by {!Mentat_tool.Call.input}. Before returning it
    proves what resume will later demand: the stored payload decodes to a
    canonical value with the same requests.

    Returns [Error message] if [jsont] cannot round-trip [plan] to a stable
    canonical value — a construction defect in the staged tool. *)

val tool : t -> string
(** [tool t] is the name of the staged tool that produced [t]. *)

val description : t -> string
(** [description t] is the presentation description [describe] produced for
    [t]'s plan. *)

val input : t -> Jsont.json
(** [input t] is the canonical provider input [t] was prepared from. {!Call}
    compares it to a fresh decode's input before resuming. *)

val requests : t -> Mentat_permission.Request.t list
(** [requests t] is the final permission requests [t]'s plan needs. *)

(** The type for reasons {!reconstruct} rejects a prepared value. *)
type reconstruction =
  | Undecodable of string
      (** [t]'s stored payload does not decode with the codec. *)
  | Drift
      (** [t]'s stored payload decodes but is not canonical: re-encoding the
          decoded plan does not reproduce it. *)

val reconstruct : t -> 'a Jsont.t -> ('a, reconstruction) result
(** [reconstruct t jsont] decodes [t]'s stored payload with [jsont] and returns
    the plan, having checked the payload is canonical. It never re-runs
    preparation. {!Mentat_tool.Call.resume} uses it, then recomputes the final
    requests from the returned plan and requires they still match {!requests}.
*)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same tool, canonical input,
    canonical payload, final requests, and description, comparing the JSON
    fields with member order significant. It is the round-trip check a stored
    envelope must satisfy. *)

val jsont : t Jsont.t
(** [jsont] maps prepared values to and from their version-1 JSON envelope.
    Payload and input are stored already canonical; decoding rejects an unknown
    envelope version. *)
