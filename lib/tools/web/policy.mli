(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Host-selected limits for public web fetches.

    A policy is inert configuration. It neither grants network access nor
    performs I/O; {!Web_fetch} projects an admitted URL into a permission
    request and {!Transport.t} enforces the limits while performing the request.
*)

module Error : sig
  (** The field whose configured value was invalid. *)
  type field =
    | Max_fetch_bytes
    | Max_output_chars
    | Default_timeout_ms
    | Max_timeout_ms

  type t =
    | Non_positive of { field : field; value : int }
        (** A byte, character, or timeout bound was not positive. *)
    | Default_timeout_exceeds_max of {
        default_timeout_ms : int;
        max_timeout_ms : int;
      }  (** The default timeout could not satisfy the configured maximum. *)

  val message : t -> string
  (** [message error] is a human-readable configuration diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf error] formats [error] for diagnostics. *)
end

module Timeout_error : sig
  type t =
    | Non_positive of int  (** The requested timeout was not positive. *)
    | Exceeds_max of { requested : int; maximum : int }
        (** The requested timeout exceeded the policy maximum. *)

  val message : t -> string
  (** [message error] is a human-readable input diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf error] formats [error] for diagnostics. *)
end

type t
(** The type for validated web-fetch policy.

    Response and output limits are positive. The default timeout is positive and
    no greater than the maximum timeout. *)

val make :
  ?allow_private_network:bool ->
  ?max_fetch_bytes:int ->
  ?max_output_chars:int ->
  ?default_timeout_ms:int ->
  ?max_timeout_ms:int ->
  unit ->
  (t, Error.t) result
(** [make ()] is a conservative policy with private-network access disabled, a 5
    MiB response limit, a 100,000-character body-projection limit, a 30-second
    default timeout, and a 120-second maximum timeout.

    Invalid host configuration is returned as a structured {!Error.t}. *)

val allow_private_network : t -> bool
(** [allow_private_network t] is whether local and private network addresses may
    be fetched. The transport must apply this policy to every resolved address
    immediately before connecting. *)

val max_fetch_bytes : t -> int
(** [max_fetch_bytes t] is the maximum response body size in bytes. *)

val max_output_chars : t -> int
(** [max_output_chars t] is the maximum number of Unicode characters retained
    from a fetched body or HTTP-error preview. Fixed response headings and a
    truncation note are added outside this body-projection bound. *)

val default_timeout_ms : t -> int
(** [default_timeout_ms t] is the timeout used when an input omits one. *)

val max_timeout_ms : t -> int
(** [max_timeout_ms t] is the largest timeout an input may request. *)

val resolve_timeout_ms : t -> int option -> (int, Timeout_error.t) result
(** [resolve_timeout_ms t requested] is [t]'s default timeout for [None], or the
    positive requested timeout when it does not exceed {!max_timeout_ms}. *)
