(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Compact durable facts for a web fetch.

    Response text, URLs, content type, durations, redirect targets, and
    diagnostics remain solely in the authoritative model-visible text or
    terminal result error. This projection deliberately carries no request
    headers or credentials. *)

(** The kind of HTTP observation returned by the tool. *)
type disposition =
  | Fetched  (** A successful 2xx response body was returned. *)
  | Redirected  (** A cross-authority redirect was reported, not followed. *)
  | Http_error  (** A non-2xx response was returned as a failed result. *)

type t
(** The type for compact fetch facts. [Fetched] has a 2xx status, [Redirected]
    has a 3xx status and a zero byte count, and [Http_error] has a non-2xx
    status. Status codes are in [[100];[599]] and byte counts are non-negative.
*)

val make : disposition:disposition -> status:int -> bytes:int -> t
(** [make ~disposition ~status ~bytes] is one fetch observation.

    Raises [Invalid_argument] if [status] is outside [[100];[599]], if it does
    not agree with [disposition], if [bytes] is negative or outside JSON's safe
    integer range, or if a redirect has a non-zero byte count. *)

val disposition : t -> disposition
(** [disposition t] is the kind of HTTP observation. *)

val status : t -> int
(** [status t] is the observed HTTP status code. *)

val bytes : t -> int
(** [bytes t] is the number of response-body bytes observed. *)

val jsont : t Jsont.t
(** [jsont] maps fetch facts to their closed version-1 JSON shape. Numeric
    members decode only from exact integers in JSON's safe integer range;
    fractions, non-finite numbers, and out-of-range values are rejected. *)
