(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Execution-lifecycle stage.

    A stage names the next callback boundary of a tool call. It is the one stage
    vocabulary the whole system shares: a decoded ordinary call is {!Direct}, a
    decoded staged call is {!Prepare}, and a staged call resumed from a durable
    prepared value is {!Run}. The session derives its claim identifiers from
    {!to_string}, so those tokens are stable. *)

type t =
  | Direct  (** an ordinary call: one guarded callback *)
  | Prepare  (** a staged call before preparation *)
  | Run  (** a staged call resumed to run its prepared plan *)

val to_string : t -> string
(** [to_string t] is the stable token ["direct"], ["prepare"], or ["run"]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same stage. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats {!to_string} on [ppf]. *)

val jsont : t Jsont.t
(** [jsont] maps a stage to and from its {!to_string} token. *)
