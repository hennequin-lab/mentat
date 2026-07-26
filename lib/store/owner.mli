(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Run-lock owner identities.

    An owner is the ephemeral identity of a session's current driver — never a
    durable session fact: session ids encode no host or pid, but a lock owner
    may, because it is runtime state, not a stored id. A would-be second driver
    receives the holder's owner so protocol [Busy] can name it. *)

type t
(** The type for run-lock owner identities. *)

val make : ?label:string -> unit -> t
(** [make ?label ()] captures this process's identity: pid, host, a wall-clock
    [since], and the optional human [label] (the command or frontend). The wall
    clock is read directly because an owner is ephemeral display state that
    never enters a document — the store's no-clock law governs what it writes
    into supplied values, and this writes none.

    Raises [Invalid_argument] if [label] is empty. *)

val pid : t -> int
(** [pid t] is the owning process id. *)

val host : t -> string
(** [host t] is the owning host name. *)

val label : t -> string option
(** [label t] is the human display label, if one was supplied. *)

val since : t -> float
(** [since t] is the Unix time at which the owner identity was captured. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same owner identity. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an owner for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps owners to JSON objects — the in-band owner line of a run lock.
    Decoding validates the non-empty label invariant of {!make} and rejects
    unknown members. *)
