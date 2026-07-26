(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Dune RPC workspace-level diagnostic state.

    {!Instance} is the shareable workspace object that polls the Dune registry,
    selects the matching endpoint, and reports the latest-known diagnostic state
    for tools and host watchers. *)

module Diagnostic : sig
  (** Dune diagnostic identifiers and latest-known stores. *)

  module Id : sig
    (** Stable identifier for a Dune diagnostic event. *)

    type t
    (** The type for non-empty diagnostic identifiers. Diagnostics are keyed by
        this id inside the store; the projection consumers read the diagnostic
        value and ignore the id, so it stays opaque. *)
  end

  type id = Id.t
  (** The type for diagnostic identifiers. *)

  module Store : sig
    (** Latest-known Dune diagnostic set, keyed by Dune diagnostic id. *)

    type t
    (** The type for a diagnostic set keyed by Dune diagnostic id. *)

    val to_list : t -> (id * Mentat_ocaml.Diagnostic.t) list
    (** [to_list store] is the diagnostics in deterministic adapter order. *)
  end
end

module Instance : sig
  (** Workspace-level Dune RPC state shared by tools and watchers.

      One instance should be created per Mentat workspace and reused by the
      diagnostics tool and the host Dune watcher. The instance discovers
      already-running Dune RPC servers through the registry; it never starts
      Dune. This shared state keeps explicit tools and proactive host notices on
      the same endpoint and diagnostic store.

      The instance observes Dune one-shot: {!build_health} re-queries the full
      diagnostic set per call. A live diagnostics panel (a future milestone)
      would restore a streaming subscription over this same
      registry-and-connection core; that is wiring, not new design. *)

  type t
  (** The type for a workspace-level Dune RPC instance. *)

  val create :
    fs:_ Eio.Path.t ->
    net:_ Eio.Net.t ->
    workspace:Mentat_workspace.t ->
    ?env:(string -> string option) ->
    unit ->
    t
  (** [create ~fs ~net ~workspace ()] is a workspace-level Dune RPC instance.

      [fs] is used to poll Dune's registry. [net] is used to connect to the
      selected endpoint. [env] defaults to {!Sys.getenv_opt} and is used for XDG
      registry discovery.

      The value owns registry polling and the latest diagnostic state for
      [workspace]. It never starts Dune; it only observes an already-running RPC
      instance. *)

  val diagnostics : t -> Diagnostic.Store.t
  (** [diagnostics t] is the latest-known diagnostic set observed through [t].
      It is updated by successful {!build_health} queries. *)

  module Health : sig
    (** One-shot build-health verdict from Dune's current diagnostics.

        A verdict is the collapse of connectivity and diagnostic count into the
        fact a frontend shows at a glance. It is derived by {!build_health} and
        is not latched: each call re-queries Dune. *)

    type t =
      | Disconnected
          (** No matching Dune RPC endpoint is registered for the workspace, or
              registry discovery failed. Build diagnostics are unavailable,
              which is not an error. *)
      | Clean  (** Connected, with an empty current diagnostic set. *)
      | Failing of int
          (** Connected, with the current diagnostic count, which is at least
              [1]. *)
      | Unknown
          (** Connected, but the current diagnostic set could not be retrieved
              within the query bound. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] iff [a] and [b] are the same verdict. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] formats [t] for diagnostics. *)
  end

  val build_health :
    t -> clock:_ Eio.Time.clock -> ?timeout_s:float -> unit -> Health.t
  (** [build_health t ~clock ()] is a one-shot build-health verdict for the
      workspace, derived from Dune's current diagnostic set.

      It polls the Dune RPC registry for a matching endpoint and, when one is
      found, opens a short-lived connection and requests the current diagnostic
      set, bounded to [timeout_s] wall-clock seconds (default [0.5]). A
      successful request updates {!diagnostics} and yields {!Health.Clean} for
      an empty set or {!Health.Failing} for a non-empty one; a missing endpoint
      or a discovery failure yields {!Health.Disconnected}; a connection failure
      or a timeout yields {!Health.Unknown}.

      This query never spawns Dune: it only observes an already-running RPC
      instance. It is intended to be cheap enough to call at frontend startup.
  *)
end
