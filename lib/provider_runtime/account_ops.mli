(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Private readiness, refresh, and logout machinery.

    This module accepts the private {!Runtime.t}; the public archive exposes
    discovery at its root and save/logout through [Login]. Account checks,
    refresh, store snapshots, and credential locks remain implementation
    details. No value returned here contains credential material. *)

val load :
  Runtime.t ->
  ?process:Mentat_provider.Credential.t list ->
  environment:(string * string) list ->
  unit ->
  (Mentat_provider.Account.Discovery.t list, Store_error.t) result
(** [load t ~environment ()] reads the current store once and resolves every
    declaration from the supplied process/environment snapshots. The result
    preserves catalog order and cardinality: each route is a known account or a
    provider-local resolution failure. A shared store failure is the outer
    [Error]. [process] defaults to [[]]. Network-free. *)

val check :
  Driver.registration ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?base_url:string ->
  Mentat_provider.Credential.t ->
  Mentat_provider.Account.t
(** [check registration ~sw ~env ?base_url credential] observes [credential]
    once. It returns a checked account for a configured route and a passive
    present account otherwise. Ordinary provider and transport outcomes inhabit
    the account's problem facts; cancellation and implementation faults escape.
    A checked timestamp is sampled when the observation completes. *)

val refresh :
  Runtime.t ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  now:Mentat_provider.Credential.timestamp ->
  ?force:bool ->
  ?auth_base_url:string ->
  Mentat_provider.Credential.t ->
  (Mentat_provider.Credential.t option, Error.t) result
(** [refresh t ~sw ~env ~now ?force ?auth_base_url credential] applies the
    provider's non-interactive refresh policy. Store-backed refresh holds the
    provider/name credential lock, re-reads the stored secret under the store
    lock, and conditionally commits only when it still matches the observed
    secret — so two turns racing a refresh on one slot serialize and converge on
    a single write. The durable commit runs under [Eio.Cancel.protect], so a
    cancel between minting the fresh secret and writing it cannot lose the
    token. It returns the exact current credential. [force] defaults to [false].
*)

val logout :
  Runtime.t ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  provider:Mentat_llm.Provider.t ->
  ?name:Mentat_provider.Credential.Name.t ->
  ?revoke:bool ->
  ?auth_base_url:string ->
  ?process:Mentat_provider.Credential.t list ->
  environment:(string * string) list ->
  unit ->
  (Mentat_provider.Account.Logout.t, Error.t) result
(** [logout t ~sw ~env ~provider ?name ?revoke ?auth_base_url ?process
     ~environment ()] settles local removal and optional revocation under the
    provider/name credential lock. [name] defaults to [Credential.Name.default],
    [revoke] to [false], and [process] to [[]].

    The returned discovery is derived while still holding the credential lock,
    from the exact post-settlement store snapshot and the supplied
    process/environment snapshots. Ordinary logout does not write the store when
    the slot is already absent. Revoking logout performs remote I/O without the
    global store lock, then conditionally removes only the observed secret; a
    replacement yields [Logout.Superseded]. An absent route and an explicit
    unsupported response both yield [Logout.Unsupported]. Durable removal is
    cancellation-protected. Cancellation during remote revocation may leave
    provider state unknown and may be retried; only a returned value attests
    local settlement. *)
