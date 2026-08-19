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
  Runtime.t ->
  Driver.registration ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?base_url:string ->
  ?auth_base_url:string ->
  Mentat_provider.Credential.t ->
  Mentat_provider.Account.t
(** [check t registration ~sw ~env ?base_url ?auth_base_url credential]
    observes [credential] once. It returns a checked account for a configured
    route and a passive present account otherwise, and retains the
    observation's server listing in [t] under the credential's fingerprint and
    [base_url]. Ordinary provider and transport outcomes inhabit the account's
    problem facts; cancellation and implementation faults escape. A checked
    timestamp is sampled when the observation completes. *)

val refresh_listings :
  Runtime.t ->
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?providers:Mentat_llm.Provider.t list ->
  ?base_url:(Mentat_llm.Provider.t -> string option) ->
  ?auth_base_url:(Mentat_llm.Provider.t -> string option) ->
  ?process:Mentat_provider.Credential.t list ->
  environment:(string * string) list ->
  unit ->
  (unit, Store_error.t) result
(** [refresh_listings t ~sw ~env ?providers ?base_url ?auth_base_url ?process
     ~environment ()] runs every selected provider's driver check in parallel
    fibers and retains the observed listings in [t]. Credentials resolve from
    the supplied snapshots exactly as discovery does; a required-auth provider
    with no resolved credential and a provider whose resolution fails are
    skipped, not failed, and a provider without a check is skipped. [providers]
    defaults to every catalog provider; [base_url] and [auth_base_url] default
    to no overrides; [process] to [[]]. The store is read once; a shared store
    failure is the outer error. *)

val listings :
  Runtime.t ->
  ?providers:Mentat_llm.Provider.t list ->
  ?process:Mentat_provider.Credential.t list ->
  ?base_url:(Mentat_llm.Provider.t -> string option) ->
  ?auth_base_url:(Mentat_llm.Provider.t -> string option) ->
  environment:(string * string) list ->
  unit ->
  ( (Mentat_llm.Provider.t * Mentat_provider.Listing.t) list,
    Store_error.t )
  result
(** [listings t ?providers ?process ?base_url ?auth_base_url ~environment ()]
    is the retained listing of every selected provider whose slot still
    matches the provider's currently resolving credential fingerprint and
    endpoint overrides — so a rotated credential or moved endpoint silently
    drops its stale listing. [providers] defaults to the whole catalog.
    Catalog order; providers with no retained listing are absent.
    Network-free. *)

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
