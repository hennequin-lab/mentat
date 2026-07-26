(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Credential-free account readiness facts.

    [Account] is the credential-free half of the account vocabulary shared by
    the engine, provider drivers, status surfaces, and session diagnostics: what
    is known about one provider route — whether a credential resolved, what an
    account check observed, and the derived readiness {!Phase.t}. The
    secret-bearing values live in {!Credential}, a different namespace; account
    values never retain secret material.

    Credential-free values may still carry privacy-sensitive metadata such as
    email addresses, organization names, environment variable names, and stored
    credential names. Callers decide which logging, diagnostics, or session
    surfaces may store them.

    The module performs no I/O. It does not read environment variables, open
    credential stores, use keychains, run OAuth flows, refresh tokens, validate
    accounts, construct provider clients, or discover model catalogs.
    [mentat.provider_runtime] owns those effects and lowers their observations
    into this vocabulary. *)

type timestamp = int64
(** Unix timestamp in seconds.

    Constructors that accept timestamps reject negative values. The type does
    not encode a clock source; callers decide whether a timestamp comes from a
    provider response, local wall clock, or persisted observation. *)

(** {1:subjects Profiles and organizations} *)

module Profile : sig
  type t = private {
    id : string option;
    email : string option;
    name : string option;
  }
  (** Credential-free provider account profile.

      Profiles identify the signed-in account independently of credentials and
      organization selection. They contain no credential material, but may still
      be privacy-sensitive. *)

  val make : ?id:string -> ?email:string -> ?name:string -> unit -> t
  (** [make ()] is a provider account profile.

      Raises [Invalid_argument] if all fields are omitted or if a provided
      string field is empty. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same profile. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics.

      The output contains no credential material and is not stable storage
      syntax. *)
end

module Org : sig
  (** Credential-free organization and workspace selection. *)

  type t = private { id : string; name : string option }
  (** Credential-free organization or workspace selection.

      Organizations are the account scope selected for provider and console
      operations. Account discovery, organization listing, and switching are
      effects; {!Org.t} is only the credential-free selected scope observed for
      an account. *)

  val make : id:string -> ?name:string -> unit -> t
  (** [make ~id ()] is an organization selection.

      Raises [Invalid_argument] if [id] or [name] is empty. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same organization. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics.

      The output contains no credential material and is not stable storage
      syntax. *)
end

(** {1:problems Problems} *)

module Problem : sig
  type label
  (** Checked label for an unknown provider-neutral account problem. *)

  (** Provider-neutral account readiness problem.

      [Other label] is for stable labels not yet represented by a dedicated
      constructor; use {!other} to construct them.

      Problems are account facts, not UI actions. Login prompts, refresh
      buttons, retry labels, and administrator guidance belong to product
      surfaces that interpret these facts. *)
  type t =
    | Invalid_credential  (** Credential is malformed or rejected. *)
    | Expired_credential  (** Credential has expired. *)
    | Refresh_failed  (** Credential refresh failed. *)
    | Revoked  (** Credential was revoked. *)
    | Wrong_account  (** Credential resolves to the wrong provider account. *)
    | Wrong_organization  (** Credential resolves to the wrong organization. *)
    | Rate_limited  (** Account or credential is rate limited. *)
    | Quota_exceeded  (** Account or credential has exhausted quota. *)
    | Network  (** Account check failed due to network availability. *)
    | Unsupported  (** Provider cannot perform the requested account check. *)
    | Other of label  (** Unknown provider-neutral problem label. *)

  val other : string -> t
  (** [other label] is an account problem with unknown provider-neutral label
      [label].

      [label] must start with an ASCII lowercase letter and then contain only
      ASCII lowercase letters, digits, and [_]. Labels reserved by dedicated
      constructors are rejected. Raises [Invalid_argument] otherwise. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s stable storage spelling. *)

  val of_string : string -> t option
  (** [of_string s] decodes stable storage spelling [s].

      Unknown valid, non-reserved spellings decode to [Some (Other label)].
      Invalid spellings decode to [None]. *)

  val fatal : t -> bool
  (** [fatal t] is [true] iff only user action can fix [t]: exactly
      [Invalid_credential], [Expired_credential], [Refresh_failed], [Revoked],
      [Wrong_account], and [Wrong_organization].

      Fatal problems make new runs certainly doomed until the user acts, so they
      are the only problems that may block a run; see {!phase}. Problems that
      self-heal ([Network], [Rate_limited], [Quota_exceeded]) and problems of
      unknown nature ([Unsupported], [Other]) are not fatal. *)

  val transient : t -> bool
  (** [transient t] is [true] iff [t] self-heals within seconds to minutes:
      exactly [Network] and [Rate_limited].

      Transient problems are observations too short-lived to persist; callers
      report them without replacing previously durable readiness. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same problem. *)

  val compare : t -> t -> int
  (** [compare a b] orders problems by stable storage label. The order is
      compatible with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics.

      The output contains no credential material and is not stable storage
      syntax. *)
end

(** {1:accounts Accounts} *)

type t
(** Credential-free account facts for one provider route.

    Account values never retain API keys, bearer tokens, OAuth access tokens,
    refresh tokens, authorization codes, callback URLs, PKCE verifiers, or
    device codes. Constructors that accept {!Credential.t} immediately project
    the supplied secret-bearing credential to credential-free account facts.

    An account built with {!missing} has no resolved credential facts and no
    checked facts. An account built with {!present} has scalar credential facts
    such as {!source}, {!credential_kind}, and {!fingerprint}, but no checked
    facts. An account built with {!checked} has those scalar credential facts,
    normalized problems, and any optional checked facts supplied by the host. *)

module Phase : sig
  (** Derived route readiness phase for product surfaces.

      The first two phases mirror the constructor that built the value; the
      remaining three classify what a check observed. *)
  type t =
    | Missing  (** No credential resolved for the provider route. *)
    | Unchecked
        (** A credential resolved, but no account check completed yet. *)
    | Ready  (** Checked, and no account problem was observed. *)
    | Degraded  (** Checked with problems, none of which is {!Problem.fatal}. *)
    | Blocked  (** Checked, and a {!Problem.fatal} problem blocks the route. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s stable diagnostic spelling: ["missing"],
      ["unchecked"], ["ready"], ["degraded"], or ["blocked"]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same phase. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats {!to_string}. *)

  val jsont : t Jsont.t
  (** [jsont] maps phases to and from their {!to_string} spellings. Decoding
      errors on unknown spellings. *)
end

val missing : provider:Mentat_llm.Provider.t -> t
(** [missing ~provider] is account status for a provider route with no resolved
    credential.

    [source], [credential_kind], [checked_at], [profile], and [org] are [None];
    [problems] is [[]]. *)

val present : Credential.t -> t
(** [present credential] is account status for a resolved credential that has
    not been checked.

    [present credential] keeps the provider, source, and credential kind, but
    does not retain [credential]'s secret material. [checked_at], [profile], and
    [org] are [None]; [problems] is [[]]. *)

val checked :
  Credential.t ->
  ?at:timestamp ->
  ?profile:Profile.t ->
  ?org:Org.t ->
  ?problems:Problem.t list ->
  ?models:string list ->
  unit ->
  t
(** [checked credential ?at ?profile ?org ?problems ?models ()] is account
    status for a resolved credential after host or provider observation.

    [problems] defaults to [[]]. An empty list means the route was checked and
    no account problem was observed. Non-empty [problems] are provider-neutral
    facts about why the route may be degraded or unusable. Problems are
    deduplicated and stored in {!Problem.compare} order.

    [profile], if present, is the credential-free provider account. [org], if
    present, is the selected organization or workspace scope. [at], if present,
    is the Unix timestamp in seconds for the observation. [models], if present,
    is the set of provider-visible model ids revealed by the check.

    [checked] keeps the provider, source, and credential kind, but does not
    retain [credential]'s secret material. Models are deduplicated and stored in
    [String.compare] order.

    Raises [Invalid_argument] if [at] is negative. *)

val provider : t -> Mentat_llm.Provider.t
(** [provider t] is [t]'s provider route. *)

val phase : t -> Phase.t
(** [phase t] is [t]'s derived readiness phase.

    {!missing} accounts are [Missing] and {!present} accounts are [Unchecked].
    {!checked} accounts are [Ready] without problems, [Blocked] when any problem
    is {!Problem.fatal}, and [Degraded] otherwise. *)

val connected : t -> bool
(** [connected t] is [true] iff a credential resolved and no fatal problem
    blocks it: exactly phases [Ready], [Degraded], and [Unchecked].

    Connectivity is an account fact, not a general provider-availability proof:
    a declaration may allow a route with no credential. Callers may use it to
    derive a {!Selection.main} provider preference or as one input to their
    selected-route credential policy. *)

val source : t -> Credential.Source.t option
(** [source t] is the resolved credential source for {!present} and {!checked}
    accounts, and [None] for {!missing} accounts. *)

val credential_kind : t -> Credential.Kind.t option
(** [credential_kind t] is the resolved credential kind for {!present} and
    {!checked} accounts, and [None] for {!missing} accounts. *)

val fingerprint : t -> string option
(** [fingerprint t] is the resolved credential fingerprint, if one can be
    derived safely, and [None] otherwise.

    [fingerprint t] is always [None] for {!missing} accounts. *)

val checked_at : t -> timestamp option
(** [checked_at t] is the observation timestamp for checked accounts, if
    recorded, and [None] for missing or unchecked-present accounts. *)

val profile : t -> Profile.t option
(** [profile t] is the credential-free provider account profile observed by an
    account check, if any, and [None] for missing or unchecked-present accounts.
*)

val org : t -> Org.t option
(** [org t] is the credential-free organization or workspace scope observed by
    an account check, if any, and [None] for missing or unchecked-present
    accounts. *)

val problems : t -> Problem.t list
(** [problems t] is the sorted, duplicate-free list of checked account problems.
    It is [[]] for missing and unchecked-present accounts. *)

val models : t -> string list option
(** [models t] are provider-visible model ids revealed by a check, if any. *)

val model_available : t -> string -> [ `Available | `Unavailable | `Unknown ]
(** [model_available t model] is whether the check revealed [model] as visible
    to the account. [`Unknown] means the route was not checked or the check did
    not reveal model entitlement. The readiness projection restates this same
    trichotomy nominally as {!Model_readiness.Availability.t}. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] have the same provider, status,
    credential summary, checked facts, and normalized problems. *)

val jsont : t Jsont.t
(** [jsont] maps credential-free account facts to strict JSON objects tagged by
    ["status"] as ["missing"], ["present"], or ["checked"].

    The encoding preserves credential provenance, kind, bounded fingerprint, and
    checked facts, but cannot contain credential material because {!type:t} does
    not retain it. Decoding rejects unknown statuses and members, invalid nested
    values, negative timestamps, and fingerprints other than four characters. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics.

    The output contains no credential material and is not stable storage syntax.
*)

(** {1:outcomes Account operation outcomes} *)

module Discovery : sig
  (** One provider route in an account discovery batch.

      Discovery is pointwise: a rejected credential remains a value for its
      provider rather than failing or erasing the rest of the batch. *)

  type nonrec t =
    | Known of t
        (** Credential resolution produced account facts, including a
            {!Phase.Missing} account when no credential candidate resolved. *)
    | Resolution_failed of {
        provider : Mentat_llm.Provider.t;
            (** Provider whose selected candidate was rejected. *)
        error : Credential_error.t;
            (** Structured reason the candidate could not be resolved. *)
      }
        (** The selected candidate was invalid for its provider. No account
            facts exist for this route. *)

  val provider : t -> Mentat_llm.Provider.t
  (** [provider t] is the provider route represented by [t]. *)

  val connected : t -> bool
  (** [connected t] is [true] iff [t] is a known account with a connected
      credential. A resolution failure is never connected. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same known account or the
      same provider-local resolution failure. *)

  val jsont : t Jsont.t
  (** [jsont] maps discoveries to strict objects tagged as ["known"] or
      ["resolution_failed"]. Decoding rejects unknown tags and members. *)
end

module Logout : sig
  (** Credential-free logout settlement.

      The result combines the exact post-operation account discovery with the
      optional remote/local revocation settlement. Presentation layers derive
      messages from these facts; this vocabulary carries no UI policy. *)

  type remote =
    | Revoked  (** The provider-side revoke request succeeded. *)
    | Unsupported
        (** No revoke route exists or the provider reported revocation as
            unsupported. These observations have the same product meaning. *)
    | Failed of Problem.t
        (** The provider-side revoke attempt failed with a credential-free
            account problem. *)

  type local =
    | Removed  (** The observed stored credential is absent after settlement. *)
    | Superseded
        (** Another credential replaced the observed one and remains stored. *)

  type revocation =
    | Not_stored  (** No credential occupied the requested store slot. *)
    | Settled of {
        remote : remote;  (** Provider-side outcome. *)
        local : local;  (** Conditional local-removal outcome. *)
      }
        (** A stored credential was observed and remote/local settlement
            completed. *)

  type t = {
    current : Discovery.t;
        (** Discovery derived from the exact post-settlement store snapshot and
            the operation's process/environment snapshots. It describes this
            operation's settlement; a later operation may change the store. *)
    revocation : revocation option;
        (** [None] iff revocation was not requested. Requested revocation of an
            empty slot is [Some Not_stored]. *)
  }

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same post-operation
      discovery and revocation settlement. *)

  val jsont : t Jsont.t
  (** [jsont] maps logout results and their nested settlements to strict tagged
      JSON objects. Decoding rejects unknown tags and members. *)
end
