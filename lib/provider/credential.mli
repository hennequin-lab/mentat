(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Secret-bearing provider credentials.

    [Credential] is the secret-bearing half of the account vocabulary:
    {!Secret.t} payloads, routed {!type:t} candidates, and inert {!Store.t}
    snapshots may all contain secret material and must not be written to logs,
    session events, model-visible conversation state, or diagnostics.
    Credential-free account readiness facts are {!Account.t}, a different type
    that never carries secret material.

    The module performs no I/O. It does not read environment variables, open
    credential stores, use keychains, run OAuth flows, or refresh tokens.
    [mentat.provider_runtime] owns those effects and lowers their observations
    into this vocabulary. *)

type timestamp = int64
(** Unix timestamp in seconds.

    Constructors that accept timestamps reject negative values. The type does
    not encode a clock source; callers decide whether a timestamp comes from a
    provider response, local wall clock, or persisted observation. *)

(** {1:kinds Kinds} *)

module Kind : sig
  (** Credential kind without credential material. *)
  type t =
    | Api_key  (** API-key credential material. *)
    | Bearer  (** Bearer-token credential material. *)
    | OAuth  (** OAuth access-token credential material. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same credential kind. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics.

      The output contains no credential material and is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps kinds to and from their stable spellings: ["api_key"],
      ["bearer"], ["oauth"]. *)
end

(** {1:secrets Secrets} *)

module Secret : sig
  type t
  (** Provider- and source-free secret credential payload.

      Secret values result from API-key entry, OAuth flows, token refresh,
      environment decoding, and credential-store loading. They deliberately
      carry no provider identity or provenance; the credential boundary attaches
      those. *)

  val api_key : string -> t
  (** [api_key key] is an API-key secret. Raises [Invalid_argument] if [key] is
      empty or is not valid UTF-8 text. *)

  val bearer : string -> t
  (** [bearer token] is a bearer-token secret. Raises [Invalid_argument] if
      [token] is empty or is not valid UTF-8 text. *)

  val oauth :
    access_token:string ->
    ?refresh_token:string ->
    ?expires_at:timestamp ->
    ?account_id:string ->
    unit ->
    t
  (** [oauth ~access_token ()] is an OAuth token secret.

      [expires_at], if present, is the absolute expiration timestamp in Unix
      seconds. [account_id], if present, is provider-specific credential
      metadata and is not used as an [Account.Profile.t].

      Raises [Invalid_argument] if [access_token], [refresh_token], or
      [account_id] is empty or not valid UTF-8 text, or if [expires_at] is
      negative. *)

  val kind : t -> Kind.t
  (** [kind t] is [t]'s credential kind without secret material. *)

  val fingerprint : t -> string option
  (** [fingerprint t] is a short redacted identifier for [t], if one can be
      derived safely.

      API-key and bearer fingerprints are the last 4 characters of the material;
      material shorter than 8 characters has no fingerprint, since no suffix
      would avoid disclosing most of the secret. OAuth fingerprints apply the
      same last-4 rule to the credential account id when present — which is
      stable across token refresh — and otherwise to the access token.

      Fingerprints identify which credential is in use across credential-free
      surfaces; readiness is recomputed per process. Fingerprints are safe to
      show: they contain at most 4 characters of secret material. *)

  val expires_at : t -> timestamp option
  (** [expires_at t] is [t]'s expiry for OAuth secrets that declare one, and
      [None] otherwise.

      Expiry is credential-free metadata: callers use it to decide when a
      refresh is needed without exposing token material. *)

  val has_refresh_token : t -> bool
  (** [has_refresh_token t] is [true] iff [t] is an OAuth secret carrying a
      refresh token. Reveals no secret material. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] contain exactly the same credential
      material and metadata.

      This is state equality for conditional credential-store updates. It is not
      constant-time and must not be used to authenticate untrusted input. *)

  val expose :
    t ->
    api_key:(key:string -> 'a) ->
    bearer:(token:string -> 'a) ->
    oauth:
      (access_token:string ->
      refresh_token:string option ->
      expires_at:timestamp option ->
      account_id:string option ->
      'a) ->
    'a
  (** [expose t ~api_key ~bearer ~oauth] applies the matching callback to [t]'s
      secret-bearing payload.

      {b Warning.} This is the deliberate escape hatch for provider drivers,
      auth-flow interpreters, and credential persistence. The supplied callbacks
      must not write secret material to logs, diagnostics, model-visible state,
      or credential-free session metadata. *)
end

(** {1:names Names and sources} *)

module Name : sig
  (** Provider-local names for saved credentials. *)

  type t
  (** Provider-local name for a saved credential.

      Names select saved secrets within a provider namespace. They are not
      provider account ids and do not imply which credential is active for a
      run; configuration or runtime choice decides which name to read. *)

  val default : t
  (** [default] is the conventional name used when no name is configured. *)

  val make : string -> t
  (** [make s] is credential name [s].

      [s] must be non-empty and contain only ASCII letters, digits, [_], [-],
      and [.]. Raises [Invalid_argument] otherwise. *)

  val to_string : t -> string
  (** [to_string t] is [t]'s stable storage spelling. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same credential name. *)

  val compare : t -> t -> int
  (** [compare a b] orders credential names by storage spelling. The order is
      compatible with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps names to and from their storage spelling. Decoding errors on
      any string {!make} rejects. *)
end

module Source : sig
  (** Credential provenance.

      [Process] credentials were supplied directly by the host process.
      [Env name] credentials came from environment variable [name]. [Store name]
      credentials came from a provider-local stored credential name. Stored
      credential names are not secret, but may still be privacy-sensitive
      display or routing metadata.

      The variant is private: construct with the functions below, destructure by
      pattern matching. *)
  type t = private Process | Env of string | Store of Name.t

  val process : t
  (** [process] is process-local credential provenance. *)

  val env : string -> t
  (** [env name] is environment-variable credential provenance.

      [name] must be non-empty shell-compatible environment-variable syntax: an
      ASCII letter or [_] followed by ASCII letters, digits, or [_]. Raises
      [Invalid_argument] otherwise. *)

  val store : ?name:Name.t -> unit -> t
  (** [store ?name ()] is stored credential provenance. [name] defaults to
      {!Name.default}. *)

  val name : t -> string option
  (** [name t] is the environment variable name for [Env], the credential name's
      storage spelling for [Store], and [None] for [Process]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same credential source. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics.

      The output contains no credential material and is not stable storage
      syntax. *)

  val jsont : t Jsont.t
  (** [jsont] maps sources to strict tagged JSON objects. [Process] carries no
      additional member; [Env] and [Store] carry their validated ["name"].
      Decoding rejects unknown tags and members. *)
end

(** {1:credentials Credentials} *)

type t
(** One provider credential candidate.

    Credential values may contain secret material. They must not be written to
    logs, session events, model-visible conversation state, or diagnostics. Use
    [Account.t] for credential-free account facts.

    Credentials are abstract so callers cannot accidentally inspect secret
    fields. Provider drivers that must attach credentials to requests use
    {!secret} and {!Secret.expose}. *)

val make : provider:Mentat_llm.Provider.t -> source:Source.t -> Secret.t -> t
(** [make ~provider ~source secret] routes [secret] to [provider] with
    provenance [source].

    The function does not validate [secret] with the provider and performs no
    I/O. Provider drivers decide whether the secret kind is supported. *)

val provider : t -> Mentat_llm.Provider.t
(** [provider t] is [t]'s provider route. *)

val source : t -> Source.t
(** [source t] is [t]'s credential provenance. *)

val kind : t -> Kind.t
(** [kind t] is [t]'s credential kind without credential material. *)

val fingerprint : t -> string option
(** [fingerprint t] is {!Secret.fingerprint} of [t]'s secret. *)

val secret : t -> Secret.t
(** [secret t] is [t]'s provider/source-free secret payload.

    {b Warning.} The returned value is secret-bearing. Prefer {!kind} unless the
    caller explicitly needs to persist or use the secret; inspect the payload
    with {!Secret.expose}. *)

(** {1:stores Stores} *)

module Store : sig
  type credential := t

  type t
  (** A user-scoped credential-store snapshot.

      A store is inert provider/name-indexed data answering which secret is
      saved under a provider credential name. It does not choose which name a
      run should use and has no active-account or active-name invariant.

      The store provides no filesystem, keychain, locking, migration, or
      encryption behavior. Callers own how snapshots are loaded and saved. *)

  val empty : t
  (** [empty] contains no stored credentials. *)

  val of_list : (Mentat_llm.Provider.t * Name.t * Secret.t) list -> t
  (** [of_list bindings] is a store snapshot containing [bindings].

      Raises [Invalid_argument] if a provider/name pair appears more than once.
  *)

  val names : t -> provider:Mentat_llm.Provider.t -> Name.t list
  (** [names t ~provider] is [provider]'s saved credential names in
      deterministic {!Name.compare} order. *)

  val secret :
    t ->
    provider:Mentat_llm.Provider.t ->
    ?name:Name.t ->
    unit ->
    Secret.t option
  (** [secret t ~provider ?name ()] is the stored secret for [provider]/[name],
      if present. [name] defaults to {!Name.default}. *)

  val credential :
    t ->
    provider:Mentat_llm.Provider.t ->
    ?name:Name.t ->
    unit ->
    credential option
  (** [credential t ~provider ?name ()] is the stored credential for
      [provider]/[name], if present.

      [name] defaults to {!Name.default}. Returned credentials use
      {!Source.store} with the selected name as their provenance. *)

  val set : provider:Mentat_llm.Provider.t -> ?name:Name.t -> Secret.t -> t -> t
  (** [set ~provider ?name secret t] stores [secret] under [provider]/[name],
      replacing any existing secret for that provider/name pair. [name] defaults
      to {!Name.default}. The returned store preserves deterministic
      provider/name ordering. *)

  val remove : provider:Mentat_llm.Provider.t -> ?name:Name.t -> t -> t
  (** [remove ~provider ?name t] is [t] without [provider]/[name]. [name]
      defaults to {!Name.default}. Removing an absent binding leaves the store
      unchanged. The store is the last argument so removals pipeline like
      {!set}. *)

  val bindings :
    ?provider:Mentat_llm.Provider.t ->
    t ->
    (Mentat_llm.Provider.t * Name.t * Secret.t) list
  (** [bindings ?provider t] is [t]'s stored provider/name/secret bindings in
      deterministic provider/name order. If [provider] is supplied, only its
      bindings are returned.

      {b Warning.} The returned list contains secret material. It is intended
      for persistence and trusted store editing, not diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps store snapshots to JSON, including secret credential
      material.

      {b Warning.} This codec is persistence syntax for trusted storage, not
      diagnostic output.

      The decoder accepts only version [1]. It rejects unknown fields, duplicate
      provider or name fields, invalid provider ids, invalid names, malformed
      secrets, contradictory per-kind fields, and obsolete formats. *)
end
