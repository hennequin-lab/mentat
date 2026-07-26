(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Provider authentication declarations.

    An auth declaration describes what credential sources and login methods a
    provider offers. It reads no environment, opens no browser, performs no
    OAuth request, prompts no user, and mutates no store. *)

module Env : sig
  (** Provider-local environment credential declarations. *)

  type t
  (** The type for an environment credential declaration. It states how one
      environment variable is interpreted as provider/source-free credential
      material; it does not read the variable or carry a provider id.
      [Mentat_provider.resolve_credential] interprets declared variables against
      an environment snapshot. *)

  val api_key : string -> t
  (** [api_key name] declares API-key material in environment variable [name].

      Raises [Invalid_argument] if [name] is not an environment variable name: a
      non-empty ASCII identifier starting with a letter or ['_'] and followed by
      letters, digits, or ['_']. *)

  val bearer : string -> t
  (** [bearer name] declares bearer-token material in environment variable
      [name]. Raises [Invalid_argument] as {!api_key}. *)

  val oauth_access_token : string -> t
  (** [oauth_access_token name] declares OAuth access-token material in
      environment variable [name]. Raises [Invalid_argument] as {!api_key}. *)

  val name : t -> string
  (** [name t] is [t]'s environment variable name. *)

  val kind : t -> Credential.Kind.t
  (** [kind t] is the credential kind a value of [t]'s variable decodes to. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. The output contains no credential
      material. *)
end

module Login : sig
  (** Provider-declared login methods. *)

  module Progress : sig
    (** Ephemeral, presentation-neutral progress from an interactive login.

        Values are display capabilities for the frontend currently running the
        flow. They may contain a short-lived authorization URL or user code and
        must not be logged, copied into a session transcript, or written to a
        durable store. *)

    type t =
      | Browser_url of Uri.t
          (** The authorization URL the frontend should open. *)
      | Listening of {
          redirect_uri : Uri.t;
              (** The loopback redirect URI whose callback listener is ready. *)
        }
      | Device_challenge of {
          url : Uri.t;  (** Verification URL the user should open. *)
          user_code : string;  (** Code the user should enter at [url]. *)
          expires_in : int;  (** Remaining challenge lifetime in seconds. *)
        }
  end

  module Id : sig
    (** Provider-local login method ids.

        Ids also tag {!Protocol.Provider_defined} flows, so the matching driver
        dispatches on a checked value rather than a free-form string. *)

    type t
    (** The type for a login method id. *)

    val make : string -> t
    (** [make s] is login method id [s]. [s] must start with a lowercase ASCII
        letter and then contain lowercase letters, digits, ['-'], or ['_'].
        Raises [Invalid_argument] otherwise; this is trusted construction from
        built-in data. Parse untrusted input with {!of_string}. *)

    val of_string : string -> t option
    (** [of_string s] parses [s] as a login method id — for example the id
        carried by a Login command from a frontend. Invalid ids are [None]. *)

    val to_string : t -> string
    (** [to_string t] is [t]'s spelling. *)

    val equal : t -> t -> bool
    (** [equal a b] is [true] iff [a] and [b] are the same id. *)

    val compare : t -> t -> int
    (** [compare a b] orders ids by spelling. The order is compatible with
        {!equal}. *)

    val pp : Format.formatter -> t -> unit
    (** [pp ppf t] formats [t]'s spelling. *)
  end

  module Protocol : sig
    (** Provider login protocol declarations. Values are static parameters; they
        carry no callbacks or effectful login code. *)

    (** The type for a login protocol declaration. Hosts decide how to present
        and run a protocol. *)
    type t =
      | Api_key  (** Manual API-key entry. *)
      | OAuth2_device_code of {
          client : Oauth2.Client.t;
          device_endpoint : Uri.t;
          token_endpoint : Uri.t;
          scope : string list;
          extra : (string * string) list;
        }  (** Standard OAuth 2.0 device-code flow. *)
      | OAuth2_authorization_code of {
          client : Oauth2.Client.t;
          authorization_endpoint : Uri.t;
          token_endpoint : Uri.t;
          redirect_uri : Uri.t option;
          scope : string list;
          extra : (string * string) list;
          pkce : bool;
        }  (** Standard OAuth 2.0 authorization-code flow. *)
      | Provider_defined of {
          protocol : Id.t;
          credential_kind : Credential.Kind.t;
        }
          (** A provider-specific flow that Mentat drives but that is not a
              standard OAuth protocol. [protocol] is the checked id of the
              driver that runs it; [credential_kind] is the material it
              produces. *)
      | External of {
          instructions : string option;
          credential_kind : Credential.Kind.t option;
        }
          (** Provider login completed outside Mentat. [instructions], when
              present, is user-facing guidance; [credential_kind], when known,
              is the material the flow ultimately yields. *)
  end

  type t
  (** The type for a provider-declared login method. It is a pure declaration
      carrying no runnable login function. *)

  val make : id:Id.t -> label:string -> Protocol.t -> t
  (** [make ~id ~label protocol] is a login method. Raises [Invalid_argument] if
      [label] is empty. *)

  val api_key : ?id:Id.t -> ?label:string -> unit -> t
  (** [api_key ()] declares an API-key login method. [id] defaults to
      [Id.make "api-key"] and [label] to ["API key"]. Raises [Invalid_argument]
      if [label] is empty. *)

  val oauth2_device_code :
    ?id:Id.t ->
    ?label:string ->
    ?scope:string list ->
    ?extra:(string * string) list ->
    client:Oauth2.Client.t ->
    device_endpoint:Uri.t ->
    token_endpoint:Uri.t ->
    unit ->
    t
  (** [oauth2_device_code ~client ~device_endpoint ~token_endpoint ()] declares
      a standard OAuth device-code login method. [id] defaults to
      [Id.make "device-code"], [label] to ["Device code"], [scope] to [[]], and
      [extra] to [[]]. Raises [Invalid_argument] if [label] is empty. *)

  val oauth2_authorization_code :
    ?id:Id.t ->
    ?label:string ->
    ?scope:string list ->
    ?extra:(string * string) list ->
    ?redirect_uri:Uri.t ->
    ?pkce:bool ->
    client:Oauth2.Client.t ->
    authorization_endpoint:Uri.t ->
    token_endpoint:Uri.t ->
    unit ->
    t
  (** [oauth2_authorization_code ~client ~authorization_endpoint ~token_endpoint
       ()] declares a standard OAuth browser login method. [id] defaults to
      [Id.make "browser"], [label] to ["Browser"], [scope] to [[]], [extra] to
      [[]], and [pkce] to [true]. Raises [Invalid_argument] if [label] is empty.
  *)

  val id : t -> Id.t
  (** [id t] is [t]'s login method id. *)

  val label : t -> string
  (** [label t] is [t]'s user-facing method label. *)

  val protocol : t -> Protocol.t
  (** [protocol t] is [t]'s protocol declaration. *)
end

type t
(** The type for a provider authentication declaration. Environment and login
    lists preserve declaration order; environment names and login ids are unique
    within one declaration. *)

val none : t
(** [none] declares no auth inputs or login methods. It accepts {e nothing}: its
    {!accepted_kinds} is empty, so credential resolution never resolves a
    credential for it — without candidates the result is empty, and a process or
    store candidate is rejected as unsupported rather than being silently used.
*)

val make : ?required:bool -> ?env:Env.t list -> ?login:Login.t list -> unit -> t
(** [make ?required ?env ?login ()] is an auth declaration.

    [env] and [login] default to [[]]. [required] states whether a usable
    credential is mandatory; it defaults to [true] when a method is declared and
    to [false] otherwise. Declaring [~required:false] alongside methods
    describes optional authentication.

    Raises [Invalid_argument] if two environment declarations use the same
    variable name, if two login methods use the same id, or if [required] is
    [true] while no method is declared. *)

val required : t -> bool
(** [required t] is whether a usable credential is mandatory. [false] for
    {!none} and for optional-auth declarations. *)

val accepted_kinds : t -> Credential.Kind.t list
(** [accepted_kinds t] is the set of credential kinds [t]'s declared methods can
    produce, deduplicated in declaration order (environment declarations first,
    then login methods). It is empty for {!none}; an empty set accepts
    {e nothing}, not everything. *)

val env : t -> Env.t list
(** [env t] is [t]'s environment credential declarations in declaration order.
*)

val logins : t -> Login.t list
(** [logins t] is [t]'s declared login methods in declaration order. *)

val login_by_id : t -> Login.Id.t -> Login.t option
(** [login_by_id t id] is the login method declared as [id], if any. *)
