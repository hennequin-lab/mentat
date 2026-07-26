(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured provider-runtime operation errors.

    Readiness and settled account-operation facts travel as provider values.
    Failures that prevent settlement use this sum and expose one structured
    {!diagnostic}; the private OAuth interpreter lowers its errors here without
    publishing another error vocabulary. *)

type t =
  | Unknown_provider of Mentat_llm.Provider.t
      (** No built-in driver is registered for the provider. *)
  | Credential of {
      provider : Mentat_llm.Provider.t;
      error : Mentat_provider.Credential_error.t;
    }
      (** The selected provider route received a credential candidate that its
          declaration rejects. *)
  | Missing_credential of Mentat_llm.Provider.t
      (** A provider whose auth is mandatory resolved no usable credential. *)
  | Blocked_credential of {
      provider : Mentat_llm.Provider.t;
      problems : Mentat_provider.Account.Problem.t list;
    }  (** A credential resolved but a refresh settled it as fatally blocked. *)
  | Store of Store_error.t  (** The [auth.json] file machinery failed. *)
  | Invalid_base_url of { provider : Mentat_llm.Provider.t; message : string }
      (** A configured base URL for the provider is malformed. *)
  | Login of {
      provider : Mentat_llm.Provider.t;
      diagnostic : Mentat_diagnostic.t;
    }
      (** A login operation could not settle, including invalid direct API-key
          text and interactive protocol failure. The diagnostic may include
          protocol-specific context or a headless-use hint, but no credential
          material. *)

val diagnostic : t -> Mentat_diagnostic.t
(** [diagnostic t] is [t]'s structured, renderable diagnostic. It carries no
    credential material. *)

val message : t -> string
(** [message t] is {!diagnostic} rendered for plain-text logs and
    provider-boundary errors. It is not stable syntax. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats {!message}. *)

val to_llm : t -> Mentat_llm.Error.t
(** [to_llm t] renders a client-construction failure as a provider-boundary
    error for surfaces that want one: [kind = Auth] for {!Credential},
    {!Missing_credential}, and {!Blocked_credential}; [kind = Transport] for
    {!Invalid_base_url} and {!Store}; [kind = Provider] for {!Unknown_provider};
    and [kind = Auth] for {!Login}. The executable uses this mapping when
    per-request construction fails after a provider claim; transport failures
    already speak [Mentat_llm.Error.t] natively. *)
