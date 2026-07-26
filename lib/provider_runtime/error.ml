(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Unknown_provider of Mentat_llm.Provider.t
  | Credential of {
      provider : Mentat_llm.Provider.t;
      error : Mentat_provider.Credential_error.t;
    }
  | Missing_credential of Mentat_llm.Provider.t
  | Blocked_credential of {
      provider : Mentat_llm.Provider.t;
      problems : Mentat_provider.Account.Problem.t list;
    }
  | Store of Store_error.t
  | Invalid_base_url of { provider : Mentat_llm.Provider.t; message : string }
  | Login of {
      provider : Mentat_llm.Provider.t;
      diagnostic : Mentat_diagnostic.t;
    }

let text = function
  | Unknown_provider provider ->
      "unknown provider: " ^ Mentat_llm.Provider.id provider
  | Credential { provider; error } ->
      Printf.sprintf "credential for provider %s: %s"
        (Mentat_llm.Provider.id provider)
        (Mentat_provider.Credential_error.message error)
  | Missing_credential provider ->
      "no credential for provider: " ^ Mentat_llm.Provider.id provider
  | Blocked_credential { provider; problems } ->
      let problems =
        String.concat ", "
          (List.map Mentat_provider.Account.Problem.to_string problems)
      in
      Printf.sprintf "credential blocked for provider %s: %s"
        (Mentat_llm.Provider.id provider)
        (if String.equal problems "" then "blocked" else problems)
  | Store store_error -> Store_error.message store_error
  | Invalid_base_url { provider; message } ->
      Printf.sprintf "invalid base URL for provider %s: %s"
        (Mentat_llm.Provider.id provider)
        message
  | Login { diagnostic; _ } -> Mentat_diagnostic.to_string diagnostic

let diagnostic_of_text text =
  Mentat_diagnostic.of_text_or ~fallback:"provider runtime operation failed"
    text

let diagnostic = function
  | Login { diagnostic; _ } -> diagnostic
  | error -> diagnostic_of_text (text error)

let message error = Mentat_diagnostic.to_string (diagnostic error)
let pp ppf t = Format.pp_print_string ppf (message t)

let to_llm t =
  let kind, provider =
    match t with
    | Credential { provider; _ } -> (Mentat_llm.Error.Auth, Some provider)
    | Missing_credential provider -> (Mentat_llm.Error.Auth, Some provider)
    | Blocked_credential { provider; _ } ->
        (Mentat_llm.Error.Auth, Some provider)
    | Invalid_base_url { provider; _ } ->
        (Mentat_llm.Error.Transport, Some provider)
    | Store _ -> (Mentat_llm.Error.Transport, None)
    | Unknown_provider provider -> (Mentat_llm.Error.Provider, Some provider)
    | Login { provider; _ } -> (Mentat_llm.Error.Auth, Some provider)
  in
  Mentat_llm.Error.make ~kind ?provider (message t)
