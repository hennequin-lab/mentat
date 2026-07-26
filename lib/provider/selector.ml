(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Error = struct
  type t =
    | Empty
    | Missing_separator
    | Empty_provider
    | Empty_model
    | Invalid_provider of { input : string; message : string }

  let message = function
    | Empty -> "model selector must not be empty"
    | Missing_separator -> "model selector must be in the form provider/model"
    | Empty_provider -> "model selector provider must not be empty"
    | Empty_model -> "model selector model must not be empty"
    | Invalid_provider { input; message } ->
        Printf.sprintf "model selector provider %S is invalid: %s" input message

  let pp ppf error = Format.pp_print_string ppf (message error)
end

type t = { provider : Mentat_llm.Provider.t; id : string }

let make ~provider ~id =
  if String.is_empty id then
    invalid_arg "Mentat_provider.Selector.make: id must not be empty";
  if not (String.equal (String.trim id) id) then
    invalid_arg
      "Mentat_provider.Selector.make: id must not have surrounding whitespace";
  { provider; id }

let of_model model =
  make
    ~provider:(Mentat_llm.Model.provider model)
    ~id:(Mentat_llm.Model.id model)

let of_string raw =
  let value = String.trim raw in
  if String.is_empty value then Error Error.Empty
  else
    match String.split_first ~sep:"/" value with
    | None -> Error Error.Missing_separator
    | Some ("", _) -> Error Error.Empty_provider
    | Some (provider, id) -> (
        (* Trim the id segment so every selector carries a trim-stable id, the
           same invariant [make] enforces; round-trips are unconditional. *)
        match String.trim id with
        | "" -> Error Error.Empty_model
        | id -> (
            match Mentat_llm.Provider.make provider with
            | provider -> Ok { provider; id }
            | exception Invalid_argument message ->
                Error (Error.Invalid_provider { input = provider; message })))

let provider t = t.provider
let id t = t.id
let to_string t = Mentat_llm.Provider.id t.provider ^ "/" ^ t.id

let equal a b =
  Mentat_llm.Provider.equal a.provider b.provider && String.equal a.id b.id

let compare a b =
  match Mentat_llm.Provider.compare a.provider b.provider with
  | 0 -> String.compare a.id b.id
  | order -> order

let jsont =
  Jsont.map ~kind:"model selector"
    ~dec:(fun s ->
      match of_string s with
      | Ok selector -> selector
      | Error error -> Jsont.Error.msg Jsont.Meta.none (Error.message error))
    ~enc:to_string Jsont.string
