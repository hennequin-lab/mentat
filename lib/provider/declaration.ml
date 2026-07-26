(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message = invalid_arg ("Mentat_provider." ^ fn ^ ": " ^ message)

let check_optional_non_empty fn field = function
  | None -> ()
  | Some value ->
      if String.is_empty value then invalid fn (field ^ " must not be empty")

let check_no_duplicates fn field compare values =
  let rec loop = function
    | first :: second :: rest ->
        if compare first second = 0 then
          invalid fn (field ^ " contain duplicates");
        loop (second :: rest)
    | [] | [ _ ] -> ()
  in
  loop (List.sort compare values)

module Credential_error = Credential_error

type t = {
  id : Mentat_llm.Provider.t;
  display_name : string option;
  auth : Auth.t;
  models : Model.t list;
  default_model : Model.t option;
  dynamic : (string -> Model.t option) option;
}

let make id ?display_name ?(auth = Auth.none) ?default_model ?dynamic models =
  check_optional_non_empty "make" "display_name" display_name;
  List.iter
    (fun model ->
      if not (Mentat_llm.Provider.equal id (Model.provider model)) then
        invalid "make" "model provider does not match declaration provider")
    models;
  check_no_duplicates "make" "models" Mentat_llm.Model.compare
    (List.map Model.llm models);
  check_no_duplicates "make" "model ids" String.compare
    (List.map Model.id models);
  let default_model =
    match default_model with
    | None -> None
    | Some default -> (
        match
          List.find_opt
            (fun model ->
              Mentat_llm.Model.equal (Model.llm model) (Model.llm default))
            models
        with
        | Some declared ->
            if not (Model.selectable declared) then
              invalid "make" "default_model is not selectable";
            Some declared
        | None -> invalid "make" "default_model is not declared")
  in
  { id; display_name; auth; models; default_model; dynamic }

let id t = t.id
let display_name t = t.display_name
let auth t = t.auth
let default_model t = t.default_model
let models t = t.models

let model t llm =
  List.find_opt
    (fun model -> Mentat_llm.Model.equal llm (Model.llm model))
    t.models

(* The dynamic rule is trusted provider-author code; an inconsistent synthesis
   is a bug in it, so the check raises rather than returning a value. *)
let resolve_dynamic t requested =
  match t.dynamic with
  | None -> None
  | Some synthesize -> (
      match synthesize requested with
      | None -> None
      | Some model ->
          let actual = Model.provider model in
          if not (Mentat_llm.Provider.equal t.id actual) then
            invalid "resolve_dynamic"
              (Printf.sprintf
                 "dynamic model resolved to provider %S but declaration is %S"
                 (Mentat_llm.Provider.id actual)
                 (Mentat_llm.Provider.id t.id));
          if not (String.equal requested (Model.id model)) then
            invalid "resolve_dynamic"
              (Printf.sprintf
                 "dynamic model resolved id %S but %S was requested"
                 (Model.id model) requested);
          Some model)

let decode_environment_secret env ~source value =
  let kind = Auth.Env.kind env in
  match
    match kind with
    | Credential.Kind.Api_key -> Credential.Secret.api_key value
    | Credential.Kind.Bearer -> Credential.Secret.bearer value
    | Credential.Kind.OAuth -> Credential.Secret.oauth ~access_token:value ()
  with
  | secret -> Ok secret
  | exception Invalid_argument _ ->
      Error (Credential_error.Invalid_credential_text { source; kind })

let resolve_credential t ~process ~environment ~store ?name () =
  let accepted = Auth.accepted_kinds t.auth in
  let validate credential =
    let kind = Credential.kind credential in
    if List.exists (Credential.Kind.equal kind) accepted then
      Ok (Some credential)
    else
      Error
        (Credential_error.Unsupported_credential_kind
           { source = Credential.source credential; kind; accepted })
  in
  let from_environment () =
    let rec loop = function
      | [] -> Ok None
      | env :: rest -> (
          match List.assoc_opt (Auth.Env.name env) environment with
          | None | Some "" -> loop rest
          | Some value -> (
              let source = Credential.Source.env (Auth.Env.name env) in
              match decode_environment_secret env ~source value with
              | Error _ as error -> error
              | Ok secret ->
                  Ok (Some (Credential.make ~provider:t.id ~source secret))))
    in
    loop (Auth.env t.auth)
  in
  match
    List.find_opt
      (fun credential ->
        Mentat_llm.Provider.equal t.id (Credential.provider credential))
      process
  with
  | Some credential -> validate credential
  | None -> (
      match from_environment () with
      | Error _ as error -> error
      | Ok (Some credential) -> validate credential
      | Ok None -> (
          match Credential.Store.credential store ~provider:t.id ?name () with
          | Some credential -> validate credential
          | None -> Ok None))
