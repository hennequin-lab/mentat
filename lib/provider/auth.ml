(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message = invalid_arg ("Mentat_provider." ^ fn ^ ": " ^ message)

let valid_env_name name =
  let len = String.length name in
  let valid_first = function '_' -> true | c -> Char.Ascii.is_letter c in
  let valid_rest = function
    | '_' -> true
    | c -> Char.Ascii.is_letter c || Char.Ascii.is_digit c
  in
  let rec loop index =
    index = len
    || (valid_rest (String.unsafe_get name index) && loop (index + 1))
  in
  len > 0 && valid_first (String.unsafe_get name 0) && loop 1

(* Login-method-id and env-name grammars are validated locally so declarations
   never depend on catalog code to check a tag. *)
let valid_tag tag =
  let is_first c = Char.Ascii.is_lower c in
  let is_rest c =
    Char.Ascii.is_lower c || Char.Ascii.is_digit c || Char.equal c '-'
    || Char.equal c '_'
  in
  let len = String.length tag in
  let rec loop index =
    index = len || (is_rest (String.unsafe_get tag index) && loop (index + 1))
  in
  len > 0 && is_first (String.unsafe_get tag 0) && loop 1

let check_no_duplicates fn field compare values =
  let rec loop = function
    | first :: second :: rest ->
        if compare first second = 0 then
          invalid fn (field ^ " contain duplicates");
        loop (second :: rest)
    | [] | [ _ ] -> ()
  in
  loop (List.sort compare values)

module Env = struct
  type t = { name : string; kind : Credential.Kind.t }

  let declare ~fn ~name ~kind =
    if not (valid_env_name name) then invalid fn "name is invalid";
    { name; kind }

  let api_key name =
    declare ~fn:"Auth.Env.api_key" ~name ~kind:Credential.Kind.Api_key

  let bearer name =
    declare ~fn:"Auth.Env.bearer" ~name ~kind:Credential.Kind.Bearer

  let oauth_access_token name =
    declare ~fn:"Auth.Env.oauth_access_token" ~name ~kind:Credential.Kind.OAuth

  let name t = t.name
  let kind t = t.kind
  let pp ppf t = Format.fprintf ppf "%s:%a" t.name Credential.Kind.pp t.kind
end

module Login = struct
  module Progress = struct
    type t =
      | Browser_url of Uri.t
      | Listening of { redirect_uri : Uri.t }
      | Device_challenge of {
          url : Uri.t;
          user_code : string;
          expires_in : int;
        }
  end

  module Id = struct
    type t = string

    let make id =
      if not (valid_tag id) then
        invalid "Auth.Login.Id.make" "login method id is invalid";
      id

    let of_string id = if valid_tag id then Some id else None
    let to_string t = t
    let equal = String.equal
    let compare = String.compare
    let pp ppf t = Format.pp_print_string ppf t
  end

  module Protocol = struct
    type t =
      | Api_key
      | OAuth2_device_code of {
          client : Oauth2.Client.t;
          device_endpoint : Uri.t;
          token_endpoint : Uri.t;
          scope : string list;
          extra : (string * string) list;
        }
      | OAuth2_authorization_code of {
          client : Oauth2.Client.t;
          authorization_endpoint : Uri.t;
          token_endpoint : Uri.t;
          redirect_uri : Uri.t option;
          scope : string list;
          extra : (string * string) list;
          pkce : bool;
        }
      | Provider_defined of {
          protocol : Id.t;
          credential_kind : Credential.Kind.t;
        }
      | External of {
          instructions : string option;
          credential_kind : Credential.Kind.t option;
        }
  end

  type t = { id : Id.t; label : string; protocol : Protocol.t }

  let make ~id ~label protocol =
    if String.is_empty label then
      invalid "Auth.Login.make" "label must not be empty";
    { id; label; protocol }

  let api_key ?(id = Id.make "api-key") ?(label = "API key") () =
    make ~id ~label Protocol.Api_key

  let oauth2_device_code ?(id = Id.make "device-code") ?(label = "Device code")
      ?(scope = []) ?(extra = []) ~client ~device_endpoint ~token_endpoint () =
    make ~id ~label
      (Protocol.OAuth2_device_code
         { client; device_endpoint; token_endpoint; scope; extra })

  let oauth2_authorization_code ?(id = Id.make "browser") ?(label = "Browser")
      ?(scope = []) ?(extra = []) ?redirect_uri ?(pkce = true) ~client
      ~authorization_endpoint ~token_endpoint () =
    make ~id ~label
      (Protocol.OAuth2_authorization_code
         {
           client;
           authorization_endpoint;
           token_endpoint;
           redirect_uri;
           scope;
           extra;
           pkce;
         })

  let id t = t.id
  let label t = t.label
  let protocol t = t.protocol
end

type t = { required : bool; env : Env.t list; login : Login.t list }

let make ?required ?(env = []) ?(login = []) () =
  check_no_duplicates "Auth.make" "environment declarations" String.compare
    (List.map Env.name env);
  check_no_duplicates "Auth.make" "login methods" Login.Id.compare
    (List.map Login.id login);
  let has_method = not (List.is_empty env && List.is_empty login) in
  let required = Option.value required ~default:has_method in
  if required && not has_method then
    invalid "Auth.make" "required auth declares no env or login method";
  { required; env; login }

let none = make ()
let required t = t.required
let env t = t.env
let logins t = t.login

let login_by_id t id =
  List.find_opt (fun login -> Login.Id.equal id (Login.id login)) t.login

let protocol_kind = function
  | Login.Protocol.Api_key -> Some Credential.Kind.Api_key
  | Login.Protocol.OAuth2_device_code _
  | Login.Protocol.OAuth2_authorization_code _ ->
      Some Credential.Kind.OAuth
  | Login.Protocol.Provider_defined { credential_kind; _ } ->
      Some credential_kind
  | Login.Protocol.External { credential_kind; _ } -> credential_kind

let accepted_kinds t =
  let kinds =
    List.map Env.kind t.env
    @ List.filter_map
        (fun login -> protocol_kind (Login.protocol login))
        t.login
  in
  List.rev
    (List.fold_left
       (fun acc kind ->
         if List.exists (Credential.Kind.equal kind) acc then acc
         else kind :: acc)
       [] kinds)
