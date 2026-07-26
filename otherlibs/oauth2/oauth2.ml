(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Result.Syntax

let is_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let pct_encode_form s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if is_unreserved c then Buffer.add_char b c
      else if Char.equal c ' ' then Buffer.add_char b '+'
      else Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let pct_decode_form s =
  String.map (function '+' -> ' ' | c -> c) s |> Uri.pct_decode

let decode_form query =
  let decode_pair pair =
    match String.split_first ~sep:"=" pair with
    | None -> (pct_decode_form pair, "")
    | Some (name, value) -> (pct_decode_form name, pct_decode_form value)
  in
  query |> String.split_all ~sep:"&" |> List.map decode_pair

let base64url_no_pad s =
  Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s

let encode_form params =
  params
  |> List.map (fun (name, value) ->
      pct_encode_form name ^ "=" ^ pct_encode_form value)
  |> String.concat "&"

module Params = struct
  type t = (string * string) list

  let empty = []
  let of_list params = params
  let to_list params = params
  let add name value params = params @ [ (name, value) ]
  let append a b = a @ b

  let reject ~names params =
    let rec loop = function
      | [] -> Ok params
      | (name, _) :: rest ->
          if List.exists (String.equal name) names then Error (`Reserved name)
          else loop rest
    in
    loop params

  let get_all name params =
    List.filter_map
      (fun (field, value) ->
        if String.equal field name then Some value else None)
      params

  let get_unique name params =
    match get_all name params with
    | [] -> Ok None
    | [ value ] -> Ok (Some value)
    | _ :: _ :: _ -> Error (`Duplicate name)
end

type random = int -> string

let random_bytes ~random n =
  let bytes = random n in
  if String.length bytes <> n then
    invalid_arg "OAuth2 random supplier returned the wrong number of bytes";
  bytes

module Client = struct
  type auth = [ `Public | `Secret_post of string | `Secret_basic of string ]
  type t = { id : string; auth : auth }

  let make ~id ?(auth = `Public) () = { id; auth }
  let id t = t.id

  let body_params t =
    match t.auth with
    | `Public -> Params.of_list [ ("client_id", t.id) ]
    | `Secret_basic _ -> Params.empty
    | `Secret_post secret ->
        Params.of_list [ ("client_id", t.id); ("client_secret", secret) ]

  let headers t =
    match t.auth with
    | `Secret_basic secret ->
        let credentials = pct_encode_form t.id ^ ":" ^ pct_encode_form secret in
        [ ("Authorization", "Basic " ^ Base64.encode_string credentials) ]
    | `Public | `Secret_post _ -> []
end

type malformed = {
  field : string option;
  message : string;
  raw : Jsont.json option;
}

let pp_malformed ppf t =
  match t.field with
  | None -> Format.fprintf ppf "malformed OAuth response: %s" t.message
  | Some field ->
      Format.fprintf ppf "malformed OAuth response field %S: %s" field t.message

module Param_error = struct
  type t = [ `Reserved of string ]

  let message = function
    | `Reserved name ->
        Printf.sprintf "reserved OAuth parameter %S was supplied by caller" name

  let pp ppf t = Format.pp_print_string ppf (message t)
end

let malformed ?field ?raw message = { field; message; raw }

module Json = struct
  let max_exact_json_int = 9_007_199_254_740_991.

  let field_values name = function
    | Jsont.Object (fields, _) ->
        List.filter_map
          (fun ((field, _), value) ->
            if String.equal field name then Some value else None)
          fields
    | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
    | Jsont.Array _ ->
        []

  let field name json =
    match field_values name json with value :: _ -> Some value | [] -> None

  let required name decode json =
    match field_values name json with
    | [] -> Error (malformed ~field:name "missing required field")
    | [ value ] -> decode name value
    | _ :: _ :: _ -> Error (malformed ~field:name "duplicate field")

  let optional name decode json =
    match field_values name json with
    | [] -> Ok None
    | [ Jsont.Null _ ] -> Ok None
    | [ value ] -> Result.map Option.some (decode name value)
    | _ :: _ :: _ -> Error (malformed ~field:name "duplicate field")

  let string field = function
    | Jsont.String (value, _) -> Ok value
    | value -> Error (malformed ~field ~raw:value "expected string")

  let int field = function
    | Jsont.Number (value, _)
      when Float.is_integer value
           && Float.abs value <= max_exact_json_int
           && value >= Float.of_int min_int
           && value <= Float.of_int max_int ->
        Ok (int_of_float value)
    | value -> Error (malformed ~field ~raw:value "expected integer")

  let non_negative_int field json =
    let* value = int field json in
    if value >= 0 then Ok value
    else Error (malformed ~field ~raw:json "expected non-negative integer")

  let positive_int field json =
    let* value = int field json in
    if value > 0 then Ok value
    else Error (malformed ~field ~raw:json "expected positive integer")

  let uri field json = Result.map Uri.of_string (string field json)

  let parse body =
    match Jsont_bytesrw.decode_string Jsont.json body with
    | Ok json -> Ok json
    | Error message -> Error (malformed ("invalid JSON: " ^ message))
end

module Error = struct
  type t = { code : string; description : string option; uri : Uri.t option }

  let make ~code ?description ?uri () = { code; description; uri }
  let code t = t.code
  let description t = t.description
  let uri t = t.uri

  let parse_json json =
    match Json.field "error" json with
    | None -> Ok None
    | Some _ ->
        let* code = Json.required "error" Json.string json in
        let* description = Json.optional "error_description" Json.string json in
        let* uri = Json.optional "error_uri" Json.uri json in
        Ok (Some { code; description; uri })

  let of_params params =
    let get name = Params.get_unique name params in
    match get "error" with
    | Error (`Duplicate name) -> Error (`Duplicate name)
    | Ok None -> Ok None
    | Ok (Some code) -> (
        match get "error_description" with
        | Error (`Duplicate name) -> Error (`Duplicate name)
        | Ok description -> (
            match get "error_uri" with
            | Error (`Duplicate name) -> Error (`Duplicate name)
            | Ok uri ->
                Ok
                  (Some
                     { code; description; uri = Option.map Uri.of_string uri }))
        )

  let pp ppf t =
    match (t.description, t.uri) with
    | Some description, Some uri ->
        Format.fprintf ppf "%s: %s (see %a)" t.code description Uri.pp_hum uri
    | Some description, None -> Format.fprintf ppf "%s: %s" t.code description
    | None, Some uri -> Format.fprintf ppf "%s (see %a)" t.code Uri.pp_hum uri
    | None, None -> Format.pp_print_string ppf t.code

  let to_string t = Format.asprintf "%a" pp t
end

type response_error = [ `Oauth of Error.t | `Malformed of malformed ]

module Response = struct
  type t = { status : int; headers : (string * string) list; body : string }
  type decode_error = [ response_error | `Http of t ]

  let is_success t = t.status >= 200 && t.status < 300

  let content_type t =
    List.find_map
      (fun (name, value) ->
        if String.equal (String.lowercase_ascii name) "content-type" then
          Some value
        else None)
      t.headers

  let json t = Json.parse t.body

  let oauth_error t =
    match json t with Ok json -> Error.parse_json json | Error _ -> Ok None

  let error_of_non_success t =
    match oauth_error t with
    | Ok (Some error) -> `Oauth error
    | Ok None -> `Http t
    | Error malformed -> `Malformed malformed

  let decode_json parse t =
    if is_success t then
      match json t with
      | Error malformed -> Error (`Malformed malformed)
      | Ok json -> (
          match parse json with
          | Ok value -> Ok value
          | Error (`Oauth error) -> Error (`Oauth error)
          | Error (`Malformed malformed) -> Error (`Malformed malformed))
    else Error (error_of_non_success t)
end

module Sha256 = struct
  (* A self-contained SHA-256 (FIPS 180-4). oauth2 vendors it so the PKCE S256
     challenge — BASE64URL(SHA256(verifier)) — needs no external hash library and
     the protocol core stays standalone. Arithmetic runs on native [int] masked
     to 32 bits; a partial sum of a few 32-bit words stays well under the 63-bit
     range, so nothing overflows before it is masked. *)

  let k =
    [|
      0x428a2f98;
      0x71374491;
      0xb5c0fbcf;
      0xe9b5dba5;
      0x3956c25b;
      0x59f111f1;
      0x923f82a4;
      0xab1c5ed5;
      0xd807aa98;
      0x12835b01;
      0x243185be;
      0x550c7dc3;
      0x72be5d74;
      0x80deb1fe;
      0x9bdc06a7;
      0xc19bf174;
      0xe49b69c1;
      0xefbe4786;
      0x0fc19dc6;
      0x240ca1cc;
      0x2de92c6f;
      0x4a7484aa;
      0x5cb0a9dc;
      0x76f988da;
      0x983e5152;
      0xa831c66d;
      0xb00327c8;
      0xbf597fc7;
      0xc6e00bf3;
      0xd5a79147;
      0x06ca6351;
      0x14292967;
      0x27b70a85;
      0x2e1b2138;
      0x4d2c6dfc;
      0x53380d13;
      0x650a7354;
      0x766a0abb;
      0x81c2c92e;
      0x92722c85;
      0xa2bfe8a1;
      0xa81a664b;
      0xc24b8b70;
      0xc76c51a3;
      0xd192e819;
      0xd6990624;
      0xf40e3585;
      0x106aa070;
      0x19a4c116;
      0x1e376c08;
      0x2748774c;
      0x34b0bcb5;
      0x391c0cb3;
      0x4ed8aa4a;
      0x5b9cca4f;
      0x682e6ff3;
      0x748f82ee;
      0x78a5636f;
      0x84c87814;
      0x8cc70208;
      0x90befffa;
      0xa4506ceb;
      0xbef9a3f7;
      0xc67178f2;
    |]

  let mask = 0xFFFFFFFF
  let rotr x n = (x lsr n) lor (x lsl (32 - n)) land mask

  let digest_string msg =
    let len = String.length msg in
    (* Pad to a multiple of 64 bytes with 0x80, zeros, then the 64-bit big-endian
       bit length; [total] is the least multiple of 64 with room for both. *)
    let total = (((len + 8) / 64) + 1) * 64 in
    let m = Bytes.make total '\000' in
    Bytes.blit_string msg 0 m 0 len;
    Bytes.set m len '\x80';
    let bit_len = len * 8 in
    for i = 0 to 7 do
      Bytes.set m (total - 1 - i) (Char.chr ((bit_len lsr (8 * i)) land 0xFF))
    done;
    let h0 = ref 0x6a09e667 and h1 = ref 0xbb67ae85 in
    let h2 = ref 0x3c6ef372 and h3 = ref 0xa54ff53a in
    let h4 = ref 0x510e527f and h5 = ref 0x9b05688c in
    let h6 = ref 0x1f83d9ab and h7 = ref 0x5be0cd19 in
    let w = Array.make 64 0 in
    for block = 0 to (total / 64) - 1 do
      let base = block * 64 in
      for t = 0 to 15 do
        let j = base + (t * 4) in
        w.(t) <-
          (Char.code (Bytes.get m j) lsl 24)
          lor (Char.code (Bytes.get m (j + 1)) lsl 16)
          lor (Char.code (Bytes.get m (j + 2)) lsl 8)
          lor Char.code (Bytes.get m (j + 3))
      done;
      for t = 16 to 63 do
        let s0 =
          rotr w.(t - 15) 7 lxor rotr w.(t - 15) 18 lxor (w.(t - 15) lsr 3)
        in
        let s1 =
          rotr w.(t - 2) 17 lxor rotr w.(t - 2) 19 lxor (w.(t - 2) lsr 10)
        in
        w.(t) <- (w.(t - 16) + s0 + w.(t - 7) + s1) land mask
      done;
      let a = ref !h0 and b = ref !h1 and c = ref !h2 and d = ref !h3 in
      let e = ref !h4 and f = ref !h5 and g = ref !h6 and h = ref !h7 in
      for t = 0 to 63 do
        let s1 = rotr !e 6 lxor rotr !e 11 lxor rotr !e 25 in
        let ch = !e land !f lxor (lnot !e land !g land mask) in
        let temp1 = (!h + s1 + ch + k.(t) + w.(t)) land mask in
        let s0 = rotr !a 2 lxor rotr !a 13 lxor rotr !a 22 in
        let maj = !a land !b lxor (!a land !c) lxor (!b land !c) in
        let temp2 = (s0 + maj) land mask in
        h := !g;
        g := !f;
        f := !e;
        e := (!d + temp1) land mask;
        d := !c;
        c := !b;
        b := !a;
        a := (temp1 + temp2) land mask
      done;
      h0 := (!h0 + !a) land mask;
      h1 := (!h1 + !b) land mask;
      h2 := (!h2 + !c) land mask;
      h3 := (!h3 + !d) land mask;
      h4 := (!h4 + !e) land mask;
      h5 := (!h5 + !f) land mask;
      h6 := (!h6 + !g) land mask;
      h7 := (!h7 + !h) land mask
    done;
    let out = Bytes.create 32 in
    let put i v =
      Bytes.set out (i * 4) (Char.chr ((v lsr 24) land 0xFF));
      Bytes.set out ((i * 4) + 1) (Char.chr ((v lsr 16) land 0xFF));
      Bytes.set out ((i * 4) + 2) (Char.chr ((v lsr 8) land 0xFF));
      Bytes.set out ((i * 4) + 3) (Char.chr (v land 0xFF))
    in
    put 0 !h0;
    put 1 !h1;
    put 2 !h2;
    put 3 !h3;
    put 4 !h4;
    put 5 !h5;
    put 6 !h6;
    put 7 !h7;
    Bytes.unsafe_to_string out
end

module Pkce = struct
  type t = { verifier : string; challenge : string }

  let verifier t = t.verifier
  let challenge t = t.challenge

  let is_verifier_char = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
    | _ -> false

  let validate_verifier verifier =
    let len = String.length verifier in
    if len < 43 then Error (`Invalid_verifier "shorter than 43 characters")
    else if len > 128 then
      Error (`Invalid_verifier "longer than 128 characters")
    else if String.for_all is_verifier_char verifier then Ok ()
    else Error (`Invalid_verifier "contains characters outside RFC 7636 syntax")

  let challenge_of_verifier verifier =
    verifier |> Sha256.digest_string |> base64url_no_pad

  let of_verifier verifier =
    Result.map
      (fun () -> { verifier; challenge = challenge_of_verifier verifier })
      (validate_verifier verifier)

  let generate ~random =
    let verifier = random_bytes ~random 32 |> base64url_no_pad in
    match of_verifier verifier with
    | Ok pkce -> pkce
    | Error (`Invalid_verifier reason) ->
        invalid_arg ("generated invalid PKCE verifier: " ^ reason)
end

module State = struct
  type t = string

  let generate ~random = random_bytes ~random 16 |> base64url_no_pad
  let of_string state = state
  let to_string state = state
  let pp ppf state = Format.pp_print_string ppf state
end

module Token = struct
  type t = {
    access_token : string;
    token_type : string;
    expires_in : int option;
    refresh_token : string option;
    scope : string list option;
    raw : Jsont.json;
  }

  type parse_error = response_error

  let access_token t = t.access_token
  let token_type t = t.token_type
  let expires_in t = t.expires_in
  let refresh_token t = t.refresh_token
  let scope t = t.scope
  let raw (t : t) = t.raw
  let field name (t : t) = Json.field name t.raw

  let field_string name t =
    match field name t with
    | Some (Jsont.String (value, _)) -> Some value
    | _ -> None

  let field_int name t =
    match field name t with
    | Some (Jsont.Number (value, _))
      when Float.is_integer value
           && Float.abs value <= Json.max_exact_json_int
           && value >= Float.of_int min_int
           && value <= Float.of_int max_int ->
        Some (int_of_float value)
    | Some (Jsont.Number _) -> None
    | Some (Jsont.String (value, _)) -> int_of_string_opt value
    | Some (Jsont.Null _)
    | Some (Jsont.Bool _)
    | Some (Jsont.Array _)
    | Some (Jsont.Object _)
    | None ->
        None

  let scope_of_string scope =
    scope |> String.split_on_char ' '
    |> List.filter (fun item -> not (String.equal item ""))

  let parse json =
    match Error.parse_json json with
    | Error malformed -> Error (`Malformed malformed)
    | Ok (Some error) -> Error (`Oauth error)
    | Ok None -> (
        match
          let* access_token = Json.required "access_token" Json.string json in
          let* token_type = Json.required "token_type" Json.string json in
          let* expires_in =
            Json.optional "expires_in" Json.non_negative_int json
          in
          let* refresh_token = Json.optional "refresh_token" Json.string json in
          let* scope = Json.optional "scope" Json.string json in
          Ok
            {
              access_token;
              token_type;
              expires_in;
              refresh_token;
              scope = Option.map scope_of_string scope;
              raw = json;
            }
        with
        | Ok token -> Ok token
        | Error malformed -> Error (`Malformed malformed))

  let pp ppf t =
    Format.fprintf ppf "@[<hov>{ token_type = %S; access_token = <redacted>"
      t.token_type;
    (match t.expires_in with
    | None -> ()
    | Some expires_in -> Format.fprintf ppf "; expires_in = %d" expires_in);
    (match t.refresh_token with
    | None -> ()
    | Some _ -> Format.fprintf ppf "; refresh_token = <redacted>");
    (match t.scope with
    | None -> ()
    | Some scope ->
        Format.fprintf ppf "; scope = [%s]" (String.concat "; " scope));
    Format.fprintf ppf " }@]"
end

module Request = struct
  type +'a t = {
    uri : Uri.t;
    headers : (string * string) list;
    params : (string * string) list;
    decode : Response.t -> ('a, Response.decode_error) result;
  }

  let post_form ~uri ?(headers = []) ~params ~decode () =
    { uri; headers; params; decode }

  let uri t = t.uri
  let headers (t : _ t) = t.headers
  let params (t : _ t) = t.params
  let body t = encode_form t.params
  let decode t response = t.decode response

  let with_header name value (t : _ t) =
    { t with headers = t.headers @ [ (name, value) ] }
end

module Device = struct
  type t = {
    device_code : string;
    user_code : string;
    verification_uri : Uri.t;
    verification_uri_complete : Uri.t option;
    expires_in : int;
    interval : int;
    raw : Jsont.json;
  }

  type parse_error = response_error

  let reserved = [ "client_id"; "client_secret"; "scope" ]

  let parse_verification_uri field json =
    let* uri = Json.uri field json in
    let scheme = Option.map String.lowercase_ascii (Uri.scheme uri) in
    match (scheme, Uri.host uri) with
    | (Some "http" | Some "https"), Some _ -> Ok uri
    | _ ->
        Error (malformed ~field ~raw:json "expected absolute http or https URI")

  let parse json =
    match Error.parse_json json with
    | Error malformed -> Error (`Malformed malformed)
    | Ok (Some error) -> Error (`Oauth error)
    | Ok None -> (
        match
          let* device_code = Json.required "device_code" Json.string json in
          let* user_code = Json.required "user_code" Json.string json in
          let* verification_uri =
            Json.required "verification_uri" parse_verification_uri json
          in
          let* verification_uri_complete =
            Json.optional "verification_uri_complete" parse_verification_uri
              json
          in
          let* expires_in =
            Json.required "expires_in" Json.non_negative_int json
          in
          let* interval = Json.optional "interval" Json.positive_int json in
          Ok
            {
              device_code;
              user_code;
              verification_uri;
              verification_uri_complete;
              expires_in;
              interval = Option.value interval ~default:5;
              raw = json;
            }
        with
        | Ok device -> Ok device
        | Error malformed -> Error (`Malformed malformed))

  let request ~client ~endpoint ?scope ?(extra = []) () =
    match Params.reject ~names:reserved extra with
    | Error (`Reserved name) -> Error (`Reserved name)
    | Ok extra ->
        let params = Client.body_params client in
        let params =
          match scope with
          | None | Some [] -> params
          | Some scope -> Params.add "scope" (String.concat " " scope) params
        in
        Ok
          (Request.post_form ~uri:endpoint ~headers:(Client.headers client)
             ~params:(Params.append params extra)
             ~decode:(Response.decode_json parse)
             ())

  let device_code t = t.device_code
  let user_code t = t.user_code
  let verification_uri t = t.verification_uri
  let verification_uri_complete t = t.verification_uri_complete
  let expires_in t = t.expires_in
  let interval t = t.interval
  let raw (t : t) = t.raw

  type poll_error = [ `Authorization_pending | `Slow_down | `Other of Error.t ]

  let classify_poll_error error =
    match Error.code error with
    | "authorization_pending" -> `Authorization_pending
    | "slow_down" -> `Slow_down
    | _ -> `Other error
end

module Form_request = struct
  type t = { params : Params.t; reserved : string list }

  let make ~reserved params = { params; reserved }

  let with_extra extra t =
    match Params.reject ~names:t.reserved extra with
    | Error (`Reserved name) -> Error (`Reserved name)
    | Ok extra -> Ok { t with params = Params.append t.params extra }

  let compile ~client ~endpoint ~decode t =
    Request.post_form ~uri:endpoint ~headers:(Client.headers client)
      ~params:(Params.append (Client.body_params client) t.params)
      ~decode ()
end

module Grant = struct
  type t = Form_request.t

  let names params = List.map fst params
  let with_extra extra t = Form_request.with_extra extra t
  let client_reserved = [ "client_id"; "client_secret" ]

  let authorization_code ~code ~redirect_uri ?pkce () =
    let params =
      Params.of_list
        [
          ("grant_type", "authorization_code");
          ("code", code);
          ("redirect_uri", Uri.to_string redirect_uri);
        ]
    in
    let params =
      match pkce with
      | None -> params
      | Some pkce -> Params.add "code_verifier" (Pkce.verifier pkce) params
    in
    Form_request.make
      ~reserved:
        ([ "grant_type"; "code"; "redirect_uri"; "code_verifier" ]
        @ client_reserved)
      params

  let refresh_token ~refresh_token ?scope () =
    let params =
      Params.of_list
        [ ("grant_type", "refresh_token"); ("refresh_token", refresh_token) ]
    in
    let params =
      match scope with
      | None | Some [] -> params
      | Some scope -> Params.add "scope" (String.concat " " scope) params
    in
    Form_request.make
      ~reserved:([ "grant_type"; "refresh_token"; "scope" ] @ client_reserved)
      params

  let client_credentials ?scope () =
    let params = Params.of_list [ ("grant_type", "client_credentials") ] in
    let params =
      match scope with
      | None | Some [] -> params
      | Some scope -> Params.add "scope" (String.concat " " scope) params
    in
    Form_request.make
      ~reserved:([ "grant_type"; "scope" ] @ client_reserved)
      params

  let device_code device =
    let params =
      Params.of_list
        [
          ("grant_type", "urn:ietf:params:oauth:grant-type:device_code");
          ("device_code", Device.device_code device);
        ]
    in
    Form_request.make
      ~reserved:([ "grant_type"; "device_code" ] @ client_reserved)
      params

  let extension ~grant_type ~params =
    match Params.reject ~names:([ "grant_type" ] @ client_reserved) params with
    | Error (`Reserved name) -> Error (`Reserved name)
    | Ok params ->
        Ok
          (Form_request.make
             ~reserved:(("grant_type" :: client_reserved) @ names params)
             (Params.add "grant_type" grant_type params))

  let request ~client ~endpoint t =
    Form_request.compile ~client ~endpoint
      ~decode:(Response.decode_json Token.parse)
      t
end

module Authorization = struct
  type t = {
    uri : Uri.t;
    state : State.t;
    pkce : Pkce.t option;
    redirect_uri : Uri.t;
  }

  type code = {
    value : string;
    code_redirect_uri : Uri.t;
    code_pkce : Pkce.t option;
  }

  module Callback_error = struct
    type t =
      [ `Oauth of Error.t
      | `Missing of string
      | `Duplicate of string
      | `State_mismatch
      | `Redirect_uri_mismatch ]
  end

  let reserved =
    [
      "response_type";
      "client_id";
      "redirect_uri";
      "state";
      "scope";
      "code_challenge";
      "code_challenge_method";
    ]

  let callback_reserved =
    [ "code"; "state"; "error"; "error_description"; "error_uri" ]

  let params_of_uri uri =
    (match Uri.verbatim_query uri with
      | None -> []
      | Some query -> decode_form query)
    |> Params.of_list

  let reject_endpoint_query endpoint =
    endpoint |> params_of_uri |> Params.reject ~names:reserved

  let reject_redirect_query redirect_uri =
    redirect_uri |> params_of_uri |> Params.reject ~names:callback_reserved

  let redirect_target uri = Uri.with_fragment (Uri.with_query' uri []) None

  let same_redirect_target expected actual =
    Uri.equal (redirect_target expected) (redirect_target actual)

  let includes_redirect_query expected actual =
    let rec remove_one item = function
      | [] -> None
      | x :: xs when x = item -> Some xs
      | x :: xs -> Option.map (fun xs -> x :: xs) (remove_one item xs)
    in
    let rec loop actual = function
      | [] -> true
      | expected :: expected_rest -> (
          match remove_one expected actual with
          | None -> false
          | Some actual -> loop actual expected_rest)
    in
    loop
      (Params.to_list (params_of_uri actual))
      (Params.to_list (params_of_uri expected))

  let make ~client ~endpoint ~redirect_uri ~state ?pkce ?scope ?(extra = []) ()
      =
    match reject_endpoint_query endpoint with
    | Error (`Reserved name) -> Error (`Reserved name)
    | Ok _ -> (
        match reject_redirect_query redirect_uri with
        | Error (`Reserved name) -> Error (`Reserved name)
        | Ok _ -> (
            match Params.reject ~names:reserved extra with
            | Error (`Reserved name) -> Error (`Reserved name)
            | Ok extra ->
                let params =
                  Params.of_list
                    [
                      ("response_type", "code");
                      ("client_id", Client.id client);
                      ("redirect_uri", Uri.to_string redirect_uri);
                      ("state", State.to_string state);
                    ]
                in
                let params =
                  match scope with
                  | None | Some [] -> params
                  | Some scope ->
                      Params.add "scope" (String.concat " " scope) params
                in
                let params =
                  match pkce with
                  | None -> params
                  | Some pkce ->
                      params
                      |> Params.add "code_challenge" (Pkce.challenge pkce)
                      |> Params.add "code_challenge_method" "S256"
                in
                let uri =
                  Uri.add_query_params' endpoint
                    (Params.to_list (Params.append params extra))
                in
                Ok { uri; state; pkce; redirect_uri }))

  let uri t = t.uri
  let state t = t.state
  let pkce t = t.pkce
  let redirect_uri t = t.redirect_uri

  let callback t uri =
    if
      (not (same_redirect_target t.redirect_uri uri))
      || not (includes_redirect_query t.redirect_uri uri)
    then Error `Redirect_uri_mismatch
    else
      let params = params_of_uri uri in
      let reject_error_metadata_without_error params =
        match Params.get_unique "error_description" params with
        | Error (`Duplicate name) -> Error (`Duplicate name)
        | Ok _ -> (
            match Params.get_unique "error_uri" params with
            | Error (`Duplicate name) -> Error (`Duplicate name)
            | Ok _ -> Ok ())
      in
      match Params.get_unique "state" params with
      | Error (`Duplicate name) -> Error (`Duplicate name)
      | Ok None -> Error (`Missing "state")
      | Ok (Some state) -> (
          if not (String.equal (State.to_string t.state) state) then
            Error `State_mismatch
          else
            match Error.of_params params with
            | Error (`Duplicate name) -> Error (`Duplicate name)
            | Ok (Some error) -> Error (`Oauth error)
            | Ok None -> (
                match reject_error_metadata_without_error params with
                | Error (`Duplicate name) -> Error (`Duplicate name)
                | Ok () -> (
                    match Params.get_unique "code" params with
                    | Error (`Duplicate name) -> Error (`Duplicate name)
                    | Ok None -> Error (`Missing "code")
                    | Ok (Some code) ->
                        Ok
                          {
                            value = code;
                            code_redirect_uri = t.redirect_uri;
                            code_pkce = t.pkce;
                          })))

  let code t = t.value

  let grant t =
    Grant.authorization_code ~code:t.value ~redirect_uri:t.code_redirect_uri
      ?pkce:t.code_pkce ()
end

module Revocation = struct
  type token_hint = [ `Access_token | `Refresh_token | `Other of string ]
  type t = Form_request.t

  let token_hint = function
    | `Access_token -> "access_token"
    | `Refresh_token -> "refresh_token"
    | `Other hint -> hint

  let reserved = [ "token"; "token_type_hint"; "client_id"; "client_secret" ]

  let make ~token ?hint () =
    let params = Params.of_list [ ("token", token) ] in
    let params =
      match hint with
      | None -> params
      | Some hint -> Params.add "token_type_hint" (token_hint hint) params
    in
    Form_request.make ~reserved params

  let with_extra extra t = Form_request.with_extra extra t

  let decode response =
    if Response.is_success response then Ok ()
    else Error (Response.error_of_non_success response)

  let request ~client ~endpoint t =
    Form_request.compile ~client ~endpoint ~decode t
end
