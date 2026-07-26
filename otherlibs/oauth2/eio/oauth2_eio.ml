(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let log_src = Logs.Src.create "mentat.oauth2" ~doc:"OAuth 2.0 Eio transport"

module Log = (val Logs.src_log log_src : Logs.LOG)

type response = Oauth2.Response.t

module Error = struct
  type transport =
    [ `Network_unavailable
    | `Invalid_endpoint_identity
    | `Response_body_read_failed ]

  type t = [ Oauth2.Response.decode_error | `Transport of transport ]

  let pp_transport fmt = function
    | `Network_unavailable ->
        Format.pp_print_string fmt "network transport unavailable"
    | `Invalid_endpoint_identity ->
        Format.pp_print_string fmt "invalid HTTPS endpoint identity"
    | `Response_body_read_failed ->
        Format.pp_print_string fmt "OAuth response body could not be read"

  let pp fmt = function
    | `Transport e -> pp_transport fmt e
    | `Oauth _ ->
        Format.pp_print_string fmt "OAuth request rejected by provider"
    | `Malformed _ -> Format.pp_print_string fmt "malformed OAuth response"
    | `Http response ->
        Format.fprintf fmt "HTTP error %d: body length %d"
          response.Oauth2.Response.status
          (String.length response.Oauth2.Response.body)
end

let has_header name headers =
  List.exists
    (fun header ->
      let key = fst header in
      String.equal (String.lowercase_ascii key) name)
    headers

let form_headers headers =
  if has_header "content-type" headers then headers
  else ("Content-Type", "application/x-www-form-urlencoded") :: headers

let default_max_response_body_size = 1_048_576

let response_body ~max_response_body_size body =
  let max_size =
    if max_response_body_size = max_int then max_int
    else max_response_body_size + 1
  in
  match Eio.Buf_read.(parse take_all) ~max_size body with
  | Ok body -> Ok body
  | Error (`Msg _) -> Error `Response_body_read_failed

let read_response_body ~max_response_body_size body =
  match response_body ~max_response_body_size body with
  | result -> result
  | exception (Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _) ->
      Error `Network_unavailable

exception Invalid_endpoint_identity

let transport_error ~host error =
  let reason =
    match error with
    | `Network_unavailable -> "network-unavailable"
    | `Invalid_endpoint_identity -> "invalid-endpoint-identity"
    | `Response_body_read_failed -> "response-body-read-failed"
  in
  Log.debug (fun m -> m "oauth request failed host=%s reason=%s" host reason);
  Error error

(* Cohttp-eio currently reports resolver failure through [Failure]. Normalize
   only the dependency call: unrelated failures in request construction,
   response handling, and decoders remain programming faults. *)
let cohttp_post http_client ~sw ~headers ~body uri =
  match Cohttp_eio.Client.post http_client ~sw ~headers ~body uri with
  | response -> Ok response
  | exception
      (Failure _ | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ | End_of_file) ->
      Error `Network_unavailable

let post http_client ~sw
    ?(max_response_body_size = default_max_response_body_size) ~uri
    ?(headers = []) ~body () =
  if max_response_body_size < 0 then
    invalid_arg "OAuth response body limit must not be negative"
  else (
    Eio.Switch.check sw;
    let host = Option.value ~default:"<none>" (Uri.host uri) in
    try
      Eio.Switch.run ~name:"oauth2-post" @@ fun request_sw ->
      let headers = Cohttp.Header.of_list (form_headers headers) in
      let body = Cohttp_eio.Body.of_string body in
      match cohttp_post http_client ~sw:request_sw ~headers ~body uri with
      | Error error -> transport_error ~host error
      | Ok (response, body) -> (
          let status =
            Cohttp.Code.code_of_status (Cohttp.Response.status response)
          in
          let headers =
            Cohttp.Header.to_list (Cohttp.Response.headers response)
          in
          match read_response_body ~max_response_body_size body with
          | Error e -> transport_error ~host e
          | Ok body ->
              Log.info (fun m ->
                  m "oauth request finished host=%s status=%d body_bytes=%d"
                    host status (String.length body));
              Ok
                {
                  Oauth2.Response.status;
                  Oauth2.Response.headers;
                  Oauth2.Response.body;
                })
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | Eio.Time.Timeout | Eio.Io _ -> transport_error ~host `Network_unavailable
    | Invalid_endpoint_identity ->
        transport_error ~host `Invalid_endpoint_identity)

let send http_client ~sw ?max_response_body_size request =
  match
    post http_client ~sw ?max_response_body_size
      ~uri:(Oauth2.Request.uri request)
      ~headers:(Oauth2.Request.headers request)
      ~body:(Oauth2.Request.body request)
      ()
  with
  | Error e -> Error (`Transport e)
  | Ok response ->
      Oauth2.Request.decode request response
      |> Result.map_error (fun e -> (e :> Error.t))

type https =
  Uri.t -> [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t -> Tls_eio.t

type tls_error = [ `System_ca_unavailable | `Tls_configuration_failed ]

let make_https () =
  Mirage_crypto_rng_unix.use_default ();
  match Ca_certs.authenticator () with
  | Error (`Msg _) -> Error `System_ca_unavailable
  | Ok authenticator -> (
      match Tls.Config.client ~authenticator () with
      | Error (`Msg _) -> Error `Tls_configuration_failed
      | Ok default_tls_config ->
          let tls_config uri =
            match Uri.host uri with
            | None -> raise Invalid_endpoint_identity
            | Some name -> (
                match Ipaddr.of_string name with
                | Ok ip -> (
                    match Tls.Config.client ~authenticator ~ip () with
                    | Ok tls_config -> (tls_config, None)
                    | Error (`Msg _) -> raise Invalid_endpoint_identity)
                | Error _ -> (
                    match Domain_name.of_string name with
                    | Error (`Msg _) -> raise Invalid_endpoint_identity
                    | Ok domain -> (
                        match Domain_name.host domain with
                        | Error (`Msg _) -> raise Invalid_endpoint_identity
                        | Ok host -> (default_tls_config, Some host))))
          in
          let handler uri raw =
            let tls_config, host = tls_config uri in
            Tls_eio.client_of_flow ?host tls_config raw
          in
          Ok handler)

let make_client ?https net = Cohttp_eio.Client.make ~https net

let make_tls_client net =
  match make_https () with
  | Error error -> Error error
  | Ok https -> Ok (make_client ~https net)
