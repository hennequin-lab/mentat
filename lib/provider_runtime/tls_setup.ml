(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let max_body_size = 1_048_576
let check_timeout_s = 10.0

exception Setup_failed

let client_config ~authenticator ?ip () =
  match Tls.Config.client ~authenticator ?ip () with
  | Ok config -> config
  | Error (`Msg _) -> raise Setup_failed

let endpoint_config ~authenticator ~default_tls_config uri =
  match Uri.host uri with
  | None -> raise Setup_failed
  | Some value -> (
      match Ipaddr.of_string value with
      | Ok ip -> (client_config ~authenticator ~ip (), None)
      | Error _ -> (
          match Domain_name.of_string value with
          | Error (`Msg _) -> raise Setup_failed
          | Ok domain -> (
              match Domain_name.host domain with
              | Error (`Msg _) -> raise Setup_failed
              | Ok host -> (default_tls_config, Some host))))

let https ~authenticator =
  let default_tls_config = client_config ~authenticator () in
  fun uri raw ->
    let tls_config, host =
      endpoint_config ~authenticator ~default_tls_config uri
    in
    Tls_eio.client_of_flow ?host tls_config raw

(* Seeding the default RNG and decoding the system trust store are both needed
   before the first TLS handshake, neither is needed to assemble a client, and
   both are costly. Deferring them through [Lazy] keeps that cost off the boot
   path and pays it once for every TLS consumer. *)
let tls_authenticator =
  lazy
    (Mirage_crypto_rng_unix.use_default ();
     match Ca_certs.authenticator () with
     | Ok authenticator -> authenticator
     | Error (`Msg _) -> raise Setup_failed)

let web_http_client env =
  Cohttp_eio.Client.make
    ~https:
      (Some
         (fun uri raw ->
           https ~authenticator:(Lazy.force tls_authenticator) uri raw))
    (Eio.Stdenv.net env)

let preflight_https uri =
  match Uri.scheme uri with
  | Some scheme when String.equal (String.lowercase_ascii scheme) "https" ->
      let authenticator = Lazy.force tls_authenticator in
      let default_tls_config = client_config ~authenticator () in
      ignore (endpoint_config ~authenticator ~default_tls_config uri)
  | Some _ | None -> ()

let cohttp_headers headers =
  List.fold_left
    (fun acc (name, value) -> Cohttp.Header.add acc name value)
    (Cohttp.Header.init ()) headers

let read_body body =
  let chunk = Cstruct.create 4096 in
  let buffer = Buffer.create 1024 in
  let rec loop remaining =
    if remaining > 0 then
      match Eio.Flow.single_read body chunk with
      | exception End_of_file -> ()
      | count ->
          let count = min count remaining in
          Buffer.add_string buffer
            (Cstruct.to_string (Cstruct.sub chunk 0 count));
          loop (remaining - count)
  in
  loop max_body_size;
  Buffer.contents buffer

(* Cohttp-eio currently reports resolver failure through [Failure]. Keep that
   third-party exception normalization scoped to the call itself, so faults in
   response handling and our code still escape. *)
let call client ~sw ~headers uri =
  match Cohttp_eio.Client.call client ~sw ~headers `GET uri with
  | response -> Ok response
  | exception
      (Failure _ | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ | End_of_file) ->
      Error ()

let read_response_body body =
  match read_body body with
  | body -> Ok body
  | exception (Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _) -> Error ()

let get ~sw ~env ~headers url =
  let uri =
    match Uri.of_string url with
    | uri -> Ok uri
    | exception Invalid_argument _ -> Error ()
  in
  match uri with
  | Error () -> Error ()
  | Ok uri -> (
      match
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) check_timeout_s
          (fun () ->
            preflight_https uri;
            let client = web_http_client env in
            match call client ~sw ~headers:(cohttp_headers headers) uri with
            | Error () -> Error ()
            | Ok (response, body) ->
                let status =
                  Cohttp.Code.code_of_status (Cohttp.Response.status response)
                in
                Result.map
                  (fun body -> (status, body))
                  (read_response_body body))
      with
      | result -> result
      | exception Eio.Time.Timeout -> Error ()
      | exception Eio.Io _ -> Error ()
      | exception Setup_failed -> Error ())
