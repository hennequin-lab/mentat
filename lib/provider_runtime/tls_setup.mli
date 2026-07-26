(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared low-level TLS for the credential runtime's own HTTP.

    One lazy authenticator seeds the default RNG and decodes the system trust
    store on first handshake, off the boot path (the home frame renders without
    waiting on entropy or X.509 decoding) and paid once for every TLS consumer.
    The account-check GET and the local-weight download both borrow this. The
    OAuth interpreter uses [mentat_oauth2_eio]'s own TLS, hit only during
    explicit login or refresh flows. *)

exception Setup_failed
(** Payload-free failure to construct the system CA/TLS configuration or
    validate an HTTPS endpoint identity. *)

val web_http_client : Eio_unix.Stdenv.base -> Cohttp_eio.Client.t
(** [web_http_client env] is a Cohttp client over [env]'s network with system-CA
    TLS, for the local-weight download path.

    An HTTPS request raises [Setup_failed] if the system TLS setup or endpoint
    identity cannot be constructed. Transport clients using this general
    connector must lower that categorical failure at their own boundary. *)

val get :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  headers:(string * string) list ->
  string ->
  (int * string, unit) result
(** [get ~sw ~env ~headers url] performs one deadline-bound HTTPS GET and
    returns the response status and a body bounded to 1 MiB. URI parsing, system
    TLS setup, endpoint identity, DNS resolution, transport, and timeout
    failures are [Error ()]. Cancellation and unrelated exceptions escape. *)
