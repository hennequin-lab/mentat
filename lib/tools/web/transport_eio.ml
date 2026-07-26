(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Request = Transport.Request

let maximum_url_bytes = 2_000
let diagnostic_bytes = 4_096
let read_chunk_bytes = 4_096

let diagnostic ?(fallback = "network operation failed") text =
  let text =
    text |> Text_helpers.utf8_lossy |> Text_helpers.strip_ansi |> String.trim
  in
  if String.is_empty text then fallback
  else Text_helpers.valid_utf8_prefix text diagnostic_bytes

let exception_diagnostic ?fallback exn =
  diagnostic ?fallback (Printexc.to_string exn)

let duration_ms mono_clock started =
  let milliseconds =
    Mtime.span started (Eio.Time.Mono.now mono_clock) |> Mtime.Span.to_float_ns
    |> fun nanoseconds -> nanoseconds /. 1_000_000.
  in
  if Float.is_nan milliseconds || milliseconds <= 0. then 0
  else if milliseconds >= Float.of_int Int.max_int then Int.max_int
  else int_of_float milliseconds

let ipaddr_text (address : Eio.Net.Ipaddr.v4v6) =
  Format.asprintf "%a" Eio.Net.Ipaddr.pp address

let byte raw index = Char.code raw.[index]

(* The IANA special-purpose registries are the source of truth here. Most
   registered blocks are not globally reachable, but 192.0.0.9/32,
   192.0.0.10/32, and several allocations nested beneath 2001::/23 are explicit
   exceptions. Keep those exceptions beside their enclosing rejection so a
   broad prefix cannot silently overrule a more-specific registry entry. The
   globally reachable NAT64 prefix is admitted only when its embedded IPv4
   destination passes the same policy; IPv4-mapped addresses remain reserved. *)
let globally_routable_ipv4 raw =
  let a = byte raw 0 in
  let b = byte raw 1 in
  let c = byte raw 2 in
  let d = byte raw 3 in
  let ietf_protocol_assignment = a = 192 && b = 0 && c = 0 in
  if ietf_protocol_assignment then d = 9 || d = 10
  else
    not
      (a = 0 || a = 10 || a = 127 || a >= 224
      || (a = 100 && b >= 64 && b <= 127)
      || (a = 169 && b = 254)
      || (a = 172 && b >= 16 && b <= 31)
      || (a = 192 && b = 0 && c = 2)
      || (a = 192 && b = 88 && c = 99)
      || (a = 192 && b = 168)
      || (a = 198 && (b = 18 || b = 19))
      || (a = 198 && b = 51 && c = 100)
      || (a = 203 && b = 0 && c = 113))

let ipv6_prefix raw a b = byte raw 0 = a && byte raw 1 = b

let bytes_are_zero raw first last =
  let rec loop index =
    index > last || (byte raw index = 0 && loop (index + 1))
  in
  loop first

let ietf_protocol_assignment_exception raw =
  let singleton =
    byte raw 2 = 0
    && byte raw 3 = 1
    && bytes_are_zero raw 4 14
    &&
    let last = byte raw 15 in
    last >= 1 && last <= 3
  in
  singleton
  || (byte raw 2 = 0 && byte raw 3 = 3)
  || (byte raw 2 = 0 && byte raw 3 = 4 && byte raw 4 = 1 && byte raw 5 = 0x12)
  || (byte raw 2 = 0 && byte raw 3 land 0xf0 = 0x20)
  || (byte raw 2 = 0 && byte raw 3 land 0xf0 = 0x30)

let globally_routable_ipv6 raw =
  let first = byte raw 0 in
  let second = byte raw 1 in
  let ipv4_mapped =
    String.starts_with
      ~prefix:"\000\000\000\000\000\000\000\000\000\000\255\255" raw
  in
  let well_known_nat64 =
    byte raw 0 = 0
    && byte raw 1 = 0x64
    && byte raw 2 = 0xff
    && byte raw 3 = 0x9b
    && bytes_are_zero raw 4 11
  in
  if ipv4_mapped then false
  else if well_known_nat64 then globally_routable_ipv4 (String.sub raw 12 4)
  else
    let ietf_protocol_assignments =
      ipv6_prefix raw 0x20 0x01 && byte raw 2 land 0xfe = 0
    in
    let documentation_3fff =
      first = 0x3f && second = 0xff && byte raw 2 land 0xf0 = 0
    in
    first land 0xe0 = 0x20
    &&
    if ietf_protocol_assignments then ietf_protocol_assignment_exception raw
    else
      not
        (ipv6_prefix raw 0x20 0x02
        || (ipv6_prefix raw 0x20 0x01 && byte raw 2 = 0x0d && byte raw 3 = 0xb8)
        || documentation_3fff)

let globally_routable (address : Eio.Net.Ipaddr.v4v6) =
  let raw = (address :> string) in
  match String.length raw with
  | 4 -> globally_routable_ipv4 raw
  | 16 -> globally_routable_ipv6 raw
  | _ -> false

let transport_error text = Error (Transport.Transport (diagnostic text))

let vet_addresses ~allow_private_network ~url = function
  | [] ->
      let host = Option.value (Uri.host url) ~default:"" in
      transport_error ("DNS resolution returned no addresses for " ^ host)
  | first :: _ as addresses ->
      let rec vet = function
        | [] -> Ok first
        | `Unix _ :: _ ->
            Error
              (Transport.Protocol
                 {
                   head = None;
                   diagnostic = "DNS resolution returned a non-TCP address";
                 })
        | `Tcp (address, _) :: rest ->
            if allow_private_network || globally_routable address then vet rest
            else
              Error
                (Transport.Private_address
                   { url; address = ipaddr_text address })
      in
      vet addresses

let resolve ~net ~allow_private_network url =
  let host = Option.value (Uri.host url) ~default:"" in
  let service =
    match Uri.port url with
    | Some port -> string_of_int port
    | None -> (
        match Option.map String.lowercase_ascii (Uri.scheme url) with
        | Some "https" -> "443"
        | Some "http" -> "80"
        | Some _ | None -> "")
  in
  let addresses =
    try Ok (Eio.Net.getaddrinfo_stream ~service net host) with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    (* A resolver failure names its own cause; render that phrase rather than
       the raw [Eio.Io] value, whose repr spans lines and repeats the host. *)
    | Eio.Io (Eio.Net.E (Eio.Net.Address_lookup_failed code), _) ->
        transport_error
          ("DNS resolution failed for " ^ host ^ ": "
          ^ Eio.Net.Getaddrinfo_error.to_message code)
    | exn ->
        let detail =
          exception_diagnostic ~fallback:"name resolution failed" exn
        in
        transport_error ("DNS resolution failed for " ^ host ^ ": " ^ detail)
  in
  match addresses with
  | Error _ as error -> error
  | Ok addresses -> vet_addresses ~allow_private_network ~url addresses

type tls = Ready of Tls.Config.client | Unavailable of string

let tls () =
  try
    Mirage_crypto_rng_unix.use_default ();
    match Ca_certs.authenticator () with
    | Error (`Msg message) ->
        Unavailable
          (diagnostic ~fallback:"system trust roots are unavailable" message)
    | Ok authenticator -> (
        match Tls.Config.client ~authenticator () with
        | Ok config -> Ready config
        | Error (`Msg message) ->
            Unavailable
              (diagnostic ~fallback:"TLS configuration failed" message))
  with exn ->
    Unavailable (exception_diagnostic ~fallback:"TLS initialization failed" exn)

type connection = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Std.r

let close connection =
  Eio.Cancel.protect (fun () -> Eio.Resource.close connection)

let close_then_reraise connection exn =
  let backtrace = Printexc.get_raw_backtrace () in
  close connection;
  Printexc.raise_with_backtrace exn backtrace

let tls_connection config uri raw =
  let host = Option.value (Uri.host uri) ~default:"" in
  match Ipaddr.of_string host with
  | Ok ip -> (Tls_eio.client_of_flow ~ip config raw :> connection)
  | Error _ ->
      let host = Domain_name.host_exn (Domain_name.of_string_exn host) in
      (Tls_eio.client_of_flow ~host config raw :> connection)

let connect ~sw ~net ~tls uri address =
  try
    let raw = (Eio.Net.connect ~sw net address :> connection) in
    match Option.map String.lowercase_ascii (Uri.scheme uri) with
    | Some "http" -> Ok raw
    | Some "https" -> (
        match tls with
        | Unavailable message ->
            close raw;
            transport_error ("TLS is unavailable: " ^ message)
        | Ready config -> (
            try Ok (tls_connection config uri raw) with
            | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn ->
                close_then_reraise raw exn
            | exn ->
                close raw;
                let detail =
                  exception_diagnostic ~fallback:"TLS handshake failed" exn
                in
                transport_error ("TLS handshake failed: " ^ detail)))
    | Some _ | None ->
        close raw;
        Error
          (Transport.Protocol
             { head = None; diagnostic = "unsupported request URL scheme" })
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
      let detail = exception_diagnostic ~fallback:"connection failed" exn in
      transport_error ("connection failed: " ^ detail)

let contains_line_control text =
  String.exists (function '\000' | '\r' | '\n' -> true | _ -> false) text

let response_status response =
  Cohttp.Response.status response |> Cohttp.Code.code_of_status

let response_content_type response =
  Cohttp.Response.headers response |> fun headers ->
  Cohttp.Header.get headers "content-type"

type content_length = Absent | Length of int64 | Invalid

let response_content_length response =
  let values =
    Cohttp.Response.headers response |> fun headers ->
    Cohttp.Header.get_multi headers "content-length"
  in
  match values with
  | [] -> Absent
  | [ value ] -> (
      let value = String.trim value in
      if
        String.is_empty value
        || not
             (String.for_all (function '0' .. '9' -> true | _ -> false) value)
      then Invalid
      else
        match Int64.of_string_opt value with
        | Some length when Int64.compare length 0L >= 0 -> Length length
        | Some _ | None -> Invalid)
  | _ -> Invalid

let redirect_status = function
  | 301 | 302 | 303 | 307 | 308 -> true
  | _ -> false

let effective_port uri =
  match Uri.port uri with
  | Some port -> port
  | None -> (
      match Option.map String.lowercase_ascii (Uri.scheme uri) with
      | Some "https" -> 443
      | Some "http" -> 80
      | Some _ | None -> 0)

let same_authority left right =
  let scheme uri =
    Option.map String.lowercase_ascii (Uri.scheme uri)
    |> Option.value ~default:""
  in
  let host uri =
    Option.map String.lowercase_ascii (Uri.host uri) |> Option.value ~default:""
  in
  String.equal (scheme left) (scheme right)
  && String.equal (host left) (host right)
  && effective_port left = effective_port right

let normalize_redirect current location =
  let invalid message =
    Error (diagnostic ~fallback:"invalid redirect" message)
  in
  try
    let target = Uri.resolve "" current (Uri.of_string location) in
    let target = Uri.with_fragment target None in
    let scheme = Option.map String.lowercase_ascii (Uri.scheme target) in
    let host = Option.map String.lowercase_ascii (Uri.host target) in
    match (scheme, host) with
    | Some (("http" | "https") as scheme), Some host
      when not (String.is_empty host) -> (
        if Option.is_some (Uri.userinfo target) then
          invalid "redirect target contains user information"
        else
          let target =
            target |> fun uri ->
            Uri.with_scheme uri (Some scheme) |> fun uri ->
            Uri.with_host uri (Some host)
          in
          let text = Uri.to_string target in
          if String.length text > maximum_url_bytes then
            invalid "redirect target exceeds 2000 bytes"
          else
            match Uri.port target with
            | Some port when port < 1 || port > 65_535 ->
                invalid "redirect target has an invalid port"
            | Some _ | None -> Ok target)
    | Some _, Some _ | Some _, None | None, Some _ | None, None ->
        invalid "redirect target must be an absolute HTTP or HTTPS URL"
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | _ -> invalid "redirect target is not a valid URL"

type read_result =
  | Read_body of string
  | Read_cancelled
  | Read_too_large of int
  | Read_protocol of string
  | Read_transport of string

let read_body ~cancelled ~limit body =
  let chunk = Cstruct.create read_chunk_bytes in
  let buffer = Buffer.create (min limit read_chunk_bytes) in
  let rec loop received =
    if cancelled () then Read_cancelled
    else
      match Eio.Flow.single_read body chunk with
      | exception End_of_file -> Read_body (Buffer.contents buffer)
      | exception ((Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn) ->
          raise exn
      | exception exn ->
          let detail =
            exception_diagnostic ~fallback:"response body read failed" exn
          in
          Read_transport ("response body read failed: " ^ detail)
      | 0 -> Read_protocol "response body produced an empty read"
      | count ->
          let observed = received + count in
          if observed > limit then Read_too_large observed
          else begin
            Buffer.add_string buffer
              (Cstruct.to_string (Cstruct.sub chunk 0 count));
            loop observed
          end
  in
  loop 0

type hop =
  | Hop_body of { status : int; content_type : string option; body : string }
  | Hop_redirect of {
      status : int;
      content_type : string option;
      target : Uri.t;
    }
  | Hop_cancelled
  | Hop_too_large of {
      status : int;
      content_type : string option;
      received : int;
    }
  | Hop_protocol of {
      status : int;
      content_type : string option;
      diagnostic : string;
    }

let call ~sw ~headers ~meth ~body client uri =
  let cohttp_meth =
    match meth with
    | Transport.Request.Get -> `GET
    | Transport.Request.Post -> `POST
  in
  let body = Option.map Cohttp_eio.Body.of_string body in
  try Ok (Cohttp_eio.Client.call client ~sw ~headers ?body cohttp_meth uri) with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | _ ->
      Error
        (Transport.Protocol
           {
             head = None;
             diagnostic = "peer returned an invalid HTTP response";
           })

let consume_body ~cancelled ~request ~status ~content_type response body =
  match response_content_length response with
  | Invalid ->
      Ok
        (Hop_protocol
           {
             status;
             content_type;
             diagnostic = "peer returned an invalid Content-Length";
           })
  | Length length
    when Int64.compare length (Int64.of_int (Request.max_body_bytes request))
         > 0 ->
      Ok (Hop_too_large { status; content_type; received = 0 })
  | Absent | Length _ -> (
      match
        read_body ~cancelled ~limit:(Request.max_body_bytes request) body
      with
      | Read_body body -> Ok (Hop_body { status; content_type; body })
      | Read_cancelled -> Ok Hop_cancelled
      | Read_too_large received ->
          Ok (Hop_too_large { status; content_type; received })
      | Read_protocol diagnostic ->
          Ok (Hop_protocol { status; content_type; diagnostic })
      | Read_transport diagnostic -> transport_error diagnostic)

let fetch_hop ~sw ~net ~tls ~cancelled ~request uri =
  if cancelled () then Ok Hop_cancelled
  else
    match
      resolve ~net
        ~allow_private_network:(Request.allow_private_network request)
        uri
    with
    | Error _ as error -> error
    | Ok address -> (
        if cancelled () then Ok Hop_cancelled
        else
          match connect ~sw ~net ~tls uri address with
          | Error _ as error -> error
          | Ok connection ->
              Fun.protect
                ~finally:(fun () -> close connection)
                (fun () ->
                  let client =
                    Cohttp_eio.Client.make_generic
                      (fun ~sw:client_sw requested ->
                        Eio.Switch.check client_sw;
                        if not (Uri.equal requested uri) then
                          invalid_arg
                            "web fetch client attempted an unexpected request";
                        connection)
                  in
                  let headers =
                    Cohttp.Header.of_list (Request.headers request)
                  in
                  match
                    call ~sw ~headers ~meth:(Request.meth request)
                      ~body:(Request.body request) client uri
                  with
                  | Error _ as error -> error
                  | Ok (response, body) ->
                      let status = response_status response in
                      let content_type = response_content_type response in
                      if
                        Option.exists contains_line_control content_type
                        || status < 100 || status > 599
                      then
                        Ok
                          (Hop_protocol
                             {
                               status = max 100 (min 599 status);
                               content_type = None;
                               diagnostic =
                                 "peer returned invalid response metadata";
                             })
                      else if redirect_status status then
                        let location =
                          Cohttp.Response.headers response |> fun headers ->
                          Cohttp.Header.get headers "location"
                        in
                        match location with
                        | None ->
                            consume_body ~cancelled ~request ~status
                              ~content_type response body
                        | Some location -> (
                            match normalize_redirect uri location with
                            | Ok target ->
                                Ok
                                  (Hop_redirect { status; content_type; target })
                            | Error diagnostic ->
                                Ok
                                  (Hop_protocol
                                     { status; content_type; diagnostic }))
                      else
                        consume_body ~cancelled ~request ~status ~content_type
                          response body))

let response_head ~mono_clock ~started ~effective_url ~status content_type =
  try
    Ok
      (Transport.make_response_head ~effective_url ~status ?content_type
         ~duration_ms:(duration_ms mono_clock started)
         ())
  with Invalid_argument _ ->
    Error
      (Transport.Protocol
         { head = None; diagnostic = "peer returned invalid response metadata" })

let rec fetch ~sw ~net ~tls ~mono_clock ~started ~cancelled ~request ~authority
    ~remaining uri =
  match fetch_hop ~sw ~net ~tls ~cancelled ~request uri with
  | Error _ as error -> error
  | Ok Hop_cancelled -> Error Transport.Cancelled
  | Ok (Hop_body { status; content_type; body }) -> (
      match
        response_head ~mono_clock ~started ~effective_url:uri ~status
          content_type
      with
      | Error _ as error -> error
      | Ok head -> Ok (Transport.Body { head; body }))
  | Ok (Hop_too_large { status; content_type; received }) -> (
      match
        response_head ~mono_clock ~started ~effective_url:uri ~status
          content_type
      with
      | Error _ as error -> error
      | Ok head ->
          Error
            (Transport.Response_too_large
               { head; limit = Request.max_body_bytes request; received }))
  | Ok (Hop_protocol { status; content_type; diagnostic }) -> (
      match
        response_head ~mono_clock ~started ~effective_url:uri ~status
          content_type
      with
      | Error _ as error -> error
      | Ok head -> Error (Transport.Protocol { head = Some head; diagnostic }))
  | Ok (Hop_redirect { status; content_type; target }) -> (
      match
        response_head ~mono_clock ~started ~effective_url:uri ~status
          content_type
      with
      | Error _ as error -> error
      | Ok head ->
          if not (same_authority authority target) then
            Ok (Transport.Redirect { head; target })
          else if remaining = 0 then
            Error
              (Transport.Too_many_redirects
                 { head; limit = Request.max_redirects request })
          else
            fetch ~sw ~net ~tls ~mono_clock ~started ~cancelled ~request
              ~authority ~remaining:(remaining - 1) target)

let make ~net ~mono_clock =
  let tls = tls () in
  fun ~sw ~cancelled request ->
    Eio.Switch.check sw;
    if cancelled () then Error Transport.Cancelled
    else
      let started = Eio.Time.Mono.now mono_clock in
      let timeout =
        Eio.Time.Timeout.seconds mono_clock
          (float_of_int (Request.timeout_ms request) /. 1_000.)
      in
      try
        Eio.Time.Timeout.run_exn timeout (fun () ->
            let authority = Request.url request in
            fetch ~sw ~net ~tls ~mono_clock ~started ~cancelled ~request
              ~authority
              ~remaining:(Request.max_redirects request)
              authority)
      with Eio.Time.Timeout -> Error Transport.Timed_out

module For_testing = struct
  let globally_routable = globally_routable
  let vet_addresses = vet_addresses
end
