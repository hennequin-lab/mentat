(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Focused transport tests for the security decision that a real DNS server
   cannot exercise hermetically through the CLI. These call the same pure
   vetting path as the product resolver: current IANA IPv4/IPv6 boundaries,
   mixed-answer rejection, and the exact socket address selected for the
   subsequent pinned connection. HTTP/TLS lifecycle and cancellation remain
   black-box contracts in web-fetch-transport.t. *)

open Windtrap
module Tools = Mentat_tools

let address text =
  match Ipaddr.of_string text with
  | Ok address -> Eio.Net.Ipaddr.of_raw (Ipaddr.to_octets address)
  | Error (`Msg message) -> failf "invalid test address %S: %s" text message

let tcp text port = `Tcp (address text, port)
let address_text address = Format.asprintf "%a" Eio.Net.Ipaddr.pp address

let sockaddr_text = function
  | `Tcp (address, port) ->
      Printf.sprintf "tcp:%s:%d" (address_text address) port
  | `Unix path -> "unix:" ^ path

let admission_cases =
  [
    ("ordinary IPv4", "8.8.8.8", true);
    ("shared-space predecessor", "100.63.255.255", true);
    ("shared-space first", "100.64.0.0", false);
    ("shared-space last", "100.127.255.255", false);
    ("shared-space successor", "100.128.0.0", true);
    ("IETF assignment before exceptions", "192.0.0.8", false);
    ("PCP anycast exception", "192.0.0.9", true);
    ("TURN anycast exception", "192.0.0.10", true);
    ("IETF assignment after exceptions", "192.0.0.11", false);
    ("deprecated relay anycast", "192.88.99.1", false);
    ("multicast", "224.0.0.1", false);
    ("mapped private IPv4", "::ffff:127.0.0.1", false);
    ("mapped public IPv4", "::ffff:8.8.8.8", false);
    ("well-known NAT64 public", "64:ff9b::808:808", true);
    ("well-known NAT64 private", "64:ff9b::7f00:1", false);
    ("local-use NAT64", "64:ff9b:1::808:808", false);
    ("Teredo", "2001::", false);
    ("PCP IPv6 anycast exception", "2001:1::1", true);
    ("TURN IPv6 anycast exception", "2001:1::2", true);
    ("DNS-SD anycast exception", "2001:1::3", true);
    ("unassigned sibling", "2001:1::4", false);
    ("benchmarking", "2001:2::", false);
    ("AMT exception", "2001:3::", true);
    ("AS112 exception", "2001:4:112::", true);
    ("ORCHIDv2 exception first", "2001:20::", true);
    ("ORCHIDv2 exception last", "2001:2f:ffff::", true);
    ("DET exception first", "2001:30::", true);
    ("DET exception last", "2001:3f:ffff::", true);
    ("IETF /23 interior", "2001:100::", false);
    ("IETF /23 last", "2001:1ff:ffff::", false);
    ("IETF /23 successor", "2001:200::", true);
    ("documentation 2001", "2001:db8::1", false);
    ("6to4", "2002::1", false);
    ("ordinary IPv6", "2606:4700:4700::1111", true);
    ("documentation 3fff last", "3fff:fff:ffff::", false);
    ("documentation 3fff successor", "3fff:1000::", true);
  ]

let boundaries =
  group "IANA special-purpose boundaries"
    [
      test "admit only global unicast addresses" (fun () ->
          List.iter
            (fun (label, text, expected) ->
              let admitted =
                Tools.Web.For_testing.globally_routable (address text)
              in
              is_true
                ~msg:
                  (Printf.sprintf "%s (%s): expected admitted=%b, got %b" label
                     text expected admitted)
                (admitted = expected))
            admission_cases);
    ]

let vet ~allow_private_network addresses =
  let url = Uri.of_string "https://example.com/docs" in
  match
    Tools.Web.For_testing.vet_addresses ~allow_private_network ~url addresses
  with
  | Ok selected -> "selected " ^ sockaddr_text selected
  | Error error -> "rejected " ^ Tools.Web.Transport.error_message error

let vetting =
  group "DNS answer vetting"
    [
      test "the whole DNS answer is vetted before one address is pinned"
        (fun () ->
          let first_public = tcp "93.184.216.34" 443 in
          let second_public = tcp "8.8.8.8" 8443 in
          let private_address = tcp "127.0.0.1" 9443 in
          equal string ~msg:"empty"
            "rejected web transport failure: DNS resolution returned no \
             addresses for example.com"
            (vet ~allow_private_network:false []);
          equal string ~msg:"non-TCP"
            "rejected web protocol failure: DNS resolution returned a non-TCP \
             address"
            (vet ~allow_private_network:false [ `Unix "/tmp/socket" ]);
          equal string ~msg:"public then private"
            "rejected web fetch resolved to a disallowed address: 127.0.0.1"
            (vet ~allow_private_network:false [ first_public; private_address ]);
          equal string ~msg:"private then public"
            "rejected web fetch resolved to a disallowed address: 127.0.0.1"
            (vet ~allow_private_network:false [ private_address; first_public ]);
          equal string ~msg:"all public" "selected tcp:93.184.216.34:443"
            (vet ~allow_private_network:false [ first_public; second_public ]);
          equal string ~msg:"private policy enabled"
            "selected tcp:127.0.0.1:9443"
            (vet ~allow_private_network:true [ private_address; first_public ]));
    ]

let () = run "mentat.tools.web-transport" [ boundaries; vetting ]
