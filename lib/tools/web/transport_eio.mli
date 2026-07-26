(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Eio HTTP transport for [web_fetch].

    The transport resolves and vets every hop immediately before opening its
    socket, connects to one address from that exact result, and retains the URL
    host for HTTP authority and TLS peer verification. Same-authority redirects
    are followed within one deadline; cross-authority redirects are returned
    before resolving or contacting the target.

    Connections are registered beneath the switch supplied to the resulting
    callback and are also closed before the callback returns. System trust roots
    authenticate TLS peers. No ambient proxy, cookie jar, or credential source
    is consulted. *)

val make : net:_ Eio.Net.t -> mono_clock:_ Eio.Time.Mono.t -> Transport.t
(** [make ~net ~mono_clock] is a fetch transport using [net] for DNS and
    connections and [mono_clock] for the whole-call deadline and observations.

    Construction installs the process RNG and loads the system trust roots once
    for this transport. A TLS-initialization failure is retained and returned as
    a structured transport error by HTTPS requests; HTTP requests remain
    available. Construction opens no socket. *)

module For_testing : sig
  (** Pure address-policy decisions shared with focused transport tests. *)

  val globally_routable : Eio.Net.Ipaddr.v4v6 -> bool
  (** [globally_routable address] is the address-admission decision used when
      private-network access is disabled. *)

  val vet_addresses :
    allow_private_network:bool ->
    url:Uri.t ->
    Eio.Net.Sockaddr.stream list ->
    (Eio.Net.Sockaddr.stream, Transport.error) result
  (** [vet_addresses ~allow_private_network ~url addresses] applies the exact
      post-resolution policy used by {!make}. Every candidate is checked before
      the first socket address is selected, so a mixed result is rejected in
      full rather than partially admitted.

      This private-module seam exists only for focused transport tests;
      production composition uses {!make}. *)
end
