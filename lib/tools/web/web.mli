(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Policy-gated public web tools.

    {!tools} binds the credential-free Eio HTTP transport to explicit network
    and monotonic-clock capabilities. {!Web_fetch.make} remains the lower-level
    transport-injection seam for tests and alternate hosts. *)

module Policy = Policy
(** Validated host policy for web fetches. *)

module Transport = Transport
(** Structured effectful fetch callback boundary. *)

module Search_service = Search_service
(** The remote MCP search backends and their request/response wire form. *)

module Web_fetch = Web_fetch
(** The model-facing bounded public-page fetch tool. *)

module Web_search = Web_search
(** The model-facing public web search tool. *)

val tools :
  policy:Policy.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  ?search:Search_service.t * string option ->
  unit ->
  Mentat_tool.t list
(** [tools ~policy ~net ~mono_clock ()] is the web family. It always contains
    {!Web_fetch}; when [search] is present it also contains {!Web_search}, bound
    to the [(backend, optional_api_key)] pair. Both tools share one transport,
    so construction initializes the process cryptographic RNG and reads the
    system trust roots but opens no socket. TLS initialization failures are
    retained and reported by HTTPS calls; HTTP remains available.

    The composition root omits the entire family, and therefore these setup
    effects, when web access is disabled, and omits {!Web_search} specifically
    when the configured search provider is [off]. *)

(**/**)

module For_testing : sig
  (** Internal access to the pure address-policy decisions made by the concrete
      transport. Production composition uses {!tools}. *)

  val globally_routable : Eio.Net.Ipaddr.v4v6 -> bool
  (** [globally_routable address] is whether [address] is admitted when private
      network access is disabled. *)

  val vet_addresses :
    allow_private_network:bool ->
    url:Uri.t ->
    Eio.Net.Sockaddr.stream list ->
    (Eio.Net.Sockaddr.stream, Transport.error) result
  (** [vet_addresses ~allow_private_network ~url addresses] vets the entire DNS
      answer before selecting its first socket address. *)
end
