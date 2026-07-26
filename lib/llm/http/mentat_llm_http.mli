(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Private HTTP integrations for LLM providers.

    This library owns the shared Eio/Cohttp/TLS mechanics and HTTP protocol
    implementations used by concrete provider adapters. Authentication, endpoint
    discovery, provider-specific protocols, retry policy, and model lifecycle
    remain in those adapters. *)

include module type of Transport

module Chat_completions = Chat_completions
(** The OpenAI-compatible Chat Completions protocol. *)

module Retry = Retry
(** Shared retry loops for HTTP-backed provider transports. *)
