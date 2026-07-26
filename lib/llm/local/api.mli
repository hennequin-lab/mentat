(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Managed llama-server readiness checks.

    This private module contains only the non-standard [/health] endpoint. Chat
    Completions transport belongs to {!Mentat_llm_http.Chat_completions}. *)

val health :
  ?timeout_s:float ->
  env:Eio_unix.Stdenv.base ->
  base_url:string ->
  unit ->
  (unit, string) result
(** [health ~env ~base_url ()] is [Ok ()] when [GET /health] answers with a 2xx
    status and its body completes. [timeout_s] defaults to two seconds. *)
