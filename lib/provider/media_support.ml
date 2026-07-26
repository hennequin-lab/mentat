(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type channel = User_content | Tool_result

(* The table is keyed by the builtin adapters' API identifiers (mentat.provider
   cannot link the adapter libraries, so the ids are strings). The drift-guard
   test in test_provider asserts these ids and their encodability against the
   real adapters, so a drift between this table and an adapter fails loudly. *)
let encodable api channel =
  match (Mentat_llm.Model.Api.id api, channel) with
  | "messages", (User_content | Tool_result) -> true
  | "responses", (User_content | Tool_result) -> true
  | "gemini", User_content -> true
  | "gemini", Tool_result -> false
  | "chat-completions", User_content -> true
  | "chat-completions", Tool_result -> false
  | _, (User_content | Tool_result) -> false
