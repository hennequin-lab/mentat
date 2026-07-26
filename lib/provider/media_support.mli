(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Whether a builtin provider API can encode image media in a given channel.

    A declared table the engine consults before assembling a request, so it can
    strip a media block gracefully to a text placeholder rather than
    hard-failing at the encoder mid-turn. The vision gate is a function of
    (channel, API, model): this is the channel/API half; the model half is
    {!Mentat_llm.Model.has_input_modality} on [Modality.image]. A drift-guard
    test asserts this table matches the builtin adapters' actual encoding. *)

type channel =
  | User_content  (** A user message's content. *)
  | Tool_result  (** A tool result's content — the read-path channel. *)

val encodable : Mentat_llm.Model.Api.t -> channel -> bool
(** [encodable api channel] is [true] iff the builtin provider [api] encodes
    image media in [channel]. Only Anthropic messages and OpenAI Responses carry
    tool-result media; Google Gemini and Chat Completions carry image media in
    user content only; an unknown API carries none. *)
