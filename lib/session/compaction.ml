(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_session.Compaction" fn message

module Reason = struct
  type t =
    | User_requested
    | Context_pressure
    | Context_overflow
    | Model_downshift

  let equal a b = a = b

  let to_string = function
    | User_requested -> "user_requested"
    | Context_pressure -> "context_pressure"
    | Context_overflow -> "context_overflow"
    | Model_downshift -> "model_downshift"

  let pp ppf reason = Format.pp_print_string ppf (to_string reason)

  let jsont =
    Jsont.enum ~kind:"compaction reason"
      [
        ("user_requested", User_requested);
        ("context_pressure", Context_pressure);
        ("context_overflow", Context_overflow);
        ("model_downshift", Model_downshift);
      ]
end

let check_non_negative fn field = function
  | Some value when value < 0 -> invalid fn (field ^ " must be non-negative")
  | Some _ | None -> ()

module Context_tokens = struct
  type t = { before : int option; after : int option }

  let make ?before ?after () =
    if Option.is_none before && Option.is_none after then
      invalid "Context_tokens.make" "at least one token count is required";
    check_non_negative "Context_tokens.make" "before" before;
    check_non_negative "Context_tokens.make" "after" after;
    { before; after }

  let before t = t.before
  let after t = t.after
  let equal a b = a = b

  let pp ppf t =
    Format.fprintf ppf "{ before = %a; after = %a }"
      (Format.pp_print_option Format.pp_print_int)
      t.before
      (Format.pp_print_option Format.pp_print_int)
      t.after

  let jsont =
    let make before after =
      decode_invalid_arg (fun () -> make ?before ?after ())
    in
    Jsont.Object.map ~kind:"compaction context tokens" make
    |> Jsont.Object.opt_mem "before" Jsont.int ~enc:before
    |> Jsont.Object.opt_mem "after" Jsont.int ~enc:after
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

type t = {
  reason : Reason.t;
  provider_claim : Provider_request.Id.t;
  request_digest : Mentat_digest.t;
  summary_messages : Mentat_llm.Message.t list;
  summarized_upto : int;
  usage : Mentat_llm.Usage.t option;
  context : Context_tokens.t option;
}

let check_summary summary_messages =
  match Mentat_llm.Transcript.of_list summary_messages with
  | Error error ->
      invalid "make"
        (Format.asprintf "summary_messages must form a valid transcript: %a"
           Mentat_llm.Transcript.Error.pp error)
  | Ok transcript -> (
      match Mentat_llm.Transcript.require_ready transcript with
      | Ok () -> ()
      | Error error ->
          invalid "make"
            (Format.asprintf "summary_messages must be request-ready: %a"
               Mentat_llm.Transcript.Error.pp error))

let make ~reason ~provider_claim ~request_digest ~summary_messages
    ~summarized_upto ?usage ?context () =
  if summarized_upto < 0 then
    invalid "make" "summarized_upto must not be negative";
  check_summary summary_messages;
  {
    reason;
    provider_claim;
    request_digest;
    summary_messages;
    summarized_upto;
    usage;
    context;
  }

let reason t = t.reason
let provider_claim t = t.provider_claim
let request_digest t = t.request_digest
let summary_messages t = t.summary_messages
let summarized_upto t = t.summarized_upto
let usage t = t.usage
let context t = t.context

let equal a b =
  Reason.equal a.reason b.reason
  && Provider_request.Id.equal a.provider_claim b.provider_claim
  && Mentat_digest.equal a.request_digest b.request_digest
  && List.equal Mentat_llm.Message.equal a.summary_messages b.summary_messages
  && Int.equal a.summarized_upto b.summarized_upto
  && Option.equal Mentat_llm.Usage.equal a.usage b.usage
  && Option.equal Context_tokens.equal a.context b.context

let pp ppf t =
  Format.fprintf ppf "compaction(reason=%a, summarized_upto=%d)" Reason.pp
    t.reason t.summarized_upto

let jsont =
  Jsont.Object.map ~kind:"compaction"
    (fun
      reason
      provider_claim
      request_digest
      summary_messages
      summarized_upto
      usage
      context
    ->
      decode_invalid_arg (fun () ->
          make ~reason ~provider_claim ~request_digest ~summary_messages
            ~summarized_upto ?usage ?context ()))
  |> Jsont.Object.mem "reason" Reason.jsont ~enc:reason
  |> Jsont.Object.mem "provider_claim" Provider_request.Id.jsont
       ~enc:provider_claim
  |> Jsont.Object.mem "request_digest" Mentat_digest.jsont ~enc:request_digest
  |> Jsont.Object.mem "summary_messages"
       (Jsont.list Mentat_llm.Message.jsont)
       ~enc:summary_messages
  |> Jsont.Object.mem "summarized_upto" Jsont.int ~enc:summarized_upto
  |> Jsont.Object.opt_mem "usage" Mentat_llm.Usage.jsont ~enc:usage
  |> Jsont.Object.opt_mem "context" Context_tokens.jsont ~enc:context
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
