(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

module Stop = struct
  let invalid fn message = invalid_arg' "Mentat_llm.Response.Stop" fn message

  type t =
    | End_turn
    | Tool_call
    | Length
    | Content_filter
    | Refusal
    | Other of string

  let is_label_char c =
    Char.Ascii.is_lower c || Char.Ascii.is_digit c || Char.equal c '_'

  let valid_label label =
    let len = String.length label in
    len > 0
    && Char.Ascii.is_lower label.[0]
    &&
    let rec loop index =
      index >= len || (is_label_char label.[index] && loop (index + 1))
    in
    loop 1

  let end_turn = End_turn
  let tool_call = Tool_call
  let length = Length
  let content_filter = Content_filter
  let refusal = Refusal

  let is_reserved = function
    | "end_turn" | "tool_call" | "length" | "content_filter" | "refusal" -> true
    | _ -> false

  let other label =
    if not (valid_label label) then
      invalid "other"
        "label must start with a lowercase ASCII letter and contain only \
         lowercase ASCII letters, digits, or '_'";
    if is_reserved label then invalid "other" "label is reserved";
    Other label

  let label = function
    | End_turn -> "end_turn"
    | Tool_call -> "tool_call"
    | Length -> "length"
    | Content_filter -> "content_filter"
    | Refusal -> "refusal"
    | Other label -> label

  let pp ppf t = Format.pp_print_string ppf (label t)

  (* Known labels decode to their constructor, other valid labels to [Other];
     an invalid label is a decode error. *)
  let of_label = function
    | "end_turn" -> Some End_turn
    | "tool_call" -> Some Tool_call
    | "length" -> Some Length
    | "content_filter" -> Some Content_filter
    | "refusal" -> Some Refusal
    | label -> if valid_label label then Some (Other label) else None

  let jsont =
    Jsont.map ~kind:"LLM stop reason"
      ~dec:(fun label ->
        match of_label label with
        | Some stop -> stop
        | None -> decode_error "invalid stop reason label")
      ~enc:label Jsont.string
end

let invalid fn message = invalid_arg' "Mentat_llm.Response" fn message

let reject_empty fn field value =
  if String.is_empty value then invalid fn (field ^ " must not be empty")

let reject_empty_option fn field = function
  | None -> ()
  | Some value -> reject_empty fn field value

type t = {
  model : Model.t;
  response_model : string option;
  response_id : string option;
  provider_stop : string option;
  stop : Stop.t option;
  usage : Usage.t option;
  reasoning_summary : string list;
  assistant : Message.Assistant.t;
}

let make ~model ?response_model ?response_id ?provider_stop ?stop ?usage
    ?(reasoning_summary = []) assistant =
  reject_empty_option "make" "response_model" response_model;
  reject_empty_option "make" "response_id" response_id;
  reject_empty_option "make" "provider_stop" provider_stop;
  List.iter (reject_empty "make" "reasoning_summary") reasoning_summary;
  {
    model;
    response_model;
    response_id;
    provider_stop;
    stop;
    usage;
    reasoning_summary;
    assistant;
  }

let assistant t = t.assistant
let message t = Message.assistant t.assistant
let texts t = Message.Assistant.texts t.assistant
let text ?(sep = "") t = String.concat sep (texts t)
let tool_calls t = Message.Assistant.tool_calls t.assistant
let has_tool_calls t = match tool_calls t with [] -> false | _ -> true
let model t = t.model
let response_model t = t.response_model
let response_id t = t.response_id
let provider_stop t = t.provider_stop
let stop t = t.stop
let usage t = t.usage
let reasoning_summary t = t.reasoning_summary

(* Structural over payload, semantic over the embedded assistant message (which
   may carry tool-call JSON): {!Message.equal} handles that, the metadata fields
   hold only strings, usage counts, and the stop variant. *)
let equal a b =
  Model.equal a.model b.model
  && Option.equal String.equal a.response_model b.response_model
  && Option.equal String.equal a.response_id b.response_id
  && Option.equal String.equal a.provider_stop b.provider_stop
  && Option.equal ( = ) a.stop b.stop
  && Option.equal Usage.equal a.usage b.usage
  && List.equal String.equal a.reasoning_summary b.reasoning_summary
  && Message.equal (message a) (message b)

let jsont =
  let make model response_model response_id provider_stop stop usage
      reasoning_summary assistant =
    decode_invalid_arg (fun () ->
        make ~model ?response_model ?response_id ?provider_stop ?stop ?usage
          ~reasoning_summary assistant)
  in
  Jsont.Object.map ~kind:"LLM response" make
  |> Jsont.Object.mem "model" Model.jsont ~enc:model
  |> Jsont.Object.opt_mem "response_model" Jsont.string ~enc:response_model
  |> Jsont.Object.opt_mem "response_id" Jsont.string ~enc:response_id
  |> Jsont.Object.opt_mem "provider_stop" Jsont.string ~enc:provider_stop
  |> Jsont.Object.opt_mem "stop" Stop.jsont ~enc:stop
  |> Jsont.Object.opt_mem "usage" Usage.jsont ~enc:usage
  |> Jsont.Object.mem "reasoning_summary" (Jsont.list Jsont.string)
       ~enc:reasoning_summary
  |> Jsont.Object.mem "assistant" Message.Assistant.jsont ~enc:assistant
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
