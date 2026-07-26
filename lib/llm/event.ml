(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_llm.Event" fn message

let reject_empty fn field value =
  if String.is_empty value then invalid fn (field ^ " must not be empty")

let reject_empty_option fn field = function
  | None -> ()
  | Some value -> reject_empty fn field value

module Tool_input = struct
  type t = {
    key : string;
    call_id : string option;
    name : string option;
    input_delta : string;
  }

  let make ~key ?call_id ?name ~input_delta () =
    reject_empty "Tool_input.make" "key" key;
    reject_empty_option "Tool_input.make" "call_id" call_id;
    reject_empty_option "Tool_input.make" "name" name;
    reject_empty "Tool_input.make" "input_delta" input_delta;
    { key; call_id; name; input_delta }

  let key t = t.key
  let call_id t = t.call_id
  let name t = t.name
  let input_delta t = t.input_delta
  let equal a b = a = b
end

module Retry = struct
  type t = { attempt : int; limit : int; delay : float; reason : string }

  let make ~attempt ~limit ~delay ~reason () =
    if attempt < 1 then invalid "Retry.make" "attempt must be at least 1";
    if limit < attempt then
      invalid "Retry.make" "limit must be at least attempt";
    if (not (Float.is_finite delay)) || delay < 0. then
      invalid "Retry.make" "delay must be finite and non-negative";
    reject_empty "Retry.make" "reason" reason;
    { attempt; limit; delay; reason }

  let attempt t = t.attempt
  let limit t = t.limit
  let delay t = t.delay
  let reason t = t.reason
  let equal a b = a = b
end

type t =
  | Text_delta of string
  | Reasoning_summary_delta of string
  | Tool_input_delta of Tool_input.t
  | Tool_call of Tool.Call.t
  | Usage of Usage.t
  | Retry of Retry.t

let text_delta value =
  reject_empty "text_delta" "text" value;
  Text_delta value

let reasoning_summary_delta value =
  reject_empty "reasoning_summary_delta" "summary" value;
  Reasoning_summary_delta value

let tool_input_delta delta = Tool_input_delta delta
let tool_call call = Tool_call call
let usage usage = Usage usage
let retry retry = Retry retry
