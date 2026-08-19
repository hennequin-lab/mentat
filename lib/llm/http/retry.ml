(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let log_src = Logs.Src.create "mentat.llm.http" ~doc:"Shared HTTP retry loops"

module Log = (val Logs.src_log log_src : Logs.LOG)

let max_honored_retry_delay = 60.

let retryable_status status =
  status = 408 || status = 409 || status = 429 || status >= 500

let capacity_status status = status = 429 || status = 503 || status = 529
let server_fault_status status = status >= 500 && not (capacity_status status)

(* Capacity exhaustion is worth outlasting: a deeper budget rides through an
   overload window that a couple of attempts would surface as a dead turn. An
   unexplained server fault deserves nearly as much, one notch shallower because
   it carries no server guidance to honor and so rides on backoff alone. Both
   budgets are the pre-first-token authority: the stream loop below surfaces an
   open failure rather than re-running it, so the depth has to live here. *)
let capacity_budget = 10
let server_fault_budget = 8

let retry_budget ~max_retries status =
  if max_retries <= 0 then max_retries
  else if capacity_status status then max capacity_budget max_retries
  else if server_fault_status status then max server_fault_budget max_retries
  else max_retries

(* Callers that fail together — a fleet of subagents against one saturated
   endpoint — must not re-issue in lockstep, so a computed backoff is spread by
   a tenth either way. A server-provided delay stays exact and acts as a floor:
   jitter may lengthen a wait, never shorten it below what the server asked. *)
let jitter delay = delay *. (0.9 +. Random.float 0.2)

let retry_reason status =
  if status = 429 then "rate limited"
  else if status = 503 || status = 529 then "provider overloaded"
  else if status = 408 then "provider timed out"
  else "provider error"

(* An explicit [x-should-retry] header is the server ruling on whether an
   identical request can succeed: [false] fails fast even on a status the table
   would retry, [true] retries a status it would not. Absent or unrecognized,
   the status table and the caller's [terminal] classifier decide. *)
let response_retryable ~terminal response =
  match
    Option.map String.lowercase_ascii
      (Transport.header_value response.Transport.headers "x-should-retry")
  with
  | Some "true" -> true
  | Some "false" -> false
  | Some _ | None ->
      retryable_status response.Transport.status && not (terminal response)

let pre_stream ~clock ~max_retries ?(terminal = fun _ -> false)
    ?(body_delay = fun _ -> None) ~on_retry attempt_post =
  let rec loop attempt delay =
    match attempt_post ~attempt with
    | Error (Transport.Response response)
      when response_retryable ~terminal response
           && attempt < retry_budget ~max_retries response.Transport.status -> (
        let server_delay =
          match
            Transport.Retry_after.delay ~now:(Eio.Time.now clock)
              response.Transport.headers
          with
          | Some _ as delay -> delay
          | None -> body_delay response
        in
        (* A server-provided delay is honored exactly, but only up to the bound:
           a server pushing the retry beyond it is saying the condition will
           outlast this loop, and re-issuing earlier than asked would only burn
           the budget against a promised refusal. Such a failure surfaces at
           once. *)
        match server_delay with
        | Some requested when requested > max_honored_retry_delay ->
            Log.warn (fun m ->
                m "not retrying status=%d: server requested %.0fs delay"
                  response.Transport.status requested);
            Error (Transport.Response response)
        | _ ->
            let sleep =
              Float.min max_honored_retry_delay
                (Float.max (jitter delay)
                   (Option.value server_delay ~default:0.))
            in
            Log.warn (fun m ->
                m "retrying after status=%d attempt=%d delay=%.1fs"
                  response.Transport.status attempt sleep);
            on_retry ~attempt:(attempt + 1)
              ~limit:(retry_budget ~max_retries response.Transport.status)
              ~delay:sleep
              ~reason:(retry_reason response.Transport.status);
            Eio.Time.sleep clock sleep;
            loop (attempt + 1) (delay *. 1.5))
    | Error (Transport.Transport _) when attempt < max_retries ->
        let sleep = jitter delay in
        Log.warn (fun m ->
            m "retrying after transport error attempt=%d delay=%.1fs" attempt
              sleep);
        on_retry ~attempt:(attempt + 1) ~limit:max_retries ~delay:sleep
          ~reason:"connection failed";
        Eio.Time.sleep clock sleep;
        loop (attempt + 1) (delay *. 1.5)
    | Error _ as error -> error
    | Ok _ as ok -> ok
  in
  loop 0 0.5

(* Stream-phase failures worth re-running: a transient server fault, a mid-stream
   overload (which classifies as rate-limited), a dropped stream, or a mid-stream
   transport drop. The terminal, request-shaped kinds — auth, quota, context
   overflow, invalid request, content policy, an unsupported feature, a decode
   fault, or a cancellation — would fail identically on a fresh request, so they
   surface at once. [Timeout] is terminal even though a stall is transient,
   because the kind only ever names failures already past their retries: a [408]
   spends the pre-stream budget before it classifies as [Timeout], and the
   client-deadline timeout cancels this loop from outside, leaving nothing to
   re-run. *)
let stream_retryable_kind = function
  | Mentat_llm.Error.Provider | Mentat_llm.Error.Rate_limited
  | Mentat_llm.Error.Malformed_stream | Mentat_llm.Error.Transport ->
      true
  | Mentat_llm.Error.Cancelled | Mentat_llm.Error.Auth | Mentat_llm.Error.Quota
  | Mentat_llm.Error.Context_overflow | Mentat_llm.Error.Invalid_request
  | Mentat_llm.Error.Unsupported | Mentat_llm.Error.Content_policy
  | Mentat_llm.Error.Timeout | Mentat_llm.Error.Decode
  | Mentat_llm.Error.Other _ ->
      false

let stream_retry_reason = function
  | Mentat_llm.Error.Rate_limited -> "rate limited"
  | Mentat_llm.Error.Malformed_stream -> "stream interrupted"
  | Mentat_llm.Error.Transport -> "connection failed"
  | Mentat_llm.Error.Provider | Mentat_llm.Error.Cancelled
  | Mentat_llm.Error.Auth | Mentat_llm.Error.Quota
  | Mentat_llm.Error.Context_overflow | Mentat_llm.Error.Invalid_request
  | Mentat_llm.Error.Unsupported | Mentat_llm.Error.Content_policy
  | Mentat_llm.Error.Timeout | Mentat_llm.Error.Decode
  | Mentat_llm.Error.Other _ ->
      "provider error"

let stream ~clock ~cancelled ~max_retries ~on_event ~open_stream ~consume =
  let on_retry ~attempt ~limit ~delay ~reason =
    on_event
      (Mentat_llm.Event.retry
         (Mentat_llm.Event.Retry.make ~attempt ~limit ~delay ~reason ()))
  in
  (* A stream-phase re-run is only safe while no assistant output has reached the
     caller: the deltas stream straight to the transcript, so replaying a fresh
     attempt over already-shown text would double it. A transient fault fails
     before the first token, so the guard still covers it while a
     partially-rendered turn surfaces. *)
  let emitted_output = ref false in
  let on_event event =
    (match event with
    | Mentat_llm.Event.Text_delta _ | Mentat_llm.Event.Reasoning_summary_delta _
    | Mentat_llm.Event.Tool_call _ | Mentat_llm.Event.Tool_input_delta _ ->
        emitted_output := true
    | Mentat_llm.Event.Usage _ | Mentat_llm.Event.Retry _ -> ());
    on_event event
  in
  let rec attempt_stream stream_attempt delay =
    match open_stream ~on_retry with
    | Error _ as error -> error
    | Ok stream -> (
        match consume ~on_event stream with
        | Ok _ as ok -> ok
        | Error error
          when stream_attempt < max_retries
               && stream_retryable_kind (Mentat_llm.Error.kind error)
               && (not !emitted_output)
               && not (cancelled ()) ->
            let sleep = jitter delay in
            Log.warn (fun m ->
                m "retrying after stream error kind=%s attempt=%d delay=%.1fs"
                  (Mentat_llm.Error.label (Mentat_llm.Error.kind error))
                  stream_attempt sleep);
            on_retry ~attempt:(stream_attempt + 1) ~limit:max_retries
              ~delay:sleep
              ~reason:(stream_retry_reason (Mentat_llm.Error.kind error));
            Eio.Time.sleep clock sleep;
            attempt_stream (stream_attempt + 1) (delay *. 1.5)
        | Error _ as error -> error)
  in
  attempt_stream 0 0.5
