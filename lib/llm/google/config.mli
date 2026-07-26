(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Google Gemini client configuration.

    Configuration values are inert. They do not read process environment
    variables, open network resources, validate credentials, or choose model
    defaults. Pass them to {!Mentat_llm_google.client} to affect subsequent HTTP
    requests. *)

type t
(** The type for Google Gemini HTTP configuration. *)

val default : t
(** [default] is [make ()].

    Provider defaults are applied by the adapter: the public Google endpoint, a
    600-second whole-request deadline, and two retry attempts after the initial
    request. *)

val make :
  ?base_url:string ->
  ?timeout_s:float ->
  ?max_retries:int ->
  ?max_stream_retries:int ->
  unit ->
  t
(** [make ()] is Google Gemini HTTP configuration.

    - [base_url] is the API root, without the endpoint path. Trailing slashes,
      when present, are removed.
    - [timeout_s] is the whole logical-request deadline, covering retries,
      backoff, and streamed response consumption. It defaults to 600 seconds.
    - [max_retries] is the number of pre-stream HTTP retry attempts after the
      initial request when present. [Some 0] disables retries entirely — both
      the pre-stream HTTP retry and the stream-phase re-run below.
    - [max_stream_retries] is the number of times a stream-phase failure (a
      transient server fault, a mid-stream rate limit or overload, or a dropped
      stream that surfaced no output yet) re-runs the whole request when
      present. It is independent of [max_retries] except that
      [max_retries = Some 0] forces it to zero.

    Raises [Invalid_argument] if [base_url] is empty, contains a newline, or
    contains only slashes, [timeout_s] is not positive and finite, or
    [max_retries] or [max_stream_retries] is negative. *)

val base_url : t -> string option
(** [base_url t] is the normalized API root, if any.

    [None] means the Google Gemini default endpoint. *)

val timeout_s : t -> float
(** [timeout_s t] is the whole logical-request deadline in seconds. *)

val max_retries : t -> int option
(** [max_retries t] is the pre-stream HTTP retry count after the initial
    attempt, if any.

    [None] means two retry attempts after the initial request. [Some 0] disables
    retries. *)

val max_stream_retries : t -> int option
(** [max_stream_retries t] is the configured stream-phase re-run count, if any.
*)
