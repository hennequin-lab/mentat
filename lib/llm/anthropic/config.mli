(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Anthropic HTTP configuration.

    Configuration values are inert. They do not read process environment
    variables, open network resources, or validate credentials. The provider
    adapter consumes them when constructing an {!Api.Client.t}; absent fields
    select the adapter defaults for Anthropic's public API endpoint,
    whole-request deadline, and retry policy. *)

(** {1:types Types} *)

type t
(** The type for Anthropic HTTP configuration. *)

(** {1:constructors Constructors} *)

val default : t
(** [default] is [make ()]. *)

val make :
  ?base_url:string ->
  ?timeout_s:float ->
  ?max_retries:int ->
  ?max_stream_retries:int ->
  unit ->
  t
(** [make ()] is Anthropic HTTP configuration.

    - [base_url] is the API root, without the endpoint path. Trailing slashes
      are ignored. The value is otherwise used as supplied.
    - [timeout_s] is the whole logical-request deadline, covering retries,
      backoff, and streamed response consumption. It defaults to 600 seconds.
    - [max_retries] is the number of pre-stream HTTP retry attempts after the
      initial request when present. [Some 0] disables retries entirely — both
      the pre-stream HTTP retry and the stream-phase re-run below.
    - [max_stream_retries] is the number of times a stream-phase failure (a
      transient server fault, a mid-stream overload, or a dropped stream that
      surfaced no output yet) re-runs the whole request when present. It is
      independent of [max_retries] except that [max_retries = Some 0] forces it
      to zero.

    Raises [Invalid_argument] if [base_url] is empty, contains a newline, or
    contains only slashes, [timeout_s] is not positive and finite, or
    [max_retries] or [max_stream_retries] is negative. *)

(** {1:queries Queries} *)

val base_url : t -> string option
(** [base_url t] is the configured API root, if any. *)

val timeout_s : t -> float
(** [timeout_s t] is the whole logical-request deadline in seconds. *)

val max_retries : t -> int option
(** [max_retries t] is the pre-stream HTTP retry count after the initial
    attempt, if any. *)

val max_stream_retries : t -> int option
(** [max_stream_retries t] is the configured stream-phase re-run count, if any.
*)
