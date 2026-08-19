(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared retry loops for HTTP-backed LLM provider transports.

    The retry policy is fixed here so that every transport shares one loop
    rather than a copied one: retryable statuses are [408], [409], [429], and
    [>= 500]; capacity statuses ([429], [503], [529]) deepen the budget to a
    minimum of ten attempts and plain server faults ([500], [502], [504], …) to
    a minimum of eight; backoff starts at half a second, grows by a factor of
    1.5, and is spread by a tenth either way so that callers failing together do
    not re-issue in lockstep. A server-provided delay up to sixty seconds is
    honored exactly, as a floor under the backoff; a longer one fails the
    request at once, since a server pushing the retry that far out is saying the
    condition will outlast this loop, and re-issuing earlier than asked would
    only burn the budget against a promised refusal. An explicit
    [x-should-retry] response header overrides the status table in both
    directions: [false] fails fast on a status the table would retry, [true]
    retries one it would not. Authentication, endpoint discovery, and response
    interpretation remain in the transports.

    A transport must wire both combinators to retry the way the others do.
    {!pre_stream} retries the request up to the first token, where re-issuing a
    failed attempt is always safe; {!stream} re-runs the streaming phase, where
    a re-run is safe only before any output has surfaced. They nest — the
    [open_stream] passed to {!stream} opens its stream through {!pre_stream}, so
    a stream re-run gets a fresh pre-stream cycle. A pre-stream failure is
    terminal for both, though: {!pre_stream} has already spent its budget on
    that request, and re-running an identical attempt would only multiply the
    budget, so the pre-first-token depth lives entirely in {!pre_stream}. Each
    combinator owns only the loop: the caller's thunk re-issues the same request
    on every attempt, so a retry preserves the request unchanged. *)

val pre_stream :
  clock:_ Eio.Time.clock ->
  max_retries:int ->
  ?terminal:(Transport.response -> bool) ->
  ?body_delay:(Transport.response -> float option) ->
  on_retry:(attempt:int -> limit:int -> delay:float -> reason:string -> unit) ->
  (attempt:int -> ('a, Transport.error) result) ->
  ('a, Transport.error) result
(** [pre_stream ~clock ~max_retries ~on_retry attempt_post] runs [attempt_post],
    numbering attempts from zero, and retries a failed attempt under the shared
    policy until [max_retries] (or the deeper capacity budget) is reached.
    [on_retry] is announced before each backoff sleep.

    [terminal] classifies a response that cannot recover within a request and so
    must fail fast rather than exhaust the budget; it defaults to never
    terminal. An explicit [x-should-retry] header outranks it: the server has
    ruled where [terminal] infers. [body_delay] supplies a retry delay parsed from the response body
    when no [Retry-After] header is present; it defaults to none.

    A {!Transport.Unresolved_host} failure surfaces on its first attempt: the
    host name has no address, so every further attempt reaches the same answer
    and would only spend the budget as backoff. *)

val stream :
  clock:_ Eio.Time.clock ->
  cancelled:(unit -> bool) ->
  max_retries:int ->
  on_event:(Mentat_llm.Event.t -> unit) ->
  open_stream:
    (on_retry:(attempt:int -> limit:int -> delay:float -> reason:string -> unit) ->
    ('stream, Mentat_llm.Error.t) result) ->
  consume:
    (on_event:(Mentat_llm.Event.t -> unit) ->
    'stream ->
    ('a, Mentat_llm.Error.t) result) ->
  ('a, Mentat_llm.Error.t) result
(** [stream ~clock ~cancelled ~max_retries ~on_event ~open_stream ~consume]
    opens a stream with [open_stream] and consumes it with [consume], re-running
    from a fresh [open_stream] on a retryable stream-phase failure until
    [max_retries] is reached.

    The combinator owns the re-run policy: which error kinds are retryable, the
    backoff (half a second, factor 1.5), and the announced reason. A re-run is
    suppressed once any assistant output has reached [on_event] — replaying over
    already-streamed text would double it — or once [cancelled] holds.
    [open_stream] receives the [on_retry] announcer to thread into its own
    pre-stream retry; [consume] receives the output-tracking [on_event] to drive
    the stream. *)
