(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The bounded backend availability probe.

    Selects the platform's enforcing backend and probes it once per resolution:
    Bubblewrap on Linux through a bounded (1s) namespace probe under a
    short-lived switch, Seatbelt on macOS through an executable check.
    Cancellation propagates as cancellation and is never recorded as a failed
    probe; the outcome is never cached — the resolver injects it into the seal
    it is building.

    Two ambient test seams force deterministic availability for black-box tests:
    [_MENTAT_TEST_SANDBOX_UNAVAILABLE] forces a refusal and
    [_MENTAT_TEST_BUBBLEWRAP_PROBE] substitutes the probed executable.
    Production backend selection and the enforcing prefixes are fixed. *)

val backend :
  stdenv:Eio_unix.Stdenv.base ->
  lookup:(string -> string option) ->
  (Mentat_sandbox.Backend.t, Mentat_sandbox.Error.t) result
(** [backend ~stdenv ~lookup] is the probe outcome {!Mentat_sandbox.confined}
    seals: the available backend, or the [Unavailable] refusal naming why none
    is usable on this host. *)
