(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The five built-in provider registrations.

    Each provider's pure declaration is re-authored onto [Mentat_provider.make]
    and paired, exactly once, with the effectful driver that serves it. This
    single list is the source [create] assembles and checks. *)

val registrations : Driver.registration list
(** [registrations] are the built-in providers in catalog order: openai,
    anthropic, google, local, ollama. Assembling this list performs no I/O. *)
