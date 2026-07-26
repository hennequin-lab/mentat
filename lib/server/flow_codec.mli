(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The two client-owned flow codecs minted here: the outcomes deferred until a
    transport carried them. Re-exposed on the public surface as
    {!Mentat_server.Flow_codec} so the wire golden corpus can pin them. *)

val compaction_result : Mentat_client.compaction_result Jsont.t
(** [compaction_result] maps the [compact] flow's outcome
    ([Installed]/[Skipped]) to a stable enum. *)

val login_step : Mentat_client.Login.step Jsont.t
(** [login_step] maps one step of the login stream ([Progress]/[Saved]/
    [Cancelled]), tagged by [step] — each progress value tagged by [kind]. *)
