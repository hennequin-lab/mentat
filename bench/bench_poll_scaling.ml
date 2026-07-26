(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Poll scaling: one feed advance must cost the same in a long session as a
    short one.

    The felt moment is a live turn in a long conversation staying as smooth as
    in a fresh one — every delta the UI polls must land in constant time however
    much has already been said. A feed hub serves that poll by growing its
    materialized projection one commit at a time through the one projector
    ([Mentat_protocol.Projection.advance]) and indexing the shared array in O(1);
    {!Mentat_agent}'s feed is a thin O(1) reader over exactly this. So the
    load-bearing property behind a flat [Feed.next] is that [advance] over a
    single-event delta is O(delta), never O(events already folded).

    This is a boolean SCALING gate, not a wall number: it folds a fixed number of
    single-event advances onto a projection that has already consumed 100 events
    and, separately, 10 000, and asserts the per-advance allocation is flat
    across the two (ratio < 2x). Allocation is exact and machine-independent, so
    the assertion never flakes; a regression that made [advance] re-fold the
    whole journal per commit — the quadratic the incremental projector exists to
    prevent — would blow the ratio to the size ratio (100x) and fail loudly. It
    runs on [runtest] because a flat-vs-not verdict is honest to gate, unlike the
    wall latencies its sibling trend records. It drives no engine: the projector
    is the mechanism [Feed.next] indexes, and exercising it directly keeps the
    gate deterministic. *)

module Projection = Mentat_protocol.Projection
module Session = Mentat_session
module Llm = Mentat_llm
module Fixture = Bench_support.Session_fixture

let empty_mutation =
  match Mentat_mutation.State.of_events [] with
  | Ok state -> state
  | Error _ -> assert false

(* A projector state that has already folded [events] journal events — the
   accumulator a hub carries after a conversation of that length. *)
let resume_after ~events =
  let session = Fixture.build ~events () in
  let resume, _facts =
    Projection.advance Projection.start ~session ~mutation:empty_mutation
      ~delta:(Session.events session)
  in
  (session, resume)

(* One fresh journal event to fold — a hub's per-commit delta. The projector
   reads only the delta and the session's owning id, never the session's own
   event list, so one representative event advances any resume. *)
let one_event = Session.Event.message_appended (Llm.Message.user_text "poll")

(* Mean bytes allocated by advancing a single-event delta [iterations] times onto
   a projection that has already consumed [events]. The first advance is folded
   outside the measured region so no lazy first-call cost is attributed. *)
let bytes_per_advance ~events ~iterations =
  let session, resume0 = resume_after ~events in
  let resume = ref resume0 in
  let advance () =
    let next, facts =
      Projection.advance !resume ~session ~mutation:empty_mutation
        ~delta:[ one_event ]
    in
    resume := next;
    ignore (Sys.opaque_identity facts)
  in
  advance ();
  let before = Gc.allocated_bytes () in
  for _ = 1 to iterations do
    advance ()
  done;
  let after = Gc.allocated_bytes () in
  (after -. before) /. float_of_int iterations

let () =
  let iterations = 2_000 in
  let short = bytes_per_advance ~events:100 ~iterations in
  let long = bytes_per_advance ~events:10_000 ~iterations in
  let ratio = if short > 0. then long /. short else 1. in
  Printf.printf
    "poll-scaling: projection advance/commit = %.1f B @100 events, %.1f B @10k \
     events, ratio %.3f\n"
    short long ratio;
  if ratio >= 2.0 then begin
    Printf.eprintf
      "poll-scaling regression: per-commit projection advance is not flat across \
       session sizes (%.1f B @10k / %.1f B @100 = %.3f >= 2.0); the incremental \
       projector may be re-folding the journal per commit.\n"
      long short ratio;
    exit 1
  end
