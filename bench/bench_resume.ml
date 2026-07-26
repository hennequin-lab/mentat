(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Resume cost: reopening a session, decode versus replay.

    When a user resumes a conversation, the wait before the transcript appears
    is the document decode plus the replay fold that rebuilds state from the
    event log. The felt moment is that pause on `mentat resume`; the event-count
    axis (100/1k/10k) says how it scales with conversation length. The two cases
    separate where the cost lives: decode is the JSON parse of the whole
    document (which also runs the replay validation), replay is the pure fold in
    isolation — so their comparison attributes a resume regression to the codec
    or to the fold rather than blaming the whole path. Both are in-process over
    pre-serialized bytes; disk is deliberately excluded (that latency is a
    separate, IO-bound concern). *)

module Session = Mentat_session
module Fixture = Bench_support.Session_fixture

let sessions =
  List.map
    (fun (label, n) -> (label, Fixture.build ~events:n ()))
    Fixture.event_sizes

let encoded =
  List.map (fun (label, session) -> (label, Fixture.encoded session)) sessions

let event_lists =
  List.map (fun (label, session) -> (label, Session.events session)) sessions

let () =
  Thumper.run "resume"
    ~budgets:
      [
        Thumper.Budget.no_more_alloc_than 0.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.cpu_time 1000.0;
      ]
    Thumper.
      [
        bench_param "decode" ~params:encoded ~f:(fun bytes ->
            Jsont_bytesrw.decode_string Session.jsont bytes);
        bench_param "replay" ~params:event_lists ~f:(fun events ->
            Session.State.of_events events);
      ]
