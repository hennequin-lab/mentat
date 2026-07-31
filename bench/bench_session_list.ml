(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Session-list cost: populating the picker.

    When a user opens the session picker, every stored session is scanned and
    decoded — there is no index, so the cost is the sum of the documents' bytes.
    The felt moment is the pause before the picker's list appears; the K axis
    (10/100/1000 sessions) says how that pause grows with a user's history. The
    scan is measured; building the stores is setup. Allocation is the gate
    (decoding a fixed set of documents is deterministic); wall time is
    filesystem-dependent and recorded as trend only.

    Measurement forks a worker per case, and a forked child cannot inherit a
    live Eio scheduler — so nothing here runs under [Eio_main.run] at top
    level. Each case's setup (inside the worker) populates a store under its
    own Eio loop; the measured closure then opens the store and scans it
    under a fresh loop. The measured unit is one whole picker-populate —
    scheduler start, store open, scan — a constant scheduler cost plus the
    K-dependent scan; the K-axis growth story is unchanged. *)

module Store = Mentat_store
module Fixture = Bench_support.Session_fixture

let list_axis = [ ("10", 10); ("100", 100); ("1k", 1_000) ]

(* Runs in the forked worker: build and populate a store, return its root. *)
let make_store count () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Eio.Switch.run @@ fun sw ->
  let raw = Filename.temp_dir "mentat-bench-list-" "" in
  let base = Unix.realpath raw in
  let handle =
    match Store.open_ ~sw (Eio.Path.( / ) fs base) with
    | Ok handle -> handle
    | Error error -> failwith (Store.Error.message error)
  in
  for i = 0 to count - 1 do
    let session = Fixture.build ~id:(Printf.sprintf "s-%d" i) ~events:10 () in
    match Store.Session.create handle session with
    | Ok _ -> ()
    | Error error -> failwith (Store.Session.Error.message error)
  done;
  base

let scan_store base =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  Eio.Switch.run @@ fun sw ->
  match Store.open_ ~sw (Eio.Path.( / ) fs base) with
  | Ok handle -> Store.Session.scan handle
  | Error error -> failwith (Store.Error.message error)

let () =
  Thumper.run "session_list"
    ~budgets:
      [
        (* Scan returns document paths built from a temp-dir root, so its
           allocation carries a few filesystem-path words that vary in length
           run to run — unlike the pure suites, it is not exactly reproducible.
           A small tolerance absorbs that jitter (observed well under 1%) while
           still catching a real regression, which would be proportional and
           far larger. Wall time is recorded but never gates. *)
        Thumper.Budget.no_more_alloc_than 0.05;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
      ]
    Thumper.
      [
        group "scan"
          (List.map
             (fun (label, count) ->
               bench_with_setup ~setup:(make_store count) label scan_store)
             list_axis);
      ]
