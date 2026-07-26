(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Instance = Mentat_ocaml_dune_rpc.Instance
module Health = Mentat_ocaml_dune_rpc.Instance.Health
module Diagnostic_store = Mentat_ocaml_dune_rpc.Diagnostic.Store
module Diagnostic = Mentat_ocaml.Diagnostic
module Notice = Mentat_workspace.Notice

(* One coalescing identity for the whole build-health lane: a newer verdict
   supersedes an older one, and the engine's progress lane keys on it too. *)
let key = "dune.build-health"
let source = "dune"

(* The drain-time probe opens a fresh short-lived RPC connection. The bound is
   the library default: it only elapses when an endpoint is actually registered
   (a missing one returns [Disconnected] at once, so a watch-less workspace pays
   nothing), and it caps the cost of one drain when one is. A turn drains once
   to prepare and once per tool claim it settles, so its total bound scales with
   the tools it runs; the per-drain bound stays modest on purpose, because a
   drain sits between a tool's result and the request that reports it. *)
let poll_timeout_s = 0.5

(* A [Clean] reading only retires a [Failing] baseline once a later reading
   agrees, and the two must be far enough apart to be independent evidence.
   Dune clears a rebuilt file's diagnostics before the rebuild republishes them,
   so every reading taken during a rebuild is clean while saying nothing about
   the build, and two tool claims settling back to back both land there.

   This is a heuristic, not a bound: a rebuild slower than the separation still
   reads as a recovery. It is set long because the two directions are not worth
   the same. A failure is the signal — the model's own edit broke something and
   here is the error — while a recovery is reassurance the model can obtain for
   itself by building; so reporting one late costs almost nothing, and reporting
   one falsely tells the model to stop working on a build that is still broken.
   Knowing rather than guessing needs dune to say it is building, which the
   one-shot diagnostics probe cannot ask. *)
let recovery_confirm_s = 15.0

type state = {
  (* The raw verdict of the last poll — what a frontend indicator reads. *)
  mutable raw : Health.t;
  (* The last [Clean]/[Failing] verdict: the dedup baseline. [Disconnected] and
     [Unknown] never overwrite it, so lost-and-regained visibility of the same
     failure does not re-notice. Starts [Clean] — the unremarkable baseline, so
     a first-seen failure is news and a first-seen clean is not. *)
  mutable concrete : Health.t;
  (* The head diagnostic text of the last [Failing] baseline, so a *different*
     failure re-notices while the same one stays quiet. *)
  mutable head : string option;
  (* When the still-unconfirmed [Clean] that stands against a [Failing] baseline
     was first read, on the monotonic clock. *)
  mutable clean_since : Mtime.t option;
}

type t = { instance : Instance.t; stdenv : Eio_unix.Stdenv.base; state : state }

let make ~stdenv ?env ~workspace () =
  let fs = Eio.Stdenv.fs stdenv in
  let net = Eio.Stdenv.net stdenv in
  let instance = Instance.create ~fs ~net ~workspace ?env () in
  {
    instance;
    stdenv;
    state =
      {
        raw = Health.Disconnected;
        concrete = Health.Clean;
        head = None;
        clean_since = None;
      };
  }

let health t = t.state.raw

(* The head is the first (deterministic-order) Dune diagnostic rendered as
   [path:line:col-line:col: message], the message clipped to its first line so a
   multi-line compiler error stays a one-line body head. *)
let render_head diagnostics =
  match Diagnostic_store.to_list diagnostics with
  | [] -> None
  | (_id, d) :: _ ->
      let message =
        let m = Diagnostic.message d in
        match String.index_opt m '\n' with
        | Some i -> String.sub m 0 i
        | None -> m
      in
      let text =
        match Diagnostic.location d with
        | Some loc ->
            Format.asprintf "%a: %s" Mentat_ocaml.Location.pp loc message
        | None -> message
      in
      if String.length text = 0 then None else Some text

let failing_notice ~count ~head =
  let title =
    Printf.sprintf "Build failing (%d diagnostic%s)" count
      (if count = 1 then "" else "s")
  in
  (* [Failing count] means [count >= 1] diagnostics, so a head is expected; the
     count-only fallback keeps the [body] invariant if the store raced empty. *)
  let body =
    match head with
    | Some head -> head
    | None -> Printf.sprintf "the build has %d unresolved diagnostic(s)" count
  in
  Notice.make ~source ~severity:Notice.Severity.Error ~title ~body ~key ()

let recovered_notice () =
  Notice.make ~source ~severity:Notice.Severity.Info ~title:"Build recovered"
    ~key ()

(* The pure dedup law: given the concrete baseline, its head, when an
   unconfirmed [Clean] first stood against it, and a fresh reading with its head
   at monotonic time [at], decide the notice and the next baseline.

   A failure speaks on the reading that finds it; a recovery speaks on a
   confirming reading at least [recovery_confirm_s] later. The asymmetry follows
   the cost of being wrong: a false recovery tells the model to stop working on
   a build that is still broken, while a recovery reported a couple of seconds
   late is reassurance, not a work item. Diagnostics that are really there are
   never transient, so nothing delays a failure.

   Lost visibility ([Disconnected]/[Unknown]) discards a pending recovery
   without touching the baseline. Confirmation is two clean readings far enough
   apart to be independent evidence, and going blind in between breaks that
   chain rather than extending it: a watch restarted across the gap reads clean
   through its whole first build, which would otherwise let an outage supply the
   separation that makes two meaningless readings count as agreement. Losing the
   pending state only delays a real recovery to the next pair of readings. *)
let transition ~concrete ~head ~clean_since ~at ~reading ~reading_head =
  let confirmed since =
    Mtime.span since at |> Mtime.Span.to_float_ns >= recovery_confirm_s *. 1e9
  in
  match (reading : Health.t) with
  | Health.Disconnected | Health.Unknown -> (None, concrete, head, None)
  | Health.Clean -> (
      match concrete with
      | Health.Failing _ -> (
          match clean_since with
          | Some since when confirmed since ->
              (Some (recovered_notice ()), Health.Clean, None, None)
          | Some _ -> (None, concrete, head, clean_since)
          | None -> (None, concrete, head, Some at))
      | Health.Clean | Health.Disconnected | Health.Unknown ->
          (None, Health.Clean, None, None))
  | Health.Failing count ->
      let changed =
        match concrete with
        | Health.Failing _ -> not (Option.equal String.equal head reading_head)
        | Health.Clean | Health.Disconnected | Health.Unknown -> true
      in
      let notice =
        if changed then Some (failing_notice ~count ~head:reading_head)
        else None
      in
      (notice, Health.Failing count, reading_head, None)

let drain t =
  let reading =
    Instance.build_health t.instance
      ~clock:(Eio.Stdenv.clock t.stdenv)
      ~timeout_s:poll_timeout_s ()
  in
  t.state.raw <- reading;
  let reading_head =
    match reading with
    | Health.Failing _ -> render_head (Instance.diagnostics t.instance)
    | Health.Clean | Health.Disconnected | Health.Unknown -> None
  in
  let notice, concrete, head, clean_since =
    transition ~concrete:t.state.concrete ~head:t.state.head
      ~clean_since:t.state.clean_since
      ~at:(Eio.Time.Mono.now (Eio.Stdenv.mono_clock t.stdenv))
      ~reading ~reading_head
  in
  t.state.concrete <- concrete;
  t.state.head <- head;
  t.state.clean_since <- clean_since;
  Option.to_list notice
