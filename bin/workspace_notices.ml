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

(* The drain-time probe opens a fresh short-lived RPC connection each turn. The
   bound is the library default: it only elapses when an endpoint is actually
   registered (a missing one returns [Disconnected] at once, so a watch-less turn
   pays nothing), and it caps the per-turn cost when one is. It stays modest on
   purpose — a turn must not stall on the health poll. *)
let poll_timeout_s = 0.5

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
}

type t = { instance : Instance.t; stdenv : Eio_unix.Stdenv.base; state : state }

let make ~stdenv ?env ~workspace () =
  let fs = Eio.Stdenv.fs stdenv in
  let net = Eio.Stdenv.net stdenv in
  let instance = Instance.create ~fs ~net ~workspace ?env () in
  {
    instance;
    stdenv;
    state = { raw = Health.Disconnected; concrete = Health.Clean; head = None };
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

(* The pure dedup law: given the concrete baseline, its head, the fresh verdict
   and its head, decide the notice and the next baseline. *)
let transition ~concrete ~head ~now ~now_head =
  match (now : Health.t) with
  | Health.Disconnected | Health.Unknown -> (None, concrete, head)
  | Health.Clean -> (
      match concrete with
      | Health.Failing _ -> (Some (recovered_notice ()), Health.Clean, None)
      | Health.Clean | Health.Disconnected | Health.Unknown ->
          (None, Health.Clean, None))
  | Health.Failing count ->
      let changed =
        match concrete with
        | Health.Failing _ -> not (Option.equal String.equal head now_head)
        | Health.Clean | Health.Disconnected | Health.Unknown -> true
      in
      let notice =
        if changed then Some (failing_notice ~count ~head:now_head) else None
      in
      (notice, Health.Failing count, now_head)

let drain t =
  let now =
    Instance.build_health t.instance
      ~clock:(Eio.Stdenv.clock t.stdenv)
      ~timeout_s:poll_timeout_s ()
  in
  t.state.raw <- now;
  let now_head =
    match now with
    | Health.Failing _ -> render_head (Instance.diagnostics t.instance)
    | Health.Clean | Health.Disconnected | Health.Unknown -> None
  in
  let notice, concrete, head =
    transition ~concrete:t.state.concrete ~head:t.state.head ~now ~now_head
  in
  t.state.concrete <- concrete;
  t.state.head <- head;
  Option.to_list notice
