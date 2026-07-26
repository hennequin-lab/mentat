(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message =
  Import.invalid_arg' "Mentat_store.Run_lock.Owner" fn message

type t = { pid : int; host : string; label : string option; since : float }

let v ~pid ~host ~label ~since =
  (match label with
  | Some "" -> invalid "make" "label must not be empty"
  | Some _ | None -> ());
  { pid; host; label; since }

let make ?label () =
  v ~pid:(Unix.getpid ()) ~host:(Unix.gethostname ()) ~label
    ~since:(Unix.gettimeofday ())

let pid t = t.pid
let host t = t.host
let label t = t.label
let since t = t.since

let equal a b =
  Int.equal a.pid b.pid && String.equal a.host b.host
  && Option.equal String.equal a.label b.label
  && Float.equal a.since b.since

let pp ppf t =
  match t.label with
  | None -> Format.fprintf ppf "pid %d on %s" t.pid t.host
  | Some label -> Format.fprintf ppf "%s (pid %d on %s)" label t.pid t.host

let jsont =
  Jsont.Object.map ~kind:"run-lock owner" (fun pid host label since ->
      Import.decode_invalid_arg (fun () -> v ~pid ~host ~label ~since))
  |> Jsont.Object.mem "pid" Jsont.int ~enc:pid
  |> Jsont.Object.mem "host" Jsont.string ~enc:host
  |> Jsont.Object.opt_mem "label" Jsont.string ~enc:label
  |> Jsont.Object.mem "since" Jsont.number ~enc:since
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
