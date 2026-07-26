(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type from = [ `Beginning | `Now | `After of Mentat_protocol.Position.t ]
type outcome = Item of Mentat_protocol.Update.t | Closed

type seam = {
  next : unit -> (outcome, Mentat_protocol.Error.t) result;
  close : unit -> unit;
}

type t = { seam : seam; mutable closed : bool; mutable failed : bool }

(* Fault containment: the seam's [next] is an untrusted closure — the engine
   bridge in v1, a socket read at S1 — that may raise rather than return an
   [Error]. A raise is contained into one terminal [Unavailable] on this feed's
   own error channel, after which the feed reports [Closed], so a broken seam
   fails only its own feed. Cancellation is runtime teardown from
   {!close}, never a seam fault, so it re-raises unchanged. Once closed or
   failed the feed is terminal and never touches the seam again. *)
let next t =
  if t.closed || t.failed then Ok Closed
  else
    match t.seam.next () with
    | result -> result
    | exception (Eio.Cancel.Cancelled _ as cancelled) -> raise cancelled
    | exception exn ->
        t.failed <- true;
        Error
          (Mentat_protocol.Error.unavailable
             (Printf.sprintf "session feed seam raised: %s"
                (Printexc.to_string exn)))

let close t =
  if not t.closed then begin
    t.closed <- true;
    t.seam.close ()
  end

let subscribe ~sw seam =
  let t = { seam; closed = false; failed = false } in
  (* Release-handler containment: [on_release] runs during switch teardown,
     where a raise from the untrusted [seam.close] would fail the switch — hard
     to attribute, and contrary to release being cleanup. A direct {!close} on
     the caller's own fiber stays uncontained (a caller-visible host bug is the
     caller's to see); only the release path swallows. *)
  Eio.Switch.on_release sw (fun () -> try close t with _ -> ());
  t
