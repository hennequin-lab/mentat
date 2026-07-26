(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type step =
  | Progress of Mentat_provider.Auth.Login.Progress.t
  | Saved of Mentat_provider.Account.t
  | Cancelled

type steps = {
  next : unit -> (step, Mentat_protocol.Error.t) result;
  cancel : unit -> unit;
}

type t = {
  steps : steps;
  mutable cancelled : bool;
  mutable terminal : (step, Mentat_protocol.Error.t) result option;
}

let cancel t =
  if not t.cancelled then begin
    t.cancelled <- true;
    t.steps.cancel ()
  end

let attach ~sw steps =
  let t = { steps; cancelled = false; terminal = None } in
  (* Release-handler containment: [on_release] runs during switch teardown,
     where a raise from the untrusted [steps.cancel] would fail the switch. A
     direct {!cancel} on the caller's own fiber stays uncontained; only the
     release path swallows. *)
  Eio.Switch.on_release sw (fun () -> try cancel t with _ -> ());
  t

let next t =
  match t.terminal with
  | Some terminal -> terminal
  | None -> (
      let step =
        match t.steps.next () with
        | step -> step
        | exception (Eio.Cancel.Cancelled _ as cancelled) -> raise cancelled
        | exception _ ->
            Error
              (Mentat_protocol.Error.unavailable
                 "login.next failed unexpectedly")
      in
      match step with
      | Ok (Progress _) -> step
      | (Ok (Saved _ | Cancelled) | Error _) as terminal ->
          t.terminal <- Some terminal;
          terminal)
