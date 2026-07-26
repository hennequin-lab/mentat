(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type child_result = Mentat_llm.Content.t list * Mentat_llm.Usage.t option

type t = {
  capacity : int;
  held : (string, unit) Hashtbl.t;
      (* Delegation edges holding a permit. May exceed [capacity] after a
         forced rebuild. *)
  settled : (string, child_result) Hashtbl.t; (* By delegation id. *)
  parents :
    (string, Mentat_session.Id.t * Mentat_session.Delegation.Id.t) Hashtbl.t;
      (* Child session id to owning parent and edge. *)
}

let create ~capacity =
  if capacity <= 0 then
    invalid_arg "Scheduler.create: capacity must be positive";
  {
    capacity;
    held = Hashtbl.create 8;
    settled = Hashtbl.create 8;
    parents = Hashtbl.create 8;
  }

let key delegation = Mentat_session.Delegation.Id.to_string delegation

(* Permit operations run on controller fibers of one domain and never suspend,
   so check-then-update is atomic. *)

let try_reserve t ~delegation =
  let key = key delegation in
  if Hashtbl.mem t.held key then `Granted
  else if Hashtbl.length t.held >= t.capacity then
    `Refused
      (Mentat_agent_step.Step.Reservation.Refusal.Capacity
         { limit = t.capacity })
  else begin
    Hashtbl.replace t.held key ();
    `Granted
  end

let force_reserve t ~delegation = Hashtbl.replace t.held (key delegation) ()
let release t ~delegation = Hashtbl.remove t.held (key delegation)

let bind t ~child ~parent ~delegation =
  Hashtbl.replace t.parents
    (Mentat_session.Id.to_string child)
    (parent, delegation)

let parent_of t child =
  Hashtbl.find_opt t.parents (Mentat_session.Id.to_string child)

let note_settled t delegation result =
  Hashtbl.replace t.settled
    (Mentat_session.Delegation.Id.to_string delegation)
    result

let settled_for t children =
  List.filter_map
    (fun id ->
      Hashtbl.find_opt t.settled (Mentat_session.Delegation.Id.to_string id)
      |> Option.map (fun result -> (id, result)))
    children
