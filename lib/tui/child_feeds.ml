(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Client = Mentat_client
module Protocol = Mentat_protocol
module Session = Mentat_session

module Generation = struct
  type t = unit ref

  let make () = ref ()
  let equal a b = a == b
end

type observation = Live | Drill

type slot = {
  observation : observation;
  generation : Generation.t;
  child : Session.Id.t;
  mutable feed : Client.Feed.t option;
  mutable last_position : Protocol.Position.t option;
  mutable resumed : bool;
}

type t = {
  sw : Eio.Switch.t;
  client : Client.t;
  emit :
    observation:observation ->
    generation:Generation.t ->
    child:Session.Id.t ->
    (Client.Feed.outcome, Protocol.Error.t) result ->
    unit;
  mutex : Eio.Mutex.t;
  mutable live : slot list;
  mutable drill : slot option;
}

let with_state t fn = Eio.Mutex.use_rw ~protect:true t.mutex fn
let same_slot a b = a == b

let is_current t slot =
  match slot.observation with
  | Live -> List.exists (same_slot slot) t.live
  | Drill -> Option.exists (same_slot slot) t.drill

let remove_current t slot =
  match slot.observation with
  | Live ->
      t.live <-
        List.filter (fun candidate -> not (same_slot slot candidate)) t.live
  | Drill -> if Option.exists (same_slot slot) t.drill then t.drill <- None

let detach_locked t slot =
  if is_current t slot then begin
    remove_current t slot;
    let feed = slot.feed in
    slot.feed <- None;
    feed
  end
  else None

let detach t slot = with_state t (fun () -> detach_locked t slot)

(* Feed.close is cleanup and must not let a broken transport's close callback
   take down the sibling observations. The client applies the same containment
   to switch-release cleanup. *)
let close_feed = function
  | None -> ()
  | Some feed ->
      Eio.Cancel.protect (fun () -> try Client.Feed.close feed with _ -> ())

let close_slot t slot = close_feed (detach t slot)

let emit t slot outcome =
  let admitted = with_state t (fun () -> is_current t slot) in
  if not admitted then false
  else
    match
      t.emit ~observation:slot.observation ~generation:slot.generation
        ~child:slot.child outcome
    with
    | () -> true
    | exception (Eio.Cancel.Cancelled _ as cancelled) -> raise cancelled
    | exception _ ->
        close_slot t slot;
        false

let unavailable operation exn =
  Protocol.Error.unavailable
    (Printf.sprintf "child session feed %s raised: %s" operation
       (Printexc.to_string exn))

let follow t slot from =
  match Client.follow_session ~sw:t.sw t.client slot.child ~from with
  | result -> result
  | exception (Eio.Cancel.Cancelled _ as cancelled) -> raise cancelled
  | exception exn -> Error (unavailable "attach" exn)

let next feed =
  match Client.Feed.next feed with
  | result -> result
  | exception (Eio.Cancel.Cancelled _ as cancelled) -> raise cancelled
  | exception exn -> Error (unavailable "pull" exn)

type item_action = Ignore | Continue | Settle

let record_item t slot update =
  with_state t @@ fun () ->
  if not (is_current t slot) then Ignore
  else
    match update with
    | Protocol.Update.Committed { position; fact } ->
        slot.last_position <- Some position;
        if
          slot.observation = Live
          && match fact with Protocol.Fact.Turn_settled _ -> true | _ -> false
        then Settle
        else Continue
    | Protocol.Update.Progress _ -> Continue

type closed_action =
  | Closed_ignore
  | Closed_emit
  | Closed_resume of Client.Feed.from

let closed_action t slot =
  with_state t @@ fun () ->
  if not (is_current t slot) then Closed_ignore
  else if slot.resumed then Closed_emit
  else
    match (slot.last_position, slot.observation) with
    | Some position, _ ->
        slot.resumed <- true;
        slot.feed <- None;
        Closed_resume (`After position)
    | None, Drill ->
        (* No committed item was emitted, so restarting the full-history read
           cannot duplicate durable transcript rows. Progress is droppable. *)
        slot.resumed <- true;
        slot.feed <- None;
        Closed_resume `Beginning
    | None, Live -> Closed_emit

let install_feed t slot feed =
  with_state t @@ fun () ->
  if is_current t slot then begin
    slot.feed <- Some feed;
    true
  end
  else false

let rec observe t slot from =
  let current = with_state t (fun () -> is_current t slot) in
  if not current then ()
  else
    match follow t slot from with
    | Error error ->
        ignore (emit t slot (Error error));
        close_slot t slot
    | Ok feed ->
        if not (install_feed t slot feed) then close_feed (Some feed)
        else pull t slot feed

and pull t slot feed =
  match next feed with
  | Error error ->
      ignore (emit t slot (Error error));
      close_slot t slot
  | Ok Client.Feed.Closed -> (
      match closed_action t slot with
      | Closed_ignore -> close_feed (Some feed)
      | Closed_emit ->
          ignore (emit t slot (Ok Client.Feed.Closed));
          close_slot t slot
      | Closed_resume from ->
          close_feed (Some feed);
          observe t slot from)
  | Ok (Client.Feed.Item update as outcome) -> (
      match record_item t slot update with
      | Ignore -> close_feed (Some feed)
      | Continue -> if emit t slot (Ok outcome) then pull t slot feed
      | Settle ->
          ignore (emit t slot (Ok outcome));
          close_slot t slot)

let start t slot from =
  Eio.Fiber.fork_daemon ~sw:t.sw (fun () ->
      (* [fork_daemon] initially runs the child. Yielding first guarantees the
         caller receives [slot.generation] before any callback is admitted. *)
      Eio.Fiber.yield ();
      observe t slot from;
      `Stop_daemon)

let make_slot observation child =
  {
    observation;
    generation = Generation.make ();
    child;
    feed = None;
    last_position = None;
    resumed = false;
  }

let close_drill t ~child ~generation =
  let feed =
    with_state t @@ fun () ->
    match t.drill with
    | Some slot
      when Session.Id.equal slot.child child
           && Generation.equal slot.generation generation ->
        t.drill <- None;
        let feed = slot.feed in
        slot.feed <- None;
        feed
    | Some _ | None -> None
  in
  close_feed feed

let close_pane t =
  let feeds =
    with_state t @@ fun () ->
    let slots =
      match t.drill with Some drill -> drill :: t.live | None -> t.live
    in
    t.live <- [];
    t.drill <- None;
    List.map
      (fun slot ->
        let feed = slot.feed in
        slot.feed <- None;
        feed)
      slots
  in
  List.iter close_feed feeds

let follow_live t child =
  Eio.Switch.check t.sw;
  let slot, fresh =
    with_state t @@ fun () ->
    match
      List.find_opt (fun slot -> Session.Id.equal slot.child child) t.live
    with
    | Some slot -> (slot, false)
    | None ->
        let slot = make_slot Live child in
        t.live <- slot :: t.live;
        (slot, true)
  in
  if fresh then start t slot `Beginning;
  slot.generation

let drill t child =
  Eio.Switch.check t.sw;
  let previous_feed, slot =
    with_state t @@ fun () ->
    let previous_feed =
      match t.drill with
      | None -> None
      | Some previous ->
          let feed = previous.feed in
          previous.feed <- None;
          feed
    in
    let slot = make_slot Drill child in
    t.drill <- Some slot;
    (previous_feed, slot)
  in
  close_feed previous_feed;
  start t slot `Beginning;
  slot.generation

let create ~sw ~client ~emit =
  let t =
    { sw; client; emit; mutex = Eio.Mutex.create (); live = []; drill = None }
  in
  Eio.Switch.on_release sw (fun () -> close_pane t);
  t
