(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let ring_capacity = 256

type feed = {
  mutable cursor : cursor;
  ring : Mentat_protocol.Progress.t Queue.t; (* Bounded, drop-oldest. *)
  mutable closed : bool;
  hub : hub;
}

and cursor =
  | From of Mentat_protocol.Position.t option
    (* Not yet resolved: the [from] the feed subscribed at ([None] is the
         whole journal). The one membership check runs on the first poll. *)
  | At of int
(* Resolved: the next index into [hub.projected] to serve. *)

and hub = {
  cond : Eio.Condition.t;
  mutable session : Mentat_session.t;
  mutable proj : Mentat_protocol.Projection.resume;
      (* The carried fold cursor: how far [projected] has materialized the
         journal. Advanced once per commit, never restarted. The mutation state
         is not held — it is joined into [projected] at fold time and never read
         back, so each advance takes the caller's current state as an argument. *)
  projected : (Mentat_protocol.Position.t * Mentat_protocol.Fact.t) Dynarray.t;
      (* The materialized projection: every emitted fact in sequence order,
         shared by all feeds and grown append-only. *)
  mutable feeds : feed list;
}

module Hub = struct
  type t = hub

  (* Materialize the whole journal once. A hub is created for a freshly loaded
     session — empty for a not-yet-driven session, or a full journal on
     re-follow after eviction — so the fold from [start] covers all of it; later
     commits extend it through [publish]. *)
  let create ~session ~mutation =
    let proj, entries =
      Mentat_protocol.Projection.advance Mentat_protocol.Projection.start
        ~session ~mutation
        ~delta:(Mentat_session.events session)
    in
    {
      cond = Eio.Condition.create ();
      session;
      proj;
      projected = Dynarray.of_list entries;
      feeds = [];
    }

  let publish t ~delta session mutation =
    let proj, entries =
      Mentat_protocol.Projection.advance t.proj ~session ~mutation ~delta
    in
    t.proj <- proj;
    List.iter (fun entry -> Dynarray.add_last t.projected entry) entries;
    t.session <- session;
    Eio.Condition.broadcast t.cond

  (* Bring the materialization up to a freshly loaded [session] that another
     process may have extended since this hub last published. The delta is the
     journal beyond what [proj] has folded, so a hub already current does no
     projection work; the fold is the same one [publish] runs, so the caught-up
     facts are byte-identical to an incremental advance. *)
  let sync t session mutation =
    let delta =
      let rec drop n = function
        | _ :: rest when n > 0 -> drop (n - 1) rest
        | events -> events
      in
      drop
        (Mentat_protocol.Projection.consumed t.proj)
        (Mentat_session.events session)
    in
    publish t ~delta session mutation

  let pulse t progress =
    List.iter
      (fun feed ->
        if not feed.closed then begin
          if Queue.length feed.ring >= ring_capacity then
            ignore (Queue.pop feed.ring);
          Queue.push progress feed.ring
        end)
      t.feeds;
    Eio.Condition.broadcast t.cond

  let head t = t.session
  let feedless t = match t.feeds with [] -> true | _ :: _ -> false

  let head_position t =
    if Dynarray.is_empty t.projected then None
    else Some (fst (Dynarray.get_last t.projected))

  let projected_length t = Dynarray.length t.projected
  let projected_entry t i = Dynarray.get t.projected i
  let await t check = Eio.Condition.loop_no_mutex t.cond check
end

type t = feed

let subscribe ?from hub =
  let feed =
    { cursor = From from; ring = Queue.create (); closed = false; hub }
  in
  hub.feeds <- feed :: hub.feeds;
  feed

(* The emitted facts of [projected] carry strictly increasing sequences (the
   projector mints a position only at an emitted fact, in scan order), so a
   position is a member of this feed iff its sequence appears here: an O(log E)
   binary search over the shared materialization. *)
let index_of_seq projected seq =
  let rec search lo hi =
    if lo > hi then None
    else
      let mid = (lo + hi) / 2 in
      let mid_seq =
        Mentat_protocol.Position.seq (fst (Dynarray.get projected mid))
      in
      if Int.equal mid_seq seq then Some mid
      else if mid_seq < seq then search (mid + 1) hi
      else search lo (mid - 1)
  in
  search 0 (Dynarray.length projected - 1)

(* Resolve the feed's cursor to the next index into the hub's materialized
   projection. On the first poll with a [from] position this is the one
   membership check — the same law [Projection.after] enforces, now an index
   lookup: a foreign session or a sequence that names no emitted fact is a
   structured error, never a silent skip. *)
let resolve t =
  match t.cursor with
  | At next -> Ok next
  | From None ->
      t.cursor <- At 0;
      Ok 0
  | From (Some position) -> (
      let hub_id = Mentat_session.id t.hub.session in
      let actual = Mentat_protocol.Position.session position in
      if not (Mentat_session.Id.equal actual hub_id) then
        Error
          (Mentat_protocol.Error.Invalid_position
             (Mentat_protocol.Position.Invalid.Foreign_session
                { expected = hub_id; actual }))
      else
        match
          index_of_seq t.hub.projected (Mentat_protocol.Position.seq position)
        with
        | Some idx ->
            let next = idx + 1 in
            t.cursor <- At next;
            Ok next
        | None ->
            Error
              (Mentat_protocol.Error.Invalid_position
                 (Mentat_protocol.Position.Invalid.Not_in_feed position)))

(* The ready-check runs under the condition's registration, so it must not
   block: serving a committed fact is an O(1) read of the shared projection at
   the feed's cursor, and catch-up is the O(log E) membership resolve above.
   Committed facts drain before progress. *)
let poll t =
  if t.closed then Some (Ok Mentat_client.Feed.Closed)
  else
    match resolve t with
    | Error e -> Some (Error e)
    | Ok next ->
        if next < Dynarray.length t.hub.projected then begin
          let position, fact = Dynarray.get t.hub.projected next in
          t.cursor <- At (next + 1);
          Some
            (Ok
               (Mentat_client.Feed.Item
                  (Mentat_protocol.Update.Committed { position; fact })))
        end
        else if Queue.is_empty t.ring then None
        else
          Some
            (Ok
               (Mentat_client.Feed.Item
                  (Mentat_protocol.Update.Progress (Queue.pop t.ring))))

let next t = Eio.Condition.loop_no_mutex t.hub.cond (fun () -> poll t)

let close t =
  t.closed <- true;
  t.hub.feeds <- List.filter (fun f -> f != t) t.hub.feeds;
  Eio.Condition.broadcast t.hub.cond
