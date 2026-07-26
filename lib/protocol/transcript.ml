(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* One projected fact on the wire: the position and the fact, the same inner
   shape [Update.Committed] carries. The fact carries its own envelope [v]; the
   position is an envelope-free leaf, so a page's array elements nest exactly as
   a committed update's do. *)
let entry_jsont =
  Jsont.Object.map ~kind:"projected fact" (fun position fact ->
      (position, fact))
  |> Jsont.Object.mem "position" Position.jsont ~enc:fst
  |> Jsont.Object.mem "fact" Fact.jsont ~enc:snd
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let entry_equal (pa, fa) (pb, fb) = Position.equal pa pb && Fact.equal fa fb

(* The projection's emitted facts carry strictly increasing sequences, so a
   position is a member iff its sequence appears — an O(log length) binary search
   over the random-access reader, the one membership check [Page.before] shares
   with {!Projection.after}. *)
let index_of_seq ~length ~get seq =
  let rec search lo hi =
    if lo > hi then None
    else
      let mid = (lo + hi) / 2 in
      let mid_seq = Position.seq (fst (get mid)) in
      if Int.equal mid_seq seq then Some mid
      else if mid_seq < seq then search (mid + 1) hi
      else search lo (mid - 1)
  in
  search 0 (length - 1)

(* Carve the bounded window [[lo, hi)] of the projection, oldest first, with the
   backward continuation: [before] is the first returned fact's position when
   facts remain below [lo], else [None]. [get] is read only within the window, so
   an empty window ([hi <= lo]) reads nothing. *)
let window ~get ~length ~hi ~n =
  let n = if n < 0 then 0 else n in
  let lo = if hi - n < 0 then 0 else hi - n in
  let facts = List.init (hi - lo) (fun i -> get (lo + i)) in
  let before = if lo > 0 && lo < length then Some (fst (get lo)) else None in
  (facts, before)

module Page = struct
  type t = { facts : (Position.t * Fact.t) list; before : Position.t option }

  let equal a b =
    List.equal entry_equal a.facts b.facts
    && Option.equal Position.equal a.before b.before

  let pp ppf t =
    Format.fprintf ppf "page(%d facts, before %a)" (List.length t.facts)
      (Format.pp_print_option Position.pp)
      t.before

  let jsont =
    Jsont.Object.map ~kind:"transcript page" (fun v facts before ->
        Wire.check v;
        { facts; before })
    |> Wire.mem
    |> Jsont.Object.mem "facts" (Jsont.list entry_jsont) ~enc:(fun t -> t.facts)
    |> Jsont.Object.opt_mem "before" Position.jsont ~enc:(fun t -> t.before)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let recent ~length ~get ~n =
    let facts, before = window ~get ~length ~hi:length ~n in
    { facts; before }

  let before ~owner ~length ~get p ~n =
    let actual = Position.session p in
    if not (Mentat_session.Id.equal actual owner) then
      Error (Position.Invalid.Foreign_session { expected = owner; actual })
    else
      match index_of_seq ~length ~get (Position.seq p) with
      | None -> Error (Position.Invalid.Not_in_feed p)
      | Some idx ->
          let facts, before = window ~get ~length ~hi:idx ~n in
          Ok { facts; before }
end

module Tail = struct
  let default_n = 100
  let max_n = 1000

  type t = {
    head : Position.t option;
    pending : Mentat_session.Decision.Requested.t option;
    page : Page.t;
  }

  let equal a b =
    Option.equal Position.equal a.head b.head
    && Option.equal Mentat_session.Decision.Requested.equal a.pending b.pending
    && Page.equal a.page b.page

  let pp ppf t =
    Format.fprintf ppf "tail(head %a, pending %b, %a)"
      (Format.pp_print_option Position.pp)
      t.head (Option.is_some t.pending) Page.pp t.page

  let jsont =
    Jsont.Object.map ~kind:"transcript tail" (fun v head pending page ->
        Wire.check v;
        { head; pending; page })
    |> Wire.mem
    |> Jsont.Object.opt_mem "head" Position.jsont ~enc:(fun t -> t.head)
    |> Jsont.Object.opt_mem "pending" Mentat_session.Decision.Requested.jsont
         ~enc:(fun t -> t.pending)
    |> Jsont.Object.mem "page" Page.jsont ~enc:(fun t -> t.page)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let of_projection ~length ~get ~n ~pending =
    let head = if length > 0 then Some (fst (get (length - 1))) else None in
    { head; pending; page = Page.recent ~length ~get ~n }
end
