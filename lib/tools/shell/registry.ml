(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Session = Mentat_workspace_io.Command.Session

let max_concurrent = 8

(* One registered process: its live session, the reader's incremental cursor
   (advanced by [read]/[kill]), and the display label. A submodule so its
   [handle] field does not collide with the exposed [started.handle]. *)
module Entry = struct
  type t = {
    handle : string;
    session : Session.t;
    mutable cursor : Session.Cursor.t;
    label : string;
  }
end

(* Entries accumulate over the session (a settled handle stays queryable) and
   are held newest-first. The count of RUNNING entries — not the total — bounds
   against [max_concurrent], so a settled process frees its slot. Single Eio
   domain, one tool callback at a time: no lock. *)
type t = {
  sw : Eio.Switch.t;
  mutable counter : int;
  mutable entries : Entry.t list;
}

let create ~sw = { sw; counter = 0; entries = [] }

type started = { handle : string; pid : int }

type start_error =
  | At_capacity of { running : string list }
  | Refused of Mentat_workspace_io.Command.Error.t

let is_running (entry : Entry.t) =
  match Session.status entry.Entry.session with
  | Session.Running -> true
  | Session.Exited _ | Session.Terminated -> false

(* Running entries in mint order (oldest handle first): entries are held
   newest-first, so the running sublist is reversed. *)
let running_entries t = List.rev (List.filter is_running t.entries)

let start t workspace_io ~label ?cwd argv =
  let running = running_entries t in
  if List.length running >= max_concurrent then
    Error
      (At_capacity
         { running = List.map (fun (e : Entry.t) -> e.Entry.handle) running })
  else
    match
      Mentat_workspace_io.Command.start_session workspace_io ~sw:t.sw ?cwd argv
    with
    | Error error -> Error (Refused error)
    | Ok session ->
        t.counter <- t.counter + 1;
        let handle = "bg_" ^ string_of_int t.counter in
        t.entries <-
          { Entry.handle; session; cursor = Session.Cursor.zero; label }
          :: t.entries;
        Ok { handle; pid = Session.pid session }

let find t ~handle =
  List.find_opt
    (fun (e : Entry.t) -> String.equal e.Entry.handle handle)
    t.entries

(* Advance the entry's cursor over one incremental read and return the chunk.
   [take] is the read: immediate for the final drain of a kill, deadlined for a
   reader that is waiting for the process to say something. *)
let advance take (entry : Entry.t) =
  let chunk = take entry.Entry.session entry.Entry.cursor in
  entry.Entry.cursor <- chunk.Session.next;
  chunk

let drain = advance (fun session from -> Session.read session ~from)

let read t ~handle ~cancelled ~seconds =
  Option.map
    (advance (fun session from ->
         Session.await session ~from ~cancelled ~seconds))
    (find t ~handle)

let kill t ~handle =
  Option.map
    (fun (entry : Entry.t) ->
      Session.signal entry.Entry.session;
      drain entry)
    (find t ~handle)

module View = struct
  type t = {
    handle : string;
    command : string;
    status : Session.status;
    since : Mtime.Span.t;
  }
end

let running t =
  List.map
    (fun (entry : Entry.t) ->
      {
        View.handle = entry.Entry.handle;
        command = entry.Entry.label;
        status = Session.status entry.Entry.session;
        since = Session.since entry.Entry.session;
      })
    (running_entries t)
