(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_session.Delegation" fn message

type session_id = Id.t

(* The delegation's own [Id] shadows the session [Id] below, so capture the
   session id module first for [parent]/[child] (as [State] does for its nested
   error families). *)
module Session_id = Id

module Id =
  String_id.Make
    (struct
      let module_path = "Mentat_session.Delegation.Id"
      let kind = "delegation id"
    end)
    ()

let derive ~source_turn ~source_call =
  Id.of_string
    (String_id.derive_id ~domain:"mentat.session.delegation.v1"
       [ Turn.Id.to_string source_turn; source_call ])

module Role = struct
  type t = Explore | Review | Verify

  let equal a b = a = b

  let to_string = function
    | Explore -> "explore"
    | Review -> "review"
    | Verify -> "verify"

  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.enum ~kind:"delegation role"
      [ ("explore", Explore); ("review", Review); ("verify", Verify) ]
end

type t = {
  child : session_id;
  source_turn : Turn.Id.t;
  source_call : string;
  description : string option;
  role : Role.t option;
  task : Mentat_llm.Content.t list;
}

(* An empty or whitespace-only label carries no identity, so it is normalized to
   absent at construction and on decode. *)
let normalize_description = function
  | None -> None
  | Some text -> if String.is_empty (String.trim text) then None else Some text

let make ~child ~source_turn ~source_call ?description ?role ~task () =
  if String.is_empty source_call then
    invalid "make" "source_call must not be empty";
  if List.is_empty task then invalid "make" "task must not be empty";
  (* R15: [task] is a media-bearing content carrier, safe today only because it
     is constructed text-only. A media block here would ride the delegation fact
     without joining the media_refs projection, document versioning, and the
     externalize/resolve passes, so a [`Base64] could reach the journal
     unrelocated or a branch's attachment blobs be incomplete. Reject media until
     a future media task explicitly joins those passes. *)
  List.iter
    (function
      | Mentat_llm.Content.Text _ -> ()
      | Mentat_llm.Content.Media _ ->
          invalid "make" "task must not carry media content")
    task;
  {
    child;
    source_turn;
    source_call;
    description = normalize_description description;
    role;
    task;
  }

let id t = derive ~source_turn:t.source_turn ~source_call:t.source_call
let child t = t.child
let source_turn t = t.source_turn
let source_call t = t.source_call
let description t = t.description
let role t = t.role
let task t = t.task

let equal a b =
  Session_id.equal a.child b.child
  && Turn.Id.equal a.source_turn b.source_turn
  && String.equal a.source_call b.source_call
  && Option.equal String.equal a.description b.description
  && Option.equal Role.equal a.role b.role
  && a.task = b.task

let pp ppf t =
  Format.fprintf ppf "@[<hov>{ id = %a; child = %a }@]" Id.pp (id t)
    Session_id.pp t.child

let jsont =
  Jsont.Object.map ~kind:"delegation edge"
    (fun child source_turn source_call description role task ->
      decode_invalid_arg (fun () ->
          make ~child ~source_turn ~source_call ?description ?role ~task ()))
  |> Jsont.Object.mem "child" Session_id.jsont ~enc:child
  |> Jsont.Object.mem "source_turn" Turn.Id.jsont ~enc:source_turn
  |> Jsont.Object.mem "source_call" Jsont.string ~enc:source_call
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:description
  |> Jsont.Object.opt_mem "role" Role.jsont ~enc:role
  |> Jsont.Object.mem "task" (Jsont.list Mentat_llm.Content.jsont) ~enc:task
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
