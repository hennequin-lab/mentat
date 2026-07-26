(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type escalation = Available | Denied of Error.t | Ignored

module Obligation = struct
  type kind = Exists | Directory
  type t = { path : Lpath.Abs.t; kind : kind }

  let path t = t.path
  let kind t = t.kind
  let kind_to_string = function Exists -> "exists" | Directory -> "directory"

  let pp ppf t =
    Format.fprintf ppf "%s %a" (kind_to_string t.kind) Lpath.Abs.pp t.path
end

type route =
  | Unconfined
  | Declared_external
  | Confine of { policy : Policy.t; profile : Profile.t; evidence : Evidence.t }
  | Refuse of { policy : Policy.t; error : Error.t }

type t = { route : route; escalation : escalation; identity : Identity.t }

let direct =
  {
    route = Unconfined;
    escalation = Ignored;
    identity = Identity.not_requested;
  }

let external_ =
  {
    route = Declared_external;
    escalation = Ignored;
    identity = Identity.declared_external;
  }

let confined ~backend policy =
  let escalation =
    match Policy.writable_roots policy with
    | [] -> Denied Error.Escalation_denied
    | _ :: _ -> Available
  in
  match backend with
  | Error error ->
      {
        route = Refuse { policy; error };
        escalation;
        identity = Identity.refused;
      }
  | Ok backend ->
      let profile = Profile.prepare backend policy in
      let evidence =
        Evidence.enforced ~backend ~profile:(Profile.digest profile)
      in
      {
        route = Confine { policy; profile; evidence };
        escalation;
        identity =
          Identity.enforced ~backend
            ~profile:(Profile.identity_digest backend policy);
      }

let policy t =
  match t.route with
  | Confine { policy; _ } | Refuse { policy; _ } -> Some policy
  | Unconfined | Declared_external -> None

let escalation t = t.escalation
let identity t = t.identity

let evidence t =
  match t.route with
  | Unconfined -> Evidence.not_requested
  | Declared_external -> Evidence.declared_external
  | Confine { evidence; _ } -> evidence
  | Refuse { error; _ } -> Evidence.refused error

let escalated_evidence _ = Evidence.not_requested

(* OS argv cannot represent an empty program or a NUL byte. The check mints
   [Error.t] here so every lowering validates; its enforcement home is the
   single launch boundary, which lowers every command through this library. *)
let validated_argv argv =
  match argv with
  | [] | "" :: _ -> Error Error.Empty_program
  | _ ->
      let rec check index = function
        | [] -> Ok argv
        | token :: rest ->
            if String.contains token '\000' then
              Error (Error.Nul_in_argv { index })
            else check (index + 1) rest
      in
      check 0 argv

let cwd_within reads cwd =
  match reads with
  | Policy.Only roots ->
      List.exists (fun root -> Lpath.Abs.is_within ~root cwd) roots
  | Policy.All -> true

let lower_argv t ~cwd argv =
  match validated_argv argv with
  | Error _ as error -> error
  | Ok argv -> (
      match t.route with
      | Unconfined | Declared_external -> Ok argv
      | Refuse { error; _ } -> Error error
      | Confine { policy; profile; _ } ->
          if cwd_within (Policy.reads policy) cwd then
            Ok (Profile.wrap profile ~cwd argv)
          else Error (Error.Cwd_outside_scope cwd))

let lower_escalated_argv t ~cwd:_ argv =
  match validated_argv argv with
  | Error _ as error -> error
  | Ok argv -> (
      match t.escalation with
      | Available -> Ok argv
      | Denied error -> Error error
      | Ignored -> Error Error.Escalation_irrelevant)

let obligations t =
  match t.route with
  | Unconfined | Declared_external | Refuse _ -> []
  | Confine { policy; _ } ->
      let readable =
        match Policy.reads policy with
        | Policy.Only roots -> roots
        | Policy.All -> []
      in
      let protected = Policy.protected_paths policy in
      let directories = Policy.scratch policy :: Policy.writable_roots policy in
      let is_directory path = List.exists (Lpath.Abs.equal path) directories in
      List.sort_uniq Lpath.Abs.compare (readable @ protected @ directories)
      |> List.map (fun path ->
          let kind =
            if is_directory path then Obligation.Directory
            else Obligation.Exists
          in
          { Obligation.path; kind })
