(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Unavailable of string
  | Cwd_outside_scope of Lpath.Abs.t
  | Escalation_denied
  | Escalation_irrelevant
  | Empty_program
  | Nul_in_argv of { index : int }
  | Stale_policy of { path : Lpath.Abs.t }
  | Grant_denied of { path : Lpath.Abs.t; denied : Lpath.Abs.t }
  | Grant_not_a_directory of { path : Lpath.Abs.t }

let kind_string = function
  | Unavailable _ -> "unavailable"
  | Cwd_outside_scope _ -> "cwd_outside_scope"
  | Escalation_denied -> "escalation_denied"
  | Escalation_irrelevant -> "escalation_irrelevant"
  | Empty_program -> "empty_program"
  | Nul_in_argv _ -> "nul_in_argv"
  | Stale_policy _ -> "stale_policy"
  | Grant_denied _ -> "grant_denied"
  | Grant_not_a_directory _ -> "grant_not_a_directory"

let message = function
  | Unavailable diagnostic -> diagnostic
  | Cwd_outside_scope cwd ->
      Printf.sprintf
        "working directory %s is outside the confined readable roots"
        (Lpath.Abs.to_string cwd)
  | Escalation_denied ->
      "the sealed posture promises no mutation: a read-only sandbox admits \
       neither an escalation nor a write grant"
  | Escalation_irrelevant ->
      "sandbox escalation is not meaningful for this execution route"
  | Empty_program -> "program must not be empty"
  | Nul_in_argv { index } ->
      Printf.sprintf "argv token %d contains a NUL byte" index
  | Stale_policy { path } ->
      Printf.sprintf
        "resolved sandbox path %s disappeared or changed kind after sealing"
        (Lpath.Abs.to_string path)
  | Grant_denied { path; denied } ->
      Printf.sprintf "%s cannot be granted: it lies under the denied path %s"
        (Lpath.Abs.to_string path)
        (Lpath.Abs.to_string denied)
  | Grant_not_a_directory { path } ->
      Printf.sprintf
        "%s cannot be granted: a grant names an existing directory; grant %s \
         instead"
        (Lpath.Abs.to_string path)
        (Filename.dirname (Lpath.Abs.to_string path))

let equal a b =
  match (a, b) with
  | Unavailable a, Unavailable b -> String.equal a b
  | Cwd_outside_scope a, Cwd_outside_scope b -> Lpath.Abs.equal a b
  | Escalation_denied, Escalation_denied -> true
  | Escalation_irrelevant, Escalation_irrelevant -> true
  | Empty_program, Empty_program -> true
  | Nul_in_argv a, Nul_in_argv b -> Int.equal a.index b.index
  | Stale_policy a, Stale_policy b -> Lpath.Abs.equal a.path b.path
  | Grant_denied a, Grant_denied b ->
      Lpath.Abs.equal a.path b.path && Lpath.Abs.equal a.denied b.denied
  | Grant_not_a_directory a, Grant_not_a_directory b ->
      Lpath.Abs.equal a.path b.path
  | ( ( Unavailable _ | Cwd_outside_scope _ | Escalation_denied
      | Escalation_irrelevant | Empty_program | Nul_in_argv _ | Stale_policy _
      | Grant_denied _ | Grant_not_a_directory _ ),
      _ ) ->
      false

let pp ppf t = Format.pp_print_string ppf (message t)

let to_json t =
  let mem name value = Jsont.Json.mem (Jsont.Json.name name) value in
  let string_mem name value = mem name (Jsont.Json.string value) in
  let structured =
    match t with
    | Cwd_outside_scope path
    | Stale_policy { path }
    | Grant_not_a_directory { path } ->
        [ string_mem "path" (Lpath.Abs.to_string path) ]
    | Grant_denied { path; denied } ->
        [
          string_mem "path" (Lpath.Abs.to_string path);
          string_mem "denied" (Lpath.Abs.to_string denied);
        ]
    | Nul_in_argv { index } -> [ mem "index" (Jsont.Json.int index) ]
    | Unavailable _ | Escalation_denied | Escalation_irrelevant | Empty_program
      ->
        []
  in
  Jsont.Json.object'
    ([ string_mem "kind" (kind_string t); string_mem "message" (message t) ]
    @ structured)
