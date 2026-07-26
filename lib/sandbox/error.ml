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

let kind_string = function
  | Unavailable _ -> "unavailable"
  | Cwd_outside_scope _ -> "cwd_outside_scope"
  | Escalation_denied -> "escalation_denied"
  | Escalation_irrelevant -> "escalation_irrelevant"
  | Empty_program -> "empty_program"
  | Nul_in_argv _ -> "nul_in_argv"
  | Stale_policy _ -> "stale_policy"

let message = function
  | Unavailable diagnostic -> diagnostic
  | Cwd_outside_scope cwd ->
      Printf.sprintf
        "working directory %s is outside the confined readable roots"
        (Lpath.Abs.to_string cwd)
  | Escalation_denied ->
      "the sealed policy has no writable roots: a read-only sandbox admits no \
       escalation"
  | Escalation_irrelevant ->
      "sandbox escalation is not meaningful for this execution route"
  | Empty_program -> "program must not be empty"
  | Nul_in_argv { index } ->
      Printf.sprintf "argv token %d contains a NUL byte" index
  | Stale_policy { path } ->
      Printf.sprintf
        "resolved sandbox path %s disappeared or changed kind after sealing"
        (Lpath.Abs.to_string path)

let equal a b =
  match (a, b) with
  | Unavailable a, Unavailable b -> String.equal a b
  | Cwd_outside_scope a, Cwd_outside_scope b -> Lpath.Abs.equal a b
  | Escalation_denied, Escalation_denied -> true
  | Escalation_irrelevant, Escalation_irrelevant -> true
  | Empty_program, Empty_program -> true
  | Nul_in_argv a, Nul_in_argv b -> Int.equal a.index b.index
  | Stale_policy a, Stale_policy b -> Lpath.Abs.equal a.path b.path
  | ( ( Unavailable _ | Cwd_outside_scope _ | Escalation_denied
      | Escalation_irrelevant | Empty_program | Nul_in_argv _ | Stale_policy _
        ),
      _ ) ->
      false

let pp ppf t = Format.pp_print_string ppf (message t)

let to_json t =
  let mem name value = Jsont.Json.mem (Jsont.Json.name name) value in
  let string_mem name value = mem name (Jsont.Json.string value) in
  let structured =
    match t with
    | Cwd_outside_scope path | Stale_policy { path } ->
        [ string_mem "path" (Lpath.Abs.to_string path) ]
    | Nul_in_argv { index } -> [ mem "index" (Jsont.Json.int index) ]
    | Unavailable _ | Escalation_denied | Escalation_irrelevant | Empty_program
      ->
        []
  in
  Jsont.Json.object'
    ([ string_mem "kind" (kind_string t); string_mem "message" (message t) ]
    @ structured)
