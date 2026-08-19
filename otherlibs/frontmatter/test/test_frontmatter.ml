(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
module F = Frontmatter

(* Frontmatter is the pure YAML-header parser shared with the build-time
   builtin-skill validator; the promptgen goldens are its other regression net,
   and these assertions pin the grammar directly. *)

let parse_ok doc =
  match F.parse doc with
  | Ok t -> t
  | Error e -> failf "parse failed: %s" (F.Error.message e)

let parse_err doc =
  match F.parse doc with Ok _ -> failf "expected a parse error" | Error e -> e

let no_fence () =
  let t = parse_ok "hello\nworld" in
  equal string ~msg:"a document without a fence is all body" "hello\nworld"
    (F.body t);
  equal (list string) ~msg:"and carries no fields" [] (F.keys t)

let empty_fence () =
  let t = parse_ok "---\n---\nbody" in
  equal (list string) ~msg:"an empty header has no keys" [] (F.keys t);
  equal string ~msg:"the body follows the closing fence" "body" (F.body t)

let mapping () =
  let t = parse_ok "---\nname: hello\ndesc: world\n---\nbody\n" in
  equal (list string) ~msg:"keys are in document order" [ "name"; "desc" ]
    (F.keys t);
  equal (option string) ~msg:"string reads a plain scalar" (Some "hello")
    (F.string "name" t);
  equal (option string) ~msg:"and another" (Some "world") (F.string "desc" t);
  equal (option string) ~msg:"an absent key is None" None (F.string "gone" t);
  equal string ~msg:"the body is the exact suffix" "body\n" (F.body t)

let non_string_values () =
  let t = parse_ok "---\ncount: 5\nflag: true\nempty: null\n---\n" in
  equal (list string) ~msg:"non-string keys stay visible"
    [ "count"; "flag"; "empty" ]
    (F.keys t);
  equal (option string) ~msg:"a number is not a string" None
    (F.string "count" t);
  equal (option string) ~msg:"a boolean is not a string" None
    (F.string "flag" t);
  equal (option string) ~msg:"a null is not a string" None (F.string "empty" t)

let field_pp ppf = function
  | `Absent -> Format.pp_print_string ppf "Absent"
  | `Not_string -> Format.pp_print_string ppf "Not_string"
  | `String s -> Format.fprintf ppf "String %S" s

let field_value = Testable.make ~pp:field_pp ~equal:( = )

let field_accessor () =
  let t = parse_ok "---\nname: hello\ncount: 5\nitems:\n  - a\n  - b\n---\n" in
  equal field_value ~msg:"a string value is the String arm" (`String "hello")
    (F.field "name" t);
  equal field_value ~msg:"a number is Not_string" `Not_string
    (F.field "count" t);
  equal field_value ~msg:"a list is Not_string" `Not_string (F.field "items" t);
  equal field_value ~msg:"an absent key is Absent" `Absent (F.field "gone" t)

let crlf_fences () =
  let t = parse_ok "---\r\nname: value\n---\r\nbody" in
  equal (option string) ~msg:"a carriage return on a fence line is accepted"
    (Some "value") (F.string "name" t);
  equal string ~msg:"the body is unchanged" "body" (F.body t)

let body_is_verbatim () =
  let t = parse_ok "---\nk: v\n---\n  spaced body\n" in
  equal string ~msg:"the body is never trimmed or normalized" "  spaced body\n"
    (F.body t)

let duplicate_keys () =
  let t = parse_ok "---\nk: first\nk: second\n---\n" in
  equal (list string) ~msg:"duplicate keys are preserved" [ "k"; "k" ]
    (F.keys t);
  equal (option string) ~msg:"string reads the first occurrence" (Some "first")
    (F.string "k" t)

let indented_dashes_are_not_a_fence () =
  let t = parse_ok " ---\nbody" in
  equal (list string) ~msg:"a leading space breaks the opening fence" []
    (F.keys t);
  equal string ~msg:"so the whole document is body" " ---\nbody" (F.body t)

let unterminated () =
  match parse_err "---\nname: x\n" with
  | F.Error.Unterminated -> ()
  | e -> failf "expected Unterminated, got %s" (F.Error.message e)

let invalid_yaml () =
  match parse_err "---\nkey: [unclosed\n---\n" with
  | F.Error.Invalid_yaml _ -> ()
  | e -> failf "expected Invalid_yaml, got %s" (F.Error.message e)

let unquoted_colon_values () =
  let t = parse_ok "---\ndesc: Build for AWS: ECS\nother: plain\n---\nbody" in
  equal (option string) ~msg:"an unquoted colon value reads as the full string"
    (Some "Build for AWS: ECS") (F.string "desc" t);
  equal (option string) ~msg:"untouched lines keep their values" (Some "plain")
    (F.string "other" t);
  let t = parse_ok "---\ndesc: don't stop: now\n---\n" in
  equal (option string) ~msg:"a single quote in the value survives repair"
    (Some "don't stop: now") (F.string "desc" t);
  match parse_err "---\ndesc: [broken: still\n---\n" with
  | F.Error.Invalid_yaml _ -> ()
  | e -> failf "expected the original Invalid_yaml, got %s" (F.Error.message e)

let not_a_mapping () =
  (match parse_err "---\n- a\n- b\n---\n" with
  | F.Error.Not_a_mapping -> ()
  | e -> failf "expected Not_a_mapping for a list, got %s" (F.Error.message e));
  match parse_err "---\njust a scalar\n---\n" with
  | F.Error.Not_a_mapping -> ()
  | e -> failf "expected Not_a_mapping for a scalar, got %s" (F.Error.message e)

let suite =
  [
    test "a document without a fence is all body" no_fence;
    test "an empty header parses to no fields" empty_fence;
    test "a mapping header yields ordered string fields" mapping;
    test "non-string values are visible but read as None" non_string_values;
    test "field classifies absent, string, and non-string values" field_accessor;
    test "carriage returns on fence lines are accepted" crlf_fences;
    test "the body is the verbatim byte suffix" body_is_verbatim;
    test "duplicate keys are preserved and read first" duplicate_keys;
    test "indented dashes do not open a fence" indented_dashes_are_not_a_fence;
    test "an unterminated fence errors" unterminated;
    test "invalid YAML errors" invalid_yaml;
    test "unquoted colon values are repaired to strings" unquoted_colon_values;
    test "a non-mapping header errors" not_a_mapping;
  ]

let () = run "frontmatter" suite
