(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
open Mentat_permission
module Json = Jsont.Json
module Workspace = Mentat_workspace

(* The suite combines focused examples with generated laws for ordered policy
   evaluation, exact grants, grouped review, fail-closed codecs, canonical
   identity, and answer invariants. *)

(* JSON helpers. [Json.equal] sorts object members before comparing (jsont's
   Json.compare), so hand-built golden objects are member-order-insensitive. *)

let obj members =
  Json.object'
    (List.map (fun (name, value) -> Json.mem (Json.name name) value) members)

let workspace_path_object root rel =
  obj [ ("root", Json.string root); ("rel", Json.string rel) ]

let json_encode codec value =
  match Json.encode codec value with
  | Ok json -> json
  | Error message -> failf "encode failed: %s" message

let json_decode codec json =
  match Json.decode codec json with
  | Ok value -> value
  | Error message -> failf "decode failed: %s" message

let roundtrip msg testable codec value =
  equal testable ~msg value (json_decode codec (json_encode codec value))

let raises_invalid msg fn =
  raises_match ~msg (function Invalid_argument _ -> true | _ -> false) fn

(* Path / workspace helpers. *)

let local path =
  let parsed =
    if String.equal path "" then Ok Lpath.Rel.root else Lpath.Rel.of_string path
  in
  match parsed with
  | Ok path -> path
  | Error error -> failf "invalid local path: %s" (Lpath.Error.message error)

let abs path =
  match Lpath.Abs.of_string path with
  | Ok path -> path
  | Error error -> failf "invalid absolute path: %s" (Lpath.Error.message error)

let workspace_root_key key =
  match Workspace.Root.Key.of_string key with
  | Ok key -> key
  | Error error ->
      failf "invalid workspace root key: %s" (Workspace.Root.Key.message error)

let workspace_path ?root_key ?(root = "/repo") relative =
  let root =
    match root_key with
    | None -> Workspace.Root.of_dir (abs root)
    | Some key -> Workspace.Root.make ~key:(workspace_root_key key) (abs root)
  in
  Workspace.Path.make ~root_key:(Workspace.Root.key root) (local relative)

let path_with_key ?(root_key = "/repo") relative =
  Workspace.Path.make ~root_key:(workspace_root_key root_key) (local relative)

let workspace_scope_key ?(root_key = "/repo") relative =
  Access.Path_scope.workspace (path_with_key ~root_key relative)

let workspace_read relative = Access.path ~op:`Read (workspace_path relative)

let workspace_modify relative =
  Access.path ~op:`Modify (workspace_path relative)

let outside_path op path = Access.outside_workspace_path ~op (abs path)
let unknown_read path = Access.unknown_path ~op:`Read path
let unknown_path op path = Access.unknown_path ~op path
let default_cwd = Access.Path_scope.workspace (workspace_path "")

let enforced ?(read = Access.Command.Confinement.Project)
    ?(write = Access.Command.Confinement.Read_only)
    ?(network = Access.Command.Confinement.Restricted) () =
  Access.Command.Enforced Access.Command.Confinement.{ read; write; network }

let default_cwd_match = Policy.Match.Path.exact (path_with_key "")

let shell ?(cwd = default_cwd) ?(execution = Access.Command.Direct) command =
  Access.command (Access.Command.shell ~cwd ~execution command)

let argv ?(cwd = default_cwd) ?(execution = Access.Command.Direct) ~program args
    =
  Access.command (Access.Command.argv ~cwd ~execution ~program args)

let exec program args = argv ~program args

let code ?(cwd = default_cwd) ?(execution = Access.Command.Direct) ~language
    source =
  Access.code ~cwd ~execution ~language source

let enforced_exec program args =
  Access.argv ~cwd:default_cwd ~execution:(enforced ()) ~program args

let argv_prefix ?(execution = Access.Command.Direct) ?(cwd = default_cwd_match)
    ~program ~args () =
  Policy.Match.command
    (Policy.Match.Command.argv_prefix ~execution ~cwd ~program ~args ())

let argv_exact ?(cwd = default_cwd) ?(execution = Access.Command.Direct)
    ~program ~args () =
  Policy.Match.command
    (Policy.Match.Command.exact
       (Access.Command.argv ~cwd ~execution ~program args))

let extension name = Access.custom name
let request access = Request.of_accesses [ access ]

let restore_review request accesses =
  let reasons =
    List.map (fun access -> (access, Policy.Review.Unmatched)) accesses
  in
  match Policy.Review.restore request reasons with
  | Ok review -> review
  | Error Policy.Review.Empty_accesses ->
      failf "review restore unexpectedly empty"
  | Error (Policy.Review.Duplicate_access access) ->
      failf "review restore found duplicate access: %a" Access.pp access
  | Error (Policy.Review.Access_not_in_request access) ->
      failf "review restore rejected access: %a" Access.pp access

let remember_exact = Policy.Review.remember

let expect_allow msg = function
  | Policy.Decision.Allowed -> ()
  | Policy.Decision.Review _ -> failf "%s: expected allow, got review" msg
  | Policy.Decision.Denied _ -> failf "%s: expected allow, got deny" msg

let expect_review msg = function
  | Policy.Decision.Review review -> review
  | Policy.Decision.Allowed -> failf "%s: expected review, got allow" msg
  | Policy.Decision.Denied _ -> failf "%s: expected review, got deny" msg

let expect_deny msg = function
  | Policy.Decision.Denied _ -> ()
  | Policy.Decision.Allowed -> failf "%s: expected deny, got allow" msg
  | Policy.Decision.Review _ -> failf "%s: expected deny, got review" msg

let expect_denial msg = function
  | Policy.Decision.Denied (denial, _) -> denial
  | Policy.Decision.Allowed -> failf "%s: expected deny, got allow" msg
  | Policy.Decision.Review _ -> failf "%s: expected deny, got review" msg

let expect_denials msg = function
  | Policy.Decision.Denied (denial, denials) -> denial :: denials
  | Policy.Decision.Allowed -> failf "%s: expected deny, got allow" msg
  | Policy.Decision.Review _ -> failf "%s: expected deny, got review" msg

let decision_tag = function
  | Policy.Decision.Allowed -> `Allowed
  | Policy.Decision.Review _ -> `Review
  | Policy.Decision.Denied _ -> `Denied

(* Generators (small, weighting edge shapes — empty lists where legal,
   deep paths, case-varied strings, NUL bytes). Path components reject NUL/dot/
   slash at construction, so NUL lives in the free-string and root-key bytes
   (both accept any non-empty byte string). *)

let component_char =
  Gen.frequency
    [
      (5, Gen.char_range 'a' 'z');
      (2, Gen.char_range 'A' 'Z');
      (2, Gen.of_list [ '0'; '1'; '-'; '_' ]);
    ]

let component_gen = Gen.string_of ~size:(Gen.int_range 1 5) component_char
let components_gen lo hi = Gen.list ~size:(Gen.int_range lo hi) component_gen

let rel_of_components = function
  | [] -> Lpath.Rel.root
  | cs -> local (String.concat "/" cs)

let abs_gen =
  Gen.map (fun cs -> abs ("/" ^ String.concat "/" cs)) (components_gen 1 3)

(* Root keys are non-empty valid UTF-8 without control bytes. Mixed case and
   punctuation still exercise byte-lexical identity. *)
let root_key_char =
  Gen.frequency
    [
      (5, Gen.char_range 'a' 'z');
      (2, Gen.char_range 'A' 'Z');
      (2, Gen.char_range '0' '9');
      (1, Gen.of_list [ '-'; '_'; '.' ]);
    ]

let root_key_gen =
  Gen.map Workspace.Root.Key.of_string_exn
    (Gen.string_of ~size:(Gen.int_range 1 6) root_key_char)

(* Free (opaque) strings for command text, hosts, custom names/subjects, and
   custom protocol names: non-empty, with NUL, shell-ish bytes, and high bytes. *)
let free_char =
  Gen.frequency
    [
      (5, Gen.char_range 'a' 'z');
      (2, Gen.char_range 'A' 'Z');
      (* [':'] is the stable_text field separator: including it stresses the
         length-framing that keeps stable_text injective. *)
      (2, Gen.of_list [ ' '; '-'; '/'; '.'; '$'; '|'; '&'; ':' ]);
      (1, Gen.pure '\000');
      (1, Gen.map Char.chr (Gen.int_range 128 255));
    ]

let free_str_gen = Gen.string_of ~size:(Gen.int_range 1 6) free_char
let path_op_gen = Gen.of_list [ `Read; `Create; `Modify; `Delete ]
let kind_gen = Gen.of_list [ `Read; `Write; `Command; `Network; `Custom ]

let protocol_gen =
  Gen.frequency
    [
      (5, Gen.of_list [ `Http; `Https; `Ssh; `Tcp; `Udp ]);
      (2, Gen.map (fun name -> `Other name) free_str_gen);
    ]

let confinement_gen =
  Gen.bind
    (Gen.of_list Access.Command.Confinement.[ Project; All ])
    (fun read ->
      Gen.bind
        (Gen.of_list Access.Command.Confinement.[ Read_only; Workspace ])
        (fun write ->
          Gen.map
            (fun network -> Access.Command.Confinement.{ read; write; network })
            (Gen.of_list Access.Command.Confinement.[ Restricted; Enabled ])))

let execution_gen =
  Gen.frequency
    [
      (2, Gen.pure Access.Command.Direct);
      (1, Gen.pure Access.Command.External);
      (2, Gen.map (fun c -> Access.Command.Enforced c) confinement_gen);
    ]

let scope_gen =
  Gen.frequency
    [
      ( 3,
        Gen.bind root_key_gen (fun root_key ->
            Gen.map
              (fun cs ->
                Access.Path_scope.workspace
                  (Workspace.Path.make ~root_key (rel_of_components cs)))
              (components_gen 0 4)) );
      (2, Gen.map (fun a -> Access.Path_scope.outside_workspace a) abs_gen);
      (2, Gen.map (fun s -> Access.Path_scope.unknown s) free_str_gen);
    ]

let command_gen =
  Gen.frequency
    [
      ( 2,
        Gen.bind scope_gen (fun cwd ->
            Gen.bind execution_gen (fun execution ->
                Gen.map
                  (fun text -> Access.Command.shell ~cwd ~execution text)
                  free_str_gen)) );
      ( 2,
        Gen.bind scope_gen (fun cwd ->
            Gen.bind execution_gen (fun execution ->
                Gen.bind free_str_gen (fun program ->
                    Gen.map
                      (fun args ->
                        Access.Command.argv ~cwd ~execution ~program args)
                      (Gen.list ~size:(Gen.int_range 0 3) free_str_gen)))) );
      ( 1,
        Gen.bind scope_gen (fun cwd ->
            Gen.bind execution_gen (fun execution ->
                Gen.bind free_str_gen (fun language ->
                    Gen.map
                      (fun source ->
                        Access.Command.code ~cwd ~execution ~language source)
                      free_str_gen))) );
    ]

let access_gen =
  Gen.frequency
    [
      ( 3,
        Gen.bind path_op_gen (fun op ->
            Gen.map (fun scope -> Access.path_scope ~op scope) scope_gen) );
      (3, Gen.map (fun command -> Access.command command) command_gen);
      ( 2,
        Gen.bind protocol_gen (fun protocol ->
            Gen.bind free_str_gen (fun host ->
                Gen.map
                  (fun port -> Access.network ~protocol ?port ~host ())
                  (Gen.option (Gen.int_range 1 65_535)))) );
      ( 2,
        Gen.bind free_str_gen (fun name ->
            Gen.map
              (fun subject -> Access.custom ?subject name)
              (Gen.option free_str_gen)) );
    ]

let path_match_gen =
  Gen.frequency
    [
      ( 2,
        Gen.bind root_key_gen (fun root_key ->
            Gen.map
              (fun cs ->
                Policy.Match.Path.exact
                  (Workspace.Path.make ~root_key (rel_of_components cs)))
              (components_gen 0 4)) );
      ( 2,
        Gen.bind root_key_gen (fun root_key ->
            Gen.map
              (fun cs ->
                Policy.Match.Path.under
                  (Workspace.Path.make ~root_key (rel_of_components cs)))
              (components_gen 0 4)) );
      ( 1,
        Gen.map
          (fun cs -> Policy.Match.Path.exact_relative (rel_of_components cs))
          (components_gen 1 3) );
      ( 1,
        Gen.map
          (fun cs -> Policy.Match.Path.under_relative (rel_of_components cs))
          (components_gen 1 3) );
      (1, Gen.pure Policy.Match.Path.workspace);
      (1, Gen.pure Policy.Match.Path.outside_workspace);
      (1, Gen.pure Policy.Match.Path.unknown);
    ]

let command_match_gen =
  Gen.frequency
    [
      (1, Gen.pure Policy.Match.Command.any);
      (1, Gen.pure Policy.Match.Command.high_impact);
      (1, Gen.map Policy.Match.Command.execution execution_gen);
      ( 1,
        Gen.map (fun command -> Policy.Match.Command.exact command) command_gen
      );
      ( 2,
        Gen.bind execution_gen (fun execution ->
            Gen.bind path_match_gen (fun cwd ->
                Gen.bind free_str_gen (fun program ->
                    Gen.map
                      (fun args ->
                        Policy.Match.Command.argv_prefix ~execution ~cwd
                          ~program ~args ())
                      (Gen.list ~size:(Gen.int_range 0 3) free_str_gen)))) );
    ]

let matcher_gen =
  Gen.frequency
    [
      (1, Gen.pure Policy.Match.any);
      (1, Gen.map Policy.Match.kind kind_gen);
      (1, Gen.map Policy.Match.exact access_gen);
      ( 2,
        Gen.bind (Gen.option path_op_gen) (fun op ->
            Gen.map (fun scope -> Policy.Match.path ?op scope) path_match_gen)
      );
      (2, Gen.map Policy.Match.command command_match_gen);
      ( 1,
        Gen.bind (Gen.option protocol_gen) (fun protocol ->
            Gen.bind free_str_gen (fun host ->
                Gen.map
                  (fun port ->
                    Policy.Match.network_host ?protocol ?port ~host ())
                  (Gen.option (Gen.int_range 1 65_535)))) );
      ( 1,
        Gen.bind free_str_gen (fun name ->
            Gen.map
              (fun subject -> Policy.Match.custom ?subject name)
              (Gen.option free_str_gen)) );
    ]

let action_gen = Gen.of_list Policy.Rule.[ Allow; Review; Deny ]

let rule_gen =
  Gen.bind action_gen (fun action ->
      Gen.map (fun matcher -> Policy.Rule.make action matcher) matcher_gen)

let policy_gen =
  Gen.map Policy.make (Gen.list ~size:(Gen.int_range 0 4) rule_gen)

let change_gen =
  Gen.frequency
    [
      (1, Gen.map (fun diff -> Request.Change.make ~diff ()) free_str_gen);
      (1, Gen.map (fun additions -> Request.Change.make ~additions ()) Gen.nat);
      ( 1,
        Gen.bind Gen.nat (fun additions ->
            Gen.map
              (fun removals -> Request.Change.make ~additions ~removals ())
              Gen.nat) );
    ]

let item_gen =
  Gen.bind access_gen (fun access ->
      Gen.bind (Gen.option free_str_gen) (fun display ->
          Gen.map
            (fun change -> Request.Item.make ?display ?change access)
            (Gen.option change_gen)))

let request_gen =
  Gen.bind
    (Gen.list ~size:(Gen.int_range 1 3) item_gen)
    (fun items ->
      Gen.bind (Gen.option free_str_gen) (fun source ->
          Gen.map
            (fun display -> Request.make ?source ?display items)
            (Gen.option free_str_gen)))

let lifetime_gen = Gen.of_list Answer.[ Conversation; User ]

let family_gen =
  Gen.bind lifetime_gen (fun lifetime ->
      Gen.map
        (fun names ->
          let names = List.sort_uniq String.compare names in
          let names = if names = [] then [ "a" ] else names in
          let rules =
            List.map (fun n -> Policy.Rule.allow (Policy.Match.custom n)) names
          in
          Answer.family ~lifetime ~rules)
        (Gen.list ~size:(Gen.int_range 1 3) free_str_gen))

let answer_gen =
  Gen.frequency
    [
      (1, Gen.pure Answer.once);
      (1, Gen.pure Answer.exact_for_conversation);
      (1, Gen.pure Answer.deny);
      (2, family_gen);
    ]

(* Testables. *)

let access_value = Testable.make ~pp:Access.pp ~equal:Access.equal
let request_value = Testable.make ~pp:Request.pp ~equal:Request.equal
let item_value = Testable.make ~pp:Request.Item.pp ~equal:Request.Item.equal

let change_value =
  Testable.make ~pp:Request.Change.pp ~equal:Request.Change.equal

let rule_value = Testable.make ~pp:Policy.Rule.pp ~equal:Policy.Rule.equal
let policy_value = Testable.make ~pp:Policy.pp ~equal:Policy.equal
let answer_value = Testable.make ~pp:Answer.pp ~equal:Answer.equal

(* Property inputs: the generators above with counterexample printers. *)
let access_gen = Gen.with_pp Access.pp access_gen
let request_gen = Gen.with_pp Request.pp request_gen
let rule_gen = Gen.with_pp Policy.Rule.pp rule_gen
let policy_gen = Gen.with_pp Policy.pp policy_gen
let answer_gen = Gen.with_pp Answer.pp answer_gen

(* Access facts: construction and validation *)

let access_constructor_validation () =
  ignore (Access.path ~op:`Read (workspace_path ~root:"/r" ""));
  raises_invalid "unknown path cannot be empty" (fun () ->
      ignore (Access.unknown_path ~op:`Read ""));
  raises_invalid "shell command cannot be empty" (fun () -> ignore (shell ""));
  raises_invalid "exec program cannot be empty" (fun () ->
      ignore (argv ~program:"" []));
  ignore (argv ~program:"printf" [ "" ]);
  raises_invalid "code language cannot be empty" (fun () ->
      ignore (code ~language:"" "1 + 1"));
  raises_invalid "code source cannot be empty" (fun () ->
      ignore (code ~language:"ocaml" ""));
  raises_invalid "unknown cwd cannot be empty" (fun () ->
      ignore (Access.Path_scope.unknown ""));
  (match Workspace.Root.Key.of_string "" with
  | Error Workspace.Root.Key.Empty -> ()
  | Error
      (Workspace.Root.Key.Invalid_utf_8 | Workspace.Root.Key.Control_character _)
    ->
      failf "empty workspace root key reported the wrong error"
  | Ok _ -> failf "empty workspace root key should be rejected");
  raises_invalid "network host cannot be empty" (fun () ->
      ignore (Access.network ~protocol:`Https ~host:"" ()));
  raises_invalid "custom protocol cannot be empty" (fun () ->
      ignore (Access.network ~protocol:(`Other "") ~host:"socket" ()));
  raises_invalid "network port must be in range" (fun () ->
      ignore (Access.network ~protocol:`Https ~port:0 ~host:"example.com" ()));
  raises_invalid "extension name cannot be empty" (fun () ->
      ignore (Access.custom ""));
  raises_invalid "extension subject cannot be empty" (fun () ->
      ignore (Access.custom ~subject:"" "capability"))

let validation_errors_name_the_function_and_constraint () =
  raises ~msg:"shell command error is actionable"
    (Invalid_argument
       "Mentat_permission.Access.Command.shell: text must not be empty")
    (fun () -> ignore (shell ""));
  raises ~msg:"request access error is actionable"
    (Invalid_argument
       "Mentat_permission.Request.of_accesses: accesses must not be empty")
    (fun () -> ignore (Request.of_accesses []))

let shell_and_exec_have_distinct_keys () =
  let shell = shell "git status" in
  let exec = argv ~program:"git" [ "status" ] in
  is_true ~msg:"shell is command class" (Access.kind shell = `Command);
  is_true ~msg:"exec is command class" (Access.kind exec = `Command);
  not_equal access_value ~msg:"shell and exec do not match exactly" shell exec

let code_identity_includes_source_language_cwd_and_route () =
  let ocaml = code ~language:"ocaml" "1 + 1" in
  let other_source = code ~language:"ocaml" "2 + 2" in
  let other_language = code ~language:"reason" "1 + 1" in
  let other_cwd =
    code
      ~cwd:(Access.Path_scope.workspace (workspace_path "lib"))
      ~language:"ocaml" "1 + 1"
  in
  let other_route = code ~execution:(enforced ()) ~language:"ocaml" "1 + 1" in
  is_true ~msg:"code is command class" (Access.kind ocaml = `Command);
  List.iter
    (fun distinct ->
      not_equal access_value ~msg:"code identity preserves every execution fact"
        ocaml distinct)
    [ other_source; other_language; other_cwd; other_route ]

let workspace_access_is_stable_identity () =
  let first = Access.path_scope ~op:`Modify (workspace_scope_key "lib/a.ml") in
  let second = Access.path_scope ~op:`Modify (workspace_scope_key "lib/a.ml") in
  equal access_value ~msg:"same workspace identity is structurally equal" first
    second;
  equal int ~msg:"comparison uses access identity" 0
    (Access.compare first second);
  let other_root =
    Access.path_scope ~op:`Modify
      (workspace_scope_key ~root_key:"/other" "lib/a.ml")
  in
  not_equal access_value ~msg:"workspace root key affects exact matching" first
    other_root;
  is_true ~msg:"compare distinguishes exactly what equal distinguishes"
    (Access.compare first other_root <> 0)

let workspace_root_key_is_used_by_live_paths () =
  let access =
    Access.path ~op:`Modify
      (workspace_path ~root:"/tmp/checkout" ~root_key:"repo-stable" "lib/a.ml")
  in
  let same_key =
    Access.path_scope ~op:`Modify
      (workspace_scope_key ~root_key:"repo-stable" "lib/a.ml")
  in
  let path_rule =
    Policy.Match.path
      (Policy.Match.Path.under
         (workspace_path ~root:"/tmp/checkout" ~root_key:"repo-stable" "lib"))
  in
  equal access_value ~msg:"live path access uses Root.key" same_key access;
  expect_allow "path-under uses Root.key"
    (Policy.decide
       (Policy.make [ Policy.Rule.allow path_rule ])
       (request access))

let access_facts =
  group "access facts"
    [
      test "constructors validate trusted claims" access_constructor_validation;
      test "validation errors name the function and constraint"
        validation_errors_name_the_function_and_constraint;
      test "shell and exec have distinct keys" shell_and_exec_have_distinct_keys;
      test "code identity includes source language cwd and route"
        code_identity_includes_source_language_cwd_and_route;
      test "workspace access is stable identity"
        workspace_access_is_stable_identity;
      test "workspace root key is used by live paths"
        workspace_root_key_is_used_by_live_paths;
    ]

(* Requests *)

let request_constructor_validation () =
  raises_invalid "requests need at least one access" (fun () ->
      ignore (Request.of_accesses []));
  raises_invalid "request source cannot be empty" (fun () ->
      ignore (Request.of_accesses ~source:"" [ extension "confirm" ]));
  let duplicate = workspace_read "README.md" in
  let same_key =
    Access.path_scope ~op:`Read (workspace_scope_key "README.md")
  in
  let r = Request.of_accesses ~source:"tool" [ extension "confirm" ] in
  equal (option string) ~msg:"request source is retained" (Some "tool")
    (Request.source r);
  equal (list access_value) ~msg:"request accesses are retained"
    [ extension "confirm" ]
    (Request.accesses r);
  let with_duplicates =
    Request.of_accesses [ duplicate; same_key; duplicate ]
  in
  equal (list access_value) ~msg:"normalized accesses remove exact duplicates"
    [ duplicate ]
    (Request.normalized_accesses with_duplicates);
  equal int ~msg:"unique accesses collapse duplicate identities" 1
    (Access.Set.cardinal (Request.unique_accesses with_duplicates))

let change_validation_and_lookup () =
  raises ~msg:"change diff cannot be empty"
    (Invalid_argument
       "Mentat_permission.Request.Change.make: diff must not be empty")
    (fun () -> Request.Change.make ~diff:"" ());
  raises_invalid "change additions must be non-negative" (fun () ->
      ignore (Request.Change.make ~additions:(-1) ()));
  raises_invalid "change removals must be non-negative" (fun () ->
      ignore (Request.Change.make ~removals:(-1) ()));
  raises ~msg:"change needs at least one field"
    (Invalid_argument
       "Mentat_permission.Request.Change.make: change must contain at least \
        one field") (fun () -> Request.Change.make ());
  let change = Request.Change.make ~diff:"+x" ~additions:1 ~removals:0 () in
  let later_change =
    Request.Change.make ~diff:"-y\n+z" ~additions:1 ~removals:1 ()
  in
  let target = workspace_modify "lib/a.ml" in
  let other = workspace_read "README.md" in
  let same_key_display =
    Access.path_scope ~op:`Modify (workspace_scope_key "lib/a.ml")
  in
  let target_item = Request.Item.make ~change target in
  let other_item = Request.Item.make other in
  let later_item = Request.Item.make ~change:later_change same_key_display in
  let r = Request.make [ target_item; other_item; later_item ] in
  equal (list item_value) ~msg:"items are found by stable access identity"
    [ target_item; later_item ]
    (Request.items_for_access r target);
  equal (list change_value) ~msg:"all changes are found by access identity"
    [ change; later_change ]
    (Request.changes_for_access r same_key_display);
  equal (list change_value) ~msg:"other accesses have no changes" []
    (Request.changes_for_access r other);
  let review = restore_review r [ target ] in
  equal (list access_value) ~msg:"review normalizes duplicate access identity"
    [ target ]
    (Policy.Review.accesses review);
  is_true ~msg:"review access set contains the reviewed access"
    (Access.Set.equal
       (Access.Set.singleton target)
       (Policy.Review.access_set review));
  equal (list item_value) ~msg:"review items preserve duplicate occurrences"
    [ target_item; later_item ]
    (Policy.Review.items review);
  equal (list change_value) ~msg:"review changes preserve duplicate evidence"
    [ change; later_change ]
    (Policy.Review.changes review)

let change_is_inert_and_durable () =
  let target = workspace_modify "lib/a.ml" in
  let change = Request.Change.make ~diff:"-a\n+b" ~additions:1 ~removals:1 () in
  let bare = Request.of_accesses ~source:"edit_file" [ target ] in
  let with_change =
    Request.make ~source:"edit_file" [ Request.Item.make ~change target ]
  in
  not_equal request_value ~msg:"request equality includes change metadata" bare
    with_change;
  equal (list access_value) ~msg:"change does not change unique accesses"
    (Access.Set.elements (Request.unique_accesses bare))
    (Access.Set.elements (Request.unique_accesses with_change));
  let policy = Policy.make [ Policy.Rule.deny (Policy.Match.kind `Write) ] in
  let denied_bare =
    expect_denial "bare request denied" (Policy.decide policy bare)
  in
  let denied_change =
    expect_denial "change request denied" (Policy.decide policy with_change)
  in
  equal rule_value ~msg:"change does not change policy decisions"
    (Policy.Denial.rule denied_bare)
    (Policy.Denial.rule denied_change);
  roundtrip "request with change roundtrips" request_value Request.jsont
    with_change;
  roundtrip "request without change roundtrips" request_value Request.jsont bare

let requests =
  group "requests"
    [
      test "constructors validate grouped facts" request_constructor_validation;
      test "change validation and lookup" change_validation_and_lookup;
      test "change is inert and durable" change_is_inert_and_durable;
    ]

(* Evaluation *)

let default_policy_reviews_everything () =
  let read = workspace_read "README.md" in
  let review =
    expect_review "review-all policy asks about reads"
      (Policy.decide Policy.default (request read))
  in
  equal (list access_value) ~msg:"review contains the unmatched access" [ read ]
    (Policy.Review.accesses review);
  let command = shell "make" in
  let review =
    expect_review "review-all policy asks about commands"
      (Policy.decide Policy.default (request command))
  in
  equal (list access_value) ~msg:"command review contains the unmatched access"
    [ command ]
    (Policy.Review.accesses review)

let all_access_rules_are_explicit () =
  let command = shell "make test" in
  expect_allow "dangerous allow-all rule allows commands"
    (Policy.decide
       (Policy.make [ Policy.Rule.allow_all_dangerously ])
       (request command));
  expect_deny "deny-all rule denies commands"
    (Policy.decide (Policy.make [ Policy.Rule.deny_all ]) (request command));
  let review =
    expect_review "review-all policy reviews unmatched commands"
      (Policy.decide Policy.default (request command))
  in
  let grants = remember_exact review Policy.Grants.empty in
  expect_allow "review-all policy still respects grants"
    (Policy.decide ~grants Policy.default (request command));
  ignore
    (expect_review "review-all rule overrides grants"
       (Policy.decide ~grants
          (Policy.make [ Policy.Rule.always_review ])
          (request command)))

let class_read_rule_allows_reads () =
  let policy = Policy.make [ Policy.Rule.allow (Policy.Match.kind `Read) ] in
  expect_allow "read class rule allows path reads"
    (Policy.decide policy (request (unknown_read "README.md")));
  ignore
    (expect_review "read class rule does not allow edits"
       (Policy.decide policy (request (workspace_modify "lib/a.ml"))))

let command_deny_rule_denies_shell_and_exec () =
  let policy = Policy.make [ Policy.Rule.deny (Policy.Match.kind `Command) ] in
  expect_deny "command class deny rejects shell"
    (Policy.decide policy (request (shell "make")));
  expect_deny "command class deny rejects exec"
    (Policy.decide policy (request (exec "make" [])))

let first_matching_rule_wins () =
  let command = request (shell "make") in
  let deny_first =
    Policy.make
      [
        Policy.Rule.deny (Policy.Match.kind `Command);
        Policy.Rule.allow (Policy.Match.kind `Command);
      ]
  in
  expect_deny "earlier deny is not overridden by later allow"
    (Policy.decide deny_first command);
  let allow_first =
    Policy.make
      [
        Policy.Rule.allow (Policy.Match.kind `Command);
        Policy.Rule.deny (Policy.Match.kind `Command);
      ]
  in
  expect_allow "earlier allow is not overridden by later deny"
    (Policy.decide allow_first command)

let grouped_request_semantics () =
  let read = workspace_read "README.md" in
  let edit = workspace_modify "lib/a.ml" in
  let command = shell "make" in
  let policy =
    Policy.make
      [
        Policy.Rule.allow (Policy.Match.kind `Read);
        Policy.Rule.deny (Policy.Match.kind `Command);
      ]
  in
  expect_deny "deny wins over review in grouped requests"
    (Policy.decide policy (Request.of_accesses [ read; edit; command ]));
  let review =
    expect_review "review contains only accesses needing review"
      (Policy.decide policy (Request.of_accesses [ read; edit ]))
  in
  equal (list access_value) ~msg:"only the edit needs review" [ edit ]
    (Policy.Review.accesses review);
  let review =
    expect_review "review deduplicates exact duplicate accesses"
      (Policy.decide policy (Request.of_accesses [ read; edit; edit ]))
  in
  equal (list access_value) ~msg:"duplicate edit appears once" [ edit ]
    (Policy.Review.accesses review);
  let allow_all =
    Policy.make
      [
        Policy.Rule.allow (Policy.Match.kind `Read);
        Policy.Rule.allow (Policy.Match.kind `Write);
      ]
  in
  expect_allow "grouped request allows only when every access is allowed"
    (Policy.decide allow_all (Request.of_accesses [ read; edit ]));
  ignore
    (expect_review "one unallowed access makes the group need review"
       (Policy.decide
          (Policy.make [ Policy.Rule.allow (Policy.Match.kind `Read) ])
          (Request.of_accesses [ read; edit ])))

let deny_decisions_are_inspectable () =
  let read = workspace_read "README.md" in
  let edit = workspace_modify "lib/a.ml" in
  let command = shell "make test" in
  let deny_command = Policy.Rule.deny (Policy.Match.kind `Command) in
  let deny_edit = Policy.Rule.deny (Policy.Match.kind `Write) in
  let policy = Policy.make [ deny_edit; deny_command ] in
  let request = Request.of_accesses [ read; edit; command ] in
  let denials =
    expect_denials "deny decisions include denied accesses"
      (Policy.decide policy request)
  in
  equal int ~msg:"all denied accesses are reported" 2 (List.length denials);
  let denial = List.hd denials in
  equal request_value ~msg:"denial retains the original request" request
    (Policy.Denial.request denial);
  equal access_value ~msg:"denial reports the first denied access" edit
    (Policy.Denial.access denial);
  equal rule_value ~msg:"denial reports the denying rule" deny_edit
    (Policy.Denial.rule denial);
  equal (list access_value) ~msg:"denials preserve normalized request order"
    [ edit; command ]
    (List.map Policy.Denial.access denials)

(* Rule-before-grant precedence, observed through [decide]: an allow rule
   allows, a review rule reviews (retaining the deciding rule), an unmatched
   access needs review, an exact grant allows a repeated access, and a deny rule
   evaluated before grants overrides them. *)
let rule_and_grant_precedence () =
  let read = workspace_read "README.md" in
  let command = shell "make test" in
  let allow_read = Policy.Rule.allow (Policy.Match.kind `Read) in
  let review_commands = Policy.Rule.review (Policy.Match.kind `Command) in
  let deny_command = Policy.Rule.deny (Policy.Match.exact command) in
  let policy = Policy.make [ allow_read; review_commands ] in
  expect_allow "an allow rule allows a matching read"
    (Policy.decide policy (request read));
  let review =
    expect_review "a review rule reviews a matching command"
      (Policy.decide policy (request command))
  in
  (match Policy.Review.reasons review with
  | [ (_, Policy.Review.By_rule rule) ] ->
      equal rule_value ~msg:"the review reason retains the review rule"
        review_commands rule
  | reasons ->
      failf "expected one by-rule review reason, got %d" (List.length reasons));
  let default_review =
    expect_review "the default policy reviews an unmatched command"
      (Policy.decide Policy.default (request command))
  in
  (match Policy.Review.reasons default_review with
  | [ (_, Policy.Review.Unmatched) ] -> ()
  | reasons ->
      failf "expected one unmatched review reason, got %d" (List.length reasons));
  let grants = remember_exact default_review Policy.Grants.empty in
  expect_allow "an exact grant allows a repeated access"
    (Policy.decide ~grants Policy.default (request command));
  expect_deny "a deny rule is evaluated before grants and overrides them"
    (Policy.decide ~grants (Policy.make [ deny_command ]) (request command))

let reviews_capture_the_deciding_reason () =
  let command = shell "dune build" in
  let read = workspace_read "README.md" in
  let rule = Policy.Rule.review (Policy.Match.exact command) in
  let review =
    expect_review "mixed request is reviewed"
      (Policy.decide (Policy.make [ rule ])
         (Request.of_accesses [ command; read ]))
  in
  match Policy.Review.reasons review with
  | [
   (reviewed_command, Policy.Review.By_rule reviewed_rule);
   (reviewed_read, Policy.Review.Unmatched);
  ] ->
      is_true ~msg:"review-rule access is preserved"
        (Access.equal command reviewed_command);
      equal rule_value ~msg:"captured review rule is preserved" rule
        reviewed_rule;
      is_true ~msg:"unmatched access is preserved"
        (Access.equal read reviewed_read)
  | reasons ->
      failf "unexpected captured review reasons: %d entries"
        (List.length reasons)

let evaluation =
  group "evaluation"
    [
      test "default policy reviews everything" default_policy_reviews_everything;
      test "all-access rules are explicit" all_access_rules_are_explicit;
      test "read class rule allows reads" class_read_rule_allows_reads;
      test "command deny rule denies shell and exec"
        command_deny_rule_denies_shell_and_exec;
      test "first matching rule wins" first_matching_rule_wins;
      test "grouped request semantics" grouped_request_semantics;
      test "deny decisions are inspectable" deny_decisions_are_inspectable;
      test "rule and grant precedence" rule_and_grant_precedence;
      test "reviews capture the deciding reason"
        reviews_capture_the_deciding_reason;
      (* all rules over the same matcher match; the head decides. *)
      prop "first matching rule decides regardless of later same-matcher rules"
        access_gen (fun a ->
          let m = Policy.Match.exact a in
          let tag_of = function
            | Policy.Rule.Allow -> `Allowed
            | Policy.Rule.Review -> `Review
            | Policy.Rule.Deny -> `Denied
          in
          List.iter
            (fun head ->
              let rules =
                List.map
                  (fun action -> Policy.Rule.make action m)
                  [
                    head;
                    Policy.Rule.Allow;
                    Policy.Rule.Review;
                    Policy.Rule.Deny;
                  ]
              in
              is_true ~msg:"first-match decides"
                (decision_tag (Policy.decide (Policy.make rules) (request a))
                = tag_of head))
            Policy.Rule.[ Allow; Review; Deny ]);
      (* a deny reports every normalized access in normalized order. *)
      prop "deny_all denies and reports every access in normalized order"
        request_gen (fun r ->
          match Policy.decide (Policy.make [ Policy.Rule.deny_all ]) r with
          | Policy.Decision.Denied (first, rest) ->
              equal (list access_value) ~msg:"denials in normalized order"
                (Request.normalized_accesses r)
                (List.map Policy.Denial.access (first :: rest))
          | Policy.Decision.Allowed | Policy.Decision.Review _ ->
              failf "deny_all must deny");
      (* allow-all allows every request; default reviews every request. *)
      prop "grouped decision is all-or-review" request_gen (fun r ->
          is_true ~msg:"allow-all allows the whole group"
            (decision_tag
               (Policy.decide
                  (Policy.make [ Policy.Rule.allow_all_dangerously ])
                  r)
            = `Allowed);
          is_true ~msg:"default reviews the whole group"
            (decision_tag (Policy.decide Policy.default r) = `Review));
    ]

(* Matchers *)

let rule_matchers_cover_common_policy_patterns () =
  raises_invalid "exec prefix program cannot be empty" (fun () ->
      ignore (argv_prefix ~program:"" ~args:[] ()));
  ignore (argv_prefix ~program:"printf" ~args:[ "" ] ());
  raises_invalid "exec exact program cannot be empty" (fun () ->
      ignore (argv_exact ~program:"" ~args:[] ()));
  (match Workspace.Root.Key.of_string "" with
  | Error Workspace.Root.Key.Empty -> ()
  | Error
      (Workspace.Root.Key.Invalid_utf_8 | Workspace.Root.Key.Control_character _)
    ->
      failf "empty cwd root key reported the wrong error"
  | Ok _ -> failf "empty cwd root key should be rejected");
  raises_invalid "network host cannot be empty" (fun () ->
      ignore (Policy.Match.network_host ~host:"" ()));
  raises_invalid "extension subject cannot be empty" (fun () ->
      ignore (Policy.Match.custom ~subject:"" "tool"));
  let path_policy =
    Policy.make
      [
        Policy.Rule.allow
          (Policy.Match.path ~op:`Modify
             (Policy.Match.Path.under (workspace_path "lib")));
      ]
  in
  expect_allow "path under allows descendants"
    (Policy.decide path_policy (request (workspace_modify "lib/a.ml")));
  let path_key_policy =
    Policy.make
      [
        Policy.Rule.allow
          (Policy.Match.path ~op:`Modify
             (Policy.Match.Path.under (path_with_key "lib")));
      ]
  in
  expect_allow "path under key allows descendants"
    (Policy.decide path_key_policy (request (workspace_modify "lib/a.ml")));
  let path_scope_policy =
    Policy.make
      [
        Policy.Rule.allow
          (Policy.Match.path ~op:`Modify
             (Policy.Match.Path.exact (workspace_path "lib/a.ml")));
      ]
  in
  expect_allow "path scope exact allows matching path"
    (Policy.decide path_scope_policy (request (workspace_modify "lib/a.ml")));
  ignore
    (expect_review "path scope exact rejects descendants"
       (Policy.decide path_scope_policy
          (request (workspace_modify "lib/a.ml/generated"))));
  let workspace_path_policy =
    Policy.make
      [
        Policy.Rule.allow
          (Policy.Match.path ~op:`Read Policy.Match.Path.workspace);
      ]
  in
  expect_allow "workspace path matcher allows classified workspace paths"
    (Policy.decide workspace_path_policy (request (workspace_read "README.md")));
  ignore
    (expect_review "workspace path matcher checks op"
       (Policy.decide workspace_path_policy
          (request (workspace_modify "README.md"))));
  ignore
    (expect_review "workspace path matcher rejects outside paths"
       (Policy.decide workspace_path_policy
          (request (outside_path `Read "/repo/README.md"))));
  ignore
    (expect_review "workspace path matcher rejects unknown paths"
       (Policy.decide workspace_path_policy
          (request (unknown_read "README.md"))));
  ignore
    (expect_review "path under is segment based"
       (Policy.decide path_policy (request (workspace_modify "liberty/a.ml"))));
  ignore
    (expect_review "path under does not match outside-workspace paths"
       (Policy.decide path_policy
          (request (outside_path `Modify "/repo/lib/a.ml"))));
  ignore
    (expect_review "path under does not match unknown paths"
       (Policy.decide path_policy
          (request (unknown_path `Modify "/repo/lib/a.ml"))));
  let outside_policy =
    Policy.make
      [
        Policy.Rule.deny
          (Policy.Match.path ~op:`Modify Policy.Match.Path.outside_workspace);
      ]
  in
  expect_deny "outside-workspace matcher denies matching paths"
    (Policy.decide outside_policy
       (request (outside_path `Modify "/tmp/outside.ml")));
  ignore
    (expect_review "outside-workspace matcher checks op"
       (Policy.decide outside_policy
          (request (outside_path `Read "/tmp/outside.ml"))));
  ignore
    (expect_review "outside-workspace matcher does not match workspace paths"
       (Policy.decide outside_policy (request (workspace_modify "lib/a.ml"))));
  let unknown_policy =
    Policy.make
      [
        Policy.Rule.review (Policy.Match.path Policy.Match.Path.unknown);
        Policy.Rule.allow_all_dangerously;
      ]
  in
  ignore
    (expect_review "unknown path matcher matches unknown paths"
       (Policy.decide unknown_policy (request (unknown_path `Delete "target"))));
  expect_allow "unknown path matcher does not match workspace paths"
    (Policy.decide unknown_policy (request (workspace_read "README.md")));
  let exec_policy =
    Policy.make
      [ Policy.Rule.allow (argv_prefix ~program:"git" ~args:[ "status" ] ()) ]
  in
  expect_allow "exec prefix allows matching argv prefix"
    (Policy.decide exec_policy (request (exec "git" [ "status"; "--short" ])));
  ignore
    (expect_review "exec prefix does not match shell text"
       (Policy.decide exec_policy (request (shell "git status --short"))));
  let command_exact_policy =
    Policy.make
      [
        Policy.Rule.allow
          (argv_exact ~program:"git" ~args:[ "status"; "--short" ] ());
      ]
  in
  expect_allow "exec exact allows matching argv"
    (Policy.decide command_exact_policy
       (request (exec "git" [ "status"; "--short" ])));
  ignore
    (expect_review "exec exact rejects extra argv"
       (Policy.decide command_exact_policy
          (request (exec "git" [ "status"; "--short"; "--branch" ]))));
  let cwd_policy =
    Policy.make
      [
        Policy.Rule.allow
          (argv_prefix
             ~cwd:(Policy.Match.Path.under (path_with_key "."))
             ~program:"dune" ~args:[ "build" ] ());
      ]
  in
  expect_allow "exec prefix can require cwd under workspace root"
    (Policy.decide cwd_policy
       (request
          (argv
             ~cwd:(Access.Path_scope.workspace (workspace_path "src"))
             ~program:"dune" [ "build"; "@all" ])));
  let workspace_cwd_policy =
    Policy.make
      [
        Policy.Rule.allow
          (argv_prefix ~cwd:Policy.Match.Path.workspace ~program:"dune"
             ~args:[ "build" ] ());
      ]
  in
  expect_allow "exec prefix can require any workspace cwd"
    (Policy.decide workspace_cwd_policy
       (request
          (argv
             ~cwd:(Access.Path_scope.workspace (workspace_path "tests"))
             ~program:"dune" [ "build"; "@all" ])));
  ignore
    (expect_review "workspace cwd matcher rejects outside cwd"
       (Policy.decide workspace_cwd_policy
          (request
             (argv
                ~cwd:(Access.Path_scope.outside_workspace (abs "/tmp/repo"))
                ~program:"dune" [ "build" ]))));
  ignore
    (expect_review "exec cwd matcher rejects another cwd"
       (Policy.decide cwd_policy
          (request
             (argv
                ~cwd:(Access.Path_scope.outside_workspace (abs "/tmp/repo"))
                ~program:"dune" [ "build" ]))));
  ignore
    (expect_review "exec cwd matcher rejects unknown cwd"
       (Policy.decide cwd_policy
          (request
             (argv
                ~cwd:(Access.Path_scope.unknown "unresolved")
                ~program:"dune" [ "build" ]))));
  let network_policy =
    Policy.make
      [
        Policy.Rule.allow
          (Policy.Match.network_host ~protocol:`Https ~port:443
             ~host:"example.com" ());
      ]
  in
  expect_allow "network host allows matching endpoint"
    (Policy.decide network_policy
       (request
          (Access.network ~protocol:`Https ~port:443 ~host:"example.com" ())));
  ignore
    (expect_review "network host checks port when provided"
       (Policy.decide network_policy
          (request
             (Access.network ~protocol:`Https ~port:8443 ~host:"example.com" ()))));
  let extension_policy =
    Policy.make
      [ Policy.Rule.allow (Policy.Match.custom ~subject:"todo" "todo_write") ]
  in
  expect_allow "extension matcher allows matching extension facts"
    (Policy.decide extension_policy
       (request (Access.custom ~subject:"todo" "todo_write")));
  ignore
    (expect_review "extension matcher checks subject when provided"
       (Policy.decide extension_policy
          (request (Access.custom ~subject:"note" "todo_write"))))

let relative_scopes_match_any_workspace_root () =
  let read_in root_key relative =
    Access.path ~op:`Read (workspace_path ~root_key relative)
  in
  let allow_under =
    Policy.make
      [
        Policy.Rule.allow
          (Policy.Match.path ~op:`Read
             (Policy.Match.Path.under_relative (local "lib")));
      ]
  in
  expect_allow "relative under matches one root"
    (Policy.decide allow_under (request (read_in "alpha" "lib/a.ml")));
  expect_allow "relative under matches another root"
    (Policy.decide allow_under (request (read_in "beta" "lib/sub/b.ml")));
  ignore
    (expect_review "relative under is segment based"
       (Policy.decide allow_under (request (read_in "alpha" "lib2/a.ml"))));
  ignore
    (expect_review "relative under skips outside paths"
       (Policy.decide allow_under (request (outside_path `Read "/lib/a.ml"))));
  ignore
    (expect_review "relative under skips unknown paths"
       (Policy.decide allow_under (request (unknown_read "lib/a.ml"))));
  let allow_exact =
    Policy.make
      [
        Policy.Rule.allow
          (Policy.Match.path ~op:`Modify
             (Policy.Match.Path.exact_relative (local "dune-project")));
      ]
  in
  expect_allow "relative exact matches any root"
    (Policy.decide allow_exact
       (request
          (Access.path ~op:`Modify
             (workspace_path ~root_key:"gamma" "dune-project"))));
  ignore
    (expect_review "relative exact skips other relatives"
       (Policy.decide allow_exact
          (request
             (Access.path ~op:`Modify
                (workspace_path ~root_key:"gamma" "doc/dune-project")))));
  let under_rule =
    Policy.Rule.allow
      (Policy.Match.path ~op:`Read
         (Policy.Match.Path.under_relative (local "lib")))
  in
  roundtrip "relative under rule JSON roundtrips" rule_value Policy.Rule.jsont
    under_rule;
  let exact_rule =
    Policy.Rule.deny
      (Policy.Match.path (Policy.Match.Path.exact_relative (local ".env")))
  in
  roundtrip "relative exact rule JSON roundtrips" rule_value Policy.Rule.jsont
    exact_rule;
  let exec_rule =
    Policy.Rule.allow
      (argv_prefix
         ~cwd:(Policy.Match.Path.under_relative (local "src"))
         ~program:"dune" ~args:[ "build" ] ())
  in
  roundtrip "relative cwd scope JSON roundtrips" rule_value Policy.Rule.jsont
    exec_rule

let high_impact_command_matcher () =
  let high_impact = Policy.Match.command Policy.Match.Command.high_impact in
  let matches access = Policy.Match.matches high_impact access in
  let yes msg access = is_true ~msg (matches access) in
  let no msg access = is_true ~msg (not (matches access)) in
  yes "rm -rf is high_impact" (exec "rm" [ "-rf"; "build" ]);
  yes "rm -r is high_impact" (exec "rm" [ "-r"; "dir" ]);
  no "rm -f of a named file is not high impact" (exec "rm" [ "-f"; "file" ]);
  yes "rm --recursive is high_impact" (exec "rm" [ "--recursive"; "dir" ]);
  no "rm of a named file is not flagged" (exec "rm" [ "notes.txt" ]);
  yes "an absolute rm path resolves by basename" (exec "/bin/rm" [ "-rf"; "x" ]);
  yes "git push --force is high_impact" (exec "git" [ "push"; "--force" ]);
  yes "git push -f is high_impact" (exec "git" [ "push"; "-f" ]);
  no "an ordinary git push is not flagged"
    (exec "git" [ "push"; "origin"; "main" ]);
  yes "git -C cannot hide a force push"
    (exec "git" [ "-C"; "repo"; "push"; "--force" ]);
  yes "git -c cannot hide a hard reset"
    (exec "git" [ "-c"; "a=b"; "reset"; "--hard" ]);
  yes "a separated --git-dir cannot hide a clean"
    (exec "git" [ "--git-dir"; "/tmp/g"; "clean"; "-f" ]);
  no "git -C of an ordinary status is not flagged"
    (exec "git" [ "-C"; "repo"; "status" ]);
  yes "git reset --hard is high_impact" (exec "git" [ "reset"; "--hard" ]);
  no "a plain git reset is not flagged" (exec "git" [ "reset" ]);
  yes "git clean --force is high_impact" (exec "git" [ "clean"; "-fd" ]);
  no "git status is not flagged" (exec "git" [ "status" ]);
  no "dd to an ordinary file is not high impact"
    (exec "dd" [ "if=/dev/zero"; "of=disk.img" ]);
  yes "dd directly to a device is high impact"
    (exec "dd" [ "if=image"; "of=/dev/disk2" ]);
  yes "an mkfs variant is high_impact" (exec "mkfs.ext4" [ "/dev/sdb1" ]);
  no "sudo is not high impact merely because it is privileged"
    (exec "sudo" [ "rm"; "x" ]);
  no "ls is not flagged" (exec "ls" [ "-la" ]);
  yes "a high_impact segment in shell text is flagged"
    (shell "cd build && rm -rf .");
  yes "a piped high_impact command is flagged" (shell "echo x | rm -rf y");
  yes "a wrapped high_impact command is flagged"
    (shell "find . -type f | xargs rm -rf");
  yes "command pass-through cannot hide destruction"
    (shell "command rm -rf build");
  yes "exec pass-through cannot hide destruction" (shell "exec rm -rf build");
  yes "a wrapper's flags cannot hide destruction"
    (shell "env -i rm -rf build");
  yes "wrapper flags in argv cannot hide destruction"
    (exec "env" [ "-i"; "rm"; "-rf"; "x" ]);
  yes "a wrapper flag with an argument cannot hide destruction"
    (shell "stdbuf -o0 rm -rf build");
  yes "an environment assignment cannot hide destruction"
    (shell "FOO=1 rm -rf build");
  no "a flag-only wrapper line is not flagged" (shell "env -i");
  yes "shell -c argv is inspected recursively"
    (exec "bash" [ "-lc"; "rm -rf build" ]);
  yes "shell -c text is inspected recursively"
    (shell "sh -c 'git reset --hard'");
  yes "destruction hidden by a substitution is flagged"
    (shell "rm -rf $(cat targets)");
  no "an opaque command substitution is left to confinement"
    (shell "echo $(pick-command)");
  no "dynamic eval is left to confinement" (shell "eval \"$ACTION\"");
  yes "an expanded flag spelling earns review" (shell "rm --recursi*");
  yes "a brace-expanded flag earns review" (shell "find x -{delete,print}");
  yes "a substituted program earns review" (shell "$CMD -rf x");
  no "an argument glob is not flagged" (shell "ls *.ml");
  no "a directory glob argument is not flagged" (shell "cat foo/*.log");
  no "an argument variable is not flagged" (shell "echo $HOME");
  no "a single-quoted flag spelling stays literal" (shell "rm '--recursi*'");
  no "a test bracket is not expansion" (shell "[ -f x ] && echo ok");
  no "a benign command with a redirect is not flagged"
    (shell "dune build 2> log.txt");
  no "a read-only shell command is not flagged" (shell "git status");
  no "opaque language source is not classified by shell heuristics"
    (code ~language:"ocaml" "Sys.remove \"important\"");
  no "the matcher ignores non-command accesses" (workspace_read "README.md");
  roundtrip "the high_impact rule roundtrips through json" rule_value
    Policy.Rule.jsont
    (Policy.Rule.review high_impact)

let enforced_command_matcher () =
  let matcher =
    Policy.Match.command (Policy.Match.Command.execution (enforced ()))
  in
  let direct = exec "dune" [ "build" ] in
  let enforced = enforced_exec "dune" [ "build" ] in
  is_true ~msg:"direct route is explicit"
    (match direct with
    | Access.Command command ->
        Access.Command.execution command = Access.Command.Direct
    | Access.Path _ | Access.Network _ | Access.Custom _ -> false);
  is_true ~msg:"enforced matcher rejects a direct route"
    (not (Policy.Match.matches matcher direct));
  is_true ~msg:"enforced matcher accepts explicit sealed evidence"
    (Policy.Match.matches matcher enforced);
  not_equal access_value ~msg:"execution route is part of permission identity"
    direct enforced;
  roundtrip "enforced access roundtrips" access_value Access.jsont enforced;
  roundtrip "enforced matcher roundtrips" rule_value Policy.Rule.jsont
    (Policy.Rule.allow matcher)

let command_prefixes_preserve_execution_and_cwd () =
  let matcher =
    argv_prefix ~execution:(enforced ()) ~program:"dune" ~args:[ "build" ] ()
  in
  let other_cwd =
    Access.Path_scope.workspace
      (workspace_path ~root_key:"/other" ~root:"/other" "")
  in
  let matches = Policy.Match.matches matcher in
  is_true ~msg:"prefix accepts its execution route and cwd"
    (matches (enforced_exec "dune" [ "build"; "@all" ]));
  is_true ~msg:"prefix rejects direct execution"
    (not (matches (exec "dune" [ "build"; "@all" ])));
  is_true ~msg:"prefix rejects externally confined execution"
    (not
       (matches
          (argv ~execution:Access.Command.External ~program:"dune"
             [ "build"; "@all" ])));
  is_true ~msg:"prefix rejects another workspace"
    (not
       (matches
          (argv ~cwd:other_cwd ~execution:(enforced ()) ~program:"dune"
             [ "build"; "@all" ])))

let rule_observers_and_match_eliminator () =
  let matcher = Policy.Match.path ~op:`Read Policy.Match.Path.workspace in
  let rule = Policy.Rule.make Policy.Rule.Allow matcher in
  (match Policy.Rule.action rule with
  | Policy.Rule.Allow -> ()
  | Policy.Rule.Review | Policy.Rule.Deny ->
      failf "rule action observer returned the wrong action");
  equal rule_value ~msg:"Rule.make constructs the same rule as Rule.allow"
    (Policy.Rule.allow matcher)
    rule;
  is_true ~msg:"matcher observer matches workspace reads"
    (Policy.Match.matches (Policy.Rule.matcher rule)
       (workspace_read "README.md"));
  is_true ~msg:"matcher observer rejects workspace writes"
    (not
       (Policy.Match.matches (Policy.Rule.matcher rule)
          (workspace_modify "README.md")));
  is_true ~msg:"direct matcher eliminator matches exact access"
    (Policy.Match.matches
       (Policy.Match.exact (shell "make test"))
       (shell "make test"))

let matchers =
  group "matchers"
    [
      test "rule matchers cover common policy patterns"
        rule_matchers_cover_common_policy_patterns;
      test "relative scopes match any workspace root"
        relative_scopes_match_any_workspace_root;
      test "high_impact command matcher classifies irreversible commands"
        high_impact_command_matcher;
      test "enforced command matcher requires explicit execution evidence"
        enforced_command_matcher;
      test "command prefixes preserve execution route and cwd"
        command_prefixes_preserve_execution_and_cwd;
      test "rule observers and matcher eliminator"
        rule_observers_and_match_eliminator;
    ]

(* Grants and review *)

let grants_allow_reviewed_accesses () =
  let command = shell "make test" in
  let review =
    expect_review "review-all policy reviews shell command"
      (Policy.decide Policy.default (request command))
  in
  let grants = remember_exact review Policy.Grants.empty in
  is_true ~msg:"grants remember reviewed access"
    (Policy.Grants.allows grants command);
  expect_allow "granted access is allowed without another review"
    (Policy.decide ~grants Policy.default (request command))

let review_of_accesses_uses_durable_request_subset () =
  let read = workspace_read "README.md" in
  let command = shell "make test" in
  let request = Request.of_accesses ~source:"tool" [ read; command ] in
  let review = restore_review request [ command ] in
  equal (list access_value) ~msg:"durable access subset preserves request order"
    [ read; command ]
    (Policy.Review.accesses (restore_review request [ command; read ]));
  (match
     Policy.Review.restore request
       [
         (command, Policy.Review.Unmatched); (command, Policy.Review.Unmatched);
       ]
   with
  | Error (Policy.Review.Duplicate_access duplicate) ->
      equal access_value ~msg:"duplicate restore identifies the access" command
        duplicate
  | Ok _
  | Error (Policy.Review.Empty_accesses | Policy.Review.Access_not_in_request _)
    ->
      failf "duplicate durable review reasons must be rejected");
  let grants = Policy.Review.remember review Policy.Grants.empty in
  is_true ~msg:"remember grants only the durable subset"
    (Policy.Grants.allows grants command
    && not (Policy.Grants.allows grants read));
  (match Policy.Review.restore request [] with
  | Error Policy.Review.Empty_accesses -> ()
  | Ok _
  | Error
      (Policy.Review.Duplicate_access _ | Policy.Review.Access_not_in_request _)
    ->
      failf "durable access subset cannot be empty");
  match
    Policy.Review.restore request
      [ (workspace_modify "lib/a.ml", Policy.Review.Unmatched) ]
  with
  | Error (Policy.Review.Access_not_in_request _) -> ()
  | Ok _
  | Error Policy.Review.Empty_accesses
  | Error (Policy.Review.Duplicate_access _) ->
      failf "durable access subset must belong to request"

let grants_are_exact_key_matches () =
  let cwd = Access.Path_scope.workspace (workspace_path "") in
  let other_cwd = Access.Path_scope.outside_workspace (abs "/tmp/repo") in
  let command = shell ~cwd "make test" in
  let other_cwd = shell ~cwd:other_cwd "make test" in
  let other_command = shell ~cwd "make build" in
  let review =
    expect_review "review-all policy reviews shell command"
      (Policy.decide Policy.default (request command))
  in
  let grants = remember_exact review Policy.Grants.empty in
  ignore
    (expect_review "grant does not allow same shell text in another cwd"
       (Policy.decide ~grants Policy.default (request other_cwd)));
  ignore
    (expect_review "grant does not allow another shell command"
       (Policy.decide ~grants Policy.default (request other_command)))

let rules_override_grants () =
  let command = shell "make test" in
  let review =
    expect_review "review-all policy reviews shell command"
      (Policy.decide Policy.default (request command))
  in
  let grants = remember_exact review Policy.Grants.empty in
  let deny_commands =
    Policy.make [ Policy.Rule.deny (Policy.Match.kind `Command) ]
  in
  expect_deny "deny rules override session grants"
    (Policy.decide ~grants deny_commands (request command));
  let review_commands =
    Policy.make [ Policy.Rule.review (Policy.Match.kind `Command) ]
  in
  ignore
    (expect_review "review rules override session grants"
       (Policy.decide ~grants review_commands (request command)))

let confinement_is_exact_command_and_grant_identity () =
  let project_read_only = enforced_exec "dune" [ "build" ] in
  let project_write =
    argv
      ~execution:(enforced ~write:Access.Command.Confinement.Workspace ())
      ~program:"dune" [ "build" ]
  in
  let read_all =
    argv
      ~execution:(enforced ~read:Access.Command.Confinement.All ())
      ~program:"dune" [ "build" ]
  in
  let network_enabled =
    argv
      ~execution:(enforced ~network:Access.Command.Confinement.Enabled ())
      ~program:"dune" [ "build" ]
  in
  List.iter
    (fun access ->
      not_equal access_value ~msg:"confinement changes command identity"
        project_read_only access;
      not_equal string ~msg:"confinement changes stable identity"
        (Access.stable_text project_read_only)
        (Access.stable_text access))
    [ project_write; read_all; network_enabled ];
  let review =
    expect_review "project command needs review under the pure default"
      (Policy.decide Policy.default (request project_read_only))
  in
  let grants = Policy.Review.remember review Policy.Grants.empty in
  is_true ~msg:"exact grant remembers its confinement"
    (Policy.Grants.allows grants project_read_only);
  List.iter
    (fun access ->
      is_true ~msg:"exact grant does not span confinement"
        (not (Policy.Grants.allows grants access)))
    [ project_write; read_all; network_enabled ]

let grants_and_review =
  group "grants and review"
    [
      test "grants allow reviewed accesses" grants_allow_reviewed_accesses;
      test "review of accesses uses durable request subset"
        review_of_accesses_uses_durable_request_subset;
      test "grants are exact key matches" grants_are_exact_key_matches;
      test "rules override grants" rules_override_grants;
      test "confinement is exact command and grant identity"
        confinement_is_exact_command_and_grant_identity;
      (* a grant matches only the exact reviewed access. *)
      prop "remember grants every reviewed access" request_gen (fun r ->
          let accesses = Request.normalized_accesses r in
          let review = restore_review r accesses in
          let grants = Policy.Review.remember review Policy.Grants.empty in
          List.iter
            (fun access ->
              is_true ~msg:"reviewed access is granted"
                (Policy.Grants.allows grants access))
            accesses);
      prop "a grant never broadens to a distinct access"
        (Gen.pair access_gen access_gen) (fun (a, b) ->
          if Access.equal a b then ()
          else begin
            let review = restore_review (request a) [ a ] in
            let grants = Policy.Review.remember review Policy.Grants.empty in
            is_true ~msg:"exact access granted" (Policy.Grants.allows grants a);
            is_true ~msg:"distinct access not granted"
              (not (Policy.Grants.allows grants b))
          end);
      (* restore reconstructs the subset without rerunning policy. *)
      prop "restore reconstructs a non-empty request subset" request_gen
        (fun r ->
          let accesses = Request.normalized_accesses r in
          let review = restore_review r accesses in
          equal (list access_value) ~msg:"restore keeps normalized order"
            accesses
            (Policy.Review.accesses review);
          equal request_value ~msg:"restore preserves the request" r
            (Policy.Review.request review);
          is_true ~msg:"empty reasons rejected"
            (match Policy.Review.restore r [] with
            | Error Policy.Review.Empty_accesses -> true
            | Ok _
            | Error
                ( Policy.Review.Duplicate_access _
                | Policy.Review.Access_not_in_request _ ) ->
                false));
    ]

(* Answer *)

let answer_family_construction_validates () =
  raises ~msg:"empty family rules rejected"
    (Invalid_argument "Mentat_permission.Answer.family: rules must not be empty")
    (fun () -> ignore (Answer.family ~lifetime:Answer.User ~rules:[]));
  raises ~msg:"non-allow family rule rejected"
    (Invalid_argument "Mentat_permission.Answer.family: rules must all allow")
    (fun () ->
      ignore
        (Answer.family ~lifetime:Answer.User
           ~rules:[ Policy.Rule.deny (Policy.Match.kind `Read) ]));
  raises ~msg:"duplicate family rules rejected"
    (Invalid_argument
       "Mentat_permission.Answer.family: rules must not contain duplicates")
    (fun () ->
      let r = Policy.Rule.allow (Policy.Match.kind `Read) in
      ignore (Answer.family ~lifetime:Answer.Conversation ~rules:[ r; r ]));
  let r1 = Policy.Rule.allow (Policy.Match.kind `Read) in
  let r2 = Policy.Rule.allow (Policy.Match.kind `Write) in
  match Answer.family ~lifetime:Answer.User ~rules:[ r1; r2 ] with
  | Answer.Allow (Answer.Family _) -> ()
  | Answer.Allow (Answer.Once | Answer.Exact_for_conversation) | Answer.Deny ->
      failf "a valid family must produce a family allowance"

let answer_allow_deny_exclusivity () =
  (match Answer.once with
  | Answer.Allow Answer.Once -> ()
  | Answer.Allow (Answer.Exact_for_conversation | Answer.Family _) | Answer.Deny
    ->
      failf "once answer shape");
  (match Answer.exact_for_conversation with
  | Answer.Allow Answer.Exact_for_conversation -> ()
  | Answer.Allow (Answer.Once | Answer.Family _) | Answer.Deny ->
      failf "exact allowance shape");
  (match Answer.deny with
  | Answer.Deny -> ()
  | Answer.Allow _ -> failf "deny answer shape");
  let r = Policy.Rule.allow (Policy.Match.kind `Read) in
  match Answer.family ~lifetime:Answer.User ~rules:[ r ] with
  | Answer.Allow (Answer.Family { lifetime = Answer.User; rules = [ rr ] }) ->
      equal rule_value ~msg:"family preserves its rule" r rr
  | Answer.Allow _ | Answer.Deny -> failf "family allowance shape"

let answer_equality () =
  let r1 = Policy.Rule.allow (Policy.Match.kind `Read) in
  let r2 = Policy.Rule.allow (Policy.Match.kind `Write) in
  is_false ~msg:"once is not exact"
    (Answer.equal Answer.once Answer.exact_for_conversation);
  is_false ~msg:"allow is not deny" (Answer.equal Answer.once Answer.deny);
  is_true ~msg:"family is reflexive"
    (Answer.equal
       (Answer.family ~lifetime:Answer.User ~rules:[ r1; r2 ])
       (Answer.family ~lifetime:Answer.User ~rules:[ r1; r2 ]));
  is_false ~msg:"family rule order participates"
    (Answer.equal
       (Answer.family ~lifetime:Answer.User ~rules:[ r1; r2 ])
       (Answer.family ~lifetime:Answer.User ~rules:[ r2; r1 ]));
  is_false ~msg:"family lifetime participates"
    (Answer.equal
       (Answer.family ~lifetime:Answer.Conversation ~rules:[ r1 ])
       (Answer.family ~lifetime:Answer.User ~rules:[ r1 ]))

let answer_wire_golden () =
  let r = Policy.Rule.allow (Policy.Match.kind `Read) in
  is_true ~msg:"once wire is versioned tagged enum"
    (Json.equal
       (obj [ ("version", Json.int 1); ("answer", Json.string "allow-once") ])
       (json_encode Answer.jsont Answer.once));
  is_true ~msg:"deny wire carries no payload"
    (Json.equal
       (obj [ ("version", Json.int 1); ("answer", Json.string "deny") ])
       (json_encode Answer.jsont Answer.deny));
  is_true ~msg:"exact-for-conversation wire"
    (Json.equal
       (obj
          [
            ("version", Json.int 1);
            ("answer", Json.string "allow-exact-for-conversation");
          ])
       (json_encode Answer.jsont Answer.exact_for_conversation));
  let fam = Answer.family ~lifetime:Answer.User ~rules:[ r ] in
  is_true ~msg:"family wire carries lifetime and rules"
    (Json.equal
       (obj
          [
            ("version", Json.int 1);
            ("answer", Json.string "allow-family");
            ("lifetime", Json.string "user");
            ("rules", Json.list [ json_encode Policy.Rule.jsont r ]);
          ])
       (json_encode Answer.jsont fam));
  roundtrip "answer once roundtrips" answer_value Answer.jsont Answer.once;
  roundtrip "answer deny roundtrips" answer_value Answer.jsont Answer.deny;
  roundtrip "answer family roundtrips" answer_value Answer.jsont fam

let allow_rule_json =
  lazy
    (json_encode Policy.Rule.jsont
       (Policy.Rule.allow (Policy.Match.kind `Read)))

let deny_rule_json =
  lazy
    (json_encode Policy.Rule.jsont (Policy.Rule.deny (Policy.Match.kind `Read)))

let answer_rejections () =
  let allow_rule = Lazy.force allow_rule_json in
  let deny_rule = Lazy.force deny_rule_json in
  [
    ( "unknown answer version",
      obj [ ("version", Json.int 2); ("answer", Json.string "allow-once") ] );
    ( "unknown answer tag",
      obj [ ("version", Json.int 1); ("answer", Json.string "bogus") ] );
    ( "unknown member",
      obj
        [
          ("version", Json.int 1);
          ("answer", Json.string "allow-once");
          ("extra", Json.int 1);
        ] );
    ( "family missing lifetime",
      obj
        [
          ("version", Json.int 1);
          ("answer", Json.string "allow-family");
          ("rules", Json.list [ allow_rule ]);
        ] );
    ( "family missing rules",
      obj
        [
          ("version", Json.int 1);
          ("answer", Json.string "allow-family");
          ("lifetime", Json.string "user");
        ] );
    ( "family empty rules",
      obj
        [
          ("version", Json.int 1);
          ("answer", Json.string "allow-family");
          ("lifetime", Json.string "user");
          ("rules", Json.list []);
        ] );
    ( "family duplicate rules",
      obj
        [
          ("version", Json.int 1);
          ("answer", Json.string "allow-family");
          ("lifetime", Json.string "user");
          ("rules", Json.list [ allow_rule; allow_rule ]);
        ] );
    ( "family non-allow rule",
      obj
        [
          ("version", Json.int 1);
          ("answer", Json.string "allow-family");
          ("lifetime", Json.string "conversation");
          ("rules", Json.list [ deny_rule ]);
        ] );
    ( "deny carrying rules",
      obj
        [
          ("version", Json.int 1);
          ("answer", Json.string "deny");
          ("rules", Json.list [ allow_rule ]);
        ] );
    ( "once carrying lifetime",
      obj
        [
          ("version", Json.int 1);
          ("answer", Json.string "allow-once");
          ("lifetime", Json.string "user");
        ] );
  ]

let answer =
  group "answer"
    [
      test "family construction validates rules"
        answer_family_construction_validates;
      test "allow and deny are exclusive" answer_allow_deny_exclusivity;
      test "answer equality is authority equality" answer_equality;
      test "answer wire is a versioned tagged enum" answer_wire_golden;
      cases "answer decode fails closed" ~name:fst (answer_rejections ())
        (fun (_, json) ->
          is_true (Result.is_error (Json.decode Answer.jsont json)));
      prop "answer jsont round-trips" answer_gen (fun a ->
          equal answer_value a
            (json_decode Answer.jsont (json_encode Answer.jsont a)));
    ]

(* Codecs *)

let json_roundtrips_core_values () =
  let accesses =
    [
      workspace_read "README.md";
      workspace_modify "lib/a.ml";
      shell
        ~cwd:(Access.Path_scope.workspace (workspace_path ""))
        "dune runtest";
      argv
        ~cwd:(Access.Path_scope.workspace (workspace_path ""))
        ~program:"dune" [ "runtest" ];
      Access.argv ~cwd:default_cwd ~execution:(enforced ()) ~program:"dune"
        [ "build" ];
      code ~execution:(enforced ()) ~language:"ocaml" "1 + 1;;";
      Access.network ~protocol:`Https ~port:443 ~host:"example.com" ();
      Access.network ~protocol:(`Other "unix") ~host:"socket" ();
      Access.custom ~subject:"todo" "todo_write";
    ]
  in
  List.iter
    (roundtrip "access JSON roundtrip" access_value Access.jsont)
    accesses;
  roundtrip "request JSON roundtrip" request_value Request.jsont
    (Request.of_accesses ~source:"tool:shell" ~display:"dune runtest" accesses);
  let rule =
    Policy.Rule.allow
      (Policy.Match.exact
         (shell
            ~cwd:(Access.Path_scope.workspace (workspace_path ""))
            "dune runtest"))
  in
  roundtrip "rule JSON roundtrip" rule_value Policy.Rule.jsont rule;
  let policy =
    Policy.make
      [
        Policy.Rule.allow (Policy.Match.kind `Read);
        Policy.Rule.review (Policy.Match.kind `Command);
        Policy.Rule.deny (Policy.Match.exact (shell "rm -rf _build"));
        Policy.Rule.allow
          (Policy.Match.path ~op:`Read Policy.Match.Path.workspace);
        Policy.Rule.allow
          (Policy.Match.path (Policy.Match.Path.under (workspace_path "test")));
        Policy.Rule.deny (Policy.Match.path Policy.Match.Path.outside_workspace);
        Policy.Rule.review
          (Policy.Match.path ~op:`Read Policy.Match.Path.unknown);
        Policy.Rule.allow (argv_prefix ~program:"git" ~args:[ "status" ] ());
        Policy.Rule.allow (argv_exact ~program:"git" ~args:[ "status" ] ());
        Policy.Rule.allow
          (argv_prefix
             ~cwd:(Policy.Match.Path.exact (path_with_key "."))
             ~program:"dune" ~args:[ "build" ] ());
        Policy.Rule.deny (Policy.Match.network_host ~host:"example.com" ());
      ]
  in
  roundtrip "policy JSON roundtrip" policy_value Policy.jsont policy

let access_json_normalizes_outside_workspace_paths () =
  let outside_key_json =
    obj
      [
        ("type", Json.string "path");
        ("op", Json.string "read");
        ("scope", Json.string "outside");
        ("path", Json.string "/a/../b");
      ]
  in
  equal access_value ~msg:"outside path access JSON is normalized"
    (outside_path `Read "/b")
    (json_decode Access.jsont outside_key_json);
  let cwd_key_json =
    obj
      [
        ("type", Json.string "command");
        ("kind", Json.string "shell");
        ("text", Json.string "make");
        ("execution", obj [ ("kind", Json.string "direct") ]);
        ( "cwd",
          obj
            [
              ("scope", Json.string "outside"); ("path", Json.string "/a/../b");
            ] );
      ]
  in
  equal access_value ~msg:"outside cwd access JSON is normalized"
    (shell ~cwd:(Access.Path_scope.outside_workspace (abs "/b")) "make")
    (json_decode Access.jsont cwd_key_json)

let some_access_json = lazy (json_encode Access.jsont (extension "ask"))

let json_rejections () =
  let unknown_cwd =
    obj [ ("scope", Json.string "unknown"); ("path", Json.string "x") ]
  in
  [
    ( "unknown request version",
      obj
        [
          ("version", Json.int 2);
          ( "items",
            Json.list [ obj [ ("access", Lazy.force some_access_json) ] ] );
        ] );
    ( "empty shell command text",
      obj
        [
          ("type", Json.string "command");
          ("kind", Json.string "shell");
          ("text", Json.string "");
          ("cwd", unknown_cwd);
          ("execution", obj [ ("kind", Json.string "direct") ]);
        ] );
    ( "enforced execution missing confinement",
      obj
        [
          ("type", Json.string "command");
          ("kind", Json.string "shell");
          ("text", Json.string "dune build");
          ("cwd", unknown_cwd);
          ("execution", obj [ ("kind", Json.string "enforced") ]);
        ] );
    ( "execution as a bare string",
      obj
        [
          ("type", Json.string "command");
          ("kind", Json.string "shell");
          ("text", Json.string "dune build");
          ("cwd", unknown_cwd);
          ("execution", Json.string "direct");
        ] );
    ( "argv prefix missing execution and cwd",
      obj
        [
          ("action", Json.string "allow");
          ( "matcher",
            obj
              [
                ("type", Json.string "command");
                ( "pattern",
                  obj
                    [
                      ("type", Json.string "argv-prefix");
                      ("program", Json.string "dune");
                      ("args", Json.list [ Json.string "build" ]);
                    ] );
              ] );
        ] );
    ( "path-under-relative cannot be absolute",
      obj
        [
          ("action", Json.string "allow");
          ( "matcher",
            obj
              [
                ("type", Json.string "path-under-relative");
                ("relative", Json.string "/lib");
              ] );
        ] );
    ( "path-under-relative cannot be empty",
      obj
        [
          ("action", Json.string "allow");
          ( "matcher",
            obj
              [
                ("type", Json.string "path-under-relative");
                ("relative", Json.string "");
              ] );
        ] );
    ( "custom access cannot impersonate a command",
      obj
        [
          ("type", Json.string "custom");
          ("kind", Json.string "command");
          ("name", Json.string "ocaml.eval");
          ("subject", Json.string "1 + 1");
        ] );
    ( "custom matcher cannot select command work",
      obj
        [
          ("action", Json.string "allow");
          ( "matcher",
            obj
              [
                ("type", Json.string "custom");
                ("kind", Json.string "command");
                ("name", Json.string "ocaml.eval");
              ] );
        ] );
    ( "unknown policy version",
      obj [ ("version", Json.int 2); ("rules", Json.list []) ] );
  ]

let review_value =
  Testable.make
    ~pp:(fun ppf _ -> Format.pp_print_string ppf "<review>")
    ~equal:Policy.Review.equal

let review_json_roundtrips () =
  let read = workspace_read "README.md" in
  let command = shell "make test" in
  let request = Request.of_accesses ~source:"tool" [ read; command ] in
  let rule = Policy.Rule.review (Policy.Match.kind `Command) in
  let review =
    expect_review "command review rule reviews the command"
      (Policy.decide (Policy.make [ rule ]) request)
  in
  (* [review] captures both reason shapes: [read] is Unmatched and [command]
     is By_rule, so the round-trip covers the whole reason vocabulary. *)
  roundtrip "review JSON roundtrip" review_value Policy.Review.jsont review;
  let restored = restore_review request [ read; command ] in
  roundtrip "restored review JSON roundtrip" review_value Policy.Review.jsont
    restored;
  is_false ~msg:"captured reasons participate in equality"
    (Policy.Review.equal review restored);
  is_true ~msg:"wire carries version, request, and captured reasons"
    (Json.equal
       (obj
          [
            ("version", Json.int 1);
            ("request", json_encode Request.jsont request);
            ( "reasons",
              Json.list
                [
                  obj
                    [
                      ("access", json_encode Access.jsont read);
                      ("reason", obj [ ("type", Json.string "unmatched") ]);
                    ];
                  obj
                    [
                      ("access", json_encode Access.jsont command);
                      ( "reason",
                        obj
                          [
                            ("type", Json.string "by-rule");
                            ("rule", json_encode Policy.Rule.jsont rule);
                          ] );
                    ];
                ] );
          ])
       (json_encode Policy.Review.jsont review))

let review_json_rejections () =
  let read = workspace_read "README.md" in
  let request_json = json_encode Request.jsont (Request.of_accesses [ read ]) in
  let unmatched = obj [ ("type", Json.string "unmatched") ] in
  let entry access =
    obj [ ("access", json_encode Access.jsont access); ("reason", unmatched) ]
  in
  let review ?(version = 1) reasons =
    obj
      [
        ("version", Json.int version);
        ("request", request_json);
        ("reasons", Json.list reasons);
      ]
  in
  [
    ("unknown review version", review ~version:2 [ entry read ]);
    ("empty review reasons", review []);
    ("duplicate review access", review [ entry read; entry read ]);
    ( "review access not in request",
      review [ entry (workspace_modify "lib/a.ml") ] );
    ( "unknown review reason type",
      review
        [
          obj
            [
              ("access", json_encode Access.jsont read);
              ("reason", obj [ ("type", Json.string "mystery") ]);
            ];
        ] );
  ]

let codecs =
  group "codecs"
    [
      test "json roundtrips core values" json_roundtrips_core_values;
      test "review json roundtrips captured reasons" review_json_roundtrips;
      cases ~name:fst "review json decode fails closed"
        (review_json_rejections ()) (fun (_, json) ->
          is_true (Result.is_error (Json.decode Policy.Review.jsont json)));
      test "access json normalizes outside paths"
        access_json_normalizes_outside_workspace_paths;
      cases "json decode fails closed" ~name:fst (json_rejections ())
        (fun (_, json) ->
          (* Each row targets a specific codec; probe both access and rule
             codecs so the row rejects under whichever owns it. *)
          is_true
            (Result.is_error (Json.decode Access.jsont json)
            && Result.is_error (Json.decode Policy.Rule.jsont json)
            && Result.is_error (Json.decode Request.jsont json)
            && Result.is_error (Json.decode Policy.jsont json)));
      prop "access jsont round-trips generated accesses" access_gen (fun a ->
          equal access_value a
            (json_decode Access.jsont (json_encode Access.jsont a)));
      prop "rule jsont round-trips generated rules" rule_gen (fun r ->
          equal rule_value r
            (json_decode Policy.Rule.jsont (json_encode Policy.Rule.jsont r)));
      prop "policy jsont round-trips generated policies" policy_gen (fun p ->
          equal policy_value p
            (json_decode Policy.jsont (json_encode Policy.jsont p)));
      prop "request jsont round-trips generated requests" request_gen (fun r ->
          equal request_value r
            (json_decode Request.jsont (json_encode Request.jsont r)));
      (* Obligation C: security semantics survive a codec round-trip. A request
         denied (or reviewed, or allowed) under a policy keeps that verdict
         after both are re-decoded from their wire. *)
      prop "decide verdict survives a jsont round-trip of policy and request"
        (Gen.pair policy_gen request_gen) (fun (p, r) ->
          let p' = json_decode Policy.jsont (json_encode Policy.jsont p) in
          let r' = json_decode Request.jsont (json_encode Request.jsont r) in
          is_true ~msg:"same verdict"
            (decision_tag (Policy.decide p r)
            = decision_tag (Policy.decide p' r')));
    ]

(* Review behavior: the per-run posture over review outcomes. *)

let behavior_value =
  Testable.make ~pp:Review_behavior.pp ~equal:Review_behavior.equal

let review_behavior_vocabulary =
  group "review behavior"
    [
      test "equal distinguishes enforce from bypass" (fun () ->
          is_true ~msg:"enforce reflexive"
            (Review_behavior.equal Review_behavior.Enforce
               Review_behavior.Enforce);
          is_true ~msg:"bypass reflexive"
            (Review_behavior.equal Review_behavior.Bypass Review_behavior.Bypass);
          is_false ~msg:"enforce is not bypass"
            (Review_behavior.equal Review_behavior.Enforce
               Review_behavior.Bypass));
      test "pp renders the stable spellings" (fun () ->
          equal string ~msg:"enforce" "enforce"
            (Format.asprintf "%a" Review_behavior.pp Review_behavior.Enforce);
          equal string ~msg:"bypass" "bypass"
            (Format.asprintf "%a" Review_behavior.pp Review_behavior.Bypass));
      test "wire is the enforce/bypass string enum" (fun () ->
          is_true ~msg:"enforce wire"
            (Json.equal (Json.string "enforce")
               (json_encode Review_behavior.jsont Review_behavior.Enforce));
          is_true ~msg:"bypass wire"
            (Json.equal (Json.string "bypass")
               (json_encode Review_behavior.jsont Review_behavior.Bypass));
          roundtrip "enforce roundtrips" behavior_value Review_behavior.jsont
            Review_behavior.Enforce;
          roundtrip "bypass roundtrips" behavior_value Review_behavior.jsont
            Review_behavior.Bypass;
          is_true ~msg:"unknown spelling rejected"
            (Result.is_error
               (Json.decode Review_behavior.jsont (Json.string "default"))));
    ]

(* Path-scope codec: permission composes Workspace.Path's single durable codec
   under [workspace_path]. Obsolete permission-local address shapes fail. *)

let ws_scope_access =
  Access.path_scope ~op:`Read
    (Access.Path_scope.workspace (path_with_key ~root_key:"the-key" "lib/a.ml"))

let ws_under_rule =
  Policy.Rule.allow
    (Policy.Match.path ~op:`Read
       (Policy.Match.Path.under (path_with_key ~root_key:"the-key" "lib")))

let workspace_scope_composes_workspace_path () =
  is_true ~msg:"workspace access composes the durable workspace path"
    (Json.equal
       (obj
          [
            ("type", Json.string "path");
            ("op", Json.string "read");
            ("scope", Json.string "workspace");
            ("workspace_path", workspace_path_object "the-key" "lib/a.ml");
          ])
       (json_encode Access.jsont ws_scope_access));
  roundtrip "workspace access round-trips through Workspace.Path.jsont"
    access_value Access.jsont ws_scope_access

let outside_and_unknown_keep_flat_path () =
  is_true ~msg:"outside scope keeps { scope; path }"
    (Json.equal
       (obj
          [
            ("type", Json.string "path");
            ("op", Json.string "read");
            ("scope", Json.string "outside");
            ("path", Json.string "/x/y");
          ])
       (json_encode Access.jsont (outside_path `Read "/x/y")));
  is_true ~msg:"unknown scope keeps { scope; path }"
    (Json.equal
       (obj
          [
            ("type", Json.string "path");
            ("op", Json.string "read");
            ("scope", Json.string "unknown");
            ("path", Json.string "z");
          ])
       (json_encode Access.jsont (unknown_read "z")))

let path_matcher_composes_workspace_path () =
  is_true ~msg:"path-under matcher composes the durable workspace path"
    (Json.equal
       (obj
          [
            ("action", Json.string "allow");
            ( "matcher",
              obj
                [
                  ("type", Json.string "path-under");
                  ("op", Json.string "read");
                  ("workspace_path", workspace_path_object "the-key" "lib");
                ] );
          ])
       (json_encode Policy.Rule.jsont ws_under_rule));
  roundtrip "path-under matcher round-trips through Workspace.Path.jsont"
    rule_value Policy.Rule.jsont ws_under_rule

let ws_access_rejections () =
  let base extra =
    obj
      ([
         ("type", Json.string "path");
         ("op", Json.string "read");
         ("scope", Json.string "workspace");
       ]
      @ extra)
  in
  [
    ("workspace scope missing workspace_path", base []);
    ( "flat legacy root_key/relative members",
      base
        [ ("root_key", Json.string "the-key"); ("relative", Json.string "lib") ]
    );
    ( "workspace scope carrying a flat path",
      base [ ("path", Json.string "lib") ] );
    ("obsolete address member", base [ ("address", Json.int 1) ]);
    ("workspace_path is not an object", base [ ("workspace_path", Json.int 1) ]);
    ( "workspace_path missing root",
      base [ ("workspace_path", obj [ ("rel", Json.string "lib") ]) ] );
    ( "workspace_path missing rel",
      base [ ("workspace_path", obj [ ("root", Json.string "k") ]) ] );
    ( "workspace_path rel escapes the root",
      base [ ("workspace_path", workspace_path_object "k" "../secret") ] );
    ( "workspace_path root key is empty",
      base [ ("workspace_path", workspace_path_object "" "lib") ] );
    ( "outside scope carrying a workspace_path",
      obj
        [
          ("type", Json.string "path");
          ("op", Json.string "read");
          ("scope", Json.string "outside");
          ("workspace_path", workspace_path_object "k" "a");
        ] );
  ]

let path_matcher_rejections () =
  [
    ( "flat legacy path-under root_key/relative",
      obj
        [
          ("action", Json.string "allow");
          ( "matcher",
            obj
              [
                ("type", Json.string "path-under");
                ("root_key", Json.string "k");
                ("relative", Json.string "lib");
              ] );
        ] );
    ( "path-under matcher missing workspace_path",
      obj
        [
          ("action", Json.string "allow");
          ( "matcher",
            obj
              [ ("type", Json.string "path-under"); ("op", Json.string "read") ]
          );
        ] );
    ( "path-exact workspace_path rel escapes the root",
      obj
        [
          ("action", Json.string "allow");
          ( "matcher",
            obj
              [
                ("type", Json.string "path-exact");
                ("workspace_path", workspace_path_object "k" "../x");
              ] );
        ] );
  ]

let path_scope_codec =
  group "path-scope codec"
    [
      test "workspace access composes Workspace.Path.jsont"
        workspace_scope_composes_workspace_path;
      test "outside and unknown scopes keep the flat path form"
        outside_and_unknown_keep_flat_path;
      test "path matcher composes Workspace.Path.jsont"
        path_matcher_composes_workspace_path;
      cases ~name:fst "workspace access decode fails closed"
        (ws_access_rejections ()) (fun (_, json) ->
          is_true (Result.is_error (Json.decode Access.jsont json)));
      cases ~name:fst "path matcher decode fails closed"
        (path_matcher_rejections ()) (fun (_, json) ->
          is_true (Result.is_error (Json.decode Policy.Rule.jsont json)));
    ]

(* stable_text *)

let rule_stable_text_distinguishes_rules () =
  let read_path scope = Policy.Match.path ~op:`Read scope in
  let under_alpha =
    Policy.Match.Path.under (path_with_key ~root_key:"alpha" "lib")
  in
  let under_beta =
    Policy.Match.Path.under (path_with_key ~root_key:"beta" "lib")
  in
  not_equal string ~msg:"root keys distinguish stable text"
    (Policy.Rule.stable_text (Policy.Rule.allow (read_path under_alpha)))
    (Policy.Rule.stable_text (Policy.Rule.allow (read_path under_beta)));
  let relative = Policy.Match.Path.under_relative (local "lib") in
  equal string ~msg:"equal rules share stable text"
    (Policy.Rule.stable_text (Policy.Rule.allow (read_path relative)))
    (Policy.Rule.stable_text (Policy.Rule.allow (read_path relative)));
  not_equal string ~msg:"relative scopes carry no root key"
    (Policy.Rule.stable_text (Policy.Rule.allow (read_path under_alpha)))
    (Policy.Rule.stable_text (Policy.Rule.allow (read_path relative)));
  not_equal string ~msg:"action distinguishes stable text"
    (Policy.Rule.stable_text (Policy.Rule.allow (read_path relative)))
    (Policy.Rule.stable_text (Policy.Rule.deny (read_path relative)));
  not_equal string ~msg:"class distinguishes stable text"
    (Policy.Rule.stable_text (Policy.Rule.allow (Policy.Match.kind `Read)))
    (Policy.Rule.stable_text (Policy.Rule.allow (Policy.Match.kind `Write)));
  not_equal string ~msg:"exec prefix and exec exact differ"
    (Policy.Rule.stable_text
       (Policy.Rule.allow (argv_prefix ~program:"dune" ~args:[ "build" ] ())))
    (Policy.Rule.stable_text
       (Policy.Rule.allow (argv_exact ~program:"dune" ~args:[ "build" ] ())));
  not_equal string ~msg:"path op distinguishes stable text"
    (Policy.Rule.stable_text (Policy.Rule.allow (read_path relative)))
    (Policy.Rule.stable_text
       (Policy.Rule.allow (Policy.Match.path ~op:`Modify relative)))

(* Length framing provides the security half of stable_text: collision
   resistance across field boundaries. It
   makes stable_text injective, so two DISTINCT authorities never share a digest
   input. Each pair below embeds the field separator [':'] so that an unframed
   encoding would let one field absorb the boundary and collide with the other
   value — which would let a grant or rule digested for one authority silently
   authorize the other. These are the concrete pairs that pin framing; the
   property below generalizes the guard across the whole access/rule space. *)
let stable_text_is_collision_resistant () =
  let colliding_without_framing =
    [
      (* custom name absorbs the [:some:] subject framing *)
      (Access.custom "n:some:s", Access.custom ~subject:"s:none" "n");
      (* network host absorbs the port option separator *)
      ( Access.network ~protocol:`Tcp ~host:"h:some:1" (),
        Access.network ~protocol:`Tcp ~port:1 ~host:"h" () );
      (* unknown path text absorbs the op/scope separators of a sibling *)
      (unknown_path `Read "a:unknown:b", unknown_path `Read "a:unknown:b:c");
    ]
  in
  List.iter
    (fun (a, b) ->
      is_false ~msg:"the pair is genuinely distinct" (Access.equal a b);
      not_equal string ~msg:"distinct accesses never share stable_text"
        (Access.stable_text a) (Access.stable_text b))
    colliding_without_framing;
  (* the same guard at the rule layer: a matcher must not collide with a
     different matcher through unframed field boundaries *)
  not_equal string ~msg:"distinct custom matchers never share stable_text"
    (Policy.Rule.stable_text
       (Policy.Rule.allow (Policy.Match.custom "n:some:s")))
    (Policy.Rule.stable_text
       (Policy.Rule.allow (Policy.Match.custom ~subject:"s:none" "n")))

let stable_text_injective_over_generated_accesses (a, b) =
  (* the dangerous direction of equal digest input implies equal
     authority. Its contrapositive is what stops a stable_text collision from
     widening a grant. *)
  if String.equal (Access.stable_text a) (Access.stable_text b) then
    is_true ~msg:"stable_text collision implies equal access" (Access.equal a b)

let stable_text_injective_over_generated_rules (a, b) =
  if String.equal (Policy.Rule.stable_text a) (Policy.Rule.stable_text b) then
    is_true ~msg:"stable_text collision implies equal rule"
      (Policy.Rule.equal a b)

let rule_ids_are_stable_digests () =
  let deny_env =
    Policy.Rule.deny
      (Policy.Match.path (Policy.Match.Path.exact_relative (local ".env")))
  in
  let allow_workspace_read =
    Policy.Rule.allow (Policy.Match.path ~op:`Read Policy.Match.Path.workspace)
  in
  let allow_dune_build =
    Policy.Rule.allow
      (argv_prefix ~execution:(enforced ()) ~cwd:Policy.Match.Path.workspace
         ~program:"dune" ~args:[ "build" ] ())
  in
  equal string ~msg:"deny .env rule id is stable" "f7cbde737157"
    (Policy.Rule.Id.to_string (Policy.Rule.id deny_env));
  equal string ~msg:"allow workspace read rule id is stable" "8f692546aae1"
    (Policy.Rule.Id.to_string (Policy.Rule.id allow_workspace_read));
  equal string ~msg:"allow dune build rule id is stable" "c62ee08ba97a"
    (Policy.Rule.Id.to_string (Policy.Rule.id allow_dune_build))

let rule_id_codec_contract () =
  let rule = Policy.Rule.allow (Policy.Match.kind `Read) in
  let id = Policy.Rule.id rule in
  is_true ~msg:"the id wire is a JSON string"
    (Json.equal
       (Json.string (Policy.Rule.Id.to_string id))
       (json_encode Policy.Rule.Id.jsont id));
  let decoded =
    json_decode Policy.Rule.Id.jsont (json_encode Policy.Rule.Id.jsont id)
  in
  is_true ~msg:"the id wire round-trips" (Policy.Rule.Id.equal id decoded);
  equal string ~msg:"the id printer uses the stable wire form"
    (Policy.Rule.Id.to_string id)
    (Format.asprintf "%a" Policy.Rule.Id.pp id);
  equal int ~msg:"id comparison is compatible with equality" 0
    (Policy.Rule.Id.compare id decoded);
  (match Policy.Rule.Id.of_string "abc" with
  | Error (Policy.Rule.Id.Error.Invalid_length { expected; actual }) ->
      equal int ~msg:"the required id length is explicit" 12 expected;
      equal int ~msg:"the malformed id length is retained" 3 actual
  | Error (Policy.Rule.Id.Error.Invalid_character _) ->
      fail "a short id must report its length before its characters"
  | Ok _ -> fail "a short rule id must be rejected");
  match Policy.Rule.Id.of_string "0123456789aF" with
  | Error (Policy.Rule.Id.Error.Invalid_character { index; character }) ->
      equal int ~msg:"the invalid byte position is retained" 11 index;
      equal char ~msg:"the invalid byte is retained" 'F' character;
      is_true ~msg:"the typed JSON codec rejects the same malformed wire"
        (Result.is_error
           (Json.decode Policy.Rule.Id.jsont (Json.string "0123456789aF")))
  | Error (Policy.Rule.Id.Error.Invalid_length _) ->
      fail "a correctly sized id must validate its characters"
  | Ok _ -> fail "uppercase hexadecimal must be rejected"

let rule_id_roundtrips_for_generated_rules rule =
  let id = Policy.Rule.id rule in
  let decoded =
    json_decode Policy.Rule.Id.jsont (json_encode Policy.Rule.Id.jsont id)
  in
  is_true ~msg:"the derived id survives its typed wire codec"
    (Policy.Rule.Id.equal id decoded)

let stable_text_bytes_are_pinned () =
  let access = Access.custom ~subject:"alpha" "tool" in
  equal string ~msg:"access stable bytes" "custom:4:tool:some:5:alpha"
    (Access.stable_text access);
  let rule = Policy.Rule.allow (Policy.Match.kind `Read) in
  equal string ~msg:"rule stable bytes" "allow:kind:read"
    (Policy.Rule.stable_text rule)

let stable_text =
  group "stable_text"
    [
      test "rule stable text distinguishes rules"
        rule_stable_text_distinguishes_rules;
      test "rule ids are stable digests" rule_ids_are_stable_digests;
      test "rule ids have a validated typed wire codec" rule_id_codec_contract;
      prop "derived rule ids round-trip through their typed wire codec" rule_gen
        rule_id_roundtrips_for_generated_rules;
      test "stable text bytes are pinned" stable_text_bytes_are_pinned;
      test "stable_text is collision resistant"
        stable_text_is_collision_resistant;
      (* length framing makes stable_text injective, so a collision on the
         digest input can only be a genuinely equal authority. *)
      prop "access stable_text collisions imply equal accesses"
        (Gen.pair access_gen access_gen)
        stable_text_injective_over_generated_accesses;
      prop "rule stable_text collisions imply equal rules"
        (Gen.pair rule_gen rule_gen)
        stable_text_injective_over_generated_rules;
      (* stable_text is a pure function of the value, hence invariant under
         a jsont round-trip (digest input must not drift on re-decode). *)
      prop "access stable_text is invariant under a jsont round-trip" access_gen
        (fun a ->
          equal string ~msg:"access stable_text stable across round-trip"
            (Access.stable_text a)
            (Access.stable_text
               (json_decode Access.jsont (json_encode Access.jsont a))));
      prop "rule stable_text is invariant under a jsont round-trip" rule_gen
        (fun r ->
          equal string ~msg:"rule stable_text stable across round-trip"
            (Policy.Rule.stable_text r)
            (Policy.Rule.stable_text
               (json_decode Policy.Rule.jsont (json_encode Policy.Rule.jsont r))));
    ]

(* [Match.to_line] is the stable one-line matcher rendering the durable-rule
   listing pins: the codec type tag then the remaining members as [name=value]
   in codec order. Pinned per matcher shape so a schema change that would drift
   the [permission list] MATCH column trips here first. *)
let matcher_to_line () =
  let line = Policy.Match.to_line in
  equal string ~msg:"any" "any" (line Policy.Match.any);
  equal string ~msg:"kind" "kind kind=read" (line (Policy.Match.kind `Read));
  equal string ~msg:"exact network"
    "exact access=network protocol=https host=api.test port=443"
    (line
       (Policy.Match.exact
          (Access.network ~protocol:`Https ~port:443 ~host:"api.test" ())));
  equal string ~msg:"exact path" "exact access=path op=read scope=repo/src"
    (line
       (Policy.Match.exact
          (Access.path ~op:`Read (path_with_key ~root_key:"repo" "src"))));
  equal string ~msg:"path-exact" "path-exact workspace_path=repo/lib/a.ml"
    (line
       (Policy.Match.path
          (Policy.Match.Path.exact (path_with_key ~root_key:"repo" "lib/a.ml"))));
  equal string ~msg:"path-under" "path-under workspace_path=repo/lib"
    (line
       (Policy.Match.path
          (Policy.Match.Path.under (path_with_key ~root_key:"repo" "lib"))));
  equal string ~msg:"path-exact-relative" "path-exact-relative relative=.env"
    (line (Policy.Match.path (Policy.Match.Path.exact_relative (local ".env"))));
  equal string ~msg:"path-under-relative" "path-under-relative relative=notes"
    (line
       (Policy.Match.path (Policy.Match.Path.under_relative (local "notes"))));
  equal string ~msg:"path op restriction"
    "path-under-relative op=modify relative=notes"
    (line
       (Policy.Match.path ~op:`Modify
          (Policy.Match.Path.under_relative (local "notes"))));
  equal string ~msg:"path-workspace" "path-workspace"
    (line (Policy.Match.path Policy.Match.Path.workspace));
  equal string ~msg:"path-outside-workspace" "path-outside-workspace"
    (line (Policy.Match.path Policy.Match.Path.outside_workspace));
  equal string ~msg:"path-unknown" "path-unknown"
    (line (Policy.Match.path Policy.Match.Path.unknown));
  equal string ~msg:"command any" "command pattern=any"
    (line (Policy.Match.command Policy.Match.Command.any));
  equal string ~msg:"command high_impact" "command pattern=high_impact"
    (line (Policy.Match.command Policy.Match.Command.high_impact));
  equal string ~msg:"command execution"
    "command pattern=execution execution=enforced(project,read-only,restricted)"
    (line (Policy.Match.command (Policy.Match.Command.execution (enforced ()))));
  equal string ~msg:"command exact"
    "command pattern=exact access=argv execution=direct cwd=repo/lib \
     program=ls args=[-a]"
    (line
       (Policy.Match.command
          (Policy.Match.Command.exact
             (Access.Command.argv
                ~cwd:
                  (Access.Path_scope.workspace
                     (path_with_key ~root_key:"repo" "lib"))
                ~execution:Access.Command.Direct ~program:"ls" [ "-a" ]))));
  equal string ~msg:"command argv-prefix"
    "command pattern=argv-prefix execution=direct cwd=path-exact \
     workspace_path=repo/lib program=ls args=[-a]"
    (line
       (argv_prefix ~execution:Access.Command.Direct
          ~cwd:(Policy.Match.Path.exact (path_with_key ~root_key:"repo" "lib"))
          ~program:"ls" ~args:[ "-a" ] ()));
  equal string ~msg:"network-host host only" "network-host host=example.com"
    (line (Policy.Match.network_host ~host:"example.com" ()));
  equal string ~msg:"network-host full"
    "network-host protocol=https host=example.com port=443"
    (line
       (Policy.Match.network_host ~protocol:`Https ~port:443 ~host:"example.com"
          ()));
  equal string ~msg:"custom name only" "custom name=tool"
    (line (Policy.Match.custom "tool"));
  equal string ~msg:"custom with subject" "custom name=tool subject=alpha"
    (line (Policy.Match.custom ~subject:"alpha" "tool"))

let matcher_line =
  group "matcher_to_line"
    [ test "to_line pins the rendering of every matcher shape" matcher_to_line ]

let () =
  run "mentat.permission"
    [
      access_facts;
      requests;
      evaluation;
      matchers;
      matcher_line;
      grants_and_review;
      answer;
      codecs;
      review_behavior_vocabulary;
      path_scope_codec;
      stable_text;
    ]
