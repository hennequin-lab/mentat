(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for one-shot Dune project
   description tool. Every effectful case crosses the real declaration,
   provider decoder, cached permission request, erased output boundary,
   workspace capability, and [Workspace_io.Command] supervisor. The scripted
   Dune peer is a real child: it records cwd, argv, and selected environment
   facts before returning deterministic workspace and test protocol payloads.

   The legacy suite covered one real tiny project, a nested project root, the
   absence of model-authored command permission, and one stderr failure. This
   suite retains those contracts and adds strict provider/schema checks,
   owning-root and auxiliary-root identity, exact child environment behavior,
   all project component kinds, compact durable JSON, rendering bounds and
   order, empty projects, adversarial protocol failures, every child-controlled
   terminal cause, per-command timeout, cancellation at each boundary, a root
   replacement race, replay without authority, and constructor invariants.

   [Command.All] cannot produce [Output_limit]; the oversized successful case
   below pins that deliberate full-capture contract. Supervision failures and
   unknown-root capabilities cannot be manufactured by a real child or a value
   constructed through [Workspace_io.resolve]; their classification belongs to
   the workspace supervisor tests. *)

open Windtrap
open Test_tools_support
module Access = Mentat_permission.Access
module Describe = Mentat_tools.Ocaml.Dune_describe
module Json = Jsont.Json
module Permission = Mentat_permission
module Project_output = Mentat_tools_output.Ocaml.Project
module Tool = Mentat_tool
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace

type current = Primary_root | Primary_nested | Auxiliary_root

type world = {
  sw : Eio.Switch.t;
  primary_dir : string;
  auxiliary_dir : string;
  outside_dir : string;
  plan_dir : string;
  io : Wio.t;
  tool : Tool.t;
}

let empty_input = json_object []

let project_exn result =
  match
    Mentat_tools_output.Codec.decode Project_output.jsont (output_exn result)
  with
  | Some project -> project
  | None -> fail "ocaml_dune_describe output carried no project semantics"

let model_call ?(name = Describe.name) provider_input =
  Mentat_llm.Tool.Call.make ~id:"dune-describe-expect" ~name
    ~input:provider_input ()

let decode_call_result tool provider_input =
  Tool.Call.decode tool (model_call provider_input)

let decode_call tool provider_input =
  match decode_call_result tool provider_input with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run_call ?(cancelled = fun () -> false) call =
  Tool.Call.run call ~cancelled |> finished

let run ?cancelled world provider_input =
  run_call ?cancelled (decode_call world.tool provider_input)

let print_status ?(normalize = Fun.id) result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) (normalize message) (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason

let write_disk path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let with_env name value fn =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect fn ~finally:(fun () ->
      match previous with
      | Some value -> Unix.putenv name value
      | None -> Unix.unsetenv name)

let fake_dune_script =
  String.concat "\n"
    [
      "#!/bin/sh";
      "plan=$1";
      "shift";
      "count_file=$plan/count";
      "if [ -f \"$count_file\" ]; then n=$(cat \"$count_file\"); else n=0; fi";
      "n=$((n + 1))";
      "printf '%s' \"$n\" > \"$count_file\"";
      "pwd > \"$plan/cwd-$n\"";
      "printf '%s\\n' \"$@\" > \"$plan/argv-$n\"";
      "if [ \"${MENTAT_DUNE_DESCRIBE_SECRET+x}\" = x ]; then secret=set; else \
       secret=unset; fi";
      (* Both were once redirected to one per-run scratch, so they matched.
         The environment is inherited now: HOME is the launcher's own and is
         not a temp directory. *)
      "if [ \"${HOME-}\" = \"${TMPDIR-}\" ]; then home_is_tmp=yes; else \
       home_is_tmp=no; fi";
      "printf 'secret=%s\\nhome_is_tmp=%s\\nterm=%s\\nno_color=%s\\n' \
       \"$secret\" \"$home_is_tmp\" \"${TERM-unset}\" \"${NO_COLOR-unset}\" > \
       \"$plan/env-$n\"";
      "behavior=response";
      "if [ -f \"$plan/behavior-$n\" ]; then behavior=$(cat \
       \"$plan/behavior-$n\"); fi";
      ": > \"$plan/started-$n\"";
      "response=$plan/response-$n";
      "error=$plan/stderr-$n";
      "case \"$behavior\" in";
      "  response)";
      "    [ ! -f \"$response\" ] || cat \"$response\"";
      "    [ ! -f \"$error\" ] || cat \"$error\" >&2";
      "    [ ! -f \"$plan/cancel-after-$n\" ] || : > \"$plan/cancel-now\"";
      "    ;;";
      "  response_exit7)";
      "    [ ! -f \"$response\" ] || cat \"$response\"";
      "    [ ! -f \"$error\" ] || cat \"$error\" >&2";
      "    exit 7";
      "    ;;";
      "  exit7) printf 'stdout before exit\\n'; printf '\\033[31mchild \
       failed\\033[0m\\n' >&2; exit 7 ;;";
      "  signal) printf 'before signal\\n'; kill -TERM $$ ;;";
      "  sleep) printf 'before sleep\\n'; exec /bin/sleep 60 ;;";
      "  remove_self)";
      "    [ ! -f \"$response\" ] || cat \"$response\"";
      "    rm \"$0\"";
      "    ;;";
      "  large_error)";
      "    printf '\\377\\033[31msecret\\033[0m:' >&2";
      "    /usr/bin/head -c 9000 /dev/zero | /usr/bin/tr '\\0' E >&2";
      "    exit 9";
      "    ;;";
      "  malformed_large)";
      "    printf '(\\377\\033[31msecret\\033[0m:'";
      "    /usr/bin/head -c 9000 /dev/zero | /usr/bin/tr '\\0' X";
      "    printf ')'";
      "    ;;";
      "  full_empty)";
      "    printf '('";
      "    /usr/bin/head -c 70000 /dev/zero | /usr/bin/tr '\\0' ' '";
      "    printf ')'";
      "    ;;";
      "  incomplete)";
      "    [ ! -f \"$response\" ] || cat \"$response\"";
      "    ( /bin/sleep 3 ) &";
      "    ;;";
      "  *) printf 'unknown behavior: %s\\n' \"$behavior\" >&2; exit 64 ;;";
      "esac";
      "";
    ]

let abs path = Lpath.Abs.of_string_exn path

let root key dir =
  Workspace.Root.make ~key:(Workspace.Root.Key.of_string_exn key) (abs dir)

let workspace ~current primary_dir auxiliary_dir =
  let primary = root "main" primary_dir in
  let auxiliary = root "aux" auxiliary_dir in
  let at root rel =
    Workspace.Path.make ~root_key:(Workspace.Root.key root)
      (Lpath.Rel.of_string_exn rel)
  in
  let cwd =
    match current with
    | Primary_root -> at primary "."
    | Primary_nested -> at primary "nested/current"
    | Auxiliary_root -> at auxiliary "."
  in
  match Workspace.make ~cwd ~primary ~read_only:[ auxiliary ] () with
  | Ok workspace -> workspace
  | Error error ->
      failf "workspace construction failed: %a" Workspace.Error.pp error

let with_world ?(current = Primary_root)
    ?(mode = Mentat_config.Mode.Danger_full_access) ?clock ?program name fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir ("mentat-dune-describe-" ^ name ^ "-") "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let primary_dir = Filename.concat base "primary" in
  let auxiliary_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  let tmp_dir = Filename.concat base "tmp" in
  List.iter mkdir [ primary_dir; auxiliary_dir; outside_dir; tmp_dir ];
  mkdir_p (Filename.concat primary_dir "nested/current");
  let plan_dir = Filename.concat primary_dir ".describe-plan" in
  let harness_dir = Filename.concat primary_dir ".harness" in
  List.iter mkdir [ plan_dir; harness_dir ];
  let fake_dune = Filename.concat harness_dir "fake-dune" in
  install_executable fake_dune fake_dune_script;
  with_env "TMPDIR" tmp_dir @@ fun () ->
  with_env "MENTAT_DUNE_DESCRIBE_SECRET" "ambient-secret" @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let logical = workspace ~current primary_dir auxiliary_dir in
  let io = resolve_exn ~sw ~stdenv ~logical ~mode () in
  let program = Option.value program ~default:[ fake_dune; plan_dir ] in
  let tool =
    match clock with
    | None -> Describe.make io ~clock:(Eio.Stdenv.mono_clock stdenv) ~program
    | Some clock -> Describe.make io ~clock ~program
  in
  fn { sw; primary_dir; auxiliary_dir; outside_dir; plan_dir; io; tool }

let plan_file world stem index =
  Filename.concat world.plan_dir (Printf.sprintf "%s-%d" stem index)

let set_response world index response =
  write_disk (plan_file world "response" index) response

let set_stderr world index stderr =
  write_disk (plan_file world "stderr" index) stderr

let set_behavior world index behavior =
  write_disk (plan_file world "behavior" index) behavior

let started world index = Sys.file_exists (plan_file world "started" index)

let invocation_count_at plan_dir =
  match read_disk (Filename.concat plan_dir "count") with
  | text -> int_of_string text
  | exception Sys_error _ -> 0

let invocation_count world = invocation_count_at world.plan_dir
let invocation world stem index = read_disk (plan_file world stem index)

let normalize world text =
  text
  |> String.replace_all ~sub:world.primary_dir ~by:"<primary>"
  |> String.replace_all ~sub:world.auxiliary_dir ~by:"<auxiliary>"
  |> String.replace_all ~sub:world.outside_dir ~by:"<outside>"

let simple_workspace =
  {|
(
 (root .)
 (build_context _build/default)
 (library
  ((name fixture)
   (uid fixture_uid)
   (local true)
   (source_dir lib)
   (requires ())
   (modules (((name Fixture) (impl lib/fixture.ml) (intf ()))))))
)|}

let simple_tests =
  {|
(
 ((name fixture_test)
  (source_dir lib)
  (target test/fixture_test.exe)
  (enabled true)
  (package fixture)
  (location lib/fixture.ml:1:0))
)|}

let rich_workspace outside_dir =
  Printf.sprintf
    {|
(
 (root .)
 (build_context _build/default)
 (library
  ((name zeta)
   (uid zeta_uid)
   (local true)
   (source_dir lib/zeta)
   (requires (external_uid))
   (modules (((name Zeta) (impl lib/zeta/zeta.ml) (intf ()))))))
 (library
  ((name external_dep)
   (uid external_uid)
   (local false)
   (source_dir %s)
   (requires ())
   (modules (((name External_dep) (impl %s/external_dep.ml) (intf ()))))))
 (executables
  ((names (cli helper))
   (requires (zeta_uid))
   (modules (((name Main) (impl bin/main.ml) (intf ()))))))
 (library
  ((name alpha)
   (uid alpha_uid)
   (local true)
   (source_dir lib/alpha)
   (requires ())
   (modules (((name Alpha) (impl lib/alpha/alpha.ml) (intf ()))))))
)|}
    outside_dir outside_dir

let rich_tests =
  {|
(
 ((name disabled_test)
  (source_dir lib/alpha)
  (target test/disabled.exe)
  (enabled false))
 ((name zeta_test)
  (source_dir lib/zeta)
  (target test/zeta.exe)
  (enabled true)
  (package zeta)
  (location lib/zeta/zeta.ml:3:4))
)|}

let set_simple_responses world =
  set_response world 1 simple_workspace;
  set_response world 2 simple_tests

let write_real_project root ~name ~with_test =
  write_disk
    (Filename.concat root "dune-project")
    (Printf.sprintf "(lang dune 3.0)\n(name %s)\n" name);
  write_disk
    (Filename.concat root "lib/dune")
    (Printf.sprintf "(library (name %s))\n" name);
  write_disk (Filename.concat root ("lib/" ^ name ^ ".ml")) "let answer = 42\n";
  if with_test then begin
    write_disk (Filename.concat root "test/dune") "(test (name fixture_test))\n";
    write_disk (Filename.concat root "test/fixture_test.ml") "let () = ()\n"
  end

let decode_verdict tool label provider_input =
  match decode_call_result tool provider_input with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty (Tool.Call.Decode_error.message error)))

let project_json result =
  match Tool.Output.json (output_exn result) with
  | Some json -> json_string json
  | None -> fail "completed Dune description carried no semantic JSON"

let print_project_summary result =
  let output = output_exn result in
  let project = project_exn result in
  Printf.printf "json: %s\ncomponents: %d tests: %d truncated: %b\n"
    (project_json result)
    (Project_output.components project)
    (Project_output.tests project)
    (Tool.Output.truncated output)

let contains text fragment = String.includes ~affix:fragment text

let%expect_test "declaration and empty provider schema are exact and strict" =
  with_world "schema" @@ fun world ->
  let declaration = Tool.declaration world.tool in
  let duplicate_unknown =
    Json.object'
      [
        Json.mem (Json.name "extra") (Json.bool true);
        Json.mem (Json.name "extra") (Json.bool false);
      ]
  in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "empty" empty_input;
  List.iter
    (fun (label, input) -> decode_verdict world.tool label input)
    [
      ("unknown", json_object [ ("extra", Json.bool true) ]);
      ("duplicate unknown", duplicate_unknown);
      ("null", Json.null ());
      ("array", Json.list []);
      ("string", Json.string "");
      ("number", Json.int 0);
      ("boolean", Json.bool false);
    ];
  (match decode_call_result world.tool duplicate_unknown with
  | Ok _ -> print_endline "duplicate-specific: false"
  | Error error ->
      Printf.printf "duplicate-specific: %b\n"
        (contains
           (Tool.Call.Decode_error.message error)
           "duplicate input member extra"));
  (match Tool.Call.decode world.tool (model_call ~name:"wrong" empty_input) with
  | Ok _ -> print_endline "wrong name: accepted"
  | Error error ->
      Printf.printf "wrong name: rejected diagnostic=%b\n"
        (not (String.is_empty (Tool.Call.Decode_error.message error))));
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    name: ocaml_dune_describe
    schema: {"type":"object","properties":{},"additionalProperties":false}
    empty: accepted canonical={}
    unknown: rejected diagnostic=true
    duplicate unknown: rejected diagnostic=true
    null: rejected diagnostic=true
    array: rejected diagnostic=true
    string: rejected diagnostic=true
    number: rejected diagnostic=true
    boolean: rejected diagnostic=true
    duplicate-specific: true
    wrong name: rejected diagnostic=true
    invocations: 0
    |}]

let print_permissions call =
  let requests = Tool.Call.permissions call in
  Printf.printf "requests: %d\n" (List.length requests);
  List.iter
    (fun request ->
      Printf.printf "source: %s display: %s\n"
        (Option.value ~default:"none" (Permission.Request.source request))
        (Option.value ~default:"none" (Permission.Request.display request));
      List.iter
        (function
          | Access.Path
              { op; scope = Access.Path_scope.Workspace workspace_path } ->
              Printf.printf "path: op=%s key=%s address=%s\n" (path_op_name op)
                (Workspace.Root.Key.to_string
                   (Workspace.Path.root_key workspace_path))
                (Workspace.Path.display workspace_path)
          | Access.Custom { name; subject } ->
              Printf.printf "custom: name=%s subject=%s\n" name
                (Option.value ~default:"none" subject)
          | access ->
              failf "unexpected Dune-describe permission: %a" Access.pp access)
        (Permission.Request.accesses request))
    requests

let%expect_test "permissions cache the owning-root read and confinement fact" =
  with_world "permission-direct" @@ fun direct ->
  let call = decode_call direct.tool empty_input in
  print_endline "-- direct --";
  print_permissions call;
  Printf.printf "cached-equal: %b invocations: %d\n"
    (List.equal Permission.Request.equal
       (Tool.Call.permissions call)
       (Tool.Call.permissions call))
    (invocation_count direct);
  with_world ~current:Auxiliary_root "permission-auxiliary" @@ fun auxiliary ->
  print_endline "-- auxiliary --";
  print_permissions (decode_call auxiliary.tool empty_input);
  with_world ~mode:Mentat_config.Mode.External_sandbox "permission-external"
  @@ fun external_ ->
  print_endline "-- external --";
  print_permissions (decode_call external_.tool empty_input);
  [%expect
    {|
    -- direct --
    requests: 1
    source: ocaml_dune_describe display: none
    path: op=read key=main address=.
    custom: name=command.confinement subject=direct
    cached-equal: true invocations: 0
    -- auxiliary --
    requests: 1
    source: ocaml_dune_describe display: none
    path: op=read key=aux address=.
    custom: name=command.confinement subject=direct
    -- external --
    requests: 1
    source: ocaml_dune_describe display: none
    path: op=read key=main address=.
    custom: name=command.confinement subject=external
    |}]

let%expect_test
    "real children receive fixed argv, owning-root cwd, and no secrets" =
  with_world ~current:Primary_nested "transport" @@ fun world ->
  set_simple_responses world;
  set_stderr world 1 "ignored workspace warning\n";
  set_stderr world 2 "ignored tests warning\n";
  let result = run world empty_input in
  print_status result;
  print_project_summary result;
  Printf.printf "text:\n%s\n" (Tool.Output.text (output_exn result));
  Printf.printf "invocations: %d\n" (invocation_count world);
  List.iter
    (fun index ->
      Printf.printf "cwd-%d: %s\nargv-%d: %S\nenv-%d: %S\n" index
        (normalize world (String.trim (invocation world "cwd" index)))
        index
        (invocation world "argv" index)
        index
        (invocation world "env" index))
    [ 1; 2 ];
  Printf.printf "stderr-leaked: %b\n"
    (contains (Tool.Output.text (output_exn result)) "ignored");
  [%expect
    {|
    status: completed
    json: {"version":1,"components":1,"tests":1}
    components: 1 tests: 1 truncated: false
    text:
    OCaml Dune project
    root: .
    build_context: _build/default
    components: 1 local=1 external=0
    tests: 1

    local components:
    - local library fixture (id=library:fixture)

    tests:
    - fixture_test: test/fixture_test.exe in lib (enabled)
    invocations: 2
    cwd-1: <primary>
    argv-1: "describe\nworkspace\n--root\n.\n--with-deps\n"
    env-1: "secret=unset\nhome_is_tmp=no\nterm=dumb\nno_color=1\n"
    cwd-2: <primary>
    argv-2: "describe\ntests\n--root\n.\n"
    env-2: "secret=unset\nhome_is_tmp=no\nterm=dumb\nno_color=1\n"
    stderr-leaked: false
    |}]

let%expect_test
    "a real Dune child describes the nested capability root, not its parent" =
  with_world ~current:Primary_nested ~program:[ "dune" ] "real-dune"
  @@ fun world ->
  let parent = Filename.dirname world.primary_dir in
  write_real_project parent ~name:"outer_fixture" ~with_test:false;
  write_real_project world.primary_dir ~name:"inner_fixture" ~with_test:true;
  let result = run world empty_input in
  print_status result;
  print_project_summary result;
  let text = Tool.Output.text (output_exn result) in
  Printf.printf "root-relative: %b inner: %b outer: %b\n"
    (contains text "root: .")
    (contains text "inner_fixture")
    (contains text "outer_fixture");
  [%expect
    {|
    status: completed
    json: {"version":1,"components":1,"tests":1}
    components: 1 tests: 1 truncated: false
    root-relative: true inner: true outer: false
    |}]

let%expect_test "the current auxiliary root owns command and decoded paths" =
  with_world ~current:Auxiliary_root "auxiliary" @@ fun world ->
  set_simple_responses world;
  let result = run world empty_input in
  print_status result;
  print_project_summary result;
  Printf.printf "cwd: %s\nroot-line: %s\n"
    (normalize world (String.trim (invocation world "cwd" 1)))
    (Tool.Output.text (output_exn result)
    |> String.split_on_char '\n'
    |> List.find (String.starts_with ~prefix:"root:"));
  [%expect
    {|
    status: completed
    json: {"version":1,"components":1,"tests":1}
    components: 1 tests: 1 truncated: false
    cwd: <auxiliary>
    root-line: root: .
    |}]

let%expect_test "all component kinds preserve order in text and compact JSON" =
  with_world "rich" @@ fun world ->
  set_response world 1 (rich_workspace world.outside_dir);
  set_response world 2 rich_tests;
  let result = run world empty_input in
  print_status result;
  print_project_summary result;
  Printf.printf "text:\n%s\n" (Tool.Output.text (output_exn result));
  [%expect
    {|
    status: completed
    json: {"version":1,"components":5,"tests":2}
    components: 5 tests: 2 truncated: false
    text:
    OCaml Dune project
    root: .
    build_context: _build/default
    components: 5 local=4 external=1
    tests: 2

    local components:
    - local library zeta (id=library:zeta)
    - executable cli (id=executable:"main":"bin":cli)
    - executable helper (id=executable:"main":"bin":helper)
    - local library alpha (id=library:alpha)

    tests:
    - disabled_test: test/disabled.exe in lib/alpha (disabled)
    - zeta_test: test/zeta.exe in lib/zeta (enabled)
    |}]

let library_item index =
  Printf.sprintf
    "(library ((name lib%02d) (uid uid%02d) (local true) (source_dir lib/%02d) \
     (requires ()) (modules ())))"
    index index index

let test_item index =
  Printf.sprintf
    "((name test%02d) (source_dir test/%02d) (target test/%02d.exe) (enabled \
     true))"
    index index index

let find_substring ~from needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    if index + needle_length > haystack_length then None
    else if String.equal (String.sub haystack index needle_length) needle then
      Some index
    else loop (index + 1)
  in
  loop from

let positions_in_order text fragments =
  let rec loop offset = function
    | [] -> true
    | fragment :: fragments -> (
        match find_substring ~from:offset fragment text with
        | None -> false
        | Some position -> loop (position + String.length fragment) fragments)
  in
  loop 0 fragments

(* The cap is expressed relative to {!Describe.max_display_items} rather than
   spelled out, so widening it moves the fixture with it instead of leaving a
   case that no longer reaches the boundary it exists to prove. *)
let%expect_test "rendering caps each ordered page and reports text truncation" =
  with_world "bounds" @@ fun world ->
  let cap = Describe.max_display_items in
  let library_name index = Printf.sprintf "lib%02d" index in
  let test_name index = Printf.sprintf "test%02d" index in
  let workspace =
    "((root .) (build_context _build/default) "
    ^ (List.init (cap + 2) library_item |> String.concat " ")
    ^ ")"
  in
  let tests =
    "(" ^ (List.init (cap + 1) test_item |> String.concat " ") ^ ")"
  in
  set_response world 1 workspace;
  set_response world 2 tests;
  let result = run world empty_input in
  let text = Tool.Output.text (output_exn result) in
  print_status result;
  print_project_summary result;
  Printf.printf
    "component-order=%b component-last=%b component-hidden=%b component-more=%b\n"
    (positions_in_order text
       [ library_name 0; library_name (cap / 2); library_name (cap - 1) ])
    (contains text (library_name (cap - 1)))
    (not (contains text (library_name cap)))
    (contains text "- ... 2 more local component(s)");
  Printf.printf "test-order=%b test-last=%b test-hidden=%b test-more=%b\n"
    (positions_in_order text
       [ test_name 0; test_name (cap / 2); test_name (cap - 1) ])
    (contains text (test_name (cap - 1)))
    (not (contains text (test_name cap)))
    (contains text "- ... 1 more test(s)");
  [%expect
    {|
    status: completed
    json: {"version":1,"components":202,"tests":201}
    components: 202 tests: 201 truncated: true
    component-order=true component-last=true component-hidden=true component-more=true
    test-order=true test-last=true test-hidden=true test-more=true
    |}]

let%expect_test "completed summaries are byte-bounded at a UTF-8 boundary" =
  with_world "output-bytes" @@ fun world ->
  let name =
    let buffer = Buffer.create (Describe.max_output_bytes + 16_384) in
    Buffer.add_string buffer "\255caf\195\169-\240\159\152\128";
    let remaining = ref 20_000 in
    while !remaining > 0 do
      Buffer.add_string buffer "\240\159\152\128";
      decr remaining
    done;
    Buffer.contents buffer
  in
  set_response world 1
    (Printf.sprintf
       "((library ((name %s) (uid unicode) (local true) (source_dir lib) \
        (requires ()) (modules ()))))"
       name);
  set_response world 2 "()";
  let result = run world empty_input in
  let output = output_exn result in
  let text = Tool.Output.text output in
  print_status result;
  print_project_summary result;
  Printf.printf "bounded: %b utf8: %b repaired: %b unicode-prefix: %b\n"
    (String.length text <= Describe.max_output_bytes)
    (String.is_valid_utf_8 text)
    (contains text "\239\191\189")
    (contains text "caf\195\169-\240\159\152\128");
  [%expect
    {|
    status: completed
    json: {"version":1,"components":1,"tests":0}
    components: 1 tests: 0 truncated: true
    bounded: true utf8: true repaired: true unicode-prefix: true
    |}]

let%expect_test "an empty Dune workspace remains a completed empty project" =
  with_world "empty" @@ fun world ->
  set_response world 1 "()";
  set_response world 2 "()";
  let result = run world empty_input in
  print_status result;
  print_project_summary result;
  Printf.printf "text:\n%s\n" (Tool.Output.text (output_exn result));
  [%expect
    {|
    status: completed
    json: {"version":1,"components":0,"tests":0}
    components: 0 tests: 0 truncated: false
    text:
    OCaml Dune project
    root: <unknown>
    build_context: <unknown>
    components: 0 local=0 external=0
    tests: 0
    |}]

let run_protocol_case label workspace_output tests_output needle =
  with_world ("protocol-" ^ label) @@ fun world ->
  set_response world 1 workspace_output;
  set_response world 2 tests_output;
  let result = run world empty_input in
  match Tool.Result.status result with
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf
        "%s: kind=%s needle=%b bounded=%b utf8=%b metadata=%b output=%b \
         invocations=%d\n"
        label (failure_name kind) (contains message needle)
        (String.length message <= Describe.max_detail_bytes)
        (String.is_valid_utf_8 message)
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output result))
        (invocation_count world)
  | Tool.Result.Completed -> Printf.printf "%s: unexpectedly completed\n" label
  | Tool.Result.Interrupted _ ->
      Printf.printf "%s: unexpectedly interrupted\n" label

let duplicate_uid_workspace =
  {|
(
 (library ((name one) (uid duplicate) (local true) (source_dir one) (requires ()) (modules ())))
 (library ((name two) (uid duplicate) (local true) (source_dir two) (requires ()) (modules ())))
)|}

let duplicate_component_workspace =
  {|
(
 (library ((name same) (uid one) (local true) (source_dir one) (requires ()) (modules ())))
 (library ((name same) (uid two) (local true) (source_dir two) (requires ()) (modules ())))
)|}

let duplicate_requires_workspace =
  {|
(
 (library ((name dependency) (uid dep) (local false) (source_dir /outside) (requires ()) (modules ())))
 (library ((name local) (uid local) (local true) (source_dir local) (requires (dep dep)) (modules ())))
)|}

let unknown_requires_workspace =
  {|
(
 (library ((name local) (uid local) (local true) (source_dir local) (requires (missing)) (modules ())))
)|}

let duplicate_record_field_workspace =
  {|
(
 (library
  ((name first)
   (name second)
   (uid local)
   (local true)
   (source_dir local)
   (requires ())
   (modules ())))
)|}

let duplicate_root_workspace = {|
(
 (root .)
 (root other)
)|}

let duplicate_build_context_workspace =
  {|
(
 (build_context _build/default)
 (build_context _build/other)
)|}

let duplicate_test_field =
  {|
(
 ((name first)
  (name second)
  (source_dir lib)
  (target test/fixture_test.exe)
  (enabled true))
)|}

let%expect_test
    "malformed and duplicate protocol data fail without partial output" =
  run_protocol_case "workspace-atom" "not-a-list" "()" "expected item list";
  run_protocol_case "unknown-item" "((mystery value))" "()"
    "unknown workspace item";
  run_protocol_case "missing-field"
    "((library ((name local) (uid local) (local true))))" "()"
    "missing field source_dir";
  run_protocol_case "duplicate-uid" duplicate_uid_workspace "()"
    "duplicate Dune library uid";
  run_protocol_case "duplicate-component" duplicate_component_workspace "()"
    "components must not contain duplicate ids";
  run_protocol_case "duplicate-requires" duplicate_requires_workspace "()"
    "requires must not contain duplicate ids";
  run_protocol_case "unknown-requires" unknown_requires_workspace "()"
    "unknown Dune library uid";
  run_protocol_case "duplicate-record-field" duplicate_record_field_workspace
    "()" "duplicate record field name";
  run_protocol_case "duplicate-root" duplicate_root_workspace "()"
    "duplicate workspace item root";
  run_protocol_case "duplicate-build-context" duplicate_build_context_workspace
    "()" "duplicate workspace item build_context";
  run_protocol_case "duplicate-test-field" simple_workspace duplicate_test_field
    "duplicate record field name";
  run_protocol_case "tests-atom" simple_workspace "not-a-list"
    "expected test list";
  [%expect
    {|
    workspace-atom: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    unknown-item: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    missing-field: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    duplicate-uid: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    duplicate-component: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    duplicate-requires: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    unknown-requires: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    duplicate-record-field: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    duplicate-root: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    duplicate-build-context: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    duplicate-test-field: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    tests-atom: kind=failed needle=true bounded=true utf8=true metadata=false output=false invocations=2
    |}]

let%expect_test
    "full capture succeeds beyond 64KiB and diagnostics stay bounded" =
  with_world "full-capture" @@ fun full ->
  set_behavior full 1 "full_empty";
  set_response full 2 "()";
  let full_result = run full empty_input in
  print_endline "-- full capture --";
  print_status full_result;
  print_project_summary full_result;
  Printf.printf "invocations: %d\n" (invocation_count full);
  with_world "large-stderr" @@ fun stderr_world ->
  set_behavior stderr_world 1 "large_error";
  let stderr_result = run stderr_world empty_input in
  print_endline "-- large invalid stderr --";
  (match Tool.Result.status stderr_result with
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf
        "kind=%s bytes=%d bounded=%b utf8=%b ansi=%b replacement=%b \
         metadata=%b output=%b\n"
        (failure_name kind) (String.length message)
        (String.length message <= Describe.max_detail_bytes)
        (String.is_valid_utf_8 message)
        (String.contains message '\x1b')
        (contains message "\239\191\189")
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output stderr_result))
  | Tool.Result.Completed -> print_endline "unexpected completion"
  | Tool.Result.Interrupted _ -> print_endline "unexpected interruption");
  with_world "large-stdout" @@ fun stdout_world ->
  set_behavior stdout_world 1 "malformed_large";
  set_response stdout_world 2 "()";
  let stdout_result = run stdout_world empty_input in
  print_endline "-- large malformed stdout --";
  (match Tool.Result.status stdout_result with
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf
        "kind=%s bytes=%d bounded=%b utf8=%b ansi=%b replacement=%b \
         metadata=%b output=%b invocations=%d\n"
        (failure_name kind) (String.length message)
        (String.length message <= Describe.max_detail_bytes)
        (String.is_valid_utf_8 message)
        (String.contains message '\x1b')
        (contains message "\239\191\189")
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output stdout_result))
        (invocation_count stdout_world)
  | Tool.Result.Completed -> print_endline "unexpected completion"
  | Tool.Result.Interrupted _ -> print_endline "unexpected interruption");
  [%expect
    {|
    -- full capture --
    status: completed
    json: {"version":1,"components":0,"tests":0}
    components: 0 tests: 0 truncated: false
    invocations: 2
    -- large invalid stderr --
    kind=failed bytes=4096 bounded=true utf8=true ansi=false replacement=true metadata=false output=false
    -- large malformed stdout --
    kind=failed bytes=4096 bounded=true utf8=true ansi=false replacement=true metadata=false output=false invocations=2
    |}]

let terminal_case label first_behavior second_behavior =
  with_world ("terminal-" ^ label) @@ fun world ->
  set_simple_responses world;
  Option.iter (set_behavior world 1) first_behavior;
  Option.iter (set_behavior world 2) second_behavior;
  let result = run world empty_input in
  match Tool.Result.status result with
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf
        "%s: kind=%s diagnostic=%b utf8=%b metadata=%b output=%b invocations=%d\n"
        label (failure_name kind)
        (not (String.is_empty message))
        (String.is_valid_utf_8 message)
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output result))
        (invocation_count world)
  | Tool.Result.Completed -> Printf.printf "%s: unexpectedly completed\n" label
  | Tool.Result.Interrupted _ ->
      Printf.printf "%s: unexpectedly interrupted\n" label

let%expect_test
    "exit signal spawn and incomplete capture have distinct failures" =
  terminal_case "workspace-exit" (Some "exit7") None;
  terminal_case "tests-exit" None (Some "exit7");
  terminal_case "tests-signal" None (Some "signal");
  terminal_case "second-spawn" (Some "remove_self") None;
  terminal_case "workspace-incomplete" (Some "incomplete") None;
  terminal_case "tests-incomplete" None (Some "incomplete");
  [%expect
    {|
    workspace-exit: kind=failed diagnostic=true utf8=true metadata=false output=false invocations=1
    tests-exit: kind=failed diagnostic=true utf8=true metadata=false output=false invocations=2
    tests-signal: kind=failed diagnostic=true utf8=true metadata=false output=false invocations=2
    second-spawn: kind=unavailable diagnostic=true utf8=true metadata=false output=false invocations=1
    workspace-incomplete: kind=failed diagnostic=true utf8=true metadata=false output=false invocations=1
    tests-incomplete: kind=failed diagnostic=true utf8=true metadata=false output=false invocations=2
    |}]

let%expect_test "each Dune command has an independent thirty-second timeout" =
  let timeout_case label index =
    let clock = Eio_mock.Clock.Mono.make () in
    Eio_mock.Clock.Mono.set_time clock (Mtime.of_uint64_ns 0L);
    with_world ~clock ("timeout-" ^ label) @@ fun world ->
    set_simple_responses world;
    set_behavior world index "sleep";
    let rec drive () =
      Eio.Fiber.yield ();
      if started world index && Eio_mock.Clock.Mono.try_advance clock then
        `Stop_daemon
      else drive ()
    in
    Eio.Fiber.fork_daemon ~sw:world.sw drive;
    let result = run world empty_input in
    Printf.printf "-- %s --\n" label;
    print_status result;
    Printf.printf "output: %b invocations: %d\n"
      (Option.is_some (Tool.Result.output result))
      (invocation_count world)
  in
  timeout_case "workspace" 1;
  timeout_case "tests" 2;
  [%expect
    {|
    +mock time is now 0
    +mock time is now 30
    +mock time is now 0
    +mock time is now 30
    -- workspace --
    status: failed timed_out
    message: dune describe workspace timed out after 30000ms
    metadata: false
    output: false invocations: 1
    -- tests --
    status: failed timed_out
    message: dune describe tests timed out after 30000ms
    metadata: false
    output: false invocations: 2
    |}]

let print_cancelled label world result =
  Printf.printf "-- %s --\n" label;
  print_status result;
  Printf.printf "output: %b invocations: %d\n"
    (Option.is_some (Tool.Result.output result))
    (invocation_count world)

let%expect_test "cancellation is terminal before between and within commands" =
  with_world "cancel-before" @@ fun before ->
  let before_result = run ~cancelled:(fun () -> true) before empty_input in
  print_cancelled "before" before before_result;
  with_world "cancel-between" @@ fun between ->
  set_simple_responses between;
  write_disk (plan_file between "cancel-after" 1) "";
  let between_result =
    run
      ~cancelled:(fun () ->
        Sys.file_exists (Filename.concat between.plan_dir "cancel-now"))
      between empty_input
  in
  print_cancelled "between" between between_result;
  with_world "cancel-after-tests" @@ fun after_tests ->
  set_simple_responses after_tests;
  write_disk (plan_file after_tests "cancel-after" 2) "";
  let after_tests_result =
    run
      ~cancelled:(fun () ->
        Sys.file_exists (Filename.concat after_tests.plan_dir "cancel-now"))
      after_tests empty_input
  in
  print_cancelled "after tests" after_tests after_tests_result;
  with_world "cancel-running-workspace" @@ fun workspace_world ->
  set_behavior workspace_world 1 "sleep";
  let workspace_deadline = ref None in
  let cancel_workspace () =
    if not (started workspace_world 1) then false
    else
      match !workspace_deadline with
      | None ->
          workspace_deadline := Some (Unix.gettimeofday () +. 0.1);
          false
      | Some deadline -> Unix.gettimeofday () >= deadline
  in
  let workspace_result =
    run ~cancelled:cancel_workspace workspace_world empty_input
  in
  print_cancelled "workspace running" workspace_world workspace_result;
  with_world "cancel-running-tests" @@ fun tests_world ->
  set_simple_responses tests_world;
  set_behavior tests_world 2 "sleep";
  let deadline = ref None in
  let cancel_tests () =
    if not (started tests_world 2) then false
    else
      match !deadline with
      | None ->
          deadline := Some (Unix.gettimeofday () +. 0.1);
          false
      | Some deadline -> Unix.gettimeofday () >= deadline
  in
  let tests_result = run ~cancelled:cancel_tests tests_world empty_input in
  print_cancelled "tests running" tests_world tests_result;
  [%expect
    {|
    -- before --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false invocations: 0
    -- between --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false invocations: 1
    -- after tests --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false invocations: 2
    -- workspace running --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false invocations: 1
    -- tests running --
    status: interrupted cancelled=true
    reason: tool call cancelled
    output: false invocations: 2
    |}]

let%expect_test "a replaced owning root is refused before a useful child" =
  with_world "root-race" @@ fun world ->
  let displaced = world.primary_dir ^ "-displaced" in
  Unix.rename world.primary_dir displaced;
  mkdir world.primary_dir;
  let result = run world empty_input in
  print_status ~normalize:(normalize world) result;
  let old_plan = Filename.concat displaced ".describe-plan" in
  Printf.printf "output: %b old-invocations: %d\n"
    (Option.is_some (Tool.Result.output result))
    (invocation_count_at old_plan);
  [%expect
    {|
    status: failed unavailable
    message: dune describe workspace launch failed: resolved sandbox path <primary> disappeared or changed kind after sealing
    metadata: false
    output: false old-invocations: 0
    |}]

let codec_verdict label json =
  match Json.decode Project_output.jsont json with
  | Ok project ->
      Printf.printf "%s: accepted components=%d tests=%d\n" label
        (Project_output.components project)
        (Project_output.tests project)
  | Error diagnostic ->
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty diagnostic))

let%expect_test "durable replay is closed, versioned, and carries no authority"
    =
  with_world "durable" @@ fun world ->
  set_simple_responses world;
  let result = run world empty_input in
  let stored = encode result_codec result in
  let replay = decode result_codec stored in
  let stored_again = encode result_codec replay in
  let original_output = output_exn result in
  let replay_output = output_exn replay in
  Printf.printf "roundtrip: %b output-equal: %b text-valid: %b\n"
    (Json.equal stored stored_again)
    (Tool.Output.equal original_output replay_output)
    (String.is_valid_utf_8 (Tool.Output.text replay_output));
  let forbidden =
    [
      "receipt";
      "checkpoint";
      "mutation";
      "claim";
      "handle";
      "capability";
      "sandbox";
      "permissions";
      "argv";
      "cwd";
      "environment";
      "program";
      "path";
      "component_name";
    ]
  in
  let leaked =
    json_member_names stored
    |> List.filter (fun name -> List.mem name forbidden)
  in
  let replay_project = project_exn replay in
  Printf.printf "components=%d tests=%d authority-fields=%d invocations=%d\n"
    (Project_output.components replay_project)
    (Project_output.tests replay_project)
    (List.length leaked) (invocation_count world);
  let stored_text = json_string stored in
  Printf.printf "physical-leak=%b secret-leak=%b\n"
    (contains stored_text world.primary_dir
    || contains stored_text world.outside_dir)
    (contains stored_text "ambient-secret");
  codec_verdict "future-version"
    (json_object
       [
         ("version", Json.int 2);
         ("components", Json.int 1);
         ("tests", Json.int 1);
       ]);
  codec_verdict "negative"
    (json_object
       [
         ("version", Json.int 1);
         ("components", Json.int (-1));
         ("tests", Json.int 1);
       ]);
  codec_verdict "unknown"
    (json_object
       [
         ("version", Json.int 1);
         ("components", Json.int 1);
         ("tests", Json.int 1);
         ("extra", Json.bool true);
       ]);
  [%expect
    {|
    roundtrip: true output-equal: true text-valid: true
    components=1 tests=1 authority-fields=0 invocations=2
    physical-leak=false secret-leak=false
    future-version: rejected diagnostic=true
    negative: rejected diagnostic=true
    unknown: rejected diagnostic=true
    |}]

let%expect_test "constructor rejects invalid immutable program prefixes" =
  with_world "constructor" @@ fun world ->
  let clock = Eio_mock.Clock.Mono.make () in
  List.iter
    (fun (label, program) ->
      let raised =
        match Describe.make world.io ~clock ~program with
        | _ -> false
        | exception Invalid_argument diagnostic ->
            Printf.printf "%s: %s\n" label diagnostic;
            true
      in
      Printf.printf "%s-raised: %b\n" label raised)
    [
      ("empty", []);
      ("empty-first", [ "" ]);
      ("empty-later", [ "dune"; "" ]);
      ("NUL", [ "du\x00ne" ]);
    ];
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    empty: Ocaml.Dune_describe.make: program prefix must not be empty
    empty-raised: true
    empty-first: Ocaml.Dune_describe.make: program token 0 must not be empty
    empty-first-raised: true
    empty-later: Ocaml.Dune_describe.make: program token 1 must not be empty
    empty-later-raised: true
    NUL: Ocaml.Dune_describe.make: program token 0 must not contain NUL
    NUL-raised: true
    invocations: 0
    |}]
