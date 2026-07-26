(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
module Ocaml = Mentat_ocaml

let expect_ok msg = function
  | Ok value -> value
  | Error error ->
      failf "%s: %s" msg (Mentat_ocaml_dune_describe.Error.message error)

let workspace () =
  let root =
    Mentat_workspace.Root.of_dir (Lpath.Abs.of_string_exn "/workspace")
  in
  Mentat_workspace.single root

let workspace_output =
  {|
((root /workspace)
 (build_context _build/default)
 (library
  ((name foo)
   (uid uid-foo)
   (local true)
   (requires (uid-unix))
   (source_dir lib)
   (modules
    (((name Foo)
      (impl lib/foo.ml)
      (intf lib/foo.mli)
      (cmt ())
      (cmti ())
      (module_deps ((for_intf (Bar)) (for_impl (Baz)))))))
   (include_dirs (_build/default/lib))))
 (library
  ((name unix)
   (uid uid-unix)
   (local false)
   (requires ())
   (source_dir /outside/toolchain/lib/unix)
   (modules
    (((name Unix)
      (impl /outside/toolchain/lib/unix/unix.ml)
      (intf ())
      (cmt ())
      (cmti ()))))
   (include_dirs ())))
 (executables
  ((names (main))
   (requires (uid-foo))
   (modules
    (((name Main)
      (impl bin/main.ml)
      (intf ())
      (cmt ())
      (cmti ())
      (module_deps ((for_intf ()) (for_impl (Foo)))))))
   (include_dirs (_build/default/bin)))))
|}

let tests_output =
  {|
(((name foo_tests)
  (source_dir lib)
  (package ())
  (enabled true)
  (location lib/dune:1:0)
  (target @lib/runtest)))
|}

let describe_outputs_normalize_project () =
  let workspace = workspace () in
  let project =
    Mentat_ocaml_dune_describe.of_outputs ~workspace ~workspace_output
      ~tests_output
    |> expect_ok "describe outputs"
  in
  equal int ~msg:"components" 3 (List.length (Ocaml.Project.components project));
  equal int ~msg:"tests" 1 (List.length (Ocaml.Project.tests project));
  let external_component =
    Ocaml.Project.components project
    |> List.find_opt (fun component ->
        String.equal (Ocaml.Project.Component.name component) "unix")
  in
  let external_component =
    match external_component with
    | Some component -> component
    | None -> failf "missing external library component"
  in
  equal (option string) ~msg:"external source dir" None
    (Option.map Mentat_workspace.Path.display
       (Ocaml.Project.Component.source_dir external_component));
  begin match Ocaml.Project.Component.units external_component with
  | [ unit_ ] ->
      equal (option string) ~msg:"external unit impl" None
        (Option.map Mentat_workspace.Path.display
           (Ocaml.Project.Compilation_unit.impl unit_))
  | units -> failf "unexpected external unit count %d" (List.length units)
  end;
  begin match Ocaml.Project.build_context project with
  | Some "_build/default" -> ()
  | Some build_context -> failf "unexpected build context %s" build_context
  | None -> failf "missing build context"
  end;
  let lib_id = Ocaml.Project.Component.Id.library "foo" in
  begin match Ocaml.Project.dependencies project lib_id with
  | Some (Ocaml.Project.Deps.Known [ dep ]) ->
      equal string ~msg:"library dependency" "unix"
        (Ocaml.Project.Component.name dep)
  | None | Some (Ocaml.Project.Deps.Unknown | Ocaml.Project.Deps.Known _) ->
      failf "unexpected library dependencies"
  end;
  let exe_id =
    Ocaml.Project.Component.Id.executable
      ~dir:
        (Mentat_workspace.Path.make
           ~root_key:
             (Mentat_workspace.Root.key (Mentat_workspace.primary workspace))
           (Lpath.Rel.of_string_exn "bin"))
      ~name:"main"
  in
  begin match Ocaml.Project.dependencies project exe_id with
  | Some (Ocaml.Project.Deps.Known [ dep ]) ->
      equal string ~msg:"executable dependency" "foo"
        (Ocaml.Project.Component.name dep)
  | None | Some (Ocaml.Project.Deps.Unknown | Ocaml.Project.Deps.Known _) ->
      failf "unexpected executable dependencies"
  end;
  let test = List.hd (Ocaml.Project.tests project) in
  equal string ~msg:"test target" "@lib/runtest"
    (Ocaml.Project.Test.target test);
  equal (option string) ~msg:"test component" (Some "library:foo")
    (Option.map Ocaml.Project.Component.Id.to_string
       (Ocaml.Project.Test.component test))

let malformed_workspace_data_returns_parse_error () =
  let workspace = workspace () in
  let output =
    {|
((root /workspace)
 (library
  ((name "")
   (uid uid-empty)
   (local true)
   (requires ())
   (source_dir lib)
   (modules ()))))
|}
  in
  match Mentat_ocaml_dune_describe.of_workspace_output ~workspace output with
  | Error
      (Mentat_ocaml_dune_describe.Error.Parse_error
         { source = Mentat_ocaml_dune_describe.Error.Workspace_describe; _ }) ->
      ()
  | Error error ->
      failf "unexpected error: %s"
        (Mentat_ocaml_dune_describe.Error.message error)
  | Ok _ -> failf "expected malformed workspace data to fail"

let malformed_module_name_returns_parse_error () =
  let workspace = workspace () in
  let output =
    {|
((root /workspace)
 (library
  ((name foo)
   (uid uid-foo)
   (local true)
   (requires ())
   (source_dir lib)
   (modules
    (((name lowercase)
      (impl lib/foo.ml)
      (intf ())
      (cmt ())
      (cmti ())))))))
|}
  in
  match Mentat_ocaml_dune_describe.of_workspace_output ~workspace output with
  | Error
      (Mentat_ocaml_dune_describe.Error.Parse_error
         { source = Mentat_ocaml_dune_describe.Error.Workspace_describe; _ }) ->
      ()
  | Error error ->
      failf "unexpected error: %s"
        (Mentat_ocaml_dune_describe.Error.message error)
  | Ok _ -> failf "expected malformed module name to fail"

let malformed_tests_data_returns_parse_error () =
  let workspace = workspace () in
  let project =
    Mentat_ocaml_dune_describe.of_workspace_output ~workspace workspace_output
    |> expect_ok "describe workspace output"
  in
  let output =
    {|
(((name "")
  (source_dir lib)
  (package ())
  (enabled true)
  (location lib/dune:1:0)
  (target @lib/runtest)))
|}
  in
  match
    Mentat_ocaml_dune_describe.of_tests_output ~workspace project output
  with
  | Error
      (Mentat_ocaml_dune_describe.Error.Parse_error
         { source = Mentat_ocaml_dune_describe.Error.Tests_describe; _ }) ->
      ()
  | Error error ->
      failf "unexpected error: %s"
        (Mentat_ocaml_dune_describe.Error.message error)
  | Ok _ -> failf "expected malformed tests data to fail"

let () =
  run "mentat.ocaml.dune_describe"
    [
      test "describe outputs normalize project"
        describe_outputs_normalize_project;
      test "malformed workspace data returns parse error"
        malformed_workspace_data_returns_parse_error;
      test "malformed module name returns parse error"
        malformed_module_name_returns_parse_error;
      test "malformed tests data returns parse error"
        malformed_tests_data_returns_parse_error;
    ]
