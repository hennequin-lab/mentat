(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The applied-within-review invariant, checked. For each write tool
   (write_file, edit_file, apply_patch) every path an apply actually commits is
   a path the tool's [permissions] already declared for the same input. Both
   sets derive from the same resolved input by construction, so the invariant
   holds silently today; this suite converts that honesty assumption into an
   engine-driven checked property.

   Applied paths are the ones an apply attributes natively: a successful apply's
   committed targets and a failed apply's committed prefix plus its uncertain
   commit target. Observationally attributed paths ([Apply_evidence.observed],
   opaque writers) are command-reviewed, not path-reviewed, and are deliberately
   excluded — including them would false-positive the check. The first test
   proves the checker is non-vacuous: it flags an applied path outside review
   and, in the same breath, refuses to flag an out-of-review observed path. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Fs = Mentat_tools.Fs
module Edit = Mentat_edit
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission

(* The applied side of the invariant: the workspace paths an evidence window
   attributes natively, [observed] deliberately excluded. *)
let applied_paths (evidence : Edit.Apply_evidence.t) =
  List.concat_map
    (function
      | Edit.Apply_evidence.Applied result ->
          List.map Edit.Result.Entry.target_path (Edit.Result.entries result)
      | Edit.Apply_evidence.Attempted
          { Edit.Apply_evidence.Attempt.applied; uncertain } ->
          List.map Edit.Result.Entry.target_path applied
          @ Option.to_list uncertain)
    evidence.Edit.Apply_evidence.applies

(* The reviewed side: the workspace paths a call's permissions declared as
   filesystem accesses, matched through the private access variant (in-repo
   tests may match private variants, not construct them). *)
let reviewed_paths call =
  Tool.Call.permissions call
  |> List.concat_map Permission.Request.accesses
  |> List.filter_map (function
    | Permission.Access.Path
        { scope = Permission.Access.Path_scope.Workspace path; _ } ->
        Some path
    | _ -> None)
  |> Workspace.Path.Set.of_list

(* The applied paths that escape review: empty iff applied ⊆ reviewed. Set
   membership relies on [Workspace.Path] identity aligning between the reviewed
   access path and the evidence path; the engine-driven test below confirms the
   alignment empirically (a mismatch would leave a real applied path reported as
   escaping). *)
let paths_outside_review ~reviewed applied =
  List.filter (fun path -> not (Workspace.Path.Set.mem path reviewed)) applied

let display_paths paths =
  match List.map Workspace.Path.display paths |> List.sort String.compare with
  | [] -> "<none>"
  | displays -> String.concat ", " displays

(* Synthetic paths in one root key, for the non-vacuous checker cases. *)
let synthetic name = Workspace.Path.make ~root_key:(root_key "main") (rel name)

let%expect_test "the checker flags applied escapes and ignores observed paths" =
  let reviewed = Workspace.Path.Set.singleton (synthetic "reviewed.txt") in
  let escaped = synthetic "escaped.txt" in
  (* An applied path outside review must be flagged. It is carried as an
     [Attempted] window's uncertain commit target — a member of the applied set
     the invariant covers — with no confirmed prefix. *)
  let escaping_evidence =
    {
      Edit.Apply_evidence.applies =
        [
          Edit.Apply_evidence.Attempted
            {
              Edit.Apply_evidence.Attempt.applied = [];
              uncertain = Some escaped;
            };
        ];
      observed = [];
    }
  in
  let escaping_applied = applied_paths escaping_evidence in
  print_case "applied path outside review";
  Printf.printf "applied: %s\n" (display_paths escaping_applied);
  Printf.printf "outside review: %s\n"
    (display_paths (paths_outside_review ~reviewed escaping_applied));
  (* The same out-of-review path, this time only observationally attributed,
     must NOT be flagged: observed paths are command-reviewed, not path-reviewed,
     so the applied set omits them and the checker stays silent. *)
  let observed_evidence =
    { Edit.Apply_evidence.applies = []; observed = [ escaped ] }
  in
  let observed_applied = applied_paths observed_evidence in
  print_case "observed path outside review";
  Printf.printf "applied: %s\n" (display_paths observed_applied);
  Printf.printf "outside review: %s\n"
    (display_paths (paths_outside_review ~reviewed observed_applied));
  [%expect
    {|
    -- applied path outside review --
    applied: escaped.txt
    outside review: escaped.txt
    -- observed path outside review --
    applied: <none>
    outside review: <none> |}]

(* Engine-driven representative assertion. Each case drives a fixed known input
   through the real tool: [Call.decode] computes the cached permissions, an open
   claim scope brackets [Call.run], and [Claim_scope.close] yields the apply
   evidence the engine would lower into the store. *)

let abs path = Lpath.Abs.of_string_exn path

let write_disk path contents =
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let with_env name value fn =
  let saved = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect
    ~finally:(fun () ->
      match saved with
      | Some value -> Unix.putenv name value
      | None -> Unix.unsetenv name)
    fn

type world = { io : Wio.t }

let with_world fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let raw = Filename.temp_dir "mentat-applied-review-" "" in
  Fun.protect ~finally:(fun () -> remove_tree raw) @@ fun () ->
  let base = Unix.realpath raw in
  let ws_dir = Filename.concat base "workspace" in
  let tmp_dir = Filename.concat base "tmp" in
  List.iter mkdir [ ws_dir; tmp_dir ];
  let file name contents = write_disk (Filename.concat ws_dir name) contents in
  file "note.txt" "alpha\n";
  file "essay.txt" "alpha\nbeta\n";
  file "poem.txt" "one\ntwo\nthree\n";
  file "stale.txt" "remove\n";
  file "movable.txt" "old\nkeep\n";
  let logical = Workspace.single (Workspace.Root.of_dir (abs ws_dir)) in
  with_env "TMPDIR" tmp_dir @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical () in
  fn { io }

let identity contents =
  Mentat_digest.Content_ref.to_token
    (Mentat_digest.Content_ref.of_contents contents)

let decode_call tool provider_input =
  match Tool.Call.decode tool (model_call tool provider_input) with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let status_name result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> "completed"
  | Tool.Result.Failed { kind; _ } -> "failed " ^ failure_name kind
  | Tool.Result.Interrupted _ -> "interrupted"

let check_case world label tool provider_input =
  let call = decode_call tool provider_input in
  let reviewed = reviewed_paths call in
  let scope = Wio.open_claim_scope world.io in
  let outcome = Tool.Call.run call ~cancelled:(fun () -> false) in
  let evidence = Wio.Claim_scope.close scope in
  let result = finished outcome in
  let applied = applied_paths evidence in
  let outside = paths_outside_review ~reviewed applied in
  print_case label;
  Printf.printf "status: %s\n" (status_name result);
  Printf.printf "reviewed: %s\n"
    (display_paths (Workspace.Path.Set.elements reviewed));
  Printf.printf "applied: %s\n" (display_paths applied);
  Printf.printf "applied within review: %b\n" (List.is_empty outside)

let write_file_input ?if_identity path contents =
  let fields =
    [ ("path", Json.string path); ("contents", Json.string contents) ]
  in
  let fields =
    match if_identity with
    | None -> fields
    | Some token -> fields @ [ ("if_identity", Json.string token) ]
  in
  json_object fields

let edit_file_input path ~old_string ~new_string =
  json_object
    [
      ("path", Json.string path);
      ("old_string", Json.string old_string);
      ("new_string", Json.string new_string);
    ]

let patch_document lines =
  json_object
    [
      ( "patch",
        Json.string
          (String.concat "\n"
             (("*** Begin Patch" :: lines) @ [ "*** End Patch" ])) );
    ]

let%expect_test "write tools apply only paths their permissions declared" =
  with_world @@ fun world ->
  check_case world "write_file create"
    (Fs.Write_file.make world.io)
    (write_file_input "created.txt" "hello\nworld\n");
  check_case world "write_file modify"
    (Fs.Write_file.make world.io)
    (write_file_input ~if_identity:(identity "alpha\n") "note.txt" "bravo\n");
  check_case world "edit_file modify"
    (Fs.Edit_file.make world.io)
    (edit_file_input "essay.txt" ~old_string:"alpha" ~new_string:"gamma");
  check_case world "apply_patch mixed operations"
    (Fs.Apply_patch.make world.io)
    (patch_document
       [
         "*** Add File: added.txt";
         "+one";
         "+two";
         "*** Update File: poem.txt";
         "@@";
         "-two";
         "+TWO";
         "*** Delete File: stale.txt";
         "*** Update File: movable.txt";
         "*** Move to: moved.txt";
         "@@";
         "-old";
         "+OLD";
       ]);
  [%expect
    {|
    -- write_file create --
    status: completed
    reviewed: created.txt
    applied: created.txt
    applied within review: true
    -- write_file modify --
    status: completed
    reviewed: note.txt
    applied: note.txt
    applied within review: true
    -- edit_file modify --
    status: completed
    reviewed: essay.txt
    applied: essay.txt
    applied within review: true
    -- apply_patch mixed operations --
    status: completed
    reviewed: added.txt, movable.txt, moved.txt, poem.txt, stale.txt
    applied: added.txt, movable.txt, moved.txt, poem.txt, stale.txt
    applied within review: true |}]
