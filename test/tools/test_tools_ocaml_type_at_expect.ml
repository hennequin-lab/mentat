(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Public-boundary integration tests for Merlin type query. Every
   effectful case crosses provider JSON decoding, cached permissions, the erased
   result boundary, a real workspace capability, and a real child process. The
   scripted child is a protocol peer rather than a private-function fake: it
   records the physical cwd, argv, and exact stdin received through
   [Workspace_io.Command], then returns one response selected by call number. *)

open Windtrap
open Test_tools_support
module Json = Jsont.Json
module Tool = Mentat_tool
module Type_at = Mentat_tools.Ocaml.Type_at
module Wio = Mentat_workspace_io
module Workspace = Mentat_workspace
module Permission = Mentat_permission
module Access = Permission.Access

type cwd = Primary_root | Primary_logical | Auxiliary_root

type world = {
  sw : Eio.Switch.t;
  ws_dir : string;
  aux_dir : string;
  outside_dir : string;
  plan_dir : string;
  io : Wio.t;
  tool : Tool.t;
}

let add_optional name encode value fields =
  match value with
  | None -> fields
  | Some value -> (name, encode value) :: fields

let input ?max_enclosings ?verbosity ?documentation ~path ~line ~column () =
  [
    ("path", Json.string path);
    ("line", Json.int line);
    ("column", Json.int column);
  ]
  |> add_optional "max_enclosings" (fun value -> Json.int value) max_enclosings
  |> add_optional "verbosity" (fun value -> Json.int value) verbosity
  |> add_optional "documentation" (fun value -> Json.bool value) documentation
  |> List.rev |> json_object

let semantic_exn result =
  match
    Mentat_tools_output.Codec.decode Mentat_tools_output.Ocaml.Type_at.jsont
      (output_exn result)
  with
  | Some semantic -> semantic
  | None -> fail "type-at output carried no type semantics"

let line_with prefix result =
  Tool.Output.text (output_exn result)
  |> String.split_on_char '\n'
  |> List.find_opt (String.starts_with ~prefix)
  |> Option.value ~default:("<missing " ^ prefix ^ ">")

let print_status ?(normalize = Fun.id) result =
  match Tool.Result.status result with
  | Tool.Result.Completed -> print_endline "status: completed"
  | Tool.Result.Failed { kind; message; metadata } ->
      Printf.printf "status: failed %s\nmessage: %s\nmetadata: %b\n"
        (failure_name kind) (normalize message) (Option.is_some metadata)
  | Tool.Result.Interrupted { reason; cancelled } ->
      Printf.printf "status: interrupted cancelled=%b\nreason: %s\n" cancelled
        reason

let decode_call_result tool provider_input =
  Tool.Call.decode tool (model_call tool provider_input)

let decode_call tool provider_input =
  match decode_call_result tool provider_input with
  | Ok call -> call
  | Error error ->
      failf "call decode failed: %s" (Tool.Call.Decode_error.message error)

let run ?(cancelled = fun () -> false) world provider_input =
  Tool.Call.run (decode_call world.tool provider_input) ~cancelled |> finished

let abs path = Lpath.Abs.of_string_exn path

let write_disk path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel contents)

let workspace ~cwd ws_dir aux_dir =
  let primary = Workspace.Root.make ~key:(root_key "main") (abs ws_dir) in
  let auxiliary = Workspace.Root.make ~key:(root_key "aux") (abs aux_dir) in
  let cwd =
    match cwd with
    | Primary_root ->
        Workspace.Path.make ~root_key:(root_key "main") Lpath.Rel.root
    | Primary_logical ->
        Workspace.Path.make ~root_key:(root_key "main") (rel "logical")
    | Auxiliary_root ->
        Workspace.Path.make ~root_key:(root_key "aux") Lpath.Rel.root
  in
  match Workspace.make ~cwd ~primary ~read_only:[ auxiliary ] () with
  | Ok workspace -> workspace
  | Error error ->
      failf "workspace construction failed: %a" Workspace.Error.pp error

let fake_merlin_script =
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
      "cat > \"$plan/stdin-$n\"";
      "behavior=response";
      "if [ -f \"$plan/behavior-$n\" ]; then behavior=$(cat \
       \"$plan/behavior-$n\"); fi";
      "case \"$behavior\" in";
      "  response)";
      "    cat \"$plan/response-$n\"";
      "    if [ -f \"$plan/cancel-after-$n\" ]; then : > \"$plan/cancel-now\"; \
       fi";
      "    ;;";
      "  exit7) printf '\\033[31mmerlin failed\\033[0m\\n' >&2; exit 7 ;;";
      "  invalid_exit) printf '\\377bad\\033]0;secret\\007 detail\\n' >&2; \
       exit 9 ;;";
      "  signal) kill -TERM $$ ;;";
      "  overflow) exec /usr/bin/yes x ;;";
      "  stderr_overflow) exec /usr/bin/yes x >&2 ;;";
      "  sleep) exec /bin/sleep 60 ;;";
      "  incomplete) ( /bin/sleep 3 ) & cat \"$plan/response-$n\" ;;";
      "  incomplete_stderr) ( /bin/sleep 3 ) >&2 & cat \"$plan/response-$n\" ;;";
      "  *) printf 'unknown behavior: %s\\n' \"$behavior\" >&2; exit 64 ;;";
      "esac";
      "";
    ]

let with_world ?(cwd = Primary_root)
    ?(mode = Mentat_config.Mode.Danger_full_access) ?clock name fn =
  Eio_main.run @@ fun stdenv ->
  let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
  let base = Unix.realpath (temp_dir ~prefix:("mentat-type-at-" ^ name) ()) in
  let ws_dir = Filename.concat base "workspace" in
  let aux_dir = Filename.concat base "auxiliary" in
  let outside_dir = Filename.concat base "outside" in
  let plan_dir = Filename.concat ws_dir ".merlin-plan" in
  List.iter mkdir [ ws_dir; aux_dir; outside_dir; plan_dir ];
  write_disk
    (Filename.concat ws_dir "logical/main.ml")
    "let answer = 42\nlet use = answer\n";
  write_disk (Filename.concat ws_dir "lib/shared.ml") "let main = 1\n";
  write_disk (Filename.concat aux_dir "lib/shared.ml") "let auxiliary = 2\n";
  write_disk (Filename.concat outside_dir "outside.ml") "let outside = 3\n";
  let fake = Filename.concat ws_dir "fake-ocamlmerlin" in
  install_executable fake fake_merlin_script;
  let logical = workspace ~cwd ws_dir aux_dir in
  Eio.Switch.run @@ fun sw ->
  let io = resolve_exn ~sw ~stdenv ~logical ~mode () in
  let tool =
    match clock with
    | None ->
        Type_at.make io
          ~clock:(Eio.Stdenv.mono_clock stdenv)
          ~program:[ fake; plan_dir ]
    | Some clock -> Type_at.make io ~clock ~program:[ fake; plan_dir ]
  in
  fn { sw; ws_dir; aux_dir; outside_dir; plan_dir; io; tool }

let plan_file world stem index =
  Filename.concat world.plan_dir (Printf.sprintf "%s-%d" stem index)

let set_response world index response =
  write_disk (plan_file world "response" index) response

let set_behavior world index behavior =
  write_disk (plan_file world "behavior" index) behavior

let invocation_count world =
  match read_disk (Filename.concat world.plan_dir "count") with
  | text -> int_of_string text
  | exception Sys_error _ -> 0

let invocation world stem index = read_disk (plan_file world stem index)

let normalize world text =
  text
  |> String.replace_all ~sub:world.aux_dir ~by:"<auxiliary>"
  |> String.replace_all ~sub:world.outside_dir ~by:"<outside>"
  |> String.replace_all ~sub:world.ws_dir ~by:"<workspace>"

let frame ?(tail = "no") ~sl ~sc ~el ~ec type_json =
  Printf.sprintf
    {|{"start":{"line":%d,"col":%d},"end":{"line":%d,"col":%d},"type":%s,"tail":%S}|}
    sl sc el ec type_json tail

let return value = Printf.sprintf {|{"class":"return","value":%s}|} value
let return_frames frames = return ("[" ^ String.concat "," frames ^ "]")
let return_string text = return (json_string (Json.string text))
let standard_frame type_json = frame ~sl:1 ~sc:4 ~el:1 ~ec:10 type_json

let standard_response type_text =
  return_frames [ standard_frame (json_string (Json.string type_text)) ]

let decode_verdict tool label provider_input =
  match decode_call_result tool provider_input with
  | Ok call ->
      Printf.printf "%s: accepted canonical=%s\n" label
        (json_string (Tool.Call.input call))
  | Error error ->
      Printf.printf "%s: rejected diagnostic=%b\n" label
        (not (String.is_empty (Tool.Call.Decode_error.message error)))

let print_result ?(json = false) world result =
  print_status ~normalize:(normalize world) result;
  match Tool.Result.output result with
  | None -> ()
  | Some output ->
      Printf.printf "text: %S\ntruncated: %b\n"
        (normalize world (Tool.Output.text output))
        (Tool.Output.truncated output);
      if json then
        Printf.printf "json: %s\n"
          (match Tool.Output.json output with
          | None -> "none"
          | Some value -> normalize world (json_string value))

let%expect_test "declaration and provider input are exact, bounded, and safe" =
  with_world "schema" @@ fun world ->
  let declaration = Tool.declaration world.tool in
  Printf.printf "name: %s\n" (Mentat_llm.Tool.name declaration);
  Printf.printf "schema: %s\n"
    (json_string (Mentat_llm.Tool.input_schema declaration));
  decode_verdict world.tool "minimal"
    (input ~path:"logical/main.ml" ~line:1 ~column:4 ());
  decode_verdict world.tool "full"
    (input ~path:"logical/main.ml" ~line:2 ~column:8 ~max_enclosings:8
       ~verbosity:3 ~documentation:true ());
  List.iter
    (fun (label, provider_input) ->
      decode_verdict world.tool label provider_input)
    [
      ( "missing path",
        json_object [ ("line", Json.int 1); ("column", Json.int 0) ] );
      ( "unknown member",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("expression", Json.string "answer");
          ] );
      ( "duplicate path",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("path", Json.string "logical/other.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
          ] );
      ( "duplicate optional",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("verbosity", Json.int 1);
            ("verbosity", Json.int 2);
          ] );
      ("empty path", input ~path:"" ~line:1 ~column:0 ());
      ("NUL path", input ~path:"bad\000path" ~line:1 ~column:0 ());
      ("line zero", input ~path:"logical/main.ml" ~line:0 ~column:0 ());
      ("column negative", input ~path:"logical/main.ml" ~line:1 ~column:(-1) ());
      ( "line string",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.string "1");
            ("column", Json.int 0);
          ] );
      ( "column fraction",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.number 0.5);
          ] );
      ( "line unsafe",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.number 9_007_199_254_740_992.);
            ("column", Json.int 0);
          ] );
      ( "safe integer maximum",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.number 9_007_199_254_740_991.);
            ("column", Json.number 9_007_199_254_740_991.);
          ] );
      ( "column infinity",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.number Float.infinity);
          ] );
      ( "enclosings zero",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~max_enclosings:0 () );
      ( "enclosings high",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~max_enclosings:9 () );
      ( "verbosity negative",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~verbosity:(-1) () );
      ( "verbosity high",
        input ~path:"logical/main.ml" ~line:1 ~column:0 ~verbosity:4 () );
      ( "documentation type",
        json_object
          [
            ("path", Json.string "logical/main.ml");
            ("line", Json.int 1);
            ("column", Json.int 0);
            ("documentation", Json.string "yes");
          ] );
    ];
  [%expect
    {|
    name: ocaml_type_at
    schema: {"type":"object","properties":{"path":{"type":"string","description":"Workspace-relative or workspace-contained absolute OCaml source file path.","minLength":1},"line":{"type":"integer","description":"One-based source line.","minimum":1,"maximum":9007199254740991},"column":{"type":"integer","description":"Zero-based byte column in the source line, matching OCaml and Merlin locations.","minimum":0,"maximum":9007199254740991},"max_enclosings":{"type":"integer","description":"Maximum enclosing type frames, innermost first. Every returned frame costs one Merlin type query. Defaults to 1.","minimum":1,"maximum":8},"verbosity":{"type":"integer","description":"Merlin alias and module-type expansion depth. Zero uses Merlin's default and omits the flag. Defaults to 0.","minimum":0,"maximum":3},"documentation":{"type":"boolean","description":"Fetch the entity's odoc documentation with one additional Merlin query. Defaults to false."}},"required":["path","line","column"],"additionalProperties":false}
    minimal: accepted canonical={"column":4,"line":1,"path":"logical/main.ml"}
    full: accepted canonical={"column":8,"documentation":true,"line":2,"max_enclosings":8,"path":"logical/main.ml","verbosity":3}
    missing path: rejected diagnostic=true
    unknown member: rejected diagnostic=true
    duplicate path: rejected diagnostic=true
    duplicate optional: rejected diagnostic=true
    empty path: rejected diagnostic=true
    NUL path: rejected diagnostic=true
    line zero: rejected diagnostic=true
    column negative: rejected diagnostic=true
    line string: rejected diagnostic=true
    column fraction: rejected diagnostic=true
    line unsafe: rejected diagnostic=true
    safe integer maximum: accepted canonical={"column":9007199254740991,"line":9007199254740991,"path":"logical/main.ml"}
    column infinity: rejected diagnostic=true
    enclosings zero: rejected diagnostic=true
    enclosings high: rejected diagnostic=true
    verbosity negative: rejected diagnostic=true
    verbosity high: rejected diagnostic=true
    documentation type: rejected diagnostic=true |}]

let print_permissions call =
  let requests = Tool.Call.permissions call in
  Printf.printf "requests: %d\n" (List.length requests);
  List.iter
    (fun request ->
      Printf.printf "source: %s\n"
        (Option.value ~default:"none" (Permission.Request.source request));
      List.iter
        (function
          | Access.Path { op; scope = Access.Path_scope.Workspace path } ->
              Printf.printf "path: %s key=%s address=%s\n" (path_op_name op)
                (Workspace.Root.Key.to_string (Workspace.Path.root_key path))
                (Workspace.Path.display path)
          | Access.Custom { name; subject } ->
              Printf.printf "custom: name=%s subject=%s\n" name
                (Option.value ~default:"none" subject)
          | access -> failf "unexpected type-at permission: %a" Access.pp access)
        (Permission.Request.accesses request))
    requests

let%expect_test "permissions bind one read and one command-confinement fact" =
  with_world "permissions" @@ fun world ->
  let primary =
    decode_call world.tool (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
  in
  print_endline "-- primary --";
  print_permissions primary;
  print_endline "-- auxiliary --";
  print_permissions
    (decode_call world.tool
       (input
          ~path:(Filename.concat world.aux_dir "lib/shared.ml")
          ~line:1 ~column:4 ()));
  print_endline "-- unresolved --";
  print_permissions
    (decode_call world.tool
       (input ~path:"../outside/nope.ml" ~line:1 ~column:4 ()));
  Unix.unlink (Filename.concat world.ws_dir "logical/main.ml");
  print_endline "-- cached after deletion --";
  print_permissions primary;
  [%expect
    {|
    -- primary --
    requests: 1
    source: ocaml_type_at
    path: read key=main address=logical/main.ml
    custom: name=command.confinement subject=direct
    -- auxiliary --
    requests: 1
    source: ocaml_type_at
    path: read key=aux address=lib/shared.ml
    custom: name=command.confinement subject=direct
    -- unresolved --
    requests: 0
    -- cached after deletion --
    requests: 1
    source: ocaml_type_at
    path: read key=main address=logical/main.ml
    custom: name=command.confinement subject=direct |}]

let%expect_test "the real command boundary receives exact source cwd and argv" =
  with_world ~cwd:Primary_logical "transport" @@ fun world ->
  set_response world 1 (standard_response "int");
  let result = run world (input ~path:"main.ml" ~line:1 ~column:4 ()) in
  print_result ~json:true world result;
  Printf.printf "cwd: %s\n"
    (normalize world (String.trim (invocation world "cwd" 1)));
  Printf.printf "argv: %S\n" (normalize world (invocation world "argv" 1));
  Printf.printf "stdin exact: %b\n"
    (String.equal
       (invocation world "stdin" 1)
       "let answer = 42\nlet use = answer\n");
  [%expect
    {|
    status: completed
    text: "OCaml type at main.ml:1:4\n- main.ml:1:4  int\nbackend: ocamlmerlin"
    truncated: false
    json: {"version":1,"head":"int","more":0}
    cwd: <workspace>
    argv: "single\ntype-enclosing\n-position\n1:4\n-index\n0\n-printer-width\n80\n-filename\n<workspace>/logical/main.ml\n"
    stdin exact: true |}]

let%expect_test
    "auxiliary and sibling addresses stay replayable and collision-free" =
  with_world ~cwd:Primary_logical "roots" @@ fun world ->
  set_response world 1 (standard_response "main");
  let main =
    run world
      (input
         ~path:(Filename.concat world.ws_dir "lib/shared.ml")
         ~line:1 ~column:4 ())
  in
  print_endline "-- primary sibling --";
  print_result ~json:true world main;
  Printf.printf "cwd: %s\n"
    (normalize world (String.trim (invocation world "cwd" 1)));
  set_response world 2 (standard_response "auxiliary");
  let auxiliary_path = Filename.concat world.aux_dir "lib/shared.ml" in
  let auxiliary = run world (input ~path:auxiliary_path ~line:1 ~column:4 ()) in
  print_endline "-- auxiliary --";
  print_result ~json:true world auxiliary;
  Printf.printf "cwd: %s\n"
    (normalize world (String.trim (invocation world "cwd" 2)));
  let replay_path = auxiliary_path in
  set_response world 3 (standard_response "auxiliary replay");
  let replay = run world (input ~path:replay_path ~line:1 ~column:4 ()) in
  Printf.printf "replay: %s\n"
    (semantic_exn replay |> Mentat_tools_output.Ocaml.Type_at.head);
  [%expect
    {|
    -- primary sibling --
    status: completed
    text: "OCaml type at <workspace>/lib/shared.ml:1:4\n- <workspace>/lib/shared.ml:1:4  main\nbackend: ocamlmerlin"
    truncated: false
    json: {"version":1,"head":"main","more":0}
    cwd: <workspace>
    -- auxiliary --
    status: completed
    text: "OCaml type at <auxiliary>/lib/shared.ml:1:4\n- <auxiliary>/lib/shared.ml:1:4  auxiliary\nbackend: ocamlmerlin"
    truncated: false
    json: {"version":1,"head":"auxiliary","more":0}
    cwd: <auxiliary>
    replay: auxiliary replay |}]

let%expect_test "an auxiliary logical cwd keeps its short provider address" =
  with_world ~cwd:Auxiliary_root "aux-cwd" @@ fun world ->
  set_response world 1 (standard_response "auxiliary cwd");
  let result = run world (input ~path:"lib/shared.ml" ~line:1 ~column:4 ()) in
  print_result ~json:true world result;
  Printf.printf "cwd: %s\n"
    (normalize world (String.trim (invocation world "cwd" 1)));
  [%expect
    {|
    status: completed
    text: "OCaml type at lib/shared.ml:1:4\n- lib/shared.ml:1:4  auxiliary cwd\nbackend: ocamlmerlin"
    truncated: false
    json: {"version":1,"head":"auxiliary cwd","more":0}
    cwd: <auxiliary> |}]

let frame_summary result =
  let semantic = semantic_exn result in
  Printf.printf "semantic: head=%S more=%d truncated=%b\ntext: %S\n"
    (Mentat_tools_output.Ocaml.Type_at.head semantic)
    (Mentat_tools_output.Ocaml.Type_at.more semantic)
    (Tool.Output.truncated (output_exn result))
    (Tool.Output.text (output_exn result))

let invocation_indices world =
  let index_of_call index =
    let tokens =
      invocation world "argv" index
      |> String.split_on_char '\n'
      |> List.filter (Fun.negate String.is_empty)
    in
    let rec find = function
      | "-index" :: value :: _ -> Some value
      | _ :: rest -> find rest
      | [] -> None
    in
    find tokens
  in
  List.init (invocation_count world) (fun index -> index_of_call (index + 1))
  |> List.filter_map Fun.id

let%expect_test
    "dedup preserves original indices and charges one query per frame" =
  with_world "frames" @@ fun world ->
  let inner = frame ~sl:1 ~sc:4 ~el:1 ~ec:10 in
  let outer = frame ~sl:1 ~sc:0 ~el:2 ~ec:0 in
  set_response world 1 (return_frames [ inner "\"int\""; inner "1"; outer "2" ]);
  set_response world 2
    (return_frames [ inner "0"; inner "1"; outer "\"int list\"" ]);
  let result =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~max_enclosings:2
         ~verbosity:2 ())
  in
  print_status result;
  frame_summary result;
  Printf.printf "indices: %s\n" (String.concat "," (invocation_indices world));
  Printf.printf "verbosity every call: %b\n"
    (List.init (invocation_count world) (fun index ->
         String.includes ~affix:"-verbosity\n2\n"
           (invocation world "argv" (index + 1)))
    |> List.for_all Fun.id);
  [%expect
    {|
    status: completed
    semantic: head="int" more=1 truncated=false
    text: "OCaml type at logical/main.ml:1:4\n- logical/main.ml:1:4  int\n- logical/main.ml:1:0  int list\nbackend: ocamlmerlin"
    indices: 0,2
    verbosity every call: true |}]

let%expect_test "default verbosity is omitted and the selected depth clamps" =
  with_world "depth" @@ fun world ->
  let inner = frame ~sl:1 ~sc:4 ~el:1 ~ec:10 in
  let outer = frame ~sl:1 ~sc:0 ~el:2 ~ec:0 in
  set_response world 1 (return_frames [ inner "\"int\""; outer "1" ]);
  set_response world 2 (return_frames [ inner "0"; outer "\"int list\"" ]);
  let result =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~max_enclosings:8 ())
  in
  print_status result;
  frame_summary result;
  Printf.printf "invocations: %d\n" (invocation_count world);
  Printf.printf "mentions verbosity: %b\n"
    (List.init (invocation_count world) (fun index ->
         String.includes ~affix:"-verbosity"
           (invocation world "argv" (index + 1)))
    |> List.exists Fun.id);
  [%expect
    {|
    status: completed
    semantic: head="int" more=1 truncated=false
    text: "OCaml type at logical/main.ml:1:4\n- logical/main.ml:1:4  int\n- logical/main.ml:1:0  int list\nbackend: ocamlmerlin"
    invocations: 2
    mentions verbosity: false |}]

let%expect_test "empty and malformed type stacks fail without partial evidence"
    =
  let cases =
    [
      ("empty", return_frames []);
      ("value not array", return_string "not an array");
      ("frame not object", return_frames [ "1" ]);
      ( "missing tail",
        return_frames
          [
            {|{"start":{"line":1,"col":0},"end":{"line":1,"col":1},"type":"int"}|};
          ] );
      ( "invalid tail",
        return_frames
          [ frame ~tail:"unknown" ~sl:1 ~sc:4 ~el:1 ~ec:10 "\"int\"" ] );
      ( "extra frame member",
        return_frames
          [
            {|{"start":{"line":1,"col":0},"end":{"line":1,"col":1},"type":"int","tail":"no","extra":true}|};
          ] );
      ( "duplicate frame member",
        return_frames
          [
            {|{"start":{"line":1,"col":0},"end":{"line":1,"col":1},"type":"int","type":"string","tail":"no"}|};
          ] );
      ( "duplicate position member",
        return_frames
          [
            {|{"start":{"line":1,"line":1,"col":0},"end":{"line":1,"col":1},"type":"int","tail":"no"}|};
          ] );
      ( "fractional position",
        return_frames
          [
            {|{"start":{"line":1.5,"col":0},"end":{"line":1,"col":1},"type":"int","tail":"no"}|};
          ] );
      ( "unsafe position",
        return_frames
          [
            {|{"start":{"line":9007199254740992,"col":0},"end":{"line":1,"col":1},"type":"int","tail":"no"}|};
          ] );
      ("bad position", return_frames [ frame ~sl:0 ~sc:0 ~el:1 ~ec:1 "\"int\"" ]);
      ( "reversed range",
        return_frames [ frame ~sl:2 ~sc:0 ~el:1 ~ec:1 "\"int\"" ] );
      ( "empty printed type",
        return_frames [ standard_frame (json_string (Json.string "")) ] );
      ("missing printed type", return_frames [ standard_frame "0" ]);
    ]
  in
  List.iter
    (fun (label, response) ->
      with_world ("malformed-" ^ label) @@ fun world ->
      set_response world 1 response;
      Printf.printf "-- %s --\n" label;
      print_result world
        (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ())))
    cases;
  [%expect
    {|
    -- empty --
    status: failed not_found
    message: no type at position 1:4
    metadata: false
    -- value not array --
    status: failed failed
    message: could not decode ocamlmerlin response: type-enclosing value is not an array
    metadata: false
    -- frame not object --
    status: failed failed
    message: could not decode ocamlmerlin response: frame is not an object
    metadata: false
    -- missing tail --
    status: failed failed
    message: could not decode ocamlmerlin response: frame has unexpected or duplicate members
    metadata: false
    -- invalid tail --
    status: failed failed
    message: could not decode ocamlmerlin response: frame tail is invalid
    metadata: false
    -- extra frame member --
    status: failed failed
    message: could not decode ocamlmerlin response: frame has unexpected or duplicate members
    metadata: false
    -- duplicate frame member --
    status: failed failed
    message: could not decode ocamlmerlin response: frame has unexpected or duplicate members
    metadata: false
    -- duplicate position member --
    status: failed failed
    message: could not decode ocamlmerlin response: position has unexpected or duplicate members
    metadata: false
    -- fractional position --
    status: failed failed
    message: could not decode ocamlmerlin response: position members must be safe integers
    metadata: false
    -- unsafe position --
    status: failed failed
    message: could not decode ocamlmerlin response: position members must be safe integers
    metadata: false
    -- bad position --
    status: failed failed
    message: could not decode ocamlmerlin response: Mentat_ocaml.Position.make: line must be >= 1
    metadata: false
    -- reversed range --
    status: failed failed
    message: could not decode ocamlmerlin response: Mentat_ocaml.Range.make: end_ must not be before start
    metadata: false
    -- empty printed type --
    status: failed failed
    message: could not decode ocamlmerlin response: frame 0 has an empty printed type
    metadata: false
    -- missing printed type --
    status: failed failed
    message: could not decode ocamlmerlin response: frame 0 has no printed type
    metadata: false |}]

let%expect_test "all documented Merlin tail variants are accepted" =
  with_world "tail-variants" @@ fun world ->
  let inner = frame ~tail:"position" ~sl:1 ~sc:4 ~el:1 ~ec:10 in
  let outer = frame ~tail:"call" ~sl:1 ~sc:0 ~el:2 ~ec:0 in
  set_response world 1 (return_frames [ inner "\"int\""; outer "1" ]);
  set_response world 2 (return_frames [ inner "0"; outer "\"int list\"" ]);
  let result =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~max_enclosings:2 ())
  in
  print_status result;
  frame_summary result;
  [%expect
    {|
    status: completed
    semantic: head="int" more=1 truncated=false
    text: "OCaml type at logical/main.ml:1:4\n- logical/main.ml:1:4  int\n- logical/main.ml:1:0  int list\nbackend: ocamlmerlin" |}]

let%expect_test "targeted frames must preserve their index and range" =
  let run_case label response =
    with_world ("target-" ^ label) @@ fun world ->
    let inner = frame ~sl:1 ~sc:4 ~el:1 ~ec:10 in
    let outer = frame ~sl:1 ~sc:0 ~el:2 ~ec:0 in
    set_response world 1 (return_frames [ inner "\"int\""; outer "1" ]);
    set_response world 2 response;
    Printf.printf "-- %s --\n" label;
    print_result world
      (run world
         (input ~path:"logical/main.ml" ~line:1 ~column:4 ~max_enclosings:2 ()))
  in
  let inner = frame ~sl:1 ~sc:4 ~el:1 ~ec:10 in
  let outer = frame ~sl:1 ~sc:0 ~el:2 ~ec:0 in
  run_case "missing index" (return_frames [ inner "0" ]);
  run_case "range drift"
    (return_frames [ inner "0"; frame ~sl:1 ~sc:0 ~el:3 ~ec:0 "\"int list\"" ]);
  run_case "type absent" (return_frames [ inner "0"; outer "null" ]);
  [%expect
    {|
    -- missing index --
    status: failed failed
    message: could not decode ocamlmerlin response: frame 1 is missing
    metadata: false
    -- range drift --
    status: failed failed
    message: could not decode ocamlmerlin response: frame 1 changed range
    metadata: false
    -- type absent --
    status: failed failed
    message: could not decode ocamlmerlin response: frame 1 has no printed type
    metadata: false |}]

let documentation_summary result =
  Printf.printf "documentation=%S overall=%b\n"
    (line_with "documentation:" result)
    (Tool.Output.truncated (output_exn result))

let%expect_test
    "documentation uses one extra query and classifies every sentinel" =
  let cases =
    [
      ("available", "A useful value.");
      ("no documentation", "No documentation available");
      ("invalid identifier", "Not a valid identifier");
      ("lookup failure", "didn't manage to find answer");
      ("environment", "Not in environment answer");
      ("builtin", "int is a builtin, no documentation is available");
      ( "missing cmi",
        "answer was expected in fixture.cmi but could not be found" );
    ]
  in
  List.iter
    (fun (label, documentation) ->
      with_world ("doc-" ^ label) @@ fun world ->
      set_response world 1 (standard_response "int");
      set_response world 2 (return_string documentation);
      let result =
        run world
          (input ~path:"logical/main.ml" ~line:1 ~column:4 ~documentation:true
             ())
      in
      Printf.printf "-- %s --\n" label;
      print_status result;
      documentation_summary result;
      Printf.printf "commands: %s invocations=%d\n"
        (List.init (invocation_count world) (fun index ->
             let tokens =
               invocation world "argv" (index + 1) |> String.split_on_char '\n'
             in
             List.nth_opt tokens 1 |> Option.value ~default:"missing")
        |> String.concat ",")
        (invocation_count world))
    cases;
  [%expect
    {|
    -- available --
    status: completed
    documentation="documentation: A useful value." overall=false
    commands: type-enclosing,document invocations=2
    -- no documentation --
    status: completed
    documentation="documentation: unavailable (No documentation available)" overall=false
    commands: type-enclosing,document invocations=2
    -- invalid identifier --
    status: completed
    documentation="documentation: unavailable (Not a valid identifier)" overall=false
    commands: type-enclosing,document invocations=2
    -- lookup failure --
    status: completed
    documentation="documentation: unavailable (didn't manage to find answer)" overall=false
    commands: type-enclosing,document invocations=2
    -- environment --
    status: completed
    documentation="documentation: unavailable (Not in environment answer)" overall=false
    commands: type-enclosing,document invocations=2
    -- builtin --
    status: completed
    documentation="documentation: unavailable (int is a builtin, no documentation is available)" overall=false
    commands: type-enclosing,document invocations=2
    -- missing cmi --
    status: completed
    documentation="documentation: unavailable (answer was expected in fixture.cmi but could not be found)" overall=false
    commands: type-enclosing,document invocations=2 |}]

let%expect_test
    "documentation failures are explicit non-fatal unavailable slots" =
  let run_case label configure =
    with_world ("doc-failure-" ^ label) @@ fun world ->
    set_response world 1 (standard_response "int");
    configure world;
    let result =
      run world
        (input ~path:"logical/main.ml" ~line:1 ~column:4 ~documentation:true ())
    in
    Printf.printf "-- %s --\n" label;
    print_status result;
    documentation_summary result
  in
  run_case "non-string" (fun world -> set_response world 2 (return "42"));
  run_case "query error" (fun world ->
      set_response world 2 {|{"class":"error","value":"no doc"}|});
  run_case "malformed" (fun world -> set_response world 2 "not json");
  run_case "nonzero" (fun world -> set_behavior world 2 "exit7");
  run_case "output limit" (fun world -> set_behavior world 2 "overflow");
  [%expect
    {|
    -- non-string --
    status: completed
    documentation="documentation: unavailable (documentation lookup failed)" overall=false
    -- query error --
    status: completed
    documentation="documentation: unavailable (documentation lookup failed)" overall=false
    -- malformed --
    status: completed
    documentation="documentation: unavailable (documentation lookup failed)" overall=false
    -- nonzero --
    status: completed
    documentation="documentation: unavailable (documentation lookup failed)" overall=false
    -- output limit --
    status: completed
    documentation="documentation: unavailable (documentation lookup failed)" overall=false |}]

let repeat count text =
  let buffer = Buffer.create (count * String.length text) in
  for _ = 1 to count do
    Buffer.add_string buffer text
  done;
  Buffer.contents buffer

let%expect_test "type documentation and sentinel bounds preserve UTF-8" =
  let check_type () =
    with_world "long-type" @@ fun world ->
    set_response world 1 (standard_response (repeat 3_000 "é"));
    let result =
      run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
    in
    let head = semantic_exn result |> Mentat_tools_output.Ocaml.Type_at.head in
    Printf.printf "type: head_bytes=%d valid=%b overall=%b\n"
      (String.length head)
      (String.is_valid_utf_8 head)
      (Tool.Output.truncated (output_exn result))
  in
  let check_documentation label prefix =
    with_world ("long-" ^ label) @@ fun world ->
    set_response world 1 (standard_response "int");
    set_response world 2 (return_string (prefix ^ repeat 5_000 "é"));
    let result =
      run world
        (input ~path:"logical/main.ml" ~line:1 ~column:4 ~documentation:true ())
    in
    let text = line_with "documentation:" result in
    Printf.printf "%s: line_bytes=%d valid=%b overall=%b\n" label
      (String.length text)
      (String.is_valid_utf_8 text)
      (Tool.Output.truncated (output_exn result))
  in
  check_type ();
  check_documentation "doc" "";
  check_documentation "reason" "Not in environment ";
  [%expect
    {|
    type: head_bytes=512 valid=true overall=true
    doc: line_bytes=8219 valid=true overall=true
    reason: line_bytes=8232 valid=true overall=true |}]

let run_filesystem_case label setup path =
  with_world ("fs-" ^ label) @@ fun world ->
  setup world;
  Printf.printf "-- %s --\n" label;
  let result = run world (input ~path:(path world) ~line:1 ~column:0 ()) in
  print_result world result;
  Printf.printf "invocations: %d\n" (invocation_count world)

let%expect_test
    "source validation refuses missing nonregular escape and oversize" =
  run_filesystem_case "missing" ignore (fun _ -> "missing.ml");
  run_filesystem_case "directory" ignore (fun _ -> "logical");
  run_filesystem_case "symlink"
    (fun world ->
      Unix.symlink world.outside_dir (Filename.concat world.ws_dir "escape"))
    (fun _ -> "escape/outside.ml");
  run_filesystem_case "outside" ignore (fun world ->
      Filename.concat world.outside_dir "outside.ml");
  run_filesystem_case "oversize"
    (fun world ->
      let path = Filename.concat world.ws_dir "large.ml" in
      let channel = Unix.openfile path [ Unix.O_CREAT; Unix.O_WRONLY ] 0o644 in
      Fun.protect ~finally:(fun () -> Unix.close channel) @@ fun () ->
      Unix.LargeFile.ftruncate channel
        (Int64.of_int (Type_at.max_source_bytes + 1)))
    (fun _ -> "large.ml");
  [%expect
    {|
    -- missing --
    status: failed not_found
    message: missing.ml: path does not exist
    metadata: false
    invocations: 0
    -- directory --
    status: failed invalid_input
    message: logical: expected a regular file, found directory
    metadata: false
    invocations: 0
    -- symlink --
    status: failed invalid_input
    message: escape/outside.ml: path resolves outside workspace
    metadata: false
    invocations: 0
    -- outside --
    status: failed invalid_input
    message: path is outside workspace: <outside>/outside.ml
    metadata: false
    invocations: 0
    -- oversize --
    status: failed invalid_input
    message: large.ml: file is too large (8388609 bytes, max 8388608)
    metadata: false
    invocations: 0 |}]

let%expect_test "a decoded path is revalidated after filesystem replacement" =
  with_world "stale-source" @@ fun world ->
  let call =
    decode_call world.tool (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
  in
  let source = Filename.concat world.ws_dir "logical/main.ml" in
  Unix.unlink source;
  Unix.symlink world.outside_dir source;
  let result = Tool.Call.run call ~cancelled:(fun () -> false) |> finished in
  print_result world result;
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    status: failed invalid_input
    message: logical/main.ml: path resolves outside workspace
    metadata: false
    invocations: 0 |}]

let run_protocol_case label configure =
  with_world ("protocol-" ^ label) @@ fun world ->
  configure world;
  Printf.printf "-- %s --\n" label;
  print_result world
    (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ()))

let%expect_test "Merlin envelopes reject missing duplicate and invalid classes"
    =
  run_protocol_case "not JSON" (fun world -> set_response world 1 "not-json");
  run_protocol_case "not object" (fun world -> set_response world 1 "[]");
  run_protocol_case "missing class" (fun world ->
      set_response world 1 {|{"value":[]}|});
  run_protocol_case "class type" (fun world ->
      set_response world 1 {|{"class":1,"value":[]}|});
  run_protocol_case "unknown class" (fun world ->
      set_response world 1 {|{"class":"wat","value":[]}|});
  run_protocol_case "duplicate class" (fun world ->
      set_response world 1 {|{"class":"return","class":"return","value":[]}|});
  run_protocol_case "duplicate value" (fun world ->
      set_response world 1 {|{"class":"return","value":[],"value":[]}|});
  List.iter
    (fun class_ ->
      run_protocol_case class_ (fun world ->
          set_response world 1
            (Printf.sprintf {|{"class":%S,"value":"boom"}|} class_)))
    [ "failure"; "error"; "exception" ];
  [%expect
    {|
    -- not JSON --
    status: failed failed
    message: could not decode ocamlmerlin response: Expected u while parsing null but found: o
    File "-", line 1, characters 0-2:
    metadata: false
    -- not object --
    status: failed failed
    message: could not decode ocamlmerlin response: response is not an object
    metadata: false
    -- missing class --
    status: failed failed
    message: could not decode ocamlmerlin response: response has no class
    metadata: false
    -- class type --
    status: failed failed
    message: could not decode ocamlmerlin response: response class is not a string
    metadata: false
    -- unknown class --
    status: failed failed
    message: could not decode ocamlmerlin response: unexpected response class wat
    metadata: false
    -- duplicate class --
    status: failed failed
    message: could not decode ocamlmerlin response: response has duplicate class members
    metadata: false
    -- duplicate value --
    status: failed failed
    message: could not decode ocamlmerlin response: response has duplicate value members
    metadata: false
    -- failure --
    status: failed failed
    message: ocamlmerlin returned failure: boom
    metadata: false
    -- error --
    status: failed failed
    message: ocamlmerlin returned error: boom
    metadata: false
    -- exception --
    status: failed failed
    message: ocamlmerlin returned exception: boom
    metadata: false |}]

let%expect_test
    "spawn exit signal output and incomplete-drain failures stay distinct" =
  with_world "missing-program" @@ fun world ->
  let clock = Eio_mock.Clock.Mono.make () in
  let tool =
    Type_at.make world.io ~clock
      ~program:[ Filename.concat world.ws_dir "missing-merlin" ]
  in
  let call =
    decode_call tool (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
  in
  print_endline "-- spawn --";
  let spawn = Tool.Call.run call ~cancelled:(fun () -> false) |> finished in
  (match Tool.Result.status spawn with
  | Tool.Result.Failed { kind = `Unavailable; message; metadata } ->
      Printf.printf "unavailable=true diagnostic=%b metadata=%b output=%b\n"
        (not (String.is_empty message))
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output spawn))
  | _ ->
      failf "missing executable was not unavailable: %s"
        (json_string (encode result_codec spawn)));
  List.iter
    (fun behavior ->
      run_protocol_case behavior (fun world -> set_behavior world 1 behavior))
    [ "exit7"; "invalid_exit" ];
  with_world "signal" @@ fun signal_world ->
  set_behavior signal_world 1 "signal";
  let signal =
    run signal_world (input ~path:"logical/main.ml" ~line:1 ~column:4 ())
  in
  print_endline "-- signal --";
  (match Tool.Result.status signal with
  | Tool.Result.Failed { kind = `Failed; message; metadata } ->
      Printf.printf "signal-diagnostic=%b metadata=%b output=%b\n"
        (String.starts_with ~prefix:"ocamlmerlin was terminated by signal "
           message)
        (Option.is_some metadata)
        (Option.is_some (Tool.Result.output signal))
  | _ ->
      failf "signaled executable was not failed: %s"
        (json_string (encode result_codec signal)));
  run_protocol_case "overflow" (fun overflow ->
      set_behavior overflow 1 "overflow");
  run_protocol_case "stderr_overflow" (fun overflow ->
      set_behavior overflow 1 "stderr_overflow");
  run_protocol_case "incomplete" (fun world ->
      set_response world 1 (standard_response "int");
      set_behavior world 1 "incomplete");
  run_protocol_case "incomplete_stderr" (fun world ->
      set_response world 1 (standard_response "int");
      set_behavior world 1 "incomplete_stderr");
  [%expect
    {|
    -- spawn --
    unavailable=true diagnostic=true metadata=false output=false
    -- exit7 --
    status: failed failed
    message: merlin failed
    metadata: false
    -- invalid_exit --
    status: failed failed
    message: �bad detail
    metadata: false
    -- signal --
    signal-diagnostic=true metadata=false output=false
    -- overflow --
    status: failed failed
    message: ocamlmerlin stdout exceeded 1048576-byte output limit
    metadata: false
    -- stderr_overflow --
    status: failed failed
    message: ocamlmerlin stderr exceeded 1048576-byte output limit
    metadata: false
    -- incomplete --
    status: failed failed
    message: ocamlmerlin stdout was incomplete after the child exited
    metadata: false
    -- incomplete_stderr --
    status: failed failed
    message: ocamlmerlin stderr was incomplete after the child exited
    metadata: false |}]

let%expect_test "timeout and cooperative cancellation stop the real child" =
  let mock_clock = Eio_mock.Clock.Mono.make () in
  Eio_mock.Clock.Mono.set_time mock_clock (Mtime.of_uint64_ns 0L);
  with_world ~clock:mock_clock "timeout" @@ fun world ->
  set_behavior world 1 "sleep";
  let rec advance_when_scheduled () =
    Eio.Fiber.yield ();
    if Eio_mock.Clock.Mono.try_advance mock_clock then `Stop_daemon
    else advance_when_scheduled ()
  in
  Eio.Fiber.fork_daemon ~sw:world.sw advance_when_scheduled;
  print_endline "-- timeout --";
  print_result world
    (run world (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  with_world "cancel-running" @@ fun cancelled_world ->
  set_behavior cancelled_world 1 "sleep";
  let deadline = Unix.gettimeofday () +. 0.1 in
  print_endline "-- running cancellation --";
  print_result cancelled_world
    (run
       ~cancelled:(fun () -> Unix.gettimeofday () >= deadline)
       cancelled_world
       (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  with_world "cancel-before" @@ fun before ->
  set_response before 1 (standard_response "never");
  print_endline "-- before observation --";
  print_result before
    (run
       ~cancelled:(fun () -> true)
       before
       (input ~path:"logical/main.ml" ~line:1 ~column:4 ()));
  Printf.printf "invocations: %d\n" (invocation_count before);
  [%expect
    {|
    +mock time is now 0
    -- timeout --
    +mock time is now 30
    status: failed timed_out
    message: ocamlmerlin timed out after 30000ms
    metadata: false
    -- running cancellation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    -- before observation --
    status: interrupted cancelled=true
    reason: tool call cancelled
    invocations: 0 |}]

let%expect_test
    "cancellation between frame and documentation queries is terminal" =
  with_world "cancel-checkpoints" @@ fun world ->
  let inner = frame ~sl:1 ~sc:4 ~el:1 ~ec:10 in
  let outer = frame ~sl:1 ~sc:0 ~el:2 ~ec:0 in
  set_response world 1 (return_frames [ inner "\"int\""; outer "1" ]);
  set_response world 2 (return_frames [ inner "0"; outer "\"int list\"" ]);
  set_response world 3 (return_string "should not be retained");
  write_disk (plan_file world "cancel-after" 2) "";
  let result =
    run
      ~cancelled:(fun () ->
        Sys.file_exists (Filename.concat world.plan_dir "cancel-now"))
      world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~max_enclosings:2
         ~documentation:true ())
  in
  print_result world result;
  Printf.printf "invocations: %d\n" (invocation_count world);
  [%expect
    {|
    status: interrupted cancelled=true
    reason: tool call cancelled
    invocations: 2 |}]

let%expect_test
    "durable replay preserves authoritative JSON and excludes D2 names" =
  with_world "durable" @@ fun world ->
  set_response world 1 (standard_response "int");
  set_response world 2 (return_string "A durable doc.");
  let result =
    run world
      (input ~path:"logical/main.ml" ~line:1 ~column:4 ~documentation:true ())
  in
  let stored = encode result_codec result in
  let replayed = decode result_codec stored in
  let stored_again = encode result_codec replayed in
  Printf.printf "roundtrip equal: %b\n" (Json.equal stored stored_again);
  Printf.printf "output text equal: %b json equal: %b truncated equal: %b\n"
    (String.equal
       (Tool.Output.text (output_exn result))
       (Tool.Output.text (output_exn replayed)))
    (Option.equal Json.equal
       (Tool.Output.json (output_exn result))
       (Tool.Output.json (output_exn replayed)))
    (Bool.equal
       (Tool.Output.truncated (output_exn result))
       (Tool.Output.truncated (output_exn replayed)));
  let forbidden =
    [
      "value";
      "evidence";
      "mutation";
      "receipt";
      "revert";
      "sandbox";
      "permissions";
      "permission_requests";
      "claim";
      "settlement";
    ]
  in
  let names = json_member_names stored in
  Printf.printf "forbidden members: %s\n"
    (match List.filter (fun name -> List.mem name names) forbidden with
    | [] -> "<none>"
    | present -> String.concat "," present);
  let semantic = semantic_exn replayed in
  Printf.printf "head=%S more=%d invocations-after-replay=%d\n"
    (Mentat_tools_output.Ocaml.Type_at.head semantic)
    (Mentat_tools_output.Ocaml.Type_at.more semantic)
    (invocation_count world);
  [%expect
    {|
    roundtrip equal: true
    output text equal: true json equal: true truncated equal: true
    forbidden members: <none>
    head="int" more=0 invocations-after-replay=2 |}]

let%expect_test "constructor rejects an empty immutable program prefix" =
  with_world "constructor" @@ fun world ->
  let clock = Eio_mock.Clock.Mono.make () in
  let raised =
    match Type_at.make world.io ~clock ~program:[] with
    | _ -> false
    | exception Invalid_argument diagnostic ->
        Printf.printf "diagnostic: %s\n" diagnostic;
        true
  in
  Printf.printf "raised: %b invocations: %d\n" raised (invocation_count world);
  [%expect
    {|
    diagnostic: Ocaml.Type_at.make: program prefix must not be empty
    raised: true invocations: 0 |}]
