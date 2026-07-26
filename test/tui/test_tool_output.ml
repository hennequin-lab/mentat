(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Json = Jsont.Json
module Llm = Mentat_llm
module Output = Mentat_tools_output
module Tool = Mentat_tool

let json_object fields =
  Json.object'
    (List.map (fun (name, value) -> Json.mem (Json.name name) value) fields)

let call ~id ~name = Llm.Tool.Call.make ~id ~name ~input:(Json.object' []) ()

let completed ~id ~name output =
  Tui.Turn_script.tool ~call:(call ~id ~name)
    ~result:(Tool.Result.completed ~output ())

let settled ~id ~name result =
  Tui.Turn_script.tool ~call:(call ~id ~name) ~result

let encoded ?(text = "model-facing output") codec value =
  Output.Codec.encode codec ~text value

let submit t prompt =
  Tui.settle t;
  Tui.keys t prompt;
  Tui.enter t;
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t

let run ~name ~size ~prompt tools =
  let turn =
    Tui.Turn_script.complete ~prompt ~tools "The tool results are settled."
  in
  Tui.run
    ~home:(fun _ -> None)
    ~name ~size ~turns:[ turn ]
    (fun t -> submit t prompt)

let%expect_test "known built-ins never expand undecodable text as a summary" =
  let forbidden label = label ^ " MUST NOT RENDER " ^ String.make 2_000 'x' in
  let absent = Tool.Output.make ~text:(forbidden "ABSENT") () in
  let malformed =
    Tool.Output.make ~text:(forbidden "MALFORMED")
      ~json:(Json.string "not a summary object")
      ()
  in
  let wrong_version =
    Tool.Output.make ~text:(forbidden "VERSION")
      ~json:
        (json_object
           [
             ("version", Json.int 2);
             ("shape", Json.string "file");
             ("count", Json.int 1);
           ])
      ()
  in
  let wrong_shape =
    Tool.Output.make ~text:(forbidden "SHAPE")
      ~json:
        (json_object
           [
             ("version", Json.int 1);
             ("shape", Json.string "wrote");
             ("count", Json.int 1);
           ])
      ()
  in
  run ~name:"t" ~size:(92, 30) ~prompt:"inspect invalid built-in summaries"
    [
      completed ~id:"absent" ~name:"read_file" absent;
      completed ~id:"malformed" ~name:"read_file" malformed;
      completed ~id:"version" ~name:"read_file" wrong_version;
      completed ~id:"shape" ~name:"read_file" wrong_shape;
    ];
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ inspect invalid built-in summaries
06 |
07 | ⏺ Read
08 |   ⎿  details unavailable
09 |
10 | ⏺ Read
11 |   ⎿  details unavailable
12 |
13 | ⏺ Read
14 |   ⎿  details unavailable
15 |
16 | ⏺ Read
17 |   ⎿  details unavailable
18 |
19 | ⏺ The tool results are settled.
20 |
21 |
22 |
23 |
24 |
25 |
26 |
27 | ────────────────────────────────────────────────────────────────────────────────────────────
28 | ❯ message mentat
29 | ────────────────────────────────────────────────────────────────────────────────────────────
30 |   ! not logged in · /login · /private/tmp/menta… · openai/gpt-5… · ! full acce… ? for sho…|}]

let%expect_test "read preview clamps rows while typed facts own counts" =
  let text =
    String.concat "\n"
      [
        "note.txt lines=1-8 returned=8/8 status=complete";
        "1\talpha";
        "2\tbravo";
        "3\tcharlie";
        "4\tdelta";
        "5\techo";
        "6\tfoxtrot";
        "7\tgolf";
        "8\thotel";
        "next: this envelope is not preview content";
        "";
      ]
  in
  let output =
    Output.Codec.encode Output.Read.jsont ~text (Output.Read.file ~lines:8)
  in
  run ~name:"t" ~size:(92, 28) ~prompt:"read the note"
    [ completed ~id:"read" ~name:"read_file" output ];
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ read the note
06 |
07 | ⏺ Read
08 |   ⎿  Read 8 lines
09 |       1  alpha
10 |       2  bravo
11 |       3  charlie
12 |       4  delta
13 |       5  echo
14 |       … +4 lines
15 |
16 | ⏺ The tool results are settled.
17 |
18 |
19 |
20 |
21 |
22 |
23 |
24 |
25 | ────────────────────────────────────────────────────────────────────────────────────────────
26 | ❯ message mentat
27 | ────────────────────────────────────────────────────────────────────────────────────────────
28 |   ! not logged in · /login · /private/tmp/menta… · openai/gpt-5… · ! full acce… ? for sho…|}]

let%expect_test "extension preview clamps rows and truncates wide lines" =
  let row_text =
    List.init 10 (fun index -> Printf.sprintf "row %d" (index + 1))
    |> String.concat "\n"
  in
  let wide_text =
    "wide " ^ String.make 4_096 'x' ^ "\nvisible after the former byte cap"
  in
  let rows = Tool.Output.make ~text:row_text () in
  let wide = Tool.Output.make ~text:wide_text () in
  run ~name:"t" ~size:(92, 32) ~prompt:"inspect extension output"
    [
      completed ~id:"rows" ~name:"extension_rows" rows;
      completed ~id:"wide" ~name:"extension_wide" wide;
    ];
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ inspect extension output
06 |
07 | ⏺ Extension_rows
08 |   ⎿  done
09 |       row 1
10 |       row 2
11 |       row 3
12 |       row 4
13 |       row 5
14 |       … +5 lines
15 |
16 | ⏺ Extension_wide
17 |   ⎿  done
18 |       wide xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx…
19 |       visible after the former byte cap
20 |
21 | ⏺ The tool results are settled.
22 |
23 |
24 |
25 |
26 |
27 |
28 |
29 | ────────────────────────────────────────────────────────────────────────────────────────────
30 | ❯ message mentat
31 | ────────────────────────────────────────────────────────────────────────────────────────────
32 |   ! not logged in · /login · /private/tmp/menta… · openai/gpt-5… · ! full acce… ? for sho…|}]

let%expect_test "compact built-in facts retain summaries and Process output" =
  let search =
    encoded Output.Search.jsont (Output.Search.matches ~total:7 ~files:3)
  in
  let write = encoded Output.Write.jsont (Output.Write.wrote ~lines:6) in
  let update =
    encoded Output.Update.jsont
      (Output.Update.make ~disposition:Output.Update.Applied ~files:2
         ~additions:(Some 8) ~deletions:(Some 3) ~skipped:1)
  in
  let process =
    (* More rows than the cap proves the preview clamps to the last edge and
       marks the hidden head, rather than growing the block or scrolling. *)
    let text =
      List.init 7 (fun index ->
          Printf.sprintf "shell output row %d" (index + 1))
      |> String.concat "\n"
    in
    encoded ~text Output.Process.jsont
      (Output.Process.make ~termination:(Output.Process.Exited 3)
         ~duration_ms:15)
  in
  let type_at =
    encoded Output.Ocaml.Type_at.jsont
      (Output.Ocaml.Type_at.make ~head:"int -> string" ~more:2)
  in
  run ~name:"t" ~size:(92, 34) ~prompt:"summarize the tool facts"
    [
      completed ~id:"search" ~name:"search_text" search;
      completed ~id:"write" ~name:"write_file" write;
      completed ~id:"update" ~name:"apply_patch" update;
      completed ~id:"process" ~name:"shell" process;
      completed ~id:"type" ~name:"ocaml_type_at" type_at;
    ];
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ summarize the tool facts
06 |
07 | ⏺ Search
08 |   ⎿  Found 7 matches in 3 files
09 |
10 | ⏺ Create
11 |   ⎿  Wrote 6 lines
12 |
13 | ⏺ Patch
14 |   ⎿  Updated 2 files (+8, -3, 1 skipped)
15 |
16 | ⏺ Shell
17 |   ⎿  Exited with code 3 in 15 ms
18 |       … +2 lines
19 |       shell output row 3
20 |       shell output row 4
21 |       shell output row 5
22 |       shell output row 6
23 |       shell output row 7
24 |
25 | ⏺ OCaml Type
26 |   ⎿  int -> string (+2 more)
27 |
28 | ⏺ The tool results are settled.
29 |
30 |
31 | ────────────────────────────────────────────────────────────────────────────────────────────
32 | ❯ message mentat
33 | ────────────────────────────────────────────────────────────────────────────────────────────
34 |   ! not logged in · /login · /private/tmp/menta… · openai/gpt-5… · ! full acce… ? for sho…|}]

(* The shell and eval transcripts a real run stores in {!Mentat_tool.Output}:
   a metadata envelope, then the captured streams under fixed [stdout:] and
   [stderr:] labels. The preview must show only the captured output. *)
let shell_transcript ~command ~status ~stdout ~stderr =
  Printf.sprintf
    "Command: %s\n\
     Workdir: /workspace\n\
     Status: %s\n\
     Duration: 22ms\n\
     Timeout: 60000ms\n\
     Sandbox: not_requested\n\n\
     stdout:\n\
     %s\n\
     stderr:\n\
     %s"
    command status stdout stderr

let shell_output ~command ~status ~termination ~stdout ~stderr =
  encoded
    ~text:(shell_transcript ~command ~status ~stdout ~stderr)
    Output.Process.jsont
    (Output.Process.make ~termination ~duration_ms:22)

let%expect_test "shell preview shows captured output without envelope or labels"
    =
  let single =
    shell_output ~command:"pwd" ~status:"exited 0"
      ~termination:(Output.Process.Exited 0) ~stdout:"/home/user/project"
      ~stderr:""
  in
  let both =
    shell_output ~command:"build" ~status:"exited 1"
      ~termination:(Output.Process.Exited 1) ~stdout:"compiling\ndone"
      ~stderr:"warning: unused binding\nError: build failed"
  in
  let silent =
    shell_output ~command:"true" ~status:"exited 0"
      ~termination:(Output.Process.Exited 0) ~stdout:"" ~stderr:""
  in
  run ~name:"t" ~size:(92, 30) ~prompt:"run shell commands"
    [
      completed ~id:"single" ~name:"shell" single;
      completed ~id:"both" ~name:"shell" both;
      completed ~id:"silent" ~name:"shell" silent;
    ];
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ run shell commands
06 |
07 | ⏺ Shell
08 |   ⎿  Completed in 22 ms
09 |       /home/user/project
10 |
11 | ⏺ Shell
12 |   ⎿  Exited with code 1 in 22 ms
13 |       compiling
14 |       done
15 |       warning: unused binding
16 |       Error: build failed
17 |
18 | ⏺ Shell
19 |   ⎿  Completed in 22 ms
20 |
21 | ⏺ The tool results are settled.
22 |
23 |
24 |
25 |
26 |
27 | ────────────────────────────────────────────────────────────────────────────────────────────
28 | ❯ message mentat
29 | ────────────────────────────────────────────────────────────────────────────────────────────
30 |   ! not logged in · /login · /private/tmp/menta… · openai/gpt-5… · ! full acce… ? for sho…|}]

let%expect_test
    "preview shows a lone overflow line instead of a plus-one marker" =
  let exactly_one_over =
    (* Six rows against the five-row cap: the marker would cost the same row as
       the sixth line, so the sixth line is shown and no marker appears. *)
    shell_output ~command:"seq 6" ~status:"exited 0"
      ~termination:(Output.Process.Exited 0)
      ~stdout:
        (String.concat "\n"
           (List.init 6 (fun i -> Printf.sprintf "%d" (i + 1))))
      ~stderr:""
  in
  run ~name:"t" ~size:(92, 22) ~prompt:"count to six"
    [ completed ~id:"seq" ~name:"shell" exactly_one_over ];
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ count to six
06 |
07 | ⏺ Shell
08 |   ⎿  Completed in 22 ms
09 |       1
10 |       2
11 |       3
12 |       4
13 |       5
14 |       6
15 |
16 | ⏺ The tool results are settled.
17 |
18 |
19 | ────────────────────────────────────────────────────────────────────────────────────────────
20 | ❯ message mentat
21 | ────────────────────────────────────────────────────────────────────────────────────────────
22 |   ! not logged in · /login · /private/tmp/menta… · openai/gpt-5… · ! full acce… ? for sho…|}]

let%expect_test "failed and interrupted partial results settle and wrap" =
  let malformed =
    Tool.Output.make
      ~text:("MUST NOT RENDER " ^ String.make 2_000 'x')
      ~json:(Json.string "not a read summary")
      ~truncated:true ()
  in
  let partial_search =
    encoded ~text:"model-only partial matches" Output.Search.jsont
      (Output.Search.matches ~total:3 ~files:2)
  in
  let failed =
    Tool.Result.failed ~output:malformed `Not_found
      ("The requested read path is deeply nested and unavailable; inspect the "
     ^ "workspace root before retrying UNIQUEENDTOKEN.")
  in
  let interrupted =
    Tool.Result.interrupted ~output:partial_search
      ~reason:"Stopped after the caller changed direction." ~cancelled:true ()
  in
  let prompt = "inspect partial tool outcomes" in
  let turn =
    Tui.Turn_script.complete ~prompt
      ~tools:
        [
          settled ~id:"failed" ~name:"read_file" failed;
          settled ~id:"interrupted" ~name:"search_text" interrupted;
        ]
      "The partial outcomes are recorded."
  in
  Tui.run
    ~home:(fun _ -> None)
    ~name:"t" ~size:(72, 28) ~turns:[ turn ]
    (fun t -> submit t prompt);
  [%expect
    {|01 |
02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  $TESTCASE_ROOT
04 |
05 | ❯ inspect partial tool outcomes
06 |
07 | ⏺ Read
08 |   ⎿  The requested read path is deeply nested and unavailable; inspect
09 |      the workspace root before retrying UNIQUEENDTOKEN.
10 |
11 |      details unavailable · not found · partial output truncated
12 |
13 | ⏺ Search
14 |   ⎿  Stopped after the caller changed direction.
15 |
16 |      Found 3 matches in 2 files · cancelled
17 |
18 | ⏺ The partial outcomes are recorded.
19 |
20 |
21 |
22 |
23 |
24 |
25 | ────────────────────────────────────────────────────────────────────────
26 | ❯ message mentat
27 | ────────────────────────────────────────────────────────────────────────
28 |   ! not logged in · /login · /priv… · openai/gpt… · ! full access ? f…|}]

[%%run_tests "mentat.tui.tool_output"]
