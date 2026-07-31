(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
open Test_support
module Diagnostic = Mentat_diagnostic

let diagnostic =
  Testable.make
    ~pp:(fun ppf d -> Format.pp_print_string ppf (Diagnostic.to_string d))
    ~equal:Diagnostic.equal

let make_invariants () =
  expect_invalid_arg "empty message raises" (fun () -> Diagnostic.make "");
  expect_invalid_arg "message rejects LF" (fun () ->
      Diagnostic.make "unknown\nkey");
  expect_invalid_arg "message rejects CR" (fun () ->
      Diagnostic.make "unknown\rkey");
  expect_invalid_arg "empty context raises" (fun () ->
      Diagnostic.make ~context:"" "unknown key");
  expect_invalid_arg "empty hint raises" (fun () ->
      Diagnostic.make ~hints:[ "first"; "" ] "unknown key");
  expect_invalid_arg "hint rejects LF" (fun () ->
      Diagnostic.make ~hints:[ "first\nsecond" ] "unknown key");
  expect_invalid_arg "hint rejects CR" (fun () ->
      Diagnostic.make ~hints:[ "first\rsecond" ] "unknown key");
  expect_invalid_arg "empty suggest candidate raises" (fun () ->
      Diagnostic.suggest [ "build"; "" ]);
  expect_invalid_arg "suggest candidate rejects LF" (fun () ->
      Diagnostic.suggest [ "build\nplan" ]);
  expect_invalid_arg "suggest candidate rejects CR" (fun () ->
      Diagnostic.suggest [ "build\rplan" ]);
  expect_invalid_arg "did-you-mean validates all candidates" (fun () ->
      Diagnostic.did_you_mean "deploy" ~candidates:[ "" ]);
  expect_invalid_arg "did-you-mean candidate rejects LF" (fun () ->
      Diagnostic.did_you_mean "build" ~candidates:[ "build\nplan" ]);
  expect_invalid_arg "did-you-mean candidate rejects CR" (fun () ->
      Diagnostic.did_you_mean "build" ~candidates:[ "build\rplan" ])

let rendering () =
  let plain = Diagnostic.make "unknown key" in
  equal string ~msg:"message only" "unknown key" (Diagnostic.to_string plain);
  let with_context =
    Diagnostic.make ~context:"in project config" "unknown key"
  in
  equal string ~msg:"context follows message on its own line"
    "unknown key\nin project config"
    (Diagnostic.to_string with_context);
  let with_hints = Diagnostic.make ~hints:[ "first"; "second" ] "unknown key" in
  equal string ~msg:"one Hint line per hint"
    "unknown key\nHint: first\nHint: second"
    (Diagnostic.to_string with_hints);
  let full =
    Diagnostic.make ~context:"in project config" ~hints:[ "first" ]
      "unknown key"
  in
  equal string ~msg:"context precedes hints"
    "unknown key\nin project config\nHint: first"
    (Diagnostic.to_string full)

let opaque_text () =
  equal string ~msg:"single-line opaque text" "malformed config"
    (Diagnostic.to_string (Diagnostic.of_text "malformed config"));
  equal string ~msg:"first line becomes message, tail becomes context"
    "malformed config\nFile \"-\", line 1\nFile \"-\": in member model"
    (Diagnostic.to_string
       (Diagnostic.of_text
          "malformed config\nFile \"-\", line 1\nFile \"-\": in member model"));
  equal string ~msg:"crlf separator is treated as one line break"
    "malformed config\nFile \"-\", line 1"
    (Diagnostic.to_string
       (Diagnostic.of_text "malformed config\r\nFile \"-\", line 1"));
  equal string ~msg:"single trailing newline is ignored" "malformed config"
    (Diagnostic.to_string (Diagnostic.of_text "malformed config\n"));
  equal string ~msg:"hints still render after context"
    "malformed config\nFile \"-\", line 1\nHint: fix config.json"
    (Diagnostic.to_string
       (Diagnostic.of_text ~hints:[ "fix config.json" ]
          "malformed config\nFile \"-\", line 1"));
  expect_invalid_arg "empty opaque text raises" (fun () ->
      Diagnostic.of_text "");
  expect_invalid_arg "empty first line raises" (fun () ->
      Diagnostic.of_text "\ncontext")

let of_text_or () =
  equal string ~msg:"empty text falls back to the fallback message" "fell back"
    (Diagnostic.to_string (Diagnostic.of_text_or ~fallback:"fell back" ""));
  equal string
    ~msg:"blank first line falls back, keeping the full text as context"
    "fell back\n\ndetail line"
    (Diagnostic.to_string
       (Diagnostic.of_text_or ~fallback:"fell back" "\ndetail line"));
  equal string ~msg:"usable text passes through unchanged"
    "real message\ncontext detail"
    (Diagnostic.to_string
       (Diagnostic.of_text_or ~fallback:"fell back"
          "real message\ncontext detail"));
  equal string ~msg:"hints thread through the fallback branch"
    "fell back\nHint: retry"
    (Diagnostic.to_string
       (Diagnostic.of_text_or ~fallback:"fell back" ~hints:[ "retry" ] ""));
  equal string ~msg:"hints thread through the passthrough branch"
    "real message\nHint: retry"
    (Diagnostic.to_string
       (Diagnostic.of_text_or ~fallback:"fell back" ~hints:[ "retry" ]
          "real message"))

let suggest () =
  equal (list string) ~msg:"empty candidates make no hint" []
    (Diagnostic.suggest []);
  equal (list string) ~msg:"one candidate" [ "did you mean build?" ]
    (Diagnostic.suggest [ "build" ]);
  equal (list string) ~msg:"two candidates joined with or"
    [ "did you mean build or plan?" ]
    (Diagnostic.suggest [ "build"; "plan" ]);
  equal (list string) ~msg:"three candidates use commas then or"
    [ "did you mean build, plan or review?" ]
    (Diagnostic.suggest [ "build"; "plan"; "review" ])

let did_you_mean () =
  let candidates = [ "build"; "plan"; "review" ] in
  equal (list string) ~msg:"distance one becomes a hint"
    [ "did you mean build?" ]
    (Diagnostic.did_you_mean "buld" ~candidates);
  equal (list string) ~msg:"distance two becomes a hint"
    [ "did you mean plan?" ]
    (Diagnostic.did_you_mean "pn" ~candidates);
  equal (list string) ~msg:"distance three is too far" []
    (Diagnostic.did_you_mean "rvw" ~candidates);
  equal (list string) ~msg:"close match becomes a hint"
    [ "did you mean build?" ]
    (Diagnostic.did_you_mean "biuld" ~candidates);
  equal (list string) ~msg:"several close matches share one hint"
    [ "did you mean bat or cat?" ]
    (Diagnostic.did_you_mean "hat" ~candidates:[ "bat"; "moon"; "cat" ]);
  equal (list string) ~msg:"candidate order is preserved"
    [ "did you mean abcde or abz?" ]
    (Diagnostic.did_you_mean "abc" ~candidates:[ "abcde"; "abz" ]);
  equal (list string) ~msg:"no close match means no hint" []
    (Diagnostic.did_you_mean "deploy" ~candidates);
  equal (list string) ~msg:"impossible length differences make no hint" []
    (Diagnostic.did_you_mean "very-long-mode-name"
       ~candidates:[ "go"; "run"; "ask" ]);
  equal (list string) ~msg:"exact match is never suggested" []
    (Diagnostic.did_you_mean "plan" ~candidates);
  equal (list string) ~msg:"empty candidates make no hint" []
    (Diagnostic.did_you_mean "anything" ~candidates:[]);
  equal (list string) ~msg:"empty input matches short candidates"
    [ "did you mean go?" ]
    (Diagnostic.did_you_mean "" ~candidates:[ "go"; "build" ]);
  let rendered =
    Diagnostic.to_string
      (Diagnostic.make
         ~hints:(Diagnostic.did_you_mean "biuld" ~candidates)
         "unknown workflow mode: biuld")
  in
  check "hint feeds make and reaches the rendered output"
    (Option.is_some
       (String.find_first ~sub:"Hint: did you mean build?" rendered))

let equality () =
  let full =
    Diagnostic.make ~context:"in project config" ~hints:[ "first" ]
      "unknown key"
  in
  is_true ~msg:"equal fields are equal"
    (Diagnostic.equal full
       (Diagnostic.make ~context:"in project config" ~hints:[ "first" ]
          "unknown key"));
  is_false ~msg:"message participates"
    (Diagnostic.equal (Diagnostic.make "a") (Diagnostic.make "b"));
  is_false ~msg:"context participates"
    (Diagnostic.equal (Diagnostic.make "a") (Diagnostic.make ~context:"c" "a"));
  is_false ~msg:"hints participate"
    (Diagnostic.equal (Diagnostic.make "a")
       (Diagnostic.make ~hints:[ "h" ] "a"));
  is_false ~msg:"hint order participates"
    (Diagnostic.equal
       (Diagnostic.make ~hints:[ "h1"; "h2" ] "a")
       (Diagnostic.make ~hints:[ "h2"; "h1" ] "a"))

let codec () =
  let full =
    Diagnostic.make ~context:"in project config" ~hints:[ "first"; "second" ]
      "unknown key"
  in
  check "wire shape carries every field"
    (Json.equal
       (json_object
          [
            ("message", Json.string "unknown key");
            ("context", Json.string "in project config");
            ("hints", json_array [ Json.string "first"; Json.string "second" ]);
          ])
       (encode Diagnostic.jsont full));
  let bare = Diagnostic.make "unknown key" in
  check "absent context and empty hints are omitted, never null"
    (Json.equal
       (json_object [ ("message", Json.string "unknown key") ])
       (encode Diagnostic.jsont bare));
  equal diagnostic ~msg:"decode inverts encode" full
    (decode Diagnostic.jsont (encode Diagnostic.jsont full));
  equal diagnostic ~msg:"a missing hints member decodes as no hints" bare
    (decode Diagnostic.jsont
       (json_object [ ("message", Json.string "unknown key") ]));
  let multiline = Diagnostic.make ~context:"line one\nline two" "unknown key" in
  equal diagnostic ~msg:"multi-line context round-trips" multiline
    (decode Diagnostic.jsont (encode Diagnostic.jsont multiline));
  equal string ~msg:"a decoded diagnostic renders as the encoded one"
    (Diagnostic.to_string full)
    (Diagnostic.to_string
       (decode Diagnostic.jsont (encode Diagnostic.jsont full)));
  let rejects msg json =
    is_true ~msg (Result.is_error (Json.decode Diagnostic.jsont json))
  in
  rejects "not an object" (Json.string "unknown key");
  rejects "missing message" (json_object []);
  rejects "empty message" (json_object [ ("message", Json.string "") ]);
  rejects "multi-line message" (json_object [ ("message", Json.string "a\nb") ]);
  rejects "empty context"
    (json_object [ ("message", Json.string "m"); ("context", Json.string "") ]);
  rejects "null context"
    (json_object [ ("message", Json.string "m"); ("context", Json.null ()) ]);
  rejects "empty hint"
    (json_object
       [
         ("message", Json.string "m"); ("hints", json_array [ Json.string "" ]);
       ]);
  rejects "multi-line hint"
    (json_object
       [
         ("message", Json.string "m");
         ("hints", json_array [ Json.string "a\nb" ]);
       ]);
  rejects "hints wrong type"
    (json_object [ ("message", Json.string "m"); ("hints", Json.string "h") ]);
  rejects "unknown member"
    (json_object [ ("message", Json.string "m"); ("extra", Json.int 1) ])

let () =
  run "mentat.diagnostic"
    [
      test "constructor validates invariants" make_invariants;
      test "renders message, context, and hints" rendering;
      test "converts opaque text to message and context" opaque_text;
      test "total opaque-text constructor falls back" of_text_or;
      test "formats suggestion hints" suggest;
      test "suggests close matches" did_you_mean;
      test "structural equality" equality;
      test "round-trips through jsont" codec;
    ]
