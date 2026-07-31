(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Unit suite for [mentat_tool], the typed, effect-neutral waist between
   provider JSON and a live OCaml callback. The suite pins the
   library's durable and boundary contracts: canonical input as the single
   stored claim shape, the decode-once permission flow, output erasure with the
   durable semantic JSON, the terminal result algebra and its
   version-pinned [to_model] lowering, upstream-declaration identity, the shared
   execution stage, and the staged [Prepared.t] lifecycle with its resume gate.

   Everything is tested through the [Mentat_tool] facade. A prepared value
   is only obtainable by running a staged call — [Prepared] exposes no
   constructor — so every staged test drives the same path production does.
   Tampered envelopes are produced by editing the durable JSON and decoding it
   back through [Prepared.jsont], which is exactly the journal-edit surface the
   resume gate guards. *)

open Windtrap
open Test_support
module Tool = Mentat_tool
module Json = Jsont.Json

(* JSON helpers. *)

let rec pp_json ppf = function
  | Jsont.Null _ -> Format.pp_print_string ppf "null"
  | Jsont.Bool (b, _) -> Format.pp_print_bool ppf b
  | Jsont.Number (n, _) -> Format.pp_print_float ppf n
  | Jsont.String (s, _) -> Format.fprintf ppf "%S" s
  | Jsont.Array (elements, _) ->
      Format.fprintf ppf "[@[%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
           pp_json)
        elements
  | Jsont.Object (members, _) ->
      let pp_member ppf ((name, _), value) =
        Format.fprintf ppf "%S: %a" name pp_json value
      in
      Format.fprintf ppf "{@[%a@]}"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
           pp_member)
        members

(* Order-sensitive structural equality: the oracle for "the stored form has a
   fixed member order". [Jsont.Json.equal] is order-insensitive, so it cannot
   distinguish a canonical value from a reordering of it. *)
let rec ordered_equal a b =
  match (a, b) with
  | Jsont.Null _, Jsont.Null _ -> true
  | Jsont.Bool (a, _), Jsont.Bool (b, _) -> Bool.equal a b
  | Jsont.Number (a, _), Jsont.Number (b, _) -> Float.equal a b
  | Jsont.String (a, _), Jsont.String (b, _) -> String.equal a b
  | Jsont.Array (a, _), Jsont.Array (b, _) -> List.equal ordered_equal a b
  | Jsont.Object (a, _), Jsont.Object (b, _) ->
      List.equal
        (fun ((na, _), va) ((nb, _), vb) ->
          String.equal na nb && ordered_equal va vb)
        a b
  | _, _ -> false

let json_value = Testable.make ~pp:pp_json ~equal:Json.equal
let canonical_json_value = Testable.make ~pp:pp_json ~equal:ordered_equal

let member name = function
  | Jsont.Object (members, _) -> (
      match List.find_opt (fun ((n, _), _) -> String.equal n name) members with
      | Some (_, value) -> value
      | None -> failf "missing %S member" name)
  | _ -> fail "expected a JSON object"

let member_names = function
  | Jsont.Object (members, _) -> List.map (fun ((n, _), _) -> n) members
  | _ -> fail "expected a JSON object"

let set_member name value = function
  | Jsont.Object (members, meta) ->
      let hit = ref false in
      let members =
        List.map
          (fun (((n, _) as key), v) ->
            if String.equal n name then begin
              hit := true;
              (key, value)
            end
            else (key, v))
          members
      in
      if not !hit then failf "no %S member to set" name;
      Jsont.Object (members, meta)
  | _ -> fail "expected a JSON object"

let add_member name value = function
  | Jsont.Object (members, meta) ->
      Jsont.Object (members @ [ Json.mem (Json.name name) value ], meta)
  | _ -> fail "expected a JSON object"

let decode_rejects ~msg ?contains codec json =
  match Json.decode codec json with
  | Ok _ -> failf "%s: decode unexpectedly succeeded" msg
  | Error message -> (
      match contains with
      | None -> ()
      | Some affix ->
          if not (String.includes ~affix message) then
            failf "%s: diagnostic %S does not mention %S" msg message affix)

(* Fixtures. *)

(* Cancellation is now a labeled closure the callback polls, not a context
   value. Most tests never cancel. *)
let no_cancel () = false

let request_for text =
  Mentat_permission.Request.of_accesses
    [
      Mentat_permission.Access.shell
        ~cwd:(Mentat_permission.Access.Path_scope.unknown "test-cwd")
        ~execution:Mentat_permission.Access.Command.Direct text;
    ]

let request_value =
  Testable.make ~pp:Mentat_permission.Request.pp ~equal:Mentat_permission.Request.equal

let echo_schema =
  json_object
    [
      ("type", Json.string "object");
      ( "properties",
        json_object [ ("text", json_object [ ("type", Json.string "string") ]) ]
      );
      ("required", json_array [ Json.string "text" ]);
      ("additionalProperties", Json.bool false);
    ]

let text_codec =
  Jsont.Object.map ~kind:"echo input" Fun.id
  |> Jsont.Object.mem "text" Jsont.string ~enc:Fun.id
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let text_input = Tool.Input.make text_codec ~schema:echo_schema
let input_json text = json_object [ ("text", Json.string text) ]
let output_of_string s = Tool.Output.make ~text:s ~json:(Json.string s) ()

let echo_tool ?permissions
    ?(run = fun ~cancelled:_ input -> Tool.Result.completed ~output:input ()) ()
    =
  Tool.make ~name:"echo" ~description:"Echo input text." ~input:text_input
    ~output:output_of_string ?permissions ~run ()

let decl_name t = Mentat_llm.Tool.name (Tool.declaration t)

let decl_description t =
  Option.value ~default:"" (Mentat_llm.Tool.description (Tool.declaration t))

let decl_schema t = Mentat_llm.Tool.input_schema (Tool.declaration t)

let model_call ?(id = "call-1") ?name ?signature tool input =
  let name = Option.value name ~default:(decl_name tool) in
  Mentat_llm.Tool.Call.make ~id ~name ~input ?signature ()

let decode_ok tool json =
  match Tool.Call.decode tool (model_call tool json) with
  | Ok call -> call
  | Error error ->
      failf "decode failed: %s" (Tool.Call.Decode_error.message error)

let finished = function
  | Tool.Call.Finished result -> result
  | Tool.Call.Prepared _ -> fail "expected a finished outcome"

let staged_prepared = function
  | Tool.Call.Prepared prepared -> prepared
  | Tool.Call.Finished _ -> fail "expected a prepared outcome"

let expect_stage ~msg expected call =
  equal string ~msg
    (Tool.Stage.to_string expected)
    (Tool.Stage.to_string (Tool.Call.stage call))

(* A pass-through input contract: [Jsont.json] decodes any value to itself, so
   [Call.input] exposes the canonicalization of arbitrary provider JSON. *)
let passthrough_tool =
  Tool.make ~name:"passthrough" ~description:"Accept any JSON value."
    ~input:
      (Tool.Input.make Jsont.json
         ~schema:(json_object [ ("type", Json.string "object") ]))
    ~output:(fun (_ : Jsont.json) -> Tool.Output.make ~text:"ok" ())
    ~run:(fun ~cancelled:_ json -> Tool.Result.completed ~output:json ())
    ()

let canonical_of json = Tool.Call.input (decode_ok passthrough_tool json)

(* The staged fixture plan. The codec declares "target" before "count" so the
   canonical (sorted) payload order differs from the codec's encode order. *)
type plan = { target : string; count : int }

let plan_equal a b = String.equal a.target b.target && Int.equal a.count b.count

let plan_jsont =
  let make target count = { target; count } in
  Jsont.Object.map ~kind:"test plan" make
  |> Jsont.Object.mem "target" Jsont.string ~enc:(fun p -> p.target)
  |> Jsont.Object.mem "count" Jsont.int ~enc:(fun p -> p.count)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let describe_plan p = Printf.sprintf "write %s %d times" p.target p.count

let staged_tool ?(name = "staged") ?(prepare_permissions = fun _ -> [])
    ?(permissions = fun plan -> [ request_for plan.target ])
    ?(output = output_of_string)
    ?(prepare =
      fun ~cancelled:_ input ->
        `Prepared { target = input; count = String.length input })
    ?(run =
      fun ~cancelled:_ plan ->
        Tool.Result.completed ~output:(describe_plan plan) ()) () =
  Tool.make_staged ~name ~description:"Stage a plan before running it."
    ~input:text_input ~prepared:plan_jsont ~describe:describe_plan ~output
    ~prepare_permissions ~prepare ~permissions ~run ()

(* Stage a prepared value the only way the facade allows: decode + run. *)
let prepare_value ?(input = "alpha") tool =
  staged_prepared
    (Tool.Call.run (decode_ok tool (input_json input)) ~cancelled:no_cancel)

(* A staged tool over a two-member input, so the stored canonical input has an
   observable member order and reordering tampers are expressible. *)
let wide_codec =
  let make text level = (text, level) in
  Jsont.Object.map ~kind:"wide input" make
  |> Jsont.Object.mem "text" Jsont.string ~enc:fst
  |> Jsont.Object.mem "level" Jsont.int ~enc:snd
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let wide_staged_tool () =
  Tool.make_staged ~name:"wide" ~description:"Stage from a two-member input."
    ~input:
      (Tool.Input.make wide_codec
         ~schema:(json_object [ ("type", Json.string "object") ]))
    ~prepared:plan_jsont ~describe:describe_plan ~output:output_of_string
    ~prepare:(fun ~cancelled:_ (text, level) ->
      `Prepared { target = text; count = level })
    ~permissions:(fun plan -> [ request_for plan.target ])
    ~run:(fun ~cancelled:_ plan ->
      Tool.Result.completed ~output:(describe_plan plan) ())
    ()

let wide_json =
  json_object [ ("text", Json.string "alpha"); ("level", Json.int 2) ]

(* Reverse a JSON object's member list: a semantic no-op that only disturbs
   the canonical member order. *)
let reorder_members = function
  | Jsont.Object (members, meta) -> Jsont.Object (List.rev members, meta)
  | _ -> fail "expected a JSON object"

let prepared_value =
  Testable.make ~pp:(fun ppf p ->
      Format.fprintf ppf "prepared %s: %s" (Tool.Prepared.tool p)
        (Tool.Prepared.description p)) ~equal:Tool.Prepared.equal

let resume_error_label = function
  | Tool.Call.Resume_error.Not_staged -> "not_staged"
  | Tool.Call.Resume_error.Tool_mismatch -> "tool_mismatch"
  | Tool.Call.Resume_error.Input_mismatch -> "input_mismatch"
  | Tool.Call.Resume_error.Invalid_prepared _ -> "invalid_prepared"
  | Tool.Call.Resume_error.Prepared_drift -> "prepared_drift"
  | Tool.Call.Resume_error.Permission_drift -> "permission_drift"

let resume_ok call prepared =
  match Tool.Call.resume call prepared with
  | Ok resumed -> resumed
  | Error error ->
      failf "resume failed: %s" (Tool.Call.Resume_error.message error)

let resume_rejects ~msg label call prepared =
  match Tool.Call.resume call prepared with
  | Ok _ -> failf "%s: expected a %s rejection" msg label
  | Error error -> equal string ~msg label (resume_error_label error)

(* Round-trip a prepared value through its durable envelope, optionally
   tampering with one member on the way — the journal-edit surface. *)
let reload_prepared ?tamper prepared =
  let envelope = encode Tool.Prepared.jsont prepared in
  let envelope = match tamper with None -> envelope | Some f -> f envelope in
  decode Tool.Prepared.jsont envelope

(* Result helpers. *)

let failure_spellings =
  [
    (`Invalid_input, "invalid_input");
    (`Permission_denied, "permission_denied");
    (`Not_found, "not_found");
    (`Stale, "stale");
    (`Unavailable, "unavailable");
    (`Timed_out, "timed_out");
    (`Failed, "failed");
  ]

let failure_label kind = List.assoc kind failure_spellings

let status_equal a b =
  match (a, b) with
  | Tool.Result.Completed, Tool.Result.Completed -> true
  | ( Tool.Result.Failed { kind = ka; message = ma; metadata = da },
      Tool.Result.Failed { kind = kb; message = mb; metadata = db } ) ->
      String.equal (failure_label ka) (failure_label kb)
      && String.equal ma mb
      && Option.equal Json.equal da db
  | ( Tool.Result.Interrupted { reason = ra; cancelled = ca },
      Tool.Result.Interrupted { reason = rb; cancelled = cb } ) ->
      String.equal ra rb && Bool.equal ca cb
  | ( (Tool.Result.Completed | Tool.Result.Failed _ | Tool.Result.Interrupted _),
      _ ) ->
      false

let pp_status ppf = function
  | Tool.Result.Completed -> Format.pp_print_string ppf "completed"
  | Tool.Result.Failed { kind; message; _ } ->
      Format.fprintf ppf "failed[%s] %S" (failure_label kind) message
  | Tool.Result.Interrupted { reason; cancelled } ->
      Format.fprintf ppf "interrupted %S cancelled:%b" reason cancelled

let string_result =
  Testable.make ~pp:(fun ppf r ->
      Format.fprintf ppf "%a output:%a" pp_status (Tool.Result.status r)
        (Format.pp_print_option Format.pp_print_string)
        (Tool.Result.output r)) ~equal:(Tool.Result.equal String.equal)

let output_value =
  Testable.make ~pp:(fun ppf o ->
      Format.fprintf ppf "output %S truncated:%b" (Tool.Output.text o)
        (Tool.Output.truncated o)) ~equal:Tool.Output.equal

let output_result =
  Testable.make ~pp:(fun ppf r -> pp_status ppf (Tool.Result.status r)) ~equal:(Tool.Result.equal Tool.Output.equal)

(* Identity. *)

let identity_group =
  group "identity"
    [
      test "declaration is the upstream model-visible identity" (fun () ->
          let echo = echo_tool () in
          let declaration = Tool.declaration echo in
          equal string ~msg:"name" "echo" (Mentat_llm.Tool.name declaration);
          equal (option string) ~msg:"description" (Some "Echo input text.")
            (Mentat_llm.Tool.description declaration);
          equal json_value ~msg:"input schema retained" echo_schema
            (Mentat_llm.Tool.input_schema declaration);
          is_true ~msg:"identity is stable across constructions"
            (Mentat_llm.Tool.equal declaration
               (Tool.declaration (echo_tool ())));
          (* Identity is the declaration; whether a tool is staged is an
             execution shape, not identity. *)
          let staged_echo =
            Tool.make_staged ~name:"echo" ~description:"Echo input text."
              ~input:text_input ~prepared:plan_jsont ~describe:describe_plan
              ~output:output_of_string
              ~prepare:(fun ~cancelled:_ input ->
                `Prepared { target = input; count = 0 })
              ~run:(fun ~cancelled:_ plan ->
                Tool.Result.completed ~output:(describe_plan plan) ())
              ()
          in
          is_true ~msg:"stagedness does not change identity"
            (Mentat_llm.Tool.equal declaration (Tool.declaration staged_echo)));
      test "declaration drift breaks identity" (fun () ->
          let base = Tool.declaration (echo_tool ()) in
          let renamed =
            Tool.make ~name:"other" ~description:"Echo input text."
              ~input:text_input ~output:output_of_string
              ~run:(fun ~cancelled:_ v -> Tool.Result.completed ~output:v ())
              ()
          in
          is_false ~msg:"name drift"
            (Mentat_llm.Tool.equal base (Tool.declaration renamed));
          let redescribed =
            Tool.make ~name:"echo" ~description:"A different description."
              ~input:text_input ~output:output_of_string
              ~run:(fun ~cancelled:_ v -> Tool.Result.completed ~output:v ())
              ()
          in
          is_false ~msg:"description drift"
            (Mentat_llm.Tool.equal base (Tool.declaration redescribed));
          let reschemad =
            Tool.make ~name:"echo" ~description:"Echo input text."
              ~input:Tool.Input.empty ~output:output_of_string
              ~run:(fun ~cancelled:_ () ->
                Tool.Result.completed ~output:"ok" ())
              ()
          in
          is_false ~msg:"schema drift"
            (Mentat_llm.Tool.equal base (Tool.declaration reschemad)));
    ]

(* Cancellation. *)

let cancellation_group =
  group "cancellation"
    [
      test "the labeled cancelled source reaches the callback live" (fun () ->
          let flag = ref false in
          let tool =
            echo_tool
              ~run:(fun ~cancelled input ->
                if cancelled () then
                  Tool.Result.interrupted ~reason:"cancelled mid-run"
                    ~cancelled:true ()
                else Tool.Result.completed ~output:input ())
              ()
          in
          let call = decode_ok tool (input_json "job") in
          (match
             Tool.Result.status
               (finished (Tool.Call.run call ~cancelled:(fun () -> !flag)))
           with
          | Tool.Result.Completed -> ()
          | status -> failf "unexpected status: %a" pp_status status);
          flag := true;
          match
            Tool.Result.status
              (finished (Tool.Call.run call ~cancelled:(fun () -> !flag)))
          with
          | Tool.Result.Interrupted { cancelled = true; _ } -> ()
          | status -> failf "unexpected status: %a" pp_status status);
    ]

(* Input. *)

let input_group =
  group "input"
    [
      test "make rejects a non-object schema" (fun () ->
          raises
            (Invalid_argument "Mentat_tool.Input.make: schema must be a JSON object") (fun () ->
              Tool.Input.make Jsont.string ~schema:(Json.string "bad")));
      test "schema is retained unchanged" (fun () ->
          equal json_value ~msg:"schema" echo_schema
            (Tool.Input.schema text_input));
      test "decode and re-encode use the runtime codec" (fun () ->
          equal (result string pass) ~msg:"decodes provider JSON" (Ok "hello")
            (Tool.Input.decode text_input (input_json "hello"));
          equal json_value ~msg:"re-encoding is semantically faithful"
            (input_json "hello")
            (Tool.Input.encode text_input "hello");
          (match Tool.Input.decode text_input (json_object []) with
          | Ok _ -> fail "decode of a missing member should fail"
          | Error diagnostic ->
              is_true ~msg:"diagnostic is populated"
                (not (String.is_empty diagnostic));
              is_false ~msg:"diagnostic carries no ANSI escapes"
                (String.contains diagnostic '\x1b'));
          is_true ~msg:"unknown members are rejected"
            (Stdlib.Result.is_error
               (Tool.Input.decode text_input
                  (add_member "extra" (Json.bool true) (input_json "hi")))));
      test "empty reuses the upstream no-input schema" (fun () ->
          equal json_value ~msg:"empty schema is the upstream one"
            Mentat_llm.Tool.no_input_schema
            (Tool.Input.schema Tool.Input.empty);
          equal (result unit pass) ~msg:"accepts the empty object" (Ok ())
            (Tool.Input.decode Tool.Input.empty (Json.object' []));
          is_true ~msg:"rejects members"
            (Stdlib.Result.is_error
               (Tool.Input.decode Tool.Input.empty
                  (json_object [ ("x", Json.bool true) ])));
          is_true ~msg:"rejects non-objects"
            (Stdlib.Result.is_error
               (Tool.Input.decode Tool.Input.empty (Json.string "no"))));
      test "encode failure is a construction defect" (fun () ->
          let broken =
            Tool.Input.make
              (Jsont.map ~kind:"unencodable input" ~dec:Fun.id
                 ~enc:(fun (_ : string) ->
                   Jsont.Error.msg Jsont.Meta.none "cannot re-encode")
                 Jsont.string)
              ~schema:(json_object [ ("type", Json.string "string") ])
          in
          expect_invalid_arg "encode raises Invalid_argument" (fun () ->
              Tool.Input.encode broken "value"));
    ]

(* Output. *)

let output_group =
  group "output"
    [
      test "accessors and defaults" (fun () ->
          let plain = Tool.Output.make ~text:"done" () in
          equal string ~msg:"text" "done" (Tool.Output.text plain);
          equal (option json_value) ~msg:"json defaults to none" None
            (Tool.Output.json plain);
          is_false ~msg:"truncated defaults to false"
            (Tool.Output.truncated plain);
          let structured =
            Tool.Output.make ~text:"trimmed" ~json:(Json.string "trimmed")
              ~truncated:true ()
          in
          equal (option json_value) ~msg:"json is retained"
            (Some (Json.string "trimmed"))
            (Tool.Output.json structured);
          is_true ~msg:"truncated is retained"
            (Tool.Output.truncated structured));
      test "requires non-empty text but accepts whitespace" (fun () ->
          raises (Invalid_argument "Mentat_tool.Output.make: text must not be empty")
            (fun () -> Tool.Output.make ~text:"" ());
          equal string ~msg:"whitespace-only text is accepted" " "
            (Tool.Output.text (Tool.Output.make ~text:" " ())));
      test "equal covers every output projection" (fun () ->
          let output = Tool.Output.make ~text:"done" () in
          is_true ~msg:"identical outputs are equal"
            (Tool.Output.equal output (Tool.Output.make ~text:"done" ()));
          is_false ~msg:"text participates"
            (Tool.Output.equal output (Tool.Output.make ~text:"other" ()));
          is_false ~msg:"truncation participates"
            (Tool.Output.equal output
               (Tool.Output.make ~text:"done" ~truncated:true ()));
          is_false ~msg:"structured json participates"
            (Tool.Output.equal output
               (Tool.Output.make ~text:"done" ~json:(Json.bool true) ())));
      test "jsont round-trips the complete output shape" (fun () ->
          let structured_json = json_object [ ("ok", Json.bool true) ] in
          let output =
            Tool.Output.make ~text:"done" ~json:structured_json ~truncated:true
              ()
          in
          let encoded = encode Tool.Output.jsont output in
          equal json_value ~msg:"durable shape is text, json, truncated"
            (json_object
               [
                 ("text", Json.string "done");
                 ("json", structured_json);
                 ("truncated", Json.bool true);
               ])
            encoded;
          let decoded = decode Tool.Output.jsont encoded in
          equal output_value ~msg:"output equality holds across the round-trip"
            output decoded;
          decode_rejects ~msg:"empty text is rejected" Tool.Output.jsont
            (json_object
               [ ("text", Json.string ""); ("truncated", Json.bool false) ]);
          decode_rejects ~msg:"unknown member is rejected" Tool.Output.jsont
            (add_member "value" (Json.bool true) encoded));
      test "media round-trips and is absent when empty" (fun () ->
          let reference = Mentat_digest.Content_ref.of_contents "image bytes" in
          let media =
            [
              Mentat_llm.Content.media ~media_type:"image/png" (`Ref reference);
            ]
          in
          let output = Tool.Output.make ~text:"an image" ~media () in
          is_true ~msg:"media accessor"
            (List.equal Mentat_llm.Content.equal media
               (Tool.Output.media output));
          let encoded = encode Tool.Output.jsont output in
          equal output_value ~msg:"media round-trips through jsont" output
            (decode Tool.Output.jsont encoded);
          (* A text-only output omits the media member entirely: byte-identical
             to a pre-media output. *)
          equal json_value ~msg:"a text-only output has no media member"
            (json_object
               [ ("text", Json.string "t"); ("truncated", Json.bool false) ])
            (encode Tool.Output.jsont (Tool.Output.make ~text:"t" ()));
          is_false ~msg:"media participates in equality"
            (Tool.Output.equal output (Tool.Output.make ~text:"an image" ())));
    ]

(* Result. *)

let result_group =
  group "result"
    [
      test "constructors and status queries" (fun () ->
          let completed = Tool.Result.completed ~output:"ok" () in
          equal (option string) ~msg:"completed always carries output"
            (Some "ok")
            (Tool.Result.output completed);
          is_true ~msg:"completed status"
            (status_equal Tool.Result.Completed (Tool.Result.status completed));
          let metadata = json_object [ ("path", Json.string "missing.txt") ] in
          let failed =
            Tool.Result.failed ~output:"partial" ~metadata `Not_found "missing"
          in
          equal (option string) ~msg:"failed keeps partial output"
            (Some "partial")
            (Tool.Result.output failed);
          (match Tool.Result.status failed with
          | Tool.Result.Failed
              { kind = `Not_found; message; metadata = Some actual } ->
              equal string ~msg:"failure message" "missing" message;
              equal json_value ~msg:"failure metadata" metadata actual
          | status -> failf "unexpected status: %a" pp_status status);
          let interrupted =
            Tool.Result.interrupted ~reason:"stop requested" ~cancelled:true ()
          in
          equal (option string) ~msg:"interrupted output is optional" None
            (Tool.Result.output interrupted);
          (match Tool.Result.status interrupted with
          | Tool.Result.Interrupted { reason; cancelled } ->
              equal string ~msg:"interrupt reason" "stop requested" reason;
              is_true ~msg:"cancellation marker" cancelled
          | status -> failf "unexpected status: %a" pp_status status);
          raises
            (Invalid_argument "Mentat_tool.Result.failed: message must not be empty") (fun () ->
              Tool.Result.failed `Failed "");
          raises
            (Invalid_argument "Mentat_tool.Result.interrupted: reason must not be empty")
            (fun () -> Tool.Result.interrupted ~reason:"" ~cancelled:false ()));
      test "map transforms output and preserves status" (fun () ->
          let mapped =
            Tool.Result.map string_of_int (Tool.Result.completed ~output:3 ())
          in
          equal string_result ~msg:"completed maps its output"
            (Tool.Result.completed ~output:"3" ())
            mapped;
          equal string_result ~msg:"failed keeps kind, message, and metadata"
            (Tool.Result.failed ~output:"2" ~metadata:(Json.bool true) `Stale
               "old")
            (Tool.Result.map string_of_int
               (Tool.Result.failed ~output:2 ~metadata:(Json.bool true) `Stale
                  "old"));
          equal string_result ~msg:"interrupted without output stays bare"
            (Tool.Result.interrupted ~reason:"stop" ~cancelled:false ())
            (Tool.Result.map string_of_int
               (Tool.Result.interrupted ~reason:"stop" ~cancelled:false ())));
      test "equal compares status and output under the element equality"
        (fun () ->
          let a = Tool.Result.completed ~output:"x" () in
          is_true ~msg:"reflexive" (Tool.Result.equal String.equal a a);
          is_false ~msg:"output participates"
            (Tool.Result.equal String.equal a
               (Tool.Result.completed ~output:"y" ()));
          is_false ~msg:"status participates"
            (Tool.Result.equal String.equal a (Tool.Result.failed `Failed "x"));
          let fa = Tool.Result.failed ~metadata:(Json.bool true) `Stale "old" in
          is_true ~msg:"failure metadata compared as JSON"
            (Tool.Result.equal String.equal fa
               (Tool.Result.failed ~metadata:(Json.bool true) `Stale "old"));
          is_false ~msg:"failure metadata drift"
            (Tool.Result.equal String.equal fa
               (Tool.Result.failed ~metadata:(Json.bool false) `Stale "old")));
      test "jsont round-trips each status" (fun () ->
          let codec = Tool.Result.jsont Jsont.string in
          let round_trip ~msg result =
            equal string_result ~msg result (decode codec (encode codec result))
          in
          round_trip ~msg:"completed" (Tool.Result.completed ~output:"ok" ());
          round_trip ~msg:"failed with everything"
            (Tool.Result.failed ~output:"partial" ~metadata:(Json.bool true)
               `Timed_out "too slow");
          round_trip ~msg:"failed bare" (Tool.Result.failed `Failed "broke");
          round_trip ~msg:"interrupted with partial output"
            (Tool.Result.interrupted ~output:"partial" ~reason:"stop"
               ~cancelled:true ());
          round_trip ~msg:"interrupted bare"
            (Tool.Result.interrupted ~reason:"host stop" ~cancelled:false ()));
      test "the known-result codec stores erased outputs" (fun () ->
          (* [Result.jsont Output.jsont] is the durable `Returned shape the
             session stores: no wrapper, and the output nested as the
             durable output projection. *)
          let codec = Tool.Result.jsont Tool.Output.jsont in
          let result =
            Tool.Result.completed ~output:(Tool.Output.make ~text:"done" ()) ()
          in
          equal json_value ~msg:"completed durable shape"
            (json_object
               [
                 ("status", Json.string "completed");
                 ( "output",
                   json_object
                     [
                       ("text", Json.string "done");
                       ("truncated", Json.bool false);
                     ] );
               ])
            (encode codec result);
          equal output_result ~msg:"round-trip preserves the erased result"
            result
            (decode codec (encode codec result));
          equal json_value ~msg:"failed durable shape"
            (json_object
               [
                 ("status", Json.string "failed");
                 ("failure", Json.string "not_found");
                 ("message", Json.string "missing");
                 ("metadata", Json.bool true);
               ])
            (encode codec
               (Tool.Result.failed ~metadata:(Json.bool true) `Not_found
                  "missing"));
          equal json_value ~msg:"interrupted durable shape"
            (json_object
               [
                 ("status", Json.string "interrupted");
                 ("reason", Json.string "stop");
                 ("cancelled", Json.bool true);
               ])
            (encode codec
               (Tool.Result.interrupted ~reason:"stop" ~cancelled:true ())));
      test "failure kinds have pinned wire spellings" (fun () ->
          let codec = Tool.Result.jsont Jsont.string in
          List.iter
            (fun (kind, spelling) ->
              equal json_value ~msg:(spelling ^ " spelling")
                (Json.string spelling)
                (member "failure" (encode codec (Tool.Result.failed kind "m"))))
            failure_spellings);
      test "jsont rejects malformed durable results" (fun () ->
          (* Each status is a case object declaring only its own members, so a
             missing required member or an unknown status tag is a decode
             error from the codec itself. *)
          let codec = Tool.Result.jsont Jsont.string in
          decode_rejects ~msg:"completed without output"
            ~contains:"completed result requires output" codec
            (json_object [ ("status", Json.string "completed") ]);
          decode_rejects ~msg:"failed without message" codec
            (json_object
               [
                 ("status", Json.string "failed");
                 ("failure", Json.string "failed");
               ]);
          decode_rejects ~msg:"failed without failure" codec
            (json_object
               [
                 ("status", Json.string "failed"); ("message", Json.string "m");
               ]);
          decode_rejects ~msg:"interrupted without reason" codec
            (json_object
               [
                 ("status", Json.string "interrupted");
                 ("cancelled", Json.bool true);
               ]);
          decode_rejects ~msg:"interrupted without cancelled" codec
            (json_object
               [
                 ("status", Json.string "interrupted");
                 ("reason", Json.string "r");
               ]);
          decode_rejects ~msg:"unknown status" codec
            (json_object [ ("status", Json.string "finished") ]);
          decode_rejects ~msg:"unknown failure kind" codec
            (json_object
               [
                 ("status", Json.string "failed");
                 ("failure", Json.string "exploded");
                 ("message", Json.string "m");
               ]);
          decode_rejects ~msg:"unknown member" codec
            (json_object
               [
                 ("status", Json.string "completed");
                 ("output", Json.string "ok");
                 ("attempts", Json.int 2);
               ]));
      test "decode rejects cross-status members" (fun () ->
          (* A member that belongs to another status — a failed result
             carrying interrupted's "cancelled" — is a decode error, never a
             silent drop. *)
          decode_rejects ~msg:"failed with a cancelled member"
            (Tool.Result.jsont Jsont.string)
            (json_object
               [
                 ("status", Json.string "failed");
                 ("failure", Json.string "failed");
                 ("message", Json.string "m");
                 ("cancelled", Json.bool true);
               ]));
      test "to_model lowers each status to its pinned transcript answer"
        (fun () ->
          (* This lowering is part of the durable contract: the session stores
             the erased result and derives the transcript answer here at
             replay, answering the actual pending LLM call. Changing any of
             these shapes is a session-document version bump. *)
          let call =
            Mentat_llm.Tool.Call.make ~id:"call-1" ~name:"echo"
              ~input:(Json.object' []) ()
          in
          let lower result = Tool.Result.to_model ~call result in
          let completed =
            Tool.Result.completed
              ~output:(Tool.Output.make ~text:"all done" ())
              ()
          in
          let answer = lower completed in
          equal string ~msg:"call id" "call-1"
            (Mentat_llm.Tool.Result.call_id answer);
          equal string ~msg:"tool name" "echo"
            (Mentat_llm.Tool.Result.name answer);
          is_false ~msg:"completed is not an error"
            (Mentat_llm.Tool.Result.is_error answer);
          equal (list string) ~msg:"completed lowers to its output text"
            [ "all done" ]
            (Mentat_llm.Tool.Result.texts answer);
          let failed_bare = lower (Tool.Result.failed `Not_found "missing") in
          is_true ~msg:"failed is an error"
            (Mentat_llm.Tool.Result.is_error failed_bare);
          equal (list string) ~msg:"bare failure lowers to its message"
            [ "missing" ]
            (Mentat_llm.Tool.Result.texts failed_bare);
          let failed_partial =
            lower
              (Tool.Result.failed
                 ~output:(Tool.Output.make ~text:"partial text" ())
                 `Stale "went stale")
          in
          equal (list string)
            ~msg:"partial failure joins message and output text"
            [ "went stale\n\npartial text" ]
            (Mentat_llm.Tool.Result.texts failed_partial);
          let interrupted_bare =
            lower (Tool.Result.interrupted ~reason:"stopped" ~cancelled:true ())
          in
          is_true ~msg:"interrupted is an error"
            (Mentat_llm.Tool.Result.is_error interrupted_bare);
          equal (list string) ~msg:"bare interrupt lowers to its reason"
            [ "stopped" ]
            (Mentat_llm.Tool.Result.texts interrupted_bare);
          let interrupted_partial =
            lower
              (Tool.Result.interrupted
                 ~output:(Tool.Output.make ~text:"so far" ())
                 ~reason:"stopped" ~cancelled:false ())
          in
          equal (list string)
            ~msg:"partial interrupt joins reason and output text"
            [ "stopped\n\nso far" ]
            (Mentat_llm.Tool.Result.texts interrupted_partial);
          is_true ~msg:"the lowering is deterministic"
            (Mentat_llm.Tool.Result.equal (lower completed) (lower completed)));
      test "to_model lowers output media after the text (version-pinned)"
        (fun () ->
          let call =
            Mentat_llm.Tool.Call.make ~id:"call-img" ~name:"read_file"
              ~input:(Json.object' []) ()
          in
          let reference = Mentat_digest.Content_ref.of_contents "image bytes" in
          let media =
            [
              Mentat_llm.Content.media ~media_type:"image/png" (`Ref reference);
            ]
          in
          let answer =
            Tool.Result.to_model ~call
              (Tool.Result.completed
                 ~output:(Tool.Output.make ~text:"an image" ~media ())
                 ())
          in
          equal (list string) ~msg:"the text is preserved" [ "an image" ]
            (Mentat_llm.Tool.Result.texts answer);
          (match Mentat_llm.Tool.Result.content answer with
          | [
           Mentat_llm.Content.Text _;
           Mentat_llm.Content.Media { source = `Ref r; _ };
          ] ->
              is_true ~msg:"the media block follows the text"
                (Mentat_digest.Content_ref.equal reference r)
          | _ -> fail "expected a text block then the media block");
          (* Version-pinned: a text-only output lowers text-only, no media. *)
          let text_only =
            Tool.Result.to_model ~call
              (Tool.Result.completed
                 ~output:(Tool.Output.make ~text:"just text" ())
                 ())
          in
          is_false ~msg:"a text-only output carries no media block"
            (List.exists
               (function
                 | Mentat_llm.Content.Media _ -> true
                 | Mentat_llm.Content.Text _ -> false)
               (Mentat_llm.Tool.Result.content text_only)));
      test "to_model answers exactly the given call" (fun () ->
          (* No placeholder synthesis: the answer echoes the pending call's id
             and name, so a mismatch cannot hide behind supplied strings. *)
          let call =
            Mentat_llm.Tool.Call.make ~id:"c-9" ~name:"reader"
              ~input:(json_object [ ("path", Json.string "f") ])
              ()
          in
          let answer =
            Tool.Result.to_model ~call
              (Tool.Result.completed
                 ~output:(Tool.Output.make ~text:"ok" ())
                 ())
          in
          equal string ~msg:"call id echoed" "c-9"
            (Mentat_llm.Tool.Result.call_id answer);
          equal string ~msg:"name echoed" "reader"
            (Mentat_llm.Tool.Result.name answer));
    ]

(* Definition. *)

let definition_group =
  group "definition"
    [
      test "make delegates identity validation upstream and exposes it"
        (fun () ->
          let echo = echo_tool () in
          equal string ~msg:"name" "echo" (decl_name echo);
          equal string ~msg:"description" "Echo input text."
            (decl_description echo);
          equal json_value ~msg:"input schema" echo_schema (decl_schema echo);
          raises (Invalid_argument "Mentat_llm.Tool.make: name must not be empty")
            (fun () ->
              Tool.make ~name:"" ~description:"d" ~input:text_input
                ~output:output_of_string
                ~run:(fun ~cancelled:_ v -> Tool.Result.completed ~output:v ())
                ());
          raises
            (Invalid_argument "Mentat_llm.Tool.make: description must not be empty") (fun () ->
              Tool.make ~name:"n" ~description:"" ~input:text_input
                ~output:output_of_string
                ~run:(fun ~cancelled:_ v -> Tool.Result.completed ~output:v ())
                ());
          raises (Invalid_argument "Mentat_llm.Tool.make: name must not be empty")
            (fun () -> staged_tool ~name:"" ());
          raises
            (Invalid_argument "Mentat_llm.Tool.make: description must not be empty") (fun () ->
              Tool.make_staged ~name:"n" ~description:"" ~input:text_input
                ~prepared:plan_jsont ~describe:describe_plan
                ~output:output_of_string
                ~prepare:(fun ~cancelled:_ input ->
                  `Prepared { target = input; count = 0 })
                ~run:(fun ~cancelled:_ plan ->
                  Tool.Result.completed ~output:(describe_plan plan) ())
                ()));
      test "make enforces the model tool-name grammar at construction"
        (fun () ->
          (* The grammar is the upstream owner's (lib/llm/tool.mli); a bad name
             fails at authoring time, never at replay-time lowering. *)
          let make name =
            Tool.make ~name ~description:"d" ~input:text_input
              ~output:output_of_string
              ~run:(fun ~cancelled:_ v -> Tool.Result.completed ~output:v ())
              ()
          in
          expect_invalid_arg "a space is outside the grammar" (fun () ->
              make "bad name");
          expect_invalid_arg "a leading digit is outside the grammar" (fun () ->
              make "9tool");
          expect_invalid_arg "65 characters exceed the cap" (fun () ->
              make (String.make 65 'a'));
          expect_invalid_arg "make_staged applies the same grammar" (fun () ->
              staged_tool ~name:"bad name" ());
          (* The grammar's boundary is accepted: underscore first character,
             digits and dashes after, and exactly 64 characters. *)
          equal string ~msg:"underscore start and digits are valid" "_tool-2"
            (decl_name (make "_tool-2"));
          equal string ~msg:"64 characters are within the cap"
            (String.make 64 'a')
            (decl_name (make (String.make 64 'a'))));
      test "construction is pure" (fun () ->
          let touched = ref 0 in
          let tool =
            Tool.make ~name:"pure" ~description:"Never runs at construction."
              ~input:text_input ~output:output_of_string
              ~permissions:(fun _ ->
                incr touched;
                [])
              ~run:(fun ~cancelled:_ v ->
                incr touched;
                Tool.Result.completed ~output:v ())
              ()
          in
          ignore (Tool.declaration tool);
          equal int ~msg:"identity queries invoke no author callback" 0 !touched);
    ]

(* Calls: decode, queries, run. *)

let call_group =
  group "call"
    [
      test "decode rejects invalid input with the tool's diagnostic" (fun () ->
          let tool = echo_tool () in
          match Tool.Call.decode tool (model_call tool (json_object [])) with
          | Ok _ -> fail "expected a decode error"
          | Error (Tool.Call.Decode_error.Name_mismatch _) ->
              fail "the fixture uses the declaration's name"
          | Error (Tool.Call.Decode_error.Invalid_input { tool; diagnostic }) ->
              equal string ~msg:"names the tool" "echo" tool;
              is_true ~msg:"diagnostic is populated"
                (not (String.is_empty diagnostic));
              let error =
                Tool.Call.Decode_error.Invalid_input { tool; diagnostic }
              in
              is_true ~msg:"message names the tool"
                (String.includes ~affix:"tool \"echo\" rejected its input"
                   (Tool.Call.Decode_error.message error));
              equal string ~msg:"pp renders the message"
                (Tool.Call.Decode_error.message error)
                (Format.asprintf "%a" Tool.Call.Decode_error.pp error));
      test "decode rejects a call for another declaration before input"
        (fun () ->
          let tool = echo_tool () in
          let source = model_call ~name:"other" tool (json_object []) in
          match Tool.Call.decode tool source with
          | Ok _ -> fail "expected a name mismatch"
          | Error (Tool.Call.Decode_error.Invalid_input _) ->
              fail "name ownership must be checked before input decoding"
          | Error (Tool.Call.Decode_error.Name_mismatch { declaration; call })
            ->
              equal string ~msg:"declaration name" "echo" declaration;
              equal string ~msg:"source call name" "other" call);
      test "decode decodes once and plans permissions once" (fun () ->
          let decode_count = ref 0 in
          let encode_count = ref 0 in
          let permission_count = ref 0 in
          let seen = ref [] in
          let counted_input =
            Tool.Input.make
              (Jsont.map ~kind:"counted input"
                 ~dec:(fun v ->
                   incr decode_count;
                   v)
                 ~enc:(fun v ->
                   incr encode_count;
                   v)
                 text_codec)
              ~schema:echo_schema
          in
          let tool =
            Tool.make ~name:"echo" ~description:"Echo input text."
              ~input:counted_input ~output:output_of_string
              ~permissions:(fun value ->
                incr permission_count;
                seen := value :: !seen;
                if String.equal value "allow" then [ request_for value ] else [])
              ~run:(fun ~cancelled:_ value ->
                seen := value :: !seen;
                Tool.Result.completed ~output:value ())
              ()
          in
          let call = decode_ok tool (input_json "allow") in
          equal int ~msg:"provider input decoded exactly once" 1 !decode_count;
          equal int ~msg:"canonical re-encoding happens once, at decode" 1
            !encode_count;
          equal int ~msg:"the planner ran exactly once, from the decoded value"
            1 !permission_count;
          equal (list request_value) ~msg:"requests derive from the input"
            [ request_for "allow" ]
            (Tool.Call.permissions call);
          ignore (Tool.Call.permissions call);
          ignore (Tool.Call.permissions call);
          ignore (Tool.Call.input call);
          let result = finished (Tool.Call.run call ~cancelled:no_cancel) in
          equal int ~msg:"queries and run decode nothing further" 1
            !decode_count;
          equal int ~msg:"permissions is a cached observer" 1 !permission_count;
          equal (option string) ~msg:"run consumed the decoded value"
            (Some "allow")
            (Option.map Tool.Output.text (Tool.Result.output result));
          (match !seen with
          | [ run_value; planned_value ] ->
              is_true
                ~msg:"run received the exact value that derived the requests"
                (run_value == planned_value)
          | seen -> failf "expected two observations, got %d" (List.length seen));
          (* The planner is input-dependent, not a static declaration. *)
          equal (list request_value) ~msg:"another input plans no requests" []
            (Tool.Call.permissions (decode_ok tool (input_json "deny"))));
      test "queries expose name, canonical input, and stage" (fun () ->
          let tool = echo_tool () in
          let source =
            model_call ~id:"call-source" ~signature:"continuity" tool
              (input_json "hello")
          in
          let call =
            match Tool.Call.decode tool source with
            | Ok call -> call
            | Error error ->
                failf "decode failed: %s" (Tool.Call.Decode_error.message error)
          in
          equal string ~msg:"name is the tool identity" "echo"
            (Tool.Call.name call);
          is_true ~msg:"source is retained exactly"
            (Tool.Call.source call == source);
          equal canonical_json_value ~msg:"input is the canonical re-encoding"
            (input_json "hello") (Tool.Call.input call);
          expect_stage ~msg:"an ordinary call sits at the direct stage"
            Tool.Stage.Direct call);
      test "run erases the callback result through the tool encoder" (fun () ->
          let completed =
            finished
              (Tool.Call.run
                 (decode_ok (echo_tool ()) (input_json "hello"))
                 ~cancelled:no_cancel)
          in
          equal (option string) ~msg:"completed text" (Some "hello")
            (Option.map Tool.Output.text (Tool.Result.output completed));
          equal (option json_value) ~msg:"completed structured json"
            (Some (Json.string "hello"))
            (Option.bind (Tool.Result.output completed) Tool.Output.json);
          let failing =
            echo_tool
              ~run:(fun ~cancelled:_ input ->
                Tool.Result.failed ~output:("partial:" ^ input) `Stale
                  "stale input")
              ()
          in
          let failed =
            finished
              (Tool.Call.run
                 (decode_ok failing (input_json "data"))
                 ~cancelled:no_cancel)
          in
          equal (option string) ~msg:"partial output is erased too"
            (Some "partial:data")
            (Option.map Tool.Output.text (Tool.Result.output failed));
          match Tool.Result.status failed with
          | Tool.Result.Failed
              { kind = `Stale; message = "stale input"; metadata = None } ->
              ()
          | status -> failf "unexpected status: %a" pp_status status);
      test "callback exceptions propagate to the engine" (fun () ->
          let exception Boom in
          let tool = echo_tool ~run:(fun ~cancelled:_ _ -> raise Boom) () in
          raises ~msg:"an unexpected exception is not a result" Boom (fun () ->
              Tool.Call.run
                (decode_ok tool (input_json "x"))
                ~cancelled:no_cancel));
    ]

(* Canonical input. *)

let canonical_group =
  group "canonical input"
    [
      test "members are sorted recursively, arrays kept in order" (fun () ->
          let nested =
            json_object [ ("zeta", Json.int 1); ("alpha", Json.bool true) ]
          in
          let provider =
            json_object [ ("outer", nested); ("inner", json_array [ nested ]) ]
          in
          let canonical = canonical_of provider in
          is_true ~msg:"canonicalization preserves semantics"
            (Json.equal provider canonical);
          equal canonical_json_value ~msg:"sorted at every object level"
            (json_object
               [
                 ( "inner",
                   json_array
                     [
                       json_object
                         [ ("alpha", Json.bool true); ("zeta", Json.int 1) ];
                     ] );
                 ( "outer",
                   json_object
                     [ ("alpha", Json.bool true); ("zeta", Json.int 1) ] );
               ])
            canonical);
      test "duplicate members keep their relative order" (fun () ->
          (* The sort is stable on equal names, so a pathological provider
             object with duplicate members canonicalizes deterministically
             instead of scrambling. *)
          let provider =
            Json.object'
              [
                Json.mem (Json.name "b") (Json.int 1);
                Json.mem (Json.name "a") (Json.string "first");
                Json.mem (Json.name "a") (Json.string "second");
              ]
          in
          let canonical = canonical_of provider in
          equal (list string) ~msg:"stable sort on the duplicate name"
            [ "a"; "a"; "b" ] (member_names canonical);
          equal canonical_json_value ~msg:"duplicate values keep their order"
            (Json.object'
               [
                 Json.mem (Json.name "a") (Json.string "first");
                 Json.mem (Json.name "a") (Json.string "second");
                 Json.mem (Json.name "b") (Json.int 1);
               ])
            canonical);
      test "canonical input is the codec's re-encoding" (fun () ->
          (* A codec-backed input owns the stored shape: the codec re-encodes
             the decoded value, so provider spelling variants collapse. *)
          let call = decode_ok (echo_tool ()) (input_json "hi") in
          equal canonical_json_value ~msg:"echo input re-encodes canonically"
            (input_json "hi") (Tool.Call.input call));
      test "source input and normalized canonical input remain distinct"
        (fun () ->
          let codec =
            Jsont.map ~kind:"normalized echo input" ~dec:String.lowercase_ascii
              ~enc:Fun.id text_codec
          in
          let tool =
            Tool.make ~name:"normalize" ~description:"Normalize input."
              ~input:(Tool.Input.make codec ~schema:echo_schema)
              ~output:output_of_string
              ~run:(fun ~cancelled:_ input ->
                Tool.Result.completed ~output:input ())
              ()
          in
          let source =
            model_call ~id:"call-normalize" ~signature:"opaque" tool
              (input_json "LOUD")
          in
          let call =
            match Tool.Call.decode tool source with
            | Ok call -> call
            | Error error ->
                failf "decode failed: %s" (Tool.Call.Decode_error.message error)
          in
          equal canonical_json_value ~msg:"source keeps provider spelling"
            (input_json "LOUD")
            (Mentat_llm.Tool.Call.input (Tool.Call.source call));
          equal canonical_json_value ~msg:"canonical input reflects the codec"
            (input_json "loud") (Tool.Call.input call);
          equal (option string) ~msg:"source keeps provider signature"
            (Some "opaque")
            (Mentat_llm.Tool.Call.signature (Tool.Call.source call)));
    ]

(* Staged calls and prepared values. *)

let staged_group =
  group "staged"
    [
      test "staged decode sits at the prepare stage with preliminary requests"
        (fun () ->
          let tool =
            staged_tool
              ~prepare_permissions:(fun input ->
                [ request_for ("inspect " ^ input) ])
              ()
          in
          let call = decode_ok tool (input_json "alpha") in
          expect_stage ~msg:"stage" Tool.Stage.Prepare call;
          equal string ~msg:"name" "staged" (Tool.Call.name call);
          equal (list request_value)
            ~msg:"permissions are the prepare-stage requests"
            [ request_for "inspect alpha" ]
            (Tool.Call.permissions call));
      test "prepare may finish without staging" (fun () ->
          let tool =
            staged_tool
              ~prepare:(fun ~cancelled:_ input ->
                `Finished (Tool.Result.completed ~output:("done:" ^ input) ()))
              ()
          in
          let result =
            finished
              (Tool.Call.run
                 (decode_ok tool (input_json "quick"))
                 ~cancelled:no_cancel)
          in
          equal (option string) ~msg:"finished output is erased"
            (Some "done:quick")
            (Option.map Tool.Output.text (Tool.Result.output result)));
      test "staging derives everything from the reconstructed plan" (fun () ->
          (* A normalizing codec makes reconstruction observable: the stored
             payload, final requests, and description must all come from the
             decode-of-encode of the plan, never the original heap value. *)
          let normalizing_jsont =
            Jsont.map ~kind:"normalizing plan" ~dec:String.lowercase_ascii
              ~enc:Fun.id Jsont.string
          in
          let tool =
            Tool.make_staged ~name:"normalizing" ~description:"Normalize."
              ~input:text_input ~prepared:normalizing_jsont
              ~describe:(fun plan -> "plan " ^ plan)
              ~output:output_of_string
              ~prepare:(fun ~cancelled:_ input ->
                `Prepared (String.uppercase_ascii input))
              ~permissions:(fun plan -> [ request_for plan ])
              ~run:(fun ~cancelled:_ plan ->
                Tool.Result.completed ~output:plan ())
              ()
          in
          let prepared = prepare_value ~input:"Mixed" tool in
          equal string ~msg:"description from the reconstructed plan"
            "plan mixed"
            (Tool.Prepared.description prepared);
          equal (list request_value)
            ~msg:"final requests from the reconstructed plan"
            [ request_for "mixed" ]
            (Tool.Prepared.requests prepared);
          equal canonical_json_value ~msg:"stored payload is the canonical form"
            (Json.string "mixed")
            (member "payload" (encode Tool.Prepared.jsont prepared)));
      test "the envelope pins version 1 and the canonical shapes" (fun () ->
          let tool =
            staged_tool
              ~prepare:(fun ~cancelled:_ input ->
                `Prepared { target = input; count = 5 })
              ()
          in
          let prepared = prepare_value ~input:"alpha" tool in
          equal string ~msg:"tool name" "staged" (Tool.Prepared.tool prepared);
          equal string ~msg:"description accessor" "write alpha 5 times"
            (Tool.Prepared.description prepared);
          let envelope = encode Tool.Prepared.jsont prepared in
          equal json_value ~msg:"version is pinned to 1" (Json.int 1)
            (member "version" envelope);
          equal json_value ~msg:"tool member" (Json.string "staged")
            (member "tool" envelope);
          equal canonical_json_value ~msg:"canonical provider input"
            (input_json "alpha") (member "input" envelope);
          (* The codec declares target before count; the stored payload is the
             sorted canonical form. *)
          equal canonical_json_value ~msg:"canonical sorted payload"
            (json_object
               [ ("count", Json.int 5); ("target", Json.string "alpha") ])
            (member "payload" envelope);
          equal json_value ~msg:"derived description"
            (Json.string "write alpha 5 times")
            (member "description" envelope);
          equal (list string) ~msg:"the envelope carries exactly D5's fields"
            [ "description"; "input"; "payload"; "requests"; "tool"; "version" ]
            (List.sort String.compare (member_names envelope)));
      test "jsont round-trips and rejects unknown versions and members"
        (fun () ->
          let tool = staged_tool () in
          let prepared = prepare_value ~input:"alpha" tool in
          equal prepared_value ~msg:"round-trip equality" prepared
            (reload_prepared prepared);
          let envelope = encode Tool.Prepared.jsont prepared in
          decode_rejects ~msg:"unknown envelope version"
            ~contains:"unknown prepared envelope version: 2" Tool.Prepared.jsont
            (set_member "version" (Json.int 2) envelope);
          decode_rejects ~msg:"unknown member" Tool.Prepared.jsont
            (add_member "attempts" (Json.int 1) envelope);
          decode_rejects ~msg:"a missing member fails loudly"
            Tool.Prepared.jsont
            (json_object [ ("version", Json.int 1) ]));
      test "equality separates distinct plans" (fun () ->
          let tool = staged_tool () in
          let a = prepare_value ~input:"alpha" tool in
          is_true ~msg:"reflexive" (Tool.Prepared.equal a a);
          let b = prepare_value ~input:"beta" tool in
          is_false ~msg:"different plans differ" (Tool.Prepared.equal a b);
          is_false ~msg:"symmetric on the negative side"
            (Tool.Prepared.equal b a));
      test "a codec that cannot round-trip fails the preparation" (fun () ->
          let broken_jsont =
            Jsont.map ~kind:"unencodable plan" ~dec:Fun.id
              ~enc:(fun (_ : string) ->
                Jsont.Error.msg Jsont.Meta.none "plan is not serializable")
              Jsont.string
          in
          let tool =
            Tool.make_staged ~name:"broken" ~description:"Broken codec."
              ~input:text_input ~prepared:broken_jsont
              ~describe:(fun _ -> "broken")
              ~output:output_of_string
              ~prepare:(fun ~cancelled:_ input -> `Prepared input)
              ~run:(fun ~cancelled:_ plan ->
                Tool.Result.completed ~output:plan ())
              ()
          in
          let result =
            finished
              (Tool.Call.run
                 (decode_ok tool (input_json "x"))
                 ~cancelled:no_cancel)
          in
          match Tool.Result.status result with
          | Tool.Result.Failed { kind = `Failed; message; _ } ->
              is_true ~msg:"the construction defect is reported"
                (not (String.is_empty message))
          | status -> failf "unexpected status: %a" pp_status status);
      test "staging author-function exceptions propagate" (fun () ->
          (* Author-function exceptions during staging are programming errors
             and escape [Call.run] like any callback exception; only a codec
             round-trip failure is a (Failed) result. *)
          let exception Planner_boom in
          let raising_planner =
            staged_tool ~permissions:(fun _ -> raise Planner_boom) ()
          in
          raises ~msg:"a raising final planner escapes" Planner_boom (fun () ->
              Tool.Call.run
                (decode_ok raising_planner (input_json "x"))
                ~cancelled:no_cancel);
          let exception Describe_boom in
          let raising_describe =
            Tool.make_staged ~name:"describing" ~description:"Raises."
              ~input:text_input ~prepared:plan_jsont
              ~describe:(fun _ -> raise Describe_boom)
              ~output:output_of_string
              ~prepare:(fun ~cancelled:_ input ->
                `Prepared { target = input; count = 1 })
              ~run:(fun ~cancelled:_ plan ->
                Tool.Result.completed ~output:(describe_plan plan) ())
              ()
          in
          raises ~msg:"a raising describe escapes" Describe_boom (fun () ->
              Tool.Call.run
                (decode_ok raising_describe (input_json "x"))
                ~cancelled:no_cancel));
      test "an unstable codec fails at prepare, not at resume" (fun () ->
          (* Preparation self-checks decode stability: a codec whose decode
             never reaches a fixpoint fails at prepare time as a Failed
             result (a construction defect), instead of staging a payload
             that only explodes as Prepared_drift after the user approves. *)
          let unstable_jsont =
            Jsont.map ~kind:"unstable plan"
              ~dec:(fun s -> s ^ "x")
              ~enc:Fun.id Jsont.string
          in
          let tool =
            Tool.make_staged ~name:"unstable" ~description:"Unstable codec."
              ~input:text_input ~prepared:unstable_jsont
              ~describe:(fun plan -> "plan " ^ plan)
              ~output:output_of_string
              ~prepare:(fun ~cancelled:_ input -> `Prepared input)
              ~run:(fun ~cancelled:_ plan ->
                Tool.Result.completed ~output:plan ())
              ()
          in
          let result =
            finished
              (Tool.Call.run
                 (decode_ok tool (input_json "a"))
                 ~cancelled:no_cancel)
          in
          match Tool.Result.status result with
          | Tool.Result.Failed { kind = `Failed; message; _ } ->
              is_true ~msg:"the construction defect is reported at prepare"
                (not (String.is_empty message))
          | status -> failf "unexpected status: %a" pp_status status);
      test "envelope version decode is strict" (fun () ->
          (* The version gate requires an exact JSON integer: lenient number
             spellings must not slip a foreign envelope past the pin. *)
          let envelope =
            encode Tool.Prepared.jsont (prepare_value (staged_tool ()))
          in
          decode_rejects ~msg:"a fractional version is rejected"
            ~contains:"version must be an integer" Tool.Prepared.jsont
            (set_member "version" (Json.number 1.9) envelope);
          decode_rejects ~msg:"a string version is rejected"
            ~contains:"version must be an integer" Tool.Prepared.jsont
            (set_member "version" (Json.string "1") envelope));
      test "a raising prepare callback propagates" (fun () ->
          let exception Boom in
          let tool =
            staged_tool ~prepare:(fun ~cancelled:_ _ -> raise Boom) ()
          in
          raises ~msg:"prepare exceptions reach the engine" Boom (fun () ->
              Tool.Call.run
                (decode_ok tool (input_json "x"))
                ~cancelled:no_cancel));
    ]

(* Resume. *)

let resume_group =
  group "resume"
    [
      test "resume rebuilds a run-stage call that executes the same plan"
        (fun () ->
          let prepare_permission_count = ref 0 in
          let prepare_count = ref 0 in
          let final_permission_count = ref 0 in
          let run_count = ref 0 in
          let tool =
            staged_tool ~output:output_of_string
              ~prepare_permissions:(fun input ->
                incr prepare_permission_count;
                [ request_for ("inspect " ^ input) ])
              ~prepare:(fun ~cancelled:_ input ->
                incr prepare_count;
                `Prepared { target = input; count = 3 })
              ~permissions:(fun plan ->
                incr final_permission_count;
                [ request_for plan.target ])
              ~run:(fun ~cancelled:_ plan ->
                incr run_count;
                Tool.Result.completed ~output:(describe_plan plan) ())
              ()
          in
          let first = decode_ok tool (input_json "alpha") in
          equal int ~msg:"prepare planner ran once at decode" 1
            !prepare_permission_count;
          let prepared =
            staged_prepared (Tool.Call.run first ~cancelled:no_cancel)
          in
          equal int ~msg:"prepare ran once" 1 !prepare_count;
          (* Staging derives the final requests from the reconstructed plan
             (including its decode-stability self-check); the exact count is
             the construction's business. The boundary laws are the deltas:
             resume recomputes exactly once, and queries never re-invoke. *)
          let staged_count = !final_permission_count in
          is_true ~msg:"staging derived the final requests" (staged_count >= 1);
          (* Cross the permission wait's durable boundary: re-decode the same
             provider call and hand it the round-tripped prepared value. Only
             this prepared envelope crosses the codec; the result is produced
             after resume. *)
          let fresh = decode_ok tool (input_json "alpha") in
          let reloaded = reload_prepared prepared in
          let resumed = resume_ok fresh reloaded in
          expect_stage ~msg:"resumed stage" Tool.Stage.Run resumed;
          equal int ~msg:"resume recomputed the final requests exactly once"
            (staged_count + 1) !final_permission_count;
          equal int ~msg:"resume never re-runs preparation" 1 !prepare_count;
          equal (list request_value)
            ~msg:"resumed requests equal the prepared requests"
            (Tool.Prepared.requests prepared)
            (Tool.Call.permissions resumed);
          ignore (Tool.Call.permissions resumed);
          equal int ~msg:"permissions stays a cached observer"
            (staged_count + 1) !final_permission_count;
          equal canonical_json_value ~msg:"canonical input survives resume"
            (Tool.Call.input fresh) (Tool.Call.input resumed);
          is_true ~msg:"the exact source call survives resume"
            (Tool.Call.source fresh == Tool.Call.source resumed);
          equal string ~msg:"resumed name" "staged" (Tool.Call.name resumed);
          let result = finished (Tool.Call.run resumed ~cancelled:no_cancel) in
          equal int ~msg:"run ran once" 1 !run_count;
          equal (option string)
            ~msg:"the resumed run consumed the reconstructed plan"
            (Some "write alpha 3 times")
            (Option.map Tool.Output.text (Tool.Result.output result)));
      test "resume rejects ordinary and already-resumed calls" (fun () ->
          let prepared = prepare_value (staged_tool ()) in
          resume_rejects ~msg:"an ordinary call is never staged" "not_staged"
            (decode_ok (echo_tool ()) (input_json "x"))
            prepared;
          let fresh = decode_ok (staged_tool ()) (input_json "alpha") in
          let resumed = resume_ok fresh prepared in
          resume_rejects ~msg:"a resumed call is already at the run stage"
            "not_staged" resumed prepared);
      test "resume rejects a foreign tool or different input" (fun () ->
          let prepared = prepare_value (staged_tool ()) in
          let other = staged_tool ~name:"other" () in
          resume_rejects ~msg:"same-shaped tool under another name"
            "tool_mismatch"
            (decode_ok other (input_json "alpha"))
            prepared;
          resume_rejects ~msg:"same tool, different provider input"
            "input_mismatch"
            (decode_ok (staged_tool ()) (input_json "beta"))
            prepared);
      test "resume rejects a tampered payload" (fun () ->
          let tool = staged_tool () in
          let prepared = prepare_value tool in
          let fresh () = decode_ok tool (input_json "alpha") in
          let undecodable =
            reload_prepared
              ~tamper:(set_member "payload" (Json.string "junk"))
              prepared
          in
          resume_rejects ~msg:"an undecodable payload" "invalid_prepared"
            (fresh ()) undecodable;
          (match Tool.Call.resume (fresh ()) undecodable with
          | Error (Tool.Call.Resume_error.Invalid_prepared diagnostic) ->
              is_true ~msg:"the codec diagnostic is carried"
                (not (String.is_empty diagnostic))
          | Error _ | Ok _ -> fail "expected Invalid_prepared");
          (* A decodable but non-canonical payload — a reordering of the
             stored form — is drift, not silently accepted. *)
          let drifted =
            reload_prepared
              ~tamper:(fun envelope ->
                set_member "payload"
                  (reorder_members (member "payload" envelope))
                  envelope)
              prepared
          in
          resume_rejects ~msg:"a non-canonical payload" "prepared_drift"
            (fresh ()) drifted);
      test "resume rejects drifted final requests" (fun () ->
          let tool = staged_tool () in
          let prepared = prepare_value tool in
          let tampered =
            reload_prepared
              ~tamper:(set_member "requests" (json_array []))
              prepared
          in
          resume_rejects ~msg:"recomputed requests must match"
            "permission_drift"
            (decode_ok tool (input_json "alpha"))
            tampered);
      test "resume rejects a member-reordered stored input" (fun () ->
          (* The resume input comparison is order-sensitive, matching the
             payload's posture: a member-reordered stored input is drift, not
             equality modulo order. *)
          let tool = wide_staged_tool () in
          let prepared =
            staged_prepared
              (Tool.Call.run (decode_ok tool wide_json) ~cancelled:no_cancel)
          in
          let tampered =
            reload_prepared
              ~tamper:(fun envelope ->
                set_member "input"
                  (reorder_members (member "input" envelope))
                  envelope)
              prepared
          in
          resume_rejects ~msg:"a reordered input is a mismatch" "input_mismatch"
            (decode_ok tool wide_json) tampered);
      test "prepared equality is order-sensitive on stored JSON" (fun () ->
          (* [Prepared.equal] is the round-trip check a stored envelope must
             satisfy, so it compares input and payload with member order
             significant: a reordering must not compare equal. *)
          let tool = wide_staged_tool () in
          let prepared =
            staged_prepared
              (Tool.Call.run (decode_ok tool wide_json) ~cancelled:no_cancel)
          in
          let reordered_in name =
            reload_prepared
              ~tamper:(fun envelope ->
                set_member name
                  (reorder_members (member name envelope))
                  envelope)
              prepared
          in
          is_false ~msg:"a member-reordered payload is not equal"
            (Tool.Prepared.equal prepared (reordered_in "payload"));
          is_false ~msg:"a member-reordered input is not equal"
            (Tool.Prepared.equal prepared (reordered_in "input")));
      test "resume error messages are diagnostic" (fun () ->
          List.iter
            (fun (error, message) ->
              equal string ~msg:(message ^ " message") message
                (Tool.Call.Resume_error.message error);
              equal string ~msg:(message ^ " pp") message
                (Format.asprintf "%a" Tool.Call.Resume_error.pp error))
            [
              ( Tool.Call.Resume_error.Not_staged,
                "resume of a call that is not awaiting preparation" );
              ( Tool.Call.Resume_error.Tool_mismatch,
                "prepared value belongs to a different tool" );
              ( Tool.Call.Resume_error.Input_mismatch,
                "prepared value was prepared from different input" );
              ( Tool.Call.Resume_error.Invalid_prepared "bad",
                "prepared payload does not decode: bad" );
              ( Tool.Call.Resume_error.Prepared_drift,
                "prepared payload is not canonical" );
              ( Tool.Call.Resume_error.Permission_drift,
                "final permission requests changed since preparation" );
            ]);
    ]

(* Stage. *)

let stage_group =
  group "stage"
    [
      test "the shared stage type round-trips its stable tokens" (fun () ->
          List.iter
            (fun (stage, token) ->
              equal string ~msg:(token ^ " to_string") token
                (Tool.Stage.to_string stage);
              equal json_value ~msg:(token ^ " wire spelling")
                (Json.string token)
                (encode Tool.Stage.jsont stage);
              is_true ~msg:(token ^ " decodes back")
                (Tool.Stage.equal stage
                   (decode Tool.Stage.jsont (Json.string token)));
              equal string ~msg:(token ^ " pp") token
                (Format.asprintf "%a" Tool.Stage.pp stage))
            [
              (Tool.Stage.Direct, "direct");
              (Tool.Stage.Prepare, "prepare");
              (Tool.Stage.Run, "run");
            ];
          is_false ~msg:"distinct stages differ"
            (Tool.Stage.equal Tool.Stage.Direct Tool.Stage.Run);
          decode_rejects ~msg:"an unknown stage token is rejected"
            Tool.Stage.jsont (Json.string "prepared"));
    ]

(* Properties. *)

(* Generated JSON objects with distinct member names per object, one nesting
   level, and small scalars. Distinctness keeps the order-shuffling law crisp
   (duplicates are covered by an example test). *)
let json_gen =
  let scalar =
    Gen.one_of
      [
        Gen.pure (Json.null ());
        Gen.map (fun b -> Json.bool b) Gen.bool;
        Gen.map (fun n -> Json.int n) (Gen.int_range (-99) 99);
        Gen.map
          (fun s -> Json.string s)
          (Gen.string_of ~size:(Gen.int_range 0 4) (Gen.of_list [ 'a'; 'b'; 'z' ]));
      ]
  in
  let keys = Gen.of_list [ "delta"; "alpha"; "echo"; "bravo"; "zulu" ] in
  let dedupe pairs =
    List.fold_left
      (fun acc ((key, _) as pair) ->
        if List.exists (fun (k, _) -> String.equal k key) acc then acc
        else acc @ [ pair ])
      [] pairs
  in
  let obj_of value_gen =
    Gen.map
      (fun pairs -> json_object (dedupe pairs))
      (Gen.list ~size:(Gen.int_range 0 4) (Gen.pair keys value_gen))
  in
  let value =
    Gen.one_of
      [
        scalar;
        obj_of scalar;
        Gen.map
          (fun l -> json_array l)
          (Gen.list ~size:(Gen.int_range 0 3) scalar);
      ]
  in
  obj_of value

let generated_json_gen = Gen.with_pp pp_json json_gen

(* Reverse every object's member list, leaving arrays alone: a semantic no-op
   that scrambles exactly what canonicalization must fix. *)
let rec shuffled = function
  | Jsont.Object (members, meta) ->
      Jsont.Object
        ( List.rev_map (fun (name, value) -> (name, shuffled value)) members,
          meta )
  | Jsont.Array (elements, meta) ->
      Jsont.Array (List.map shuffled elements, meta)
  | leaf -> leaf

let rec assert_sorted json =
  match json with
  | Jsont.Object (members, _) ->
      let names = List.map (fun ((n, _), _) -> n) members in
      equal (list string) ~msg:"member names are sorted"
        (List.sort String.compare names)
        names;
      List.iter (fun (_, value) -> assert_sorted value) members
  | Jsont.Array (elements, _) -> List.iter assert_sorted elements
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _ -> ()

let plan_gen =
  let open Gen in
  let+ target = string_of ~size:(int_range 1 6) (of_list [ 'a'; 'b'; 'c' ])
  and+ count = int_range 0 9 in
  { target; count }

let plan_gen =
  Gen.with_pp
    (fun ppf p -> Format.fprintf ppf "{target=%S; count=%d}" p.target p.count)
    plan_gen

(* Stage a generated plan through the only path the facade allows. *)
let stage_plan plan =
  let tool =
    Tool.make_staged ~name:"generated" ~description:"Stage a generated plan."
      ~input:Tool.Input.empty ~prepared:plan_jsont ~describe:describe_plan
      ~output:output_of_string
      ~prepare:(fun ~cancelled:_ () -> `Prepared plan)
      ~permissions:(fun plan -> [ request_for plan.target ])
      ~run:(fun ~cancelled:_ plan ->
        Tool.Result.completed ~output:(describe_plan plan) ())
      ()
  in
  ( tool,
    staged_prepared
      (Tool.Call.run (decode_ok tool (Json.object' [])) ~cancelled:no_cancel) )

let result_gen =
  let message_gen =
    Gen.string_of ~size:(Gen.int_range 1 8) (Gen.of_list [ 'a'; 'b'; 'c'; ' ' ])
  in
  let output_gen =
    Gen.string_of ~size:(Gen.int_range 0 6) (Gen.of_list [ 'x'; 'y'; 'z' ])
  in
  let metadata_gen = Gen.option (Gen.pure (Json.bool true)) in
  Gen.one_of
    [
      Gen.map (fun output -> Tool.Result.completed ~output ()) output_gen;
      Gen.bind
        (Gen.of_list (List.map fst failure_spellings))
        (fun kind ->
          Gen.bind message_gen (fun message ->
              Gen.bind metadata_gen (fun metadata ->
                  Gen.map
                    (fun output ->
                      Tool.Result.failed ?output ?metadata kind message)
                    (Gen.option output_gen))));
      Gen.bind message_gen (fun reason ->
          Gen.bind Gen.bool (fun cancelled ->
              Gen.map
                (fun output ->
                  Tool.Result.interrupted ?output ~reason ~cancelled ())
                (Gen.option output_gen)));
    ]

let generated_result_gen = Gen.with_pp (Testable.pp string_result) result_gen

let property_group =
  group "properties"
    [
      prop "canonical input canonicalizes any generated object" generated_json_gen
        (fun json ->
          let canonical = canonical_of json in
          is_true ~msg:"semantics preserved" (Json.equal json canonical);
          assert_sorted canonical;
          equal canonical_json_value ~msg:"member order is irrelevant" canonical
            (canonical_of (shuffled json));
          equal canonical_json_value ~msg:"canonicalization is idempotent"
            canonical (canonical_of canonical));
      prop "prepared envelopes round-trip and resume for any plan" plan_gen
        (fun plan ->
          let tool, prepared = stage_plan plan in
          is_true ~msg:"equal is reflexive"
            (Tool.Prepared.equal prepared prepared);
          let reloaded = reload_prepared prepared in
          equal prepared_value ~msg:"envelope round-trip preserves equality"
            prepared reloaded;
          equal string ~msg:"description tracks the plan" (describe_plan plan)
            (Tool.Prepared.description prepared);
          equal (list request_value) ~msg:"final requests track the plan"
            [ request_for plan.target ]
            (Tool.Prepared.requests prepared);
          let fresh = decode_ok tool (Json.object' []) in
          let resumed = resume_ok fresh reloaded in
          let result = finished (Tool.Call.run resumed ~cancelled:no_cancel) in
          equal (option string) ~msg:"run consumes the round-tripped plan"
            (Some (describe_plan plan))
            (Option.map Tool.Output.text (Tool.Result.output result)));
      prop "prepared equality is symmetric and coincides with plan equality"
        (Gen.pair plan_gen plan_gen) (fun (a, b) ->
          let _, pa = stage_plan a in
          let _, pb = stage_plan b in
          equal bool ~msg:"symmetry"
            (Tool.Prepared.equal pa pb)
            (Tool.Prepared.equal pb pa);
          equal bool ~msg:"equality is exactly plan equality" (plan_equal a b)
            (Tool.Prepared.equal pa pb));
      prop "durable results survive their codec round-trip" generated_result_gen
        (fun result ->
          let codec = Tool.Result.jsont Jsont.string in
          equal string_result ~msg:"round-trip" result
            (decode codec (encode codec result)));
    ]

(* Suite. *)

let () =
  run "mentat.tool"
    [
      identity_group;
      cancellation_group;
      input_group;
      output_group;
      result_group;
      definition_group;
      call_group;
      canonical_group;
      staged_group;
      resume_group;
      stage_group;
      property_group;
    ]
