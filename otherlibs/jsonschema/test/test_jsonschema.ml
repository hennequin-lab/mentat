(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
module Schema = Jsonschema

(* This suite pins [Jsonschema]'s contract: [of_json] admits exactly the
   supported subset and rejects every out-of-subset keyword by name and schema
   path; [validate] enforces the whole admitted schema, reporting
   JSON-Pointer-pathed violations in instance-traversal order. It tests the
   documented subset table, not the implementation. *)

(* JSON construction helpers. *)

let str = Jsont.Json.string
let num f = Jsont.Json.number f
let intj i = Jsont.Json.number (float_of_int i)
let boolj = Jsont.Json.bool
let nullj = Jsont.Json.null ()
let arr = Jsont.Json.list

let obj members =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) members)

let type_ name = ("type", str name)

(* Schema and validation helpers. *)

let schema_of json =
  match Schema.of_json json with
  | Ok t -> t
  | Error e -> failf "of_json rejected a subset schema: %a" Schema.Error.pp e

let conforms t v =
  match Schema.validate t v with
  | Ok () -> ()
  | Error vs ->
      failf "expected conformance, got violations: %a"
        (Format.pp_print_list Schema.Violation.pp)
        vs

let violates t v =
  match Schema.validate t v with
  | Ok () -> failf "expected violations, value conformed"
  | Error vs -> vs

let violation_paths vs = List.map (fun v -> v.Schema.Violation.path) vs

let expect_unsupported ~keyword ~path json =
  match Schema.of_json json with
  | Error (Schema.Error.Unsupported_keyword u) ->
      equal string ~msg:"keyword" keyword u.keyword;
      equal string ~msg:"schema path" path u.path
  | Error e -> failf "expected Unsupported_keyword, got %a" Schema.Error.pp e
  | Ok _ -> failf "expected rejection of keyword %S" keyword

let expect_malformed ~path json =
  match Schema.of_json json with
  | Error (Schema.Error.Malformed m) ->
      equal string ~msg:"schema path" path m.path
  | Error e -> failf "expected Malformed, got %a" Schema.Error.pp e
  | Ok _ -> failf "expected a malformed rejection"

(* [of_json]: the supported subset is admitted. *)

let of_json_supported =
  group "of_json admits the subset"
    [
      test "every scalar type name" (fun () ->
          List.iter
            (fun name -> ignore (schema_of (obj [ type_ name ])))
            [
              "object";
              "array";
              "string";
              "number";
              "integer";
              "boolean";
              "null";
            ]);
      test "an array of type names" (fun () ->
          ignore
            (schema_of (obj [ ("type", arr [ str "string"; str "null" ]) ])));
      test "properties, required, additionalProperties together" (fun () ->
          ignore
            (schema_of
               (obj
                  [
                    type_ "object";
                    ("properties", obj [ ("a", obj [ type_ "string" ]) ]);
                    ("required", arr [ str "a" ]);
                    ("additionalProperties", boolj false);
                  ])));
      test "items, enum, const" (fun () ->
          ignore
            (schema_of
               (obj [ type_ "array"; ("items", obj [ type_ "integer" ]) ]));
          ignore (schema_of (obj [ ("enum", arr [ str "a"; intj 1 ]) ]));
          ignore (schema_of (obj [ ("const", intj 42) ])));
      test "arbitrary nesting" (fun () ->
          ignore
            (schema_of
               (obj
                  [
                    type_ "object";
                    ( "properties",
                      obj
                        [
                          ( "items",
                            obj
                              [
                                type_ "array"; ("items", obj [ type_ "object" ]);
                              ] );
                        ] );
                  ])));
      test "pure-annotation keywords are accepted and ignored" (fun () ->
          let t =
            schema_of
              (obj
                 [
                   type_ "object";
                   ( "$schema",
                     str "https://json-schema.org/draft/2020-12/schema" );
                   ("$id", str "urn:x");
                   ("title", str "A record");
                   ("description", str "docs");
                   ("$comment", str "note");
                   ("default", obj []);
                   ("examples", arr [ obj [] ]);
                 ])
          in
          conforms t (obj []));
    ]

(* [of_json]: out-of-subset keywords are rejected by name and path. *)

let of_json_unsupported =
  group "of_json rejects out-of-subset keywords"
    [
      test "constraint keywords at the root, named" (fun () ->
          List.iter
            (fun keyword ->
              expect_unsupported ~keyword ~path:""
                (obj [ type_ "string"; (keyword, str "x") ]))
            [
              "pattern";
              "format";
              "$ref";
              "$defs";
              "oneOf";
              "anyOf";
              "allOf";
              "not";
              "patternProperties";
              "minimum";
              "maximum";
              "minLength";
              "maxLength";
              "minItems";
              "uniqueItems";
            ]);
      test "an unsupported keyword nested under properties carries its path"
        (fun () ->
          expect_unsupported ~keyword:"minimum" ~path:"/properties/count"
            (obj
               [
                 type_ "object";
                 ( "properties",
                   obj
                     [ ("count", obj [ type_ "integer"; ("minimum", intj 0) ]) ]
                 );
               ]));
      test "an unsupported keyword nested under items carries its path"
        (fun () ->
          expect_unsupported ~keyword:"pattern" ~path:"/items"
            (obj
               [
                 type_ "array";
                 ("items", obj [ type_ "string"; ("pattern", str "x") ]);
               ]));
    ]

(* [of_json]: malformed subset schemas are rejected. *)

let of_json_malformed =
  group "of_json rejects malformed schemas"
    [
      test "a non-object schema" (fun () ->
          expect_malformed ~path:"" (str "nope"));
      test "an unknown type name" (fun () ->
          expect_malformed ~path:"" (obj [ type_ "record" ]));
      test "required not an array of strings" (fun () ->
          expect_malformed ~path:"" (obj [ ("required", str "a") ]);
          expect_malformed ~path:"" (obj [ ("required", arr [ intj 1 ]) ]));
      test "additionalProperties not a boolean" (fun () ->
          expect_malformed ~path:"" (obj [ ("additionalProperties", obj []) ]));
      test "a non-object subschema carries its path" (fun () ->
          expect_malformed ~path:"/items"
            (obj [ type_ "array"; ("items", str "x") ]);
          expect_malformed ~path:"/properties/a"
            (obj [ type_ "object"; ("properties", obj [ ("a", intj 1) ]) ]));
    ]

(* [validate]: type, enum, and const on scalar values. *)

let validate_scalars =
  group "validate: scalars"
    [
      test "type match and mismatch" (fun () ->
          let t = schema_of (obj [ type_ "string" ]) in
          conforms t (str "hello");
          let vs = violates t (intj 3) in
          equal (list string) ~msg:"path" [ "" ] (violation_paths vs));
      test "integer requires an integral number" (fun () ->
          let t = schema_of (obj [ type_ "integer" ]) in
          conforms t (intj 7);
          conforms t (num 7.0);
          ignore (violates t (num 7.5)));
      test "number accepts any number" (fun () ->
          let t = schema_of (obj [ type_ "number" ]) in
          conforms t (num 7.5);
          conforms t (intj 7));
      test "an array of type names is a union" (fun () ->
          let t =
            schema_of (obj [ ("type", arr [ str "string"; str "null" ]) ])
          in
          conforms t (str "x");
          conforms t nullj;
          ignore (violates t (intj 1)));
      test "enum membership" (fun () ->
          let t =
            schema_of (obj [ ("enum", arr [ str "red"; str "green" ]) ])
          in
          conforms t (str "green");
          let vs = violates t (str "blue") in
          equal (list string) ~msg:"path" [ "" ] (violation_paths vs));
      test "const equality" (fun () ->
          let t = schema_of (obj [ ("const", intj 42) ]) in
          conforms t (intj 42);
          ignore (violates t (intj 41)));
    ]

(* [validate]: object keywords with correct instance paths. *)

let validate_objects =
  group "validate: objects"
    [
      test "missing required properties, at the object path" (fun () ->
          let t =
            schema_of
              (obj [ type_ "object"; ("required", arr [ str "a"; str "b" ]) ])
          in
          conforms t (obj [ ("a", intj 1); ("b", intj 2) ]);
          let vs = violates t (obj [ ("a", intj 1) ]) in
          equal (list string) ~msg:"path" [ "" ] (violation_paths vs));
      test "additionalProperties:false rejects extra keys at their path"
        (fun () ->
          let t =
            schema_of
              (obj
                 [
                   type_ "object";
                   ("properties", obj [ ("a", obj [ type_ "integer" ]) ]);
                   ("additionalProperties", boolj false);
                 ])
          in
          conforms t (obj [ ("a", intj 1) ]);
          let vs = violates t (obj [ ("a", intj 1); ("x", intj 2) ]) in
          equal (list string) ~msg:"path" [ "/x" ] (violation_paths vs));
      test "additionalProperties:true permits extra keys" (fun () ->
          let t =
            schema_of
              (obj
                 [
                   type_ "object";
                   ("properties", obj [ ("a", obj [ type_ "integer" ]) ]);
                   ("additionalProperties", boolj true);
                 ])
          in
          conforms t (obj [ ("a", intj 1); ("x", intj 2) ]));
      test "property schemas apply only to present keys" (fun () ->
          let t =
            schema_of
              (obj
                 [
                   type_ "object";
                   ("properties", obj [ ("a", obj [ type_ "string" ]) ]);
                 ])
          in
          conforms t (obj []);
          conforms t (obj [ ("a", str "ok") ]);
          let vs = violates t (obj [ ("a", intj 1) ]) in
          equal (list string) ~msg:"path" [ "/a" ] (violation_paths vs));
    ]

(* [validate]: nested objects and arrays produce deep JSON-Pointer paths. *)

let validate_nested =
  group "validate: nested paths"
    [
      test "a nested object property violation" (fun () ->
          let t =
            schema_of
              (obj
                 [
                   type_ "object";
                   ( "properties",
                     obj
                       [
                         ( "a",
                           obj
                             [
                               type_ "object";
                               ( "properties",
                                 obj [ ("b", obj [ type_ "string" ]) ] );
                             ] );
                       ] );
                 ])
          in
          conforms t (obj [ ("a", obj [ ("b", str "ok") ]) ]);
          let vs = violates t (obj [ ("a", obj [ ("b", intj 5) ]) ]) in
          equal (list string) ~msg:"path" [ "/a/b" ] (violation_paths vs));
      test "array items violations index by position" (fun () ->
          let t =
            schema_of
              (obj [ type_ "array"; ("items", obj [ type_ "integer" ]) ])
          in
          conforms t (arr [ intj 1; intj 2 ]);
          let vs = violates t (arr [ intj 1; str "x"; num 2.5 ]) in
          equal (list string) ~msg:"paths" [ "/1"; "/2" ] (violation_paths vs));
      test "a JSON-Pointer key escape" (fun () ->
          let t =
            schema_of
              (obj
                 [
                   type_ "object";
                   ("properties", obj [ ("a/b", obj [ type_ "string" ]) ]);
                 ])
          in
          let vs = violates t (obj [ ("a/b", intj 1) ]) in
          equal (list string) ~msg:"escaped path" [ "/a~1b" ]
            (violation_paths vs));
    ]

(* Property: [validate] agrees with an independent flat-object oracle. *)

let ( let+ ) = Gen.( let+ )
let ( and+ ) = Gen.( and+ )

type sty = SString | SNumber | SInteger | SBoolean | SNull

let sty_name = function
  | SString -> "string"
  | SNumber -> "number"
  | SInteger -> "integer"
  | SBoolean -> "boolean"
  | SNull -> "null"

let sty_matches sty v =
  match (sty, v) with
  | SString, Jsont.String _ -> true
  | SBoolean, Jsont.Bool _ -> true
  | SNull, Jsont.Null _ -> true
  | SNumber, Jsont.Number _ -> true
  | SInteger, Jsont.Number (f, _) -> Float.is_integer f
  | _ -> false

let sty_gen = Gen.of_list [ SString; SNumber; SInteger; SBoolean; SNull ]

(* Scalar instance values spanning all sorts, so type checks both pass and
   fail across generated cases. *)
let scalar_gen =
  Gen.frequency
    [
      ( 2,
        Gen.map
          (fun s -> str s)
          (Gen.string_of ~size:(Gen.int_range 0 3) (Gen.char_range 'a' 'c')) );
      (2, Gen.map intj (Gen.int_range 0 4));
      (1, Gen.map num (Gen.of_list [ 0.5; 2.5 ]));
      (2, Gen.map (fun b -> boolj b) Gen.bool);
      (1, Gen.pure nullj);
    ]

(* One generated case: a flat object schema (typed properties over keys a/b/c, a
   required subset, an additionalProperties flag) plus a candidate object value
   over keys a/b/c and the always-extra key x. *)
let case_gen =
  let optional g =
    Gen.frequency [ (1, Gen.pure None); (2, Gen.map Option.some g) ]
  in
  let prop k = Gen.map (Option.map (fun ty -> (k, ty))) (optional sty_gen) in
  let req k = Gen.map (fun b -> if b then Some k else None) Gen.bool in
  let mem k = Gen.map (Option.map (fun v -> (k, v))) (optional scalar_gen) in
  let+ pa = prop "a"
  and+ pb = prop "b"
  and+ pc = prop "c"
  and+ ra = req "a"
  and+ rb = req "b"
  and+ rc = req "c"
  and+ ma = mem "a"
  and+ mb = mem "b"
  and+ mc = mem "c"
  and+ mx = mem "x"
  and+ additional = Gen.bool in
  let props = List.filter_map Fun.id [ pa; pb; pc ] in
  let required = List.filter_map Fun.id [ ra; rb; rc ] in
  let members = List.filter_map Fun.id [ ma; mb; mc; mx ] in
  let schema_json =
    obj
      [
        type_ "object";
        ( "properties",
          obj (List.map (fun (k, ty) -> (k, obj [ type_ (sty_name ty) ])) props)
        );
        ("required", arr (List.map (fun s -> str s) required));
        ("additionalProperties", boolj additional);
      ]
  in
  let value_json = obj members in
  let oracle =
    List.for_all
      (fun (k, ty) ->
        match List.assoc_opt k members with
        | Some v -> sty_matches ty v
        | None -> true)
      props
    && List.for_all (fun k -> List.mem_assoc k members) required
    && (additional
       || List.for_all (fun (k, _) -> List.mem_assoc k props) members)
  in
  (schema_json, value_json, oracle)

let case_gen =
  Gen.with_pp
    (fun ppf (s, v, e) ->
      Format.fprintf ppf "schema=%a value=%a expect=%b" Jsont.Json.pp s
        Jsont.Json.pp v e)
    case_gen

let oracle_property =
  group "validate agrees with the oracle"
    [
      prop "flat-object conformance matches the oracle" case_gen
        (fun (schema_json, value_json, expected) ->
          let t = schema_of schema_json in
          let got =
            match Schema.validate t value_json with
            | Ok () -> true
            | Error _ -> false
          in
          equal bool ~msg:"conformance" expected got);
    ]

let () =
  run "mentat.llm.schema"
    [
      of_json_supported;
      of_json_unsupported;
      of_json_malformed;
      validate_scalars;
      validate_objects;
      validate_nested;
      oracle_property;
    ]
