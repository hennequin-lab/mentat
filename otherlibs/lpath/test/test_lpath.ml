(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap

(* This suite is self-contained: the reference predicates and path generators
   below are inlined rather than shared, and are written independently of the
   implementation so the oracle properties are real cross-checks rather than
   tautologies.

   The numbered law ledger below maps every Lpath law L1-L27 to at least
   one test or property:

     L1  round-trip .................. "relative paths" / "absolute paths"
     L2  components round-trip ....... "relative paths" / "absolute paths"
     L3  normalization closure ....... "relative paths" / "absolute paths"
     L4  component grammar ........... "component grammar"
     L5  add_component ............... "component grammar"
     L6  append identity ............. "append algebra"
     L7  append associativity ........ "append algebra"
     L8  component homomorphism ...... "append algebra"
     L9  append/relativize inverse ... "containment"
     L10 is_within <-> relativize .... "containment"
     L11 reflexive/irreflexive ....... "containment"
     L12 strict law .................. "containment"
     L13 root contains all ........... "containment"
     L14 component boundary (security) "containment"
     L18 resolve root = of_string .... "resolve dispatch"
     L19 resolve_any absolute ........ "resolve dispatch"
     L20 resolve_any relative ........ "resolve dispatch"
     L21 Abs clamps .................. "the .. asymmetry"
     L22 Rel over-pops ............... "the .. asymmetry"
     L23 byte order, not component ... "ordering and equality"
     L24 case sensitivity ............ "ordering and equality"
     L25 empty ....................... "error classification"
     L26 Rel absolute-looking ........ "error classification"
     L27 Abs relative ................ "error classification" *)

(* Pure helpers *)

let is_ascii_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let sign n = if n < 0 then -1 else if n > 0 then 1 else 0

let ok_exn what = function
  | Ok value -> value
  | Error e -> failf "%s: %s" what (Lpath.Error.message e)

let rel s = ok_exn ("Rel.of_string " ^ String.escaped s) (Lpath.Rel.of_string s)
let abs s = ok_exn ("Abs.of_string " ^ String.escaped s) (Lpath.Abs.of_string s)
let rel_string s = Result.map Lpath.Rel.to_string (Lpath.Rel.of_string s)
let abs_string s = Result.map Lpath.Abs.to_string (Lpath.Abs.of_string s)

(* The component grammar (L4), computed independently of the library so
   {!Lpath.Rel.is_component} can be checked against it as an oracle. A
   component is valid iff it is non-empty, not [.] or [..], contains no NUL,
   [/], or backslash, and does not begin with an ASCII-letter drive prefix. *)
let ref_is_component c =
  c <> "" && c <> "." && c <> ".."
  && (not (String.exists (fun b -> b = '\000' || b = '/' || b = '\\') c))
  && not (String.length c >= 2 && is_ascii_letter c.[0] && c.[1] = ':')

(* The Rel absolute-looking classifier (L26): a non-empty string is
   absolute-looking iff it begins with [/], a backslash, or a drive prefix. *)
let starts_absolute s =
  s <> ""
  && (s.[0] = '/'
     || s.[0] = '\\'
     || (String.length s >= 2 && is_ascii_letter s.[0] && s.[1] = ':'))

(* No Abs operation ever escapes its root (L21): a helper to assert an error,
   if any, is not [Escapes_root], polymorphic in the Ok payload. *)
let not_escapes what = function
  | Error Lpath.Error.Escapes_root ->
      failf "%s unexpectedly returned Escapes_root" what
  | _ -> ()

(* Split into path segments, so "no [.]/[..]/[//] segment" is checked by
   membership rather than substring (a valid component may contain [..]). *)
let segments s = String.split_on_char '/' s

(* Generators *)

(* Component bytes reach well past plain [a-z]: mixed case, spaces,
   newlines, tabs, non-leading colons, and high (invalid-UTF-8) bytes, so the
   "bytes, not text" stance (T9) is actually exercised. *)
let component_byte_gen =
  Gen.frequency
    [
      (6, Gen.char_range 'a' 'z');
      (1, Gen.char_range 'A' 'Z');
      (2, Gen.of_list [ '.'; '-'; '_'; ' '; ':'; '\n'; '\t'; '0'; '9' ]);
      (2, Gen.map Char.chr (Gen.int_range 128 255));
    ]

(* Repair a raw byte string into a valid component that is also safe as a
   leading path segment: never empty, [.], [..], or an ASCII-letter drive
   prefix (which would make the whole path parse as absolute). *)
let fix_component s =
  if s = "" || s = "." || s = ".." then "x" ^ s
  else if String.length s >= 2 && is_ascii_letter s.[0] && s.[1] = ':' then
    "x" ^ s
  else s

let component_gen =
  Gen.map fix_component (Gen.string_of ~size:(Gen.int_range 1 6) component_byte_gen)

let rel_components_gen = Gen.list ~size:(Gen.int_range 0 6) component_gen

let rel_of_components = function
  | [] -> Lpath.Rel.root
  | cs -> rel (String.concat "/" cs)

let abs_of_components cs = abs ("/" ^ String.concat "/" cs)
let rel_gen = Gen.map rel_of_components rel_components_gen
let abs_gen = Gen.map abs_of_components rel_components_gen

let nonempty_rel_gen =
  Gen.map
    (fun cs -> rel (String.concat "/" cs))
    (Gen.list ~size:(Gen.int_range 1 4) component_gen)

(* Raw parser inputs: rich dot-segment and separator soup, reaching deep [..]
   chains (up to 8 segments plus a [../../] prefix) and drive/backslash/NUL
   forms, so the closure and rejection properties see the escape and clamp
   paths, not just clean input. *)
let raw_segment_gen =
  Gen.frequency
    [
      (5, component_gen);
      (3, Gen.of_list [ "."; ".." ]);
      (1, Gen.of_list [ ""; "a\\b"; "a\000b"; "C:a"; "1:a"; "a:b" ]);
    ]

let raw_path_input_gen =
  Gen.bind
    (Gen.list ~size:(Gen.int_range 0 8) raw_segment_gen)
    (fun segs ->
      Gen.map
        (fun prefix -> prefix ^ String.concat "/" segs)
        (Gen.of_list [ ""; "./"; "../"; "../../"; "/"; "//"; "///"; "\\"; "C:" ]))

(* Arbitrary short byte strings for the component-grammar oracle, drawing the
   structurally significant bytes (dot, colon, slash, backslash, NUL) and high
   bytes with enough weight to hit the drive-prefix and empty-string edges. *)
let grammar_string_gen =
  Gen.string_of ~size:(Gen.int_range 0 4)
    (Gen.frequency
       [
         (4, Gen.char_range 'a' 'z');
         (1, Gen.char_range 'A' 'Z');
         (3, Gen.of_list [ '.'; ':'; '/'; '\\'; '\000'; '-'; ' '; '1' ]);
         (2, Gen.map Char.chr (Gen.int_range 128 255));
       ])

(* Drive-prefix boundary: a leading byte followed by [:] and a safe tail. The
   tail carries no invalidating bytes, so the component is invalid iff the
   leading byte is an ASCII letter (the drive prefix), pinning the narrowness
   of the rule (only ASCII-letter-then-colon: [1:a] is valid, [a:b] is not). *)
let drive_first_gen =
  Gen.frequency
    [
      (3, Gen.char_range 'a' 'z');
      (2, Gen.char_range 'A' 'Z');
      (2, Gen.of_list [ '0'; '1'; '9'; '-'; '_'; '.' ]);
      (1, Gen.map Char.chr (Gen.int_range 128 255));
    ]

let drive_tail_gen =
  Gen.string_of ~size:(Gen.int_range 0 3)
    (Gen.frequency
       [
         (5, Gen.char_range 'a' 'z');
         (2, Gen.of_list [ '.'; '-'; '_'; ' '; '0'; ':' ]);
         (2, Gen.map Char.chr (Gen.int_range 128 255));
       ])

let drive_case_gen =
  Gen.bind drive_first_gen (fun f ->
      Gen.map (fun tail -> (f, tail)) drive_tail_gen)

(* Containment pairs biased to hit all three regions - equal, strict
   descendant, and disjoint - so the containment predicates are exercised on
   both boolean outcomes rather than the near-always-disjoint independent
   pairs. *)
let rel_containment_pair_gen =
  Gen.one_of
    [
      Gen.map (fun p -> (p, p)) rel_gen;
      Gen.bind rel_gen (fun root ->
          Gen.map
            (fun suffix -> (root, Lpath.Rel.append root suffix))
            nonempty_rel_gen);
      Gen.bind rel_gen (fun a -> Gen.map (fun b -> (a, b)) rel_gen);
    ]

let abs_containment_pair_gen =
  Gen.one_of
    [
      Gen.map (fun p -> (p, p)) abs_gen;
      Gen.bind abs_gen (fun root ->
          Gen.map
            (fun r -> (root, Lpath.Abs.append_rel root r))
            nonempty_rel_gen);
      Gen.bind abs_gen (fun a -> Gen.map (fun b -> (a, b)) abs_gen);
    ]

(* Testables *)

let error = Testable.make ~pp:Lpath.Error.pp ~equal:Lpath.Error.equal
let rel_path = Testable.make ~pp:Lpath.Rel.pp ~equal:Lpath.Rel.equal
let abs_path = Testable.make ~pp:Lpath.Abs.pp ~equal:Lpath.Abs.equal

(* Attach counterexample printers to the property generators. *)

let pp_raw ppf s = Format.fprintf ppf "%S" s
let rel_gen = Gen.with_pp Lpath.Rel.pp rel_gen
let abs_gen = Gen.with_pp Lpath.Abs.pp abs_gen
let raw_path_input_gen = Gen.with_pp pp_raw raw_path_input_gen
let grammar_string_gen = Gen.with_pp pp_raw grammar_string_gen

let drive_case_gen =
  Gen.with_pp
    (fun ppf (f, tail) -> Format.fprintf ppf "%C:%S" f tail)
    drive_case_gen

let rel_containment_pair =
  Gen.with_pp
    (fun ppf (a, b) ->
      Format.fprintf ppf "(%a, %a)" Lpath.Rel.pp a Lpath.Rel.pp b)
    rel_containment_pair_gen

let abs_containment_pair =
  Gen.with_pp
    (fun ppf (a, b) ->
      Format.fprintf ppf "(%a, %a)" Lpath.Abs.pp a Lpath.Abs.pp b)
    abs_containment_pair_gen

(* Shared invariant checks for the raw-input fuzz properties. *)
let rel_reparses p =
  equal (result rel_path error)
    ~msg:("reparse " ^ String.escaped (Lpath.Rel.to_string p))
    (Ok p)
    (Lpath.Rel.of_string (Lpath.Rel.to_string p));
  List.iter
    (fun c ->
      is_true
        ~msg:("valid component " ^ String.escaped c)
        (Lpath.Rel.is_component c))
    (Lpath.Rel.components p)

let abs_reparses p =
  equal (result abs_path error)
    ~msg:("reparse " ^ String.escaped (Lpath.Abs.to_string p))
    (Ok p)
    (Lpath.Abs.of_string (Lpath.Abs.to_string p));
  List.iter
    (fun c ->
      is_true
        ~msg:("valid component " ^ String.escaped c)
        (Lpath.Rel.is_component c))
    (Lpath.Abs.components p)

(* Groups *)

let error_diagnostics =
  let all =
    [
      Lpath.Error.Empty;
      Lpath.Error.Relative;
      Lpath.Error.Absolute;
      Lpath.Error.Escapes_root;
      Lpath.Error.Malformed_component "C:a";
    ]
  in
  group "error diagnostics"
    [
      test "message is informative for every constructor" (fun () ->
          List.iter
            (fun e ->
              is_false ~msg:"non-empty message" (Lpath.Error.message e = ""))
            all);
      test "pp renders message" (fun () ->
          List.iter
            (fun e ->
              equal string ~msg:(Lpath.Error.message e) (Lpath.Error.message e)
                (Format.asprintf "%a" Lpath.Error.pp e))
            all);
      test "equal is reflexive and distinguishes constructors" (fun () ->
          List.iter
            (fun e -> is_true ~msg:"reflexive" (Lpath.Error.equal e e))
            all;
          is_false ~msg:"Empty <> Relative"
            (Lpath.Error.equal Lpath.Error.Empty Lpath.Error.Relative);
          is_false ~msg:"Absolute <> Relative"
            (Lpath.Error.equal Lpath.Error.Absolute Lpath.Error.Relative);
          is_false ~msg:"Malformed distinguishes its component"
            (Lpath.Error.equal (Lpath.Error.Malformed_component "a")
               (Lpath.Error.Malformed_component "b"));
          is_true ~msg:"Malformed equal on same component"
            (Lpath.Error.equal (Lpath.Error.Malformed_component "a")
               (Lpath.Error.Malformed_component "a")));
    ]

let component_grammar =
  group "component grammar"
    [
      test "l4_is_component_table" (fun () ->
          (* This exhaustive table is the sole is_component cover for the
             multi-character and empty-tail drive-prefix rows ("ab:cd", ":a",
             "C:"): the drive-prefix-boundary property generates only
             single-letter prefixes and cannot reach them. *)
          List.iter
            (fun (c, expected) ->
              equal bool
                ~msg:("is_component " ^ String.escaped c)
                expected (Lpath.Rel.is_component c))
            [
              ("a", true);
              ("a.ml", true);
              ("a-b", true);
              ("a b", true);
              ("1:a", true);
              (* [a:b] is an ASCII letter immediately followed by ':', i.e. a
                 drive prefix (L4), so it is not a valid component. *)
              ("a:b", false);
              ("ab:cd", true);
              ("C", true);
              (":", true);
              (":a", true);
              ("file\nname", true);
              ("\xc3\xa9", true);
              ("", false);
              (".", false);
              ("..", false);
              ("a/b", false);
              ("a\\b", false);
              ("a\000b", false);
              ("C:a", false);
              ("Z:x", false);
              ("c:d", false);
              ("C:", false);
            ]);
      prop "l4_is_component_matches_grammar" grammar_string_gen (fun c ->
          cover ~label:"valid" ~at_least:15.0 (ref_is_component c);
          cover ~label:"invalid" ~at_least:15.0 (not (ref_is_component c));
          equal bool ~msg:(String.escaped c) (ref_is_component c)
            (Lpath.Rel.is_component c));
      prop "l5_rel_add_component" (Gen.pair rel_gen grammar_string_gen) (fun (p, c) ->
          match Lpath.Rel.add_component p c with
          | Ok q ->
              is_true ~msg:(String.escaped c) (Lpath.Rel.is_component c);
              equal (list string) ~msg:(String.escaped c)
                (Lpath.Rel.components p @ [ c ])
                (Lpath.Rel.components q)
          | Error e ->
              is_false ~msg:(String.escaped c) (Lpath.Rel.is_component c);
              equal error ~msg:(String.escaped c)
                (Lpath.Error.Malformed_component c) e);
      prop "l5_abs_add_component" (Gen.pair abs_gen grammar_string_gen) (fun (p, c) ->
          match Lpath.Abs.add_component p c with
          | Ok q ->
              is_true ~msg:(String.escaped c) (Lpath.Rel.is_component c);
              equal (list string) ~msg:(String.escaped c)
                (Lpath.Abs.components p @ [ c ])
                (Lpath.Abs.components q)
          | Error e ->
              is_false ~msg:(String.escaped c) (Lpath.Rel.is_component c);
              equal error ~msg:(String.escaped c)
                (Lpath.Error.Malformed_component c) e);
      test "l5_add_component_examples" (fun () ->
          equal (result string error) ~msg:"rel appends one component"
            (Ok "a/b")
            (Result.map Lpath.Rel.to_string
               (Lpath.Rel.add_component (rel "a") "b"));
          equal (result string error) ~msg:"rel rejects a separator"
            (Error (Lpath.Error.Malformed_component "a/b"))
            (Result.map Lpath.Rel.to_string
               (Lpath.Rel.add_component Lpath.Rel.root "a/b"));
          equal (result string error) ~msg:"abs appends one component"
            (Ok "/a/b")
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.add_component (abs "/a") "b"));
          equal (result string error) ~msg:"abs rejects a separator"
            (Error (Lpath.Error.Malformed_component "a/b"))
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.add_component Lpath.Abs.root "a/b")));
    ]

let drive_prefix_boundary =
  group "drive-prefix boundary"
    [
      test "a leading drive prefix makes a whole path absolute-looking"
        (fun () ->
          (* [C:a] and [a:b] are ASCII-letter drive prefixes, so a whole path
             beginning with one is absolute-looking; [1:a] and [ab:cd] are not
             drive prefixes, so they parse as ordinary relative paths. *)
          equal (result string error) ~msg:"C:a" (Error Lpath.Error.Absolute)
            (rel_string "C:a");
          equal (result string error) ~msg:"a:b" (Error Lpath.Error.Absolute)
            (rel_string "a:b");
          equal (result string error) ~msg:"1:a" (Ok "1:a") (rel_string "1:a");
          equal (result string error) ~msg:"ab:cd" (Ok "ab:cd")
            (rel_string "ab:cd"));
      prop "drive_prefix_only_ascii_letter_colon" drive_case_gen (fun (f, tail) ->
          let c = Printf.sprintf "%c:%s" f tail in
          cover ~label:"letter (drive, invalid)" ~at_least:20.0
            (is_ascii_letter f);
          cover ~label:"non-letter (valid)" ~at_least:20.0
            (not (is_ascii_letter f));
          equal bool ~msg:(String.escaped c)
            (not (is_ascii_letter f))
            (Lpath.Rel.is_component c));
    ]

let byte_components =
  group "byte components"
    [
      test "unusual valid components are preserved" (fun () ->
          List.iter
            (fun c ->
              let msg = String.escaped c in
              is_true ~msg:("is_component " ^ msg) (Lpath.Rel.is_component c);
              equal (result string error) ~msg:("rel preserves " ^ msg) (Ok c)
                (rel_string c);
              equal (result string error)
                ~msg:("rel add_component preserves " ^ msg)
                (Ok ("dir/" ^ c))
                (Result.map Lpath.Rel.to_string
                   (Lpath.Rel.add_component (rel "dir") c));
              equal (result string error) ~msg:("abs preserves " ^ msg)
                (Ok ("/" ^ c))
                (abs_string ("/" ^ c));
              equal (result string error)
                ~msg:("abs add_component preserves " ^ msg)
                (Ok ("/dir/" ^ c))
                (Result.map Lpath.Abs.to_string
                   (Lpath.Abs.add_component (abs "/dir") c)))
            [
              "a-b";
              "a.b";
              "a b";
              "1:a";
              "ab:cd";
              "file\nname";
              "\xc3\xa9";
              "\xff\xfe";
              "a\xffb";
            ]);
      test "an ascii-letter drive prefix is rejected everywhere" (fun () ->
          (* [a:b] is an ASCII letter immediately followed by ':', i.e. a drive
             prefix (L4), so it is neither a valid component nor a valid leading
             path segment: is_component, of_string, and add_component all reject
             it. *)
          is_false ~msg:"is_component a:b" (Lpath.Rel.is_component "a:b");
          equal (result string error) ~msg:"of_string a:b"
            (Error Lpath.Error.Absolute) (rel_string "a:b");
          equal (result string error) ~msg:"add_component dir a:b"
            (Error (Lpath.Error.Malformed_component "a:b"))
            (Result.map Lpath.Rel.to_string
               (Lpath.Rel.add_component (rel "dir") "a:b")));
    ]

let relative_paths =
  group "relative paths"
    [
      test "parses and normalizes" (fun () ->
          List.iter
            (fun (i, o) ->
              equal (result string error) ~msg:i (Ok o) (rel_string i))
            [
              (".", ".");
              ("a", "a");
              ("./a//b/.", "a/b");
              ("a/../b", "b");
              ("a/b/..", "a");
            ]);
      test "root is the dot path" (fun () ->
          equal string ~msg:"to_string" "." (Lpath.Rel.to_string Lpath.Rel.root);
          is_true ~msg:"is_root" (Lpath.Rel.is_root Lpath.Rel.root);
          equal (list string) ~msg:"components" []
            (Lpath.Rel.components Lpath.Rel.root);
          equal (option pass) ~msg:"parent" None (Lpath.Rel.parent Lpath.Rel.root);
          equal (option pass) ~msg:"basename" None (Lpath.Rel.basename Lpath.Rel.root));
      test "components and accessors" (fun () ->
          equal (list string) ~msg:"components" [ "a"; "b" ]
            (Lpath.Rel.components (rel "a/b"));
          equal (option string) ~msg:"parent" (Some "a")
            (Option.map Lpath.Rel.to_string (Lpath.Rel.parent (rel "a/b")));
          equal (option string) ~msg:"basename" (Some "b")
            (Lpath.Rel.basename (rel "a/b"));
          equal (option string) ~msg:"parent of a single component is root"
            (Some ".")
            (Option.map Lpath.Rel.to_string (Lpath.Rel.parent (rel "a"))));
      prop "l1_rel_round_trip" rel_gen (fun p ->
          equal (result rel_path error) ~msg:"of_string (to_string p)" (Ok p)
            (Lpath.Rel.of_string (Lpath.Rel.to_string p)));
      prop "l2_rel_components_round_trip" rel_gen (fun p ->
          let text =
            match Lpath.Rel.components p with
            | [] -> "."
            | cs -> String.concat "/" cs
          in
          equal (result rel_path error) ~msg:"components reconstruct" (Ok p)
            (Lpath.Rel.of_string text));
      prop "l3_rel_normalization_closure" raw_path_input_gen (fun s ->
          match Lpath.Rel.of_string s with
          | Error _ -> ()
          | Ok p ->
              let cs = Lpath.Rel.components p in
              List.iter
                (fun c ->
                  is_true
                    ~msg:("component " ^ String.escaped c)
                    (Lpath.Rel.is_component c))
                cs;
              let text = Lpath.Rel.to_string p in
              if not (Lpath.Rel.is_root p) then
                equal (list string)
                  ~msg:("segments of " ^ String.escaped text)
                  cs (segments text);
              equal (result rel_path error) ~msg:"reparse" (Ok p)
                (Lpath.Rel.of_string text));
      prop "raw_relative_resolve_preserves_invariants"
        (Gen.pair rel_gen raw_path_input_gen) (fun (start, input) ->
          match Lpath.Rel.resolve start input with
          | Error _ -> ()
          | Ok p -> rel_reparses p);
    ]

let absolute_paths =
  group "absolute paths"
    [
      test "parses and normalizes" (fun () ->
          List.iter
            (fun (i, o) ->
              equal (result string error) ~msg:i (Ok o) (abs_string i))
            [
              ("/", "/");
              ("/a", "/a");
              ("/a//./b/.", "/a/b");
              ("/a/../b", "/b");
              ("/../a", "/a");
            ]);
      test "root is the slash path" (fun () ->
          equal string ~msg:"to_string" "/" (Lpath.Abs.to_string Lpath.Abs.root);
          is_true ~msg:"is_root" (Lpath.Abs.is_root Lpath.Abs.root);
          equal (list string) ~msg:"components" []
            (Lpath.Abs.components Lpath.Abs.root);
          equal (option pass) ~msg:"parent" None (Lpath.Abs.parent Lpath.Abs.root);
          equal (option pass) ~msg:"basename" None (Lpath.Abs.basename Lpath.Abs.root));
      test "components and accessors" (fun () ->
          equal (list string) ~msg:"components" [ "a"; "b" ]
            (Lpath.Abs.components (abs "/a/b"));
          equal (option string) ~msg:"parent" (Some "/a")
            (Option.map Lpath.Abs.to_string (Lpath.Abs.parent (abs "/a/b")));
          equal (option string) ~msg:"basename" (Some "b")
            (Lpath.Abs.basename (abs "/a/b"));
          equal (option string) ~msg:"parent of a single component is root"
            (Some "/")
            (Option.map Lpath.Abs.to_string (Lpath.Abs.parent (abs "/a"))));
      prop "l1_abs_round_trip" abs_gen (fun p ->
          equal (result abs_path error) ~msg:"of_string (to_string p)" (Ok p)
            (Lpath.Abs.of_string (Lpath.Abs.to_string p)));
      prop "l2_abs_components_round_trip" abs_gen (fun p ->
          equal (result abs_path error) ~msg:"components reconstruct" (Ok p)
            (Lpath.Abs.of_string
               ("/" ^ String.concat "/" (Lpath.Abs.components p))));
      prop "l3_abs_normalization_closure" raw_path_input_gen (fun s ->
          match Lpath.Abs.of_string s with
          | Error _ -> ()
          | Ok p ->
              let cs = Lpath.Abs.components p in
              List.iter
                (fun c ->
                  is_true
                    ~msg:("component " ^ String.escaped c)
                    (Lpath.Rel.is_component c))
                cs;
              let text = Lpath.Abs.to_string p in
              if not (Lpath.Abs.is_root p) then
                equal (list string)
                  ~msg:("segments of " ^ String.escaped text)
                  ("" :: cs) (segments text);
              equal (result abs_path error) ~msg:"reparse" (Ok p)
                (Lpath.Abs.of_string text));
      prop "raw_absolute_parse_preserves_invariants" raw_path_input_gen (fun input ->
          match Lpath.Abs.of_string input with
          | Error _ -> ()
          | Ok p -> abs_reparses p);
      prop "raw_absolute_resolve_preserves_invariants"
        (Gen.pair abs_gen raw_path_input_gen) (fun (base, input) ->
          match Lpath.Abs.resolve base input with
          | Error _ -> ()
          | Ok p -> abs_reparses p);
      prop "raw_absolute_resolve_any_preserves_invariants"
        (Gen.pair abs_gen raw_path_input_gen) (fun (base, input) ->
          match Lpath.Abs.resolve_any ~base input with
          | Error _ -> ()
          | Ok p -> abs_reparses p);
    ]

let append_algebra =
  group "append algebra"
    [
      test "examples" (fun () ->
          equal string ~msg:"rel append" "src/lib/a.ml"
            (Lpath.Rel.to_string
               (Lpath.Rel.append (rel "src") (rel "lib/a.ml")));
          equal string ~msg:"abs append_rel" "/workspace/src/a.ml"
            (Lpath.Abs.to_string
               (Lpath.Abs.append_rel (abs "/workspace") (rel "src/a.ml")));
          equal string ~msg:"abs append_rel at root" "/src/a.ml"
            (Lpath.Abs.to_string
               (Lpath.Abs.append_rel Lpath.Abs.root (rel "src/a.ml"))));
      prop "l6_rel_append_root_identity" rel_gen (fun p ->
          equal rel_path ~msg:"append root p" p
            (Lpath.Rel.append Lpath.Rel.root p);
          equal rel_path ~msg:"append p root" p
            (Lpath.Rel.append p Lpath.Rel.root));
      prop "l6_abs_append_rel_root_identity" abs_gen (fun p ->
          equal abs_path ~msg:"append_rel a root" p
            (Lpath.Abs.append_rel p Lpath.Rel.root));
      prop "l7_rel_append_associative" (Gen.triple rel_gen rel_gen rel_gen)
        (fun (a, b, c) ->
          equal rel_path ~msg:"associativity"
            (Lpath.Rel.append (Lpath.Rel.append a b) c)
            (Lpath.Rel.append a (Lpath.Rel.append b c)));
      prop "l8_rel_append_components" (Gen.pair rel_gen rel_gen) (fun (a, b) ->
          equal (list string) ~msg:"components (append a b)"
            (Lpath.Rel.components a @ Lpath.Rel.components b)
            (Lpath.Rel.components (Lpath.Rel.append a b)));
      prop "l8_abs_append_rel_components" (Gen.pair abs_gen rel_gen)
        (fun (a, r) ->
          equal (list string) ~msg:"components (append_rel a r)"
            (Lpath.Abs.components a @ Lpath.Rel.components r)
            (Lpath.Abs.components (Lpath.Abs.append_rel a r)));
    ]

let containment =
  group "containment"
    [
      test "examples" (fun () ->
          equal (option string) ~msg:"rel relativize suffix" (Some "lib/a.ml")
            (Option.map Lpath.Rel.to_string
               (Lpath.Rel.relativize ~root:(rel "src") (rel "src/lib/a.ml")));
          equal (option string) ~msg:"abs relativize suffix" (Some "src/a.ml")
            (Option.map Lpath.Rel.to_string
               (Lpath.Abs.relativize ~root:(abs "/workspace")
                  (abs "/workspace/src/a.ml")));
          equal (option string) ~msg:"abs relativize sibling is None" None
            (Option.map Lpath.Rel.to_string
               (Lpath.Abs.relativize ~root:(abs "/workspace")
                  (abs "/workspace2/a.ml")));
          equal (option string) ~msg:"rel relativize ancestor is None" None
            (Option.map Lpath.Rel.to_string
               (Lpath.Rel.relativize ~root:(rel "src/lib") (rel "src"))));
      prop "l9_rel_append_relativize_inverse" (Gen.pair rel_gen rel_gen)
        (fun (root, suffix) ->
          equal (option rel_path) ~msg:"relativize (append root suffix)"
            (Some suffix)
            (Lpath.Rel.relativize ~root (Lpath.Rel.append root suffix)));
      prop "l9_abs_append_rel_relativize_inverse" (Gen.pair abs_gen rel_gen)
        (fun (root, suffix) ->
          equal (option rel_path) ~msg:"relativize (append_rel root suffix)"
            (Some suffix)
            (Lpath.Abs.relativize ~root (Lpath.Abs.append_rel root suffix)));
      prop "l10_rel_is_within_iff_relativize" rel_containment_pair
        (fun (root, p) ->
          let within = Option.is_some (Lpath.Rel.relativize ~root p) in
          cover ~label:"within" ~at_least:30.0 within;
          cover ~label:"disjoint" ~at_least:10.0 (not within);
          equal bool ~msg:"is_within = is_some relativize" within
            (Lpath.Rel.is_within ~root p));
      prop "l10_abs_is_within_iff_relativize" abs_containment_pair
        (fun (root, p) ->
          let within = Option.is_some (Lpath.Abs.relativize ~root p) in
          equal bool ~msg:"is_within = is_some relativize" within
            (Lpath.Abs.is_within ~root p));
      prop "l11_rel_reflexive_inclusive_irreflexive_strict" rel_gen (fun p ->
          equal (option rel_path) ~msg:"relativize p p = Some root"
            (Some Lpath.Rel.root)
            (Lpath.Rel.relativize ~root:p p);
          is_true ~msg:"is_within p p" (Lpath.Rel.is_within ~root:p p);
          is_false ~msg:"not strict p p"
            (Lpath.Rel.is_strictly_within ~root:p p));
      prop "l11_abs_reflexive_inclusive_irreflexive_strict" abs_gen (fun p ->
          equal (option rel_path) ~msg:"relativize p p = Some root"
            (Some Lpath.Rel.root)
            (Lpath.Abs.relativize ~root:p p);
          is_true ~msg:"is_within p p" (Lpath.Abs.is_within ~root:p p);
          is_false ~msg:"not strict p p"
            (Lpath.Abs.is_strictly_within ~root:p p));
      test "l11_root_edges" (fun () ->
          let r = rel "a/b" in
          equal (option rel_path) ~msg:"rel equal relativizes to root"
            (Some Lpath.Rel.root)
            (Lpath.Rel.relativize ~root:r r);
          equal (option rel_path) ~msg:"rel root relativizes every path"
            (Some r)
            (Lpath.Rel.relativize ~root:Lpath.Rel.root r);
          let a = abs "/a/b" in
          equal (option rel_path) ~msg:"abs equal relativizes to root"
            (Some Lpath.Rel.root)
            (Lpath.Abs.relativize ~root:a a);
          equal (option rel_path) ~msg:"abs root relativizes every path"
            (Some (rel "a/b"))
            (Lpath.Abs.relativize ~root:Lpath.Abs.root a));
      prop "l12_rel_strict_is_within_minus_equal" rel_containment_pair
        (fun (root, p) ->
          equal bool ~msg:"strict = within && not equal"
            (Lpath.Rel.is_within ~root p && not (Lpath.Rel.equal root p))
            (Lpath.Rel.is_strictly_within ~root p));
      prop "l12_abs_strict_is_within_minus_equal" abs_containment_pair
        (fun (root, p) ->
          equal bool ~msg:"strict = within && not equal"
            (Lpath.Abs.is_within ~root p && not (Lpath.Abs.equal root p))
            (Lpath.Abs.is_strictly_within ~root p));
      test "l12_strict_excludes_equal_root" (fun () ->
          (* The equality boundary the strict predicate exists to get right: a
             root de-duplicator using the inclusive form would prune a root as a
             descendant of itself. *)
          is_false ~msg:"rel strict a a"
            (Lpath.Rel.is_strictly_within ~root:(rel "a") (rel "a"));
          is_true ~msg:"rel within a a"
            (Lpath.Rel.is_within ~root:(rel "a") (rel "a"));
          is_true ~msg:"rel strict a a/b"
            (Lpath.Rel.is_strictly_within ~root:(rel "a") (rel "a/b"));
          is_false ~msg:"abs strict /a /a"
            (Lpath.Abs.is_strictly_within ~root:(abs "/a") (abs "/a"));
          is_true ~msg:"abs strict /a /a/b"
            (Lpath.Abs.is_strictly_within ~root:(abs "/a") (abs "/a/b")));
      prop "l13_rel_root_contains_all" rel_gen (fun p ->
          is_true ~msg:"root within"
            (Lpath.Rel.is_within ~root:Lpath.Rel.root p));
      prop "l13_abs_root_contains_all" abs_gen (fun p ->
          is_true ~msg:"root within"
            (Lpath.Abs.is_within ~root:Lpath.Abs.root p));
      test "l14_component_boundary_not_string_prefix" (fun () ->
          is_false ~msg:"rel src does not contain src-lib/a"
            (Lpath.Rel.is_within ~root:(rel "src") (rel "src-lib/a"));
          is_true ~msg:"rel src contains src/lib"
            (Lpath.Rel.is_within ~root:(rel "src") (rel "src/lib"));
          is_false ~msg:"abs /src does not contain /src-lib/a"
            (Lpath.Abs.is_within ~root:(abs "/src") (abs "/src-lib/a"));
          is_true ~msg:"abs /src contains /src/lib"
            (Lpath.Abs.is_within ~root:(abs "/src") (abs "/src/lib"));
          equal (option string) ~msg:"relativize sibling prefix is None" None
            (Option.map Lpath.Rel.to_string
               (Lpath.Rel.relativize ~root:(rel "src") (rel "src-lib/a.ml"))));
      prop "l14_string_prefix_is_not_containment" rel_gen (fun root ->
          if Lpath.Rel.is_root root then ()
          else
            let sibling = rel (Lpath.Rel.to_string root ^ "x") in
            is_false
              ~msg:
                (Lpath.Rel.to_string sibling
                ^ " not within " ^ Lpath.Rel.to_string root)
              (Lpath.Rel.is_within ~root sibling));
    ]

let resolve_dispatch =
  group "resolve dispatch"
    [
      test "resolve examples" (fun () ->
          equal (result string error) ~msg:"rel resolve normalizes below base"
            (Ok "src/test/a.ml")
            (Result.map Lpath.Rel.to_string
               (Lpath.Rel.resolve (rel "src") "lib/../test/a.ml"));
          equal (result string error) ~msg:"abs resolve accepts relative"
            (Ok "/test/a.ml")
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve (abs "/workspace") "../test/a.ml"));
          equal (result string error) ~msg:"abs resolve rejects absolute"
            (Error Lpath.Error.Absolute)
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve (abs "/workspace") "/test/a.ml")));
      test "resolve_any dispatches on absolute versus relative" (fun () ->
          let base = abs "/workspace/project" in
          equal (result string error) ~msg:"absolute ignores base"
            (Ok "/etc/hosts")
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve_any ~base "/etc/hosts"));
          equal (result string error) ~msg:"absolute collapses . and .."
            (Ok "/etc/hosts")
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve_any ~base "/etc/./sub/../hosts"));
          equal (result string error) ~msg:"relative resolves below base"
            (Ok "/workspace/project/src/a.ml")
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve_any ~base "src/a.ml"));
          equal (result string error) ~msg:"relative .. climbs against base"
            (Ok "/workspace/src/a.ml")
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve_any ~base "../src/a.ml"));
          equal (result string error) ~msg:"empty is rejected"
            (Error Lpath.Error.Empty)
            (Result.map Lpath.Abs.to_string (Lpath.Abs.resolve_any ~base ""));
          equal (result string error) ~msg:"malformed relative component"
            (Error (Lpath.Error.Malformed_component "a\000b"))
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve_any ~base "a\000b"));
          equal (result string error) ~msg:"backslash is absolute-looking"
            (Error Lpath.Error.Absolute)
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve_any ~base "\\etc"));
          equal (result string error) ~msg:"drive prefix is absolute-looking"
            (Error Lpath.Error.Absolute)
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve_any ~base "C:etc"));
          equal (result string error) ~msg:"drive slash is absolute-looking"
            (Error Lpath.Error.Absolute)
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve_any ~base "C:/etc")));
      prop "l18_rel_resolve_root_equals_of_string" raw_path_input_gen (fun s ->
          equal (result rel_path error) ~msg:(String.escaped s)
            (Lpath.Rel.of_string s)
            (Lpath.Rel.resolve Lpath.Rel.root s));
      prop "l19_resolve_any_absolute_ignores_base" (Gen.pair abs_gen abs_gen)
        (fun (base, p) ->
          equal (result abs_path error) ~msg:"resolve_any (to_string p)" (Ok p)
            (Lpath.Abs.resolve_any ~base (Lpath.Abs.to_string p)));
      prop "l20_resolve_any_agrees_on_relative" (Gen.pair abs_gen rel_gen)
        (fun (base, r) ->
          let s = Lpath.Rel.to_string r in
          equal (result abs_path error)
            ~msg:("resolve_any agrees with resolve on " ^ String.escaped s)
            (Lpath.Abs.resolve base s)
            (Lpath.Abs.resolve_any ~base s));
      (* The canonical property above never feeds a [.] or [..] segment; raw,
         non-slash-rooted input ([./a], [a/../b], deep [..] climbs) is the
         interesting relative surface, and resolve_any must still delegate to
         resolve there. *)
      prop "l20_resolve_any_agrees_on_raw_relative" (Gen.pair abs_gen raw_path_input_gen)
        (fun (base, s) ->
          if String.starts_with ~prefix:"/" s then ()
          else
            equal (result abs_path error)
              ~msg:("resolve_any agrees with resolve on " ^ String.escaped s)
              (Lpath.Abs.resolve base s)
              (Lpath.Abs.resolve_any ~base s));
    ]

let dotdot_asymmetry =
  group "the .. asymmetry"
    [
      test "l21_abs_clamps_at_root" (fun () ->
          equal (result string error) ~msg:"/../a normalizes to /a" (Ok "/a")
            (abs_string "/../a");
          equal (result string error) ~msg:"/.. normalizes to /" (Ok "/")
            (abs_string "/..");
          equal (result string error) ~msg:"/a/../.. normalizes to /" (Ok "/")
            (abs_string "/a/../..");
          equal (result string error) ~msg:"resolve root .. stays at root"
            (Ok "/")
            (Result.map Lpath.Abs.to_string
               (Lpath.Abs.resolve Lpath.Abs.root "..")));
      prop "l21_abs_never_escapes_root" (Gen.pair abs_gen raw_path_input_gen)
        (fun (base, s) ->
          not_escapes "Abs.of_string" (Lpath.Abs.of_string s);
          not_escapes "Abs.resolve" (Lpath.Abs.resolve base s);
          not_escapes "Abs.resolve_any" (Lpath.Abs.resolve_any ~base s));
      test "l22_rel_over_pop_escapes_root" (fun () ->
          equal (result string error) ~msg:"../a"
            (Error Lpath.Error.Escapes_root) (rel_string "../a");
          equal (result string error) ~msg:"a/../../b"
            (Error Lpath.Error.Escapes_root) (rel_string "a/../../b");
          equal (result string error) ~msg:".." (Error Lpath.Error.Escapes_root)
            (rel_string "..");
          equal (result string error) ~msg:"resolve root .."
            (Error Lpath.Error.Escapes_root)
            (Result.map Lpath.Rel.to_string
               (Lpath.Rel.resolve Lpath.Rel.root "..")));
      prop "l22_over_pop_beyond_depth_escapes_root" rel_gen (fun p ->
          let n = List.length (Lpath.Rel.components p) in
          let pops = String.concat "/" (List.init (n + 1) (fun _ -> "..")) in
          let input = Lpath.Rel.to_string p ^ "/" ^ pops in
          equal (result rel_path error) ~msg:(String.escaped input)
            (Error Lpath.Error.Escapes_root)
            (Lpath.Rel.of_string input));
    ]

let error_classification =
  group "error classification"
    [
      test "l25_empty_is_empty_error" (fun () ->
          equal (result string error) ~msg:"rel" (Error Lpath.Error.Empty)
            (rel_string "");
          equal (result string error) ~msg:"abs" (Error Lpath.Error.Empty)
            (abs_string ""));
      test "l26_rel_absolute_looking_witnesses" (fun () ->
          List.iter
            (fun s ->
              equal (result string error) ~msg:s (Error Lpath.Error.Absolute)
                (rel_string s))
            [ "/a"; "\\a"; "C:a" ]);
      prop "l26_rel_absolute_looking_iff" raw_path_input_gen (fun s ->
          cover ~label:"absolute-looking" ~at_least:15.0 (starts_absolute s);
          equal bool ~msg:(String.escaped s) (starts_absolute s)
            (match Lpath.Rel.of_string s with
            | Error Lpath.Error.Absolute -> true
            | _ -> false));
      test "l27_abs_relative_witnesses" (fun () ->
          List.iter
            (fun (s, e) ->
              equal (result string error) ~msg:s (Error e) (abs_string s))
            [
              ("a", Lpath.Error.Relative);
              ("\\a", Lpath.Error.Relative);
              ("C:a", Lpath.Error.Relative);
              ("/a\\b", Lpath.Error.Malformed_component "a\\b");
              ("/a\000b", Lpath.Error.Malformed_component "a\000b");
            ]);
      prop "l27_abs_relative_iff" raw_path_input_gen (fun s ->
          let relative = s <> "" && not (String.starts_with ~prefix:"/" s) in
          cover ~label:"relative" ~at_least:15.0 relative;
          equal bool ~msg:(String.escaped s) relative
            (match Lpath.Abs.of_string s with
            | Error Lpath.Error.Relative -> true
            | _ -> false));
      test "t6_rel_of_string_exn_message" (fun () ->
          raises
            (Invalid_argument "Lpath.Rel.of_string_exn \"../a\": path escapes root") (fun () ->
              Lpath.Rel.of_string_exn "../a"));
      test "t6_abs_of_string_exn_message" (fun () ->
          raises
            (Invalid_argument "Lpath.Abs.of_string_exn \"a\": path must be absolute") (fun () ->
              Lpath.Abs.of_string_exn "a"));
      test "malformed message names the rejected component" (fun () ->
          match Lpath.Rel.add_component (rel "dir") "C:temp" with
          | Ok p ->
              failf "expected malformed component, got %s"
                (Lpath.Rel.to_string p)
          | Error e ->
              let message = Lpath.Error.message e in
              is_true ~msg:message (String.includes ~affix:"C:temp" message));
    ]

let ordering_and_equality =
  group "ordering and equality"
    [
      test "l23_byte_order_witness" (fun () ->
          (* Byte order, not component order: '-' (0x2d) precedes '/' (0x2f), so
             "a-b" sorts before "a/b" although grouping by component orders
             "a/b" first. *)
          is_true ~msg:"rel a-b < a/b"
            (Lpath.Rel.compare (rel "a-b") (rel "a/b") < 0);
          is_true ~msg:"abs /a-b < /a/b"
            (Lpath.Abs.compare (abs "/a-b") (abs "/a/b") < 0));
      prop "l23_rel_compare_is_byte_order" (Gen.pair rel_gen rel_gen)
        (fun (a, b) ->
          equal int ~msg:"sign matches String.compare on to_string"
            (sign
               (String.compare (Lpath.Rel.to_string a) (Lpath.Rel.to_string b)))
            (sign (Lpath.Rel.compare a b)));
      prop "l23_abs_compare_is_byte_order" (Gen.pair abs_gen abs_gen)
        (fun (a, b) ->
          equal int ~msg:"sign matches String.compare on to_string"
            (sign
               (String.compare (Lpath.Abs.to_string a) (Lpath.Abs.to_string b)))
            (sign (Lpath.Abs.compare a b)));
      test "l24_case_sensitivity" (fun () ->
          is_false ~msg:"rel A <> a" (Lpath.Rel.equal (rel "A") (rel "a"));
          is_true ~msg:"rel compare A a <> 0"
            (Lpath.Rel.compare (rel "A") (rel "a") <> 0);
          is_false ~msg:"abs /A <> /a" (Lpath.Abs.equal (abs "/A") (abs "/a")));
      test "rel equality and collections use normalized keys" (fun () ->
          let normalized = rel "src/./a.ml" and path = rel "src/a.ml" in
          is_true ~msg:"equal" (Lpath.Rel.equal normalized path);
          equal int ~msg:"compare = 0" 0 (Lpath.Rel.compare normalized path);
          equal int ~msg:"hash" (Lpath.Rel.hash path)
            (Lpath.Rel.hash normalized);
          is_true ~msg:"set membership"
            (Lpath.Rel.Set.mem normalized (Lpath.Rel.Set.singleton path));
          equal (option string) ~msg:"map lookup" (Some "file")
            (Lpath.Rel.Map.find_opt normalized
               (Lpath.Rel.Map.singleton path "file")));
      test "abs equality and collections use normalized keys" (fun () ->
          let normalized = abs "/workspace/./src/a.ml"
          and path = abs "/workspace/src/a.ml" in
          is_true ~msg:"equal" (Lpath.Abs.equal normalized path);
          equal int ~msg:"compare = 0" 0 (Lpath.Abs.compare normalized path);
          equal int ~msg:"hash" (Lpath.Abs.hash path)
            (Lpath.Abs.hash normalized);
          is_true ~msg:"set membership"
            (Lpath.Abs.Set.mem normalized (Lpath.Abs.Set.singleton path));
          equal (option string) ~msg:"map lookup" (Some "file")
            (Lpath.Abs.Map.find_opt normalized
               (Lpath.Abs.Map.singleton path "file")));
      prop "rel compare = 0 agrees with equal" (Gen.pair rel_gen rel_gen)
        (fun (a, b) ->
          is_true (Lpath.Rel.compare a b = 0 = Lpath.Rel.equal a b));
      prop "abs compare = 0 agrees with equal" (Gen.pair abs_gen abs_gen)
        (fun (a, b) ->
          is_true (Lpath.Abs.compare a b = 0 = Lpath.Abs.equal a b));
      prop "rel compare sign is antisymmetric" (Gen.pair rel_gen rel_gen)
        (fun (a, b) ->
          is_true (sign (Lpath.Rel.compare a b) = -sign (Lpath.Rel.compare b a)));
      prop "abs compare sign is antisymmetric" (Gen.pair abs_gen abs_gen)
        (fun (a, b) ->
          is_true (sign (Lpath.Abs.compare a b) = -sign (Lpath.Abs.compare b a)));
      prop "rel compare is a transitive total order"
        (Gen.triple rel_gen rel_gen rel_gen) (fun (a, b, c) ->
          match List.sort Lpath.Rel.compare [ a; b; c ] with
          | [ x; y; z ] ->
              is_true
                (Lpath.Rel.compare x y <= 0
                && Lpath.Rel.compare y z <= 0
                && Lpath.Rel.compare x z <= 0)
          | _ -> ());
      prop "abs compare is a transitive total order"
        (Gen.triple abs_gen abs_gen abs_gen) (fun (a, b, c) ->
          match List.sort Lpath.Abs.compare [ a; b; c ] with
          | [ x; y; z ] ->
              is_true
                (Lpath.Abs.compare x y <= 0
                && Lpath.Abs.compare y z <= 0
                && Lpath.Abs.compare x z <= 0)
          | _ -> ());
    ]

let pretty_printers =
  group "pretty printers"
    [
      prop "rel pp is to_string" rel_gen (fun p ->
          equal string (Format.asprintf "%a" Lpath.Rel.pp p)
            (Lpath.Rel.to_string p));
      prop "abs pp is to_string" abs_gen (fun p ->
          equal string (Format.asprintf "%a" Lpath.Abs.pp p)
            (Lpath.Abs.to_string p));
    ]

let () =
  run "lpath"
    [
      error_diagnostics;
      component_grammar;
      drive_prefix_boundary;
      byte_components;
      relative_paths;
      absolute_paths;
      append_algebra;
      containment;
      resolve_dispatch;
      dotdot_asymmetry;
      error_classification;
      ordering_and_equality;
      pretty_printers;
    ]
