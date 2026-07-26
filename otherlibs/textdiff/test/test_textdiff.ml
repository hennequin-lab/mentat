(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
module Diff = Textdiff
module Json = Jsont.Json

(* Self-contained test helpers (the otherlibs member ships with its own
   regression net; these mirror the unit suite's shared helpers). *)

let expect_invalid_arg ?expected msg f =
  match expected with
  | Some expected -> raises_invalid_arg ~msg expected (fun () -> ignore (f ()))
  | None -> (
      match f () with
      | _ -> failf "%s: expected Invalid_argument" msg
      | exception Invalid_argument _ -> ())

let check msg predicate = is_true ~msg predicate

let json_object fields =
  fields
  |> List.map (fun (name, value) -> Json.mem (Json.name name) value)
  |> Json.object'

let json_array values = Json.list values

let decode codec json =
  match Json.decode codec json with
  | Ok value -> value
  | Error message -> failf "decode failed: %s" message

let encode codec value =
  match Json.encode codec value with
  | Ok json -> json
  | Error message -> failf "encode failed: %s" message

(* This suite pins the [textdiff] contract: the three
   projections that never mix (hunks / render / stats), newline-as-line-identity,
   display-safety as an invariant of [render], the [Limits] mechanism, and the
   wire codecs [Hunk.jsont] (byte-exact, deriving the counts and line numbers
   on decode) and [Stats.jsont] (the one wire form for the counts). It covers
   both the unit corpus and the rendered-output snapshots directly. The surface
   has no [render_mode]/[`Raw],
   no [Label.equal]/[compare], and one [Hunk.jsont] codec.
   It tests the documented contract, not the implementation. *)

(* Helpers *)

let lbl = Diff.Label.of_string
let some_change msg = function Some c -> c | None -> failf "%s" msg
let some_hunks msg = function Some hs -> hs | None -> failf "%s" msg

let pp_stats ppf (s : Diff.stats) =
  Format.fprintf ppf "{ files = %d; additions = %d; deletions = %d }"
    s.Diff.files s.Diff.additions s.Diff.deletions

let stats_eq (a : Diff.stats) (b : Diff.stats) =
  Int.equal a.Diff.files b.Diff.files
  && Int.equal a.Diff.additions b.Diff.additions
  && Int.equal a.Diff.deletions b.Diff.deletions

let stats_testable = testable ~pp:pp_stats ~equal:stats_eq ()
let hunk = testable ~pp:Diff.Hunk.pp ~equal:Diff.Hunk.equal ()

let render_text ?limits ?context changes =
  Diff.to_string (Diff.render ?limits ?context changes)

let line_repr line = Format.asprintf "%a" Diff.Hunk.Line.pp line
let hunk_line_reprs hunk = List.map line_repr (Diff.Hunk.lines hunk)

let hunk_header hunk =
  match String.split_on_char '\n' (Format.asprintf "%a" Diff.Hunk.pp hunk) with
  | header :: _ -> header
  | [] -> failf "empty hunk rendering"

let numbered_lines count =
  let buffer = Buffer.create (count * 8) in
  for i = 1 to count do
    Buffer.add_string buffer "line ";
    Buffer.add_string buffer (string_of_int i);
    Buffer.add_char buffer '\n'
  done;
  Buffer.contents buffer

(* Generators (safe display content: no control bytes to escape, so rendered
   headers and stats are unaffected by escaping). *)

let label_gen =
  Gen.map Diff.Label.escaped
    (Gen.string_size (Gen.int_range 0 12)
       (Gen.oneofl [ 'a'; 'b'; 'c'; '/'; '+'; '-'; '\n'; '\r'; '\000' ]))

let text_gen =
  Gen.string_size (Gen.int_range 0 12)
    (Gen.oneofl [ 'a'; 'b'; 'c'; '+'; '-'; ' '; '\n' ])

let file_change_gen =
  Gen.bind label_gen (fun label ->
      Gen.bind text_gen (fun before ->
          Gen.bind text_gen (fun after ->
              Gen.oneof
                [
                  Gen.pure (Diff.File_change.create ~label ~contents:after);
                  Gen.pure (Diff.File_change.delete ~label ~contents:before);
                  Gen.pure (Diff.File_change.modify ~label ~before ~after);
                ])))

let pp_file_change ppf change =
  Format.fprintf ppf "<change %a>" Diff.Label.pp (Diff.File_change.label change)

let file_change = testable ~pp:pp_file_change ~gen:file_change_gen ()

let text_pair_gen =
  Gen.bind text_gen (fun before ->
      Gen.map (fun after -> (before, after)) text_gen)

let pp_text_pair ppf (before, after) =
  Format.fprintf ppf "(%S, %S)" before after

let text_pair = testable ~pp:pp_text_pair ~gen:text_pair_gen ()

(* Random *byte* pairs, newline-delimited, so lines routinely hold invalid
   UTF-8. [Gen.char] is uniform over all 256 byte values. *)
let byte_line_char = Gen.frequency [ (1, Gen.pure '\n'); (5, Gen.char) ]
let byte_text_gen = Gen.string_size (Gen.int_range 0 24) byte_line_char
let byte_pair_gen = Gen.pair byte_text_gen byte_text_gen
let byte_pair = testable ~pp:pp_text_pair ~gen:byte_pair_gen ()

(* Adversarial display content: raw control bytes, DEL, C1, and spliced bidi
   sequences. *)
let bidi_sequences =
  [
    "\226\128\174";
    "\226\128\142";
    "\226\128\143";
    "\216\156";
    "\226\129\169";
    "\226\128\170";
  ]

let adversarial_chunk =
  Gen.frequency
    [
      (5, Gen.map (String.make 1) Gen.char);
      (2, Gen.map (String.make 1) (Gen.oneofl [ '\n'; '\t'; '\r'; '\027' ]));
      (1, Gen.oneofl bidi_sequences);
    ]

let adversarial_text_gen =
  Gen.map (String.concat "")
    (Gen.list_size (Gen.int_range 0 20) adversarial_chunk)

let adversarial_text =
  testable
    ~pp:(fun ppf s -> Format.fprintf ppf "%S" s)
    ~gen:adversarial_text_gen ()

(* Codec helpers. *)

let wire_line ~kind ?text ?hex ~newline () =
  let base = [ ("kind", Json.string kind) ] in
  let base =
    match text with
    | Some t -> base @ [ ("text", Json.string t) ]
    | None -> base
  in
  let base =
    match hex with Some h -> base @ [ ("hex", Json.string h) ] | None -> base
  in
  json_object (base @ [ ("newline", Json.bool newline) ])

let wire_hunk ~old_start ~new_start lines =
  json_object
    [
      ("old_start", Json.int old_start);
      ("new_start", Json.int new_start);
      ("lines", json_array lines);
    ]

let roundtrip_hunk h = decode Diff.Hunk.jsont (encode Diff.Hunk.jsont h)

let decode_fails ~msg json =
  is_true ~msg (Result.is_error (Json.decode Diff.Hunk.jsont json))

(* The member names present on each encoded line, in encode order. *)
let encoded_line_member_names h =
  match encode Diff.Hunk.jsont h with
  | Jsont.Object (members, _) -> (
      match Json.find_mem "lines" members with
      | Some (_, Jsont.Array (lines, _)) ->
          List.map
            (function
              | Jsont.Object (m, _) -> Json.object_names m
              | _ -> failf "encoded line is not an object")
            lines
      | _ -> failf "encoded hunk has no lines array")
  | _ -> failf "encoded hunk is not an object"

(* Labels. *)

let labels =
  group "labels"
    [
      test "validates and formats labels" (fun () ->
          equal string ~msg:"label formats as display text" "lib/a.ml"
            (Diff.Label.to_string (lbl "lib/a.ml"));
          equal string ~msg:"pp matches to_string" "lib/a.ml"
            (Format.asprintf "%a" Diff.Label.pp (lbl "lib/a.ml"));
          expect_invalid_arg ~expected:"diff label must not be empty"
            "empty labels are rejected" (fun () -> Diff.Label.of_string "");
          expect_invalid_arg ~expected:"diff label is malformed"
            "newline labels are rejected" (fun () ->
              Diff.Label.of_string "a\nb");
          expect_invalid_arg ~expected:"diff label is malformed"
            "carriage-return labels are rejected" (fun () ->
              Diff.Label.of_string "a\rb");
          expect_invalid_arg ~expected:"diff label is malformed"
            "NUL labels are rejected" (fun () -> Diff.Label.of_string "a\000b"));
      test "escapes arbitrary labels" (fun () ->
          equal string ~msg:"newlines are escaped" "a\\nb"
            (Diff.Label.to_string (Diff.Label.escaped "a\nb"));
          equal string ~msg:"control characters are escaped" "a\\rb\\000c"
            (Diff.Label.to_string (Diff.Label.escaped "a\rb\000c"));
          equal string ~msg:"empty escaped labels are stable" "<empty>"
            (Diff.Label.to_string (Diff.Label.escaped ""));
          equal string ~msg:"escaped labels are accepted by of_string"
            "a\\nb\\000c"
            (Diff.Label.to_string
               (Diff.Label.of_string
                  (Diff.Label.to_string (Diff.Label.escaped "a\nb\000c")))));
      prop' "escaped labels round-trip through of_string" string (fun text ->
          let label = Diff.Label.escaped text in
          let reparsed =
            no_raise (fun () ->
                Diff.Label.of_string (Diff.Label.to_string label))
          in
          equal string ~msg:"escaped label re-parses to the same text"
            (Diff.Label.to_string label)
            (Diff.Label.to_string reparsed));
    ]

(* File changes. *)

let file_changes =
  group "file changes"
    [
      test "builds file changes from optional states" (fun () ->
          let l = lbl "state.txt" in
          is_none ~msg:"absent-to-absent is None"
            (Diff.File_change.of_states ~label:l ~before:None ~after:None);
          let created =
            some_change "expected create"
              (Diff.File_change.of_states ~label:l ~before:None
                 ~after:(Some "new\n"))
          in
          let deleted =
            some_change "expected delete"
              (Diff.File_change.of_states ~label:l ~before:(Some "old\n")
                 ~after:None)
          in
          let modified =
            some_change "expected modify"
              (Diff.File_change.of_states ~label:l ~before:(Some "old\n")
                 ~after:(Some "new\n"))
          in
          equal string ~msg:"label is preserved" "state.txt"
            (Diff.Label.to_string (Diff.File_change.label created));
          equal (option string) ~msg:"created has no before" None
            (Diff.File_change.before created);
          equal (option string) ~msg:"created has after contents" (Some "new\n")
            (Diff.File_change.after created);
          equal (option string) ~msg:"deleted has before contents"
            (Some "old\n")
            (Diff.File_change.before deleted);
          equal (option string) ~msg:"deleted has no after" None
            (Diff.File_change.after deleted);
          equal stats_testable ~msg:"modify counts one add and one delete"
            (Diff.Stats.v ~files:1 ~additions:1 ~deletions:1)
            (Diff.Stats.of_changes [ modified ]));
      test "constructors expose their sides" (fun () ->
          let l = lbl "a.ml" in
          let created = Diff.File_change.create ~label:l ~contents:"x\n" in
          equal (option string) ~msg:"create before" None
            (Diff.File_change.before created);
          equal (option string) ~msg:"create after" (Some "x\n")
            (Diff.File_change.after created);
          let deleted = Diff.File_change.delete ~label:l ~contents:"y\n" in
          equal (option string) ~msg:"delete before" (Some "y\n")
            (Diff.File_change.before deleted);
          equal (option string) ~msg:"delete after" None
            (Diff.File_change.after deleted);
          let modified =
            Diff.File_change.modify ~label:l ~before:"y\n" ~after:"z\n"
          in
          equal (option string) ~msg:"modify before" (Some "y\n")
            (Diff.File_change.before modified);
          equal (option string) ~msg:"modify after" (Some "z\n")
            (Diff.File_change.after modified));
      test "omits no-op modifications" (fun () ->
          let diff =
            Diff.render
              [
                Diff.File_change.modify ~label:(lbl "same.txt") ~before:"same\n"
                  ~after:"same\n";
              ]
          in
          is_true ~msg:"unchanged files do not render"
            (Int.equal (Diff.stats diff).Diff.files 0);
          equal string ~msg:"empty diff has empty text" "" (Diff.to_string diff);
          equal stats_testable ~msg:"empty diff has zero stats"
            (Diff.Stats.v ~files:0 ~additions:0 ~deletions:0)
            (Diff.stats diff));
    ]

(* Statistics. *)

let statistics =
  group "statistics"
    [
      test "Stats.of_changes matches rendered stats" (fun () ->
          let changes =
            [
              Diff.File_change.create ~label:(lbl "new.txt") ~contents:"new\n";
              Diff.File_change.delete ~label:(lbl "old.txt") ~contents:"old\n";
              Diff.File_change.modify ~label:(lbl "changed.txt")
                ~before:"a\nb\n" ~after:"a\nB\n";
              Diff.File_change.modify ~label:(lbl "same.txt") ~before:"same\n"
                ~after:"same\n";
            ]
          in
          equal stats_testable ~msg:"Stats.of_changes equals render stats"
            (Diff.stats (Diff.render changes))
            (Diff.Stats.of_changes changes);
          equal int ~msg:"unlimited render omits nothing" 0
            (Diff.omitted (Diff.render changes)));
      test "pure creates and deletes count lines directly" (fun () ->
          let line_count = 2_048 in
          let contents = numbered_lines line_count in
          let created =
            Diff.File_change.create ~label:(lbl "created.txt") ~contents
          in
          let deleted =
            Diff.File_change.delete ~label:(lbl "deleted.txt") ~contents
          in
          equal stats_testable ~msg:"unrendered pure counts"
            (Diff.Stats.v ~files:2 ~additions:line_count ~deletions:line_count)
            (Diff.Stats.of_changes [ created; deleted ]);
          let diff = Diff.render ~context:0 [ created; deleted ] in
          equal stats_testable ~msg:"rendered pure counts"
            (Diff.Stats.v ~files:2 ~additions:line_count ~deletions:line_count)
            (Diff.stats diff);
          equal int ~msg:"pure render omits nothing" 0 (Diff.omitted diff));
      test "Stats.v carries external counts (numstat bridge)" (fun () ->
          let s = Diff.Stats.v ~files:2 ~additions:5 ~deletions:3 in
          equal int ~msg:"files" 2 s.Diff.files;
          equal int ~msg:"additions" 5 s.Diff.additions;
          equal int ~msg:"deletions" 3 s.Diff.deletions);
      test "Stats.v rejects negative counts" (fun () ->
          expect_invalid_arg "negative files are rejected" (fun () ->
              Diff.Stats.v ~files:(-1) ~additions:0 ~deletions:0);
          expect_invalid_arg "negative additions are rejected" (fun () ->
              Diff.Stats.v ~files:0 ~additions:(-1) ~deletions:0);
          expect_invalid_arg "negative deletions are rejected" (fun () ->
              Diff.Stats.v ~files:0 ~additions:0 ~deletions:(-1)));
      prop' "Stats.of_changes equals render stats without limits"
        (list file_change) (fun changes ->
          let diff = Diff.render changes in
          equal stats_testable ~msg:"stats agree" (Diff.stats diff)
            (Diff.Stats.of_changes changes);
          equal int ~msg:"unlimited render omits nothing" 0 (Diff.omitted diff));
    ]

(* Rendering. *)

let rendering =
  group "rendering"
    [
      test "renders a basic modification" (fun () ->
          equal string ~msg:"basic modify"
            "--- file.txt\n+++ file.txt\n@@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n"
            (render_text
               [
                 Diff.File_change.modify ~label:(lbl "file.txt")
                   ~before:"a\nb\nc\n" ~after:"a\nB\nc\n";
               ]));
      test "creations and deletions use /dev/null" (fun () ->
          equal string ~msg:"create and delete"
            (String.concat ""
               [
                 "--- /dev/null\n+++ new.txt\n@@ -0,0 +1,1 @@\n+hello\n";
                 "--- old.txt\n+++ /dev/null\n@@ -1,1 +0,0 @@\n-bye\n";
               ])
            (render_text
               [
                 Diff.File_change.create ~label:(lbl "new.txt")
                   ~contents:"hello\n";
                 Diff.File_change.delete ~label:(lbl "old.txt")
                   ~contents:"bye\n";
               ]));
      test "honours a requested context" (fun () ->
          equal string ~msg:"context 0"
            "--- file.txt\n+++ file.txt\n@@ -2,1 +2,1 @@\n-b\n+B\n"
            (render_text ~context:0
               [
                 Diff.File_change.modify ~label:(lbl "file.txt")
                   ~before:"a\nb\nc\n" ~after:"a\nB\nc\n";
               ]));
      test "splits separated changes into hunks" (fun () ->
          equal string ~msg:"two hunks"
            (String.concat ""
               [
                 "--- multi.txt\n+++ multi.txt\n";
                 "@@ -1,3 +1,3 @@\n a\n-b\n+B\n c\n";
                 "@@ -5,3 +5,3 @@\n e\n-f\n+F\n g\n";
               ])
            (render_text ~context:1
               [
                 Diff.File_change.modify ~label:(lbl "multi.txt")
                   ~before:"a\nb\nc\nd\ne\nf\ng\n"
                   ~after:"a\nB\nc\nd\ne\nF\ng\n";
               ]));
      test "marks a missing final newline on each side" (fun () ->
          equal string ~msg:"modified without a trailing newline"
            (String.concat ""
               [
                 "--- file.txt\n+++ file.txt\n@@ -1,1 +1,1 @@\n";
                 "-hello\n\\ No newline at end of file\n+hello\n";
               ])
            (render_text
               [
                 Diff.File_change.modify ~label:(lbl "file.txt") ~before:"hello"
                   ~after:"hello\n";
               ]);
          equal string ~msg:"deleted without a trailing newline"
            (String.concat ""
               [
                 "--- old.txt\n+++ /dev/null\n@@ -1,1 +0,0 @@\n";
                 "-bye\n\\ No newline at end of file\n";
               ])
            (render_text
               [
                 Diff.File_change.delete ~label:(lbl "old.txt") ~contents:"bye";
               ]);
          equal string ~msg:"created without a trailing newline"
            (String.concat ""
               [
                 "--- /dev/null\n+++ new.txt\n@@ -0,0 +1,1 @@\n";
                 "+hello\n\\ No newline at end of file\n";
               ])
            (render_text
               [
                 Diff.File_change.create ~label:(lbl "new.txt")
                   ~contents:"hello";
               ]));
      test "renders headers for empty creations and deletions" (fun () ->
          equal string ~msg:"empty create and delete"
            "--- /dev/null\n\
             +++ empty-new.txt\n\
             --- empty-old.txt\n\
             +++ /dev/null\n"
            (render_text
               [
                 Diff.File_change.create ~label:(lbl "empty-new.txt")
                   ~contents:"";
                 Diff.File_change.delete ~label:(lbl "empty-old.txt")
                   ~contents:"";
               ]));
      test "marks boundary insertions and deletions" (fun () ->
          equal string ~msg:"insertions at both boundaries"
            (String.concat ""
               [
                 "--- file.txt\n+++ file.txt\n";
                 "@@ -0,0 +1,1 @@\n+a\n";
                 "@@ -2,0 +4,1 @@\n+d\n";
               ])
            (render_text ~context:0
               [
                 Diff.File_change.modify ~label:(lbl "file.txt")
                   ~before:"b\nc\n" ~after:"a\nb\nc\nd\n";
               ]);
          equal string ~msg:"deletions at both boundaries"
            (String.concat ""
               [
                 "--- file.txt\n+++ file.txt\n";
                 "@@ -1,1 +0,0 @@\n-a\n";
                 "@@ -4,1 +2,0 @@\n-d\n";
               ])
            (render_text ~context:0
               [
                 Diff.File_change.modify ~label:(lbl "file.txt")
                   ~before:"a\nb\nc\nd\n" ~after:"b\nc\n";
               ]));
      test "keeps nearby and overlapping changes in one hunk" (fun () ->
          equal string ~msg:"nearby changes"
            (String.concat ""
               [
                 "--- file.txt\n+++ file.txt\n@@ -1,3 +1,3 @@\n";
                 "-a\n+A\n b\n-c\n+C\n";
               ])
            (render_text ~context:1
               [
                 Diff.File_change.modify ~label:(lbl "file.txt")
                   ~before:"a\nb\nc\n" ~after:"A\nb\nC\n";
               ]);
          equal string ~msg:"overlapping context merges"
            (String.concat ""
               [
                 "--- file.txt\n+++ file.txt\n@@ -1,10 +1,10 @@\n";
                 " a\n-b\n+B\n c\n d\n e\n f\n g\n h\n-i\n+I\n j\n";
               ])
            (render_text ~context:3
               [
                 Diff.File_change.modify ~label:(lbl "file.txt")
                   ~before:"a\nb\nc\nd\ne\nf\ng\nh\ni\nj\n"
                   ~after:"a\nB\nc\nd\ne\nf\ng\nh\nI\nj\n";
               ]));
      test "anchors a change amid repeated lines" (fun () ->
          equal string ~msg:"repeated line context"
            (String.concat ""
               [
                 "--- repeat.txt\n+++ repeat.txt\n@@ -3,3 +3,3 @@\n";
                 " same\n-a\n+b\n same\n";
               ])
            (render_text ~context:1
               [
                 Diff.File_change.modify ~label:(lbl "repeat.txt")
                   ~before:"same\na\nsame\na\nsame\n"
                   ~after:"same\na\nsame\nb\nsame\n";
               ]));
      test "renders files in input order" (fun () ->
          equal string ~msg:"multi-file order follows input"
            (String.concat ""
               [
                 "--- /dev/null\n+++ b.txt\n@@ -0,0 +1,1 @@\n+b\n";
                 "--- /dev/null\n+++ a.txt\n@@ -0,0 +1,1 @@\n+a\n";
               ])
            (render_text
               [
                 Diff.File_change.create ~label:(lbl "b.txt") ~contents:"b\n";
                 Diff.File_change.create ~label:(lbl "a.txt") ~contents:"a\n";
               ]));
      test "rejects a negative context" (fun () ->
          expect_invalid_arg ~expected:"context must be non-negative"
            "render rejects negative context" (fun () ->
              Diff.render ~context:(-1) []));
    ]

(* Limits. *)

let limits =
  group "limits"
    [
      test "rejects negative limits" (fun () ->
          expect_invalid_arg ~expected:"max_files must be non-negative"
            "negative max_files" (fun () ->
              Diff.Limits.make ~max_files:(-1) ~max_file_bytes:0 ~max_lines:0 ());
          expect_invalid_arg ~expected:"max_file_bytes must be non-negative"
            "negative max_file_bytes" (fun () ->
              Diff.Limits.make ~max_files:0 ~max_file_bytes:(-1) ~max_lines:0 ());
          expect_invalid_arg ~expected:"max_lines must be non-negative"
            "negative max_lines" (fun () ->
              Diff.Limits.make ~max_files:0 ~max_file_bytes:0 ~max_lines:(-1) ());
          expect_invalid_arg ~expected:"max_edit_distance must be non-negative"
            "negative max_edit_distance" (fun () ->
              Diff.Limits.make ~max_files:0 ~max_file_bytes:0 ~max_lines:0
                ~max_edit_distance:(-1) ()));
      test "a byte limit omits the file's content, keeping its header"
        (fun () ->
          let diff =
            Diff.render
              ~limits:
                (Diff.Limits.make ~max_files:10 ~max_file_bytes:4 ~max_lines:100
                   ())
              [
                Diff.File_change.create ~label:(lbl "big.txt")
                  ~contents:"hello\n";
              ]
          in
          equal string ~msg:"omission note text"
            "--- /dev/null\n\
             +++ big.txt\n\
             [diff omitted: file exceeds 4 byte display limit]\n"
            (Diff.to_string diff);
          equal stats_testable ~msg:"omitted lines are not counted"
            (Diff.Stats.v ~files:1 ~additions:0 ~deletions:0)
            (Diff.stats diff);
          equal int ~msg:"one file omitted" 1 (Diff.omitted diff));
      test "an edit-distance limit omits the file's content" (fun () ->
          let limits =
            Diff.Limits.make ~max_files:10 ~max_file_bytes:1000 ~max_lines:1000
              ~max_edit_distance:1 ()
          in
          let modified =
            Diff.render ~limits
              [
                Diff.File_change.modify ~label:(lbl "rewrite.txt")
                  ~before:"a\nb\n" ~after:"c\nd\n";
              ]
          in
          equal string ~msg:"omission note text"
            "--- rewrite.txt\n\
             +++ rewrite.txt\n\
             [diff omitted: edit distance exceeds 1 display limit]\n"
            (Diff.to_string modified);
          equal stats_testable ~msg:"omitted lines are not counted"
            (Diff.Stats.v ~files:1 ~additions:0 ~deletions:0)
            (Diff.stats modified);
          equal int ~msg:"one file omitted" 1 (Diff.omitted modified);
          let created =
            Diff.render ~limits
              [
                Diff.File_change.create ~label:(lbl "create-rewrite.txt")
                  ~contents:"a\nb\n";
              ]
          in
          equal int ~msg:"a pure creation over the distance limit is omitted" 1
            (Diff.omitted created);
          equal stats_testable ~msg:"omitted creation lines are not counted"
            (Diff.Stats.v ~files:1 ~additions:0 ~deletions:0)
            (Diff.stats created));
      test "a line limit omits the file's content" (fun () ->
          let diff =
            Diff.render
              ~limits:
                (Diff.Limits.make ~max_files:10 ~max_file_bytes:1000
                   ~max_lines:1 ())
              [
                Diff.File_change.create ~label:(lbl "many.txt")
                  ~contents:"x\ny\nz\n";
              ]
          in
          equal int ~msg:"one file omitted" 1 (Diff.omitted diff);
          equal stats_testable ~msg:"omitted lines are not counted"
            (Diff.Stats.v ~files:1 ~additions:0 ~deletions:0)
            (Diff.stats diff));
      test "a file-count limit summarizes the remaining files" (fun () ->
          let diff =
            Diff.render
              ~limits:
                (Diff.Limits.make ~max_files:1 ~max_file_bytes:100
                   ~max_lines:100 ())
              [
                Diff.File_change.create ~label:(lbl "a.txt") ~contents:"a\n";
                Diff.File_change.create ~label:(lbl "b.txt") ~contents:"b\n";
              ]
          in
          equal string ~msg:"first file rendered, remainder summarized"
            (String.concat ""
               [
                 "--- /dev/null\n+++ a.txt\n@@ -0,0 +1,1 @@\n+a\n";
                 "[diff omitted: 1 file exceeds max_files display limit]\n";
               ])
            (Diff.to_string diff);
          equal stats_testable ~msg:"the elided file is counted, not its lines"
            (Diff.Stats.v ~files:2 ~additions:1 ~deletions:0)
            (Diff.stats diff);
          equal int ~msg:"one file omitted" 1 (Diff.omitted diff));
    ]

(* Display safety. *)

(* The escaping policy classifies scalars, not bytes, so a continuation byte in
   the 0x80-0x9F range is only a violation when it is a C1 scalar of its own. *)
let forbidden_output_scalar code =
  (code < 0x20 && code <> 0x09 && code <> 0x0A)
  || code = 0x7F
  || (code >= 0x80 && code <= 0x9F)

let display_safety =
  group "display safety"
    [
      test "escapes bidirectional formatting characters" (fun () ->
          let unsafe_label =
            Diff.Label.of_string "bad\216\156\226\128\142\226\128\143.txt"
          in
          let unsafe_text =
            "safe\027\216\156\226\128\142\226\128\143\226\128\174\n"
          in
          equal string ~msg:"bidi escapes"
            (String.concat ""
               [
                 "--- /dev/null\n+++ bad\\u{061c}\\u{200e}\\u{200f}.txt\n";
                 "@@ -0,0 +1,1 @@\n";
                 "+safe\\x1B\\u{061c}\\u{200e}\\u{200f}\\u{202e}\n";
               ])
            (render_text
               [
                 Diff.File_change.create ~label:unsafe_label
                   ~contents:unsafe_text;
               ]));
      test "escapes C1 control bytes" (fun () ->
          let unsafe_label = Diff.Label.of_string "bad\128\155\159.txt" in
          let unsafe_text = "safe\128\155\159\n" in
          equal string ~msg:"C1 escapes"
            (String.concat ""
               [
                 "--- /dev/null\n+++ bad\\x80\\x9B\\x9F.txt\n";
                 "@@ -0,0 +1,1 @@\n+safe\\x80\\x9B\\x9F\n";
               ])
            (render_text
               [
                 Diff.File_change.create ~label:unsafe_label
                   ~contents:unsafe_text;
               ]));
      test "escapes a carriage return as an ordinary control byte" (fun () ->
          equal string ~msg:"CRLF renders a \\x0D marker per line"
            (String.concat ""
               [
                 "--- /dev/null\n+++ crlf.txt\n@@ -0,0 +1,2 @@\n";
                 "+a\\x0D\n+b\\x0D\n";
               ])
            (render_text
               [
                 Diff.File_change.create ~label:(lbl "crlf.txt")
                   ~contents:"a\r\nb\r\n";
               ]));
      test "preserves printable Unicode" (fun () ->
          equal string ~msg:"multi-byte scalars survive display escaping"
            (String.concat ""
               [
                 "--- /dev/null\n+++ caf\195\169.txt\n@@ -0,0 +1,2 @@\n";
                 "+\230\151\165\230\156\172\232\170\158\n+\195\129\n";
               ])
            (render_text
               [
                 Diff.File_change.create ~label:(lbl "caf\195\169.txt")
                   ~contents:"\230\151\165\230\156\172\232\170\158\n\195\129\n";
               ]));
      prop' "render output is valid UTF-8 without an unescaped control scalar"
        adversarial_text (fun content ->
          let out =
            render_text
              [
                Diff.File_change.create ~label:(lbl "adversarial.txt")
                  ~contents:content;
              ]
          in
          if not (String.is_valid_utf_8 out) then
            failf "render output is not valid UTF-8: %S" out;
          let len = String.length out in
          let rec check i =
            if i < len then
              let decoded = String.get_utf_8_uchar out i in
              let code = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
              if forbidden_output_scalar code then
                failf "unescaped scalar U+%04X in render output: %S" code out
              else check (i + Uchar.utf_decode_length decoded)
          in
          check 0);
    ]

(* Hunks. *)

let hunks =
  group "hunks"
    [
      test "line_counts sums additions and deletions across hunks" (fun () ->
          let hs =
            some_hunks "expected hunks"
              (Diff.hunks ~context:1 ~before:"a\nb\nc\nd\ne\nf\ng\n"
                 ~after:"a\nb\nc\nx\ne\nf\ng\nh\n" ())
          in
          let adds, dels = Diff.Hunk.line_counts hs in
          equal int ~msg:"additions across hunks" 2 adds;
          equal int ~msg:"deletions across hunks" 1 dels;
          let empty_adds, empty_dels = Diff.Hunk.line_counts [] in
          equal int ~msg:"no hunks means no additions" 0 empty_adds;
          equal int ~msg:"no hunks means no deletions" 0 empty_dels);
      test "reports positions and kinds" (fun () ->
          let before = "a\nb\nc\nd\ne\nf\ng\n" in
          let after = "a\nb\nc\nx\ne\nf\ng\nh\n" in
          let hs =
            some_hunks "expected hunks"
              (Diff.hunks ~context:1 ~before ~after ())
          in
          equal int ~msg:"two hunks" 2 (List.length hs);
          let first = List.nth hs 0 in
          let second = List.nth hs 1 in
          equal int ~msg:"first old_start" 3 (Diff.Hunk.old_start first);
          equal int ~msg:"first old_count" 3 (Diff.Hunk.old_count first);
          equal int ~msg:"first new_start" 3 (Diff.Hunk.new_start first);
          equal int ~msg:"first new_count" 3 (Diff.Hunk.new_count first);
          equal (list string) ~msg:"first hunk lines" [ " c"; "-d"; "+x"; " e" ]
            (hunk_line_reprs first);
          let removed = List.nth (Diff.Hunk.lines first) 1 in
          equal (option int) ~msg:"removed keeps its before number" (Some 4)
            (Diff.Hunk.Line.old_line removed);
          equal (option int) ~msg:"removed has no after number" None
            (Diff.Hunk.Line.new_line removed);
          let added = List.nth (Diff.Hunk.lines first) 2 in
          equal (option int) ~msg:"added has no before number" None
            (Diff.Hunk.Line.old_line added);
          equal (option int) ~msg:"added keeps its after number" (Some 4)
            (Diff.Hunk.Line.new_line added);
          equal int ~msg:"second old_start" 7 (Diff.Hunk.old_start second);
          equal int ~msg:"second old_count" 1 (Diff.Hunk.old_count second);
          equal int ~msg:"second new_count" 2 (Diff.Hunk.new_count second);
          let appended = List.nth (Diff.Hunk.lines second) 1 in
          equal (option int) ~msg:"appended line number" (Some 8)
            (Diff.Hunk.Line.new_line appended));
      test "merges touching context into one hunk" (fun () ->
          let hs =
            some_hunks "expected hunks"
              (Diff.hunks ~context:3 ~before:"a\nb\nc\nd\ne\nf\ng\n"
                 ~after:"a\nb\nc\nx\ne\nf\ng\nh\n" ())
          in
          equal int ~msg:"one merged hunk" 1 (List.length hs);
          let h = List.hd hs in
          equal int ~msg:"merged old_count" 7 (Diff.Hunk.old_count h);
          equal int ~msg:"merged new_count" 8 (Diff.Hunk.new_count h));
      test "saturates a huge context to the whole file" (fun () ->
          let hs =
            some_hunks "expected hunks"
              (Diff.hunks ~context:max_int ~before:"a\nb\nc\n"
                 ~after:"a\nB\nc\n" ())
          in
          equal int ~msg:"one hunk" 1 (List.length hs);
          let h = List.hd hs in
          equal int ~msg:"old_start" 1 (Diff.Hunk.old_start h);
          equal int ~msg:"old_count" 3 (Diff.Hunk.old_count h);
          equal int ~msg:"new_start" 1 (Diff.Hunk.new_start h);
          equal int ~msg:"new_count" 3 (Diff.Hunk.new_count h);
          equal (list string) ~msg:"keeps the full file"
            [ " a"; "-b"; "+B"; " c" ] (hunk_line_reprs h));
      test "marks a pure insertion with the empty-range convention" (fun () ->
          let hs =
            some_hunks "expected hunks"
              (Diff.hunks ~context:0 ~before:"a\nb\n" ~after:"a\nx\nb\n" ())
          in
          equal int ~msg:"one hunk" 1 (List.length hs);
          let h = List.hd hs in
          equal int ~msg:"insertion covers no before lines" 0
            (Diff.Hunk.old_count h);
          equal int ~msg:"insertion precedes before line 2" 2
            (Diff.Hunk.old_start h);
          equal int ~msg:"insertion new_start" 2 (Diff.Hunk.new_start h);
          equal int ~msg:"insertion new_count" 1 (Diff.Hunk.new_count h);
          equal string ~msg:"unified empty-range header" "@@ -1,0 +2,1 @@"
            (hunk_header h));
      test "diffs equal and empty texts to Some []" (fun () ->
          equal
            (option (list hunk))
            ~msg:"equal texts are Some []" (Some [])
            (Diff.hunks ~before:"a\nb\n" ~after:"a\nb\n" ());
          equal
            (option (list hunk))
            ~msg:"empty texts are Some []" (Some [])
            (Diff.hunks ~before:"" ~after:"" ()));
      test "treats a trailing-newline change as a real change" (fun () ->
          let hs =
            some_hunks "expected hunks" (Diff.hunks ~before:"a" ~after:"a\n" ())
          in
          equal int ~msg:"one hunk" 1 (List.length hs);
          let lines = Diff.Hunk.lines (List.hd hs) in
          equal (list string) ~msg:"newline change is a remove/add pair"
            [ "-a"; "+a" ] (List.map line_repr lines);
          is_false ~msg:"removed side has no final newline"
            (Diff.Hunk.Line.newline (List.nth lines 0));
          is_true ~msg:"added side has a final newline"
            (Diff.Hunk.Line.newline (List.nth lines 1)));
      test "keeps a carriage return as part of line identity" (fun () ->
          let hs =
            some_hunks "expected hunks"
              (Diff.hunks ~before:"a\n" ~after:"a\r\n" ())
          in
          equal (list string) ~msg:"a CRLF line differs from its LF twin"
            [ "-a"; "+a\r" ]
            (List.map line_repr (Diff.Hunk.lines (List.hd hs)));
          let added =
            some_hunks "expected hunks"
              (Diff.hunks ~before:"" ~after:"a\r\n" ())
          in
          equal string ~msg:"the carriage return is raw in Line.text" "a\r"
            (Diff.Hunk.Line.text (List.hd (Diff.Hunk.lines (List.hd added)))));
      test "bounds hunks by max_edit_distance" (fun () ->
          is_none ~msg:"exceeded distance is None"
            (Diff.hunks ~max_edit_distance:1 ~before:"a\nb\nc\n"
               ~after:"x\ny\nz\n" ());
          let hs =
            some_hunks "within distance"
              (Diff.hunks ~max_edit_distance:6 ~before:"a\nb\nc\n"
                 ~after:"x\ny\nz\n" ())
          in
          equal int ~msg:"within the limit yields hunks" 1 (List.length hs));
      test "rejects a negative context and max_edit_distance" (fun () ->
          expect_invalid_arg ~expected:"context must be non-negative"
            "negative context" (fun () ->
              Diff.hunks ~context:(-1) ~before:"" ~after:"" ());
          expect_invalid_arg ~expected:"max_edit_distance must be non-negative"
            "negative max_edit_distance" (fun () ->
              Diff.hunks ~max_edit_distance:(-1) ~before:"" ~after:"" ()));
      test "Hunk.equal is reflexive and distinguishes content" (fun () ->
          let h =
            List.hd
              (some_hunks "hunks"
                 (Diff.hunks ~before:"a\nb\n" ~after:"a\nB\n" ()))
          in
          is_true ~msg:"reflexive" (Diff.Hunk.equal h h);
          let other =
            List.hd
              (some_hunks "hunks"
                 (Diff.hunks ~before:"a\nb\n" ~after:"A\nb\n" ()))
          in
          is_false ~msg:"different content is not equal"
            (Diff.Hunk.equal h other));
      prop' "hunk ranges agree with rendered @@ headers" text_pair
        (fun (before, after) ->
          let hs = some_hunks "expected hunks" (Diff.hunks ~before ~after ()) in
          let rendered =
            render_text
              [ Diff.File_change.modify ~label:(lbl "file") ~before ~after ]
          in
          let rendered_headers =
            List.filter
              (String.starts_with ~prefix:"@@")
              (String.split_on_char '\n' rendered)
          in
          equal (list string) ~msg:"hunk headers match rendered headers"
            rendered_headers (List.map hunk_header hs));
      prop' "hunk counts agree with their line kinds" text_pair
        (fun (before, after) ->
          let hs = some_hunks "expected hunks" (Diff.hunks ~before ~after ()) in
          List.iter
            (fun h ->
              let context, added, removed =
                List.fold_left
                  (fun (context, added, removed) line ->
                    match Diff.Hunk.Line.kind line with
                    | Diff.Hunk.Line.Context -> (context + 1, added, removed)
                    | Diff.Hunk.Line.Added -> (context, added + 1, removed)
                    | Diff.Hunk.Line.Removed -> (context, added, removed + 1))
                  (0, 0, 0) (Diff.Hunk.lines h)
              in
              equal int ~msg:"old_count counts context and removed lines"
                (Diff.Hunk.old_count h) (context + removed);
              equal int ~msg:"new_count counts context and added lines"
                (Diff.Hunk.new_count h) (context + added))
            hs);
    ]

(* Word-level emphasis. *)

let pp_spans ppf spans =
  Format.fprintf ppf "[%s]"
    (String.concat "; "
       (List.map (fun (a, b) -> Printf.sprintf "(%d,%d)" a b) spans))

let spans_eq a b = List.equal (fun (a, b) (c, d) -> a = c && b = d) a b

let spans_pair =
  testable
    ~pp:(fun ppf (rem, add) ->
      Format.fprintf ppf "(%a, %a)" pp_spans rem pp_spans add)
    ~equal:(fun (r1, a1) (r2, a2) -> spans_eq r1 r2 && spans_eq a1 a2)
    ()

let word_spans =
  group "word spans"
    [
      test "emphasizes one changed word, byte-offset on each side" (fun () ->
          equal spans_pair ~msg:"middle word substituted"
            ([ (4, 7) ], [ (4, 7) ])
            (Diff.word_spans ~before:"foo bar baz" ~after:"foo qux baz" ()));
      test "keeps identical lines span-free" (fun () ->
          equal spans_pair ~msg:"no change means no spans" ([], [])
            (Diff.word_spans ~before:"same line" ~after:"same line" ()));
      test "recovers a changed whitespace run" (fun () ->
          equal spans_pair ~msg:"three spaces to one is a recoverable edit"
            ([ (1, 4) ], [ (1, 2) ])
            (Diff.word_spans ~before:"a   b" ~after:"a b" ()));
      test "suppresses a whole-line rewrite" (fun () ->
          equal spans_pair ~msg:"lines sharing too few words emphasize nothing"
            ([], [])
            (Diff.word_spans ~before:"hello world" ~after:"goodbye cruel planet"
               ());
          equal spans_pair ~msg:"a single differing word token is a rewrite"
            ([], [])
            (Diff.word_spans ~before:"alpha" ~after:"omega" ()));
      test "merges inserted tokens but not across a kept token" (fun () ->
          equal spans_pair ~msg:"an inserted word and space merge into one span"
            ([], [ (4, 8) ])
            (Diff.word_spans ~before:"foo baz" ~after:"foo bar baz" ());
          equal spans_pair
            ~msg:"two changed words split by a kept space stay separate"
            ([ (2, 5); (6, 9) ], [ (2, 5); (6, 9) ])
            (Diff.word_spans ~before:"x foo bar y" ~after:"x qux quz y" ()));
      test "counts multibyte codepoints in byte offsets" (fun () ->
          equal spans_pair
            ~msg:"a CJK word before the change shifts byte offsets"
            ([ (7, 12) ], [ (7, 12) ])
            (Diff.word_spans ~before:"\228\189\160\229\165\189 world"
               ~after:"\228\189\160\229\165\189 earth" ());
          equal spans_pair
            ~msg:"a space-free script does not collapse to one token"
            ([ (3, 6) ], [ (3, 6) ])
            (Diff.word_spans ~before:"\228\189\160\229\165\189"
               ~after:"\228\189\160\229\128\145" ()));
      test "bounds the token diff by max_edit_distance" (fun () ->
          equal spans_pair ~msg:"within the bound emphasizes the changed word"
            ([ (3, 5) ], [ (3, 5) ])
            (Diff.word_spans ~before:"aa bb cc dd" ~after:"aa xx cc dd" ());
          equal spans_pair ~msg:"exceeding the bound emphasizes nothing" ([], [])
            (Diff.word_spans ~max_edit_distance:1 ~before:"aa bb cc dd"
               ~after:"aa xx cc dd" ()));
    ]

(* Three-way merge (diff3). *)

let some_merge msg = function Some m -> m | None -> failf "%s" msg

let pp_region ppf = function
  | Diff.Merge.Region.Stable s -> Format.fprintf ppf "Stable %S" s
  | Diff.Merge.Region.Resolved s -> Format.fprintf ppf "Resolved %S" s
  | Diff.Merge.Region.Conflict { base; ours; theirs } ->
      Format.fprintf ppf "Conflict(base=%S ours=%S theirs=%S)" base ours theirs

let merge_testable =
  testable
    ~pp:(fun ppf m ->
      Format.fprintf ppf "@[<v>%a@]"
        (Format.pp_print_list pp_region)
        (Diff.Merge.regions m))
    ~equal:Diff.Merge.equal ()

let roundtrip m = decode Diff.Merge.jsont (encode Diff.Merge.jsont m)

let merge =
  group "merge"
    [
      test "an unchanged base merges to itself" (fun () ->
          let m =
            some_merge "identity"
              (Diff.Merge.v ~base:"a\nb\nc\n" ~ours:"a\nb\nc\n"
                 ~theirs:"a\nb\nc\n" ())
          in
          is_true ~msg:"clean" (Diff.Merge.is_clean m);
          equal (option string) ~msg:"resolves to base" (Some "a\nb\nc\n")
            (Diff.Merge.resolved m));
      test "a one-sided change is taken verbatim" (fun () ->
          let ours =
            some_merge "ours"
              (Diff.Merge.v ~base:"a\nb\nc\n" ~ours:"a\nB\nc\n"
                 ~theirs:"a\nb\nc\n" ())
          in
          equal (option string) ~msg:"takes ours" (Some "a\nB\nc\n")
            (Diff.Merge.resolved ours);
          let theirs =
            some_merge "theirs"
              (Diff.Merge.v ~base:"a\nb\nc\n" ~ours:"a\nb\nc\n"
                 ~theirs:"a\nb\nC\n" ())
          in
          equal (option string) ~msg:"takes theirs" (Some "a\nb\nC\n")
            (Diff.Merge.resolved theirs));
      test "disjoint changes both apply (the revert property)" (fun () ->
          (* [base] is a selection's net-after; [ours] the current file carrying
             a later change on line 4; [theirs] the net-before reverting line 2.
             The merge keeps line 4 and applies the line-2 reversal. *)
          let m =
            some_merge "disjoint"
              (Diff.Merge.v ~base:"a\nb\nc\nd\n" ~ours:"a\nb\nc\nD\n"
                 ~theirs:"a\nB\nc\nd\n" ())
          in
          is_true ~msg:"clean" (Diff.Merge.is_clean m);
          equal (option string)
            ~msg:"keeps ours' line 4, applies theirs' line 2"
            (Some "a\nB\nc\nD\n") (Diff.Merge.resolved m));
      test "identical changes on both sides resolve without conflict" (fun () ->
          let m =
            some_merge "same"
              (Diff.Merge.v ~base:"a\nb\nc\n" ~ours:"a\nZ\nc\n"
                 ~theirs:"a\nZ\nc\n" ())
          in
          is_true ~msg:"clean" (Diff.Merge.is_clean m);
          equal (option string) ~msg:"resolves to the shared change"
            (Some "a\nZ\nc\n") (Diff.Merge.resolved m));
      test "overlapping changes conflict, carrying all three texts" (fun () ->
          let m =
            some_merge "conflict"
              (Diff.Merge.v ~base:"a\nb\nc\n" ~ours:"a\nX\nc\n"
                 ~theirs:"a\nY\nc\n" ())
          in
          is_false ~msg:"not clean" (Diff.Merge.is_clean m);
          equal (option string) ~msg:"no resolved text" None
            (Diff.Merge.resolved m);
          match Diff.Merge.conflicts m with
          | [ Diff.Merge.Region.Conflict { base; ours; theirs } ] ->
              equal string ~msg:"conflict base" "b\n" base;
              equal string ~msg:"conflict ours" "X\n" ours;
              equal string ~msg:"conflict theirs" "Y\n" theirs
          | _ -> failf "expected exactly one conflict region");
      test "no clean span silently clobbers either edit" (fun () ->
          (* Two edits on distinct lines must both survive; neither is dropped as
             the other side's bytes. *)
          let m =
            some_merge "sound"
              (Diff.Merge.v ~base:"1\n2\n3\n4\n5\n" ~ours:"1\nX\n3\n4\n5\n"
                 ~theirs:"1\n2\n3\nY\n5\n" ())
          in
          is_true ~msg:"clean (disjoint)" (Diff.Merge.is_clean m);
          equal (option string) ~msg:"both edits present"
            (Some "1\nX\n3\nY\n5\n") (Diff.Merge.resolved m));
      test "a missing final newline survives reconstruction" (fun () ->
          let m =
            some_merge "eof"
              (Diff.Merge.v ~base:"a\nb" ~ours:"a\nB" ~theirs:"a\nb" ())
          in
          equal (option string) ~msg:"no trailing newline added" (Some "a\nB")
            (Diff.Merge.resolved m);
          let flip =
            some_merge "flip"
              (Diff.Merge.v ~base:"a\nb" ~ours:"a\nb\n" ~theirs:"a\nb" ())
          in
          equal (option string) ~msg:"a trailing-newline flip is a real change"
            (Some "a\nb\n") (Diff.Merge.resolved flip));
      test "identical inputs give equal merges (determinism)" (fun () ->
          let make () =
            some_merge "det"
              (Diff.Merge.v ~base:"a\nb\nc\n" ~ours:"a\nX\nc\n"
                 ~theirs:"a\nY\nc\n" ())
          in
          equal merge_testable ~msg:"equal" (make ()) (make ()));
      test "max_edit_distance bounds the merge" (fun () ->
          is_true ~msg:"within the bound is Some"
            (Option.is_some
               (Diff.Merge.v ~max_edit_distance:2 ~base:"a\nb\nc\n"
                  ~ours:"a\nX\nc\n" ~theirs:"a\nb\nc\n" ()));
          is_true ~msg:"exceeding the bound is None"
            (Option.is_none
               (Diff.Merge.v ~max_edit_distance:1 ~base:"a\nb\nc\nd\n"
                  ~ours:"W\nX\nY\nZ\n" ~theirs:"a\nb\nc\nd\n" ())));
      test "jsont round-trips clean, conflicting, and non-UTF-8 merges"
        (fun () ->
          let clean =
            some_merge "clean"
              (Diff.Merge.v ~base:"a\nb\nc\nd\n" ~ours:"a\nb\nc\nD\n"
                 ~theirs:"a\nB\nc\nd\n" ())
          in
          equal merge_testable ~msg:"clean round-trips" clean (roundtrip clean);
          let conflict =
            some_merge "conflict"
              (Diff.Merge.v ~base:"a\nb\nc\n" ~ours:"a\nX\nc\n"
                 ~theirs:"a\nY\nc\n" ())
          in
          equal merge_testable ~msg:"conflict round-trips" conflict
            (roundtrip conflict);
          let raw =
            some_merge "raw"
              (Diff.Merge.v ~base:"a\n" ~ours:"\xff\xfe\n" ~theirs:"a\n" ())
          in
          equal merge_testable ~msg:"non-UTF-8 travels as hex" raw
            (roundtrip raw));
      test "render_markers wraps conflicts and copies clean spans" (fun () ->
          let m =
            some_merge "markers"
              (Diff.Merge.v ~base:"a\nb\nc\n" ~ours:"a\nX\nc\n"
                 ~theirs:"a\nY\nc\n" ())
          in
          equal string ~msg:"git-style whole-file markers"
            "a\n<<<<<<< ours\nX\n=======\nY\n>>>>>>> theirs\nc\n"
            (Diff.Merge.render_markers m));
    ]

(* Codec. *)

let codec =
  group "codec"
    [
      test "decodes the documented wire example, deriving counts and numbers"
        (fun () ->
          let json =
            wire_hunk ~old_start:12 ~new_start:12
              [
                wire_line ~kind:"context" ~text:"unchanged line" ~newline:true
                  ();
                wire_line ~kind:"removed" ~text:"old line" ~newline:true ();
                wire_line ~kind:"added" ~hex:"ff00ab" ~newline:false ();
              ]
          in
          let h = decode Diff.Hunk.jsont json in
          equal int ~msg:"old_start" 12 (Diff.Hunk.old_start h);
          equal int ~msg:"new_start" 12 (Diff.Hunk.new_start h);
          equal int ~msg:"derived old_count" 2 (Diff.Hunk.old_count h);
          equal int ~msg:"derived new_count" 2 (Diff.Hunk.new_count h);
          let lines = Diff.Hunk.lines h in
          let context = List.nth lines 0 in
          equal (option int) ~msg:"context old_line" (Some 12)
            (Diff.Hunk.Line.old_line context);
          equal (option int) ~msg:"context new_line" (Some 12)
            (Diff.Hunk.Line.new_line context);
          let removed = List.nth lines 1 in
          equal (option int) ~msg:"removed old_line" (Some 13)
            (Diff.Hunk.Line.old_line removed);
          equal (option int) ~msg:"removed has no new_line" None
            (Diff.Hunk.Line.new_line removed);
          let added = List.nth lines 2 in
          equal (option int) ~msg:"added has no old_line" None
            (Diff.Hunk.Line.old_line added);
          equal (option int) ~msg:"added new_line" (Some 13)
            (Diff.Hunk.Line.new_line added);
          equal string ~msg:"hex member decodes to raw bytes" "\xff\x00\xab"
            (Diff.Hunk.Line.text added);
          is_false ~msg:"added newline flag" (Diff.Hunk.Line.newline added));
      test "selects text for UTF-8 lines and hex for non-UTF-8 lines" (fun () ->
          let utf8 =
            List.hd
              (some_hunks "utf8" (Diff.hunks ~before:"" ~after:"hello\n" ()))
          in
          let raw =
            List.hd
              (some_hunks "raw" (Diff.hunks ~before:"" ~after:"\xff\n" ()))
          in
          (match encoded_line_member_names utf8 with
          | [ names ] ->
              is_true ~msg:"UTF-8 line carries text" (List.mem "text" names);
              is_false ~msg:"UTF-8 line omits hex" (List.mem "hex" names)
          | _ -> failf "expected a single encoded line");
          match encoded_line_member_names raw with
          | [ names ] ->
              is_true ~msg:"non-UTF-8 line carries hex" (List.mem "hex" names);
              is_false ~msg:"non-UTF-8 line omits text" (List.mem "text" names)
          | _ -> failf "expected a single encoded line");
      test "round-trips a non-UTF-8 line byte-exactly" (fun () ->
          let h =
            List.hd
              (some_hunks "hunks"
                 (Diff.hunks ~before:"" ~after:"\xff\x00\xab\n" ()))
          in
          equal hunk ~msg:"round-trip preserves the hunk" h (roundtrip_hunk h);
          equal string ~msg:"content preserved byte for byte" "\xff\x00\xab"
            (Diff.Hunk.Line.text (List.hd (Diff.Hunk.lines h))));
      test "rejects malformed line and hunk members" (fun () ->
          decode_fails ~msg:"both text and hex on a line"
            (wire_hunk ~old_start:1 ~new_start:1
               [ wire_line ~kind:"added" ~text:"x" ~hex:"78" ~newline:true () ]);
          decode_fails ~msg:"neither text nor hex on a line"
            (wire_hunk ~old_start:1 ~new_start:1
               [ wire_line ~kind:"added" ~newline:true () ]);
          decode_fails ~msg:"unknown line member"
            (wire_hunk ~old_start:1 ~new_start:1
               [
                 json_object
                   [
                     ("kind", Json.string "added");
                     ("text", Json.string "x");
                     ("newline", Json.bool true);
                     ("extra", Json.bool true);
                   ];
               ]);
          decode_fails ~msg:"unknown hunk member"
            (json_object
               [
                 ("old_start", Json.int 1);
                 ("new_start", Json.int 1);
                 ("lines", json_array []);
                 ("old_count", Json.int 3);
               ]));
      test "stats cross in one wire form" (fun () ->
          let s = Diff.Stats.v ~files:2 ~additions:5 ~deletions:3 in
          check "wire shape frames the three counts"
            (Json.equal
               (json_object
                  [
                    ("files", Json.int 2);
                    ("additions", Json.int 5);
                    ("deletions", Json.int 3);
                  ])
               (encode Diff.Stats.jsont s));
          equal stats_testable ~msg:"decode inverts encode" s
            (decode Diff.Stats.jsont (encode Diff.Stats.jsont s));
          let zero = Diff.Stats.v ~files:0 ~additions:0 ~deletions:0 in
          equal stats_testable ~msg:"zero counts round-trip" zero
            (decode Diff.Stats.jsont (encode Diff.Stats.jsont zero)));
      test "rejects malformed stats" (fun () ->
          let stats_object ?(files = Some (Json.int 1))
              ?(additions = Some (Json.int 0)) ?(deletions = Some (Json.int 0))
              ?(extra = []) () =
            json_object
              (List.filter_map
                 (fun (name, v) -> Option.map (fun v -> (name, v)) v)
                 [
                   ("files", files);
                   ("additions", additions);
                   ("deletions", deletions);
                 ]
              @ extra)
          in
          let rejects msg json =
            is_true ~msg (Result.is_error (Json.decode Diff.Stats.jsont json))
          in
          rejects "not an object" (Json.int 1);
          rejects "missing files" (stats_object ~files:None ());
          rejects "missing additions" (stats_object ~additions:None ());
          rejects "missing deletions" (stats_object ~deletions:None ());
          rejects "negative files"
            (stats_object ~files:(Some (Json.int (-1))) ());
          rejects "negative additions"
            (stats_object ~additions:(Some (Json.int (-1))) ());
          rejects "negative deletions"
            (stats_object ~deletions:(Some (Json.int (-1))) ());
          rejects "count wrong type"
            (stats_object ~files:(Some (Json.string "many")) ());
          rejects "unknown member"
            (stats_object ~extra:[ ("omitted", Json.int 0) ] ()));
      prop' "round-trips every hunk of a text pair byte-exactly" text_pair
        (fun (before, after) ->
          let hs = some_hunks "expected hunks" (Diff.hunks ~before ~after ()) in
          List.iter
            (fun h -> equal hunk ~msg:"round-trip" h (roundtrip_hunk h))
            hs);
      prop' "round-trips every hunk of a random byte pair byte-exactly"
        byte_pair (fun (before, after) ->
          let hs = some_hunks "expected hunks" (Diff.hunks ~before ~after ()) in
          List.iter
            (fun h -> equal hunk ~msg:"round-trip" h (roundtrip_hunk h))
            hs);
    ]

(* Determinism. *)

let determinism =
  group "determinism"
    [
      prop' "render is byte-identical across runs" (list file_change)
        (fun changes ->
          equal string ~msg:"render text is stable" (render_text changes)
            (render_text changes);
          equal stats_testable ~msg:"render stats are stable"
            (Diff.stats (Diff.render changes))
            (Diff.stats (Diff.render changes)));
      prop' "hunks and their encoding are byte-identical across runs" byte_pair
        (fun (before, after) ->
          let once =
            some_hunks "expected hunks" (Diff.hunks ~before ~after ())
          in
          let twice =
            some_hunks "expected hunks" (Diff.hunks ~before ~after ())
          in
          equal (list hunk) ~msg:"hunks are stable" once twice;
          List.iter
            (fun h ->
              is_true ~msg:"codec encode is deterministic"
                (Json.equal (encode Diff.Hunk.jsont h)
                   (encode Diff.Hunk.jsont h)))
            once);
    ]

let () =
  run "textdiff"
    [
      labels;
      file_changes;
      statistics;
      rendering;
      limits;
      display_safety;
      hunks;
      word_spans;
      merge;
      codec;
      determinism;
    ]
