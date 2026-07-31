(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap

let event_t =
  let pp ppf ({ Sse.Event.id; name; data } : Sse.Event.t) =
    Format.fprintf ppf "{id=%a; name=%S; data=%S}"
      (Format.pp_print_option Format.pp_print_string)
      id name data
  in
  let equal ({ Sse.Event.id; name; data } : Sse.Event.t) (b : Sse.Event.t) =
    id = b.Sse.Event.id
    && String.equal name b.Sse.Event.name
    && String.equal data b.Sse.Event.data
  in
  Testable.make ~pp ~equal

(* A line source over a string, yielding each '\n'-separated segment — the shape
   a byte reader hands to [Sse.Reader.next]. *)
let lines_of text =
  let remaining = ref (String.split_on_char '\n' text) in
  fun () ->
    match !remaining with
    | [] -> None
    | line :: rest ->
        remaining := rest;
        Some line

let writer_frames () =
  equal string ~msg:"a bare data frame" "data: hello\n\n"
    (Sse.Writer.frame ~data:"hello" ());
  equal string ~msg:"id and event lines precede the data, in order"
    "id: 7\nevent: delta\ndata: a\ndata: b\n\n"
    (Sse.Writer.frame ~id:"7" ~event:"delta" ~data:"a\nb" ());
  equal string ~msg:"a multi-line data value splits across data: lines"
    "data: one\ndata: two\ndata: three\n\n"
    (Sse.Writer.frame ~data:"one\ntwo\nthree" ());
  equal string ~msg:"the heartbeat is a bare comment frame" ": hb\n\n"
    Sse.Writer.heartbeat

let reader_parses () =
  let read =
    lines_of "event: delta\ndata: first\ndata: second\n\n: hb\n\ndata: last"
  in
  equal (option event_t) ~msg:"the first event joins its data lines"
    (Some { Sse.Event.id = None; name = "delta"; data = "first\nsecond" })
    (Sse.Reader.next read);
  equal (option event_t)
    ~msg:"a comment line is skipped and EOF flushes the last event"
    (Some { Sse.Event.id = None; name = ""; data = "last" })
    (Sse.Reader.next read);
  equal (option event_t) ~msg:"end of stream is None and stable" None
    (Sse.Reader.next read)

let reader_parses_id () =
  equal (option event_t) ~msg:"the id field is captured"
    (Some { Sse.Event.id = Some "42"; name = ""; data = "x" })
    (Sse.Reader.next (lines_of "id: 42\ndata: x\n\n"));
  (* A field line with no colon, and an unknown field, are both ignored. *)
  equal (option event_t) ~msg:"unknown and colon-less field lines are ignored"
    (Some { Sse.Event.id = None; name = ""; data = "y" })
    (Sse.Reader.next (lines_of "retry\nfoo: bar\ndata: y\n\n"))

(* Writing an event and reading it back yields the same event: the reader and
   writer are inverse over the fields the framing carries. *)
let round_trips () =
  let check (({ Sse.Event.id; name; data } : Sse.Event.t) as event) =
    let frame =
      Sse.Writer.frame ?id
        ?event:(if String.equal name "" then None else Some name)
        ~data ()
    in
    equal (option event_t) ~msg:"a framed event reads back unchanged"
      (Some event)
      (Sse.Reader.next (lines_of frame))
  in
  check { Sse.Event.id = None; name = ""; data = "plain" };
  check { Sse.Event.id = Some "3"; name = "delta"; data = "a\nb\nc" };
  check { Sse.Event.id = Some "99"; name = "done"; data = "" }

let eio_incremental () =
  Eio_main.run @@ fun _env ->
  let source =
    (Eio.Flow.string_source "event: x\ndata: 1\n\ndata: 2\n\n"
      :> Eio.Flow.source_ty Eio.Std.r)
  in
  let reader = Sse_eio.Reader.of_source source in
  equal (option event_t) ~msg:"the first event decodes from the byte stream"
    (Some { Sse.Event.id = None; name = "x"; data = "1" })
    (Sse_eio.Reader.next reader);
  equal (option event_t) ~msg:"the second event decodes"
    (Some { Sse.Event.id = None; name = ""; data = "2" })
    (Sse_eio.Reader.next reader);
  equal (option event_t) ~msg:"the stream ends" None
    (Sse_eio.Reader.next reader)

let () =
  run "sse"
    [
      test "the writer frames events" writer_frames;
      test "the reader parses events" reader_parses;
      test "the reader captures id and ignores stray fields" reader_parses_id;
      test "the reader and writer round-trip" round_trips;
      test "the eio reader decodes incrementally" eio_incremental;
    ]
