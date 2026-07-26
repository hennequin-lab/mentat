(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Bytes = struct
  let deterministic size = String.init size (fun i -> Char.chr (i land 0xff))
  let small = deterministic 1_024
  let medium = deterministic (64 * 1_024)
  let large = deterministic (1_024 * 1_024)
end

module Diff_fixture = struct
  let label name = Textdiff.Label.of_string name

  let lines ~count ~changed =
    let buffer = Buffer.create (count * 12) in
    for i = 1 to count do
      if i = changed then Buffer.add_string buffer "changed line\n"
      else begin
        Buffer.add_string buffer "line ";
        Buffer.add_string buffer (string_of_int i);
        Buffer.add_char buffer '\n'
      end
    done;
    Buffer.contents buffer

  let small_change =
    Textdiff.File_change.modify ~label:(label "small.txt") ~before:"a\nb\nc\n"
      ~after:"a\nB\nc\n"

  let medium_change =
    Textdiff.File_change.modify ~label:(label "medium.txt")
      ~before:(lines ~count:200 ~changed:80)
      ~after:(lines ~count:200 ~changed:120)

  let multi_changes =
    let rec loop acc i =
      if i = 0 then acc
      else
        let label = label ("file-" ^ string_of_int i ^ ".txt") in
        let before = lines ~count:80 ~changed:(10 + (i mod 20)) in
        let after = lines ~count:80 ~changed:(40 + (i mod 20)) in
        loop (Textdiff.File_change.modify ~label ~before ~after :: acc) (i - 1)
    in
    loop [] 20
end

module Session_fixture = struct
  module Session = Mentat_session
  module Llm = Mentat_llm

  let event_sizes = [ ("100", 100); ("1k", 1_000); ("10k", 10_000) ]

  let build ?(id = "bench") ~events () =
    let session =
      Session.create
        ~id:(Session.Id.of_string id)
        ~cwd:(Lpath.Abs.of_string_exn "/workspace")
        ~created_at:(Session.Time.of_unix_ms 0L)
        ()
    in
    (* A user-message log: [message_appended] admits user messages directly,
       while assistant content must be recorded as a provider settlement inside a
       turn. The event count — what decode and replay scale on — is what this
       fixture pins; the per-event shape is one representative fact. *)
    let log =
      List.init events (fun i ->
          Session.Event.message_appended
            (Llm.Message.user_text (Printf.sprintf "user message %d" i)))
    in
    match Session.append_all log session with
    | Ok session -> session
    | Error error ->
        failwith
          (Printf.sprintf "session_fixture: %s" (Session.Error.message error))

  let encoded session =
    match Jsont_bytesrw.encode_string Mentat_session.jsont session with
    | Ok text -> text
    | Error message -> failwith ("session_fixture encode: " ^ message)
end

module Ledger = struct
  module M = Mentat_mutation
  module Edit = Mentat_edit
  module W = Mentat_workspace
  module Session = Mentat_session
  module Content_ref = Mentat_digest.Content_ref

  let ws_root = W.Root.of_dir (Lpath.Abs.of_string_exn "/workspace")
  let root_key = W.Root.key ws_root
  let workspace = W.single ws_root
  let wpath text = W.Path.make ~root_key (Lpath.Rel.of_string_exn text)

  (* Pure in-memory edit IO: [fs] is the visible file set the apply reads. *)
  let pure_io fs =
    {
      Edit.Apply.with_write_lock = (fun _paths f -> f ());
      revalidate = (fun path -> Ok path);
      read =
        (fun path ->
          match List.find_opt (fun (p, _) -> W.Path.equal p path) fs with
          | Some (_, contents) -> Ok (Edit.Observed.Text contents)
          | None -> Ok Edit.Observed.Missing);
      commit = (fun ~path:_ ~before:_ ~after:_ -> Ok ());
    }

  let failf fmt = Format.kasprintf failwith fmt

  let apply_exn ~fs plan =
    match Edit.apply ~io:(pure_io fs) ~workspace plan with
    | Ok result -> result
    | Error error -> failf "ledger apply: %a" Edit.Apply_error.pp error

  let plan_exn = function
    | Ok plan -> plan
    | Error error -> failf "ledger plan: %a" Edit.Error.pp error

  let create_event ~turn ~claim ~ordinal ~path ~contents =
    let plan = plan_exn (Edit.create ~path ~contents) in
    M.Event.of_edit
      ~turn:(Session.Turn.Id.of_string turn)
      ~claim:(Session.Tool_claim.Id.of_string claim)
      ~ordinal ~checkpoint:None
      (apply_exn ~fs:[] plan)

  let build ~claims ~edits_per_claim =
    let per_claim c =
      let turn = Printf.sprintf "t-%d" c in
      let claim = Printf.sprintf "c-%d" c in
      List.init edits_per_claim (fun j ->
          create_event ~turn ~claim ~ordinal:j
            ~path:(wpath (Printf.sprintf "c%d/f%d.ml" c j))
            ~contents:(Printf.sprintf "content %d %d\n" c j))
    in
    List.concat (List.init claims per_claim)

  let settle_axis =
    [
      (* O(k^2) probe: one claim, rising edits. *)
      ("edits/100", (1, 100));
      ("edits/500", (1, 500));
      ("edits/1000", (1, 1_000));
      (* Linear reference: rising claims, one edit each. *)
      ("claims/100", (100, 1));
      ("claims/1000", (1_000, 1));
    ]

  type revert_case = {
    state : M.State.t;
    selection : M.Revert.Selection.t;
    evidence : M.Revert.Evidence.t;
    resolve : Content_ref.t -> string option;
  }

  let superseded_axis = [ ("0%", 0); ("50%", 50); ("100%", 100) ]

  (* A fixed 200-path ledger; [superseded_pct] of the paths take a second,
     head-advancing rewrite so preparation must three-way merge them. *)
  let revert_case ~superseded_pct =
    let count = 200 in
    let is_superseded i = i * 100 < count * superseded_pct in
    let first_events =
      List.init count (fun i ->
          create_event ~turn:"t-a" ~claim:"c-a" ~ordinal:i
            ~path:(wpath (Printf.sprintf "p%d.ml" i))
            ~contents:(Printf.sprintf "v1 %d\n" i))
    in
    let second_events =
      (* One [c-b] claim rewrites each superseded path in ledger order; its
         ordinal is dense over the superseded subset. *)
      let ordinal = ref 0 in
      List.filter_map
        (fun i ->
          if not (is_superseded i) then None
          else begin
            let path = wpath (Printf.sprintf "p%d.ml" i) in
            let plan =
              plan_exn
                (Edit.rewrite ~path
                   ~before:(Printf.sprintf "v1 %d\n" i)
                   ~after:(Printf.sprintf "v2 %d\n" i))
            in
            let result =
              apply_exn ~fs:[ (path, Printf.sprintf "v1 %d\n" i) ] plan
            in
            let event =
              M.Event.of_edit
                ~turn:(Session.Turn.Id.of_string "t-b")
                ~claim:(Session.Tool_claim.Id.of_string "c-b")
                ~ordinal:!ordinal ~checkpoint:None result
            in
            incr ordinal;
            Some event
          end)
        (List.init count Fun.id)
    in
    let events = first_events @ second_events in
    let state =
      match M.State.of_events events with
      | Ok state -> state
      | Error error -> failf "revert_case replay: %a" M.State.Error.pp error
    in
    (* Current read = the recorded head for every path; blobs resolve every
       recorded image. *)
    let head_contents i =
      if is_superseded i then Printf.sprintf "v2 %d\n" i
      else Printf.sprintf "v1 %d\n" i
    in
    let current =
      List.init count (fun i ->
          ( wpath (Printf.sprintf "p%d.ml" i),
            Edit.Observed.Text (head_contents i) ))
    in
    let blob_contents =
      List.init count (fun i -> Printf.sprintf "v1 %d\n" i)
      @ List.init count (fun i -> Printf.sprintf "v2 %d\n" i)
    in
    let blobs =
      List.map (fun c -> (Content_ref.of_contents c, c)) blob_contents
    in
    let evidence = M.Revert.Evidence.make ~current ~blobs in
    let resolve ref =
      List.find_map
        (fun (r, c) -> if Content_ref.equal r ref then Some c else None)
        blobs
    in
    { state; selection = M.Revert.Selection.all; evidence; resolve }
end

module Workspace = struct
  let rec mkdir_p dir =
    if not (Sys.file_exists dir) then begin
      mkdir_p (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end

  let write_file base rel contents =
    let path = Filename.concat base rel in
    mkdir_p (Filename.dirname path);
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc contents)

  let materialize ~root =
    write_file root "AGENTS.md"
      "# Project\n\nBe helpful. Prefer small, reviewable changes.\n";
    write_file root "lib/core.ml" "let hello () = print_string \"hi\"\n";
    write_file root "lib/util.ml" "let id x = x\n";
    write_file root "test/test_core.ml" "let () = ()\n";
    write_file root ".mentat/skills/tidy/SKILL.md"
      "---\nname: tidy\ndescription: Tidy code.\n---\n\nTidy the code.\n"
end

module Enter = struct
  module Llm = Mentat_llm

  (* Odd count so the log ends on a user message (even index) — a request-ready
     transcript for [Request.make]. *)
  let session = Session_fixture.build ~events:201 ()

  let model =
    Llm.Model.make
      ~provider:(Llm.Provider.make "openai")
      ~api:(Llm.Model.Api.make "responses")
      ~id:"gpt-5"
end

module Transcript_fixture = struct
  module T = Mentat_tui.Transcript
  module Tool_block = Mentat_tui.Tool_block

  let block_sizes = [ ("100", 100); ("1k", 1_000); ("5k", 5_000) ]
  let palette = Mentat_tui.Theme.Palette.default

  let tool_block i =
    Tool_block.make Tool_block.Read
      ~argument:(Printf.sprintf "lib/file%d.ml" i)
      ~dot:Tool_block.Succeeded
      ~summary:(Printf.sprintf "read %d lines" (i mod 400))
      ()

  let build ~blocks =
    let rec loop t i =
      if i >= blocks then t
      else
        let block =
          match i mod 4 with
          | 0 -> T.user (Printf.sprintf "Please look at file %d." i)
          | 1 -> T.assistant (Printf.sprintf "Here is what file %d contains." i)
          | 2 ->
              T.reasoning ~duration_s:(i mod 9)
                ~body:(Printf.sprintf "Considering approach %d." i)
                ()
          | _ -> T.tool (tool_block i)
        in
        loop (T.append t block) (i + 1)
    in
    loop T.empty 0
end
