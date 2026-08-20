(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Session = Mentat_session

let provider = Mentat_llm.Provider.make "openai"
let api = Mentat_llm.Model.Api.make "responses"
let model = Mentat_llm.Model.make ~provider ~api ~id:"gpt-5.6-sol"

let contract =
  Session.Contract.make ~mode:Session.Contract.Mode.Build ~model
    ~options:
      (Mentat_llm.Request.Options.make
         ~reasoning_effort:Mentat_llm.Request.Options.Reasoning_effort.Medium ())
    ~declarations:[] ~policy:Mentat_permission.Policy.default
    ~review:Mentat_permission.Review_behavior.Enforce
    ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
    ()

let time seconds =
  seconds |> Int64.of_int |> Int64.mul 1_000L |> Session.Time.of_unix_ms

(* Persist owner-encoded next sessions as launch fixtures. The executable still
   performs the real store scan, selection, client attach, replay, and render.
   The recorded cwd is the canonical root: the executable canonicalizes its
   working directory before it records a session or scopes a listing to the
   workspace, so a document spelled any other way is one the executable would
   never have written and [--last] would rightly not find. *)
let seed_session project ~id ~prompt ~updated_at =
  let session_id = Session.Id.of_string id in
  let metadata =
    Session.Metadata.make
      ~cwd:(Lpath.Abs.of_string_exn (Project.canonical_root project))
      ~created_at:(time 1) ~updated_at:(time updated_at) ()
  in
  let turn =
    Session.Turn.make
      ~id:(Session.Turn.Id.of_string ("turn-" ^ id))
      ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text prompt)
      ~max_steps:100 ~contract ()
  in
  let events =
    [
      Session.Event.turn_started turn;
      Session.Event.turn_finished ~turn:(Session.Turn.id turn)
        Session.Turn.Outcome.completed;
    ]
  in
  let session =
    match Session.make ~id:session_id ~metadata ~events with
    | Ok session -> session
    | Error error -> failwith (Session.Error.message error)
  in
  let encoded =
    match Jsont_bytesrw.encode_string Session.jsont session with
    | Ok encoded -> encoded
    | Error message -> failwith ("session does not encode: " ^ message)
  in
  Project.write_path
    (Project.data project
       (Filename.concat "sessions" (Filename.concat id "session.json")))
    (encoded ^ "\n")

let trust_store project = Project.scratch project "config/mentat/trust.json"
let require condition message = if not condition then failwith message

let rec waitpid_blocking pid =
  match Unix.waitpid [] pid with
  | _, status -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_blocking pid

let rec await_process pid limit =
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | 0, _ when Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) *. 1e-9 < limit ->
      Unix.sleepf 0.01;
      await_process pid limit
  | 0, _ -> false
  | _, _ -> true
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> await_process pid limit
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> true

let stop_process pid =
  let now () = Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) *. 1e-9 in
  if not (await_process pid (now ())) then begin
    (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
    if not (await_process pid (now () +. 0.5)) then begin
      (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (waitpid_blocking pid : Unix.process_status)
    end
  end

let create_provider_process ~executable ~arguments ~log =
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close null)
    (fun () ->
      let log_fd =
        Unix.openfile log [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY ] 0o600
      in
      Fun.protect
        ~finally:(fun () -> Unix.close log_fd)
        (fun () ->
          Unix.create_process_env executable arguments (Unix.environment ())
            null log_fd log_fd))

let serve_script_file project f =
  let script = Project.scratch project "provider/script.jsonl" in
  let capture = Project.scratch project "provider/capture" in
  let port_file = Project.scratch project "provider/port" in
  let log = Project.scratch project "provider/log" in
  let executable = Project.resolve_env_path "MENTAT_FAKE_PROVIDER_BIN" in
  let arguments =
    [|
      executable;
      "--script";
      script;
      "--capture";
      capture;
      "--port-file";
      port_file;
      "--accept-timeout";
      "30";
    |]
  in
  let pid = create_provider_process ~executable ~arguments ~log in
  Fun.protect
    ~finally:(fun () -> stop_process pid)
    (fun () ->
      Project.wait_for_file port_file;
      let port = Project.read_path port_file |> String.trim in
      f ("http://127.0.0.1:" ^ port ^ "/v1"))

let with_provider project ~prompt ~answer f =
  Project.write_path
    (Project.scratch project "provider/script.jsonl")
    (Printf.sprintf
       {|{"expect":{"body_contains":[%S]},"response":{"id":"cli-prompt","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":%S}]}]}}
|}
       prompt answer);
  serve_script_file project f

(* Two ordered responses: the first prompt's answer, then a manual compaction's
   summary held for [summary_delay_ms] so the composer-issued compaction stays
   in flight long enough for a test to observe its live indicator. *)
let with_compaction_provider project ~prompt ~answer ~summary ~summary_delay_ms
    f =
  Project.write_path
    (Project.scratch project "provider/script.jsonl")
    (Printf.sprintf
       {|{"expect":{"body_contains":[%S]},"response":{"id":"cli-prompt","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":%S}]}]}}
{"delay_ms":%d,"response":{"id":"cli-summary","status":"completed","model":"gpt-5.6-sol","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":%S}]}]}}
|}
       prompt answer summary_delay_ms summary);
  serve_script_file project f

let substring_at text ~index substring =
  let length = String.length substring in
  index + length <= String.length text
  && String.equal (String.sub text index length) substring

let rec find_substring text substring index =
  if index + String.length substring > String.length text then None
  else if substring_at text ~index substring then Some index
  else find_substring text substring (index + 1)

(* Every OSC window-title payload the child emitted, in order. Matrix frames the
   title as [ESC ] 0 ; <title> ESC \], and the VTE decodes it into terminal
   state a screen golden never shows, so the raw stream is the only observation
   point. *)
let osc_titles raw =
  let intro = "\027]0;" and terminator = "\027\\" in
  let rec loop index acc =
    match find_substring raw intro index with
    | None -> List.rev acc
    | Some start -> (
        let payload = start + String.length intro in
        match find_substring raw terminator payload with
        | None -> List.rev acc
        | Some stop ->
            let title = String.sub raw payload (stop - payload) in
            loop (stop + String.length terminator) (title :: acc))
  in
  loop 0 []

(* A real process reports wall-clock elapsed milliseconds. Normalize only that
   measured field so the golden remains stable while all 24 rows of the actual
   80x24 screen, including the visible completion sentence, are still printed. *)
let censor_shell_elapsed_ms screen =
  let prefix = "Completed in " in
  let suffix = " ms" in
  match find_substring screen prefix 0 with
  | None -> screen
  | Some prefix_start ->
      let digits_start = prefix_start + String.length prefix in
      let rec scan_digits index =
        if index < String.length screen then
          match screen.[index] with
          | '0' .. '9' -> scan_digits (index + 1)
          | _ -> index
        else index
      in
      let digits_end = scan_digits digits_start in
      if
        digits_end = digits_start
        || not (substring_at screen ~index:digits_end suffix)
      then screen
      else
        String.sub screen 0 digits_start
        ^ "$TIME"
        ^ String.sub screen digits_end (String.length screen - digits_end)

let require_trust project status =
  let path = trust_store project in
  require (Sys.file_exists path) "the trust decision was not persisted";
  require
    (Screen.contains (Project.read_path path)
       (Printf.sprintf "\":\"%s\"" status))
    ("the persisted trust decision is not " ^ status)
