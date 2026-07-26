(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Session = Mentat_workspace_io.Command.Session

let name = "shell_output"
let max_read_bytes = 64 * 1024
let default_wait_ms = 5_000
let min_wait_ms = 5_000
let max_wait_ms = 300_000
let json_string = Jsont.Json.string
let json_int = Jsont.Json.int
let json_bool = Jsont.Json.bool

module Input = struct
  type t = { handle : string; filter : string option; wait_ms : int option }

  let validate_string member value =
    if String.is_empty value then invalid_arg (member ^ " must not be empty");
    if String.contains value '\000' then
      invalid_arg (member ^ " must not contain NUL")

  (* The bound is declared in the schema and enforced here, so an out-of-range
     wait is refused rather than quietly moved: a model that asked for a wait it
     did not get would reason about a deadline the read never had. *)
  let validate_wait = function
    | None -> ()
    | Some wait_ms ->
        if wait_ms < min_wait_ms || wait_ms > max_wait_ms then
          invalid_arg
            (Printf.sprintf "wait_ms must be between %d and %d, got %d"
               min_wait_ms max_wait_ms wait_ms)

  let make handle filter wait_ms =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    validate_string "handle" handle;
    Option.iter (validate_string "filter") filter;
    validate_wait wait_ms;
    { handle; filter; wait_ms }

  let max_input_integer =
    Float.min 9_007_199_254_740_991. (float_of_int Int.max_int)

  let exact_integer =
    let decode = function
      | Jsont.Number (value, _)
        when Float.is_integer value && Float.abs value <= max_input_integer ->
          int_of_float value
      | Jsont.Number _ | Jsont.Null _ | Jsont.Bool _ | Jsont.String _
      | Jsont.Array _ | Jsont.Object _ ->
          Codec.decode_error "expected an integer in JSON's safe integer range"
    in
    Jsont.map ~kind:"integer" ~dec:decode
      ~enc:(fun value -> json_int value)
      Jsont.json

  let object_codec =
    Jsont.Object.map ~kind:"shell_output input" make
    |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun input -> input.handle)
    |> Jsont.Object.opt_mem "filter" Jsont.string ~enc:(fun input ->
        input.filter)
    |> Jsont.Object.opt_mem "wait_ms" exact_integer ~enc:(fun input ->
        input.wait_ms)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict shell_output input" object_codec

  let property kind description fields =
    Codec.obj
      (("type", json_string kind)
      :: ("description", json_string description)
      :: fields)

  let schema =
    Codec.obj
      [
        ("type", json_string "object");
        ( "properties",
          Codec.obj
            [
              ( "handle",
                property "string"
                  "A handle returned by a background shell, e.g. bg_1."
                  [ ("minLength", json_int 1) ] );
              ( "filter",
                property "string"
                  "Optional regular expression; only output lines matching it \
                   are returned."
                  [ ("minLength", json_int 1) ] );
              ( "wait_ms",
                property "integer"
                  (Printf.sprintf
                     "How long to wait for new output, in milliseconds, from \
                      %d to %d. Defaults to %d. The read returns as soon as \
                      output arrives or the command exits."
                     min_wait_ms max_wait_ms default_wait_ms)
                  [
                    ("minimum", json_int min_wait_ms);
                    ("maximum", json_int max_wait_ms);
                  ] );
            ] );
        ("required", Jsont.Json.list [ json_string "handle" ]);
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema

  let effective_wait_ms input =
    Option.value input.wait_ms ~default:default_wait_ms
end

(* An optional Perl-compatible filter, compiled once per call; a malformed
   pattern is a domain [Invalid_input], never an exception. *)
let compile_filter = function
  | None -> Ok None
  | Some pattern -> (
      match Re.Pcre.re_result pattern with
      | Ok re -> Ok (Some (Re.compile re))
      | Error _ -> Error pattern)

(* A read that returns nothing while the command is still running waited its
   whole budget for it: the read only ends early on output, on the process
   settling, or on a stop. Saying so is what keeps a repeat read from looking
   free. A settled process has nothing more to say at any budget, and its
   status line already carries that. *)
let empty_note ~status ~wait_ms =
  match status with
  | Session.Running -> Printf.sprintf "(no new output in %dms)" wait_ms
  | Session.Exited _ | Session.Terminated -> "(no new output)"

let text ~handle ~status ~dropped ~new_bytes ~wait_ms ~stdout ~stderr =
  if new_bytes = 0 && dropped = 0 then
    Printf.sprintf "Handle: %s\nStatus: %s\n%s" handle
      (Bg_render.status_line status)
      (empty_note ~status ~wait_ms)
  else
    Printf.sprintf "Handle: %s\nStatus: %s\n%sstdout:\n%s\nstderr:\n%s" handle
      (Bg_render.status_line status)
      (Bg_render.dropped_note dropped)
      stdout stderr

let encode ~handle ~status ~new_bytes ~dropped ~text =
  let json =
    Codec.obj
      [
        ("handle", json_string handle);
        ("status", json_string (Bg_render.status_keyword status));
        ("new_bytes", json_int new_bytes);
        ("dropped", json_int dropped);
      ]
  in
  Mentat_tool.Output.make ~text ~json ~truncated:(dropped > 0) ()

let run registry ~cancelled input =
  if cancelled () then Mentat_tool.Result.cancelled ()
  else
    match compile_filter input.Input.filter with
    | Error pattern ->
        Mentat_tool.Result.failed `Invalid_input
          ("invalid filter regular expression: " ^ pattern)
    | Ok filter -> (
        let wait_ms = Input.effective_wait_ms input in
        match
          Registry.read registry ~handle:input.Input.handle ~cancelled
            ~seconds:(float_of_int wait_ms /. 1_000.)
        with
        | None ->
            Mentat_tool.Result.failed `Not_found
              (Bg_render.not_found_message input.Input.handle)
        | Some chunk ->
            let stdout, stdout_capped =
              Bg_render.render_stream ?filter ~max_bytes:max_read_bytes
                chunk.Session.stdout
            in
            let stderr, stderr_capped =
              Bg_render.render_stream ?filter ~max_bytes:max_read_bytes
                chunk.Session.stderr
            in
            let new_bytes =
              String.length chunk.Session.stdout
              + String.length chunk.Session.stderr
            in
            let dropped =
              chunk.Session.dropped + stdout_capped + stderr_capped
            in
            let status = chunk.Session.status in
            (* A stop ends the wait. The read has already consumed the cursor,
               so bytes it took are reported rather than thrown away — those
               bytes are gone from the next read either way. It is the empty
               stopped read that must not be dressed up as an answer: the
               command was not silent for its budget, the wait was cut
               short. *)
            if new_bytes = 0 && dropped = 0 && cancelled () then
              Mentat_tool.Result.cancelled ()
            else
              let text =
                text ~handle:input.Input.handle ~status ~dropped ~new_bytes
                  ~wait_ms ~stdout ~stderr
              in
              Mentat_tool.Result.completed
                ~output:
                  (encode ~handle:input.Input.handle ~status ~new_bytes ~dropped
                     ~text)
                ())

let make registry =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.shell_output
    ~input:Input.contract ~output:Fun.id
    ~run:(fun ~cancelled input -> run registry ~cancelled input)
    ()
