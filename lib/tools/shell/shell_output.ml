(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Session = Mentat_workspace_io.Command.Session

let name = "shell_output"
let max_read_bytes = 64 * 1024
let json_string = Jsont.Json.string
let json_int = Jsont.Json.int
let json_bool = Jsont.Json.bool

module Input = struct
  type t = { handle : string; filter : string option }

  let validate_string member value =
    if String.is_empty value then invalid_arg (member ^ " must not be empty");
    if String.contains value '\000' then
      invalid_arg (member ^ " must not contain NUL")

  let make handle filter =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    validate_string "handle" handle;
    Option.iter (validate_string "filter") filter;
    { handle; filter }

  let object_codec =
    Jsont.Object.map ~kind:"shell_output input" make
    |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun input -> input.handle)
    |> Jsont.Object.opt_mem "filter" Jsont.string ~enc:(fun input ->
        input.filter)
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
            ] );
        ("required", Jsont.Json.list [ json_string "handle" ]);
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

(* An optional Perl-compatible filter, compiled once per call; a malformed
   pattern is a domain [Invalid_input], never an exception. *)
let compile_filter = function
  | None -> Ok None
  | Some pattern -> (
      match Re.Pcre.re_result pattern with
      | Ok re -> Ok (Some (Re.compile re))
      | Error _ -> Error pattern)

let text ~handle ~status ~dropped ~new_bytes ~stdout ~stderr =
  if new_bytes = 0 && dropped = 0 then
    Printf.sprintf "Handle: %s\nStatus: %s\n(no new output)" handle
      (Bg_render.status_line status)
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
        match Registry.read registry ~handle:input.Input.handle with
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
            let text =
              text ~handle:input.Input.handle ~status ~dropped ~new_bytes
                ~stdout ~stderr
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
