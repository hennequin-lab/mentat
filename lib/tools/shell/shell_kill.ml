(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Session = Mentat_workspace_io.Command.Session

let name = "shell_kill"
let max_tail_bytes = 64 * 1024
let json_string = Jsont.Json.string
let json_int = Jsont.Json.int
let json_bool = Jsont.Json.bool

module Input = struct
  type t = { handle : string }

  let validate_string member value =
    if String.is_empty value then invalid_arg (member ^ " must not be empty");
    if String.contains value '\000' then
      invalid_arg (member ^ " must not contain NUL")

  let make handle =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    validate_string "handle" handle;
    { handle }

  let object_codec =
    Jsont.Object.map ~kind:"shell_kill input" make
    |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun input -> input.handle)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict shell_kill input" object_codec

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
            ] );
        ("required", Jsont.Json.list [ json_string "handle" ]);
        ("additionalProperties", json_bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

(* What the kill reached, narrowed to what actually happened. [Terminated] is
   the only status the registry produces by signalling; every other status is a
   process that had already settled, which the kill left alone — so the two
   cases carry different residues and neither may borrow the other's sentence.
   Stated in the result, consistent with the start receipt, never papered
   over. *)
let reach = function
  | Session.Terminated ->
      "The signal reached this process and the group it leads, so the workers \
       it forked stopped with it; one that left that group, or that ignored \
       the graceful signal, may still be running."
  | Session.Running | Session.Exited _ ->
      "This command had already settled, so nothing was signalled; workers it \
       left behind are still running."

let text ~handle ~status ~dropped ~stdout ~stderr =
  Printf.sprintf
    "Killed background process %s.\n\
     Final status: %s\n\
     %s\n\
     %sstdout:\n\
     %s\n\
     stderr:\n\
     %s"
    handle
    (Bg_render.status_line status)
    (reach status)
    (Bg_render.dropped_note dropped)
    stdout stderr

let run registry ~cancelled input =
  if cancelled () then Mentat_tool.Result.cancelled ()
  else
    match Registry.kill registry ~handle:input.Input.handle with
    | None ->
        Mentat_tool.Result.failed `Not_found
          (Bg_render.not_found_message input.Input.handle)
    | Some chunk ->
        let stdout, stdout_capped =
          Bg_render.render_stream ~max_bytes:max_tail_bytes chunk.Session.stdout
        in
        let stderr, stderr_capped =
          Bg_render.render_stream ~max_bytes:max_tail_bytes chunk.Session.stderr
        in
        let dropped = chunk.Session.dropped + stdout_capped + stderr_capped in
        let status = chunk.Session.status in
        let text =
          text ~handle:input.Input.handle ~status ~dropped ~stdout ~stderr
        in
        let json =
          Codec.obj
            [
              ("handle", json_string input.Input.handle);
              ("status", json_string (Bg_render.status_keyword status));
            ]
        in
        Mentat_tool.Result.completed
          ~output:
            (Mentat_tool.Output.make ~text ~json ~truncated:(dropped > 0) ())
          ()

let make registry =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.shell_kill
    ~input:Input.contract ~output:Fun.id
    ~run:(fun ~cancelled input -> run registry ~cancelled input)
    ()
