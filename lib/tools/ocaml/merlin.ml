(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let max_detail_bytes = 4 * 1024
let timeout_seconds = 30.
let max_output_bytes = 1024 * 1024

type error =
  | Cancelled
  | Unavailable of string
  | Timed_out
  | Signaled of int
  | Exited of { code : int; detail : string }
  | Output_exceeded of {
      stream : Mentat_workspace_io.Command.stream;
      limit : int;
    }
  | Supervision_failed of string
  | Incomplete_output of string
  | Query_failure of { class_ : string; detail : string }
  | Malformed of string

let stream_name = function
  | Mentat_workspace_io.Command.Stdout -> "stdout"
  | Mentat_workspace_io.Command.Stderr -> "stderr"

let error_message = function
  | Cancelled -> "ocamlmerlin query cancelled"
  | Unavailable detail ->
      "could not start ocamlmerlin: "
      ^ Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes detail
  | Timed_out -> "ocamlmerlin timed out after 30000ms"
  | Signaled signal ->
      "ocamlmerlin was terminated by signal " ^ string_of_int signal
  | Exited { code; detail } ->
      let detail =
        Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes detail
      in
      if String.is_empty detail then
        "ocamlmerlin exited with status " ^ string_of_int code
      else detail
  | Output_exceeded { stream; limit } ->
      Printf.sprintf "ocamlmerlin %s exceeded %d-byte output limit"
        (stream_name stream) limit
  | Supervision_failed detail ->
      "ocamlmerlin supervision failed: "
      ^ Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes detail
  | Incomplete_output stream ->
      "ocamlmerlin " ^ stream ^ " was incomplete after the child exited"
  | Query_failure { class_; detail } ->
      "ocamlmerlin returned " ^ class_ ^ ": "
      ^ Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes detail
  | Malformed detail ->
      "could not decode ocamlmerlin response: "
      ^ Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes detail

let json_to_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok text -> text
  | Error diagnostic -> "<unencodable value: " ^ diagnostic ^ ">"

let named_values name fields =
  List.filter_map
    (fun ((member_name, _), value) ->
      if String.equal member_name name then Some value else None)
    fields

let required_member name fields =
  match named_values name fields with
  | [ value ] -> Ok value
  | [] -> Error ("response has no " ^ name)
  | _ :: _ :: _ -> Error ("response has duplicate " ^ name ^ " members")

let value_detail value =
  match value with
  | Jsont.String (detail, _) ->
      Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes detail
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.Array _
  | Jsont.Object _ ->
      Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes
        (json_to_string value)

let parse_envelope stdout =
  match Jsont_bytesrw.decode_string Jsont.json stdout with
  | Error diagnostic ->
      Error
        (Malformed
           (Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes
              diagnostic))
  | Ok (Jsont.Object (fields, _)) -> (
      match
        (required_member "class" fields, required_member "value" fields)
      with
      | Error diagnostic, _ | _, Error diagnostic ->
          Error (Malformed diagnostic)
      | Ok (Jsont.String (class_, _)), Ok value -> (
          match class_ with
          | "return" -> Ok value
          | "failure" | "error" | "exception" ->
              Error (Query_failure { class_; detail = value_detail value })
          | _ ->
              Error
                (Malformed
                   ("unexpected response class "
                   ^ Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes
                       class_)))
      | Ok _, Ok _ -> Error (Malformed "response class is not a string"))
  | Ok
      ( Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
      | Jsont.Array _ ) ->
      Error (Malformed "response is not an object")

let command_error error =
  Mentat_workspace_io.Command.Error.message error
  |> Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes

let captured_text captured =
  Mentat_workspace_io.Command.Captured.render captured

let nonzero_detail outcome =
  let stderr =
    captured_text outcome.Mentat_workspace_io.Command.stderr
    |> Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes
  in
  if String.is_empty stderr then
    captured_text outcome.Mentat_workspace_io.Command.stdout
    |> Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes
  else stderr

let run workspace_io ~clock ~program ~cwd ~command ~args ~source ~cancelled =
  if List.is_empty program then
    invalid_arg "Ocaml.Merlin.run: program prefix must not be empty";
  let argv = program @ ("single" :: command :: args) in
  let stdin = Eio.Flow.string_source source in
  let timeout = Eio.Time.Timeout.seconds clock timeout_seconds in
  match
    Mentat_workspace_io.Command.run workspace_io ~cwd ~stdin
      ~capture:(Mentat_workspace_io.Command.Limit max_output_bytes) ~timeout
      ~cancelled argv
  with
  | Error error -> Error (Unavailable (command_error error))
  | Ok outcome -> (
      match outcome.Mentat_workspace_io.Command.termination with
      | Mentat_workspace_io.Command.Stopped -> Error Cancelled
      | Mentat_workspace_io.Command.Timed_out -> Error Timed_out
      | Mentat_workspace_io.Command.Output_limit { stream; limit } ->
          Error (Output_exceeded { stream; limit })
      | Mentat_workspace_io.Command.Supervision_failed error ->
          Error
            (Supervision_failed
               (Format.asprintf "%a" Eio.Exn.pp_err error
               |> Text_helpers.bounded_diagnostic ~max_bytes:max_detail_bytes))
      | Mentat_workspace_io.Command.Exited (`Signaled signal) ->
          Error (Signaled signal)
      | Mentat_workspace_io.Command.Exited (`Exited code) ->
          if code <> 0 then
            Error (Exited { code; detail = nonzero_detail outcome })
          else
            let stdout = outcome.Mentat_workspace_io.Command.stdout in
            let stderr = outcome.Mentat_workspace_io.Command.stderr in
            if not (Mentat_workspace_io.Command.Captured.is_complete stdout)
            then Error (Incomplete_output "stdout")
            else if
              not (Mentat_workspace_io.Command.Captured.is_complete stderr)
            then Error (Incomplete_output "stderr")
            else parse_envelope (captured_text stdout))
