(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let name = "write_file"
let max_file_bytes = 1024 * 1024

let validate_text contents =
  if not (String.is_valid_utf_8 contents) then
    invalid_arg "contents must be valid UTF-8";
  if Text_helpers.looks_binary contents then
    invalid_arg "contents must be UTF-8 text"

module Input = struct
  type precondition = Missing | Identity of Mentat_digest.Content_ref.t
  type t = { path : string; contents : string; precondition : precondition }

  let make ~path ~contents ~precondition =
    if String.is_empty path then invalid_arg "path must not be empty";
    validate_text contents;
    { path; contents; precondition }

  let path (input : t) = input.path
  let contents (input : t) = input.contents
  let precondition (input : t) = input.precondition

  let identity = function
    | None -> Missing
    | Some token -> (
        match Mentat_digest.Content_ref.of_token token with
        | Ok identity -> Identity identity
        | Error error ->
            invalid_arg
              ("if_identity is not a file identity: "
              ^ Mentat_digest.Error.message error))

  let object_codec =
    Jsont.Object.map ~kind:"write_file input" (fun path contents if_identity ->
        Mentat_tool.Codec.decode_invalid_arg (fun () ->
            make ~path ~contents ~precondition:(identity if_identity)))
    |> Jsont.Object.mem "path" Jsont.string ~enc:path
    |> Jsont.Object.mem "contents" Jsont.string ~enc:contents
    |> Jsont.Object.opt_mem "if_identity" Jsont.string ~enc:(fun input ->
        match precondition input with
        | Missing -> None
        | Identity identity ->
            Some (Mentat_digest.Content_ref.to_token identity))
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict write_file input" object_codec

  let string_property description =
    Codec.obj
      [
        ("type", Jsont.Json.string "string");
        ("description", Jsont.Json.string description);
      ]

  let non_empty_string_property description =
    Codec.obj
      [
        ("type", Jsont.Json.string "string");
        ("description", Jsont.Json.string description);
        ("minLength", Jsont.Json.int 1);
      ]

  let schema =
    Codec.obj
      [
        ("type", Jsont.Json.string "object");
        ( "properties",
          Codec.obj
            [
              ( "path",
                non_empty_string_property
                  "Workspace-relative or workspace-contained absolute file \
                   path to write." );
              ( "contents",
                string_property "Complete UTF-8 file contents to write." );
              ( "if_identity",
                string_property
                  "Complete-file identity from a previous complete read. \
                   Supply it to overwrite an existing file; if the target is \
                   missing, the file is created." );
            ] );
        ( "required",
          Jsont.Json.list
            [ Jsont.Json.string "path"; Jsont.Json.string "contents" ] );
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

module Output = struct
  type operation = Create | Modify | Unchanged

  type t = {
    path : Mentat_workspace.Path.t;
    operation : operation;
    added : int;
    removed : int option;
  }

  let make ~path ~operation ~added ~removed =
    { path; operation; added; removed }

  let operation_string = function
    | Create -> "create"
    | Modify -> "modify"
    | Unchanged -> "unchanged"

  let text output =
    let removed =
      match output.removed with
      | Some removed -> string_of_int removed
      | None -> "unknown"
    in
    Printf.sprintf "%s: %s added=%d removed=%s\n"
      (operation_string output.operation)
      (Mentat_workspace.Path.display output.path)
      output.added removed

  let encode output =
    let semantic =
      match output.operation with
      | Unchanged -> Mentat_tools_output.Write.unchanged
      | Create | Modify -> Mentat_tools_output.Write.wrote ~lines:output.added
    in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Write.jsont
      ~text:(text output) semantic
end

let utf8_bom = "\239\187\191"
let has_utf8_bom contents = String.starts_with ~prefix:utf8_bom contents

let preserve_bom ~current contents =
  if has_utf8_bom current && not (has_utf8_bom contents) then
    utf8_bom ^ contents
  else contents

let interrupted () = Mentat_tool.Result.cancelled ()

let fail_too_large path size =
  Edit_error.failed
    (Mentat_edit.Error.too_large ~path ~size
       ~max_size:(Int64.of_int max_file_bytes))

let apply workspace_io plan output =
  Edit_error.applied workspace_io plan ~output

let create workspace_io path contents =
  match Mentat_edit.create ~path ~contents with
  | Error error -> Edit_error.failed error
  | Ok plan ->
      let added = Text_helpers.logical_line_count contents in
      let output =
        Output.make ~path ~operation:Output.Create ~added ~removed:(Some 0)
      in
      apply workspace_io plan output

let stale path =
  Mentat_tool.Result.failed `Stale
    (Mentat_workspace.Path.display path ^ ": stale file identity")

let current_text workspace_io path =
  match Mentat_workspace_io.Edit.observe workspace_io path with
  | Ok Mentat_edit.Observed.Missing -> `Missing
  | Ok (Mentat_edit.Observed.Text contents) -> `Text contents
  | Ok Mentat_edit.Observed.Other ->
      `Error
        (Edit_error.failed
           (Mentat_edit.Error.state_mismatch ~path ~expected:`Text
              ~actual:`Other))
  | Error error -> `Error (Edit_error.failed error)

let replace workspace_io path expected contents =
  match current_text workspace_io path with
  | `Missing -> create workspace_io path contents
  | `Error result -> result
  | `Text current -> (
      if not (Mentat_digest.Content_ref.matches expected current) then
        stale path
      else
        let final_contents = preserve_bom ~current contents in
        let final_size = String.length final_contents in
        if final_size > max_file_bytes then
          fail_too_large path (Int64.of_int final_size)
        else if String.equal current final_contents then
          let output =
            Output.make ~path ~operation:Output.Unchanged ~added:0
              ~removed:(Some 0)
          in
          Mentat_tool.Result.completed ~output ()
        else
          match
            Mentat_edit.rewrite ~path ~before:current ~after:final_contents
          with
          | Error error -> Edit_error.failed error
          | Ok plan ->
              let output =
                Output.make ~path ~operation:Output.Modify
                  ~added:(Text_helpers.logical_line_count contents)
                  ~removed:None
              in
              apply workspace_io plan output)

let run workspace_io input ~cancelled =
  if cancelled () then interrupted ()
  else
    Permissions.with_resolved_run workspace_io (Input.path input) (fun path ->
        if String.length (Input.contents input) > max_file_bytes then
          fail_too_large path
            (Int64.of_int (String.length (Input.contents input)))
        else
          match Input.precondition input with
          | Input.Missing -> create workspace_io path (Input.contents input)
          | Input.Identity expected ->
              replace workspace_io path expected (Input.contents input))

let permissions workspace_io input =
  Permissions.with_resolved workspace_io (Input.path input) (fun path ->
      let item op change =
        Mentat_permission.Request.Item.make ~change
          (Mentat_permission.Access.path ~op path)
      in
      let items =
        match Input.precondition input with
        | Input.Missing ->
            [
              item `Create
                (Change_evidence.creation ~path (Input.contents input));
            ]
        | Input.Identity _ ->
            [
              item `Create
                (Change_evidence.creation ~path (Input.contents input));
              item `Modify
                (Change_evidence.replacement ~path (Input.contents input));
            ]
      in
      [ Mentat_permission.Request.make ~source:name items ])

let make workspace_io =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.write_file
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io)
    ~run:(fun ~cancelled input -> run workspace_io input ~cancelled)
    ()
