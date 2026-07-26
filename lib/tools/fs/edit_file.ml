(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let name = "edit_file"
let max_file_bytes = 1024 * 1024
let utf8_bom = "\239\187\191"

let validate_text member text =
  if not (String.is_valid_utf_8 text) then
    invalid_arg (member ^ " must be valid UTF-8");
  if Text_helpers.looks_binary text then
    invalid_arg (member ^ " must be UTF-8 text")

module Input = struct
  type occurrence = Once | All

  type t = {
    path : string;
    old_string : string;
    new_string : string;
    occurrence : occurrence;
    if_identity : Mentat_digest.Content_ref.t option;
  }

  let make ~path ~old_string ~new_string ~occurrence ~if_identity =
    if String.is_empty path then invalid_arg "path must not be empty";
    if String.is_empty old_string then
      invalid_arg "old_string must not be empty";
    if String.equal old_string new_string then
      invalid_arg "old_string and new_string must differ";
    validate_text "old_string" old_string;
    validate_text "new_string" new_string;
    { path; old_string; new_string; occurrence; if_identity }

  let path input = input.path
  let old_string input = input.old_string
  let new_string input = input.new_string
  let occurrence input = input.occurrence
  let if_identity input = input.if_identity
  let occurrence_string = function Once -> "once" | All -> "all"

  let occurrence_of_json = function
    | None | Some "once" -> Once
    | Some "all" -> All
    | Some value ->
        invalid_arg ("occurrence must be \"once\" or \"all\": " ^ value)

  let identity = function
    | None -> None
    | Some "" ->
        invalid_arg
          "if_identity must be the identity from a complete read_file result; \
           omit it when no identity is known"
    | Some token -> (
        match Mentat_digest.Content_ref.of_token token with
        | Ok identity -> Some identity
        | Error error ->
            invalid_arg
              ("if_identity is not a file identity: "
              ^ Mentat_digest.Error.message error
              ^ "; copy the identity string from a complete read_file result \
                 verbatim, or omit it"))

  let object_codec =
    Jsont.Object.map ~kind:"edit_file input"
      (fun path old_string new_string occurrence if_identity ->
        Mentat_tool.Codec.decode_invalid_arg (fun () ->
            make ~path ~old_string ~new_string
              ~occurrence:(occurrence_of_json occurrence)
              ~if_identity:(identity if_identity)))
    |> Jsont.Object.mem "path" Jsont.string ~enc:path
    |> Jsont.Object.mem "old_string" Jsont.string ~enc:old_string
    |> Jsont.Object.mem "new_string" Jsont.string ~enc:new_string
    |> Jsont.Object.opt_mem "occurrence" Jsont.string ~enc:(fun input ->
        match occurrence input with Once -> None | All -> Some "all")
    |> Jsont.Object.opt_mem "if_identity" Jsont.string ~enc:(fun input ->
        Option.map Mentat_digest.Content_ref.to_token (if_identity input))
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict edit_file input" object_codec

  let string_property description fields =
    Codec.obj
      (("type", Jsont.Json.string "string")
      :: ("description", Jsont.Json.string description)
      :: fields)

  let schema =
    Codec.obj
      [
        ("type", Jsont.Json.string "object");
        ( "properties",
          Codec.obj
            [
              ( "path",
                string_property
                  "Workspace-relative or workspace-contained absolute existing \
                   file path to edit."
                  [ ("minLength", Jsont.Json.int 1) ] );
              ( "old_string",
                string_property
                  "Exact non-empty UTF-8 text to replace. By default it must \
                   occur exactly once."
                  [ ("minLength", Jsont.Json.int 1) ] );
              ( "new_string",
                string_property
                  "Replacement UTF-8 text. It must differ from old_string and \
                   may be empty."
                  [] );
              ( "occurrence",
                Codec.obj
                  [
                    ("type", Jsont.Json.string "string");
                    ( "description",
                      Jsont.Json.string
                        "Replacement multiplicity. Defaults to once; all \
                         replaces every non-overlapping occurrence." );
                    ( "enum",
                      Jsont.Json.list
                        [ Jsont.Json.string "once"; Jsont.Json.string "all" ] );
                  ] );
              ( "if_identity",
                string_property
                  "Complete-file identity from a previous complete read_file \
                   observation."
                  [ ("minLength", Jsont.Json.int 1) ] );
            ] );
        ( "required",
          Jsont.Json.list
            [
              Jsont.Json.string "path";
              Jsont.Json.string "old_string";
              Jsont.Json.string "new_string";
            ] );
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

module Output = struct
  module Update = Mentat_tools_output.Update

  type operation = Modify | Unchanged

  type t = {
    path : string;
    operation : operation;
    added : int option;
    removed : int option;
  }

  let operation_string = function
    | Modify -> "modify"
    | Unchanged -> "unchanged"

  let count_text = function
    | Some count -> string_of_int count
    | None -> "unknown"

  let text output =
    Printf.sprintf "%s: %s added=%s removed=%s\n"
      (operation_string output.operation)
      output.path (count_text output.added)
      (count_text output.removed)

  let encode output =
    let files = match output.operation with Modify -> 1 | Unchanged -> 0 in
    let update =
      Update.make ~disposition:Update.Applied ~files ~additions:output.added
        ~deletions:output.removed ~skipped:0
    in
    Mentat_tools_output.Codec.encode Update.jsont ~text:(text output) update

  let modified ~path change =
    {
      path;
      operation = Modify;
      added = Mentat_permission.Request.Change.additions change;
      removed = Mentat_permission.Request.Change.removals change;
    }

  let unchanged ~path =
    { path; operation = Unchanged; added = Some 0; removed = Some 0 }
end

let has_utf8_bom text = String.starts_with ~prefix:utf8_bom text

let drop_utf8_bom text =
  String.sub text (String.length utf8_bom)
    (String.length text - String.length utf8_bom)

let strip_utf8_bom text = if has_utf8_bom text then drop_utf8_bom text else text

type line_ending = Lf | Crlf

let target_line_ending text =
  let rec loop saw_crlf saw_lf index =
    if index = String.length text then
      if saw_crlf && not saw_lf then Crlf else Lf
    else if
      Char.equal text.[index] '\r'
      && index + 1 < String.length text
      && Char.equal text.[index + 1] '\n'
    then loop true saw_lf (index + 2)
    else if Char.equal text.[index] '\n' then loop saw_crlf true (index + 1)
    else loop saw_crlf saw_lf (index + 1)
  in
  loop false false 0

let normalize_line_endings style text =
  let buffer = Buffer.create (String.length text) in
  let add_newline () =
    match style with
    | Lf -> Buffer.add_char buffer '\n'
    | Crlf -> Buffer.add_string buffer "\r\n"
  in
  let rec loop index =
    if index = String.length text then Buffer.contents buffer
    else if Char.equal text.[index] '\r' then begin
      add_newline ();
      if index + 1 < String.length text && Char.equal text.[index + 1] '\n' then
        loop (index + 2)
      else loop (index + 1)
    end
    else if Char.equal text.[index] '\n' then begin
      add_newline ();
      loop (index + 1)
    end
    else begin
      Buffer.add_char buffer text.[index];
      loop (index + 1)
    end
  in
  loop 0

let normalized_size style text =
  let newline_size = match style with Lf -> 1L | Crlf -> 2L in
  let rec loop size index =
    if index = String.length text then size
    else if Char.equal text.[index] '\r' then
      let next =
        if index + 1 < String.length text && Char.equal text.[index + 1] '\n'
        then index + 2
        else index + 1
      in
      loop (Int64.add size newline_size) next
    else if Char.equal text.[index] '\n' then
      loop (Int64.add size newline_size) (index + 1)
    else loop (Int64.succ size) (index + 1)
  in
  loop 0L 0

let saturating_mul left right =
  if Int64.equal left 0L || Int64.equal right 0L then 0L
  else if Int64.compare left (Int64.div Int64.max_int right) > 0 then
    Int64.max_int
  else Int64.mul left right

let saturating_add left right =
  if Int64.compare left (Int64.sub Int64.max_int right) > 0 then Int64.max_int
  else Int64.add left right

let matching_contents contents =
  if has_utf8_bom contents then (`Bom, drop_utf8_bom contents)
  else (`Raw, contents)

let restore_bom mode contents =
  match mode with
  | `Bom -> if has_utf8_bom contents then contents else utf8_bom ^ contents
  | `Raw -> contents

let count_non_overlapping ~pattern text =
  String.find_all ~sub:pattern (fun _ count -> count + 1) text 0

let replace_non_overlapping ~all ~old_string ~new_string text =
  if all then String.replace_all ~sub:old_string ~by:new_string text
  else String.replace_first ~sub:old_string ~by:new_string text

let interrupted () = Mentat_tool.Result.cancelled ()

let stale path =
  Mentat_tool.Result.failed `Stale
    (Mentat_workspace.Path.display path ^ ": stale file identity")

let replacement_failure path occurrence matches =
  let failure, message =
    if matches = 0 then
      ( `Not_found,
        Mentat_workspace.Path.display path ^ ": old_string was not found" )
    else
      ( `Stale,
        Printf.sprintf
          "%s: old_string matched %d times; provide more context or set \
           occurrence=all"
          (Mentat_workspace.Path.display path)
          matches )
  in
  let metadata =
    Codec.obj
      [
        ("path", Jsont.Json.string (Mentat_workspace.Path.display path));
        ("matches", Jsont.Json.int matches);
        ("occurrence", Jsont.Json.string (Input.occurrence_string occurrence));
      ]
  in
  Mentat_tool.Result.failed ~metadata failure message

let empty_old_string path =
  Mentat_tool.Result.failed `Invalid_input
    (Mentat_workspace.Path.display path
    ^ ": old_string must contain text beyond the UTF-8 BOM")

let fail_too_large path size =
  Edit_error.failed
    (Mentat_edit.Error.too_large ~path ~size
       ~max_size:(Int64.of_int max_file_bytes))

let planned_change path input =
  Change_evidence.modify ~path ~before:(Input.old_string input)
    ~after:(Input.new_string input)

let apply workspace_io plan output =
  Edit_error.applied workspace_io plan ~output

let edit workspace_io ~cancelled path input current =
  match Input.if_identity input with
  | Some expected when not (Mentat_digest.Content_ref.matches expected current)
    ->
      stale path
  | Some _ | None -> (
      let mode, match_contents = matching_contents current in
      let line_ending = target_line_ending match_contents in
      let old_argument, new_argument =
        match mode with
        | `Raw -> (Input.old_string input, Input.new_string input)
        | `Bom ->
            ( strip_utf8_bom (Input.old_string input),
              strip_utf8_bom (Input.new_string input) )
      in
      let occurrence = Input.occurrence input in
      if String.is_empty old_argument then empty_old_string path
      else if
        Int64.compare
          (normalized_size line_ending old_argument)
          (Int64.of_int (String.length match_contents))
        > 0
      then replacement_failure path occurrence 0
      else
        let old_string = normalize_line_endings line_ending old_argument in
        let matches =
          count_non_overlapping ~pattern:old_string match_contents
        in
        if matches = 0 || (occurrence = Input.Once && matches <> 1) then
          replacement_failure path occurrence matches
        else
          let replacements = if occurrence = Input.All then matches else 1 in
          let retained_size =
            String.length match_contents
            - (replacements * String.length old_string)
          in
          let new_size = normalized_size line_ending new_argument in
          let max_size = Int64.of_int max_file_bytes in
          let match_size =
            saturating_add
              (Int64.of_int retained_size)
              (saturating_mul (Int64.of_int replacements) new_size)
          in
          if Int64.compare match_size max_size > 0 then
            fail_too_large path match_size
          else
            let new_string = normalize_line_endings line_ending new_argument in
            let after_contents =
              replace_non_overlapping ~all:(occurrence = Input.All) ~old_string
                ~new_string match_contents
              |> restore_bom mode
            in
            let final_size = String.length after_contents in
            if final_size > max_file_bytes then
              fail_too_large path (Int64.of_int final_size)
            else if Text_helpers.looks_binary after_contents then
              Edit_error.failed
                (Mentat_edit.Error.invalid_text ~path "binary file")
            else if String.equal current after_contents then
              Mentat_tool.Result.completed
                ~output:
                  (Output.unchanged ~path:(Address.display workspace_io path))
                ()
            else
              match
                Mentat_edit.rewrite ~path ~before:current ~after:after_contents
              with
              | Error error -> Edit_error.failed error
              | Ok plan ->
                  if cancelled () then interrupted ()
                  else
                    apply workspace_io plan
                      (Output.modified
                         ~path:(Address.display workspace_io path)
                         (planned_change path input)))

let current_text workspace_io path =
  match Mentat_workspace_io.Edit.observe workspace_io path with
  | Ok (Mentat_edit.Observed.Text contents) -> Ok contents
  | Ok Mentat_edit.Observed.Missing ->
      Error
        (Edit_error.failed
           (Mentat_edit.Error.state_mismatch ~path ~expected:`Text
              ~actual:`Missing))
  | Ok Mentat_edit.Observed.Other ->
      Error
        (Edit_error.failed
           (Mentat_edit.Error.state_mismatch ~path ~expected:`Text
              ~actual:`Other))
  | Error error -> Error (Edit_error.failed error)

let run workspace_io input ~cancelled =
  if cancelled () then interrupted ()
  else
    Permissions.with_resolved_run workspace_io (Input.path input) (fun path ->
        match current_text workspace_io path with
        | Error result -> result
        | Ok current -> edit workspace_io ~cancelled path input current)

let permissions workspace_io input =
  Permissions.with_resolved workspace_io (Input.path input) (fun path ->
      let item =
        Mentat_permission.Request.Item.make
          ~change:(planned_change path input)
          (Mentat_permission.Access.path ~op:`Modify path)
      in
      [ Mentat_permission.Request.make ~source:name [ item ] ])

let make workspace_io =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.edit_file
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io)
    ~run:(fun ~cancelled input -> run workspace_io input ~cancelled)
    ()
