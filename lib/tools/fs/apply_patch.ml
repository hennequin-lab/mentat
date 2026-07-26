(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let name = "apply_patch"
let max_file_bytes = 1024 * 1024

module Input = struct
  type t = { patch : string; operations : Patch.Operation.t list }

  let make patch =
    if String.is_empty patch then invalid_arg "patch must not be empty";
    match Patch.parse patch with
    | Ok operations -> { patch; operations }
    | Error error -> invalid_arg (Patch.Error.message error)

  let operations input = input.operations

  let object_codec =
    Jsont.Object.map ~kind:"apply_patch input" (fun patch ->
        Mentat_tool.Codec.decode_invalid_arg (fun () -> make patch))
    |> Jsont.Object.mem "patch" Jsont.string ~enc:(fun input -> input.patch)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict apply_patch input" object_codec

  let schema =
    Codec.obj
      [
        ("type", Jsont.Json.string "object");
        ( "properties",
          Codec.obj
            [
              ( "patch",
                Codec.obj
                  [
                    ("type", Jsont.Json.string "string");
                    ( "description",
                      Jsont.Json.string
                        "Complete Codex-style patch text. Paths are relative \
                         to the root containing the workspace current \
                         directory." );
                    ("minLength", Jsont.Json.int 1);
                  ] );
            ] );
        ("required", Jsont.Json.list [ Jsont.Json.string "patch" ]);
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

type summary = { added_lines : int option; removed_lines : int option }

let zero_summary = { added_lines = Some 0; removed_lines = Some 0 }

let add_known left right =
  match (left, right) with
  | Some left, Some right -> Some (left + right)
  | None, _ | _, None -> None

let combine_summary left right =
  {
    added_lines = add_known left.added_lines right.added_lines;
    removed_lines = add_known left.removed_lines right.removed_lines;
  }

let summary_of_change change =
  {
    added_lines = Mentat_permission.Request.Change.additions change;
    removed_lines = Mentat_permission.Request.Change.removals change;
  }

module Output = struct
  module Update = Mentat_tools_output.Update

  type operation =
    | Create
    | Modify
    | Delete
    | Move of { from : Mentat_workspace.Path.t }

  type entry = {
    path : Mentat_workspace.Path.t;
    operation : operation;
    added : int option;
    removed : int option;
  }

  type t = entry list

  let make_entry ~path ~operation (summary : summary) =
    {
      path;
      operation;
      added = summary.added_lines;
      removed = summary.removed_lines;
    }

  let operation_string = function
    | Create -> "create"
    | Modify -> "modify"
    | Delete -> "delete"
    | Move _ -> "move"

  let summary_line entry =
    match entry.operation with
    | Create | Modify | Delete ->
        operation_string entry.operation
        ^ " "
        ^ Mentat_workspace.Path.display entry.path
    | Move { from } ->
        "move "
        ^ Mentat_workspace.Path.display from
        ^ " -> "
        ^ Mentat_workspace.Path.display entry.path

  let text entries =
    let lines =
      "Success. Updated the following files:" :: List.map summary_line entries
    in
    String.concat "\n" lines ^ "\n"

  let aggregate field entries =
    List.fold_left
      (fun total entry -> add_known total (field entry))
      (Some 0) entries

  let encode entries =
    let update =
      Update.make ~disposition:Update.Applied ~files:(List.length entries)
        ~additions:(aggregate (fun entry -> entry.added) entries)
        ~deletions:(aggregate (fun entry -> entry.removed) entries)
        ~skipped:0
    in
    Mentat_tools_output.Codec.encode Update.jsont ~text:(text entries) update
end

type plan_error = Edit of Mentat_edit.Error.t | Patch of string | Cancelled
type planned = { plan : Mentat_edit.t; entries : Output.t }

let interrupted () = Mentat_tool.Result.cancelled ()

let failed_plan = function
  | Edit error -> Edit_error.failed error
  | Patch message -> Mentat_tool.Result.failed `Invalid_input message
  | Cancelled -> interrupted ()

let cwd_root_key workspace_io =
  Ok (Mentat_workspace.Path.root_key (Mentat_workspace_io.cwd workspace_io))

let rooted_path root_key relative =
  Mentat_workspace.Path.make ~root_key relative

let is_strict_ancestor ~parent path =
  (not (Mentat_workspace.Path.equal parent path))
  && Mentat_workspace.Path.is_within ~root:parent path

let operation_paths root_key operation =
  let source = rooted_path root_key (Patch.Operation.path operation) in
  let output = rooted_path root_key (Patch.Operation.output_path operation) in
  if Mentat_workspace.Path.equal source output then [ source ]
  else [ source; output ]

let nested_path_conflict ~cancelled operations root_key =
  let paths = List.concat_map (operation_paths root_key) operations in
  let rec check_path path = function
    | _ when cancelled () -> Error Cancelled
    | [] -> Ok None
    | candidate :: candidates ->
        if is_strict_ancestor ~parent:path candidate then
          Ok (Some (path, candidate))
        else if is_strict_ancestor ~parent:candidate path then
          Ok (Some (candidate, path))
        else check_path path candidates
  in
  let rec loop = function
    | _ when cancelled () -> Error Cancelled
    | [] -> Ok None
    | path :: paths -> (
        match check_path path paths with
        | Error _ as error -> error
        | Ok (Some _ as conflict) -> Ok conflict
        | Ok None -> loop paths)
  in
  loop paths

let nested_path_conflict_message (parent, child) =
  Printf.sprintf "%s: patch path conflicts with nested path %s"
    (Mentat_workspace.Path.display parent)
    (Mentat_workspace.Path.display child)

let check_new_contents path contents =
  if not (String.is_valid_utf_8 contents) then
    Error (Edit (Mentat_edit.Error.invalid_text ~path "invalid UTF-8"))
  else if String.length contents > max_file_bytes then
    Error
      (Edit
         (Mentat_edit.Error.too_large ~path
            ~size:(Int64.of_int (String.length contents))
            ~max_size:(Int64.of_int max_file_bytes)))
  else if Text_helpers.looks_binary contents then
    Error (Edit (Mentat_edit.Error.invalid_text ~path "binary file"))
  else Ok ()

let patch_apply_error path error =
  Patch
    (Printf.sprintf "%s: %s"
       (Mentat_workspace.Path.display path)
       (Patch.Update.Error.message error))

type operation_kind = Add | Delete | Update | Move
type operation_ref = { index : int; kind : operation_kind }

type virtual_text = {
  contents : string;
  origin : Mentat_workspace.Path.t option;
  summary : summary;
}

type virtual_state = Missing | Text of virtual_text

type virtual_file = {
  original : Mentat_edit.State.t;
  current : virtual_state;
  last_operation : operation_ref option;
}

type virtual_files = {
  files : virtual_file Mentat_workspace.Path.Map.t;
  reverse_order : Mentat_workspace.Path.t list;
}

let empty_virtual_files =
  { files = Mentat_workspace.Path.Map.empty; reverse_order = [] }

let operation_ref index = function
  | Patch.Operation.Add _ -> { index; kind = Add }
  | Patch.Operation.Delete _ -> { index; kind = Delete }
  | Patch.Operation.Update { move_to = None; _ } -> { index; kind = Update }
  | Patch.Operation.Update { move_to = Some _; _ } -> { index; kind = Move }

let operation_kind_name = function
  | Add -> "Add"
  | Delete -> "Delete"
  | Update -> "Update"
  | Move -> "Move"

let add_initial path file state =
  {
    files = Mentat_workspace.Path.Map.add path file state.files;
    reverse_order = path :: state.reverse_order;
  }

let replace_file path file state =
  { state with files = Mentat_workspace.Path.Map.add path file state.files }

let virtual_kind = function Missing -> "missing" | Text _ -> "text"

let sequential_conflict path operation file expected =
  let previous =
    match file.last_operation with
    | Some previous ->
        Printf.sprintf "operation %d (%s)" previous.index
          (operation_kind_name previous.kind)
    | None -> "the original filesystem state"
  in
  Error
    (Patch
       (Printf.sprintf
          "%s: operation %d (%s) conflicts with %s: expected %s, found %s"
          (Mentat_workspace.Path.display path)
          operation.index
          (operation_kind_name operation.kind)
          previous expected
          (virtual_kind file.current)))

let state_mismatch path expected actual =
  Error (Edit (Mentat_edit.Error.state_mismatch ~path ~expected ~actual))

let require_missing workspace_io path operation state =
  match Mentat_workspace.Path.Map.find_opt path state.files with
  | Some ({ current = Missing; _ } as file) -> Ok (state, file)
  | Some file -> sequential_conflict path operation file "missing"
  | None -> (
      match Mentat_workspace_io.Edit.observe workspace_io path with
      | Error error -> Error (Edit error)
      | Ok Mentat_edit.Observed.Missing ->
          let file =
            {
              original = Mentat_edit.State.Missing;
              current = Missing;
              last_operation = None;
            }
          in
          Ok (add_initial path file state, file)
      | Ok (Mentat_edit.Observed.Text _) -> state_mismatch path `Missing `Text
      | Ok Mentat_edit.Observed.Other -> state_mismatch path `Missing `Other)

let require_text workspace_io path operation state =
  match Mentat_workspace.Path.Map.find_opt path state.files with
  | Some ({ current = Text text; _ } as file) -> Ok (state, file, text)
  | Some file -> sequential_conflict path operation file "text"
  | None -> (
      match Mentat_workspace_io.Edit.observe workspace_io path with
      | Error error -> Error (Edit error)
      | Ok (Mentat_edit.Observed.Text contents) ->
          let text = { contents; origin = Some path; summary = zero_summary } in
          let file =
            {
              original = Mentat_edit.State.Text contents;
              current = Text text;
              last_operation = None;
            }
          in
          Ok (add_initial path file state, file, text)
      | Ok Mentat_edit.Observed.Missing -> state_mismatch path `Text `Missing
      | Ok Mentat_edit.Observed.Other -> state_mismatch path `Text `Other)

let set_current path file current operation state =
  replace_file path { file with current; last_operation = Some operation } state

let utf8_bom = "\239\187\191"
let has_utf8_bom text = String.starts_with ~prefix:utf8_bom text

let drop_utf8_bom text =
  String.sub text (String.length utf8_bom)
    (String.length text - String.length utf8_bom)

type line_ending = Lf | Crlf

let target_line_ending text =
  let rec loop saw_crlf saw_lf index =
    if index >= String.length text then
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

let normalize_newlines_to_lf text =
  let buffer = Buffer.create (String.length text) in
  let rec loop index =
    if index = String.length text then Buffer.contents buffer
    else if Char.equal text.[index] '\r' then begin
      Buffer.add_char buffer '\n';
      if index + 1 < String.length text && Char.equal text.[index + 1] '\n' then
        loop (index + 2)
      else loop (index + 1)
    end
    else begin
      Buffer.add_char buffer text.[index];
      loop (index + 1)
    end
  in
  loop 0

let restore_line_endings style text =
  match style with
  | Lf -> text
  | Crlf ->
      let buffer = Buffer.create (String.length text) in
      String.iter
        (fun char ->
          if Char.equal char '\n' then Buffer.add_string buffer "\r\n"
          else Buffer.add_char buffer char)
        text;
      Buffer.contents buffer

let patch_contents contents =
  let bom, text =
    if has_utf8_bom contents then (`Preserve, drop_utf8_bom contents)
    else (`Absent, contents)
  in
  (bom, target_line_ending text, normalize_newlines_to_lf text)

let restore_patch_contents bom line_ending text =
  let text = restore_line_endings line_ending text in
  match bom with `Preserve -> utf8_bom ^ text | `Absent -> text

let apply_update path update before =
  let bom, line_ending, patch_before = patch_contents before in
  match Patch.Update.apply update patch_before with
  | Error error -> Error (patch_apply_error path error)
  | Ok after -> Ok (restore_patch_contents bom line_ending after)

let evidence_of_operation ~path = function
  | Patch.Operation.Add { contents; _ } ->
      Some (Change_evidence.creation ~path contents)
  | Patch.Operation.Update { update; _ } ->
      let chunks = Patch.Update.chunks update in
      let side select =
        String.concat "\n"
          (List.concat_map
             (fun (chunk : Patch.Update.chunk) -> select chunk)
             chunks)
      in
      Some
        (Change_evidence.modify ~path
           ~before:(side (fun chunk -> chunk.Patch.Update.old_lines))
           ~after:(side (fun chunk -> chunk.Patch.Update.new_lines)))
  | Patch.Operation.Delete _ -> None

let update_summary path operation =
  match evidence_of_operation ~path operation with
  | Some change -> summary_of_change change
  | None -> zero_summary

let same_move_path_error path operation =
  Error
    (Patch
       (Printf.sprintf "%s: operation %d (Move) has the same source and output"
          (Mentat_workspace.Path.display path)
          operation.index))

let plan_operation workspace_io root_key state index operation =
  let operation_ref = operation_ref index operation in
  match operation with
  | Patch.Operation.Add { path; contents } ->
      let path = rooted_path root_key path in
      begin match check_new_contents path contents with
      | Error _ as error -> error
      | Ok () -> (
          match require_missing workspace_io path operation_ref state with
          | Error _ as error -> error
          | Ok (state, file) ->
              let summary =
                Change_evidence.creation ~path contents |> summary_of_change
              in
              let current = Text { contents; origin = None; summary } in
              Ok (set_current path file current operation_ref state))
      end
  | Patch.Operation.Delete { path } ->
      let path = rooted_path root_key path in
      begin match require_text workspace_io path operation_ref state with
      | Error _ as error -> error
      | Ok (state, file, _) ->
          Ok (set_current path file Missing operation_ref state)
      end
  | Patch.Operation.Update { path; move_to; update } ->
      let path = rooted_path root_key path in
      begin match require_text workspace_io path operation_ref state with
      | Error _ as error -> error
      | Ok (state, file, before) -> (
          match apply_update path update before.contents with
          | Error _ as error -> error
          | Ok contents ->
              let output_path =
                match move_to with
                | None -> path
                | Some output -> rooted_path root_key output
              in
              begin match check_new_contents output_path contents with
              | Error _ as error -> error
              | Ok () ->
                  let summary =
                    combine_summary before.summary
                      (update_summary output_path operation)
                  in
                  let current = Text { before with contents; summary } in
                  begin match move_to with
                  | None ->
                      Ok (set_current path file current operation_ref state)
                  | Some _ when Mentat_workspace.Path.equal path output_path ->
                      same_move_path_error path operation_ref
                  | Some _ -> (
                      match
                        require_missing workspace_io output_path operation_ref
                          state
                      with
                      | Error _ as error -> error
                      | Ok (state, output_file) ->
                          let state =
                            set_current path file Missing operation_ref state
                          in
                          Ok
                            (set_current output_path output_file current
                               operation_ref state))
                  end
              end)
      end

type final_change = {
  path : Mentat_workspace.Path.t;
  file : virtual_file;
  file_edit : Mentat_edit.t;
}

let current_edit_state = function
  | Missing -> Mentat_edit.State.Missing
  | Text text -> Mentat_edit.State.Text text.contents

let edit_for_transition path before after =
  match (before, after) with
  | Mentat_edit.State.Missing, Mentat_edit.State.Missing -> Ok Mentat_edit.empty
  | Mentat_edit.State.Missing, Mentat_edit.State.Text contents ->
      Mentat_edit.create ~path ~contents
  | Mentat_edit.State.Text before, Mentat_edit.State.Missing ->
      Mentat_edit.delete ~path ~before
  | Mentat_edit.State.Text before, Mentat_edit.State.Text after ->
      Mentat_edit.rewrite ~path ~before ~after

let final_changes state =
  let rec loop changes = function
    | [] -> Ok (List.rev changes)
    | path :: paths ->
        let file = Mentat_workspace.Path.Map.find path state.files in
        begin match
          edit_for_transition path file.original
            (current_edit_state file.current)
        with
        | Error error -> Error (Edit error)
        | Ok edit when Mentat_edit.is_empty edit -> loop changes paths
        | Ok file_edit -> loop ({ path; file; file_edit } :: changes) paths
        end
  in
  loop [] (List.rev state.reverse_order)

let moved_source files change =
  match change.file.current with
  | Missing | Text { origin = None; _ } -> None
  | Text { origin = Some source; _ }
    when Mentat_workspace.Path.equal source change.path ->
      None
  | Text { origin = Some source; _ } -> (
      match Mentat_workspace.Path.Map.find_opt source files with
      | Some
          {
            original = Mentat_edit.State.Text _;
            current = Missing;
            last_operation = _;
          } ->
          Some source
      | None
      | Some
          {
            original = Mentat_edit.State.Missing;
            current = _;
            last_operation = _;
          }
      | Some
          {
            original = Mentat_edit.State.Text _;
            current = Text _;
            last_operation = _;
          } ->
          None)

let final_summary change =
  match (change.file.original, change.file.current) with
  | Mentat_edit.State.Missing, Text { origin = None; contents; _ } ->
      {
        added_lines = Some (Text_helpers.logical_line_count contents);
        removed_lines = Some 0;
      }
  | Mentat_edit.State.Missing, Text { origin = Some _; summary; _ } -> summary
  | Mentat_edit.State.Text _, Missing ->
      { added_lines = Some 0; removed_lines = None }
  | Mentat_edit.State.Text _, Text { origin = None; contents; _ } ->
      {
        added_lines = Some (Text_helpers.logical_line_count contents);
        removed_lines = None;
      }
  | Mentat_edit.State.Text _, Text text -> text.summary
  | Mentat_edit.State.Missing, Missing -> assert false

let output_entries files changes =
  let moves, suppressed =
    List.fold_left
      (fun (moves, suppressed) change ->
        match moved_source files change with
        | None -> (moves, suppressed)
        | Some source ->
            ( Mentat_workspace.Path.Map.add change.path source moves,
              Mentat_workspace.Path.Set.add source suppressed ))
      (Mentat_workspace.Path.Map.empty, Mentat_workspace.Path.Set.empty)
      changes
  in
  List.filter_map
    (fun change ->
      if Mentat_workspace.Path.Set.mem change.path suppressed then None
      else
        let operation =
          match Mentat_workspace.Path.Map.find_opt change.path moves with
          | Some from -> Output.Move { from }
          | None -> (
              match (change.file.original, change.file.current) with
              | Mentat_edit.State.Missing, Text _ -> Output.Create
              | Mentat_edit.State.Text _, Text _ -> Output.Modify
              | Mentat_edit.State.Text _, Missing -> Output.Delete
              | Mentat_edit.State.Missing, Missing -> assert false)
        in
        Some
          (Output.make_entry ~path:change.path ~operation (final_summary change)))
    changes

let plan workspace_io ~cancelled input =
  match cwd_root_key workspace_io with
  | Error _ as error -> error
  | Ok root_key ->
      let operations = Input.operations input in
      begin match nested_path_conflict ~cancelled operations root_key with
      | Error _ as error -> error
      | Ok (Some conflict) ->
          Error (Patch (nested_path_conflict_message conflict))
      | Ok None ->
          let rec loop state index = function
            | _ when cancelled () -> Error Cancelled
            | [] -> (
                match final_changes state with
                | Error _ as error -> error
                | Ok changes -> (
                    match
                      Mentat_edit.concat
                        (List.map (fun change -> change.file_edit) changes)
                    with
                    | Error error -> Error (Edit error)
                    | Ok edit ->
                        Ok
                          {
                            plan = edit;
                            entries = output_entries state.files changes;
                          }))
            | operation :: operations -> (
                match
                  plan_operation workspace_io root_key state index operation
                with
                | Error _ as error -> error
                | Ok state -> loop state (index + 1) operations)
          in
          loop empty_virtual_files 1 operations
      end

let access_evidence_of_operation root_key operation =
  let access op relative =
    let path = rooted_path root_key relative in
    (Mentat_permission.Access.path ~op path, path)
  in
  match operation with
  | Patch.Operation.Add { path; _ } ->
      let access, path = access `Create path in
      [ (access, evidence_of_operation ~path operation) ]
  | Patch.Operation.Delete { path } ->
      let access =
        Mentat_permission.Access.path ~op:`Delete (rooted_path root_key path)
      in
      [ (access, None) ]
  | Patch.Operation.Update { path; move_to = None; _ } ->
      let access, path = access `Modify path in
      [ (access, evidence_of_operation ~path operation) ]
  | Patch.Operation.Update { path; move_to = Some destination; _ } ->
      let delete_access =
        Mentat_permission.Access.path ~op:`Delete (rooted_path root_key path)
      in
      let create_access, destination = access `Create destination in
      [
        (delete_access, None);
        (create_access, evidence_of_operation ~path:destination operation);
      ]

let permissions workspace_io input =
  match cwd_root_key workspace_io with
  | Error _ -> []
  | Ok root_key ->
      let accesses =
        List.concat_map
          (access_evidence_of_operation root_key)
          (Input.operations input)
      in
      let items =
        List.map
          (fun (access, change) ->
            Mentat_permission.Request.Item.make ?change access)
          accesses
      in
      [ Mentat_permission.Request.make ~source:name items ]

let run workspace_io input ~cancelled =
  if cancelled () then interrupted ()
  else
    match plan workspace_io ~cancelled input with
    | Error error -> failed_plan error
    | Ok planned -> (
        if cancelled () then interrupted ()
        else if Mentat_edit.is_empty planned.plan then
          Mentat_tool.Result.completed ~output:planned.entries ()
        else
          match Mentat_workspace_io.Edit.apply workspace_io planned.plan with
          | Error error -> Edit_error.failed_apply error
          | Ok _ -> Mentat_tool.Result.completed ~output:planned.entries ())

let make workspace_io =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.apply_patch
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io)
    ~run:(fun ~cancelled input -> run workspace_io input ~cancelled)
    ()
