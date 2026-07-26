(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let local_override_filename = "AGENTS.override.md"
let agents_filename = "AGENTS.md"
let claude_filename = "CLAUDE.md"
let git_marker = ".git"
let scan_cap = 2048
let scan_skip_dirs = [ ".git"; ".svn"; ".hg"; ".bzr"; ".jj"; ".sl" ]
let digest_string text = Mentat_digest.Content_ref.(to_token (of_contents text))

(* JSON building and the hand-decoders behind the support projection. The
   source projects to a stable support shape and decodes back; the projection
   is complete, so a decoded source round-trips (see {!Source.jsont}). *)

let json_mem name value = Jsont.Json.mem (Jsont.Json.name name) value
let json_object fields = Jsont.Json.object' fields

let json_field name = function
  | Jsont.Object (fields, _) -> Option.map snd (Jsont.Json.find_mem name fields)
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      None

let decode_error message = Jsont.Error.msg Jsont.Meta.none message

let require_string context name json =
  match json_field name json with
  | Some (Jsont.String (value, _)) -> value
  | Some _ -> decode_error (context ^ " " ^ name ^ " must be a string")
  | None -> decode_error (context ^ " missing " ^ name)

let optional_string context name json =
  match json_field name json with
  | None -> None
  | Some (Jsont.String (value, _)) -> Some value
  | Some _ -> decode_error (context ^ " " ^ name ^ " must be a string")

let require_int context name json =
  match json_field name json with
  | Some (Jsont.Number (value, _)) -> int_of_float value
  | Some _ -> decode_error (context ^ " " ^ name ^ " must be a number")
  | None -> decode_error (context ^ " missing " ^ name)

let require_bool context name json =
  match json_field name json with
  | Some (Jsont.Bool (value, _)) -> value
  | Some _ -> decode_error (context ^ " " ^ name ^ " must be a boolean")
  | None -> decode_error (context ^ " missing " ^ name)

type skip_reason =
  [ `Not_file
  | `Outside_workspace
  | `Unreadable of string
  | `Empty
  | `Budget_exhausted ]

module Source = struct
  type kind = Global | Project | Local_override | Compatibility

  type content = {
    bytes : int;
    digest : string;
    included_bytes : int;
    included_digest : string;
    omitted_bytes : int;
    utf8_repaired : bool;
  }

  type status =
    | Active of content
    | Shadowed of { by : Lpath.Abs.t }
    | Disabled of [ `Instructions | `Project_instructions | `Compatibility ]
    | Not_activated
    | Skipped of skip_reason

  type t = {
    path : Lpath.Abs.t;
    display_path : string;
    kind : kind;
    status : status;
  }

  let path t = t.path
  let display_path t = t.display_path
  let kind t = t.kind
  let status t = t.status

  let kind_string = function
    | Global -> "global"
    | Project -> "project"
    | Local_override -> "local_override"
    | Compatibility -> "compatibility"

  let kind_of_string = function
    | "global" -> Some Global
    | "project" -> Some Project
    | "local_override" -> Some Local_override
    | "compatibility" -> Some Compatibility
    | _ -> None

  let state_string = function
    | Active _ -> "active"
    | Shadowed _ -> "shadowed"
    | Disabled _ -> "disabled"
    | Not_activated -> "not_activated"
    | Skipped _ -> "skipped"

  let reason_string = function
    | Active _ -> None
    | Shadowed { by } ->
        let by = Option.value (Lpath.Abs.basename by) ~default:"" in
        if String.equal by local_override_filename then
          Some "shadowed_by_override"
        else Some "shadowed_by_agents"
    | Disabled `Instructions -> Some "instructions_disabled"
    | Disabled `Project_instructions -> Some "project_instructions_disabled"
    | Disabled `Compatibility -> Some "compatibility_disabled"
    | Not_activated -> Some "nested_not_activated"
    | Skipped `Not_file -> Some "not_file"
    | Skipped `Outside_workspace -> Some "outside_workspace"
    | Skipped (`Unreadable _) -> Some "unreadable"
    | Skipped `Empty -> Some "empty"
    | Skipped `Budget_exhausted -> Some "budget_exhausted"

  let detail_string = function
    | Shadowed { by } -> Some (Lpath.Abs.to_string by)
    | Skipped (`Unreadable message) -> Some message
    | Active _ | Disabled _ | Not_activated
    | Skipped (`Not_file | `Outside_workspace | `Empty | `Budget_exhausted) ->
        None

  let to_json t =
    let reason =
      match reason_string t.status with
      | None -> []
      | Some reason -> [ json_mem "reason" (Jsont.Json.string reason) ]
    in
    let detail =
      match detail_string t.status with
      | None -> []
      | Some detail -> [ json_mem "detail" (Jsont.Json.string detail) ]
    in
    let content =
      match t.status with
      | Active content ->
          [
            json_mem "bytes" (Jsont.Json.int content.bytes);
            json_mem "included_bytes" (Jsont.Json.int content.included_bytes);
            json_mem "digest" (Jsont.Json.string content.digest);
            json_mem "included_digest"
              (Jsont.Json.string content.included_digest);
            json_mem "omitted_bytes" (Jsont.Json.int content.omitted_bytes);
            json_mem "utf8_repaired" (Jsont.Json.bool content.utf8_repaired);
          ]
      | Shadowed _ | Disabled _ | Not_activated | Skipped _ -> []
    in
    json_object
      ([
         json_mem "path" (Jsont.Json.string (Lpath.Abs.to_string t.path));
         json_mem "display_path" (Jsont.Json.string t.display_path);
         json_mem "kind" (Jsont.Json.string (kind_string t.kind));
         json_mem "state" (Jsont.Json.string (state_string t.status));
       ]
      @ reason @ detail @ content)

  let content_of_json json =
    {
      bytes = require_int "context source" "bytes" json;
      included_bytes = require_int "context source" "included_bytes" json;
      digest = require_string "context source" "digest" json;
      included_digest = require_string "context source" "included_digest" json;
      omitted_bytes = require_int "context source" "omitted_bytes" json;
      utf8_repaired = require_bool "context source" "utf8_repaired" json;
    }

  let abs_of_json name json =
    match Lpath.Abs.of_string (require_string "context source" name json) with
    | Ok abs -> abs
    | Error error -> decode_error (Lpath.Error.message error)

  let detail json =
    Option.value ~default:"" (optional_string "context source" "detail" json)

  (* Reconstruct a status from the projected fields. The projection is
     complete, so no field decodes to a placeholder. *)
  let status_of_json json =
    match require_string "context source" "state" json with
    | "active" -> Active (content_of_json json)
    | "shadowed" -> (
        match Lpath.Abs.of_string (detail json) with
        | Ok by -> Shadowed { by }
        | Error error -> decode_error (Lpath.Error.message error))
    | "disabled" -> (
        match optional_string "context source" "reason" json with
        | Some "instructions_disabled" -> Disabled `Instructions
        | Some "project_instructions_disabled" -> Disabled `Project_instructions
        | Some "compatibility_disabled" -> Disabled `Compatibility
        | Some other -> decode_error ("unknown disabled reason: " ^ other)
        | None -> decode_error "disabled source missing reason")
    | "not_activated" -> Not_activated
    | "skipped" -> (
        match optional_string "context source" "reason" json with
        | Some "not_file" -> Skipped `Not_file
        | Some "outside_workspace" -> Skipped `Outside_workspace
        | Some "unreadable" -> Skipped (`Unreadable (detail json))
        | Some "empty" -> Skipped `Empty
        | Some "budget_exhausted" -> Skipped `Budget_exhausted
        | Some other -> decode_error ("unknown skipped reason: " ^ other)
        | None -> decode_error "skipped source missing reason")
    | other -> decode_error ("unknown context source state: " ^ other)

  let of_json json =
    match json with
    | Jsont.Object _ ->
        let kind =
          match
            kind_of_string (require_string "context source" "kind" json)
          with
          | Some kind -> kind
          | None -> decode_error "unknown context source kind"
        in
        {
          path = abs_of_json "path" json;
          display_path = require_string "context source" "display_path" json;
          kind;
          status = status_of_json json;
        }
    | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
    | Jsont.Array _ ->
        decode_error "context source must be an object"

  let jsont =
    Jsont.map ~kind:"context source" ~dec:of_json ~enc:to_json Jsont.json
end

module Fragment = struct
  type role = System | Developer | User
  type t = { role : role; text : string; sources : Lpath.Abs.t list }

  let role t = t.role
  let text t = t.text

  let role_string = function
    | System -> "system"
    | Developer -> "developer"
    | User -> "user"

  let to_json t =
    json_object
      [
        json_mem "role" (Jsont.Json.string (role_string t.role));
        json_mem "sources"
          (Jsont.Json.list
             (List.map
                (fun path -> Jsont.Json.string (Lpath.Abs.to_string path))
                t.sources));
        json_mem "text" (Jsont.Json.string t.text);
      ]
end

type t = {
  cwd : Lpath.Abs.t;
  root : Lpath.Abs.t;
  root_marker : string option;
  budget_used : int;
  nested_scan : [ `Off | `Complete | `Capped ];
  sources : Source.t list;
  fragments : Fragment.t list;
  rendered_digest : string;
}

let cwd t = t.cwd
let root t = t.root
let root_marker t = t.root_marker
let budget_used t = t.budget_used
let nested_scan t = t.nested_scan
let sources t = t.sources
let rendered_digest t = t.rendered_digest

(* Path helpers over the resolved directories. The candidate filenames are
   valid path components, so appending them cannot fail. *)

let child dir component =
  match Lpath.Abs.add_component dir component with
  | Ok abs -> abs
  | Error _ -> assert false

let project_display ~root abs =
  match Lpath.Abs.relativize ~root abs with
  | Some rel -> "./" ^ Lpath.Rel.to_string rel
  | None -> Lpath.Abs.to_string abs

let project_dirs ~root cwd_rel =
  let step (dir, acc) component =
    let next = child dir component in
    (next, next :: acc)
  in
  let _, acc =
    List.fold_left step (root, [ root ]) (Lpath.Rel.components cwd_rel)
  in
  List.rev acc

(* Candidate observation. Metadata only: a followed symlink must resolve inside
   the containment root; content is never read here. *)

type observed = Missing | File | Bad of skip_reason

let observe_candidate ~fs ~root abs =
  match Io.observe ~fs ~root abs with
  | Io.Missing -> Missing
  | Io.Found Io.Regular -> File
  | Io.Found (Io.Directory | Io.Other) -> Bad `Not_file
  | Io.Escapes -> Bad `Outside_workspace
  | Io.Failed message -> Bad (`Unreadable message)

type enabled = { global : bool; project : bool; claude_md : bool }

type pending =
  | Settled of Source.t
  | Read_candidate of {
      abs : Lpath.Abs.t;
      display : string;
      kind : Source.kind;
    }

let make_source ~abs ~display ~kind status =
  { Source.path = abs; display_path = display; kind; status }

let is_compatibility = function
  | Source.Compatibility -> true
  | Source.Global | Source.Project | Source.Local_override -> false

let resolve_dir ~fs ~root ~enabled dir =
  let candidates =
    [
      (local_override_filename, Source.Local_override);
      (agents_filename, Source.Project);
      (claude_filename, Source.Compatibility);
    ]
  in
  let observed =
    List.filter_map
      (fun (name, kind) ->
        let abs = child dir name in
        match observe_candidate ~fs ~root abs with
        | Missing -> None
        | obs -> Some (kind, abs, obs))
      candidates
  in
  let settle (winner, acc) (kind, abs, obs) =
    let display = project_display ~root abs in
    let settled status =
      (winner, Settled (make_source ~abs ~display ~kind status) :: acc)
    in
    match obs with
    | Missing -> (winner, acc)
    | (File | Bad _) when not enabled.project ->
        settled (Source.Disabled `Project_instructions)
    | (File | Bad _) when is_compatibility kind && not enabled.claude_md ->
        settled (Source.Disabled `Compatibility)
    | Bad reason -> settled (Source.Skipped reason)
    | File -> (
        match winner with
        | Some by -> settled (Source.Shadowed { by })
        | None -> (Some abs, Read_candidate { abs; display; kind } :: acc))
  in
  let _, acc = List.fold_left settle (None, []) observed in
  List.rev acc

(* Text projection. UTF-8 repair, trimming, and budget accounting are pure.
   The budget counts original file bytes; truncation slices the repaired text
   at a character boundary. *)

let utf8_lossy text =
  if String.is_valid_utf_8 text then text
  else
    let replacement = Uchar.of_int 0xFFFD in
    let buffer = Buffer.create (String.length text) in
    let rec loop index =
      if index >= String.length text then Buffer.contents buffer
      else
        let decode = String.get_utf_8_uchar text index in
        let length = max 1 (Uchar.utf_decode_length decode) in
        let uchar =
          if Uchar.utf_decode_is_valid decode then Uchar.utf_decode_uchar decode
          else replacement
        in
        Buffer.add_utf_8_uchar buffer uchar;
        loop (index + length)
    in
    loop 0

let take_utf8_prefix text max_bytes =
  if max_bytes <= 0 then ""
  else if String.length text <= max_bytes then text
  else
    let rec boundary stop =
      if stop >= max_bytes then stop
      else
        let decode = String.get_utf_8_uchar text stop in
        let length = max 1 (Uchar.utf_decode_length decode) in
        if stop + length > max_bytes then stop else boundary (stop + length)
    in
    String.sub text 0 (boundary 0)

let instruction_header display = "Instructions from: " ^ display

let truncated_marker ~budget ~omitted =
  "[Instruction file truncated: omitted " ^ string_of_int omitted
  ^ " byte(s) due to the " ^ string_of_int budget
  ^ "-byte project instruction budget]"

let omitted_marker =
  "[Instruction file omitted: project instruction budget exhausted]"

let read_facts raw =
  ( String.length raw,
    digest_string raw,
    not (String.is_valid_utf_8 raw),
    String.trim (utf8_lossy raw) )

let global_entry ~fs ~abs ~display ~kind =
  match Io.read_file ~fs ~max_bytes:Io.read_limit abs with
  | Error message ->
      ( make_source ~abs ~display ~kind (Source.Skipped (`Unreadable message)),
        None )
  | Ok raw ->
      let bytes, digest, utf8_repaired, text = read_facts raw in
      if String.is_empty text then
        (make_source ~abs ~display ~kind (Source.Skipped `Empty), None)
      else
        let content =
          {
            Source.bytes;
            digest;
            included_bytes = String.length text;
            included_digest = digest_string text;
            omitted_bytes = 0;
            utf8_repaired;
          }
        in
        ( make_source ~abs ~display ~kind (Source.Active content),
          Some (instruction_header display ^ "\n" ^ text) )

let project_entry ~fs ~budget ~remaining ~abs ~display ~kind =
  if remaining <= 0 then
    ( make_source ~abs ~display ~kind (Source.Skipped `Budget_exhausted),
      Some (instruction_header display ^ "\n" ^ omitted_marker),
      remaining )
  else
    match Io.read_file ~fs ~max_bytes:Io.read_limit abs with
    | Error message ->
        ( make_source ~abs ~display ~kind (Source.Skipped (`Unreadable message)),
          None,
          remaining )
    | Ok raw ->
        let bytes, digest, utf8_repaired, text = read_facts raw in
        let consumed = min bytes remaining in
        let omitted = bytes - consumed in
        let remaining = remaining - consumed in
        let text =
          if omitted = 0 then text
          else String.trim (take_utf8_prefix text consumed)
        in
        if String.is_empty text && omitted = 0 then
          ( make_source ~abs ~display ~kind (Source.Skipped `Empty),
            None,
            remaining )
        else
          let body =
            if omitted = 0 then text
            else if String.is_empty text then truncated_marker ~budget ~omitted
            else text ^ "\n\n" ^ truncated_marker ~budget ~omitted
          in
          let content =
            {
              Source.bytes;
              digest;
              included_bytes = String.length text;
              included_digest = digest_string text;
              omitted_bytes = omitted;
              utf8_repaired;
            }
          in
          ( make_source ~abs ~display ~kind (Source.Active content),
            Some (instruction_header display ^ "\n" ^ body),
            remaining )

let read_project ~fs ~budget pendings =
  let step (sources, blocks, remaining) = function
    | Settled source -> (source :: sources, blocks, remaining)
    | Read_candidate { abs; display; kind } ->
        let source, block, remaining =
          project_entry ~fs ~budget ~remaining ~abs ~display ~kind
        in
        let blocks =
          match block with
          | None -> blocks
          | Some block -> (abs, block) :: blocks
        in
        (source :: sources, blocks, remaining)
  in
  let sources, blocks, remaining =
    List.fold_left step ([], [], budget) pendings
  in
  (List.rev sources, List.rev blocks, remaining)

(* The global candidate is a single [AGENTS.md] in the user config directory,
   observed against its own directory as containment root and never budgeted. *)

let global_candidate ~fs ~enabled ~user_config_file =
  match Lpath.Abs.parent user_config_file with
  | None -> ([], [])
  | Some dir -> (
      let abs = child dir agents_filename in
      let display = Lpath.Abs.to_string abs in
      let kind = Source.Global in
      let settled status = ([ make_source ~abs ~display ~kind status ], []) in
      match observe_candidate ~fs ~root:dir abs with
      | Missing -> ([], [])
      | (File | Bad _) when not enabled.global ->
          settled (Source.Disabled `Instructions)
      | Bad reason -> settled (Source.Skipped reason)
      | File ->
          let source, block = global_entry ~fs ~abs ~display ~kind in
          let blocks =
            match block with None -> [] | Some block -> [ (abs, block) ]
          in
          ([ source ], blocks))

(* Nested audit scan: directories strictly below cwd, lexicographic, no symlink
   following, VCS metadata skipped, capped. Facts only. *)

let nested_scan_sources ~fs ~root cwd =
  let found = ref [] in
  let visited = ref 0 in
  let capped = ref false in
  let rec walk ~record dir =
    if !capped then ()
    else if !visited >= scan_cap then capped := true
    else begin
      incr visited;
      match Io.read_dir_names ~fs dir with
      | Error _ -> ()
      | Ok names ->
          let names = List.sort String.compare names in
          List.iter
            (fun name ->
              if not !capped then
                let entry = child dir name in
                match Io.entry_kind ~fs entry with
                | Some Io.Directory when not (List.mem name scan_skip_dirs) ->
                    walk ~record:true entry
                | Some Io.Regular
                  when record && String.equal name agents_filename ->
                    found := entry :: !found
                | Some (Io.Directory | Io.Regular | Io.Other) | None -> ())
            names
    end
  in
  walk ~record:false cwd;
  let sources =
    List.rev_map
      (fun abs ->
        make_source ~abs
          ~display:(project_display ~root abs)
          ~kind:Source.Project Source.Not_activated)
      !found
  in
  (sources, if !capped then `Capped else `Complete)

(* Projection rendering. The layout is byte-stable: a base system identity, a
   workspace developer message, and one contextual user message wrapping the
   per-source instruction blocks. *)

module Environment = struct
  type t = {
    date : string option;
    model : string option;
    model_cutoff : string option;
    platform : string option;
  }

  let none = { date = None; model = None; model_cutoff = None; platform = None }

  let make ?date ?model ?model_cutoff ?platform () =
    { date; model; model_cutoff; platform }
end

(* The git flag is derived from [root_marker], the [.git] marker [load] already
   detects; the remaining facts are sourced at the composition root and passed
   in, so this render never reads a clock and stays deterministic. *)
let environment_text ~cwd_text ~root_marker (env : Environment.t) =
  let buffer = Buffer.create 160 in
  let line label value =
    Buffer.add_string buffer "\n- ";
    Buffer.add_string buffer label;
    Buffer.add_string buffer value
  in
  Buffer.add_string buffer "# Environment\n\n- Current working directory: ";
  Buffer.add_string buffer cwd_text;
  Option.iter (line "Today's date: ") env.Environment.date;
  (match env.Environment.model with
  | None -> ()
  | Some model ->
      line "Model: " model;
      Option.iter
        (fun cutoff ->
          Buffer.add_string buffer "; knowledge cutoff ";
          Buffer.add_string buffer cutoff)
        env.Environment.model_cutoff);
  line "Git repository: "
    (match root_marker with Some _ -> "yes" | None -> "no");
  Option.iter (line "Platform: ") env.Environment.platform;
  Buffer.contents buffer

let instructions_text cwd_text blocks =
  "# AGENTS.md instructions for " ^ cwd_text ^ "\n\n<INSTRUCTIONS>\n"
  ^ String.concat "\n\n" blocks
  ^ "\n</INSTRUCTIONS>"

let render_fragments ~environment ~cwd_text ~root_marker blocks =
  let base =
    [
      {
        Fragment.role = Fragment.System;
        text = Mentat_prompts.system;
        sources = [];
      };
      {
        Fragment.role = Fragment.Developer;
        text = environment_text ~cwd_text ~root_marker environment;
        sources = [];
      };
    ]
  in
  match blocks with
  | [] -> base
  | blocks ->
      base
      @ [
          {
            Fragment.role = Fragment.User;
            text = instructions_text cwd_text (List.map snd blocks);
            sources = List.map fst blocks;
          };
        ]

(* The length prefix keeps the encoding injective over the (role, text)
   fragment sequence: digest equality implies byte equality. It must not be
   reduced to plain concatenation, which would confuse fragment boundaries. *)
let digest_fragments fragments =
  let buffer = Buffer.create 1024 in
  List.iter
    (fun fragment ->
      let add text =
        Buffer.add_string buffer (string_of_int (String.length text));
        Buffer.add_char buffer ':';
        Buffer.add_string buffer text
      in
      add (Fragment.role_string (Fragment.role fragment));
      add (Fragment.text fragment))
    fragments;
  digest_string (Buffer.contents buffer)

let message_of_fragment fragment =
  match Fragment.role fragment with
  | Fragment.System -> Mentat_llm.Message.system (Fragment.text fragment)
  | Fragment.Developer -> Mentat_llm.Message.developer (Fragment.text fragment)
  | Fragment.User -> Mentat_llm.Message.user_text (Fragment.text fragment)

let fragments t = List.map message_of_fragment t.fragments

let prelude t ~developer ~skills =
  fragments t @ developer @ Option.to_list (Skills.catalog_fragment skills)

let fragments_json t = List.map Fragment.to_json t.fragments

let warnings t =
  let source_warnings source =
    let display = Source.display_path source in
    match Source.status source with
    | Source.Skipped (`Unreadable message) -> [ display ^ ": " ^ message ]
    | Source.Skipped `Budget_exhausted ->
        [ display ^ ": omitted: project instruction budget exhausted" ]
    | Source.Active content ->
        (if content.Source.omitted_bytes > 0 then
           [
             display ^ ": truncated: omitted "
             ^ string_of_int content.Source.omitted_bytes
             ^ " byte(s) by the project instruction budget";
           ]
         else [])
        @
        if content.Source.utf8_repaired then
          [ display ^ ": invalid UTF-8 replaced with U+FFFD" ]
        else []
    | Source.Shadowed _ | Source.Disabled _ | Source.Not_activated
    | Source.Skipped (`Not_file | `Outside_workspace | `Empty) ->
        []
  in
  let compatibility_guidance =
    (* A project whose only instruction file is a disabled CLAUDE.md should say
       so clearly and point to the way out. *)
    let disabled_compatibility source =
      match Source.status source with
      | Source.Disabled `Compatibility -> true
      | Source.Active _ | Source.Shadowed _
      | Source.Disabled (`Instructions | `Project_instructions)
      | Source.Not_activated | Source.Skipped _ ->
          false
    in
    let active_project source =
      match (Source.kind source, Source.status source) with
      | ( (Source.Project | Source.Local_override | Source.Compatibility),
          Source.Active _ ) ->
          true
      | _, _ -> false
    in
    if
      List.exists disabled_compatibility t.sources
      && not (List.exists active_project t.sources)
    then
      [
        "CLAUDE.md compatibility is disabled; enable instructions.claude_md or \
         migrate to AGENTS.md";
      ]
    else []
  in
  List.concat_map source_warnings t.sources
  @ compatibility_guidance
  @
  match t.nested_scan with
  | `Capped ->
      [
        "nested instruction scan stopped at " ^ string_of_int scan_cap
        ^ " directories";
      ]
  | `Off | `Complete -> []

let load ~environment ~stdenv ~nested_scan ~config ~trusted ~root ~cwd
    ~user_config_file =
  match Lpath.Abs.relativize ~root cwd with
  | None ->
      Error
        (Mentat_diagnostic.make
           ~context:
             (Printf.sprintf "root: %s\ncwd:  %s" (Lpath.Abs.to_string root)
                (Lpath.Abs.to_string cwd))
           "the run directory is not within the workspace root")
  | Some cwd_rel ->
      let cwd_text = Lpath.Abs.to_string cwd in
      let fs = Eio.Stdenv.fs stdenv in
      let get field = Mentat_config.Resolved.get field config in
      let enabled =
        {
          global = get Mentat_config.Field.instructions_global;
          project = get Mentat_config.Field.instructions_project;
          claude_md = get Mentat_config.Field.instructions_claude_md;
        }
      in
      let budget = get Mentat_config.Field.instructions_project_max_bytes in
      let root_marker =
        if Io.is_file_or_directory ~fs (child root git_marker) then
          Some git_marker
        else None
      in
      let global_sources, global_blocks =
        global_candidate ~fs ~enabled ~user_config_file
      in
      let project_sources, project_blocks, remaining =
        if trusted then
          let pendings =
            List.concat_map
              (resolve_dir ~fs ~root ~enabled)
              (project_dirs ~root cwd_rel)
          in
          read_project ~fs ~budget pendings
        else ([], [], budget)
      in
      let nested_sources, scan_state =
        if trusted && nested_scan then nested_scan_sources ~fs ~root cwd
        else ([], `Off)
      in
      let fragments =
        render_fragments ~environment ~cwd_text ~root_marker
          (global_blocks @ project_blocks)
      in
      let sources = global_sources @ project_sources @ nested_sources in
      Ok
        {
          cwd;
          root;
          root_marker;
          budget_used = budget - remaining;
          nested_scan = scan_state;
          sources;
          fragments;
          rendered_digest = digest_fragments fragments;
        }
