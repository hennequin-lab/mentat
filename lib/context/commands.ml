(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let digest_string text = Mentat_digest.Content_ref.(to_token (of_contents text))

(* JSON building and the hand-decoders behind the support projection. The
   projection omits command bodies and raw text, so a decoded value carries
   those empty (see {!Command.jsont}). *)

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

let string_list context name json =
  match json_field name json with
  | None -> []
  | Some (Jsont.Array (items, _)) ->
      List.map
        (function
          | Jsont.String (value, _) -> value
          | _ -> decode_error (context ^ " " ^ name ^ " must be strings"))
        items
  | Some _ -> decode_error (context ^ " " ^ name ^ " must be an array")

module Name = struct
  type t = string

  let max_segment_bytes = 64

  let valid_segment segment =
    String.length segment > 0
    && String.length segment <= max_segment_bytes
    && (match segment.[0] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false)
    && String.for_all
         (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
         segment

  let valid name =
    String.length name > 0
    && List.for_all valid_segment (String.split_on_char ':' name)

  let of_string name =
    if valid name then Ok name
    else
      Error
        (Printf.sprintf
           "%S is not a command name (one or more :-joined segments of \
            lowercase letters, digits, and hyphens, each at most %d bytes and \
            leading with a letter or digit)"
           name max_segment_bytes)

  let to_string name = name
  let equal = String.equal
  let compare = String.compare
  let pp = Format.pp_print_string
end

module Command = struct
  type kind = Project | Compat_agents | Compat_claude | User

  type content = {
    description : string option;
    argument_hint : string option;
    body : string;
    text : string;
    bytes : int;
    digest : string;
    ignored_keys : string list;
  }

  type invalid =
    [ `Invalid_name of string
    | `Empty_body
    | `Invalid_frontmatter of string
    | `Unreadable of string ]

  type status =
    | Active of content
    | Shadowed of { by : string }
    | Disabled of [ `Project | `Compat | `Config ]
    | Invalid of invalid

  type t = {
    name : string;
    valid_name : Name.t option;
    kind : kind;
    status : status;
    origin : string;
  }

  let name t = t.name
  let kind t = t.kind
  let status t = t.status
  let origin t = t.origin

  let kind_string = function
    | Project -> "project"
    | Compat_agents -> "compat_agents"
    | Compat_claude -> "compat_claude"
    | User -> "user"

  let kind_of_string = function
    | "project" -> Some Project
    | "compat_agents" -> Some Compat_agents
    | "compat_claude" -> Some Compat_claude
    | "user" -> Some User
    | _ -> None

  let state_string = function
    | Active _ -> "active"
    | Shadowed _ -> "shadowed"
    | Disabled _ -> "disabled"
    | Invalid _ -> "invalid"

  let reason_string = function
    | Active _ -> None
    | Shadowed _ -> Some "shadowed"
    | Disabled `Project -> Some "project_disabled"
    | Disabled `Compat -> Some "compat_disabled"
    | Disabled `Config -> Some "config_disabled"
    | Invalid (`Invalid_name _) -> Some "invalid_name"
    | Invalid `Empty_body -> Some "empty_body"
    | Invalid (`Invalid_frontmatter _) -> Some "invalid_frontmatter"
    | Invalid (`Unreadable _) -> Some "unreadable"

  let detail_string = function
    | Shadowed { by } -> Some by
    | Invalid (`Invalid_name raw) -> Some raw
    | Invalid (`Invalid_frontmatter message) | Invalid (`Unreadable message) ->
        Some message
    | Active _ | Disabled _ | Invalid `Empty_body -> None

  let to_json t =
    let base =
      [
        json_mem "name" (Jsont.Json.string t.name);
        json_mem "kind" (Jsont.Json.string (kind_string t.kind));
        json_mem "origin" (Jsont.Json.string t.origin);
        json_mem "state" (Jsont.Json.string (state_string t.status));
      ]
    in
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
    let string_opt name = function
      | None -> []
      | Some value -> [ json_mem name (Jsont.Json.string value) ]
    in
    let content =
      match t.status with
      | Active content ->
          [
            json_mem "bytes" (Jsont.Json.int content.bytes);
            json_mem "digest" (Jsont.Json.string content.digest);
            json_mem "ignored_keys"
              (Jsont.Json.list
                 (List.map
                    (fun value -> Jsont.Json.string value)
                    content.ignored_keys));
          ]
          @ string_opt "description" content.description
          @ string_opt "argument_hint" content.argument_hint
      | Shadowed _ | Disabled _ | Invalid _ -> []
    in
    json_object (base @ reason @ detail @ content)

  (* Reconstruct a status from the projected fields. [text] and [body] are not
     carried on the wire and decode empty. *)
  let status_of_json json =
    match require_string "command" "state" json with
    | "active" ->
        Active
          {
            description = optional_string "command" "description" json;
            argument_hint = optional_string "command" "argument_hint" json;
            body = "";
            text = "";
            bytes = require_int "command" "bytes" json;
            digest = require_string "command" "digest" json;
            ignored_keys = string_list "command" "ignored_keys" json;
          }
    | "shadowed" ->
        Shadowed
          {
            by =
              Option.value ~default:"" (optional_string "command" "detail" json);
          }
    | "disabled" -> (
        match optional_string "command" "reason" json with
        | Some "project_disabled" -> Disabled `Project
        | Some "compat_disabled" -> Disabled `Compat
        | Some "config_disabled" -> Disabled `Config
        | Some other -> decode_error ("unknown disabled reason: " ^ other)
        | None -> decode_error "disabled command missing reason")
    | "invalid" -> (
        let detail () =
          Option.value ~default:"" (optional_string "command" "detail" json)
        in
        match optional_string "command" "reason" json with
        | Some "invalid_name" -> Invalid (`Invalid_name (detail ()))
        | Some "empty_body" -> Invalid `Empty_body
        | Some "invalid_frontmatter" ->
            Invalid (`Invalid_frontmatter (detail ()))
        | Some "unreadable" -> Invalid (`Unreadable (detail ()))
        | Some other -> decode_error ("unknown invalid reason: " ^ other)
        | None -> decode_error "invalid command missing reason")
    | other -> decode_error ("unknown command state: " ^ other)

  let of_json json =
    match json with
    | Jsont.Object _ ->
        let name = require_string "command" "name" json in
        let kind =
          match kind_of_string (require_string "command" "kind" json) with
          | Some kind -> kind
          | None -> decode_error "unknown command kind"
        in
        {
          name;
          valid_name = Name.of_string name |> Result.to_option;
          kind;
          status = status_of_json json;
          origin = require_string "command" "origin" json;
        }
    | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
    | Jsont.Array _ ->
        decode_error "command must be an object"

  let jsont = Jsont.map ~kind:"command" ~dec:of_json ~enc:to_json Jsont.json
end

(* Expansion. A single left-to-right pass over the body, pure by construction:
   no filesystem, no process, no network. Substituted values are spliced in
   place and never re-scanned. *)

let arguments_token = "ARGUMENTS"

(* The positional tokens: maximal runs of bytes that are neither an ASCII space
   nor an ASCII tab. Every other byte, a newline included, is part of a token. *)
let positional_tokens arguments =
  let is_horizontal_space = function ' ' | '\t' -> true | _ -> false in
  let n = String.length arguments in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < n do
    while !i < n && is_horizontal_space arguments.[!i] do
      incr i
    done;
    if !i < n then begin
      let start = !i in
      while !i < n && not (is_horizontal_space arguments.[!i]) do
        incr i
      done;
      tokens := String.sub arguments start (!i - start) :: !tokens
    end
  done;
  Array.of_list (List.rev !tokens)

let matches_at body pos token =
  let len = String.length token in
  pos + len <= String.length body
  && String.equal (String.sub body pos len) token

(* The [$]-arm, shared by the pure [substitute] and the effectful [expand_body]
   so the two stay byte-identical on [$]. [body.[i]] is ['$']; appends the
   substitution — the whole argument string for [$ARGUMENTS], the positional (or
   nothing when absent) for [$1]..[$9] — or the literal ['$'] otherwise, and
   returns the index just past what it consumed. *)
let dollar_step ~buf ~body ~positionals ~arguments i =
  if matches_at body (i + 1) arguments_token then begin
    Buffer.add_string buf arguments;
    i + 1 + String.length arguments_token
  end
  else if
    i + 1 < String.length body
    && match body.[i + 1] with '1' .. '9' -> true | _ -> false
  then begin
    let index = Char.code body.[i + 1] - Char.code '1' in
    if index < Array.length positionals then
      Buffer.add_string buf positionals.(index);
    i + 2
  end
  else begin
    Buffer.add_char buf '$';
    i + 1
  end

let substitute ~body ~arguments =
  let positionals = positional_tokens arguments in
  let n = String.length body in
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if body.[!i] = '$' then
      i := dollar_step ~buf ~body ~positionals ~arguments !i
    else begin
      Buffer.add_char buf body.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* [@file] embedding. [expand] layers this effectful pass over the pure [$]
   substitution as one left-to-right walk: a [$]-token splices inert argument
   text and an [@]-token embeds a file's bytes. Neither spliced arguments nor
   embedded bytes are re-scanned, so both sigils are inert inside them. Reads go
   through the same contained authority discovery uses ([Io]), and only under a
   trusted workspace. *)

type file_error = { path : string; reason : string }

(* Repair invalid UTF-8 by replacing each ill-formed sequence with U+FFFD, as
   instruction reads do (context.ml). *)
let utf8_lossy text =
  if String.is_valid_utf_8 text then text
  else begin
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
  end

(* Cut [text] to at most [max_bytes] without splitting a UTF-8 sequence. *)
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

(* An embedded file is bounded like a skill resource read (skills.ml): an
   over-cap file is repaired, cut at a UTF-8 boundary, and flagged, never spliced
   whole. *)
let max_embed_bytes = 128 * 1024
let embed_truncation_marker = "\n[... file truncated by mentat ...]"

let embed_bytes raw =
  let repaired = utf8_lossy raw in
  if String.length repaired <= max_embed_bytes then repaired
  else take_utf8_prefix repaired max_embed_bytes ^ embed_truncation_marker

(* The [@]-path run stops at any ASCII whitespace: a newline or carriage return
   ends a path just as a space or tab does. *)
let is_path_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\011' | '\012' -> true
  | _ -> false

(* The combined walk. [resolve path] yields the bytes to embed for [@path] or a
   [file_error]; the first failure aborts the whole expansion. The [$]-arm is the
   shared [dollar_step]; the added [@]-arm consumes [@] plus the maximal
   non-whitespace run as one file-reference token, so a [$] inside that run is
   part of the path, not a positional. Neither the spliced argument text nor the
   embedded file bytes are revisited by the cursor, so both are inert. *)
let expand_body ~resolve ~body ~arguments =
  let positionals = positional_tokens arguments in
  let n = String.length body in
  let buf = Buffer.create n in
  let i = ref 0 in
  let exception Unresolved of file_error in
  try
    while !i < n do
      let c = body.[!i] in
      if c = '$' then i := dollar_step ~buf ~body ~positionals ~arguments !i
      else if c = '@' && !i + 1 < n && not (is_path_whitespace body.[!i + 1])
      then begin
        let start = !i + 1 in
        let stop = ref start in
        while !stop < n && not (is_path_whitespace body.[!stop]) do
          incr stop
        done;
        let path = String.sub body start (!stop - start) in
        (match resolve path with
        | Ok bytes -> Buffer.add_string buf bytes
        | Error error -> raise (Unresolved error));
        i := !stop
      end
      else begin
        Buffer.add_char buf c;
        incr i
      end
    done;
    Ok (Buffer.contents buf)
  with Unresolved error -> Error error

(* Discovery. Filesystem reads go through the lib's contained-read helper [Io];
   directory descent uses the no-follow [Io.entry_kind] so a symlinked
   subdirectory cannot loop, and each candidate file is confirmed in-root with
   [Io.observe] before it is read. *)

let md_ext = ".md"
let max_depth = 64

let is_consumed_key key =
  String.equal key "description" || String.equal key "argument-hint"

let duplicate_consumed_key keys =
  let rec loop seen = function
    | [] -> None
    | key :: keys ->
        if is_consumed_key key && List.exists (String.equal key) seen then
          Some key
        else
          let seen = if is_consumed_key key then key :: seen else seen in
          loop seen keys
  in
  loop [] keys

let metadata_text field value =
  if
    String.exists
      (fun c ->
        let code = Char.code c in
        code < 0x20 || code = 0x7F)
      value
  then
    Error
      (`Invalid_frontmatter
         (Printf.sprintf "frontmatter %s must be single-line text" field))
  else Ok value

let optional_metadata field header =
  match Frontmatter.field field header with
  | `Absent -> Ok None
  | `Not_string ->
      Error
        (`Invalid_frontmatter
           (Printf.sprintf "frontmatter %s must be a string" field))
  | `String value -> (
      match metadata_text field value with
      | Error _ as error -> error
      | Ok value -> Ok (Some value))

(* Builds content facts for one read command file. *)
let content_of_text text =
  match Frontmatter.parse text with
  | Error error ->
      Error (`Invalid_frontmatter (Frontmatter.Error.message error))
  | Ok header -> (
      match duplicate_consumed_key (Frontmatter.keys header) with
      | Some key ->
          Error
            (`Invalid_frontmatter
               (Printf.sprintf "frontmatter %s must appear only once" key))
      | None -> (
          match optional_metadata "description" header with
          | Error _ as error -> error
          | Ok description -> (
              match optional_metadata "argument-hint" header with
              | Error _ as error -> error
              | Ok argument_hint ->
                  let body = Frontmatter.body header in
                  if String.equal (String.trim body) "" then Error `Empty_body
                  else
                    let ignored_keys =
                      Frontmatter.keys header
                      |> List.filter (fun key -> not (is_consumed_key key))
                    in
                    Ok
                      {
                        Command.description;
                        argument_hint;
                        body;
                        text;
                        bytes = String.length text;
                        digest = digest_string text;
                        ignored_keys;
                      })))

let child abs component =
  Lpath.Abs.add_component abs component |> Result.to_option

(* Classify one [*.md] file at path [file_abs], reached through [rev_segments]
   directory components (innermost first). [name] is the on-disk basename. *)
let classify_file ~fs ~root_abs ~kind ~gate ~rev_segments ~name file_abs =
  let file_segment = Filename.chop_suffix name md_ext in
  let components = List.rev (file_segment :: rev_segments) in
  let raw = String.concat ":" components in
  let valid_name =
    if List.for_all Name.valid_segment components then Some raw else None
  in
  let origin = Lpath.Abs.to_string file_abs in
  let make status =
    Some { Command.name = raw; valid_name; kind; status; origin }
  in
  match Io.observe ~fs ~root:root_abs file_abs with
  | Io.Missing | Io.Found (Io.Directory | Io.Other) ->
      None (* not a regular command file *)
  | Io.Escapes ->
      make (Command.Invalid (`Unreadable "resolves outside the commands root"))
  | Io.Failed message -> make (Command.Invalid (`Unreadable message))
  | Io.Found Io.Regular -> (
      match gate with
      | Some disabled -> make (Command.Disabled disabled)
      | None -> (
          match valid_name with
          | None -> make (Command.Invalid (`Invalid_name raw))
          | Some _ -> (
              match Io.read_file ~fs ~max_bytes:Io.read_limit file_abs with
              | Error message -> make (Command.Invalid (`Unreadable message))
              | Ok text -> (
                  match content_of_text text with
                  | Ok content -> make (Command.Active content)
                  | Error invalid -> make (Command.Invalid invalid)))))

(* Recursively walk one commands root, gathering candidates in lexicographic
   depth-first order. Entries whose name starts with a dot are skipped, so a
   command directory's [.git] or dotfiles never become candidates. *)
let rec walk ~fs ~root_abs ~kind ~gate ~rev_segments ~depth dir_abs =
  if depth > max_depth then []
  else
    match Io.read_dir_names ~fs dir_abs with
    | Error _ -> []
    | Ok names ->
        names |> List.sort String.compare
        |> List.concat_map (fun name ->
            if String.length name = 0 || name.[0] = '.' then []
            else
              match child dir_abs name with
              | None -> []
              | Some path -> (
                  match Io.entry_kind ~fs path with
                  | Some Io.Directory ->
                      walk ~fs ~root_abs ~kind ~gate
                        ~rev_segments:(name :: rev_segments) ~depth:(depth + 1)
                        path
                  | Some (Io.Regular | Io.Other) | None ->
                      if Filename.check_suffix name md_ext then
                        match
                          classify_file ~fs ~root_abs ~kind ~gate ~rev_segments
                            ~name path
                        with
                        | Some candidate -> [ candidate ]
                        | None -> []
                      else []))

let walk_root ~fs ~kind ~gate root_abs =
  walk ~fs ~root_abs ~kind ~gate ~rev_segments:[] ~depth:0 root_abs

let under base segments =
  List.fold_left
    (fun abs segment ->
      match Lpath.Abs.add_component abs segment with
      | Ok abs -> abs
      | Error _ -> abs)
    base segments

(* Per-command config disable ([commands.disabled]). Every candidate whose name
   is listed becomes [Disabled `Config], before shadowing, so no lower-precedence
   candidate of the name is promoted to active in its place. *)
let apply_config_disable disabled candidates =
  match disabled with
  | [] -> candidates
  | disabled ->
      List.map
        (fun (command : Command.t) ->
          match command.Command.valid_name with
          | Some name when List.exists (Name.equal name) disabled ->
              { command with Command.status = Command.Disabled `Config }
          | _ -> command)
        candidates

(* Shadowing: the first active candidate per name wins; invalid and disabled
   candidates never claim a name. *)
let apply_shadowing candidates =
  let winners = Hashtbl.create 16 in
  List.map
    (fun (command : Command.t) ->
      match command.Command.status with
      | Command.Active _ -> (
          match Hashtbl.find_opt winners command.Command.name with
          | None ->
              Hashtbl.add winners command.Command.name command.Command.origin;
              command
          | Some by -> { command with Command.status = Command.Shadowed { by } }
          )
      | _ -> command)
    candidates

(* [stdenv] and [root] are the read authority [expand] resolves [@file]
   references against: the workspace captured at [load]. Discovery is complete
   after [load]; only [expand] re-observes the filesystem, through them. *)
type t = {
  enabled : bool;
  trust_restricted : bool;
  commands : Command.t list;
  stdenv : Eio_unix.Stdenv.base;
  root : Lpath.Abs.t;
}

let enabled t = t.enabled
let commands t = t.commands

let find_active t name =
  List.find_map
    (fun (command : Command.t) ->
      match command.Command.status with
      | Command.Active content
        when match command.Command.valid_name with
             | Some n -> Name.equal n name
             | None -> false ->
          Some content
      | _ -> None)
    t.commands

let load ~stdenv ~config ~trusted ~root ~user_config_file =
  let get field = Mentat_config.Resolved.get field config in
  if not (get Mentat_config.Field.commands_enabled) then
    { enabled = false; trust_restricted = false; commands = []; stdenv; root }
  else
    let fs = Eio.Stdenv.fs stdenv in
    let project = get Mentat_config.Field.commands_project in
    let compat = get Mentat_config.Field.commands_compat in
    let project_gate = if project then None else Some `Project in
    let compat_gate =
      if not project then Some `Project
      else if not compat then Some `Compat
      else None
    in
    let user_dir =
      match Lpath.Abs.parent user_config_file with
      | Some dir -> dir
      | None -> user_config_file
    in
    let candidates =
      (if trusted then
         walk_root ~fs ~kind:Command.Project ~gate:project_gate
           (under root [ ".mentat"; "commands" ])
         @ walk_root ~fs ~kind:Command.Compat_agents ~gate:compat_gate
             (under root [ ".agents"; "commands" ])
         @ walk_root ~fs ~kind:Command.Compat_claude ~gate:compat_gate
             (under root [ ".claude"; "commands" ])
       else [])
      @ walk_root ~fs ~kind:Command.User ~gate:None
          (under user_dir [ "commands" ])
    in
    let disabled_names =
      get Mentat_config.Field.commands_disabled
      |> List.filter_map (fun raw -> Name.of_string raw |> Result.to_option)
    in
    let commands =
      apply_shadowing (apply_config_disable disabled_names candidates)
    in
    { enabled = true; trust_restricted = not trusted; commands; stdenv; root }

(* Resolve one [@path] against the workspace [t] snapshotted, through the same
   contained authority discovery uses. Resolution needs a trusted workspace: an
   untrusted checkout's file bytes are exactly the injection [expand]'s trust
   gate keeps out of a turn. [Abs.resolve] clamps [..] at [root] and rejects an
   absolute [path]; [Io.observe] then rejects a symlink that escapes [root]. *)
let resolve_file t path =
  if t.trust_restricted then Error { path; reason = "workspace is not trusted" }
  else
    let fs = Eio.Stdenv.fs t.stdenv in
    match Lpath.Abs.resolve t.root path with
    | Error error -> Error { path; reason = Lpath.Error.message error }
    | Ok file_abs -> (
        match Io.observe ~fs ~root:t.root file_abs with
        | Io.Missing -> Error { path; reason = "no such file" }
        | Io.Escapes ->
            Error { path; reason = "resolves outside the workspace" }
        | Io.Found (Io.Directory | Io.Other) ->
            Error { path; reason = "not a regular file" }
        | Io.Failed message -> Error { path; reason = message }
        | Io.Found Io.Regular -> (
            match Io.read_file ~fs ~max_bytes:Io.read_limit file_abs with
            | Error message -> Error { path; reason = message }
            | Ok raw -> Ok (embed_bytes raw)))

let expand t ~name ~arguments =
  match find_active t name with
  | None -> Error (`Unknown (Name.to_string name))
  | Some content -> (
      match
        expand_body ~resolve:(resolve_file t) ~body:content.Command.body
          ~arguments
      with
      | Error error -> Error (`File_unresolved error)
      | Ok text ->
          if String.equal (String.trim text) "" then Ok []
          else Ok [ Mentat_llm.Content.text text ])

let warnings t =
  let trust_warnings =
    if t.trust_restricted then
      [ "project commands disabled: workspace is not trusted" ]
    else []
  in
  let unsupported_key key =
    String.equal key "model" || String.equal key "allowed-tools"
  in
  let command_warnings =
    List.concat_map
      (fun (command : Command.t) ->
        let name = command.Command.name in
        match command.Command.status with
        | Command.Invalid invalid ->
            let reason =
              match invalid with
              | `Invalid_name _ ->
                  "a path segment is not lowercase letters, digits, and hyphens"
              | `Empty_body -> "template body is empty"
              | `Invalid_frontmatter message | `Unreadable message -> message
            in
            [
              Printf.sprintf "command %s (%s): %s" name command.Command.origin
                reason;
            ]
        | Command.Active { Command.ignored_keys = _ :: _ as keys; _ } -> (
            let unsupported = List.filter unsupported_key keys in
            let others = List.filter (fun k -> not (unsupported_key k)) keys in
            (match unsupported with
              | [] -> []
              | keys ->
                  [
                    Printf.sprintf
                      "command %s: frontmatter %s not supported in this version"
                      name (String.concat ", " keys);
                  ])
            @
            match others with
            | [] -> []
            | keys ->
                [
                  Printf.sprintf "command %s: ignored frontmatter keys: %s" name
                    (String.concat ", " keys);
                ])
        | Command.Active _ | Command.Shadowed _ | Command.Disabled _ -> [])
      t.commands
  in
  trust_warnings @ command_warnings
