(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let digest_string text = Mentat_digest.Content_ref.(to_token (of_contents text))

(* Cut [text] to at most [max_bytes] without splitting a UTF-8 sequence. *)
let take_utf8_prefix text max_bytes =
  if String.length text <= max_bytes then text
  else begin
    let stop = ref max_bytes in
    while !stop > 0 && Char.code text.[!stop] land 0xC0 = 0x80 do
      decr stop
    done;
    String.sub text 0 !stop
  end

(* JSON building and the hand-decoders behind the support projections. Both
   the skill and the catalog project to a stable support shape and decode back;
   the projection omits skill bodies and catalog text, so a decoded value
   carries those empty (see the respective [jsont] documentation). *)

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

module Skill = struct
  module Name = struct
    type t = string

    let max_bytes = 64

    let valid name =
      String.length name > 0
      && String.length name <= max_bytes
      && (match name.[0] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false)
      && String.for_all
           (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
           name

    let of_string name =
      if valid name then Ok name
      else
        Error
          (Printf.sprintf
             "%S is not a skill name (lowercase letters, digits, and hyphens; \
              at most %d bytes)"
             name max_bytes)

    let to_string name = name
    let equal = String.equal
    let compare = String.compare
    let pp = Format.pp_print_string
  end

  type kind = Project | Compat_agents | Compat_claude | User | Path | Builtin

  type content = {
    description : string;
    display_name : string option;
    text : string;
    body : string;
    bytes : int;
    digest : string;
    resources : string list;
    ignored_keys : string list;
  }

  type invalid =
    [ `Description_missing
    | `Description_too_long
    | `Invalid_frontmatter of string
    | `Unreadable of string ]

  type status =
    | Active of content
    | Shadowed of { by : string }
    | Disabled of [ `Project_skills | `Compat | `Builtin | `Config ]
    | Invalid of invalid

  type t = {
    name : Name.t;
    kind : kind;
    status : status;
    origin : string;
    dir : Lpath.Abs.t option;
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
    | Path -> "path"
    | Builtin -> "builtin"

  let kind_of_string = function
    | "project" -> Some Project
    | "compat_agents" -> Some Compat_agents
    | "compat_claude" -> Some Compat_claude
    | "user" -> Some User
    | "path" -> Some Path
    | "builtin" -> Some Builtin
    | _ -> None

  let state_string = function
    | Active _ -> "active"
    | Shadowed _ -> "shadowed"
    | Disabled _ -> "disabled"
    | Invalid _ -> "invalid"

  let reason_string = function
    | Active _ -> None
    | Shadowed _ -> Some "shadowed"
    | Disabled `Project_skills -> Some "project_skills_disabled"
    | Disabled `Compat -> Some "compat_disabled"
    | Disabled `Builtin -> Some "builtin_disabled"
    | Disabled `Config -> Some "config_disabled"
    | Invalid `Description_missing -> Some "description_missing"
    | Invalid `Description_too_long -> Some "description_too_long"
    | Invalid (`Invalid_frontmatter _) -> Some "invalid_frontmatter"
    | Invalid (`Unreadable _) -> Some "unreadable"

  let detail_string = function
    | Invalid (`Invalid_frontmatter message) | Invalid (`Unreadable message) ->
        Some message
    | Shadowed { by } -> Some by
    | Active _ | Disabled _
    | Invalid (`Description_missing | `Description_too_long) ->
        None

  let to_json t =
    let base =
      [
        json_mem "name" (Jsont.Json.string (Name.to_string t.name));
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
    let content =
      match t.status with
      | Active content -> (
          [
            json_mem "description" (Jsont.Json.string content.description);
            json_mem "bytes" (Jsont.Json.int content.bytes);
            json_mem "digest" (Jsont.Json.string content.digest);
            json_mem "resources"
              (Jsont.Json.list
                 (List.map
                    (fun value -> Jsont.Json.string value)
                    content.resources));
            json_mem "ignored_keys"
              (Jsont.Json.list
                 (List.map
                    (fun value -> Jsont.Json.string value)
                    content.ignored_keys));
          ]
          @
          match content.display_name with
          | None -> []
          | Some display ->
              [ json_mem "display_name" (Jsont.Json.string display) ])
      | Shadowed _ | Disabled _ | Invalid _ -> []
    in
    json_object (base @ reason @ detail @ content)

  (* Reconstruct a status from the projected fields. [text] and [body] are not
     carried on the wire and decode empty. *)
  let status_of_json json =
    match require_string "skill" "state" json with
    | "active" ->
        let description = require_string "skill" "description" json in
        Active
          {
            description;
            display_name = optional_string "skill" "display_name" json;
            text = "";
            body = "";
            bytes = require_int "skill" "bytes" json;
            digest = require_string "skill" "digest" json;
            resources = string_list "skill" "resources" json;
            ignored_keys = string_list "skill" "ignored_keys" json;
          }
    | "shadowed" ->
        Shadowed
          {
            by =
              Option.value ~default:"" (optional_string "skill" "detail" json);
          }
    | "disabled" -> (
        match optional_string "skill" "reason" json with
        | Some "project_skills_disabled" -> Disabled `Project_skills
        | Some "compat_disabled" -> Disabled `Compat
        | Some "builtin_disabled" -> Disabled `Builtin
        | Some "config_disabled" -> Disabled `Config
        | Some other -> decode_error ("unknown disabled reason: " ^ other)
        | None -> decode_error "disabled skill missing reason")
    | "invalid" -> (
        let detail () =
          Option.value ~default:"" (optional_string "skill" "detail" json)
        in
        match optional_string "skill" "reason" json with
        | Some "description_missing" -> Invalid `Description_missing
        | Some "description_too_long" -> Invalid `Description_too_long
        | Some "invalid_frontmatter" ->
            Invalid (`Invalid_frontmatter (detail ()))
        | Some "unreadable" -> Invalid (`Unreadable (detail ()))
        | Some other -> decode_error ("unknown invalid reason: " ^ other)
        | None -> decode_error "invalid skill missing reason")
    | other -> decode_error ("unknown skill state: " ^ other)

  let of_json json =
    match json with
    | Jsont.Object _ ->
        let name =
          match Name.of_string (require_string "skill" "name" json) with
          | Ok name -> name
          | Error message -> decode_error message
        in
        let kind =
          match kind_of_string (require_string "skill" "kind" json) with
          | Some kind -> kind
          | None -> decode_error "unknown skill kind"
        in
        {
          name;
          kind;
          status = status_of_json json;
          origin = require_string "skill" "origin" json;
          dir = None;
        }
    | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
    | Jsont.Array _ ->
        decode_error "skill must be an object"

  let jsont = Jsont.map ~kind:"skill" ~dec:of_json ~enc:to_json Jsont.json
end

module Catalog = struct
  type t = {
    text : string;
    digest : string;
    trimmed : Skill.Name.t list;
    names_only : bool;
  }

  let text t = t.text
  let bytes t = String.length t.text
  let digest t = t.digest
  let trimmed t = t.trimmed
  let names_only t = t.names_only

  let to_json t =
    json_object
      [
        json_mem "bytes" (Jsont.Json.int (bytes t));
        json_mem "digest" (Jsont.Json.string t.digest);
        json_mem "trimmed"
          (Jsont.Json.list
             (List.map
                (fun name -> Jsont.Json.string (Skill.Name.to_string name))
                t.trimmed));
        json_mem "names_only" (Jsont.Json.bool t.names_only);
      ]

  let of_json json =
    match json with
    | Jsont.Object _ ->
        let trimmed =
          string_list "skill catalog" "trimmed" json
          |> List.map (fun raw ->
              match Skill.Name.of_string raw with
              | Ok name -> name
              | Error message -> decode_error message)
        in
        {
          text = "";
          digest = require_string "skill catalog" "digest" json;
          trimmed;
          names_only = require_bool "skill catalog" "names_only" json;
        }
    | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
    | Jsont.Array _ ->
        decode_error "skill catalog must be an object"

  let jsont =
    Jsont.map ~kind:"skill catalog" ~dec:of_json ~enc:to_json Jsont.json

  let ellipsis = "\xE2\x80\xA6" (* U+2026, the visible trim marker *)
  let min_description_bytes = 16

  let entry_line ~name ~description =
    if String.equal description "" then "- " ^ name
    else "- " ^ name ^ ": " ^ description

  (* Render [(name, description, builtin)] entries under [budget] bytes.
     Builtin descriptions never trim; names are never dropped. Over budget,
     non-builtin descriptions share the remaining bytes equally and longer
     ones are cut at a UTF-8 boundary with a visible ellipsis. If even that
     floor does not fit, non-builtin descriptions drop entirely. *)
  let render ~budget entries =
    let line (name, description, _) = entry_line ~name ~description in
    let full = String.concat "\n" (List.map line entries) in
    let finish ?(trimmed = []) ?(names_only = false) text =
      { text; digest = digest_string text; trimmed; names_only }
    in
    if String.length full <= budget || entries = [] then finish full
    else
      let builtin, other =
        List.partition (fun (_, _, builtin) -> builtin) entries
      in
      let fixed_bytes =
        List.fold_left
          (fun total entry -> total + String.length (line entry) + 1)
          0 builtin
      in
      let name_overhead =
        List.fold_left
          (fun total (name, _, _) ->
            (* "- name: " plus the joining newline. *)
            total + String.length name + 5)
          0 other
      in
      let available = budget - fixed_bytes - name_overhead in
      let per_entry = if other = [] then 0 else available / List.length other in
      if per_entry < min_description_bytes then
        let text =
          entries
          |> List.map (fun (name, description, builtin) ->
              if builtin then entry_line ~name ~description
              else entry_line ~name ~description:"")
          |> String.concat "\n"
        in
        finish ~names_only:true text
      else
        let trimmed = ref [] in
        let text =
          entries
          |> List.map (fun (name, description, builtin) ->
              if builtin || String.length description <= per_entry then
                entry_line ~name ~description
              else begin
                trimmed := name :: !trimmed;
                let cut =
                  take_utf8_prefix description
                    (per_entry - String.length ellipsis)
                in
                entry_line ~name ~description:(cut ^ ellipsis)
              end)
          |> String.concat "\n"
        in
        finish ~trimmed:(List.rev !trimmed) text
end

type t = {
  enabled : bool;
  trust_restricted : bool;
  skills : Skill.t list;
  catalog : Catalog.t;
}

let enabled t = t.enabled
let skills t = t.skills
let catalog t = t.catalog

let find_active t name =
  List.find_map
    (fun (skill : Skill.t) ->
      match skill.Skill.status with
      | Skill.Active content when Skill.Name.equal skill.Skill.name name ->
          Some (skill, content)
      | _ -> None)
    t.skills

(* Discovery. Filesystem reads go through the lib's contained-read helper
   [Io]; one pass per root, following symlinks only while they stay in-root. *)

let skill_file = "SKILL.md"
let max_description_bytes = 1024

let is_consumed_metadata_key key =
  String.equal key "name" || String.equal key "description"

let duplicate_consumed_metadata_key keys =
  let rec loop seen = function
    | [] -> None
    | key :: keys ->
        if is_consumed_metadata_key key && List.exists (String.equal key) seen
        then Some key
        else
          let seen =
            if is_consumed_metadata_key key then key :: seen else seen
          in
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

(* Builds content facts for one read skill file. *)
let content_of_text ~resources text =
  match Frontmatter.parse text with
  | Error error ->
      Error (`Invalid_frontmatter (Frontmatter.Error.message error))
  | Ok header -> (
      match duplicate_consumed_metadata_key (Frontmatter.keys header) with
      | Some key ->
          Error
            (`Invalid_frontmatter
               (Printf.sprintf "frontmatter %s must appear only once" key))
      | None -> (
          match Frontmatter.field "description" header with
          | `Absent -> Error `Description_missing
          | `Not_string ->
              Error
                (`Invalid_frontmatter "frontmatter description must be a string")
          | `String description
            when String.length description > max_description_bytes ->
              Error `Description_too_long
          | `String description -> (
              match metadata_text "description" description with
              | Error _ as error -> error
              | Ok description -> (
                  let display_name =
                    match Frontmatter.string "name" header with
                    | None -> Ok None
                    | Some name -> (
                        match metadata_text "name" name with
                        | Error _ as error -> error
                        | Ok name -> Ok (Some name))
                  in
                  match display_name with
                  | Error _ as error -> error
                  | Ok display_name ->
                      let body = Frontmatter.body header in
                      let ignored_keys =
                        Frontmatter.keys header
                        |> List.filter (fun key ->
                            not (is_consumed_metadata_key key))
                      in
                      Ok
                        {
                          Skill.description;
                          display_name;
                          text;
                          body;
                          bytes = String.length text;
                          digest = digest_string text;
                          resources;
                          ignored_keys;
                        }))))

let child abs component =
  Lpath.Abs.add_component abs component |> Result.to_option

(* The skill directory's other files, sorted, each an in-directory regular
   file — the resources loadable through the skill tool. *)
let list_resources ~fs dir_abs =
  match Io.read_dir_names ~fs dir_abs with
  | Error _ -> []
  | Ok names ->
      names
      |> List.filter_map (fun name ->
          if String.equal name skill_file then None
          else
            match child dir_abs name with
            | None -> None
            | Some path -> (
                match Io.observe ~fs ~root:dir_abs path with
                | Io.Found Io.Regular -> Some name
                | Io.Found (Io.Directory | Io.Other)
                | Io.Missing | Io.Escapes | Io.Failed _ ->
                    None))
      |> List.sort String.compare

let read_candidate ~fs ~dir_abs ~skill_abs =
  match Io.read_file ~fs ~max_bytes:Io.read_limit skill_abs with
  | Error message -> Skill.Invalid (`Unreadable message)
  | Ok text -> (
      let resources = list_resources ~fs dir_abs in
      match content_of_text ~resources text with
      | Ok content -> Skill.Active content
      | Error invalid -> Skill.Invalid invalid)

(* A directory holding a [SKILL.md] is a skill named by that directory — its
   subdirectories are resources, never further skills — and a directory
   without one is a grouping folder searched in turn, so shared packs may
   organize skills in nested folders. Dot-directories are skipped; the depth
   bound keeps an in-root symbolic-link cycle finite. *)
let max_group_depth = 8

let rec directory_candidates ~fs ~root_abs ~kind ~gate ~depth dir_abs =
  match Io.read_dir_names ~fs dir_abs with
  | Error _ -> []
  | Ok names ->
      names |> List.sort String.compare
      |> List.concat_map (fun entry ->
          if entry = "" || entry.[0] = '.' then []
          else
            match child dir_abs entry with
            | None -> []
            | Some entry_abs ->
                classify_entry ~fs ~root_abs ~kind ~gate ~depth entry_abs entry)

and classify_entry ~fs ~root_abs ~kind ~gate ~depth dir_abs entry =
  let candidate status =
    match Skill.Name.of_string entry with
    | Error _ -> [] (* not a candidate, like misnamed directories *)
    | Ok name ->
        let origin = Lpath.Abs.to_string dir_abs in
        [ { Skill.name; kind; status; origin; dir = Some dir_abs } ]
  in
  match child dir_abs skill_file with
  | None -> []
  | Some skill_abs -> (
      match Io.observe ~fs ~root:root_abs skill_abs with
      | Io.Missing ->
          (* No SKILL.md: possibly a grouping folder, searched while it stays
             a directory in-root and within the depth bound. *)
          if depth >= max_group_depth then []
          else (
            match Io.observe ~fs ~root:root_abs dir_abs with
            | Io.Found Io.Directory ->
                directory_candidates ~fs ~root_abs ~kind ~gate
                  ~depth:(depth + 1) dir_abs
            | Io.Found (Io.Regular | Io.Other)
            | Io.Missing | Io.Escapes | Io.Failed _ ->
                [])
      | Io.Found Io.Regular -> (
          match gate with
          | Some disabled -> candidate (Skill.Disabled disabled)
          | None -> candidate (read_candidate ~fs ~dir_abs ~skill_abs))
      | Io.Found (Io.Directory | Io.Other) ->
          [] (* SKILL.md is not a regular file *)
      | Io.Escapes ->
          candidate
            (Skill.Invalid
               (`Unreadable
                  (skill_file ^ " resolves outside the skill directory")))
      | Io.Failed message -> candidate (Skill.Invalid (`Unreadable message)))

let filesystem_root ~fs ~kind ~gate root_abs =
  directory_candidates ~fs ~root_abs ~kind ~gate ~depth:0 root_abs

let builtin_skills ~builtins ~origin ~gate =
  List.filter_map
    (fun (entry, text) ->
      match Skill.Name.of_string entry with
      | Error _ -> None (* the generator already enforces the grammar *)
      | Ok name -> (
          let make status =
            Some
              { Skill.name; kind = Skill.Builtin; status; origin; dir = None }
          in
          match gate with
          | Some disabled -> make (Skill.Disabled disabled)
          | None -> (
              match content_of_text ~resources:[] text with
              | Ok content -> make (Skill.Active content)
              | Error invalid -> make (Skill.Invalid invalid))))
    builtins

(* Per-skill config disable ([skills.disabled]). Every candidate whose name is
   listed becomes [Disabled `Config], before shadowing, so no lower-precedence
   candidate of the name is promoted to active in its place. Listed names that
   match no candidate are inert. *)
let apply_config_disable disabled candidates =
  match disabled with
  | [] -> candidates
  | disabled ->
      List.map
        (fun (skill : Skill.t) ->
          if List.exists (Skill.Name.equal skill.Skill.name) disabled then
            { skill with Skill.status = Skill.Disabled `Config }
          else skill)
        candidates

(* Shadowing: the first active candidate per name wins; invalid and disabled
   candidates never claim a name. *)
let apply_shadowing candidates =
  let winners = Hashtbl.create 16 in
  List.map
    (fun (skill : Skill.t) ->
      match skill.Skill.status with
      | Skill.Active _ -> (
          match Hashtbl.find_opt winners skill.Skill.name with
          | None ->
              Hashtbl.add winners skill.Skill.name skill.Skill.origin;
              skill
          | Some by -> { skill with Skill.status = Skill.Shadowed { by } })
      | _ -> skill)
    candidates

(* A configured search root whose real path lies inside an untrusted workspace
   would smuggle project skills past the trust gate. [Io.observe] resolves the
   candidate with [realpath] before testing containment, so a symlinked root is
   caught too. *)
let contained_in ~fs ~root path =
  match Io.observe ~fs ~root path with
  | Io.Found _ -> true
  | Io.Missing | Io.Escapes | Io.Failed _ -> false

let under base segments =
  List.fold_left
    (fun abs segment ->
      match Lpath.Abs.add_component abs segment with
      | Ok abs -> abs
      | Error _ -> abs)
    base segments

let load ~stdenv ~builtins ~config ~trusted ~root ~cwd ~user_config_file =
  let get field = Mentat_config.Resolved.get field config in
  if not (get Mentat_config.Field.skills_enabled) then
    {
      enabled = false;
      trust_restricted = false;
      skills = [];
      catalog = Catalog.render ~budget:0 [];
    }
  else
    let fs = Eio.Stdenv.fs stdenv in
    let project = get Mentat_config.Field.skills_project in
    let compat = get Mentat_config.Field.skills_compat in
    let builtin = get Mentat_config.Field.skills_builtin in
    let project_gate = if project then None else Some `Project_skills in
    let compat_gate =
      if not project then Some `Project_skills
      else if not compat then Some `Compat
      else None
    in
    let builtin_gate = if builtin then None else Some `Builtin in
    let user_dir =
      match Lpath.Abs.parent user_config_file with
      | Some dir -> dir
      | None -> user_config_file
    in
    (* Relative [skills.paths] entries anchor on [cwd], matching every other
       discovery root, so a [--cwd] override does not split the path root off
       from the rest of the run. *)
    let path_roots =
      get Mentat_config.Field.skills_paths
      |> List.filter_map (fun text ->
          Lpath.Abs.resolve_any ~base:cwd text |> Result.to_option)
      |> List.filter (fun path -> trusted || not (contained_in ~fs ~root path))
    in
    let candidates =
      (if trusted then
         filesystem_root ~fs ~kind:Skill.Project ~gate:project_gate
           (under root [ ".mentat"; "skills" ])
         @ filesystem_root ~fs ~kind:Skill.Compat_agents ~gate:compat_gate
             (under root [ ".agents"; "skills" ])
         @ filesystem_root ~fs ~kind:Skill.Compat_claude ~gate:compat_gate
             (under root [ ".claude"; "skills" ])
       else [])
      @ filesystem_root ~fs ~kind:Skill.User ~gate:None
          (under user_dir [ "skills" ])
      @ List.concat_map
          (fun root_abs ->
            filesystem_root ~fs ~kind:Skill.Path ~gate:None root_abs)
          path_roots
      @ builtin_skills ~builtins ~origin:"builtin" ~gate:builtin_gate
    in
    (* [skills.disabled] carries raw names; keep the well-formed ones (a
       malformed name can match no discovered candidate). *)
    let disabled_names =
      get Mentat_config.Field.skills_disabled
      |> List.filter_map (fun raw ->
          Skill.Name.of_string raw |> Result.to_option)
    in
    let skills =
      apply_shadowing (apply_config_disable disabled_names candidates)
    in
    let entries =
      List.filter_map
        (fun (skill : Skill.t) ->
          match skill.Skill.status with
          | Skill.Active content ->
              Some
                ( Skill.Name.to_string skill.Skill.name,
                  content.Skill.description,
                  skill.Skill.kind = Skill.Builtin )
          | _ -> None)
        skills
    in
    let catalog =
      Catalog.render
        ~budget:(get Mentat_config.Field.skills_catalog_max_bytes)
        entries
    in
    { enabled = true; trust_restricted = not trusted; skills; catalog }

(* The skill tool. A per-call refreshing view of the snapshot: the handler
   serves current snapshot text and call-time resource reads. *)

module Tool_input = struct
  type t = { name : string; resource : string option }

  let optional_resource = function
    | None -> None
    | Some resource -> (
        match String.trim resource with
        | "" | "/" | "." | "./" -> None
        | resource -> Some resource)

  let codec =
    Jsont.Object.map ~kind:"skill input" (fun name resource ->
        { name; resource = optional_resource resource })
    |> Jsont.Object.mem "name" Jsont.string ~enc:(fun t -> t.name)
    |> Jsont.Object.opt_mem "resource" Jsont.string ~enc:(fun t -> t.resource)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let schema =
    json_object
      [
        json_mem "type" (Jsont.Json.string "object");
        json_mem "properties"
          (json_object
             [
               json_mem "name"
                 (json_object
                    [
                      json_mem "type" (Jsont.Json.string "string");
                      json_mem "description"
                        (Jsont.Json.string
                           "Name of the skill to load, from the available \
                            skills listing.");
                    ]);
               json_mem "resource"
                 (json_object
                    [
                      json_mem "type" (Jsont.Json.string "string");
                      json_mem "minLength" (Jsont.Json.int 1);
                      json_mem "description"
                        (Jsont.Json.string
                           "Optional path of a resource file to read, relative \
                            to the skill directory. Use only resource names \
                            listed by the skill guidance. Omit this field, or \
                            pass /, to load the skill guidance.");
                    ]);
             ]);
        json_mem "required" (Jsont.Json.list [ Jsont.Json.string "name" ]);
        json_mem "additionalProperties" (Jsont.Json.bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

let max_resource_bytes = 128 * 1024
let resource_truncation_marker = "\n[... resource truncated by mentat ...]"

(* A skill body is the model's largest single injection, so it is bounded like a
   resource read — at twice the cap, since the guidance is the primary content —
   rather than sent whole. *)
let max_body_bytes = 256 * 1024
let body_truncation_marker = "\n[... skill guidance truncated by mentat ...]"

let active_names t =
  List.filter_map
    (fun (skill : Skill.t) ->
      match skill.Skill.status with
      | Skill.Active _ -> Some (Skill.Name.to_string skill.Skill.name)
      | _ -> None)
    t.skills

(* The model-facing guidance the [skill] tool and [--skill] injections serve.
   The body is bounded: an over-cap body is cut at a UTF-8 boundary and flagged
   so the model knows it is truncated, never sent whole. *)
let skill_payload content =
  let body =
    if String.length content.Skill.body <= max_body_bytes then
      content.Skill.body
    else
      take_utf8_prefix content.Skill.body max_body_bytes
      ^ body_truncation_marker
  in
  match content.Skill.resources with
  | [] -> body
  | resources ->
      body ^ "\n\nResources (load with the skill tool's resource field):\n"
      ^ String.concat "\n" (List.map (fun name -> "- " ^ name) resources)

let read_resource ~fs (skill : Skill.t) resource =
  match skill.Skill.dir with
  | None ->
      Error
        (Printf.sprintf
           "skill %S has no resource files; call the skill tool without \
            resource to load its guidance"
           (Skill.Name.to_string skill.Skill.name))
  | Some dir_abs -> (
      match Lpath.Abs.resolve dir_abs resource with
      | Error error -> Error (Lpath.Error.message error)
      | Ok resource_abs -> (
          match Io.observe ~fs ~root:dir_abs resource_abs with
          | Io.Missing ->
              Error (Printf.sprintf "resource %S not found" resource)
          | Io.Escapes -> Error "path escapes root"
          | Io.Found (Io.Directory | Io.Other) ->
              Error (Printf.sprintf "resource %S is not a file" resource)
          | Io.Failed message -> Error message
          | Io.Found Io.Regular -> (
              match Io.read_file ~fs ~max_bytes:Io.read_limit resource_abs with
              | Error message -> Error message
              | Ok text ->
                  if String.length text <= max_resource_bytes then Ok text
                  else
                    Ok
                      (take_utf8_prefix text max_resource_bytes
                      ^ resource_truncation_marker))))

let tool_name = "skill"

(* The declaration is static so it stays a stable member of the boot-fixed tool
   catalog turn contracts seal; the catalog itself lives in the prelude
   ([catalog_fragment]), not in this description. The handler re-discovers per
   call so a skill added or edited mid-session is served from disk. *)
let tool_description =
  "Load a named skill's full guidance on demand. Available skills are listed \
   in your context."

let tool ~stdenv ~discover =
  let boot = discover () in
  if (not boot.enabled) || active_names boot = [] then None
  else
    let fs = Eio.Stdenv.fs stdenv in
    let run ~cancelled:_ (input : Tool_input.t) =
      let t = discover () in
      let fail kind message = Mentat_tool.Result.failed kind message in
      match Skill.Name.of_string input.Tool_input.name with
      | Error message -> fail `Invalid_input message
      | Ok name -> (
          match find_active t name with
          | None ->
              fail `Not_found
                (Printf.sprintf "unknown skill %S; available skills: %s"
                   input.Tool_input.name
                   (String.concat ", " (active_names t)))
          | Some (skill, content) -> (
              match input.Tool_input.resource with
              | None ->
                  Mentat_tool.Result.completed ~output:(skill_payload content)
                    ()
              | Some resource -> (
                  match read_resource ~fs skill resource with
                  | Ok text -> Mentat_tool.Result.completed ~output:text ()
                  | Error message -> fail `Not_found message)))
    in
    Some
      (Mentat_tool.make ~name:tool_name ~description:tool_description
         ~input:Tool_input.contract
         ~output:(fun s ->
           Mentat_tool.Output.make ~text:s ~json:(Jsont.Json.string s) ())
         ~run ())

let catalog_fragment t =
  if (not t.enabled) || active_names t = [] then None
  else
    Some
      (Mentat_llm.Message.developer
         (Mentat_prompts.Tools.skill ^ "\n\nAvailable skills:\n"
        ^ Catalog.text t.catalog))

let injections t ~names =
  let resolve raw =
    match Skill.Name.of_string raw with
    | Error message -> Error message
    | Ok name -> (
        match find_active t name with
        | None ->
            Error
              (Printf.sprintf "unknown skill %S; available skills: %s" raw
                 (match active_names t with
                 | [] -> "(none)"
                 | names -> String.concat ", " names))
        | Some (_, content) ->
            Ok
              (Printf.sprintf
                 "The user invoked the %S skill. Follow it for this task.\n\n%s"
                 raw (skill_payload content)))
  in
  List.fold_left
    (fun acc raw ->
      match acc with
      | Error _ as error -> error
      | Ok texts -> (
          match resolve raw with
          | Ok text -> Ok (text :: texts)
          | Error _ as error -> error))
    (Ok []) names
  |> Result.map List.rev

let warnings t =
  let trust_warnings =
    if t.trust_restricted then
      [ "project skills disabled: workspace is not trusted" ]
    else []
  in
  let skill_warnings =
    List.filter_map
      (fun (skill : Skill.t) ->
        let name = Skill.Name.to_string skill.Skill.name in
        match skill.Skill.status with
        | Skill.Invalid invalid ->
            let reason =
              match invalid with
              | `Description_missing -> "frontmatter has no description"
              | `Description_too_long ->
                  Printf.sprintf "description exceeds %d bytes"
                    max_description_bytes
              | `Invalid_frontmatter message | `Unreadable message -> message
            in
            Some
              (Printf.sprintf "skill %s (%s): %s" name skill.Skill.origin reason)
        | Skill.Active { Skill.ignored_keys = _ :: _ as keys; _ } ->
            Some
              (Printf.sprintf "skill %s: ignored frontmatter keys: %s" name
                 (String.concat ", " keys))
        | Skill.Active _ | Skill.Shadowed _ | Skill.Disabled _ -> None)
      t.skills
  in
  let catalog_warnings =
    if Catalog.names_only t.catalog then
      [
        "skill catalog over budget: descriptions dropped for non-builtin skills";
      ]
    else
      match Catalog.trimmed t.catalog with
      | [] -> []
      | trimmed ->
          [
            Printf.sprintf
              "skill catalog over budget: descriptions trimmed for %s"
              (String.concat ", " (List.map Skill.Name.to_string trimmed));
          ]
  in
  trust_warnings @ skill_warnings @ catalog_warnings
