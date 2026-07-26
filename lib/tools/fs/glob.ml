(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let name = "glob"
let default_limit = 100
let max_limit = 1_000
let max_ignore_file_bytes = 16 * 1024 * 1024

let json_object fields =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       fields)

let encode_json json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok text -> text
  | Error message -> invalid_arg ("could not encode glob JSON: " ^ message)

module Input = struct
  type sort = Path | Modified

  type t = {
    pattern : string;
    path : string option;
    offset : int option;
    limit : int option;
    sort : sort;
  }

  let sort_to_string = function Path -> "path" | Modified -> "modified"

  let sort_of_string = function
    | None | Some "path" -> Path
    | Some "modified" -> Modified
    | Some sort -> invalid_arg ("unknown sort: " ^ sort)

  let make ~pattern ~path ~offset ~limit ~sort =
    if String.is_empty pattern then invalid_arg "pattern must not be empty";
    if String.contains pattern '\x00' then
      invalid_arg "pattern must not contain NUL";
    (match path with
    | Some "" -> invalid_arg "path must not be empty"
    | Some path when String.contains path '\x00' ->
        invalid_arg "path must not contain NUL"
    | Some _ | None -> ());
    (match offset with
    | Some offset when offset < 1 -> invalid_arg "offset must be at least 1"
    | Some _ | None -> ());
    (match limit with
    | Some limit when limit < 1 -> invalid_arg "limit must be positive"
    | Some limit when limit > max_limit ->
        invalid_arg ("limit must be at most " ^ string_of_int max_limit)
    | Some _ | None -> ());
    { pattern; path; offset; limit; sort }

  let max_input_integer =
    Float.min 9_007_199_254_740_991. (float_of_int Int.max_int)

  (* [Jsont.int] accepts numeric strings and truncates fractions. Provider
     input instead follows JSON Schema's integer semantics. *)
  let exact_integer =
    let decode = function
      | Jsont.Number (value, _)
        when Float.is_integer value && Float.abs value <= max_input_integer ->
          int_of_float value
      | Jsont.Number _ | Jsont.Null _ | Jsont.Bool _ | Jsont.String _
      | Jsont.Array _ | Jsont.Object _ ->
          Jsont.Error.msg Jsont.Meta.none
            "expected an integer in JSON's safe integer range"
    in
    Jsont.map ~kind:"integer" ~dec:decode
      ~enc:(fun value -> Jsont.Json.int value)
      Jsont.json

  let object_codec =
    Jsont.Object.map ~kind:"glob input" (fun pattern path offset limit sort ->
        Mentat_tool.Codec.decode_invalid_arg (fun () ->
            make ~pattern ~path ~offset ~limit ~sort:(sort_of_string sort)))
    |> Jsont.Object.mem "pattern" Jsont.string ~enc:(fun input -> input.pattern)
    |> Jsont.Object.opt_mem "path" Jsont.string ~enc:(fun input -> input.path)
    |> Jsont.Object.opt_mem "offset" exact_integer ~enc:(fun input ->
        input.offset)
    |> Jsont.Object.opt_mem "limit" exact_integer ~enc:(fun input ->
        input.limit)
    |> Jsont.Object.opt_mem "sort" Jsont.string ~enc:(fun input ->
        Some (sort_to_string input.sort))
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict glob input" object_codec

  let schema_property kind description fields =
    json_object
      (("type", Jsont.Json.string kind)
      :: ("description", Jsont.Json.string description)
      :: fields)

  let schema =
    json_object
      [
        ("type", Jsont.Json.string "object");
        ( "properties",
          json_object
            [
              ( "pattern",
                schema_property "string"
                  "Glob for workspace-relative file paths, for example **/*.ml \
                   or **/*.{ts,tsx}."
                  [ ("minLength", Jsont.Json.int 1) ] );
              ( "path",
                schema_property "string"
                  "Workspace-relative or workspace-contained absolute \
                   directory root. Defaults to the logical workspace current \
                   directory."
                  [ ("minLength", Jsont.Json.int 1) ] );
              ( "offset",
                schema_property "integer"
                  "One-based first matching file. Defaults to 1."
                  [
                    ("minimum", Jsont.Json.int 1);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
              ( "limit",
                schema_property "integer"
                  "Maximum matching files returned. Defaults to 100."
                  [
                    ("minimum", Jsont.Json.int 1);
                    ("maximum", Jsont.Json.int max_limit);
                  ] );
              ( "sort",
                schema_property "string"
                  "Ordering: path, or newest modification time with path as \
                   tie-breaker. Defaults to path."
                  [
                    ( "enum",
                      Jsont.Json.list
                        [
                          Jsont.Json.string "path"; Jsont.Json.string "modified";
                        ] );
                  ] );
            ] );
        ("required", Jsont.Json.list [ Jsont.Json.string "pattern" ]);
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
  let effective_path input = Option.value input.path ~default:"."
  let effective_offset input = Option.value input.offset ~default:1
  let effective_limit input = Option.value input.limit ~default:default_limit

  let to_json input =
    let fields = [ ("pattern", Jsont.Json.string input.pattern) ] in
    let fields =
      match input.path with
      | None -> fields
      | Some path -> fields @ [ ("path", Jsont.Json.string path) ]
    in
    let fields =
      match input.offset with
      | None -> fields
      | Some offset -> fields @ [ ("offset", Jsont.Json.int offset) ]
    in
    let fields =
      match input.limit with
      | None -> fields
      | Some limit -> fields @ [ ("limit", Jsont.Json.int limit) ]
    in
    fields @ [ ("sort", Jsont.Json.string (sort_to_string input.sort)) ]
    |> json_object

  let continuation input ~path ~offset ~limit =
    {
      pattern = input.pattern;
      path = Some path;
      offset = Some offset;
      limit = Some limit;
      sort = input.sort;
    }
end

module Matcher = struct
  type character_set = { complement : bool; ranges : (char * char) list }

  type atom =
    | Literal of char
    | Any
    | Star
    | Recursive
    | Recursive_prefix
    | Character_set of character_set
    | Alternatives of atom list list

  type instruction =
    | Accept_state
    | Literal_state of char * int
    | Any_state of int
    | Character_set_state of character_set * int
    | Star_state of int
    | Recursive_state of int
    | Recursive_to_slash_state of int
    | Branch_state of int list

  type t = { program : instruction array; basename_only : bool; negated : bool }

  type parser = {
    source : string;
    mutable index : int;
    mutable saw_slash : bool;
  }

  exception Invalid_pattern of string

  let error parser message =
    raise_notrace
      (Invalid_pattern
         (Printf.sprintf "%s at byte %d" message (parser.index + 1)))

  let at_end parser = parser.index = String.length parser.source
  let current parser = parser.source.[parser.index]

  let take parser =
    let byte = current parser in
    parser.index <- parser.index + 1;
    if Char.equal byte '/' then parser.saw_slash <- true;
    byte

  let escaped parser =
    parser.index <- parser.index + 1;
    if at_end parser then error parser "trailing escape";
    take parser

  let class_character parser =
    if at_end parser then error parser "unclosed character class";
    if Char.equal (current parser) '\\' then escaped parser else take parser

  let character_set parser =
    parser.index <- parser.index + 1;
    let negated =
      if
        (not (at_end parser))
        && (Char.equal (current parser) '!' || Char.equal (current parser) '^')
      then begin
        parser.index <- parser.index + 1;
        true
      end
      else false
    in
    let rec ranges acc =
      if at_end parser then error parser "unclosed character class";
      if (not (List.is_empty acc)) && Char.equal (current parser) ']' then begin
        parser.index <- parser.index + 1;
        Character_set { complement = negated; ranges = List.rev acc }
      end
      else
        let first = class_character parser in
        if (not (at_end parser)) && Char.equal (current parser) '-' then
          begin if
            parser.index + 1 < String.length parser.source
            && not (Char.equal parser.source.[parser.index + 1] ']')
          then begin
            parser.index <- parser.index + 1;
            let last = class_character parser in
            if Char.code first > Char.code last then
              error parser "descending character-class range";
            ranges ((first, last) :: acc)
          end
          else ranges ((first, first) :: acc)
          end
        else ranges ((first, first) :: acc)
    in
    ranges []

  let rec sequence parser ~in_braces =
    let rec loop atoms =
      if at_end parser then
        if in_braces then error parser "unclosed brace expansion"
        else (List.rev atoms, `End)
      else
        match current parser with
        | ',' when in_braces ->
            parser.index <- parser.index + 1;
            (List.rev atoms, `Comma)
        | '}' when in_braces ->
            parser.index <- parser.index + 1;
            (List.rev atoms, `Close)
        | '{' ->
            parser.index <- parser.index + 1;
            begin match alternatives parser with
            | None -> loop atoms
            | Some alternatives -> loop (alternatives :: atoms)
            end
        | '[' -> loop (character_set parser :: atoms)
        | '\\' -> loop (Literal (escaped parser) :: atoms)
        | '?' ->
            parser.index <- parser.index + 1;
            loop (Any :: atoms)
        | '*' ->
            let first = parser.index in
            while (not (at_end parser)) && Char.equal (current parser) '*' do
              parser.index <- parser.index + 1
            done;
            let count = parser.index - first in
            let component_start =
              first = 0
              || match parser.source.[first - 1] with '/' -> true | _ -> false
            in
            let branch_component_start =
              component_start
              ||
              match parser.source.[first - 1] with
              | '{' | ',' -> true
              | _ -> false
            in
            if
              count = 2 && branch_component_start
              && (not (at_end parser))
              && Char.equal (current parser) '/'
            then begin
              ignore (take parser : char);
              loop (Recursive_prefix :: atoms)
            end
            else if count = 2 && component_start && at_end parser then
              loop (Recursive :: atoms)
            else loop (Star :: atoms)
        | _ -> loop (Literal (take parser) :: atoms)
    in
    loop []

  and alternatives parser =
    let rec loop alternatives =
      let alternative, stop = sequence parser ~in_braces:true in
      match stop with
      | `Comma -> loop (alternative :: alternatives)
      | `Close ->
          let alternatives =
            alternative :: alternatives
            |> List.rev
            |> List.filter (fun alternative -> not (List.is_empty alternative))
          in
          if List.is_empty alternatives then None
          else Some (Alternatives alternatives)
      | `End -> assert false
    in
    loop []

  let parse source =
    let parser = { source; index = 0; saw_slash = false } in
    let atoms, stop = sequence parser ~in_braces:false in
    match stop with
    | `End -> (atoms, parser.saw_slash)
    | `Comma | `Close -> assert false

  let compile ?(negation = true) source =
    let negated, source =
      if negation && String.length source > 0 && Char.equal source.[0] '!' then
        (true, String.sub source 1 (String.length source - 1))
      else (false, source)
    in
    if String.is_empty source && negated then
      Ok
        {
          program = [| Star_state 1; Accept_state |];
          basename_only = true;
          negated = true;
        }
    else if String.is_empty source then
      Error "glob pattern has no positive body"
    else
      let source, anchored =
        if Char.equal source.[0] '/' then
          (String.sub source 1 (String.length source - 1), true)
        else (source, false)
      in
      match parse source with
      | exception Invalid_pattern message -> Error message
      | atoms, saw_slash ->
          let instructions = ref [] in
          let next_instruction = ref 0 in
          let emit instruction =
            let index = !next_instruction in
            incr next_instruction;
            instructions := instruction :: !instructions;
            index
          in
          let rec compile_sequence atoms next =
            match atoms with
            | [] -> next
            | atom :: atoms -> compile_atom atom (compile_sequence atoms next)
          and compile_atom atom next =
            match atom with
            | Literal byte -> emit (Literal_state (byte, next))
            | Any -> emit (Any_state next)
            | Character_set set -> emit (Character_set_state (set, next))
            | Star -> emit (Star_state next)
            | Recursive -> emit (Recursive_state next)
            | Recursive_prefix ->
                let loop = emit (Recursive_to_slash_state next) in
                emit (Branch_state [ next; loop ])
            | Alternatives alternatives ->
                emit
                  (Branch_state
                     (List.map
                        (fun alternative -> compile_sequence alternative next)
                        alternatives))
          in
          let accept = emit Accept_state in
          let start = compile_sequence atoms accept in
          let program = Array.of_list (List.rev !instructions) in
          if start = 0 then
            Ok { program; basename_only = not (anchored || saw_slash); negated }
          else
            (* Compilation emits continuations before predecessors, so the
               entry instruction is the final emitted index. Put one explicit
               branch at state zero to make matching independent of that order. *)
            let shifted =
              Array.init
                (Array.length program + 1)
                (fun index ->
                  if index = 0 then Branch_state [ start + 1 ]
                  else
                    let shift = function
                      | Accept_state -> Accept_state
                      | Literal_state (byte, next) ->
                          Literal_state (byte, next + 1)
                      | Any_state next -> Any_state (next + 1)
                      | Character_set_state (set, next) ->
                          Character_set_state (set, next + 1)
                      | Star_state next -> Star_state (next + 1)
                      | Recursive_state next -> Recursive_state (next + 1)
                      | Recursive_to_slash_state next ->
                          Recursive_to_slash_state (next + 1)
                      | Branch_state branches ->
                          Branch_state
                            (List.map (fun branch -> branch + 1) branches)
                    in
                    shift program.(index - 1))
            in
            Ok
              {
                program = shifted;
                basename_only = not (anchored || saw_slash);
                negated;
              }

  let set_matches set byte =
    let inside =
      List.exists
        (fun (first, last) ->
          Char.code first <= Char.code byte && Char.code byte <= Char.code last)
        set.ranges
    in
    if set.complement then not inside else inside

  let close program seeds =
    let seen = Array.make (Array.length program) false in
    let rec visit active = function
      | [] -> active
      | state :: states when seen.(state) -> visit active states
      | state :: states -> (
          seen.(state) <- true;
          match program.(state) with
          | Branch_state branches ->
              visit active (List.rev_append branches states)
          | Star_state next | Recursive_state next ->
              visit (state :: active) (next :: states)
          | Accept_state | Literal_state _ | Any_state _ | Character_set_state _
          | Recursive_to_slash_state _ ->
              visit (state :: active) states)
    in
    visit [] seeds

  let run program text =
    let active = ref (close program [ 0 ]) in
    String.iter
      (fun byte ->
        let seeds =
          List.fold_left
            (fun seeds state ->
              match program.(state) with
              | Literal_state (expected, next) when Char.equal byte expected ->
                  next :: seeds
              | Any_state next when not (Char.equal byte '/') -> next :: seeds
              | Character_set_state (set, next) when set_matches set byte ->
                  next :: seeds
              | Star_state _ when not (Char.equal byte '/') -> state :: seeds
              | Recursive_state _ -> state :: seeds
              | Recursive_to_slash_state next when Char.equal byte '/' ->
                  next :: state :: seeds
              | Recursive_to_slash_state _ -> state :: seeds
              | Accept_state | Literal_state _ | Any_state _
              | Character_set_state _ | Star_state _ | Branch_state _ ->
                  seeds)
            [] !active
        in
        active := close program seeds)
      text;
    List.exists
      (fun state ->
        match program.(state) with Accept_state -> true | _ -> false)
      !active

  let basename path =
    match String.rindex_opt path '/' with
    | None -> path
    | Some index -> String.sub path (index + 1) (String.length path - index - 1)

  let matches matcher path =
    let target = if matcher.basename_only then basename path else path in
    let matched = run matcher.program target in
    if matcher.negated then not matched else matched

  let excludes matcher path = matcher.negated && not (matches matcher path)
end

type ignore_pattern = {
  matcher : Matcher.t;
  excluded : bool;
  directory_only : bool;
}

type ignore_rule = { base : Mentat_workspace.Path.t; pattern : ignore_pattern }

let trim_cr line =
  let length = String.length line in
  if length > 0 && Char.equal line.[length - 1] '\r' then
    String.sub line 0 (length - 1)
  else line

(* Git and ripgrep treat the immediately preceding backslash as quoting a
   trailing space. Earlier backslashes belong to the pattern and do not alter
   that decision. *)
let trim_unescaped_trailing_spaces line =
  let rec boundary stop =
    if stop = 0 || not (Char.equal line.[stop - 1] ' ') then stop
    else if stop > 1 && Char.equal line.[stop - 2] '\\' then stop
    else boundary (stop - 1)
  in
  let stop = boundary (String.length line) in
  if stop = String.length line then line else String.sub line 0 stop

let ignore_pattern_of_line line =
  let line = line |> trim_cr |> trim_unescaped_trailing_spaces in
  if String.is_empty line || Char.equal line.[0] '#' then None
  else
    let escaped_marker =
      String.length line > 1
      && Char.equal line.[0] '\\'
      && (Char.equal line.[1] '#' || Char.equal line.[1] '!')
    in
    let excluded, line =
      if escaped_marker then (true, String.sub line 1 (String.length line - 1))
      else if Char.equal line.[0] '!' then
        (false, String.sub line 1 (String.length line - 1))
      else (true, line)
    in
    if String.is_empty line then None
    else
      let length = String.length line in
      let directory_only = Char.equal line.[length - 1] '/' in
      let line =
        if directory_only then String.sub line 0 (length - 1) else line
      in
      if String.is_empty line then None
      else
        match Matcher.compile ~negation:false line with
        | Error _ -> None
        | Ok matcher -> Some { matcher; excluded; directory_only }

let ignore_rule ~base line =
  Option.map (fun pattern -> { base; pattern }) (ignore_pattern_of_line line)

let parse_ignore_file ~base contents =
  contents |> String.split_on_char '\n' |> List.filter_map (ignore_rule ~base)

let is_ignored rules ~is_directory path =
  List.fold_left
    (fun ignored { base; pattern } ->
      if pattern.directory_only && not is_directory then ignored
      else
        match Mentat_workspace.Path.relativize ~root:base path with
        | None -> ignored
        | Some relative ->
            let relative = Lpath.Rel.to_string relative in
            if Matcher.matches pattern.matcher relative then pattern.excluded
            else ignored)
    false rules

let vcs_metadata_dirs = [ ".git"; ".svn"; ".hg"; ".bzr"; ".jj"; ".sl" ]

let is_vcs_metadata path =
  match Mentat_workspace.Path.basename path with
  | None -> false
  | Some name -> List.exists (String.equal name) vcs_metadata_dirs

type discovered = {
  path : Mentat_workspace.Path.t;
  relative : string;
  mtime : float;
}

type walk_error = Cancelled | File_error of Mentat_workspace_io.File_error.t

let ignore_file_names = [ ".gitignore"; ".ignore"; ".rgignore" ]

let read_local_rules workspace_io ~cancelled ~base entries inherited =
  let rec loop rules = function
    | [] -> Ok rules
    | name :: names -> (
        if cancelled () then Error Cancelled
        else
          match
            List.find_opt
              (fun (_, path) ->
                match Mentat_workspace.Path.basename path with
                | Some basename -> String.equal basename name
                | None -> false)
              entries
          with
          | None -> loop rules names
          | Some (`Regular_file, path) -> (
              match
                Mentat_workspace_io.File.load workspace_io path
                  ~max_bytes:max_ignore_file_bytes
              with
              | Ok contents ->
                  loop (rules @ parse_ignore_file ~base contents) names
              | Error (Mentat_workspace_io.File_error.Not_found _) ->
                  loop rules names
              | Error error -> Error (File_error error))
          | Some
              ( ( `Directory | `Symbolic_link | `Unknown | `Fifo
                | `Character_special | `Block_device | `Socket ),
                _ ) ->
              loop rules names)
  in
  loop inherited ignore_file_names

let ancestor_chain path =
  let rec ascend ancestors path =
    if Mentat_workspace.Path.is_root path then path :: ancestors
    else
      match Mentat_workspace.Path.parent path with
      | Some parent -> ascend (path :: ancestors) parent
      | None -> assert false
  in
  ascend [] path

let inherited_rules workspace_io ~cancelled root =
  let rec loop rules = function
    | [] -> assert false
    | [ _ ] -> Ok (Some rules)
    | directory :: (child :: _ as directories) ->
        if cancelled () then Error Cancelled
        else
          begin match
            Mentat_workspace_io.File.read_dir_entries workspace_io directory
          with
          | Error error -> Error (File_error error)
          | Ok entries ->
              begin match
                read_local_rules workspace_io ~cancelled ~base:directory entries
                  rules
              with
              | Error _ as error -> error
              | Ok rules ->
                  if
                    is_vcs_metadata child
                    || is_ignored rules ~is_directory:true child
                  then Ok None
                  else loop rules directories
              end
          end
  in
  loop [] (ancestor_chain root)

let workspace_relative path =
  path |> Mentat_workspace.Path.rel |> Lpath.Rel.to_string

let walk workspace_io ~cancelled ~root ~inherited matcher =
  let rec directories files = function
    | [] -> Ok files
    | (directory, inherited_rules) :: pending -> (
        if cancelled () then Error Cancelled
        else
          match
            Mentat_workspace_io.File.read_dir_entries workspace_io directory
          with
          | Error error -> Error (File_error error)
          | Ok entries -> (
              match
                read_local_rules workspace_io ~cancelled ~base:directory entries
                  inherited_rules
              with
              | Error _ as error -> error
              | Ok rules ->
                  let rec collect files pending = function
                    | [] -> directories files pending
                    | (kind, path) :: paths -> (
                        if cancelled () then Error Cancelled
                        else
                          match kind with
                          | `Directory ->
                              let pending =
                                if
                                  is_vcs_metadata path
                                  || is_ignored rules ~is_directory:true path
                                  || Matcher.excludes matcher
                                       (workspace_relative path)
                                then pending
                                else (path, rules) :: pending
                              in
                              collect files pending paths
                          | `Regular_file -> (
                              let relative = workspace_relative path in
                              if
                                is_ignored rules ~is_directory:false path
                                || not (Matcher.matches matcher relative)
                              then collect files pending paths
                              else
                                (* Only a file that survives every filter is
                                   worth a stat: the walk needs its modification
                                   time solely to order the results. *)
                                match
                                  Mentat_workspace_io.File.lstat workspace_io
                                    path
                                with
                                | Error error -> Error (File_error error)
                                | Ok stat ->
                                    let file =
                                      {
                                        path;
                                        relative;
                                        mtime = stat.Eio.File.Stat.mtime;
                                      }
                                    in
                                    collect (file :: files) pending paths)
                          | `Symbolic_link | `Unknown | `Fifo
                          | `Character_special | `Block_device | `Socket ->
                              collect files pending paths)
                  in
                  collect files pending entries))
  in
  if is_vcs_metadata root then Ok [] else directories [] [ (root, inherited) ]

module Enumeration = struct
  type error =
    [ `Cancelled
    | `File_error of Mentat_workspace_io.File_error.t
    | `Invalid_pattern of string ]

  let paths workspace_io ~cancelled ~root ~pattern =
    match Matcher.compile pattern with
    | Error message -> Error (`Invalid_pattern message)
    | Ok matcher -> (
        match inherited_rules workspace_io ~cancelled root with
        | Error Cancelled -> Error `Cancelled
        | Error (File_error error) -> Error (`File_error error)
        | Ok None -> Ok []
        | Ok (Some inherited) -> (
            match walk workspace_io ~cancelled ~root ~inherited matcher with
            | Error Cancelled -> Error `Cancelled
            | Error (File_error error) -> Error (`File_error error)
            | Ok files ->
                let paths =
                  files
                  |> List.sort (fun left right ->
                      String.compare left.relative right.relative)
                  |> List.map (fun file -> file.path)
                in
                Ok paths))
end

module Ignore = struct
  type rules = ignore_pattern list

  let empty = []

  let parse contents =
    contents |> String.split_on_char '\n'
    |> List.filter_map ignore_pattern_of_line

  let join earlier later = earlier @ later

  (* Subtree-pruning evaluation: every candidate is treated as a directory, so
     directory-only rules apply. A same-named regular file is over-pruned by a
     directory-only rule, which is the safe direction for a scanner deciding
     what not to descend into or report. *)
  let prunes rules path =
    match Lpath.Rel.to_string path with
    | "." -> false
    | relative ->
        List.fold_left
          (fun ignored pattern ->
            if Matcher.matches pattern.matcher relative then pattern.excluded
            else ignored)
          false rules
end

module Output = struct
  type t = {
    pattern : string;
    root : string;
    sort : Input.sort;
    paths : string list;
    offset : int;
    limit : int;
    total : int;
    next : Input.t option;
  }

  let complete output = Option.is_none output.next

  let text (output : t) =
    let buffer = Buffer.create 256 in
    Printf.bprintf buffer
      "pattern=%s root=%s files=%d/%d offset=%d limit=%d sort=%s status=%s\n"
      (encode_json (Jsont.Json.string output.pattern))
      output.root (List.length output.paths) output.total output.offset
      output.limit
      (Input.sort_to_string output.sort)
      (if complete output then "complete" else "partial");
    (match output.paths with
    | [] -> Buffer.add_string buffer "No files\n"
    | paths ->
        List.iter
          (fun path ->
            Buffer.add_string buffer path;
            Buffer.add_char buffer '\n')
          paths);
    (match output.next with
    | None -> ()
    | Some next ->
        Buffer.add_string buffer "next: ";
        Buffer.add_string buffer name;
        Buffer.add_char buffer ' ';
        Buffer.add_string buffer (encode_json (Input.to_json next));
        Buffer.add_char buffer '\n');
    Buffer.contents buffer

  let encode output =
    let semantic = Mentat_tools_output.Search.files ~total:output.total in
    Mentat_tools_output.Codec.encode Mentat_tools_output.Search.jsont
      ~text:(text output)
      ~truncated:(not (complete output))
      semantic
end

let compare_path left right = String.compare left.relative right.relative

let compare_modified left right =
  match Float.compare right.mtime left.mtime with
  | 0 -> compare_path left right
  | order -> order

let take count values =
  let rec loop taken count = function
    | _ when count = 0 -> List.rev taken
    | [] -> List.rev taken
    | value :: values -> loop (value :: taken) (count - 1) values
  in
  loop [] count values

let drop count values =
  let rec loop count values =
    if count = 0 then values
    else match values with [] -> [] | _ :: values -> loop (count - 1) values
  in
  loop count values

let page workspace_io input root files =
  let compare =
    match input.Input.sort with
    | Input.Path -> compare_path
    | Input.Modified -> compare_modified
  in
  let files = List.sort compare files in
  let total = List.length files in
  let offset = Input.effective_offset input in
  let limit = Input.effective_limit input in
  let selected = files |> drop (offset - 1) |> take limit in
  let returned = List.length selected in
  let has_more = offset <= total && offset + returned <= total in
  let cwd = Mentat_workspace_io.cwd workspace_io in
  let continuation_path =
    let path = Input.effective_path input in
    match Lpath.Abs.of_string path with
    | Error _ -> path
    | Ok _ -> Address.absolute_exn workspace_io root
  in
  let root_address = Address.display_relative workspace_io ~cwd root in
  let next =
    if has_more then
      Some
        (Input.continuation input ~path:continuation_path
           ~offset:(offset + returned) ~limit)
    else None
  in
  {
    Output.pattern = input.Input.pattern;
    root = root_address;
    sort = input.Input.sort;
    paths =
      List.map
        (fun file -> Address.display_relative workspace_io ~cwd file.path)
        selected;
    offset;
    limit;
    total;
    next;
  }

let interrupted () = Mentat_tool.Result.cancelled ()

let run workspace_io input ~cancelled =
  if cancelled () then interrupted ()
  else
    Permissions.with_resolved_run workspace_io (Input.effective_path input)
      (fun root ->
        begin match Mentat_workspace_io.File.lstat workspace_io root with
        | Error error -> Fs_error.failed error
        | Ok stat ->
            begin match stat.Eio.File.Stat.kind with
            | `Symbolic_link ->
                Mentat_tool.Result.failed `Invalid_input
                  (Mentat_workspace.Path.display root
                  ^ ": symlink search roots are not supported")
            | `Regular_file | `Unknown | `Fifo | `Character_special
            | `Block_device | `Socket ->
                Mentat_tool.Result.failed `Invalid_input
                  (Mentat_workspace.Path.display root ^ ": not a directory")
            | `Directory ->
                begin match Matcher.compile input.Input.pattern with
                | Error message ->
                    Mentat_tool.Result.failed `Invalid_input
                      ("invalid glob pattern: " ^ message)
                | Ok matcher ->
                    begin match
                      inherited_rules workspace_io ~cancelled root
                    with
                    | Error Cancelled -> interrupted ()
                    | Error (File_error error) -> Fs_error.failed error
                    | Ok None ->
                        Mentat_tool.Result.completed
                          ~output:(page workspace_io input root [])
                          ()
                    | Ok (Some inherited) ->
                        begin match
                          walk workspace_io ~cancelled ~root ~inherited matcher
                        with
                        | Error Cancelled -> interrupted ()
                        | Error (File_error error) -> Fs_error.failed error
                        | Ok files ->
                            Mentat_tool.Result.completed
                              ~output:(page workspace_io input root files)
                              ()
                        end
                    end
                end
            end
        end)

let permissions workspace_io input =
  Permissions.with_resolved workspace_io (Input.effective_path input)
    (fun path ->
      [
        Mentat_permission.Request.of_accesses ~source:name
          [ Mentat_permission.Access.path ~op:`Read path ];
      ])

let make workspace_io =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.glob
    ~input:Input.contract ~output:Output.encode
    ~permissions:(permissions workspace_io)
    ~run:(fun ~cancelled input -> run workspace_io input ~cancelled)
    ()
