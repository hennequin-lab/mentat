(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let normalize_utf_8 text =
  let buffer = Buffer.create (String.length text) in
  let rec loop index =
    if index < String.length text then begin
      let decoded = String.get_utf_8_uchar text index in
      let length = Uchar.utf_decode_length decoded in
      if Uchar.utf_decode_is_valid decoded then
        Buffer.add_substring buffer text index length
      else Buffer.add_utf_8_uchar buffer Uchar.rep;
      loop (index + length)
    end
  in
  loop 0;
  Buffer.contents buffer

module Error = struct
  type json_kind = [ `Array | `Boolean | `Null | `Number | `Object | `String ]

  type t =
    | Malformed_json of string
    | Missing_member of string
    | Wrong_kind of { path : string; expected : json_kind; found : json_kind }
    | Unsupported_version of float
    | Unsupported_type of string
    | Invalid_decimal_integer of { path : string; value : string }
    | Invalid_session_id
    | Empty_value of string
    | Invalid_span of { path : string; first : int; last : int }
    | Noncanonical_draft
    | Blank_draft

  let kind_message = function
    | `Array -> "array"
    | `Boolean -> "boolean"
    | `Null -> "null"
    | `Number -> "number"
    | `Object -> "object"
    | `String -> "string"

  let message = function
    | Malformed_json detail -> "malformed JSON: " ^ detail
    | Missing_member path -> "missing required member " ^ path
    | Wrong_kind { path; expected; found } ->
        Printf.sprintf "%s must be a JSON %s, found %s" path
          (kind_message expected) (kind_message found)
    | Unsupported_version version ->
        Printf.sprintf "unsupported history schema version %g" version
    | Unsupported_type type_ ->
        Printf.sprintf "unsupported history record type %S" type_
    | Invalid_decimal_integer { path; value } ->
        Printf.sprintf "%s is not a canonical in-range decimal integer: %S" path
          value
    | Invalid_session_id -> "history session_id is empty"
    | Empty_value path -> path ^ " must not be empty"
    | Invalid_span { path; first; last } ->
        Printf.sprintf "%s is not a valid half-open span: [%d,%d)" path first
          last
    | Noncanonical_draft ->
        "history draft metadata does not canonically describe its text"
    | Blank_draft -> "history draft expands to blank text"

  let pp ppf error = Format.pp_print_string ppf (message error)
end

let json_kind = function
  | Jsont.Null _ -> `Null
  | Jsont.Bool _ -> `Boolean
  | Jsont.Number _ -> `Number
  | Jsont.String _ -> `String
  | Jsont.Array _ -> `Array
  | Jsont.Object _ -> `Object

let child_path path name = path ^ "." ^ name

let object_members ~path = function
  | Jsont.Object (members, _) -> Ok members
  | json ->
      Error
        (Error.Wrong_kind { path; expected = `Object; found = json_kind json })

let required_member ~path name json =
  let open Result.Syntax in
  let* members = object_members ~path json in
  match Jsont.Json.find_mem name members with
  | Some (_, value) -> Ok value
  | None -> Error (Error.Missing_member (child_path path name))

let string_member ~path name json =
  let open Result.Syntax in
  let member_path = child_path path name in
  let* value = required_member ~path name json in
  match value with
  | Jsont.String (text, _) -> Ok text
  | value ->
      Error
        (Error.Wrong_kind
           { path = member_path; expected = `String; found = json_kind value })

let number_member ~path name json =
  let open Result.Syntax in
  let member_path = child_path path name in
  let* value = required_member ~path name json in
  match value with
  | Jsont.Number (number, _) -> Ok number
  | value ->
      Error
        (Error.Wrong_kind
           { path = member_path; expected = `Number; found = json_kind value })

let decimal_int_member ~path name json =
  let open Result.Syntax in
  let member_path = child_path path name in
  let* encoded = string_member ~path name json in
  match int_of_string_opt encoded with
  | Some value when String.equal encoded (string_of_int value) -> Ok value
  | Some _ | None ->
      Error
        (Error.Invalid_decimal_integer { path = member_path; value = encoded })

let rec map_all ~path decode index acc = function
  | [] -> Ok (List.rev acc)
  | item :: rest ->
      let open Result.Syntax in
      let* decoded = decode ~path:(Printf.sprintf "%s[%d]" path index) item in
      map_all ~path decode (index + 1) (decoded :: acc) rest

let optional_list_member ~path name decode json =
  let open Result.Syntax in
  let* members = object_members ~path json in
  let member_path = child_path path name in
  match Jsont.Json.find_mem name members with
  | None -> Ok []
  | Some (_, Jsont.Array (items, _)) ->
      map_all ~path:member_path decode 0 [] items
  | Some (_, value) ->
      Error
        (Error.Wrong_kind
           { path = member_path; expected = `Array; found = json_kind value })

module Entry = struct
  type t = {
    session : Mentat_session.Id.t;
    ts : int;
    draft : Draft.History_entry.t;
  }

  let session t = t.session
  let draft t = t.draft
  let text t = Draft.History_entry.text t.draft
  let canonical_draft draft = draft |> Draft.of_history_entry

  let of_draft ~session ~ts draft =
    match Option.map fst (Draft.submit (canonical_draft draft)) with
    | None -> None
    | Some submitted ->
        let draft =
          submitted.Draft.submitted_history_entry |> canonical_draft
          |> Draft.history_entry
        in
        Some { session; ts; draft }

  let span_json span =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "start")
          (Jsont.Json.string (Draft.Span.first span |> string_of_int));
        Jsont.Json.mem (Jsont.Json.name "end")
          (Jsont.Json.string (Draft.Span.last span |> string_of_int));
      ]

  let file_ref_json (span, file_ref) =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "span") (span_json span);
        Jsont.Json.mem (Jsont.Json.name "path")
          (Jsont.Json.string (Draft.File_ref.path file_ref));
        Jsont.Json.mem (Jsont.Json.name "label")
          (Jsont.Json.string (Draft.File_ref.label file_ref));
      ]

  let pending_paste_json (paste : Draft.pending_paste) =
    Jsont.Json.object'
      [
        Jsont.Json.mem
          (Jsont.Json.name "placeholder")
          (Jsont.Json.string paste.Draft.paste_placeholder);
        Jsont.Json.mem (Jsont.Json.name "text")
          (Jsont.Json.string paste.Draft.paste_text);
      ]

  let draft_json draft =
    let fields =
      [
        Jsont.Json.mem (Jsont.Json.name "text")
          (Jsont.Json.string (Draft.History_entry.text draft));
      ]
    in
    let fields =
      match Draft.History_entry.file_refs draft with
      | [] -> fields
      | file_refs ->
          fields
          @ [
              Jsont.Json.mem
                (Jsont.Json.name "file_refs")
                (Jsont.Json.list (List.map file_ref_json file_refs));
            ]
    in
    let fields =
      match Draft.History_entry.pending_pastes draft with
      | [] -> fields
      | pending_pastes ->
          fields
          @ [
              Jsont.Json.mem
                (Jsont.Json.name "pending_pastes")
                (Jsont.Json.list (List.map pending_paste_json pending_pastes));
            ]
    in
    Jsont.Json.object' fields

  let to_json t =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "schema_version") (Jsont.Json.int 1);
        Jsont.Json.mem (Jsont.Json.name "type")
          (Jsont.Json.string "mentat.tui.history_entry");
        Jsont.Json.mem
          (Jsont.Json.name "session_id")
          (Jsont.Json.string (Mentat_session.Id.to_string t.session));
        Jsont.Json.mem
          (Jsont.Json.name "timestamp")
          (Jsont.Json.string (string_of_int t.ts));
        Jsont.Json.mem (Jsont.Json.name "draft") (draft_json t.draft);
      ]

  let span_of_json ~path json =
    let open Result.Syntax in
    let* first = decimal_int_member ~path "start" json in
    let* last = decimal_int_member ~path "end" json in
    if first < 0 || last < first then
      Error (Error.Invalid_span { path; first; last })
    else Ok (Draft.Span.make ~first ~last)

  let file_ref_of_json ~path json =
    let open Result.Syntax in
    let* span_json = required_member ~path "span" json in
    let* span = span_of_json ~path:(child_path path "span") span_json in
    let* file_path = string_member ~path "path" json in
    let* label = string_member ~path "label" json in
    if String.equal file_path "" then
      Error (Error.Empty_value (child_path path "path"))
    else if String.equal label "" then
      Error (Error.Empty_value (child_path path "label"))
    else Ok (span, Draft.File_ref.make ~label file_path)

  let pending_paste_of_json ~path json =
    let open Result.Syntax in
    let* placeholder = string_member ~path "placeholder" json in
    let* text = string_member ~path "text" json in
    if String.equal placeholder "" then
      Error (Error.Empty_value (child_path path "placeholder"))
    else Ok { Draft.paste_placeholder = placeholder; paste_text = text }

  let draft_of_json ~path json =
    let open Result.Syntax in
    let* text = string_member ~path "text" json in
    let* file_refs =
      optional_list_member ~path "file_refs" file_ref_of_json json
    in
    let* pending_pastes =
      optional_list_member ~path "pending_pastes" pending_paste_of_json json
    in
    let draft = Draft.History_entry.make ~file_refs ~pending_pastes text in
    let canonical = canonical_draft draft |> Draft.history_entry in
    if Draft.History_entry.equal draft canonical then Ok draft
    else Error Error.Noncanonical_draft

  let of_json json =
    let open Result.Syntax in
    let path = "$" in
    let* version = number_member ~path "schema_version" json in
    if not (Float.equal version 1.) then
      Error (Error.Unsupported_version version)
    else
      let* type_ = string_member ~path "type" json in
      if not (String.equal type_ "mentat.tui.history_entry") then
        Error (Error.Unsupported_type type_)
      else
        let* session_text = string_member ~path "session_id" json in
        if String.equal session_text "" then Error Error.Invalid_session_id
        else
          let session = Mentat_session.Id.of_string session_text in
          let* ts = decimal_int_member ~path "timestamp" json in
          let* draft_json = required_member ~path "draft" json in
          let* draft = draft_of_json ~path:"$.draft" draft_json in
          match of_draft ~session ~ts draft with
          | None -> Error Error.Blank_draft
          | Some entry when Draft.History_entry.equal entry.draft draft ->
              Ok entry
          | Some _ -> Error Error.Noncanonical_draft
end

let encode entry =
  match Jsont_bytesrw.encode_string Jsont.json (Entry.to_json entry) with
  | Ok line -> line
  | Error message -> invalid_arg ("Mentat_tui.History.encode: " ^ message)

let decode line =
  match Jsont_bytesrw.decode_string Jsont.json line with
  | Error message -> Error (Error.Malformed_json message)
  | Ok json -> Entry.of_json json

type rejection = { line : int; error : Error.t }
type loaded = { entries : Entry.t list; rejected : rejection list }

let load contents =
  let lines = String.split_on_char '\n' contents in
  let entries, rejected =
    List.fold_left
      (fun (entries, rejected) (index, line) ->
        if String.equal (String.trim line) "" then (entries, rejected)
        else
          match decode line with
          | Ok entry -> (entry :: entries, rejected)
          | Error error -> (entries, { line = index + 1; error } :: rejected))
      ([], [])
      (List.mapi (fun index line -> (index, line)) lines)
  in
  { entries; rejected = List.rev rejected }

module Search = struct
  type t = {
    entries : Entry.t list;
    current : Mentat_session.Id.t option;
    query : string;
    selected : int;
  }

  let dedup entries =
    let seen = Hashtbl.create 128 in
    List.filter
      (fun entry ->
        let text = Entry.text entry in
        if Hashtbl.mem seen text then false
        else begin
          Hashtbl.add seen text ();
          true
        end)
      entries

  let make ?current ~entries () =
    { entries = dedup entries; current; query = ""; selected = 0 }

  let graphemes text =
    let items = ref [] in
    Matrix.Text.iter_graphemes
      (fun ~offset ~len -> items := String.sub text offset len :: !items)
      text;
    List.rev !items

  let fold_case grapheme = String.lowercase_ascii grapheme

  let is_subsequence ~needle text =
    let needle = Array.of_list (List.map fold_case (graphemes needle)) in
    let matched = ref 0 in
    Matrix.Text.iter_graphemes
      (fun ~offset ~len ->
        if
          !matched < Array.length needle
          && String.equal needle.(!matched)
               (String.sub text offset len |> fold_case)
        then incr matched)
      text;
    !matched = Array.length needle

  let ranked t =
    let query = String.trim t.query in
    let matching =
      if String.equal query "" then t.entries
      else
        List.filter
          (fun entry -> is_subsequence ~needle:query (Entry.text entry))
          t.entries
    in
    let current, previous =
      match t.current with
      | None -> ([], matching)
      | Some current ->
          List.partition
            (fun entry -> Mentat_session.Id.equal (Entry.session entry) current)
            matching
    in
    current @ previous

  let clamp lower upper value =
    if value < lower then lower else if value > upper then upper else value

  let with_query query t =
    let query = normalize_utf_8 query |> String.trim in
    let count = List.length (ranked { t with query }) in
    let selected =
      if String.equal query t.query then clamp 0 (max 0 (count - 1)) t.selected
      else 0
    in
    { t with query; selected }

  let refresh ?current ~entries t =
    let t = { t with entries = dedup entries; current } in
    let count = List.length (ranked t) in
    { t with selected = clamp 0 (max 0 (count - 1)) t.selected }

  let selected_entry t =
    Option.map Entry.draft (List.nth_opt (ranked t) t.selected)

  type msg = Select of int | Activate of int
  type event = Stay | Activated of Draft.History_entry.t

  let with_selected selected t =
    let count = List.length (ranked t) in
    let selected = if count = 0 then 0 else clamp 0 (count - 1) selected in
    { t with selected }

  let update message t =
    match message with
    | Select selected -> (with_selected selected t, Stay)
    | Activate selected -> (
        let t = with_selected selected t in
        match selected_entry t with
        | None -> (t, Stay)
        | Some entry -> (t, Activated entry))

  let move direction t =
    let count = List.length (ranked t) in
    if count = 0 then t
    else
      let offset = match direction with `Up -> -1 | `Down -> 1 in
      { t with selected = Option_list.wrap ~count t.selected offset }

  let first_line text =
    let rec find index =
      if index = String.length text then index
      else
        match String.get text index with
        | '\n' | '\r' -> index
        | _ -> find (index + 1)
    in
    let length = find 0 in
    String.sub text 0 length

  let title t =
    if String.equal (String.trim t.query) "" then "reverse-i-search:"
    else "reverse-i-search: " ^ t.query

  let row entry =
    Entry.text entry |> first_line |> Completion_list.segment
    |> Completion_list.row

  let view ~palette t =
    let header =
      Mosaic.text
        ~style:(Theme.Palette.muted_style palette)
        ~wrap:`None ~truncate:true ~flex_shrink:1.
        ~min_size:{ Mosaic.width = Mosaic.px 0; height = Mosaic.px 0 }
        (title t |> first_line)
    in
    let body =
      match (t.entries, ranked t) with
      | [], _ -> Completion_list.note ~palette "no prompt history"
      | _, [] -> Completion_list.note ~palette "no matching prompts"
      | _, entries ->
          Completion_list.view ~palette ~selected:t.selected
            ~on_select:(fun selected -> Select selected)
            ~on_activate:(fun selected -> Activate selected)
            (List.map row entries)
    in
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
      ~size:{ Mosaic.width = Mosaic.pct 100; height = Mosaic.auto }
      [ header; body ]
end
