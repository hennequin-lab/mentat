(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* JSON Pointer paths. A location is built from the root down; a key or index is
   appended with the RFC 6901 escape ([~] -> [~0], [/] -> [~1]). *)

let escape key =
  let buf = Buffer.create (String.length key) in
  String.iter
    (function
      | '~' -> Buffer.add_string buf "~0"
      | '/' -> Buffer.add_string buf "~1"
      | c -> Buffer.add_char buf c)
    key;
  Buffer.contents buf

let child path key = path ^ "/" ^ escape key
let index path i = path ^ "/" ^ string_of_int i

(* The supported instance types the [type] keyword names. *)

type ty = Object | Array | String | Number | Integer | Boolean | Null

let ty_of_string = function
  | "object" -> Some Object
  | "array" -> Some Array
  | "string" -> Some String
  | "number" -> Some Number
  | "integer" -> Some Integer
  | "boolean" -> Some Boolean
  | "null" -> Some Null
  | _ -> None

let ty_name = function
  | Object -> "object"
  | Array -> "array"
  | String -> "string"
  | Number -> "number"
  | Integer -> "integer"
  | Boolean -> "boolean"
  | Null -> "null"

let sort_name = function
  | Jsont.Null _ -> "null"
  | Jsont.Bool _ -> "boolean"
  | Jsont.Number _ -> "number"
  | Jsont.String _ -> "string"
  | Jsont.Array _ -> "array"
  | Jsont.Object _ -> "object"

let matches_type value ty =
  match (ty, value) with
  | Object, Jsont.Object _ -> true
  | Array, Jsont.Array _ -> true
  | String, Jsont.String _ -> true
  | Boolean, Jsont.Bool _ -> true
  | Null, Jsont.Null _ -> true
  | Number, Jsont.Number _ -> true
  | Integer, Jsont.Number (f, _) -> Float.is_integer f
  | _ -> false

(* The parsed subset schema. Every field is [None] when its keyword is absent;
   an absent keyword constrains nothing. *)

type node = {
  types : ty list option;
  properties : (string * node) list option;
  required : string list option;
  additional_properties : bool option;
  items : node option;
  enum : Jsont.json list option;
  const : Jsont.json option;
}

type t = { root : node }

(* Load. *)

module Error = struct
  type t =
    | Unsupported_keyword of { keyword : string; path : string }
    | Malformed of { path : string; expected : string }

  let at_path path = if String.equal path "" then "" else " at " ^ path

  let message = function
    | Unsupported_keyword { keyword; path } ->
        Printf.sprintf "unsupported JSON Schema keyword %S%s" keyword
          (at_path path)
    | Malformed { path; expected } ->
        Printf.sprintf "malformed JSON Schema%s: expected %s" (at_path path)
          expected

  let pp ppf e = Format.pp_print_string ppf (message e)
end

let ignored_keywords =
  [
    "$schema";
    "$id";
    "title";
    "description";
    "$comment";
    "default";
    "examples";
    "readOnly";
    "writeOnly";
    "deprecated";
  ]

exception Reject of Error.t

let reject e = raise (Reject e)

let rec parse_node path json =
  match json with
  | Jsont.Object (members, _) ->
      let node =
        {
          types = None;
          properties = None;
          required = None;
          additional_properties = None;
          items = None;
          enum = None;
          const = None;
        }
      in
      List.fold_left
        (fun node (name, value) -> parse_member path node (fst name) value)
        node members
  | _ -> reject (Error.Malformed { path; expected = "a JSON Schema object" })

and parse_member path node keyword value =
  match keyword with
  | "type" -> { node with types = Some (parse_type path value) }
  | "properties" ->
      { node with properties = Some (parse_properties path value) }
  | "required" -> { node with required = Some (parse_required path value) }
  | "additionalProperties" ->
      { node with additional_properties = Some (parse_bool path keyword value) }
  | "items" ->
      { node with items = Some (parse_node (child path "items") value) }
  | "enum" -> { node with enum = Some (parse_array path keyword value) }
  | "const" -> { node with const = Some value }
  | k when List.mem k ignored_keywords -> node
  | keyword -> reject (Error.Unsupported_keyword { keyword; path })

and parse_type path value =
  let one path = function
    | Jsont.String (name, _) -> (
        match ty_of_string name with
        | Some ty -> ty
        | None ->
            reject
              (Error.Malformed
                 { path; expected = "a known JSON type name for \"type\"" }))
    | _ -> reject (Error.Malformed { path; expected = "a type name string" })
  in
  match value with
  | Jsont.String _ -> [ one path value ]
  | Jsont.Array (elements, _) -> List.map (one path) elements
  | _ ->
      reject
        (Error.Malformed
           {
             path;
             expected = "a type name or array of type names for \"type\"";
           })

and parse_properties path value =
  match value with
  | Jsont.Object (members, _) ->
      List.map
        (fun (name, sub) ->
          let key = fst name in
          (key, parse_node (child (child path "properties") key) sub))
        members
  | _ ->
      reject
        (Error.Malformed { path; expected = "an object for \"properties\"" })

and parse_required path value =
  match value with
  | Jsont.Array (elements, _) ->
      List.map
        (function
          | Jsont.String (s, _) -> s
          | _ ->
              reject
                (Error.Malformed
                   { path; expected = "an array of strings for \"required\"" }))
        elements
  | _ ->
      reject
        (Error.Malformed
           { path; expected = "an array of strings for \"required\"" })

and parse_bool path keyword value =
  match value with
  | Jsont.Bool (b, _) -> b
  | _ ->
      reject
        (Error.Malformed
           { path; expected = Printf.sprintf "a boolean for %S" keyword })

and parse_array path keyword value =
  match value with
  | Jsont.Array (elements, _) -> elements
  | _ ->
      reject
        (Error.Malformed
           { path; expected = Printf.sprintf "an array for %S" keyword })

let of_json json =
  match parse_node "" json with
  | root -> Ok { root }
  | exception Reject e -> Error e

(* Conformance. *)

module Violation = struct
  type t = { path : string; message : string }

  let pp ppf { path; message } =
    let where = if String.equal path "" then "<root>" else path in
    Format.fprintf ppf "%s: %s" where message
end

let violation path message = { Violation.path; message }
let ty_list_name tys = String.concat " or " (List.map ty_name tys)

(* Collect violations of [value] against [node] at instance [path], newest
   first onto [acc]. Applicable keywords only fire for the matching value sort,
   as JSON Schema specifies: [properties]/[required]/[additionalProperties] on
   objects, [items] on arrays; [type]/[enum]/[const] on any value. *)
let rec check node value path acc =
  let acc =
    match node.types with
    | Some tys when not (List.exists (matches_type value) tys) ->
        violation path
          (Printf.sprintf "expected %s but found %s" (ty_list_name tys)
             (sort_name value))
        :: acc
    | _ -> acc
  in
  let acc =
    match node.const with
    | Some c when not (Jsont.Json.equal c value) ->
        violation path "value does not equal the required const" :: acc
    | _ -> acc
  in
  let acc =
    match node.enum with
    | Some values when not (List.exists (Jsont.Json.equal value) values) ->
        violation path "value is not one of the permitted enum values" :: acc
    | _ -> acc
  in
  let acc =
    match value with
    | Jsont.Object (members, _) -> check_object node members path acc
    | _ -> acc
  in
  match (value, node.items) with
  | Jsont.Array (elements, _), Some item ->
      let _, acc =
        List.fold_left
          (fun (i, acc) element ->
            (i + 1, check item element (index path i) acc))
          (0, acc) elements
      in
      acc
  | _ -> acc

and check_object node members path acc =
  let acc =
    match node.required with
    | Some keys ->
        List.fold_left
          (fun acc key ->
            if Option.is_some (Jsont.Json.find_mem key members) then acc
            else
              violation path (Printf.sprintf "missing required property %S" key)
              :: acc)
          acc keys
    | None -> acc
  in
  let acc =
    match node.properties with
    | Some props ->
        List.fold_left
          (fun acc (key, sub) ->
            match Jsont.Json.find_mem key members with
            | Some (_, sub_value) -> check sub sub_value (child path key) acc
            | None -> acc)
          acc props
    | None -> acc
  in
  match node.additional_properties with
  | Some false ->
      let allowed =
        match node.properties with
        | Some props -> List.map fst props
        | None -> []
      in
      List.fold_left
        (fun acc name ->
          if List.mem name allowed then acc
          else
            violation (child path name)
              (Printf.sprintf
                 "unexpected property %S not permitted by additionalProperties"
                 name)
            :: acc)
        acc
        (Jsont.Json.object_names members)
  | Some true | None -> acc

let validate t value =
  match List.rev (check t.root value "" []) with
  | [] -> Ok ()
  | violations -> Error violations
