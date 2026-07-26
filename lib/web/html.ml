(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The escaper is the whole XSS boundary: it replaces the five characters that
   can break out of text position or a double/single-quoted attribute value. *)
let escape_into buffer string =
  String.iter
    (fun char ->
      match char with
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&#39;"
      | char -> Buffer.add_char buffer char)
    string

module At = struct
  type t = Value of string * string | Present of string | Absent

  let v name value = Value (name, value)
  let bool name present = if present then Present name else Absent
  let id value = Value ("id", value)
  let class_ value = Value ("class", value)
  let href value = Value ("href", value)
  let type' value = Value ("type", value)
  let name value = Value ("name", value)
  let value value = Value ("value", value)
  let action value = Value ("action", value)
  let method' value = Value ("method", value)
  let data key value = Value ("data-" ^ key, value)

  let write buffer = function
    | Absent -> ()
    | Present name ->
        Buffer.add_char buffer ' ';
        Buffer.add_string buffer name
    | Value (name, value) ->
        Buffer.add_char buffer ' ';
        Buffer.add_string buffer name;
        Buffer.add_string buffer "=\"";
        escape_into buffer value;
        Buffer.add_char buffer '"'
end

type t =
  | Text of string
  | Raw of string
  | Element of { tag : string; at : At.t list; children : t list }
  | Splice of t list
  | Empty

(* CommonMark output and the inventory reach only this void set. A void tag
   self-closes and never renders children. *)
let is_void = function
  | "br" | "hr" | "img" | "input" | "meta" | "link" | "area" | "base" | "col"
  | "embed" | "source" | "track" | "wbr" ->
      true
  | _ -> false

module El = struct
  let txt string = Text string
  let v ?(at = []) tag children = Element { tag; at; children }
  let unsafe_raw string = Raw string
  let splice nodes = Splice nodes
  let void = Empty
  let section ?at children = v ?at "section" children
  let article ?at children = v ?at "article" children
  let aside ?at children = v ?at "aside" children
  let div ?at children = v ?at "div" children
  let span ?at children = v ?at "span" children
  let p ?at children = v ?at "p" children
  let pre ?at children = v ?at "pre" children
  let code ?at children = v ?at "code" children
  let ul ?at children = v ?at "ul" children
  let ol ?at children = v ?at "ol" children
  let li ?at children = v ?at "li" children
  let a ?at children = v ?at "a" children
  let form ?at children = v ?at "form" children
  let button ?at children = v ?at "button" children
  let textarea ?at children = v ?at "textarea" children
  let input ?at () = v ?at "input" []
  let label ?at children = v ?at "label" children
  let details ?at children = v ?at "details" children
  let summary ?at children = v ?at "summary" children
  let time ?at children = v ?at "time" children
  let blockquote ?at children = v ?at "blockquote" children
  let em ?at children = v ?at "em" children
  let strong ?at children = v ?at "strong" children
  let br ?at () = v ?at "br" []
  let hr ?at () = v ?at "hr" []
  let progress ?at children = v ?at "progress" children

  let h level ?at children =
    let level = if level < 1 then 1 else if level > 6 then 6 else level in
    v ?at ("h" ^ string_of_int level) children
end

let to_string node =
  let buffer = Buffer.create 256 in
  let rec write = function
    | Empty -> ()
    | Text string -> escape_into buffer string
    | Raw string -> Buffer.add_string buffer string
    | Splice nodes -> List.iter write nodes
    | Element { tag; at; children } ->
        Buffer.add_char buffer '<';
        Buffer.add_string buffer tag;
        List.iter (At.write buffer) at;
        if is_void tag then Buffer.add_string buffer " />"
        else begin
          Buffer.add_char buffer '>';
          List.iter write children;
          Buffer.add_string buffer "</";
          Buffer.add_string buffer tag;
          Buffer.add_char buffer '>'
        end
  in
  write node;
  Buffer.contents buffer
