(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

(* The complete shipped grammar ladder. Unknown languages get no colour rather
   than guessed token hues. [ml] is the transcript's fence alias for [ocaml];
   the review diff reaches the same ladder through [language_of_filename]. *)
let grammar language =
  match String.lowercase_ascii (String.trim language) with
  | "ocaml" | "ml" -> Some Tree_sitter_ocaml.highlight_ocaml
  | "mli" -> Some Tree_sitter_ocaml.highlight_interface
  | "json" -> Some Tree_sitter_json.highlight
  | _ -> None

let language_of_filename name =
  match String.lowercase_ascii (Filename.extension name) with
  | ".ml" -> Some "ocaml"
  | ".mli" -> Some "mli"
  | ".json" -> Some "json"
  | _ -> None

(* One palette for every code surface. The OCaml grammar tags constructors and
   module names as [type], so the type hue covers those too. Scope resolution
   falls back along dotted prefixes, so these family roots cover scopes such as
   [keyword.control] and [string.quoted]. *)
let style ~palette ~base =
  Syntax_style.make ~base
    [
      ("keyword", Theme.Palette.code_keyword_style palette);
      ("type", Theme.Palette.code_type_style palette);
      ("string", Theme.Palette.code_string_style palette);
      ("number", Theme.Palette.code_number_style palette);
      ("comment", Theme.Palette.faint_style palette);
    ]
