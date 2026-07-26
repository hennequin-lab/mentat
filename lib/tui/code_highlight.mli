(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Tree-sitter syntax highlighting for the code surfaces of the TUI.

    This is the single home for the shipped grammar ladder, the filename
    extension map, and the colour palette. The transcript's fenced code and the
    review diff both resolve highlighting through here, so a keyword, type,
    string, number, or comment wears the same hue wherever code appears. *)

val grammar : string -> (string -> (int * int * string) list) option
(** [grammar language] is the Tree-sitter highlighter for [language], or [None]
    when no shipped grammar covers it. [language] is matched case-insensitively
    after trimming surrounding whitespace: ["ocaml"] and its ["ml"] fence alias
    resolve to the OCaml grammar, ["mli"] to the OCaml-interface grammar, and
    ["json"] to the JSON grammar. The result maps [content] to
    [(start_byte, end_byte, scope)] triples. *)

val language_of_filename : string -> string option
(** [language_of_filename name] is the {!grammar} language keyed by [name]'s
    extension ([".ml"] to ["ocaml"], [".mli"] to ["mli"], [".json"] to
    ["json"]), or [None] for any other extension. The extension is matched
    case-insensitively. *)

val style :
  palette:Theme.Palette.t -> base:Mosaic.Ansi.Style.t -> Mosaic.Syntax_style.t
(** [style ~base] is the shared code palette layered over [base].

    Keywords, types (which the OCaml grammar also uses for constructors and
    module names), strings, numbers, and comments are tinted; identifiers,
    operators, and punctuation stay on [base] so code reads calm rather than
    fully polychrome. Keyword and type carry enough contrast to separate from a
    muted [base]; comments are faint. The tints come from {!Theme}'s code hues,
    which sit below the outcome colours so no token reads as success, warning,
    or error.

    [base] is the colour of untinted text, so callers pass the surface's own
    resting text style — terminal default for a transcript fence, the muted diff
    text for a review diff. The tinted hues are identical across surfaces; only
    [base] differs.

    Allocate once per [base] and keep the result: {!Mosaic.Code} and
    {!Mosaic.Diff} compare syntax settings by physical equality when deciding
    whether to re-highlight. *)
