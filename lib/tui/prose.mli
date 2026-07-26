(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Markdown views for assistant text and reasoning.

    Both views render borderless Markdown at the full width offered by their
    parent. They add no transcript marker or gutter; the surrounding transcript
    owns those pixels.

    Assistant code fences use settled-only Tree-sitter highlighting for the
    shipped OCaml, OCaml-interface, and JSON grammars. Unsupported or unlabelled
    fences remain readable as plain terminal text. Reasoning stays visually
    quiet and never uses the fence palette. *)

val view : ?streaming:bool -> palette:Theme.Palette.t -> string -> 'msg Mosaic.t
(** [view ?streaming md] renders assistant Markdown [md]. Inline code is
    accented; headings, lists, quotes, tables, tasks, and thematic breaks use
    the assistant prose palette.

    [streaming] defaults to [false]. When [true], incomplete trailing Markdown
    is tolerated and fenced code remains plain. When [false], fences tagged
    [ocaml] or [ml], [mli], and [json] are syntax-highlighted. Language tags are
    matched case-insensitively after removing surrounding whitespace; missing
    and unknown tags degrade to plain code. Deferring highlighting until
    settlement avoids repeatedly parsing a growing fence on the UI domain. *)

val thinking :
  ?streaming:bool -> palette:Theme.Palette.t -> string -> 'msg Mosaic.t
(** [thinking ?streaming md] renders reasoning Markdown [md] with muted text
    while preserving structural emphasis. [streaming] defaults to [false] and
    has the same incomplete-Markdown behavior as {!view}. Code fences remain
    muted and are not syntax-highlighted in either mode. *)
