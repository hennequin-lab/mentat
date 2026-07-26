(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

(* The prose style is monochrome except for inline code. Every Markdown key is
   explicit so changes to Mosaic's closed style vocabulary require review here.
   Each resolver is memoized on the palette's physical identity ({!Prims.per_palette})
   so Mosaic's physical-equality comparison sees a stable value every frame and
   re-derives only on a runtime theme swap. *)
let md_style =
  Prims.per_palette (fun palette : Markdown.style ->
      let open Markdown in
      function
      | Default -> Ansi.Style.default
      | Heading 1 -> Ansi.Style.make ~bold:true ~underline:true ()
      | Heading (2 | 3) -> Ansi.Style.make ~bold:true ()
      | Heading _ ->
          Ansi.Style.make ~fg:(Theme.Palette.muted palette) ~bold:true ()
      | Emphasis -> Ansi.Style.make ~italic:true ()
      | Strong -> Ansi.Style.make ~bold:true ()
      | Code_span -> Theme.Palette.atom_style palette
      | Code_block -> Ansi.Style.default
      | Link -> Ansi.Style.make ~underline:true ()
      | Image -> Theme.Palette.muted_style palette
      | Blockquote -> Theme.Palette.thinking_style palette
      | Thematic_break -> Theme.Palette.rule_style palette
      | List_marker -> Theme.Palette.muted_style palette
      | Strikethrough -> Ansi.Style.make ~strikethrough:true ()
      | Task_marker -> Ansi.Style.make ~bold:true ()
      | Table_border -> Theme.Palette.rule_style palette
      | Conceal_punctuation -> Theme.Palette.faint_style palette)

(* Reasoning preserves document structure but keeps every element muted or
   fainter. *)
let md_style_thinking =
  Prims.per_palette (fun palette : Markdown.style ->
      let open Markdown in
      function
      | Heading _ | Strong | Task_marker ->
          Ansi.Style.make ~fg:(Theme.Palette.muted palette) ~bold:true ()
      | Emphasis | Blockquote -> Theme.Palette.thinking_style palette
      | Strikethrough ->
          Ansi.Style.make
            ~fg:(Theme.Palette.muted palette)
            ~strikethrough:true ()
      | Thematic_break | Table_border -> Theme.Palette.rule_style palette
      | Conceal_punctuation -> Theme.Palette.faint_style palette
      | Default | Code_span | Code_block | Link | Image | List_marker ->
          Theme.Palette.muted_style palette)

(* Fenced code shares the one code palette ({!Code_highlight.style}) with the
   review diff, so a keyword or type wears the same hue everywhere. Its base is
   the terminal default: untinted tokens read as normal prose text. Memoized on
   the palette so the value Mosaic compares by physical equality stays stable. *)
let code_style =
  Prims.per_palette (fun palette ->
      Code_highlight.style ~palette ~base:Ansi.Style.default)

(* The tree-sitter parse is palette-independent, so its result is cached on the
   fence's (language, content) and reused across frames and across a live theme
   swap. Under the /theme picker's live preview a cursor move gives the palette a
   new identity; without this cache every settled fence would re-run tree-sitter
   on each arrow press. Here a preview step only re-applies the new palette's
   styling to the cached parse. The cache is cleared past a cap so a long session
   cannot accumulate settled fences unbounded. *)
let syntax_cache : (string, Syntax_highlight.t) Hashtbl.t = Hashtbl.create 64
let syntax_cache_cap = 512

let syntax_of ~language ~content =
  match language with
  | None -> None
  | Some language -> (
      match Code_highlight.grammar language with
      | None -> None
      | Some highlight ->
          let key = language ^ "\000" ^ content in
          Some
            (match Hashtbl.find_opt syntax_cache key with
            | Some triples -> triples
            | None ->
                if Hashtbl.length syntax_cache >= syntax_cache_cap then
                  Hashtbl.clear syntax_cache;
                let triples = Syntax_highlight.of_triples (highlight content) in
                Hashtbl.replace syntax_cache key triples;
                triples))

(* Mosaic compares [code_syntax] by physical equality, so the per-palette
   closure is memoized too. Allocating it inside [view] would re-render every
   settled fence on otherwise unchanged frames. *)
let code_syntax =
  Prims.per_palette (fun palette ~language ~content ->
      match syntax_of ~language ~content with
      | Some triples -> Some (Code.syntax ~style:(code_style palette) triples)
      | None -> None)

(* Streaming omits [code_syntax] entirely. Re-highlighting the growing fence
   from the start for every delta is quadratic work on the UI domain; colour is
   applied once the assistant block settles. *)
let view ?(streaming = false) ~palette md =
  if streaming then
    markdown ~md_style:(md_style palette) ~conceal:true ~streaming:true
      ~size:{ width = pct 100; height = auto }
      md
  else
    markdown ~md_style:(md_style palette) ~conceal:true
      ~code_syntax:(code_syntax palette)
      ~size:{ width = pct 100; height = auto }
      md

let thinking ?(streaming = false) ~palette md =
  markdown
    ~md_style:(md_style_thinking palette)
    ~conceal:true ~streaming
    ~size:{ width = pct 100; height = auto }
    md
