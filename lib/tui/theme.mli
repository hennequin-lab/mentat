(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The design language for the TUI: color roles, the glyph vocabulary, and the
    brand marks.

    The glyph and brand-mark vocabularies are flat constants. The color roles
    are projected from the built-in [mentat-dark] {!Palette.default}, kept for
    the few app-owned marks and the odoc cross-references that name the built-in
    identity; every widget otherwise reads its colors from a threaded
    {!Palette.t}. The heap in particular is deliberately both a brand mark and
    the footer context meter — brand and telemetry are the same drawing. *)

(** {1:colors Color roles}

    [accent] is the single brand hue (Melange burnt amber): the lockup, the
    heap, the selection cursor, spinners, and active-input affordances.
    [success], [warning], and [error] are outcome colors and mean nothing else.
    [muted] is secondary text, [faint] tertiary text, and [rule] horizontal
    rules and quiet borders. The mode colors are state colors owned by the
    composer frame — never prose, never rules elsewhere, never outcomes. *)

val color_accent : Mosaic.Ansi.Color.t
(** [color_accent] is Melange burnt amber — the one brand hue, projected from
    {!Palette.default}. *)

val color_history : Mosaic.Ansi.Color.t
(** [color_history] is the teal of the history-search input mode: the [⌕] marker
    and its footer badge, nothing else. *)

val color_muted : Mosaic.Ansi.Color.t
(** [color_muted] is secondary text. *)

val color_faint : Mosaic.Ansi.Color.t
(** [color_faint] is tertiary text. *)

val color_rule : Mosaic.Ansi.Color.t
(** [color_rule] is horizontal rules and quiet borders. *)

val color_success : Mosaic.Ansi.Color.t
(** [color_success] is the positive outcome color. *)

val color_warning : Mosaic.Ansi.Color.t
(** [color_warning] is the caution outcome color. *)

val color_error : Mosaic.Ansi.Color.t
(** [color_error] is the failure outcome color. *)

val color_user_bg : Mosaic.Ansi.Color.t
(** [color_user_bg] is the background wash behind user-authored transcript
    lines. *)

(** The code-highlight hues. {!color_code_kw}, {!color_code_type},
    {!color_code_str}, and {!color_code_num} are the four tints of the shared
    code palette (see {!Code_highlight.style}); {!color_faint} carries comments.
    Keyword violet and type blue hold enough contrast to separate from a muted
    diff background, while all four sit below the outcome colors so no token in
    code can read as success, warning, or error. They tint code surfaces only —
    transcript fences and the review diff. *)

val color_code_kw : Mosaic.Ansi.Color.t
(** [color_code_kw] is the violet of keywords. *)

val color_code_type : Mosaic.Ansi.Color.t
(** [color_code_type] is the blue of types, which the OCaml grammar also uses
    for constructors and module names. *)

val color_code_str : Mosaic.Ansi.Color.t
(** [color_code_str] is the green of string literals. *)

val color_code_num : Mosaic.Ansi.Color.t
(** [color_code_num] is the amber of numeric literals. *)

val accent : Mosaic.Ansi.Style.t
(** [accent] is bold {!color_accent} text — the brand style. *)

val atom : Mosaic.Ansi.Style.t
(** [atom] is unbolded {!color_accent} text for app-owned tokens: file
    references, paste chunks, and inline slash commands. *)

val muted : Mosaic.Ansi.Style.t
(** [muted] is {!color_muted} text. *)

val faint : Mosaic.Ansi.Style.t
(** [faint] is {!color_faint} text. *)

val rule : Mosaic.Ansi.Style.t
(** [rule] is {!color_rule} text. *)

val bold : Mosaic.Ansi.Style.t
(** [bold] is bold default-foreground text. *)

val success : Mosaic.Ansi.Style.t
(** [success] is bold {!color_success} text. *)

val warning : Mosaic.Ansi.Style.t
(** [warning] is bold {!color_warning} text. *)

val error : Mosaic.Ansi.Style.t
(** [error] is bold {!color_error} text. *)

val user : Mosaic.Ansi.Style.t
(** [user] is the {!color_user_bg} background wash. *)

val thinking : Mosaic.Ansi.Style.t
(** [thinking] is muted italic — reasoning: the [∴] mark, the settled thought
    one-liner, and the all-muted reasoning body. *)

val running : Mosaic.Ansi.Style.t
(** [running] is unbolded {!color_accent}, the running [⏺] dot and spinner. It
    is the accent role stripped of blink: a running tool is the only accent dot
    on screen and it holds still. *)

val code_kw : Mosaic.Ansi.Style.t
(** [code_kw] is {!color_code_kw} text — the keyword tint of the shared code
    palette. *)

val code_type : Mosaic.Ansi.Style.t
(** [code_type] is {!color_code_type} text — the type tint of the shared code
    palette. *)

val code_str : Mosaic.Ansi.Style.t
(** [code_str] is {!color_code_str} text — the string tint of the shared code
    palette. *)

val code_num : Mosaic.Ansi.Style.t
(** [code_num] is {!color_code_num} text — the number tint of the shared code
    palette. *)

(** {1:glyphs Glyph vocabulary}

    Every surface draws its marks, cursors, and separators from here so the same
    idea always looks the same. *)

val selection_fg : Mosaic.Ansi.Color.t
(** [selection_fg] is the foreground of a highlighted list row — the accent
    color. Selection is the accent {!cursor} in this color plus the row's
    unstyled text taking it; the {{!section:colors} color roles} of styled cells
    (outcome status, muted facts) still win. Paired with {!selection_bg}. *)

val selection_bg : Mosaic.Ansi.Color.t
(** [selection_bg] is the background of a highlighted list row: the terminal
    default, i.e. no fill. Selection never paints a wash — it is marker and
    color only (no chrome, one accent). Every picker, dialog, and panel routes
    its widget selection colors through {!selection_fg} and this, so all
    selection surfaces highlight identically. *)

val cursor : string
(** [cursor] prefixes the composer prompt and the selected list row (["❯ "]),
    the sole marker for a highlighted row (see {!selection_fg}). *)

val cursor_blank : string
(** [cursor_blank] (["  "]) keeps an unselected list row aligned with a
    {!cursor}-marked one. *)

val separator : string
(** [separator] joins inline facts ([" · "]). *)

val v_separator : string
(** [v_separator] (["│"]) is the full-height rule between the review screen's
    two panes — the sanctioned glyph of the two-column waiver, that screen only.
*)

val tree_group : string
(** [tree_group] (["▾"]) heads a directory group row in the review nav. *)

val todo_pending : string
(** [todo_pending] (["[ ]"]) is the unreviewed mark. *)

val todo_done : string
(** [todo_done] (["[✓]"]) is the reviewed mark. *)

val problem : string
(** [problem] marks a problem line (["! "]). *)

val shell_marker : string
(** [shell_marker] (["!"]) replaces the prompt marker while the composer is in
    shell mode, drawn in {!warning}. *)

val history_marker : string
(** [history_marker] (["⌕"]) replaces the prompt marker while history search
    (ctrl+r) is active, drawn in {!color_history}. *)

val kind_file : string
(** [kind_file] (["+"]) keys a file row in the unified [@] completion list. *)

val own_answer : string
(** [own_answer] (["✎"]) heads the permanent "type your own answer" row of a
    question dialog — the inline escape a question always offers. *)

(** The transcript glyph cast: six marks, one meaning each. Each carries its
    color from the surface that draws it — the mark is the shape, the
    {{!section:colors} color role} is the state. *)

val tool : string
(** [tool] ([⏺]) keys the model acting: an assistant text block and every tool
    header. The dot alone is colored — {!running} while live, {!muted} once
    settled, {!error} on failure. *)

val thought : string
(** [thought] ([∴]) keys the model thinking, drawn in {!thinking}. *)

val watcher : string
(** [watcher] ([⊙]) keys the world speaking — a data notice from a watcher. *)

val interrupted : string
(** [interrupted] ([◌]) keys a user interruption, drawn in {!muted}. *)

val failed : string
(** [failed] ([✗]) keys a failure, drawn in {!error}. *)

val gutter : string
(** [gutter] ([⎿]) opens a tool result line under its header. *)

val waiting : string
(** [waiting] ([⋯]) heads the static working line when a dialog owns the
    keyboard — no motion. *)

val panel_boundary : string
(** [panel_boundary] ([▔], upper-eighth block) is the full-width row a panel
    draws where it replaces the composer region, deliberately unlike every [─]
    rule. It is drawn in the panel's frame color. *)

val spinner_frames : string array
(** [spinner_frames] is the braille spinner cycle, drawn in {!running}: the
    working line's turning glyph and every running-tool dot animation. A view
    advances one frame per tick. *)

val mode_plan : string
(** [mode_plan] is the plan-mode glyph ([⏸], pause). *)

val mode_review : string
(** [mode_review] is the review-mode glyph ([⏴], look back). *)

val heap : string
(** [heap] is the heap ([▂▄▆▄▂]): the standalone brand mark and the settled
    context meter. *)

(** {1:brand Brand}

    The lockup and the one sanctioned animation, the pour. A view renders these
    rows in {!accent} and never recolors, stretches, or repeats them. *)

val lockup : string list
(** [lockup] is the two-row wordmark, 23 columns wide, rendered in {!accent}:

    {v
    ▄▀▀ █▀▄ · ▄▀▀ ██▀   ·
    ▄██ █▀  █ ▀▄▄ █▄▄ ▂▄▆▄▂
    v}

    The i's tittle is a grain and a poured heap sits beside the name with one
    grain aloft. Facts sit to its right; the lockup earns no vertical clearance
    of its own. *)

type pour_frame = {
  grain : string;
      (** The 3-column grain slot, drawn on row 1 above the mound. *)
  mound : string;  (** The 5-column mound (the heap region of row 2). *)
}
(** One heap-region frame of the pour: both rows, so the grain can appear and
    vanish relative to the mound as a grain drops and lands. *)

val pour_frames : pour_frame array
(** [pour_frames] are the nine two-row renderings of the pour. A grain appears
    aloft, then vanishes as it lands and raises the mound one step: the mound
    grows through the pinned keyframes ["     "], ["  ▂  "], [" ▂▄▂ "],
    ["▂▄▄▄▂"], ["▂▄▆▄▂"] while the grain drops one grain at a time.
    [pour_frames.(8)] is the lockup's rest — the grain settled to the right
    ({!grain_aloft}) over the full {!heap} — so it matches {!lockup}
    byte-for-byte. A view renders one frame per ~80–150ms tick. *)

val grain_aloft : string
(** [grain_aloft] is the single falling grain ([·]): it drops through the pour
    and comes to rest as the lockup's aloft grain. *)

(** {1:palette Color palette} *)

module Palette : sig
  (** The resolved color roles for one run.

      A user theme overrides colors only; glyphs, layout, and the brand lockup's
      shape are never themable (the lockup renders in {!accent}, so its hue
      follows the accent role but its form is fixed). Every role is total:
      {!of_json} fills omitted roles from a chosen base, so a partial theme is
      valid. The palette is the sole source of color for every widget view, and
      hot-swappable at runtime; the module-level color constants above are the
      built-in identity {!default} reproduces. *)

  type t

  val default : t
  (** [default] is the [mentat-dark] Melange identity — the brand default and
      the single table the module-level color constants project from. *)

  (* Role accessors, one per themable color. *)

  val accent : t -> Mosaic.Ansi.Color.t
  val mode_plan : t -> Mosaic.Ansi.Color.t
  val mode_review : t -> Mosaic.Ansi.Color.t
  val muted : t -> Mosaic.Ansi.Color.t
  val faint : t -> Mosaic.Ansi.Color.t
  val rule : t -> Mosaic.Ansi.Color.t
  val success : t -> Mosaic.Ansi.Color.t
  val warning : t -> Mosaic.Ansi.Color.t
  val error : t -> Mosaic.Ansi.Color.t
  val history : t -> Mosaic.Ansi.Color.t
  val chip_fg : t -> Mosaic.Ansi.Color.t
  val user_bg : t -> Mosaic.Ansi.Color.t
  val overlay : t -> Mosaic.Ansi.Color.t
  val code_keyword : t -> Mosaic.Ansi.Color.t
  val code_type : t -> Mosaic.Ansi.Color.t
  val code_string : t -> Mosaic.Ansi.Color.t
  val code_number : t -> Mosaic.Ansi.Color.t

  val selection_fg : t -> Mosaic.Ansi.Color.t
  (** [selection_fg t] is the accent foreground marking a highlighted row. *)

  val selection_bg : t -> Mosaic.Ansi.Color.t
  (** [selection_bg t] is the highlighted-row background; the terminal default
      unless themed, keeping selection a mark rather than a fill. *)

  (** The diff group. Each accessor feeds exactly one {!Mosaic.Diff.theme} field
      built by {!diff_theme}, except the two [*_emphasis] colors, which are the
      word-run colors drawn as [line_spans] behind the bytes that actually
      changed on a modified line. Content foreground is background-distinguished
      only: added, removed, and context lines differ by background over one
      shared muted text style, so there is no per-side content-fg role.

      Every shipped preset draws the four fills — both backgrounds, both gutter
      backgrounds — and the two emphases from the same green and red, tinted for
      a dark or a light terminal: a reader recognizes an addition and a deletion
      by hue before reading a word. The two signs are each preset's own
      {!success} and {!error}, and {!diff_gutter_fg} its own {!faint}, so a [+]
      and a line number keep the preset's character. All nine are ordinary
      roles, so a user theme that wants other colours may set them. *)

  val diff_added_bg : t -> Mosaic.Ansi.Color.t
  val diff_removed_bg : t -> Mosaic.Ansi.Color.t
  val diff_added_sign : t -> Mosaic.Ansi.Color.t
  val diff_removed_sign : t -> Mosaic.Ansi.Color.t
  val diff_gutter_fg : t -> Mosaic.Ansi.Color.t
  val diff_added_gutter_bg : t -> Mosaic.Ansi.Color.t
  val diff_removed_gutter_bg : t -> Mosaic.Ansi.Color.t
  val diff_added_emphasis : t -> Mosaic.Ansi.Color.t
  val diff_removed_emphasis : t -> Mosaic.Ansi.Color.t

  (* Derived styles, matching the module-level style constants. *)

  val accent_style : t -> Mosaic.Ansi.Style.t
  val atom_style : t -> Mosaic.Ansi.Style.t
  val muted_style : t -> Mosaic.Ansi.Style.t
  val faint_style : t -> Mosaic.Ansi.Style.t
  val rule_style : t -> Mosaic.Ansi.Style.t
  val success_style : t -> Mosaic.Ansi.Style.t
  val warning_style : t -> Mosaic.Ansi.Style.t
  val error_style : t -> Mosaic.Ansi.Style.t
  val user_style : t -> Mosaic.Ansi.Style.t
  val thinking_style : t -> Mosaic.Ansi.Style.t
  val running_style : t -> Mosaic.Ansi.Style.t
  val code_keyword_style : t -> Mosaic.Ansi.Style.t
  val code_type_style : t -> Mosaic.Ansi.Style.t
  val code_string_style : t -> Mosaic.Ansi.Style.t
  val code_number_style : t -> Mosaic.Ansi.Style.t

  (** {2:diff_projections Diff themes}

      The three finished {!Mosaic.Diff.theme} values review and the permission
      dialog render, built from the diff roles so no surface rebuilds one by
      hand. Every field maps to a diff role or to a principled constant: the
      context and content backgrounds and the line-number background are [None]
      (no chrome). *)

  val diff_theme : t -> Mosaic.Diff.theme
  (** [diff_theme t] is the live diff theme: the add/del backgrounds, signs, and
      gutter backgrounds from the diff roles. *)

  val diff_theme_quieted : t -> Mosaic.Diff.theme
  (** [diff_theme_quieted t] is the reviewed-scope variant: the add/del
      backgrounds drop to the terminal default and the signs grey to {!muted},
      so settled content stops shouting. *)

  val diff_theme_dimmed : t -> Mosaic.Diff.theme
  (** [diff_theme_dimmed t] is the compose-dialog variant: the same picture at
      half brightness, backgrounds halved and signs greyed to {!faint}, so the
      diff reads as dimmed rather than losing its colors. *)

  module Diagnostic : sig
    type t
    (** One rejected theme entry. The offending role keeps its default, so the
        resolved palette stays total. *)

    val message : t -> string
    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
  end

  val of_json : base:t -> Jsont.json -> t * Diagnostic.t list
  (** [of_json ~base j] overlays [j]'s valid entries onto [base]. [j] must be a
      JSON object mapping a color role to a value — a [#rrggbb] hex, a named
      ANSI color, or ["default"] (terminal default). The reserved ["extends"]
      key, which names the base and is resolved by the executable before this
      call, is skipped without a diagnostic. Unknown roles (including glyph and
      layout keys, which are not themable), non-string values, and unparseable
      colors each become a {!Diagnostic.t} and keep [base]'s value for that
      role, so the returned palette is always total. A non-object [j] yields
      [base] and one diagnostic. *)
end

(** {1:presets Built-in presets} *)

module Preset : sig
  (** The shipped palettes, compiled in as OCaml values.

      Presets are values, not embedded JSON, because the crash screen must theme
      without I/O, static release binaries carry them, and listing presets needs
      no filesystem walk. The JSON {!Palette.of_json} grammar serves the
      user-file path only. Built-in names are the resolution floor: a user file
      overlays a name but never removes it. *)

  type appearance =
    | Dark
    | Light
        (** Whether a preset assumes a dark or light terminal background. The
            picker tags each row with it. *)

  type t = { name : string; appearance : appearance; palette : Palette.t }
  (** A named preset: its stable [name], its [appearance] tag, and the resolved
      [palette]. *)

  val all : t list
  (** [all] is every shipped preset: [mentat-dark] ({!Palette.default}),
      [mentat-light], and the six ports (solarized, gruvbox, catppuccin,
      tokyonight, nord, one-dark), each with a light variant where the upstream
      defines one. Built-ins come first. *)

  val find : string -> t option
  (** [find name] is the built-in preset named [name], or [None]. This is the
      base-resolution floor: an [extends] base and a same-named user overlay
      both look up their base here, never in the merged user namespace, so no
      chain or cycle can form. *)

  val names : string list
  (** [names] is every preset name in {!all} order. *)
end

val chip :
  palette:Palette.t -> color:Mosaic.Ansi.Color.t -> string -> _ Mosaic.t
(** [chip ~palette ~color label] is the filled chip: [label] with one padding
    space each side, drawn in [palette]'s {!Palette.chip_fg} on a [color]
    background. [color] is the frame color the chip's surface currently wears —
    {!color_rule} for a plain panel, a mode color for a dialog. One drawing is
    used for panel names, screen names, and the composer's mode chips. *)

(** {1:layout Layout helpers}

    Shared by the review screen's panes. *)

val default_rule_width : int
(** [default_rule_width] is the fallback panel width when none is supplied. *)

val panel_rule : palette:Palette.t -> ?width:int -> unit -> _ Mosaic.t
(** [panel_rule ~palette ?width ()] is the [─] top rule spanning [width] in the
    palette's {!Palette.rule} color. *)

val pad_right : int -> string -> string
(** [pad_right width value] right-pads [value] with spaces to [width]. *)
