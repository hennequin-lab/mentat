(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic

(* [bold] is the one palette-independent style: bold default-foreground text. *)
let bold = Ansi.Style.make ~bold:true ()

(* Input-mode markers replace the ❯ marker glyph itself; the history and kind
   glyphs come from the interface's fixed glyph vocabulary. ◇ is reserved for MCP
   resources. *)
let cursor = "❯ "
let cursor_blank = "  "
let separator = " · "
let v_separator = "│"
let tree_group = "▾"
let todo_pending = "[ ]"
let todo_done = "[✓]"
let shell_marker = "!"
let history_marker = "⌕"
let kind_file = "+"
let own_answer = "✎"
let problem = "! "
let tool = "⏺"
let thought = "∴"
let watcher = "⊙"
let interrupted = "◌"
let failed = "✗"
let gutter = "⎿"
let waiting = "⋯"

(* The panel boundary: the upper-eighth block, deliberately unlike every [─]
   rule, marking where a panel
   replaces the composer region below the transcript. *)
let panel_boundary = "▔"

let spinner_frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let mode_plan = "⏸"
let mode_review = "⏴"
let heap = "▂▄▆▄▂"

(* MENTAT wordmark in the house letterform; trailing heap + grain kept so
   the pour still ends on the lockup byte-for-byte. This mark is the ratified
   brand lockup; www/ renders the same letterforms. *)
let lockup = [ "█▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·"; "█ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂" ]

type pour_frame = { grain : string; mound : string }

(* The pour: nine two-row heap-region frames. Each carries a 3-column grain slot (row 1, above the mound's centre)
   and the 5-column mound (row 2). A grain appears aloft, then vanishes as it
   lands and raises the mound one step, so the grain visibly drops one grain at a
   time while the mound builds empty → ▂ → ▂▄▂ → ▂▄▄▄▂ → ▂▄▆▄▂. The closing frame
   is the lockup's rest — the grain settled to the right ({!grain_aloft}) over the
   full {!heap} — so the pour ends on the lockup byte-for-byte. *)
let pour_frames =
  [|
    { grain = " · "; mound = "     " };
    { grain = "   "; mound = "  ▂  " };
    { grain = " · "; mound = "  ▂  " };
    { grain = "   "; mound = " ▂▄▂ " };
    { grain = "·  "; mound = " ▂▄▂ " };
    { grain = "   "; mound = "▂▄▄▄▂" };
    { grain = " · "; mound = "▂▄▄▄▂" };
    { grain = "   "; mound = "▂▄▆▄▂" };
    { grain = "  ·"; mound = "▂▄▆▄▂" };
  |]

let grain_aloft = "·"

(* A resolved color palette for one run: the sole source of color for every
   widget, which reads its roles from a threaded palette rather than the
   module-level constants below. {!Palette.default} is the [mentat-dark] preset
   and the single table those constants project from. Glyphs, layout, and the
   brand lockup's shape are not themable (the lockup renders in [accent], so its
   hue follows the accent role, but its form is fixed). Every role is total:
   {!Palette.of_json} fills omitted roles from a chosen base, so a partial theme
   is valid. *)
module Palette = struct
  type appearance = Dark | Light

  type t = {
    (* The terminal background the palette assumes. Not a themable role: the
       palette's author fixes it and an overlay inherits it, so a user file
       cannot flip the direction [dimmed] fades. *)
    appearance : appearance;
    accent : Ansi.Color.t;
    mode_plan : Ansi.Color.t;
    mode_review : Ansi.Color.t;
    muted : Ansi.Color.t;
    faint : Ansi.Color.t;
    rule : Ansi.Color.t;
    success : Ansi.Color.t;
    warning : Ansi.Color.t;
    error : Ansi.Color.t;
    history : Ansi.Color.t;
    chip_fg : Ansi.Color.t;
    user_bg : Ansi.Color.t;
    overlay : Ansi.Color.t;
    code_keyword : Ansi.Color.t;
    code_type : Ansi.Color.t;
    code_string : Ansi.Color.t;
    code_number : Ansi.Color.t;
    selection_bg : Ansi.Color.t;
    (* The diff group (9): each feeds one {!Mosaic.Diff.theme} field or the
       intra-line [line_spans] emphasis seam. Content foreground is
       background-distinguished only, so there is no per-side content-fg role. *)
    diff_added_bg : Ansi.Color.t;
    diff_removed_bg : Ansi.Color.t;
    diff_added_sign : Ansi.Color.t;
    diff_removed_sign : Ansi.Color.t;
    diff_gutter_fg : Ansi.Color.t;
    diff_added_gutter_bg : Ansi.Color.t;
    diff_removed_gutter_bg : Ansi.Color.t;
    diff_added_emphasis : Ansi.Color.t;
    diff_removed_emphasis : Ansi.Color.t;
  }

  (* Compile-time color literals for the shipped preset tables. A malformed hex
     is a transcription bug, so it fails loudly at module load rather than
     silently degrading to a default. *)
  let hex s =
    match Ansi.Color.of_hex s with
    | Some color -> color
    | None -> invalid_arg ("Theme: malformed preset hex " ^ s)

  (* Linear channel mix, [t] in [0,1]: [t = 0.] is [a], [t = 1.] is [b]. Both
     inputs are concrete preset hexes (never terminal default), so the RGB
     projection is always defined. *)
  let blend a b t =
    let ar, ag, ab = Ansi.Color.to_rgb a in
    let br, bg, bb = Ansi.Color.to_rgb b in
    let mix x y =
      int_of_float
        (Float.round ((float_of_int x *. (1. -. t)) +. (float_of_int y *. t)))
    in
    Ansi.Color.of_rgb (mix ar br) (mix ag bg) (mix ab bb)

  (* [mentat-dark], the brand default: Melange, a burnt-amber accent with a
     slate-blue plan hue. Every role spelled out — this table is the single
     source the module-level color constants below project from. *)
  let default =
    {
      appearance = Dark;
      accent = hex "#E39A3C";
      mode_plan = hex "#5CA9D6";
      mode_review = hex "#A98BE0";
      muted = hex "#98928A";
      faint = hex "#6C665F";
      rule = hex "#443F39";
      success = hex "#74B96A";
      warning = hex "#D9C24E";
      error = hex "#E5565B";
      history = hex "#48B0A0";
      chip_fg = hex "#191510";
      user_bg = hex "#38301F";
      overlay = hex "#16130E";
      code_keyword = hex "#B39BEA";
      code_type = hex "#6FB0D8";
      code_string = hex "#93B187";
      code_number = hex "#D8A96A";
      selection_bg = Ansi.Color.default;
      diff_added_bg = hex "#0A3418";
      diff_removed_bg = hex "#421616";
      diff_added_sign = hex "#74B96A";
      diff_removed_sign = hex "#E5565B";
      diff_gutter_fg = hex "#6C665F";
      diff_added_gutter_bg = hex "#072612";
      diff_removed_gutter_bg = hex "#2E1010";
      diff_added_emphasis = hex "#1C5232";
      diff_removed_emphasis = hex "#5E2020";
    }

  (* [mentat-light]: the same brand direction over a light terminal. *)
  let mentat_light =
    {
      appearance = Light;
      accent = hex "#B4741C";
      mode_plan = hex "#2E6FA6";
      mode_review = hex "#6E54BC";
      muted = hex "#6A645D";
      faint = hex "#8F897F";
      rule = hex "#DAD3C7";
      success = hex "#3D8F43";
      warning = hex "#977C1A";
      error = hex "#C0392E";
      history = hex "#2C8A7A";
      chip_fg = hex "#FBF7EE";
      user_bg = hex "#F3EAD6";
      overlay = hex "#FCF8EF";
      code_keyword = hex "#6E4CBE";
      code_type = hex "#2C6BA4";
      code_string = hex "#4C875A";
      code_number = hex "#9E6A2A";
      selection_bg = Ansi.Color.default;
      diff_added_bg = hex "#DBF0DC";
      diff_removed_bg = hex "#F6DADA";
      diff_added_sign = hex "#3D8F43";
      diff_removed_sign = hex "#C0392E";
      diff_gutter_fg = hex "#8F897F";
      diff_added_gutter_bg = hex "#CDE8CE";
      diff_removed_gutter_bg = hex "#F0CCCC";
      diff_added_emphasis = hex "#B6E0B8";
      diff_removed_emphasis = hex "#EEBFC0";
    }

  (* A port from an upstream palette. The upstream provides the brand, mode,
     outcome, and syntax hues directly; the mentat-only tiers (text tiers, rule,
     chip, overlay) derive from the neutral background and ink foreground by a
     fixed blend, so every port fills all 27 roles consistently.

     The diff fills — both backgrounds, both gutter backgrounds, both word-run
     emphases — come from [diff], the mentat table of the port's own terminal
     ({!default} for a dark port, {!mentat_light} for a light one), never from
     the upstream. An upstream "add" hue is routinely teal, olive, or blue, and
     tinting one toward a warm neutral lands in brown; a reader recognizes an
     addition and a deletion by hue before reading a word, so those fills stay
     green and red under every preset. The two signs are the exception: they are
     the [success] and [error] hues, which already mean green and red, so a
     preset keeps its own [+] and [-]. The gutter foreground stays the port's own
     [faint], so a line number reads against its own text tiers. All nine remain
     ordinary roles: a user theme can still set any of them.

     [diff] also fixes the port's [appearance]: naming the mentat table of the
     port's own terminal is already naming that terminal, so deriving the tag
     from it leaves no second source to drift out of agreement. *)
  let port ~diff ~neutral ~ink ~primary ~info ~accent ~success ~warning ~error
      ~keyword ~type_ ~string ~number =
    let neutral = hex neutral in
    let ink = hex ink in
    let faint = blend ink neutral 0.5 in
    {
      appearance = diff.appearance;
      accent = hex primary;
      mode_plan = hex info;
      mode_review = hex accent;
      muted = blend ink neutral 0.28;
      faint;
      rule = blend ink neutral 0.72;
      success = hex success;
      warning = hex warning;
      error = hex error;
      history = hex info;
      chip_fg = neutral;
      user_bg = blend neutral (hex primary) 0.12;
      overlay = neutral;
      code_keyword = hex keyword;
      code_type = hex type_;
      code_string = hex string;
      code_number = hex number;
      selection_bg = Ansi.Color.default;
      diff_added_bg = diff.diff_added_bg;
      diff_removed_bg = diff.diff_removed_bg;
      diff_added_sign = hex success;
      diff_removed_sign = hex error;
      diff_gutter_fg = faint;
      diff_added_gutter_bg = diff.diff_added_gutter_bg;
      diff_removed_gutter_bg = diff.diff_removed_gutter_bg;
      diff_added_emphasis = diff.diff_added_emphasis;
      diff_removed_emphasis = diff.diff_removed_emphasis;
    }

  let solarized_dark =
    port ~diff:default ~neutral:"#002b36" ~ink:"#93a1a1" ~primary:"#6c71c4"
      ~info:"#2aa198" ~accent:"#d33682" ~success:"#859900" ~warning:"#b58900"
      ~error:"#dc322f" ~keyword:"#859900" ~type_:"#268bd2" ~string:"#2aa198"
      ~number:"#d33682"

  let solarized_light =
    port ~diff:mentat_light ~neutral:"#fdf6e3" ~ink:"#586e75" ~primary:"#268bd2"
      ~info:"#2aa198" ~accent:"#d33682" ~success:"#859900" ~warning:"#b58900"
      ~error:"#dc322f" ~keyword:"#728600" ~type_:"#268bd2" ~string:"#1f8f88"
      ~number:"#d33682"

  let gruvbox_dark =
    port ~diff:default ~neutral:"#282828" ~ink:"#ebdbb2" ~primary:"#83a598"
      ~info:"#d3869b" ~accent:"#fb4934" ~success:"#b8bb26" ~warning:"#fabd2f"
      ~error:"#fb4934" ~keyword:"#fb4934" ~type_:"#83a598" ~string:"#b8bb26"
      ~number:"#d3869b"

  let gruvbox_light =
    port ~diff:mentat_light ~neutral:"#fbf1c7" ~ink:"#3c3836" ~primary:"#076678"
      ~info:"#8f3f71" ~accent:"#9d0006" ~success:"#79740e" ~warning:"#b57614"
      ~error:"#9d0006" ~keyword:"#9d0006" ~type_:"#076678" ~string:"#79740e"
      ~number:"#8f3f71"

  let catppuccin_dark =
    port ~diff:default ~neutral:"#1e1e2e" ~ink:"#cdd6f4" ~primary:"#b4befe"
      ~info:"#89dceb" ~accent:"#f38ba8" ~success:"#a6d189" ~warning:"#f4b8e4"
      ~error:"#f38ba8" ~keyword:"#cba6f7" ~type_:"#89b4fa" ~string:"#a6d189"
      ~number:"#fab387"

  let catppuccin_light =
    port ~diff:mentat_light ~neutral:"#eff1f5" ~ink:"#4c4f69" ~primary:"#7287fd"
      ~info:"#04a5e5" ~accent:"#d20f39" ~success:"#40a02b" ~warning:"#df8e1d"
      ~error:"#d20f39" ~keyword:"#8839ef" ~type_:"#1e66f5" ~string:"#40a02b"
      ~number:"#ca6702"

  let tokyonight_dark =
    port ~diff:default ~neutral:"#1a1b26" ~ink:"#c0caf5" ~primary:"#7aa2f7"
      ~info:"#7dcfff" ~accent:"#ff9e64" ~success:"#9ece6a" ~warning:"#e0af68"
      ~error:"#f7768e" ~keyword:"#bb9af7" ~type_:"#7aa2f7" ~string:"#9ece6a"
      ~number:"#ff9e64"

  let tokyonight_light =
    port ~diff:mentat_light ~neutral:"#e1e2e7" ~ink:"#273153" ~primary:"#2e7de9"
      ~info:"#007197" ~accent:"#b15c00" ~success:"#587539" ~warning:"#8c6c3e"
      ~error:"#c94060" ~keyword:"#9854f1" ~type_:"#1f6fd4" ~string:"#587539"
      ~number:"#b15c00"

  let nord_dark =
    port ~diff:default ~neutral:"#2e3440" ~ink:"#e5e9f0" ~primary:"#88c0d0"
      ~info:"#81a1c1" ~accent:"#d57780" ~success:"#a3be8c" ~warning:"#d08770"
      ~error:"#bf616a" ~keyword:"#81a1c1" ~type_:"#88c0d0" ~string:"#a3be8c"
      ~number:"#b48ead"

  let one_dark =
    port ~diff:default ~neutral:"#282c34" ~ink:"#abb2bf" ~primary:"#61afef"
      ~info:"#d19a66" ~accent:"#56b6c2" ~success:"#98c379" ~warning:"#e5c07b"
      ~error:"#e06c75" ~keyword:"#c678dd" ~type_:"#e5c07b" ~string:"#98c379"
      ~number:"#d19a66"

  let one_light =
    port ~diff:mentat_light ~neutral:"#fafafa" ~ink:"#383a42" ~primary:"#4078f2"
      ~info:"#986801" ~accent:"#0184bc" ~success:"#50a14f" ~warning:"#c18401"
      ~error:"#e45649" ~keyword:"#a626a4" ~type_:"#c18401" ~string:"#50a14f"
      ~number:"#986801"

  let appearance t = t.appearance
  let accent t = t.accent
  let mode_plan t = t.mode_plan
  let mode_review t = t.mode_review
  let muted t = t.muted
  let faint t = t.faint
  let rule t = t.rule
  let success t = t.success
  let warning t = t.warning
  let error t = t.error
  let history t = t.history
  let chip_fg t = t.chip_fg
  let user_bg t = t.user_bg
  let overlay t = t.overlay
  let code_keyword t = t.code_keyword
  let code_type t = t.code_type
  let code_string t = t.code_string
  let code_number t = t.code_number

  (* Selection follows the one-accent rule: the accent foreground marks a row,
     never a fill, so the background is the terminal default unless themed. *)
  let selection_fg t = t.accent
  let selection_bg t = t.selection_bg

  (* Diff group accessors. *)
  let diff_added_bg t = t.diff_added_bg
  let diff_removed_bg t = t.diff_removed_bg
  let diff_added_sign t = t.diff_added_sign
  let diff_removed_sign t = t.diff_removed_sign
  let diff_gutter_fg t = t.diff_gutter_fg
  let diff_added_gutter_bg t = t.diff_added_gutter_bg
  let diff_removed_gutter_bg t = t.diff_removed_gutter_bg
  let diff_added_emphasis t = t.diff_added_emphasis
  let diff_removed_emphasis t = t.diff_removed_emphasis

  (* Derived styles, one per role that carries emphasis, matching the
     module-level constants below so a palette reproduces the built-in look. *)
  let accent_style t = Ansi.Style.make ~fg:t.accent ~bold:true ()
  let atom_style t = Ansi.Style.make ~fg:t.accent ()
  let muted_style t = Ansi.Style.make ~fg:t.muted ()
  let faint_style t = Ansi.Style.make ~fg:t.faint ()
  let rule_style t = Ansi.Style.make ~fg:t.rule ()
  let success_style t = Ansi.Style.make ~fg:t.success ~bold:true ()
  let warning_style t = Ansi.Style.make ~fg:t.warning ~bold:true ()
  let error_style t = Ansi.Style.make ~fg:t.error ~bold:true ()
  let user_style t = Ansi.Style.make ~bg:t.user_bg ()
  let thinking_style t = Ansi.Style.make ~fg:t.muted ~italic:true ()
  let running_style t = Ansi.Style.make ~fg:t.accent ()
  let code_keyword_style t = Ansi.Style.make ~fg:t.code_keyword ()
  let code_type_style t = Ansi.Style.make ~fg:t.code_type ()
  let code_string_style t = Ansi.Style.make ~fg:t.code_string ()
  let code_number_style t = Ansi.Style.make ~fg:t.code_number ()

  (* [dimmed appearance color] fades [color] halfway to the terminal's own
     backdrop — black under a dark palette, white under a light one — so a
     dimmed fill recedes into the background it sits on rather than losing its
     colour. The two directions are exact mirrors: halve each channel, or halve
     each channel's distance from full. *)
  let dimmed appearance color =
    let r, g, b = Ansi.Color.to_rgb color in
    let fade channel =
      match appearance with
      | Dark -> channel / 2
      | Light -> 255 - ((255 - channel) / 2)
    in
    Ansi.Color.of_rgb (fade r) (fade g) (fade b)

  (* The three finished {!Mosaic.Diff.theme} values review renders, built from
     the diff roles so review never rebuilds one by hand. Every field maps to a
     role or to a principled constant: [context_bg], both [*_content_bg],
     [context_content_bg], and [line_number_bg] are [None] (no chrome). *)
  let diff_theme t =
    {
      Mosaic.Diff.added_bg = t.diff_added_bg;
      removed_bg = t.diff_removed_bg;
      context_bg = None;
      added_content_bg = None;
      removed_content_bg = None;
      context_content_bg = None;
      added_sign_color = t.diff_added_sign;
      removed_sign_color = t.diff_removed_sign;
      added_line_number_bg = Some t.diff_added_gutter_bg;
      removed_line_number_bg = Some t.diff_removed_gutter_bg;
      line_number_fg = t.diff_gutter_fg;
      line_number_bg = None;
    }

  (* Reviewed scope: the add/del backgrounds drop to the terminal default and
     the signs grey to [muted], so settled content stops shouting. *)
  let diff_theme_quieted t =
    {
      (diff_theme t) with
      Mosaic.Diff.added_bg = Ansi.Color.default;
      removed_bg = Ansi.Color.default;
      added_line_number_bg = None;
      removed_line_number_bg = None;
      added_sign_color = t.muted;
      removed_sign_color = t.muted;
    }

  (* Compose dialog: the same picture at half brightness — backgrounds faded
     halfway to the terminal's backdrop, signs greyed to [faint] — so the diff
     reads as dimmed, not decolored. *)
  let diff_theme_dimmed t =
    let dim = dimmed t.appearance in
    {
      (diff_theme t) with
      Mosaic.Diff.added_bg = dim t.diff_added_bg;
      removed_bg = dim t.diff_removed_bg;
      added_line_number_bg = Some (dim t.diff_added_gutter_bg);
      removed_line_number_bg = Some (dim t.diff_removed_gutter_bg);
      line_number_fg = t.faint;
      added_sign_color = t.faint;
      removed_sign_color = t.faint;
    }

  module Diagnostic = struct
    type t =
      | Unknown_role of string
      | Bad_color of { role : string; value : string }
      | Not_string of string
      | Not_object

    let message = function
      | Unknown_role role ->
          Printf.sprintf
            "unknown theme role %S (colors only; glyphs and layout are not \
             themable)"
            role
      | Bad_color { role; value } ->
          Printf.sprintf
            "role %S: %S is not a #rrggbb hex, a named ANSI color, or \
             \"default\""
            role value
      | Not_string role -> Printf.sprintf "role %S: value must be a string" role
      | Not_object ->
          "a theme must be a JSON object mapping color roles to values"

    let equal a b = String.equal (message a) (message b)
    let pp ppf d = Format.pp_print_string ppf (message d)
  end

  (* The color value grammar: ["default"] (terminal default), a ["#rrggbb"] hex,
     or a named ANSI color. *)
  let named_color = function
    | "black" -> Some Ansi.Color.black
    | "red" -> Some Ansi.Color.red
    | "green" -> Some Ansi.Color.green
    | "yellow" -> Some Ansi.Color.yellow
    | "blue" -> Some Ansi.Color.blue
    | "magenta" -> Some Ansi.Color.magenta
    | "cyan" -> Some Ansi.Color.cyan
    | "white" -> Some Ansi.Color.white
    | "bright-black" | "bright_black" -> Some Ansi.Color.bright_black
    | "bright-red" | "bright_red" -> Some Ansi.Color.bright_red
    | "bright-green" | "bright_green" -> Some Ansi.Color.bright_green
    | "bright-yellow" | "bright_yellow" -> Some Ansi.Color.bright_yellow
    | "bright-blue" | "bright_blue" -> Some Ansi.Color.bright_blue
    | "bright-magenta" | "bright_magenta" -> Some Ansi.Color.bright_magenta
    | "bright-cyan" | "bright_cyan" -> Some Ansi.Color.bright_cyan
    | "bright-white" | "bright_white" -> Some Ansi.Color.bright_white
    | _ -> None

  let color_of_string raw =
    match String.lowercase_ascii (String.trim raw) with
    | "default" -> Some Ansi.Color.default
    | s when String.length s > 0 && Char.equal s.[0] '#' -> Ansi.Color.of_hex s
    | s -> (
        match named_color s with
        | Some c -> Some c
        | None -> Ansi.Color.of_hex s)

  let roles =
    [
      "accent";
      "mode_plan";
      "mode_review";
      "muted";
      "faint";
      "rule";
      "success";
      "warning";
      "error";
      "history";
      "chip_fg";
      "user_bg";
      "overlay";
      "code_keyword";
      "code_type";
      "code_string";
      "code_number";
      "selection_bg";
      "diff_added_bg";
      "diff_removed_bg";
      "diff_added_sign";
      "diff_removed_sign";
      "diff_gutter_fg";
      "diff_added_gutter_bg";
      "diff_removed_gutter_bg";
      "diff_added_emphasis";
      "diff_removed_emphasis";
    ]

  let is_role role = List.mem role roles

  let set t role color =
    match role with
    | "accent" -> Some { t with accent = color }
    | "mode_plan" -> Some { t with mode_plan = color }
    | "mode_review" -> Some { t with mode_review = color }
    | "muted" -> Some { t with muted = color }
    | "faint" -> Some { t with faint = color }
    | "rule" -> Some { t with rule = color }
    | "success" -> Some { t with success = color }
    | "warning" -> Some { t with warning = color }
    | "error" -> Some { t with error = color }
    | "history" -> Some { t with history = color }
    | "chip_fg" -> Some { t with chip_fg = color }
    | "user_bg" -> Some { t with user_bg = color }
    | "overlay" -> Some { t with overlay = color }
    | "code_keyword" -> Some { t with code_keyword = color }
    | "code_type" -> Some { t with code_type = color }
    | "code_string" -> Some { t with code_string = color }
    | "code_number" -> Some { t with code_number = color }
    | "selection_bg" -> Some { t with selection_bg = color }
    | "diff_added_bg" -> Some { t with diff_added_bg = color }
    | "diff_removed_bg" -> Some { t with diff_removed_bg = color }
    | "diff_added_sign" -> Some { t with diff_added_sign = color }
    | "diff_removed_sign" -> Some { t with diff_removed_sign = color }
    | "diff_gutter_fg" -> Some { t with diff_gutter_fg = color }
    | "diff_added_gutter_bg" -> Some { t with diff_added_gutter_bg = color }
    | "diff_removed_gutter_bg" -> Some { t with diff_removed_gutter_bg = color }
    | "diff_added_emphasis" -> Some { t with diff_added_emphasis = color }
    | "diff_removed_emphasis" -> Some { t with diff_removed_emphasis = color }
    | _ -> None

  (* [extends] is reserved metadata naming the base to overlay, resolved by the
     executable before this call, not a role — skipped here so it never trips
     [Unknown_role]. *)
  let is_reserved role = String.equal role "extends"

  let of_json ~base json =
    match json with
    | Jsont.Object (members, _) ->
        List.fold_left
          (fun (palette, diagnostics) ((role, _), value) ->
            let add diagnostic = (palette, diagnostics @ [ diagnostic ]) in
            if is_reserved role then (palette, diagnostics)
            else if not (is_role role) then add (Diagnostic.Unknown_role role)
            else
              match value with
              | Jsont.String (raw, _) -> (
                  match color_of_string raw with
                  | None -> add (Diagnostic.Bad_color { role; value = raw })
                  | Some color ->
                      ( Option.value (set palette role color) ~default:palette,
                        diagnostics ))
              | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.Array _
              | Jsont.Object _ ->
                  add (Diagnostic.Not_string role))
          (base, []) members
    | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
    | Jsont.Array _ ->
        (base, [ Diagnostic.Not_object ])
end

(* The shipped presets, compiled in as OCaml values rather than embedded JSON:
   the crash screen must theme without I/O, static release binaries carry them,
   and listing presets needs no filesystem walk. The JSON {!Palette.of_json}
   grammar serves the user-file path only. [mentat-dark] is {!Palette.default};
   the six ports carry a light variant where the upstream defines one. *)
module Preset = struct
  type t = { name : string; palette : Palette.t }

  let make name palette = { name; palette }

  let all =
    [
      make "mentat-dark" Palette.default;
      make "mentat-light" Palette.mentat_light;
      make "solarized" Palette.solarized_dark;
      make "solarized-light" Palette.solarized_light;
      make "gruvbox" Palette.gruvbox_dark;
      make "gruvbox-light" Palette.gruvbox_light;
      make "catppuccin" Palette.catppuccin_dark;
      make "catppuccin-light" Palette.catppuccin_light;
      make "tokyonight" Palette.tokyonight_dark;
      make "tokyonight-light" Palette.tokyonight_light;
      make "nord" Palette.nord_dark;
      make "one-dark" Palette.one_dark;
      make "one-light" Palette.one_light;
    ]

  let find name = List.find_opt (fun t -> String.equal t.name name) all
  let names = List.map (fun t -> t.name) all
end

(* Module-level color constants and styles, projected from {!Palette.default}
   ([mentat-dark]) so the built-in look has one source. Every widget reads its
   colors from a threaded {!Palette.t}; these remain for the few app-owned marks
   and the odoc cross-references that name the built-in identity. *)
let color_accent = Palette.accent Palette.default
let color_muted = Palette.muted Palette.default
let color_faint = Palette.faint Palette.default
let color_rule = Palette.rule Palette.default
let color_success = Palette.success Palette.default
let color_warning = Palette.warning Palette.default
let color_error = Palette.error Palette.default
let color_history = Palette.history Palette.default
let color_user_bg = Palette.user_bg Palette.default
let color_code_kw = Palette.code_keyword Palette.default
let color_code_type = Palette.code_type Palette.default
let color_code_str = Palette.code_string Palette.default
let color_code_num = Palette.code_number Palette.default
let accent = Ansi.Style.make ~fg:color_accent ~bold:true ()
let atom = Ansi.Style.make ~fg:color_accent ()
let muted = Ansi.Style.make ~fg:color_muted ()
let faint = Ansi.Style.make ~fg:color_faint ()
let rule = Ansi.Style.make ~fg:color_rule ()
let success = Ansi.Style.make ~fg:color_success ~bold:true ()
let warning = Ansi.Style.make ~fg:color_warning ~bold:true ()
let error = Ansi.Style.make ~fg:color_error ~bold:true ()
let user = Ansi.Style.make ~bg:color_user_bg ()
let thinking = Ansi.Style.make ~fg:color_muted ~italic:true ()
let running = Ansi.Style.make ~fg:color_accent ()
let code_kw = Ansi.Style.make ~fg:color_code_kw ()
let code_type = Ansi.Style.make ~fg:color_code_type ()
let code_str = Ansi.Style.make ~fg:color_code_str ()
let code_num = Ansi.Style.make ~fg:color_code_num ()

(* Selection: a highlighted list row is marked by the accent {!cursor} and takes
   the accent foreground — it is never filled. A wash behind the row is the kind
   of chrome the interface refuses, and would fight the one-accent rule, so the
   selection background is the terminal default (no fill). Every picker, dialog, and panel
   routes its widget selection colors through this pair, so the whole TUI
   highlights the same way. *)
let selection_fg = color_accent
let selection_bg = Ansi.Color.default

(* The filled chip: the label on the current frame color with the palette's
   near-black {!Palette.chip_fg}
   foreground, one padding space each side. One drawing for panel names, screen
   names, and the composer's mode chips — the frame color is the only thing that
   varies. *)
let chip ~palette ~color label =
  text
    ~style:(Ansi.Style.make ~bg:color ~fg:(Palette.chip_fg palette) ())
    ~wrap:`None ~flex_shrink:0.
    (" " ^ label ^ " ")

(* Layout helpers shared by the review screen's panes. *)

let default_rule_width = 100

let repeat glyph width =
  let buffer = Buffer.create (width * String.length glyph) in
  for _ = 1 to width do
    Buffer.add_string buffer glyph
  done;
  Buffer.contents buffer

(* [panel_rule] tops a full-screen panel: every rule is "─" in the palette's
   rule color. *)
let panel_rule ~palette ?(width = default_rule_width) () =
  text ~style:(Palette.rule_style palette) ~wrap:`None (repeat "─" width)

let pad_right width value =
  if String.length value >= width then value
  else value ^ String.make (width - String.length value) ' '
