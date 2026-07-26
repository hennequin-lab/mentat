(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
module Palette = Mentat_tui.Theme.Palette
module Preset = Mentat_tui.Theme.Preset
module Color = Mosaic.Ansi.Color
module Diff = Mosaic.Diff

(* The palette overlay is a pure data transform with rich diagnostics — the
   sanctioned windtrap exception. This suite pins the color grammar, the overlay
   diagnostics and base folding, totality (a partial theme fills omitted roles
   from its base), the compiled presets, and the diff-theme projections. *)

let jo pairs =
  Jsont.Json.object'
    (List.map
       (fun (role, value) ->
         Jsont.Json.mem (Jsont.Json.name role) (Jsont.Json.string value))
       pairs)

let messages diagnostics = List.map Palette.Diagnostic.message diagnostics
let same a b = Color.equal a b
let hex s = Option.get (Color.of_hex s)
let of_default json = Palette.of_json ~base:Palette.default json

let appearance_name = function
  | Palette.Dark -> "dark"
  | Palette.Light -> "light"

let brightness color =
  let r, g, b = Color.to_rgb color in
  r + g + b

let overlay =
  group "of_json overlay"
    [
      test "a hex role overlays with no diagnostics" (fun () ->
          let palette, diagnostics =
            of_default (jo [ ("accent", "#ff0000") ])
          in
          equal (list string) ~msg:"no diagnostics" [] (messages diagnostics);
          is_true ~msg:"accent is the hex"
            (same (Palette.accent palette) (hex "#ff0000"));
          is_true ~msg:"omitted roles keep the base"
            (same (Palette.muted palette) (Palette.muted Palette.default)));
      test "a named ANSI color parses" (fun () ->
          let palette, diagnostics = of_default (jo [ ("accent", "blue") ]) in
          equal (list string) [] (messages diagnostics);
          is_true (same (Palette.accent palette) Color.blue));
      test "the default sentinel parses" (fun () ->
          let palette, _ = of_default (jo [ ("selection_bg", "default") ]) in
          is_true (same (Palette.selection_bg palette) Color.default));
      test "a diff role overlays" (fun () ->
          let palette, diagnostics =
            of_default (jo [ ("diff_added_bg", "#010203") ])
          in
          equal (list string) [] (messages diagnostics);
          is_true (same (Palette.diff_added_bg palette) (hex "#010203")));
      test "an unknown role is rejected and kept default" (fun () ->
          let palette, diagnostics =
            of_default (jo [ ("lockup", "#ffffff") ])
          in
          equal int ~msg:"one diagnostic" 1 (List.length diagnostics);
          is_true ~msg:"palette unchanged"
            (same (Palette.accent palette) (Palette.accent Palette.default)));
      test "an unparseable color keeps the role's default" (fun () ->
          let palette, diagnostics =
            of_default (jo [ ("accent", "not-a-color") ])
          in
          equal (list string)
            [
              {|role "accent": "not-a-color" is not a #rrggbb hex, a named ANSI color, or "default"|};
            ]
            (messages diagnostics);
          is_true ~msg:"accent kept its default"
            (same (Palette.accent palette) (Palette.accent Palette.default)));
      test "a non-string value is rejected" (fun () ->
          let json =
            Jsont.Json.object'
              [ Jsont.Json.mem (Jsont.Json.name "accent") (Jsont.Json.int 42) ]
          in
          let _, diagnostics = of_default json in
          equal (list string)
            [ {|role "accent": value must be a string|} ]
            (messages diagnostics));
      test "a non-object theme falls back to the base" (fun () ->
          let palette, diagnostics = of_default (Jsont.Json.string "oops") in
          equal (list string)
            [ "a theme must be a JSON object mapping color roles to values" ]
            (messages diagnostics);
          is_true
            (same (Palette.accent palette) (Palette.accent Palette.default)));
      test "a partial theme is total, filling omitted roles" (fun () ->
          let palette, diagnostics =
            of_default (jo [ ("error", "#000000"); ("success", "#111111") ])
          in
          equal (list string) [] (messages diagnostics);
          is_true (same (Palette.error palette) (hex "#000000"));
          is_true (same (Palette.success palette) (hex "#111111"));
          List.iter
            (fun (name, role) ->
              is_true ~msg:(name ^ " keeps default")
                (same (role palette) (role Palette.default)))
            [
              ("accent", Palette.accent);
              ("muted", Palette.muted);
              ("warning", Palette.warning);
              ("code_keyword", Palette.code_keyword);
              ("diff_removed_sign", Palette.diff_removed_sign);
            ]);
    ]

let base_and_extends =
  group "base and extends"
    [
      test "of_json overlays onto the supplied base, not the default" (fun () ->
          let base, _ =
            of_default (jo [ ("accent", "#010203"); ("muted", "#040506") ])
          in
          let palette, _ =
            Palette.of_json ~base (jo [ ("accent", "#0a0b0c") ])
          in
          is_true ~msg:"accent overridden"
            (same (Palette.accent palette) (hex "#0a0b0c"));
          is_true ~msg:"muted inherited from base"
            (same (Palette.muted palette) (hex "#040506")));
      test "the extends key is reserved metadata, not a role" (fun () ->
          let palette, diagnostics =
            of_default (jo [ ("extends", "solarized"); ("accent", "#ff0000") ])
          in
          equal (list string) ~msg:"extends emits no diagnostic" []
            (messages diagnostics);
          is_true (same (Palette.accent palette) (hex "#ff0000")));
      (* Appearance is a property of the palette, not a role a theme file can
         write: it decides which way {!Palette.diff_theme_dimmed} fades, so a
         file that could flip it could make the compose dialog paint dark bands
         on a pale terminal. It rides on the base for free. *)
      test "an overlay inherits its base's appearance" (fun () ->
          let light_base =
            match Preset.find "mentat-light" with
            | Some preset -> preset.Preset.palette
            | None -> Palette.default
          in
          let palette, _ =
            Palette.of_json ~base:light_base (jo [ ("accent", "#ff0000") ])
          in
          equal string ~msg:"light base stays light" "light"
            (appearance_name (Palette.appearance palette)));
      test "appearance is not a settable role" (fun () ->
          let palette, diagnostics =
            of_default (jo [ ("appearance", "light") ])
          in
          equal (list string)
            [
              {|unknown theme role "appearance" (colors only; glyphs and layout are not themable)|};
            ]
            (messages diagnostics);
          equal string ~msg:"the dark default is unmoved" "dark"
            (appearance_name (Palette.appearance palette)));
    ]

let presets =
  group "presets"
    [
      test "the catalog is mentat dark/light plus the six ports" (fun () ->
          equal int 13 (List.length Preset.all));
      test "preset names are unique, listed, and findable" (fun () ->
          let names = Preset.names in
          equal int ~msg:"names match all" (List.length Preset.all)
            (List.length names);
          equal int ~msg:"names are unique" (List.length names)
            (List.length (List.sort_uniq String.compare names));
          List.iter
            (fun name -> is_true ~msg:name (Option.is_some (Preset.find name)))
            names);
      test "mentat-dark is the default palette" (fun () ->
          match Preset.find "mentat-dark" with
          | Some preset ->
              is_true
                (same
                   (Palette.accent preset.Preset.palette)
                   (Palette.accent Palette.default))
          | None -> is_true ~msg:"mentat-dark present" false);
      test "an unknown name has no preset" (fun () ->
          is_true (Option.is_none (Preset.find "no-such-theme")));
      (* A port takes its appearance from the mentat diff table it copies, so
         the tag cannot disagree with the terminal the preset was built for. The
         name says which that is. *)
      test "every preset's appearance matches its name" (fun () ->
          List.iter
            (fun (preset : Preset.t) ->
              let expected =
                if String.ends_with ~suffix:"-light" preset.Preset.name then
                  "light"
                else "dark"
              in
              equal string ~msg:preset.Preset.name expected
                (appearance_name (Palette.appearance preset.Preset.palette)))
            Preset.all);
      (* Reading every preset forces construction of the ported palettes, so a
         malformed compiled hex raises here rather than at TUI launch. Each
         palette is a total 27-role record by construction; the diff projection
         being well-formed for every one witnesses totality. *)
      test "every preset yields a well-formed diff theme" (fun () ->
          List.iter
            (fun (preset : Preset.t) ->
              let dt = Palette.diff_theme preset.Preset.palette in
              is_true
                ~msg:(preset.Preset.name ^ " added_bg maps the role")
                (same dt.Diff.added_bg
                   (Palette.diff_added_bg preset.Preset.palette));
              is_true
                ~msg:(preset.Preset.name ^ " gutter fg maps the role")
                (same dt.Diff.line_number_fg
                   (Palette.diff_gutter_fg preset.Preset.palette)))
            Preset.all);
      (* An addition reads green and a deletion red before a word of the line is
         read, so every shipped preset's diff fills lead on the matching channel
         by a visible margin. Dominance alone is too weak a test: an olive fill
         can lead on green by one unit and still read brown. The shipped fills
         clear 20; 16 leaves the light pastels headroom while rejecting the
         teal, blue, and olive an upstream "add" hue routinely tints toward. *)
      let margin = 16 in
      test "every preset's diff fills are green and red" (fun () ->
          let green name role palette =
            let r, g, b = Color.to_rgb (role palette) in
            is_true ~msg:(name ^ " leads on green") (g - max r b >= margin)
          in
          let red name role palette =
            let r, g, b = Color.to_rgb (role palette) in
            is_true ~msg:(name ^ " leads on red") (r - max g b >= margin)
          in
          List.iter
            (fun (preset : Preset.t) ->
              let p = preset.Preset.palette in
              let of_ role = preset.Preset.name ^ " " ^ role in
              green (of_ "added_bg") Palette.diff_added_bg p;
              green (of_ "added_gutter_bg") Palette.diff_added_gutter_bg p;
              green (of_ "added_emphasis") Palette.diff_added_emphasis p;
              red (of_ "removed_bg") Palette.diff_removed_bg p;
              red (of_ "removed_gutter_bg") Palette.diff_removed_gutter_bg p;
              red (of_ "removed_emphasis") Palette.diff_removed_emphasis p)
            Preset.all);
    ]

let diff_projections =
  group "diff projections"
    [
      test "diff_theme maps each role to its field, chrome to None" (fun () ->
          let p = Palette.default in
          let dt = Palette.diff_theme p in
          is_true (same dt.Diff.added_bg (Palette.diff_added_bg p));
          is_true (same dt.Diff.removed_bg (Palette.diff_removed_bg p));
          is_true (same dt.Diff.added_sign_color (Palette.diff_added_sign p));
          is_true
            (same dt.Diff.removed_sign_color (Palette.diff_removed_sign p));
          is_true (same dt.Diff.line_number_fg (Palette.diff_gutter_fg p));
          is_true
            (same
               (Option.get dt.Diff.added_line_number_bg)
               (Palette.diff_added_gutter_bg p));
          is_true
            (same
               (Option.get dt.Diff.removed_line_number_bg)
               (Palette.diff_removed_gutter_bg p));
          is_true ~msg:"context_bg has no chrome"
            (Option.is_none dt.Diff.context_bg);
          is_true ~msg:"line_number_bg has no chrome"
            (Option.is_none dt.Diff.line_number_bg));
      test "diff_theme_quieted drops backgrounds and mutes the signs" (fun () ->
          let p = Palette.default in
          let dt = Palette.diff_theme_quieted p in
          is_true (same dt.Diff.added_bg Color.default);
          is_true (same dt.Diff.removed_bg Color.default);
          is_true (Option.is_none dt.Diff.added_line_number_bg);
          is_true (Option.is_none dt.Diff.removed_line_number_bg);
          is_true (same dt.Diff.added_sign_color (Palette.muted p));
          is_true (same dt.Diff.removed_sign_color (Palette.muted p)));
      test "diff_theme_dimmed halves backgrounds and faints the signs"
        (fun () ->
          let p = Palette.default in
          let dt = Palette.diff_theme_dimmed p in
          is_true (same dt.Diff.added_sign_color (Palette.faint p));
          is_true (same dt.Diff.removed_sign_color (Palette.faint p));
          is_true (same dt.Diff.line_number_fg (Palette.faint p));
          let r, g, b = Color.to_rgb (Palette.diff_added_bg p) in
          is_true ~msg:"added bg halved"
            (same dt.Diff.added_bg (Color.of_rgb (r / 2) (g / 2) (b / 2))));
      (* Dimming fades toward the terminal's own backdrop, so the compose
         dialog's diff recedes on either kind of terminal: down toward black
         under a dark palette, up toward white under a light one. A fill that
         faded the wrong way would paint dark muddy bands across a pale page. *)
      test "dimming fades every preset's fills toward its own backdrop"
        (fun () ->
          List.iter
            (fun (preset : Preset.t) ->
              let p = preset.Preset.palette in
              let dt = Palette.diff_theme_dimmed p in
              let fades role fill dimmed =
                let msg = preset.Preset.name ^ " " ^ role in
                match Palette.appearance p with
                | Palette.Dark ->
                    is_true ~msg:(msg ^ " dims darker")
                      (brightness dimmed < brightness fill)
                | Palette.Light ->
                    is_true ~msg:(msg ^ " dims lighter")
                      (brightness dimmed > brightness fill)
              in
              fades "added_bg" (Palette.diff_added_bg p) dt.Diff.added_bg;
              fades "removed_bg" (Palette.diff_removed_bg p) dt.Diff.removed_bg;
              fades "added_gutter_bg"
                (Palette.diff_added_gutter_bg p)
                (Option.get dt.Diff.added_line_number_bg);
              fades "removed_gutter_bg"
                (Palette.diff_removed_gutter_bg p)
                (Option.get dt.Diff.removed_line_number_bg))
            Preset.all);
    ]

let () =
  run "mentat.tui.theme"
    [ overlay; base_and_extends; presets; diff_projections ]
