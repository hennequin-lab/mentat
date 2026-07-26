(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The theme picker: a fuzzy-filterable list of the resolved theme catalog with
    live preview on cursor move.

    The panel browses a pre-resolved catalog of {!Theme.Preset.t} — the built-in
    presets merged with the user's theme files, resolved once by the executable
    — so it performs no I/O. Moving the cursor emits {!Preview} with the row's
    palette for the shell to apply to the whole TUI; cancelling emits {!Close}
    for the shell to restore the palette it saved when the panel opened; and
    confirming emits {!Commit} with the chosen name for the shell to keep and
    persist. Keys mirror the model panel so the interaction is learned once. *)

(** {1:state State} *)

type t
(** The immutable panel state: the catalog, the filter input, and the selected
    filtered row. *)

type msg
(** A panel input produced by {!key} or by the interactive catalog table in
    {!view}. *)

(** The type for actions interpreted by the shell. *)
type event =
  | Stay  (** Keep the panel open with the returned state. *)
  | Preview of Theme.Palette.t
      (** Apply this palette to the whole TUI as a live preview. Emitted on
          every cursor move and whenever a filter change moves the selection. *)
  | Commit of { name : string }
      (** Keep the previewed palette and persist [name] as [tui.theme]. *)
  | Close
      (** Cancel: the shell restores the palette it saved when the panel opened.
      *)

val make : presets:Theme.Preset.t list -> current:string -> t
(** [make ~presets ~current] is a newly opened panel over [presets], with the
    row named [current] preselected (the first row if none matches). *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> msg option
(** [key event] is a keyboard message for [event], or [None] for an unsupported
    key. Table navigation and activation are emitted by the Mosaic widget
    itself. *)

val paste : string -> t -> t * event
(** [paste text t] appends [text] to the filter input, normalized to inert
    inline text, and previews the newly selected row. *)

val update : msg -> t -> t * event
(** [update message t] folds one panel input into [t]. Printable characters and
    digits append to the filter and preview the first match; Backspace deletes
    one grapheme; a cursor move previews the newly selected row; Enter commits
    the selected row; Escape cancels. *)

(** {1:view View} *)

val view :
  palette:Theme.Palette.t -> frame:Mosaic.Ansi.Color.t -> t -> msg Mosaic.t
(** [view ~palette ~frame t] renders the theme panel: a quiet picker table of
    catalog names, each tagged [dark] or [light], with the current theme
    carrying a trailing ✓. [palette] is the live (previewed) palette the whole
    panel is drawn in. *)
