(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure state and rendering for the slash-command palette.

    The shell owns whether the palette is open. While the draft remains a single
    slash-command token, it builds the merged row list — builtin commands from
    {!Command.filter} plus custom user commands filtered by the same query — and
    hands it to {!with_rows}; it routes movement and completion keys through
    {!move} and {!complete}, and interprets {!activate}. A separator that begins
    an argument closes the palette at that shell boundary; this module does not
    infer visibility from query text.

    The palette is a pure function over the supplied rows: it derives movement,
    selection, and activation from the row list and its stored selection, and no
    longer calls {!Command.filter} itself. The shell, not the palette, owns the
    cached custom-command snapshot, so the palette still cannot leave a stale
    row behind — it renders exactly the list handed to it. Builtin rows lead to
    their {!Command.fate}; custom rows expand to a user turn. *)

(** {1:types Types} *)

(** The type for one palette row: a builtin catalog command or a discovered
    custom user command. *)
type row = Builtin of Command.t | Custom of Mentat_protocol.User_command.t

type t
(** The type for palette state: the supplied rows, the canonical query, and the
    selection.

    The query is canonicalized with ASCII case-folding and removal of leading
    and trailing spaces and tabs. The selection is always the index of a row, or
    [0] when there are no rows. *)

(** The type for the shell action produced by activating a selected row. *)
type activation =
  | Run of Command.t
      (** Dispatch the selected argument-free builtin command according to its
          {!Command.fate} and {!Command.availability}. *)
  | Insert of string
      (** Replace the draft with the selected argument-taking command's slash
          spelling and one trailing space, then let the user type its argument.
      *)
  | Expand of string
      (** Expand the selected argument-free custom command with empty arguments
          and submit it as a user turn. The string is the command's [:]-joined
          name. *)

type msg
(** The type for selection and activation messages produced by {!view}. *)

(** The type for actions interpreted by the shell. *)
type event =
  | Stay
      (** Keep the palette open. The returned state may contain a changed
          selection. *)
  | Activated of activation
      (** Apply the same row activation as {!activate} to the row selected by a
          table activation. *)

(** {1:constructors Constructors} *)

val make : t
(** [make] is an empty palette: no rows, empty query, selection [0]. *)

(** {1:filtering Filtering} *)

val with_rows : query:string -> row list -> t -> t
(** [with_rows ~query rows t] replaces [t]'s rows with the shell-supplied [rows]
    already filtered for [query].

    Leading and trailing spaces and tabs and ASCII letter case are canonical in
    [query], so changing only those preserves the selected row (clamped to the
    new row count). A different canonical query resets the selection to the
    first row. *)

val selected_command : t -> Command.t option
(** [selected_command t] is the selected row's builtin command, or [None] when
    no row is selected or the selected row is a custom command. *)

(** {1:transitions Transitions} *)

val move : [ `Up | `Down ] -> t -> t
(** [move direction t] moves the selection one row in [direction], wrapping
    between the first and last row. It is [t] unchanged when there are no rows.
*)

val activate : t -> activation option
(** [activate t] is the activation of the selected row: {!Run} for an
    argument-free builtin, {!Insert} for an argument-taking builtin or a custom
    command with an argument hint, and {!Expand} for an argument-free custom
    command. It is [None] when there are no rows; the shell then closes the
    palette and submits the unchanged draft through the ordinary composer path.
*)

val update : msg -> t -> t * event
(** [update message t] folds a table selection or activation into [t]. Selection
    indexes are clamped to the current rows. Activating a row selects it before
    producing {!Activated}; with no rows it produces {!Stay}. *)

val complete : t -> string option
(** [complete t] is the rows' longest common canonical slash prefix when that
    prefix strictly extends the typed slash token. It is [None] when there are
    no rows, the rows share nothing beyond the typed token, or the query matched
    only inside a spelling or description and is therefore not a prefix of the
    replacement. Slash spellings are lowercase ASCII, so completion never splits
    a user-perceived character. *)

(** {1:views Views} *)

val view :
  palette:Theme.Palette.t ->
  ?command_mode:bool ->
  ?keybind:(row -> string option) ->
  t ->
  msg Mosaic.t
(** [view t] renders the rows through {!Completion_list.view}. The slash
    spelling — or the title for a slash-less row — is the intrinsic primary
    column, and the muted description is the flexible detail column. In the
    slash palette (the default) the faint trailing column is an argument hint
    plus, for a custom command, a scope tag. In the command palette
    ([command_mode:true]) it is instead [keybind row], the row's current
    keybind, right-aligned and blank when unbound. The selected primary uses
    {!Theme.accent}. With no rows the view is an inert muted
    [no matching commands] row. *)
