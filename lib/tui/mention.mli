(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Workspace-path completion for an active [@] token over one recursive
    enumeration.

    This module is the pure state machine at the optional file-enumeration
    capability boundary. It performs no filesystem or workspace access and
    applies no ignore policy: the executable supplies one already-authorized,
    ignore-pruned, bounded recursive file list through {!loaded}, and every
    query is answered from that list. An empty or [dir/] query browses one
    directory's children; any other query matches recursively, so a nested path
    completes without stepping through its parents. When the capability is
    absent, the shell does not construct this state and [@] remains ordinary
    composer text.

    Open with {!make} followed by {!request_load}; feed the one result to
    {!loaded}; then route query, navigation, tab, and enter transitions through
    the resulting immutable value. *)

(** {1:items Items} *)

(** The type for a completable workspace-path item.

    Thread addressing is not part of file enumeration; this type contains only
    paths derived from that capability. *)
type item =
  | File of Lpath.Rel.t  (** A workspace file at the root-relative path. *)
  | Directory of Lpath.Rel.t
      (** A workspace directory, derived as an ancestor of an enumerated file. A
          directory with no enumerated descendant does not appear. *)

(** {1:state State} *)

type t
(** The type for an immutable completion state.

    A value owns its query, selected row, and the one accepted enumeration
    result. Exact duplicate files in a malformed capability result have one
    visible occurrence. *)

val make : ?query:string -> unit -> t
(** [make ?query ()] is a fresh completion with row zero selected and the
    enumeration not yet requested. [query] defaults to [""]. *)

val with_query : string -> t -> t
(** [with_query query t] sets the active [@]-token text, excluding its leading
    [@]. A changed string resets selection to row zero; an unchanged string
    retains and clamps the selection.

    The query is trimmed and ASCII case-folded; non-ASCII bytes compare exactly.
    An empty query browses: it lists only the root directory's immediate
    children, directories first, so the opening view stays quiet and {!tab}
    walks into a selected directory. A [dir/] query naming an enumerated
    directory browses that directory the same way. Any other query matches
    recursively by substring over each item's full root-relative path, with
    directories carrying their trailing [/]. Matches whose basename contains the
    query rank before matches that only contain it earlier in the path; within a
    rank, visible entries precede hidden ones (any [.]-led component) and
    enumeration order breaks ties. At most {!max_visible} rows are visible. *)

val max_visible : int
(** [max_visible] is the greatest number of matches presented for one query.
    Deeper matches are reachable by narrowing the query. *)

val select_next : t -> t
(** [select_next t] selects the following visible row, wrapping from the last
    row to the first. An empty list is unchanged. *)

val select_previous : t -> t
(** [select_previous t] selects the preceding visible row, wrapping from the
    first row to the last. An empty list is unchanged. *)

(** {1:loading Loading} *)

val request_load : t -> t * bool
(** [request_load t] is [t] with the enumeration marked in flight, paired with
    whether the caller must issue it. The enumeration is needed exactly once: an
    in-flight or settled request is never requested again. *)

val loaded : (Lpath.Rel.t list, Mentat_diagnostic.t) result -> t -> t
(** [loaded result t] records the capability result and clamps the selection.
    [Ok files] keeps the first occurrence of each file, derives each file's
    ancestor directories, and orders all items by root-relative path, so a
    directory immediately precedes its contents. [Error diagnostic] renders the
    structured boundary error as a display-safe, one-line message: malformed
    UTF-8 and terminal controls become [U+FFFD]. *)

(** {1:selection Selection} *)

val enter : t -> item option
(** [enter t] is the selected file or directory, or [None] when no item is
    visible. *)

(** The result of {!tab}. *)
type tab_result =
  | Chosen of item
      (** The selected file was chosen for insertion as an atomic mention. *)
  | Browse of Lpath.Rel.t
      (** The selected directory should become the browse location: the shell
          rewrites the active token to [@dir/], and the resulting {!with_query}
          lists that directory's children. Enter, by contrast, inserts the
          directory mention itself. *)
  | No_selection  (** No item is visible, so tab has no effect. *)

val tab : t -> tab_result
(** [tab t] chooses the selected file, walks into the selected directory, or is
    {!No_selection} when the filtered list is empty. *)

type msg
(** The type for exact-item selection and activation messages produced by
    {!view}. *)

(** The type for view events interpreted by the shell. *)
type event =
  | Stay
      (** Keep the completion open. The returned state may have a different
          selected item. *)
  | Activated of item
      (** Complete the exact file or directory activated in the table. *)

val update : msg -> t -> t * event
(** [update message t] folds one table selection or activation into [t]. A
    message for an item no longer visible is stale and leaves [t] unchanged.
    Activation first selects its exact item and then produces {!Activated}. *)

(** {1:view View} *)

val view : palette:Theme.Palette.t -> t -> msg Mosaic.t
(** [view t] renders the visible paths through {!Completion_list.view}. File and
    directory rows use the file-kind glyph; directories end in [/]. Each
    complete root-relative path occupies one flexible segment after the glyph,
    so its parent and basename remain contiguous as space contracts.

    Before the enumeration settles the view says [loading files…]. Failure,
    empty-workspace, and filter-empty states respectively render an inert error,
    [no files], and [no matching files]. Mosaic owns allocation, clipping,
    scrolling, mouse selection, and activation; this module performs no
    terminal-width measurement or path slicing. *)
