(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Intrinsic sections for the wide-terminal activity pane.

    A section pairs header metadata with its complete Mosaic content. This
    module preserves the caller's section order, supplies the shared header and
    inter-section spacing, and places the resulting stack in the pane's vertical
    viewport. Content providers never receive terminal dimensions or row grants:
    Mosaic owns measurement, wrapping, allocation, and scrolling.

    Headers contain a muted label followed by optional faint summary facts
    separated by {!Theme.separator}. Content renders directly beneath its header
    and owns any internal indentation. Adjacent sections have one blank row
    between them, with no leading or trailing gap. *)

(** {1:sections Sections} *)

type 'msg t
(** The type for a named section whose content emits messages of type ['msg]. *)

val section : label:string -> ?facts:string list -> 'msg Mosaic.t list -> 'msg t
(** [section ~label ?facts content] is a section headed by [label]. [facts]
    default to [[]] and appear after [label] in their given order. [content] is
    the complete intrinsic content below the header; it is never measured,
    clipped, or windowed by this module.

    A section with empty [content] is omitted by {!view}, including its header,
    so unavailable surfaces do not leave orphan headings. *)

(** {1:rendering Rendering} *)

val view : palette:Theme.Palette.t -> 'msg t list -> 'msg Mosaic.t list
(** [view sections] is [[]] when every section has empty content. Otherwise it
    is a singleton vertical Mosaic scroll viewport containing every non-empty
    section in list order.

    The viewport grows and shrinks within the height allocated by {!Pane.frame}.
    It shows a scrollbar only when the complete section stack overflows and does
    not take keyboard focus from an interactive descendant or the composer.
    Mosaic owns scroll position and pointer-wheel interaction. *)
