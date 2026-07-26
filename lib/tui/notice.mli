(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Transcript notices.

    A notice belongs to exactly one of the six closed classes in {!t}. Each
    class has one visible form, so adding a form requires extending the notice
    vocabulary rather than passing unclassified text to the renderer. *)

(** A countable item in a {!Data} notice. *)
type fact =
  | Fact of string
      (** Muted text, such as a file count or build-health statement. *)
  | Change of { added : int; removed : int }
      (** A change pair. [added] uses the success color and [removed] uses the
          error color. *)
  | Errors of int
      (** A build-error count. The number uses the error color; the muted noun
          is singular exactly when the count is [1]. *)

(** The outcome level of a {!Workspace} notice, driving its head color. *)
type level =
  | Info
      (** Muted: an informational observation such as a file-change batch. *)
  | Warning  (** Cautionary: a degraded watcher or recovered build. *)
  | Error  (** Failure-colored: a failing build. *)

(** A committed transcript notice. *)
type t =
  | Event of string
      (** An indent-two muted line with no standard glyph. Any glyph belonging
          to the event is part of the string. *)
  | Echo of { command : string; result : string option }
      (** A stateful slash command echoed as a muted cursor and [command],
          followed by an indent-two muted [result] when present. Commands that
          only open overlays do not produce this notice. *)
  | Interrupt
      (** The muted interruption guidance line prefixed by the interrupt glyph.
      *)
  | Failure of { message : string; next_step : string; count : int }
      (** An error-colored failure line followed by a muted [next_step]. A
          [count] greater than [1] appends a muted repetition multiplier to the
          first line. *)
  | Seam of string
      (** A centered, labeled transcript boundary, such as [compacted] or a
          resume summary. *)
  | Data of { source : string; facts : fact list; atom : string option }
      (** A muted watcher glyph and [source], followed by separator-delimited
          [facts] and at most one slash-command [atom]. *)
  | Workspace of {
      level : level;
      source : string;
      title : string;
      body : string option;
    }
      (** A durable workspace observation — a filesystem-watcher batch or a
          build-health change the model saw this turn. The head is a
          [level]-colored watcher glyph, [source], and one-line [title]; the
          optional multi-line [body] wraps below. Unlike {!Data}, it is a
          multi-line block form that renders the whole observation, not a
          truncated one-row glance. *)

val view : palette:Theme.Palette.t -> t -> 'msg Mosaic.t
(** [view notice] renders [notice] in its class's committed form.

    A {!Seam} leaves one-column margins and centers its intrinsic label between
    two flex-growing rules; Mosaic truncates the label when space is tight.
    Event text, echo results, and failure prose use word wrapping. Echo command
    heads, interrupt lines, and data notices are one-row forms that truncate
    under horizontal pressure. *)
