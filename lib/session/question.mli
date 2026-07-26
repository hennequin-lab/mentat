(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Questions posed to a reviewer.

    A question is the payload of a {!Decision} of kind [Question]: a prompt and
    optional multiple-choice options. The reviewer's reply is a {!Answer.t}. *)

module Error : sig
  (** Question construction errors. *)
  type t =
    | Empty_prompt  (** The prompt was empty. *)
    | Empty_choices  (** The choice list was present but empty. *)
    | Empty_choice  (** A choice string was empty. *)

  val message : t -> string
  (** [message e] is a human-readable diagnostic. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an error for diagnostics. *)
end

type t
(** The type for a reviewer question.

    Invariant: the prompt is non-empty; when choices are present the list is
    non-empty and every choice is non-empty. *)

val make : prompt:string -> ?choices:string list -> unit -> (t, Error.t) result
(** [make ~prompt ?choices ()] is a question, or a structured {!Error.t} if
    [prompt] is empty, or [choices] is present but empty or contains an empty
    choice. *)

val prompt : t -> string
(** [prompt t] is [t]'s prompt. *)

val choices : t -> string list option
(** [choices t] is [t]'s multiple-choice options, if it is a choice question. *)

(** {1:answers Answers} *)

module Answer : sig
  (** The type for a reviewer's reply to a question. *)
  type t = private
    | Free of string  (** A free-text reply. *)
    | Choice of int
        (** The zero-based index of the selected choice. Validity against a
            specific question's choices is checked where the answer is applied.
        *)

  val free : string -> t
  (** [free s] is a free-text reply.

      Raises [Invalid_argument] if [s] is empty. *)

  val choice : int -> t
  (** [choice i] selects the choice at zero-based index [i].

      Raises [Invalid_argument] if [i] is negative. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are the same answer. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an answer for diagnostics. *)

  val jsont : t Jsont.t
  (** [jsont] maps answers to JSON values by a per-arm tag, validating local
      invariants on decode. *)
end

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are the same question. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a question for diagnostics. *)

val jsont : t Jsont.t
(** [jsont] maps questions to JSON values, validating the same invariants as
    {!make} on decode. *)
