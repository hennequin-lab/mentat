(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Server-owned model listings.

    A listing is the pure value of one provider observation of the models a
    server currently serves: provider-native model ids, each optionally
    annotated with server-supplied metadata — including the {e wire protocol}
    as a {!Mentat_llm.Model.Api.t}, which no pure function of the id can
    invent. Drivers lower a live response into a listing; declarations
    interpret listed entries into catalog models with
    {!Declaration.resolve_listed}; readiness treats a listing as the
    availability authority for its provider.

    Listings are snapshots, not catalogs: they carry no provider identity, no
    credential material, and imply no endpoint. Construct them with {!make} or
    {!of_ids}. *)

(** {1:models Listed models} *)

module Model : sig
  (** One server-listed model: its provider-native id plus whatever metadata
      the server published for it. Every annotation is optional; a bare id is
      a valid listed model. *)

  type t
  (** The type for listed models. *)

  val make :
    id:string ->
    ?api:Mentat_llm.Model.Api.t ->
    ?display_name:string ->
    ?family:string ->
    ?context_window:int ->
    ?max_output_tokens:int ->
    ?pricing:Model.Pricing.t ->
    ?input_modalities:Model.Modality.t list ->
    ?capabilities:Model.Capability.t list ->
    ?status:Model.status ->
    unit ->
    t
  (** [make ~id ()] is listed model [id]. Every omitted annotation is unknown,
      not defaulted: absence means the server did not say.

      [api] is the wire-protocol family the server assigned to the model,
      lowered by the driver from the server's own vocabulary.

      Raises [Invalid_argument] if [id], [display_name], or [family] is empty,
      if [context_window] or [max_output_tokens] is non-positive, or if
      [input_modalities] or [capabilities] contains duplicates. *)

  val of_id : string -> t
  (** [of_id id] is [make ~id ()]: a bare listed id with no metadata. *)

  val id : t -> string
  (** [id t] is [t]'s provider-native model id. *)

  val api : t -> Mentat_llm.Model.Api.t option
  (** [api t] is the wire-protocol family the server assigned, if known. *)

  val display_name : t -> string option
  (** [display_name t] is the server's display name, if any. *)

  val family : t -> string option
  (** [family t] is the server's model family tag, if any. *)

  val context_window : t -> int option
  (** [context_window t] is the server's context-window figure, if any. *)

  val max_output_tokens : t -> int option
  (** [max_output_tokens t] is the server's output-token figure, if any. *)

  val pricing : t -> Model.Pricing.t option
  (** [pricing t] is the server's pricing, if any. *)

  val input_modalities : t -> Model.Modality.t list option
  (** [input_modalities t] is the server's input modalities, if published. *)

  val capabilities : t -> Model.Capability.t list option
  (** [capabilities t] is the server's capabilities, if published. *)

  val status : t -> Model.status option
  (** [status t] is the server's lifecycle status, if published. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] have the same id and metadata. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats [t] for diagnostics. *)
end

(** {1:listings Listings} *)

type t
(** The type for listings. Model order is the server's order. *)

val make : Model.t list -> t
(** [make models] is the listing of [models] in order.

    Raises [Invalid_argument] if two models share an id. *)

val of_ids : string list -> t
(** [of_ids ids] is [make (List.map Model.of_id ids)]: a metadata-less listing
    for servers that publish bare ids.

    Raises [Invalid_argument] if an id is empty or repeated. *)

val models : t -> Model.t list
(** [models t] is [t]'s listed models in server order. *)

val ids : t -> string list
(** [ids t] is [t]'s model ids in server order. *)

val mem : t -> string -> bool
(** [mem t id] is [true] iff [t] lists a model with id [id]. *)

val find : t -> string -> Model.t option
(** [find t id] is [t]'s listed model with id [id], if any. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] list the same models in the same
    order. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics. *)
