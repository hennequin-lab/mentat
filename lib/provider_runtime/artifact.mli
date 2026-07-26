(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Provider-neutral local model artifact vocabulary.

    Artifacts are provider-owned local files a local model needs before it can
    run. This vocabulary is provider-neutral so interactive surfaces can show
    readiness and progress without importing a provider package. It lives in the
    runtime rather than the protocol, so the runtime carries the type without
    depending on the protocol; it depends only on [Mentat_llm.Provider.t]. The
    executable adapts these values onto the protocol's model-download flow at
    the composition edge. *)

module Status : sig
  type t =
    | Installed of { path : string }
        (** The artifact exists locally at [path]. *)
    | Missing of { path : string; size : int64 option; source : string option }
        (** The artifact is not installed at [path]. [size] is the expected byte
            size when known; [source] a human-readable origin when one can be
            exposed safely. *)
    | Unavailable of { message : string }
        (** The provider could not determine artifact status. [message] is a
            display diagnostic. *)

  val summary : t -> string
  (** [summary status] is a short display string for status panes. *)

  val jsont : t Jsont.t
  (** [jsont] maps status values to and from tagged JSON objects. *)
end

module Progress : sig
  type phase =
    | Checking  (** Resolving local state or remote metadata. *)
    | Downloading  (** Transferring bytes. *)
    | Verifying  (** Checking the downloaded artifact before installation. *)
    | Ready  (** Available for use. *)

  type t = {
    provider : Mentat_llm.Provider.t;
        (** Provider namespace owning the artifact. *)
    model : string;  (** Provider-local model id being prepared. *)
    label : string;  (** Short display label for the artifact. *)
    received : int64;  (** Bytes received or verified so far. *)
    total : int64 option;  (** Total byte count when known. *)
    phase : phase;  (** Current preparation phase. *)
  }
  (** A display-oriented artifact preparation update. Provider adapters keep
      their protocol-specific download state, checksums, and mirror selection
      private. *)

  val jsont : t Jsont.t
  (** [jsont] maps progress updates to and from JSON objects. *)
end

module Download_outcome : sig
  type t =
    | Already_installed of string
        (** The artifact is already installed at the given path. *)
    | Not_downloadable
        (** The model is an explicit local weight path, so there is nothing to
            download. *)
    | Downloaded  (** The artifact was fetched, verified, and installed. *)
    | Refused of { message : string; force_hint : bool }
        (** The download did not proceed. [force_hint] is [true] when re-running
            with a force override would proceed. *)

  val jsont : t Jsont.t
  (** [jsont] maps download outcomes to and from tagged JSON objects. *)
end
