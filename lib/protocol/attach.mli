(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The image-attach request/completion vocabulary.

    Attaching an image is a request/completion flow, not a {!Command}: its
    completion is a model-visible {!Mentat_llm.Content.t} media block (source
    [`Ref]) the frontend holds in its draft, and the referenced attachment blob
    is orphan until a later prompt references it. The flow reads (for {!Path})
    or accepts (for {!Bytes}) the image, downscales it on oversize, validates it
    against the configured caps, stores its bytes as a session attachment, and
    returns the content block — or refuses with a typed reason. *)

module Rejection : sig
  (** Why an image failed the caps, format, or dimension checks. The engine's
      internal caps decision maps into this; the frontend renders it. *)

  type t =
    | Unsupported_format
        (** The bytes match no supported raster-image format. *)
    | Too_large of { bytes : int; cap : int }
        (** The image, after any downscale, exceeds the byte cap. *)
    | Too_many_pixels of { width : int; height : int; cap : int }
        (** A side exceeds the dimension bomb-guard. *)
    | Too_many_images of { count : int; cap : int }
        (** The input carries more images than the count cap allows. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats a human-readable rejection reason. *)
end

type source =
  | Path of Mentat_workspace.Path.t
      (** An image file the executable reads through its workspace boundary. *)
  | Bytes of { media_type : string; bytes : string }
      (** Bytes the frontend already holds (a clipboard paste, or a CLI-read
          file). [media_type] is a hint; the authoritative format is sniffed
          from the bytes. *)

type caps = { max_bytes : int; max_dimension : int; max_count : int }
(** The configured attach caps: the binding byte cap on the decoded image, the
    per-side decompression-bomb ceiling, and the images-per-input count. *)

val check : caps -> string -> (Mentat_llm.Image.Format.t, Rejection.t) result
(** [check caps bytes] is the pure caps decision over one already-(maybe-)
    downscaled image: it sniffs the format (magic bytes, never an extension),
    size-checks against {!caps.max_bytes}, and dimension-checks against
    {!caps.max_dimension}, returning the format or the first cap it violates.
    The executable runs any downscale before calling this, and both the CLI and
    the daemon share this one policy — the composition supplies only the IO
    (file read, downscale spawn, blob store). *)

val check_count : caps -> count:int -> (unit, Rejection.t) result
(** [check_count caps ~count] is [Error (Too_many_images …)] when [count]
    exceeds {!caps.max_count}, else [Ok ()] — the input-level half of the
    policy. *)

module Error : sig
  (** Why an attach did not yield a content block. *)

  type t =
    | Rejected of Rejection.t  (** Failed the caps/format/dimension checks. *)
    | Not_found  (** A {!Path} did not resolve to a readable file. *)
    | Not_an_image  (** The bytes' magic number matched no image format. *)
    | Unavailable of Mentat_diagnostic.t
        (** The attachment store or a read failed — an engine-side fault, not a
            client mistake. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] formats a human-readable attach error. *)
end
