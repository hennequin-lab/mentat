(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Pure recognition of raster-image bytes: format and pixel dimensions from a
    prefix, with no decode and no dependency.

    Both the engine (attach validation) and the read tool answer "is this image
    bytes, what format, what dimensions" from raw bytes. The answer is pure and
    dependency-free, so it lives beside {!Content}. Sniff attachments and read
    results; do not transform them. *)

module Format : sig
  (** A recognized raster-image container format. Exposed plain data: the format
      set is closed and carries no invariant. *)

  type t = Png | Jpeg | Gif | Webp

  val media_type : t -> string
  (** [media_type t] is [t]'s canonical IANA MIME type ["image/png"],
      ["image/jpeg"], ["image/gif"], or ["image/webp"]. *)

  val of_media_type : string -> t option
  (** [of_media_type s] is the inverse of {!media_type}, case-insensitive on the
      subtype, or [None] when [s] names no supported image format. *)

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

val sniff : string -> Format.t option
(** [sniff bytes] is the format identified by [bytes]'s magic-number prefix, or
    [None]. Never trusts a file extension. A prefix as short as the magic number
    suffices. *)

type dimensions = { width : int; height : int }
(** Pixel dimensions read from a format header. *)

val dimensions : string -> dimensions option
(** [dimensions bytes] reads pixel dimensions from the format header (PNG IHDR,
    JPEG SOFn, GIF logical screen, WebP VP8/VP8L/VP8X) without decoding pixels,
    or [None] when truncated or unrecognized. For the dimension cap and UX. *)
