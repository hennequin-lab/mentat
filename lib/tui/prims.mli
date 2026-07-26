(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Shared Mosaic view primitives with no surface-specific geometry policy. *)

val per_palette : ('a -> 'b) -> 'a -> 'b
(** [per_palette make] memoizes [make] on the {e physical identity} of its
    argument: it returns the cached result while the argument is [==] to the
    previous one and reruns [make] on a new value. It keys palette-derived
    values (Markdown style resolvers, syntax closures) so Mosaic's
    physical-equality comparison sees a stable value each frame and re-derives
    only when the palette is hot-swapped. *)

val seg : Mosaic.Ansi.Style.t -> string -> 'msg Mosaic.t
(** [seg style text] is [text] as one non-wrapping, non-shrinking intrinsic
    segment in [style]. *)

val normalize_inline : string -> string
(** [normalize_inline text] is valid, inert, single-line UTF-8. Tabs and line
    boundaries become spaces; malformed input and C0/C1 terminal controls become
    [U+FFFD]. *)

val group_digits : int -> string
(** [group_digits n] is the exact decimal spelling of [n] with a comma between
    each group of three digits from the units place ([12345] becomes
    ["12,345"]). Negative values keep a leading ['-']. *)

val compact_count : int -> string
(** [compact_count n] is a compact magnitude label for a non-negative count: the
    plain integer below [1000], one decimal thousands below [100_000]
    (["9.4K"]), whole thousands below [1_000_000] (["142K"]), and one decimal
    millions above (["1.2M"]). *)
