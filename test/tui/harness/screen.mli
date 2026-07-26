(** Complete-screen golden formatting for TUI tests.

    Observable TUI states are recorded with {!print}: it normalizes only
    machine-dependent text, preserves the complete terminal grid, and prefixes
    every row with its one-based position. Predicate helpers are exclusively for
    polling until a state is ready; they never replace a final call to {!print}.
*)

val contains : string -> string -> bool
(** [contains text fragment] is [true] iff [fragment] occurs in [text]. This is
    a synchronization predicate, not a visual assertion. *)

val has : string -> string -> bool
(** [has fragment screen] is [true] iff [screen] contains [fragment]. Use it to
    recognize a ready frame, then pass that complete frame to {!print}. *)

val print : project:Project.t -> string -> unit
(** [print ~project screen] writes every row of the normalized complete screen,
    prefixed by a zero-padded, one-based row number. Trailing spaces are removed
    from each row; blank rows and their positions are preserved. This is the
    sole formatter for visual TUI goldens. *)
