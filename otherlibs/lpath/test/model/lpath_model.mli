
val negb : bool -> bool

type nat =
| O
| S of nat

val length : 'a1 list -> nat

val app : 'a1 list -> 'a1 list -> 'a1 list

val eqb_chars : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val forallb_byte : ('a1 -> bool) -> 'a1 list -> bool

val is_dot : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val is_dotdot : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val is_component_char :
  ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> bool

val has_drive_prefix :
  ('a1 -> 'a1 -> bool) -> 'a1 -> ('a1 -> bool) -> 'a1 list -> bool

val is_nil : 'a1 list -> bool

val malformed :
  ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> ('a1 -> bool) ->
  'a1 list -> bool

val flush : 'a1 list -> 'a1 list list

val split_acc :
  ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list -> 'a1 list list

val split_slash : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list list

val join : 'a1 -> 'a1 list list -> 'a1 list

val canon : 'a1 -> 'a1 list list -> 'a1 list

type 'byte perror =
| Empty
| Relative
| Malformed of 'byte list

type 'byte presult =
| POk of 'byte list
| PErr of 'byte perror

val rev_stack : 'a1 list list -> 'a1 list list -> 'a1 list list

val build_abs : 'a1 -> 'a1 list list -> 'a1 list

val resolve_onto :
  ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> ('a1 -> bool) ->
  'a1 list list -> 'a1 list list -> 'a1 presult

val starts_with_slash : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val abs_of_string :
  ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> ('a1 -> bool) ->
  'a1 list -> 'a1 presult

val is_root : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val abs_components : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list list

val prefixb : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val nth_err : 'a1 list -> nat -> 'a1 option

val below_someb : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list -> bool

val abs_within : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list -> bool

val abs_strictly_within :
  ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list -> bool
