
type 'reason verdict =
| Available
| Unavailable of 'reason list
| Incomplete of 'reason list

val any_superseded : ('a1 -> bool) -> 'a1 list -> bool

val classify : ('a1 -> bool) -> 'a1 list -> 'a1 verdict
