
type 'reason verdict =
| Available
| Unavailable of 'reason list
| Incomplete of 'reason list

(** val any_superseded : ('a1 -> bool) -> 'a1 list -> bool **)

let rec any_superseded is_superseded = function
| [] -> false
| r :: rest -> (||) (is_superseded r) (any_superseded is_superseded rest)

(** val classify : ('a1 -> bool) -> 'a1 list -> 'a1 verdict **)

let classify is_superseded reasons = match reasons with
| [] -> Available
| _ :: _ ->
  if any_superseded is_superseded reasons
  then Unavailable reasons
  else Incomplete reasons
