
(** val full_fold :
    ('a3 -> 'a1 -> ('a1, 'a2) result) -> 'a1 -> 'a3 list -> ('a1, 'a2) result **)

let rec full_fold f s = function
| [] -> Ok s
| e :: rest ->
  (match f e s with
   | Ok s' -> full_fold f s' rest
   | Error c -> Error c)
