
type action =
| Allow
| Review
| Deny

type 'rule explanation =
| Allowed_by_rule of 'rule
| Allowed_by_grant
| Needs_review
| Needs_review_by_rule of 'rule
| Denied_by_rule of 'rule

val explanation_of_rule : ('a1 -> action) -> 'a1 -> 'a1 explanation

val explain :
  ('a2 -> 'a1 -> bool) -> ('a3 -> 'a2) -> ('a3 -> action) -> ('a1 -> bool) ->
  'a3 list -> 'a1 -> 'a3 explanation

type 'rule reason =
| Unmatched
| By_rule of 'rule

type ('access, 'rule) outcome =
| Allowed
| Reviewed of ('access * 'rule reason) list
| Denied of ('access * 'rule) * ('access * 'rule) list

val gather :
  ('a2 -> 'a1 -> bool) -> ('a3 -> 'a2) -> ('a3 -> action) -> ('a1 -> bool) ->
  'a3 list -> 'a1 list -> ('a1 * 'a3) list * ('a1 * 'a3 reason) list

val classify :
  ('a2 -> 'a1 -> bool) -> ('a3 -> 'a2) -> ('a3 -> action) -> ('a1 -> bool) ->
  'a3 list -> 'a1 list -> ('a1, 'a3) outcome
