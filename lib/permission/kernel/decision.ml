
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

(** val explanation_of_rule : ('a1 -> action) -> 'a1 -> 'a1 explanation **)

let explanation_of_rule rule_action r =
  match rule_action r with
  | Allow -> Allowed_by_rule r
  | Review -> Needs_review_by_rule r
  | Deny -> Denied_by_rule r

(** val explain :
    ('a2 -> 'a1 -> bool) -> ('a3 -> 'a2) -> ('a3 -> action) -> ('a1 -> bool)
    -> 'a3 list -> 'a1 -> 'a3 explanation **)

let rec explain matches rule_matcher rule_action allows rules a =
  match rules with
  | [] -> if allows a then Allowed_by_grant else Needs_review
  | r :: rest ->
    if matches (rule_matcher r) a
    then explanation_of_rule rule_action r
    else explain matches rule_matcher rule_action allows rest a

type 'rule reason =
| Unmatched
| By_rule of 'rule

type ('access, 'rule) outcome =
| Allowed
| Reviewed of ('access * 'rule reason) list
| Denied of ('access * 'rule) * ('access * 'rule) list

(** val gather :
    ('a2 -> 'a1 -> bool) -> ('a3 -> 'a2) -> ('a3 -> action) -> ('a1 -> bool)
    -> 'a3 list -> 'a1 list -> ('a1 * 'a3) list * ('a1 * 'a3 reason) list **)

let rec gather matches rule_matcher rule_action allows rules = function
| [] -> ([], [])
| a :: rest ->
  let (denials, reviews) =
    gather matches rule_matcher rule_action allows rules rest
  in
  (match explain matches rule_matcher rule_action allows rules a with
   | Needs_review -> (denials, ((a, Unmatched) :: reviews))
   | Needs_review_by_rule r -> (denials, ((a, (By_rule r)) :: reviews))
   | Denied_by_rule r -> (((a, r) :: denials), reviews)
   | _ -> (denials, reviews))

(** val classify :
    ('a2 -> 'a1 -> bool) -> ('a3 -> 'a2) -> ('a3 -> action) -> ('a1 -> bool)
    -> 'a3 list -> 'a1 list -> ('a1, 'a3) outcome **)

let classify matches rule_matcher rule_action allows rules accesses =
  let (denials, reviews) =
    gather matches rule_matcher rule_action allows rules accesses
  in
  (match denials with
   | [] -> (match reviews with
            | [] -> Allowed
            | _ :: _ -> Reviewed reviews)
   | p :: rest -> Denied (p, rest))
