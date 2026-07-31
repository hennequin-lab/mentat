
(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type nat =
| O
| S of nat

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val length : 'a1 list -> nat **)

let rec length = function
| [] -> O
| _ :: l' -> S (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

type 'seg path = 'seg list

(** val path_eqb : ('a1 -> 'a1 -> bool) -> 'a1 path -> 'a1 path -> bool **)

let rec path_eqb seg_eqb a b =
  match a with
  | [] -> (match b with
           | [] -> true
           | _ :: _ -> false)
  | x :: a' ->
    (match b with
     | [] -> false
     | y :: b' -> (&&) (seg_eqb x y) (path_eqb seg_eqb a' b'))

(** val withinb : ('a1 -> 'a1 -> bool) -> 'a1 path -> 'a1 path -> bool **)

let rec withinb seg_eqb root p =
  match root with
  | [] -> true
  | r :: root' ->
    (match p with
     | [] -> false
     | s :: p' -> (&&) (seg_eqb r s) (withinb seg_eqb root' p'))

(** val strictly_withinb :
    ('a1 -> 'a1 -> bool) -> 'a1 path -> 'a1 path -> bool **)

let strictly_withinb seg_eqb root p =
  (&&) (withinb seg_eqb root p) (negb (path_eqb seg_eqb root p))

type access =
| Read
| Write
| Deny

(** val acc_eqb : access -> access -> bool **)

let acc_eqb a b =
  match a with
  | Read -> (match b with
             | Read -> true
             | _ -> false)
  | Write -> (match b with
              | Write -> true
              | _ -> false)
  | Deny -> (match b with
             | Deny -> true
             | _ -> false)

(** val acc_geb : access -> access -> bool **)

let acc_geb a b =
  match a with
  | Read -> (match b with
             | Read -> true
             | _ -> false)
  | Write -> (match b with
              | Deny -> false
              | _ -> true)
  | Deny -> true

(** val acc_gtb : access -> access -> bool **)

let acc_gtb a b =
  (&&) (acc_geb a b) (negb (acc_eqb a b))

(** val acc_merge : access -> access -> access **)

let acc_merge existing incoming =
  if acc_geb existing incoming then existing else incoming

type 'seg entry = 'seg path * access

(** val nat_leb : nat -> nat -> bool **)

let rec nat_leb m n =
  match m with
  | O -> true
  | S m' -> (match n with
             | O -> false
             | S n' -> nat_leb m' n')

(** val nat_ltb : nat -> nat -> bool **)

let nat_ltb m n =
  nat_leb (S m) n

(** val depth : 'a1 path -> nat **)

let depth =
  length

(** val upsert :
    ('a1 -> 'a1 -> bool) -> 'a1 entry -> 'a1 entry list -> 'a1 entry list **)

let rec upsert seg_eqb e = function
| [] -> e :: []
| e0 :: rest ->
  let (q, b) = e0 in
  if path_eqb seg_eqb q (fst e)
  then (q, (acc_merge b (snd e))) :: rest
  else (q, b) :: (upsert seg_eqb e rest)

(** val dedup_into :
    ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry list -> 'a1 entry list **)

let rec dedup_into seg_eqb acc = function
| [] -> acc
| e :: rest -> dedup_into seg_eqb (upsert seg_eqb e acc) rest

(** val dedup : ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry list **)

let dedup seg_eqb l =
  dedup_into seg_eqb [] l

(** val entry_leb :
    ('a1 path -> 'a1 path -> bool) -> 'a1 entry -> 'a1 entry -> bool **)

let entry_leb path_leb a b =
  if nat_ltb (depth (fst a)) (depth (fst b))
  then true
  else if nat_ltb (depth (fst b)) (depth (fst a))
       then false
       else path_leb (fst a) (fst b)

(** val insert_sorted :
    ('a1 path -> 'a1 path -> bool) -> 'a1 entry -> 'a1 entry list -> 'a1 entry
    list **)

let rec insert_sorted path_leb e = function
| [] -> e :: []
| x :: rest ->
  if entry_leb path_leb e x
  then e :: (x :: rest)
  else x :: (insert_sorted path_leb e rest)

(** val sort_entries :
    ('a1 path -> 'a1 path -> bool) -> 'a1 entry list -> 'a1 entry list **)

let rec sort_entries path_leb = function
| [] -> []
| e :: rest -> insert_sorted path_leb e (sort_entries path_leb rest)

(** val normalize :
    ('a1 -> 'a1 -> bool) -> ('a1 path -> 'a1 path -> bool) -> 'a1 entry list
    -> 'a1 entry list **)

let normalize seg_eqb path_leb l =
  sort_entries path_leb (dedup seg_eqb l)

type reads_default =
| All
| Denied

(** val default_access : reads_default -> access **)

let default_access = function
| All -> Read
| Denied -> Deny

(** val deepest :
    ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 path -> (nat * access)
    option **)

let rec deepest seg_eqb l p =
  match l with
  | [] -> None
  | e :: rest ->
    let (q, a) = e in
    let best = deepest seg_eqb rest p in
    if withinb seg_eqb q p
    then (match best with
          | Some p0 ->
            let (d, b) = p0 in
            if nat_ltb d (depth q)
            then Some ((depth q), a)
            else if nat_ltb (depth q) d
                 then Some (d, b)
                 else Some (d, (acc_merge a b))
          | None -> Some ((depth q), a))
    else best

(** val resolve :
    ('a1 -> 'a1 -> bool) -> 'a1 entry list -> reads_default -> 'a1 path ->
    access **)

let resolve seg_eqb l d p =
  match deepest seg_eqb l p with
  | Some p0 -> let (_, a) = p0 in a
  | None -> default_access d

(** val last_covering :
    ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 path -> access option **)

let rec last_covering seg_eqb l p =
  match l with
  | [] -> None
  | e :: rest ->
    let (q, a) = e in
    (match last_covering seg_eqb rest p with
     | Some b -> Some b
     | None -> if withinb seg_eqb q p then Some a else None)

(** val emitted :
    ('a1 -> 'a1 -> bool) -> 'a1 entry list -> reads_default -> 'a1 path ->
    access **)

let emitted seg_eqb l d p =
  match last_covering seg_eqb l p with
  | Some a -> a
  | None -> default_access d

(** val denied_roots : 'a1 entry list -> 'a1 path list **)

let rec denied_roots = function
| [] -> []
| e :: rest ->
  let (q, a) = e in
  (match a with
   | Deny -> q :: (denied_roots rest)
   | _ -> denied_roots rest)

(** val find_path : ('a1 path -> bool) -> 'a1 path list -> 'a1 path option **)

let rec find_path f = function
| [] -> None
| q :: rest -> if f q then Some q else find_path f rest

(** val find_entry :
    ('a1 entry -> bool) -> 'a1 entry list -> 'a1 entry option **)

let rec find_entry f = function
| [] -> None
| e :: rest -> if f e then Some e else find_entry f rest

(** val any_entry : ('a1 entry -> bool) -> 'a1 entry list -> bool **)

let rec any_entry f = function
| [] -> false
| e :: rest -> (||) (f e) (any_entry f rest)

(** val filter_entries :
    ('a1 entry -> bool) -> 'a1 entry list -> 'a1 entry list **)

let rec filter_entries f = function
| [] -> []
| e :: rest ->
  if f e then e :: (filter_entries f rest) else filter_entries f rest

(** val is_carveout :
    ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry -> bool **)

let is_carveout seg_eqb all e =
  any_entry (fun enc ->
    (&&) (strictly_withinb seg_eqb (fst enc) (fst e))
      (acc_gtb (snd enc) (snd e)))
    all

type 'seg grant_result =
| Granted of 'seg entry list
| Refused of 'seg path * 'seg path

(** val first_defeat :
    ('a1 entry -> ('a1 path * 'a1 path) option) -> 'a1 entry list -> ('a1
    path * 'a1 path) option **)

let rec first_defeat f = function
| [] -> None
| g :: rest -> (match f g with
                | Some d -> Some d
                | None -> first_defeat f rest)

(** val opt_or :
    ('a1 path * 'a1 path) option -> ('a1 path * 'a1 path) option -> ('a1
    path * 'a1 path) option **)

let opt_or a b =
  match a with
  | Some _ -> a
  | None -> b

(** val root_defeat : 'a1 entry -> ('a1 path * 'a1 path) option **)

let root_defeat g =
  match fst g with
  | [] -> Some ((fst g), (fst g))
  | _ :: _ -> None

(** val denied_defeat :
    ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry -> ('a1 path * 'a1
    path) option **)

let denied_defeat seg_eqb t_entries g =
  match find_path (fun r -> withinb seg_eqb r (fst g))
          (denied_roots t_entries) with
  | Some r -> Some ((fst g), r)
  | None -> None

(** val carveout_defeat :
    ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry -> ('a1 path * 'a1
    path) option **)

let carveout_defeat seg_eqb t_entries g =
  match find_entry (fun c ->
          (&&) (withinb seg_eqb (fst c) (fst g)) (acc_gtb (snd g) (snd c)))
          (filter_entries (is_carveout seg_eqb t_entries) t_entries) with
  | Some c -> Some ((fst g), (fst c))
  | None -> None

(** val defeat_check :
    ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry -> ('a1 path * 'a1
    path) option **)

let defeat_check seg_eqb t_entries g =
  opt_or (root_defeat g)
    (opt_or (denied_defeat seg_eqb t_entries g)
      (carveout_defeat seg_eqb t_entries g))

(** val grant :
    ('a1 -> 'a1 -> bool) -> ('a1 path -> 'a1 path -> bool) -> 'a1 entry list
    -> 'a1 entry list -> 'a1 grant_result **)

let grant seg_eqb path_leb t_entries gs =
  match first_defeat (defeat_check seg_eqb t_entries) gs with
  | Some p0 -> let (p, r) = p0 in Refused (p, r)
  | None -> Granted (normalize seg_eqb path_leb (app t_entries gs))

(** val deny_paths : 'a1 path list -> 'a1 entry list **)

let rec deny_paths = function
| [] -> []
| q :: rest -> (q, Deny) :: (deny_paths rest)

(** val floor : 'a1 entry list -> 'a1 entry list **)

let floor l =
  ([], Write) :: (deny_paths (denied_roots l))
