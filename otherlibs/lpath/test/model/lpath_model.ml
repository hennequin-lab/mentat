
(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type nat =
| O
| S of nat

(** val length : 'a1 list -> nat **)

let rec length = function
| [] -> O
| _ :: l' -> S (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

(** val eqb_chars : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec eqb_chars byte_eqb a b =
  match a with
  | [] -> (match b with
           | [] -> true
           | _ :: _ -> false)
  | x :: a' ->
    (match b with
     | [] -> false
     | y :: b' -> (&&) (byte_eqb x y) (eqb_chars byte_eqb a' b'))

(** val forallb_byte : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb_byte f = function
| [] -> true
| b :: l' -> (&&) (f b) (forallb_byte f l')

(** val is_dot : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let is_dot byte_eqb dot s =
  eqb_chars byte_eqb s (dot :: [])

(** val is_dotdot : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let is_dotdot byte_eqb dot s =
  eqb_chars byte_eqb s (dot :: (dot :: []))

(** val is_component_char :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> bool **)

let is_component_char byte_eqb slash backslash nul b =
  (&&) (negb (byte_eqb b nul))
    ((&&) (negb (byte_eqb b slash)) (negb (byte_eqb b backslash)))

(** val has_drive_prefix :
    ('a1 -> 'a1 -> bool) -> 'a1 -> ('a1 -> bool) -> 'a1 list -> bool **)

let has_drive_prefix byte_eqb colon is_letter = function
| [] -> false
| c :: l ->
  (match l with
   | [] -> false
   | d :: _ -> (&&) (is_letter c) (byte_eqb d colon))

(** val is_nil : 'a1 list -> bool **)

let is_nil = function
| [] -> true
| _ :: _ -> false

(** val malformed :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> ('a1 -> bool)
    -> 'a1 list -> bool **)

let malformed byte_eqb slash dot backslash nul colon is_letter s =
  (||) (is_nil s)
    ((||) (is_dot byte_eqb dot s)
      ((||) (is_dotdot byte_eqb dot s)
        ((||) (has_drive_prefix byte_eqb colon is_letter s)
          (negb
            (forallb_byte (is_component_char byte_eqb slash backslash nul) s)))))

(** val flush : 'a1 list -> 'a1 list list **)

let flush cur = match cur with
| [] -> []
| _ :: _ -> cur :: []

(** val split_acc :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list -> 'a1 list list **)

let rec split_acc byte_eqb slash cur = function
| [] -> flush cur
| b :: rest ->
  if byte_eqb b slash
  then app (flush cur) (split_acc byte_eqb slash [] rest)
  else split_acc byte_eqb slash (app cur (b :: [])) rest

(** val split_slash :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list list **)

let split_slash byte_eqb slash bs =
  split_acc byte_eqb slash [] bs

(** val join : 'a1 -> 'a1 list list -> 'a1 list **)

let rec join slash = function
| [] -> []
| s :: rest ->
  (match rest with
   | [] -> s
   | _ :: _ -> app s (slash :: (join slash rest)))

(** val canon : 'a1 -> 'a1 list list -> 'a1 list **)

let canon slash segs =
  slash :: (join slash segs)

type 'byte perror =
| Empty
| Relative
| Malformed of 'byte list

type 'byte presult =
| POk of 'byte list
| PErr of 'byte perror

(** val rev_stack : 'a1 list list -> 'a1 list list -> 'a1 list list **)

let rec rev_stack l acc =
  match l with
  | [] -> acc
  | x :: l' -> rev_stack l' (x :: acc)

(** val build_abs : 'a1 -> 'a1 list list -> 'a1 list **)

let build_abs slash stack =
  canon slash (rev_stack stack [])

(** val resolve_onto :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> ('a1 -> bool)
    -> 'a1 list list -> 'a1 list list -> 'a1 presult **)

let rec resolve_onto byte_eqb slash dot backslash nul colon is_letter stack = function
| [] -> POk (build_abs slash stack)
| s :: rest ->
  if is_dot byte_eqb dot s
  then resolve_onto byte_eqb slash dot backslash nul colon is_letter stack
         rest
  else if is_dotdot byte_eqb dot s
       then (match stack with
             | [] ->
               resolve_onto byte_eqb slash dot backslash nul colon is_letter
                 [] rest
             | _ :: stk ->
               resolve_onto byte_eqb slash dot backslash nul colon is_letter
                 stk rest)
       else if malformed byte_eqb slash dot backslash nul colon is_letter s
            then PErr (Malformed s)
            else resolve_onto byte_eqb slash dot backslash nul colon is_letter
                   (s :: stack) rest

(** val starts_with_slash :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let starts_with_slash byte_eqb slash = function
| [] -> false
| b :: _ -> byte_eqb b slash

(** val abs_of_string :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> 'a1 -> ('a1 -> bool)
    -> 'a1 list -> 'a1 presult **)

let abs_of_string byte_eqb slash dot backslash nul colon is_letter s =
  if is_nil s
  then PErr Empty
  else if starts_with_slash byte_eqb slash s
       then resolve_onto byte_eqb slash dot backslash nul colon is_letter []
              (split_slash byte_eqb slash s)
       else PErr Relative

(** val is_root : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let is_root byte_eqb slash t =
  eqb_chars byte_eqb t (slash :: [])

(** val abs_components :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list list **)

let abs_components byte_eqb slash t =
  if is_root byte_eqb slash t then [] else split_slash byte_eqb slash t

(** val prefixb : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec prefixb byte_eqb p q =
  match p with
  | [] -> true
  | a :: p' ->
    (match q with
     | [] -> false
     | b :: q' -> (&&) (byte_eqb a b) (prefixb byte_eqb p' q'))

(** val nth_err : 'a1 list -> nat -> 'a1 option **)

let rec nth_err l n =
  match l with
  | [] -> None
  | b :: l' -> (match n with
                | O -> Some b
                | S n' -> nth_err l' n')

(** val below_someb :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list -> bool **)

let below_someb byte_eqb slash prefix t =
  match nth_err t (length prefix) with
  | Some b -> (&&) (byte_eqb b slash) (prefixb byte_eqb prefix t)
  | None -> false

(** val abs_within :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list -> bool **)

let abs_within byte_eqb slash root t =
  if eqb_chars byte_eqb t root
  then true
  else if is_root byte_eqb slash root
       then true
       else below_someb byte_eqb slash root t

(** val abs_strictly_within :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list -> bool **)

let abs_strictly_within byte_eqb slash root t =
  (&&) (abs_within byte_eqb slash root t) (negb (eqb_chars byte_eqb root t))
