
val negb : bool -> bool

type nat =
| O
| S of nat

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val length : 'a1 list -> nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type 'seg path = 'seg list

val path_eqb : ('a1 -> 'a1 -> bool) -> 'a1 path -> 'a1 path -> bool

val withinb : ('a1 -> 'a1 -> bool) -> 'a1 path -> 'a1 path -> bool

val strictly_withinb : ('a1 -> 'a1 -> bool) -> 'a1 path -> 'a1 path -> bool

type access =
| Read
| Write
| Deny

val acc_eqb : access -> access -> bool

val acc_geb : access -> access -> bool

val acc_gtb : access -> access -> bool

val acc_merge : access -> access -> access

type 'seg entry = 'seg path * access

val nat_leb : nat -> nat -> bool

val nat_ltb : nat -> nat -> bool

val depth : 'a1 path -> nat

val upsert :
  ('a1 -> 'a1 -> bool) -> 'a1 entry -> 'a1 entry list -> 'a1 entry list

val dedup_into :
  ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry list -> 'a1 entry list

val dedup : ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry list

val entry_leb :
  ('a1 path -> 'a1 path -> bool) -> 'a1 entry -> 'a1 entry -> bool

val insert_sorted :
  ('a1 path -> 'a1 path -> bool) -> 'a1 entry -> 'a1 entry list -> 'a1 entry
  list

val sort_entries :
  ('a1 path -> 'a1 path -> bool) -> 'a1 entry list -> 'a1 entry list

val normalize :
  ('a1 -> 'a1 -> bool) -> ('a1 path -> 'a1 path -> bool) -> 'a1 entry list ->
  'a1 entry list

type reads_default =
| All
| Denied

val default_access : reads_default -> access

val deepest :
  ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 path -> (nat * access) option

val resolve :
  ('a1 -> 'a1 -> bool) -> 'a1 entry list -> reads_default -> 'a1 path ->
  access

val last_covering :
  ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 path -> access option

val emitted :
  ('a1 -> 'a1 -> bool) -> 'a1 entry list -> reads_default -> 'a1 path ->
  access

val denied_roots : 'a1 entry list -> 'a1 path list

val find_path : ('a1 path -> bool) -> 'a1 path list -> 'a1 path option

val find_entry : ('a1 entry -> bool) -> 'a1 entry list -> 'a1 entry option

val any_entry : ('a1 entry -> bool) -> 'a1 entry list -> bool

val filter_entries : ('a1 entry -> bool) -> 'a1 entry list -> 'a1 entry list

val is_carveout : ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry -> bool

type 'seg grant_result =
| Granted of 'seg entry list
| Refused of 'seg path * 'seg path

val first_defeat :
  ('a1 entry -> ('a1 path * 'a1 path) option) -> 'a1 entry list -> ('a1
  path * 'a1 path) option

val opt_or :
  ('a1 path * 'a1 path) option -> ('a1 path * 'a1 path) option -> ('a1
  path * 'a1 path) option

val root_defeat : 'a1 entry -> ('a1 path * 'a1 path) option

val denied_defeat :
  ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry -> ('a1 path * 'a1 path)
  option

val carveout_defeat :
  ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry -> ('a1 path * 'a1 path)
  option

val defeat_check :
  ('a1 -> 'a1 -> bool) -> 'a1 entry list -> 'a1 entry -> ('a1 path * 'a1 path)
  option

val grant :
  ('a1 -> 'a1 -> bool) -> ('a1 path -> 'a1 path -> bool) -> 'a1 entry list ->
  'a1 entry list -> 'a1 grant_result
