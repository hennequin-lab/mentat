(** Verified model of the confinement kernel.

    This is the formal counterpart of the three laws [policy.mli] states in
    prose: the resolution law (a path's access is the access of the deepest
    clause whose path is a prefix of it; where two clauses name one path, the
    stronger wins; a path no clause covers takes the default and is never
    writable), the normalization law (duplicate paths collapse to their
    strongest access, clauses order shallowest-first), and the widening law
    ([grant] refuses exactly what a denied path or a carveout would defeat).

    The model mirrors [policy.ml] operation for operation so the extracted
    code and the OCaml implementation are comparable entry list for entry
    list. Segments stay abstract: the kernel needs only their equality, and
    the equal-depth tie-break used by normalization is a parameter because no
    law depends on which total order it is. *)

From Corelib Require Import Init.Prelude.

Open Scope list_scope.

Section Confinement.

  Variable seg : Type.
  Variable seg_eqb : seg -> seg -> bool.

  Definition path : Type := list seg.

  (* The equal-depth tie-break of normalization's sort: [Lpath.Abs.compare]
     on the OCaml side. No law depends on it; it only makes emission order
     deterministic. *)
  Variable path_leb : path -> path -> bool.

  Fixpoint path_eqb (a b : path) : bool :=
    match a, b with
    | nil, nil => true
    | nil, _ :: _ => false
    | _ :: _, nil => false
    | x :: a', y :: b' => andb (seg_eqb x y) (path_eqb a' b')
    end.

  (** [withinb root p] is [Lpath.Abs.is_within ~root p]: inclusive
      containment, [root] is a prefix of [p]. *)
  Fixpoint withinb (root p : path) : bool :=
    match root, p with
    | nil, _ => true
    | _ :: _, nil => false
    | r :: root', s :: p' => andb (seg_eqb r s) (withinb root' p')
    end.

  Definition strictly_withinb (root p : path) : bool :=
    andb (withinb root p) (negb (path_eqb root p)).

  (** What one clause grants over the path it names and everything beneath
      it. Precedence settles a tie between two clauses on one path — not an
      ordering by permissiveness: a write outranks a read, a denial outranks
      both ([Access.compare] in policy.ml). *)
  Inductive access : Type := Read | Write | Deny.

  Definition acc_eqb (a b : access) : bool :=
    match a, b with
    | Read, Read | Write, Write | Deny, Deny => true
    | _, _ => false
    end.

  (* rank a >= rank b, with rank Read = 0 < rank Write = 1 < rank Deny = 2. *)
  Definition acc_geb (a b : access) : bool :=
    match a, b with
    | Deny, _ => true
    | Write, Deny => false
    | Write, _ => true
    | Read, Read => true
    | Read, _ => false
    end.

  Definition acc_gtb (a b : access) : bool :=
    andb (acc_geb a b) (negb (acc_eqb a b)).

  (* The dedup merge: keep the existing clause unless the incoming one is
     strictly stronger, exactly the hashtable update in [normalize]. *)
  Definition acc_merge (existing incoming : access) : access :=
    if acc_geb existing incoming then existing else incoming.

  Definition entry : Type := (path * access)%type.

  Fixpoint nat_leb (m n : nat) : bool :=
    match m, n with
    | O, _ => true
    | S _, O => false
    | S m', S n' => nat_leb m' n'
    end.

  Definition nat_ltb (m n : nat) : bool := nat_leb (S m) n.

  (** Specificity is the component count ([depth] in policy.ml). *)
  Definition depth (p : path) : nat := length p.

  (** {1 Normalization} *)

  Fixpoint upsert (e : entry) (l : list entry) : list entry :=
    match l with
    | nil => e :: nil
    | (q, b) :: rest =>
        if path_eqb q (fst e) then (q, acc_merge b (snd e)) :: rest
        else (q, b) :: upsert e rest
    end.

  Fixpoint dedup_into (acc l : list entry) : list entry :=
    match l with
    | nil => acc
    | e :: rest => dedup_into (upsert e acc) rest
    end.

  Definition dedup (l : list entry) : list entry := dedup_into nil l.

  (* Shallowest-first; equal depth falls back to the abstract total order,
     as [normalize]'s sort falls back to [Lpath.Abs.compare]. *)
  Definition entry_leb (a b : entry) : bool :=
    if nat_ltb (depth (fst a)) (depth (fst b)) then true
    else if nat_ltb (depth (fst b)) (depth (fst a)) then false
    else path_leb (fst a) (fst b).

  Fixpoint insert_sorted (e : entry) (l : list entry) : list entry :=
    match l with
    | nil => e :: nil
    | x :: rest =>
        if entry_leb e x then e :: x :: rest else x :: insert_sorted e rest
    end.

  Fixpoint sort_entries (l : list entry) : list entry :=
    match l with
    | nil => nil
    | e :: rest => insert_sorted e (sort_entries rest)
    end.

  (** The normalization law: duplicate paths collapse to their strongest
      access, clauses order shallowest-first. *)
  Definition normalize (l : list entry) : list entry := sort_entries (dedup l).

  (** {1 Resolution} *)

  Inductive reads_default : Type := All | Denied.

  (* A path no clause covers takes the default and is never writable. *)
  Definition default_access (d : reads_default) : access :=
    match d with All => Read | Denied => Deny end.

  (* The deepest covering clause and its access; two clauses at one depth
     covering the same path necessarily name the same path, and the stronger
     wins. *)
  Fixpoint deepest (l : list entry) (p : path) : option (nat * access) :=
    match l with
    | nil => None
    | (q, a) :: rest =>
        let best := deepest rest p in
        if withinb q p then
          match best with
          | None => Some (depth q, a)
          | Some (d, b) =>
              if nat_ltb d (depth q) then Some (depth q, a)
              else if nat_ltb (depth q) d then Some (d, b)
              else Some (d, acc_merge a b)
          end
        else best
    end.

  (** The resolution law, as an executable function over raw clauses. *)
  Definition resolve (l : list entry) (d : reads_default) (p : path) : access :=
    match deepest l p with
    | None => default_access d
    | Some (_, a) => a
    end.

  (* What both backends implement: the last emitted clause covering a path
     wins — SBPL by rule order, bubblewrap by mount order. *)
  Fixpoint last_covering (l : list entry) (p : path) : option access :=
    match l with
    | nil => None
    | (q, a) :: rest =>
        match last_covering rest p with
        | Some b => Some b
        | None => if withinb q p then Some a else None
        end
    end.

  (** Backend semantics over an emitted entry list. *)
  Definition emitted (l : list entry) (d : reads_default) (p : path) : access :=
    match last_covering l p with
    | None => default_access d
    | Some a => a
    end.

  (** {1 Widening} *)

  Fixpoint denied_roots (l : list entry) : list path :=
    match l with
    | nil => nil
    | (q, Deny) :: rest => q :: denied_roots rest
    | _ :: rest => denied_roots rest
    end.

  Fixpoint find_path (f : path -> bool) (l : list path) : option path :=
    match l with
    | nil => None
    | q :: rest => if f q then Some q else find_path f rest
    end.

  Fixpoint find_entry (f : entry -> bool) (l : list entry) : option entry :=
    match l with
    | nil => None
    | e :: rest => if f e then Some e else find_entry f rest
    end.

  Fixpoint any_entry (f : entry -> bool) (l : list entry) : bool :=
    match l with
    | nil => false
    | e :: rest => orb (f e) (any_entry f rest)
    end.

  Fixpoint filter_entries (f : entry -> bool) (l : list entry) : list entry :=
    match l with
    | nil => nil
    | e :: rest =>
        if f e then e :: filter_entries f rest else filter_entries f rest
    end.

  (* A clause lowered on purpose beneath a stronger one: [.git] inside the
     workspace, the dune store inside its cache. What a grant must not raise. *)
  Definition is_carveout (all : list entry) (e : entry) : bool :=
    any_entry
      (fun enc =>
         andb (strictly_withinb (fst enc) (fst e)) (acc_gtb (snd enc) (snd e)))
      all.

  Inductive grant_result : Type :=
  | Granted (entries : list entry)
  | Refused (granted defeated_by : path).

  (* Refusal checks in [grant]'s order: the root spelling, then a denied
     root, then a carveout the grant would out-rank. First offending granted
     entry wins. *)
  Fixpoint first_defeat (f : entry -> option (path * path)) (gs : list entry) :
      option (path * path) :=
    match gs with
    | nil => None
    | g :: rest =>
        match f g with
        | Some d => Some d
        | None => first_defeat f rest
        end
    end.

  Definition opt_or (a b : option (path * path)) : option (path * path) :=
    match a with
    | Some _ => a
    | None => b
    end.

  (* The three refusals of one granted entry, in [grant]'s order: the root
     spelling, then a denied root, then a carveout the grant would out-rank. *)

  Definition root_defeat (g : entry) : option (path * path) :=
    match fst g with
    | nil => Some (fst g, fst g)
    | _ :: _ => None
    end.

  Definition denied_defeat (t_entries : list entry) (g : entry) :
      option (path * path) :=
    match find_path (fun r => withinb r (fst g)) (denied_roots t_entries) with
    | Some r => Some (fst g, r)
    | None => None
    end.

  Definition carveout_defeat (t_entries : list entry) (g : entry) :
      option (path * path) :=
    match
      find_entry
        (fun c => andb (withinb (fst c) (fst g)) (acc_gtb (snd g) (snd c)))
        (filter_entries (is_carveout t_entries) t_entries)
    with
    | Some c => Some (fst g, fst c)
    | None => None
    end.

  Definition defeat_check (t_entries : list entry) (g : entry) :
      option (path * path) :=
    opt_or (root_defeat g)
      (opt_or (denied_defeat t_entries g) (carveout_defeat t_entries g)).

  (** The widening law: a successful grant is [normalize (existing ++ gs)] —
      an added clause takes part in resolution like any other — and the cases
      that law would swallow silently are refused instead. *)
  Definition grant (t_entries gs : list entry) : grant_result :=
    match first_defeat (defeat_check t_entries) gs with
    | Some (p, r) => Refused p r
    | None => Granted (normalize (app t_entries gs))
    end.

End Confinement.
