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

  (** {1 The escalation floor}

      Widening a confined route to its approved escape opens the root to
      writing, but must not hand back a denied path: those are denied
      because a later unconfined process consumes what is under them as
      authority, so a floor that raised one would let an approved escape
      rewrite the approval. The floor keeps exactly the deny set. *)

  Fixpoint deny_paths (qs : list path) : list entry :=
    match qs with
    | nil => nil
    | q :: rest => (q, Deny) :: deny_paths rest
    end.

  Definition floor (l : list entry) : list entry :=
    (nil, Write) :: deny_paths (denied_roots l).

  (** {1 Backend emission}

      Both backends turn a normalized policy — the [entry] list of
      [Policy.entries], already deduplicated and depth-sorted — into an ordered
      list of rules a later-wins resolver reads back. Seatbelt emits SBPL rules
      resolved per operation by rule order ([seatbelt.ml], [clause_rules]);
      bubblewrap emits mounts resolved by mount order ([bubblewrap.ml],
      [clause]/[filesystem_args]). These pure functions mirror those emitters
      clause for clause, so the correspondence proofs retire the premise that
      the emitters implement last-clause-wins.

      Kept exactly: the per-clause emission order and each clause's per-path
      match, which is [withinb] — the union of seatbelt's [(literal p)(subpath
      p)] and bubblewrap's recursive bind at [p] — and the root mount, which
      [bwrap_root] mirrors arm for arm.

      Abstracted, and why it is faithful: the exact SBPL and argv string syntax;
      bubblewrap's namespace and network flags, its [--dev] and [--proc], which
      govern disjoint non-policy resources — [--dev] and [--proc] are hoisted
      ahead of the clause mounts ([filesystem_args]), so a clause naming [/dev]
      or [/proc] is emitted later and wins, exactly as the model says; and the
      metadata-only rules ([path-ancestors], seatbelt's [file-read-metadata]),
      which do not bear on the read-data or write-data access the model tracks.

      The seatbelt theorems characterize the {e policy-derived} rules. They hold
      at every path {e outside} a fixed base-allowance set the profile grants
      before any clause, which they do not model. That set, from [base_policy]:
      read-data (so also directory listing) is allowed on the device handles
      [/dev/null], [/dev/zero], [/dev/random], [/dev/urandom],
      [/dev/autofs_nowait], [/dev/fd] (subpath), [/dev/fd/{0,1,2}], [/dev/ptmx],
      [/dev/ttys*], and on [(literal "/")] — the root path itself; write-data is
      allowed on [/dev/null], [/dev/zero], [/dev/fd], [/dev/ptmx], [/dev/ttys*].
      Every other base rule is metadata- or existence-only ([/etc], [/tmp],
      [/var], [/private/etc/localtime], the [/System/Volumes/Data/private]
      ancestors, [/dev] and the standard stdio nodes) and so lies outside the
      read-data/write-data access the theorems concern.

      The one base rule with a policy-visible effect is [(allow file-read*
      file-test-existence (literal "/"))]: under a [Denied] read default a
      confined child can still list the host root [/] (its top-level entry
      names, not their contents — [(literal "/")] matches [/] exactly, not its
      subtree). It sits with the symlink/firmlink-ancestor allowances that
      [realpath] needs, so it reads as intended base behavior rather than a
      policy leak; it is a fixed rule, independent of any policy, and therefore
      outside what these theorems characterize. *)

  (** {2 Seatbelt: last-match SBPL rules, resolved per operation} *)

  (* An SBPL rule as it bears on one path's read-data or write-data decision:
     whether it allows (or denies), and the clause path it matches by inclusive
     prefix. SBPL resolves per operation, letting the last matching rule win. *)
  Definition op_rule : Type := (bool * path)%type.

  Fixpoint last_match (rules : list op_rule) (p : path) : option bool :=
    match rules with
    | nil => None
    | (b, q) :: rest =>
        match last_match rest p with
        | Some r => Some r
        | None => if withinb q p then Some b else None
        end
    end.

  (* [clause_rules] emits, per clause and in policy order, a [file-read*] allow
     for [Read] and [Write] and a [file-read*] deny for [Deny]. *)
  Definition sb_read_of (a : access) : bool :=
    match a with Read => true | Write => true | Deny => false end.

  (* and a [file-write*] allow only for [Write]; a [Read] clause emits an
     explicit [file-write*] deny (the carveout beneath a shallower write), and
     a [Deny] clause denies writes with reads. *)
  Definition sb_write_of (a : access) : bool :=
    match a with Read => false | Write => true | Deny => false end.

  Fixpoint sb_read_rules (l : list entry) : list op_rule :=
    match l with
    | nil => nil
    | (q, a) :: rest => (sb_read_of a, q) :: sb_read_rules rest
    end.

  Fixpoint sb_write_rules (l : list entry) : list op_rule :=
    match l with
    | nil => nil
    | (q, a) :: rest => (sb_write_of a, q) :: sb_write_rules rest
    end.

  (* The base profile opens with [(deny default)]; [default_read_rule] then
     allows all reads under [All] and nothing under [Denied]. So the read
     default is [All => allow], [Denied => deny]; nothing re-allows writes
     globally, so the write default is always deny. *)
  Definition sb_read_default (d : reads_default) : bool :=
    match d with All => true | Denied => false end.

  Definition sb_write_default (d : reads_default) : bool :=
    match d with All => false | Denied => false end.

  (** Whether seatbelt admits a read of [p] under the rules emitted for [l] and
      default [d]: the last matching read rule, or the read default. *)
  Definition sb_reads (l : list entry) (d : reads_default) (p : path) : bool :=
    match last_match (sb_read_rules l) p with
    | Some b => b
    | None => sb_read_default d
    end.

  (** Whether seatbelt admits a write of [p]: the last matching write rule, or
      the write default. *)
  Definition sb_writes (l : list entry) (d : reads_default) (p : path) : bool :=
    match last_match (sb_write_rules l) p with
    | Some b => b
    | None => sb_write_default d
    end.

  (** {2 Bubblewrap: last-mount binds, resolved by mount order} *)

  (* [filesystem_args] partitions the root clause out of the entries; this is
     its access, [None] when no clause names the root. On a normalized list at
     most one clause names the root, so the first is the only one. *)
  Fixpoint bwrap_root_clause (l : list entry) : option access :=
    match l with
    | nil => None
    | (nil, a) :: _ => Some a
    | (_ :: _, _) :: rest => bwrap_root_clause rest
    end.

  (* The non-root clauses, in order — [filesystem_args]' [rest], each emitted
     by [clause] as [--ro-bind] ([Read]), [--bind] ([Write]) or [--tmpfs]
     ([Deny], sealed read-only by [seal_denials]). Those mount types carry the
     clause's own access unchanged, so a non-root mount is its entry. *)
  Fixpoint bwrap_rest (l : list entry) : list entry :=
    match l with
    | nil => nil
    | (nil, _) :: rest => bwrap_rest rest
    | (s :: q, a) :: rest => (s :: q, a) :: bwrap_rest rest
    end.

  (* [filesystem_args]' root mount follows the root clause's own access: a
     [Write] clause binds the host root read-write ([--bind / /]), a [Read]
     clause binds it read-only ([--ro-bind / /]), a [Deny] clause makes it a
     sealed empty tmpfs ([--tmpfs /], sealed by [seal_root]). Only the root no
     clause names takes the reads default — [Read] under [All], [Deny] under
     [Denied]. This is exactly the resolution law's root fallback, so the root
     mount is faithful for every policy. *)
  Definition bwrap_root (root_acc : option access) (d : reads_default) :
      access :=
    match root_acc with
    | Some a => a
    | None => default_access d
    end.

  (* The hoisted root mount first, then the non-root mounts in policy order.
     Bubblewrap applies mounts in order and the last covering one wins, so the
     root — a prefix of every path — must precede the deeper mounts it would
     otherwise shadow; [filesystem_args] hoists it for exactly this reason. *)
  Definition bwrap_mounts (l : list entry) (d : reads_default) : list entry :=
    (nil, bwrap_root (bwrap_root_clause l) d) :: bwrap_rest l.

  (** The access bubblewrap gives [p]: the last mount covering it wins, which
      is the model's [emitted]. The root mount covers every path, so the
      default arm of [emitted] is never reached. *)
  Definition bwrap_access (l : list entry) (d : reads_default) (p : path) :
      access :=
    emitted (bwrap_mounts l d) d p.

End Confinement.
