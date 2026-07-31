(** The confinement laws, machine-checked against the model.

    Statements quantify over the abstract segment type under one hypothesis:
    segment boolean equality decides propositional equality. The equal-depth
    tie-break [path_leb] appears in statements about [normalize] and [grant]
    but no law constrains it — which is the formal counterpart of the claim
    that emission-order ties are arbitrary.

    Every law is proved; nothing is admitted. *)

From Corelib Require Import Init.Prelude.

Open Scope list_scope.
From MentatSandbox Require Import Confinement.

Section Laws.

  Variable seg : Type.
  Variable seg_eqb : seg -> seg -> bool.
  Variable path_leb : list seg -> list seg -> bool.

  Hypothesis seg_eqb_eq : forall a b, seg_eqb a b = true <-> a = b.

  (* Propositional membership, for statements only; the model stays boolean. *)
  Fixpoint InE (e : entry seg) (l : list (entry seg)) : Prop :=
    match l with
    | nil => False
    | x :: rest => x = e \/ InE e rest
    end.

  Lemma seg_eqb_refl : forall a, seg_eqb a a = true.
  Proof. intro a. apply seg_eqb_eq. reflexivity. Qed.

  Lemma withinb_refl : forall p, withinb seg seg_eqb p p = true.
  Proof.
    induction p as [| s p IH]; simpl.
    - reflexivity.
    - rewrite seg_eqb_refl. simpl. exact IH.
  Qed.

  (** {1 The closed world} *)

  (** A path no clause covers takes the default, and the default is never
      writable: the uncovered world is closed. *)
  Theorem default_never_writable :
    forall d, default_access d <> Write.
  Proof. destruct d; discriminate. Qed.

  Theorem uncovered_takes_default :
    forall l d p,
      deepest seg seg_eqb l p = None ->
      resolve seg seg_eqb l d p = default_access d.
  Proof. intros l d p H. unfold resolve. rewrite H. reflexivity. Qed.

  Theorem uncovered_emitted_default :
    forall l d p,
      last_covering seg seg_eqb l p = None ->
      emitted seg seg_eqb l d p = default_access d.
  Proof. intros l d p H. unfold emitted. rewrite H. reflexivity. Qed.

  Theorem empty_policy_grants_nothing :
    forall d p, resolve seg seg_eqb nil d p = default_access d.
  Proof. reflexivity. Qed.

  (** {1 Widening refusals} *)

  Lemma first_defeat_none_all :
    forall f gs,
      first_defeat seg f gs = None ->
      forall g, InE g gs -> f g = None.
  Proof.
    intros f gs. induction gs as [| g0 rest IH]; simpl.
    - intros _ g [].
    - destruct (f g0) as [d |] eqn:Hf.
      + discriminate.
      + intros Hrest g [Heq | Hin].
        * rewrite <- Heq. exact Hf.
        * apply IH; assumption.
  Qed.

  (* Inverting one passing refusal check yields all three facts a successful
     grant guarantees per granted entry. *)
  Lemma opt_or_none_inv :
    forall a b, opt_or seg a b = None -> a = None /\ b = None.
  Proof.
    intros a b H. destruct a; [discriminate |]. split; [reflexivity | exact H].
  Qed.

  Lemma root_defeat_none : forall g, root_defeat seg g = None -> fst g <> nil.
  Proof.
    intros g H. unfold root_defeat in H.
    destruct (fst g); [discriminate | discriminate].
  Qed.

  Lemma denied_defeat_none :
    forall t g,
      denied_defeat seg seg_eqb t g = None ->
      find_path seg (fun r => withinb seg seg_eqb r (fst g))
        (denied_roots seg t) = None.
  Proof.
    intros t g H. unfold denied_defeat in H.
    destruct (find_path seg (fun r => withinb seg seg_eqb r (fst g))
                (denied_roots seg t)) eqn:E.
    - discriminate.
    - reflexivity.
  Qed.

  Lemma carveout_defeat_none :
    forall t g,
      carveout_defeat seg seg_eqb t g = None ->
      find_entry seg
        (fun c =>
           andb (withinb seg seg_eqb (fst c) (fst g))
             (acc_gtb (snd g) (snd c)))
        (filter_entries seg (is_carveout seg seg_eqb t) t) = None.
  Proof.
    intros t g H. unfold carveout_defeat in H.
    destruct (find_entry seg
                (fun c =>
                   andb (withinb seg seg_eqb (fst c) (fst g))
                     (acc_gtb (snd g) (snd c)))
                (filter_entries seg (is_carveout seg seg_eqb t) t)) eqn:E.
    - discriminate.
    - reflexivity.
  Qed.

  Lemma defeat_check_none_inv :
    forall t g,
      defeat_check seg seg_eqb t g = None ->
      root_defeat seg g = None
      /\ denied_defeat seg seg_eqb t g = None
      /\ carveout_defeat seg seg_eqb t g = None.
  Proof.
    intros t g H. unfold defeat_check in H.
    destruct (opt_or_none_inv _ _ H) as [H1 H2].
    destruct (opt_or_none_inv _ _ H2) as [H3 H4].
    split; [exact H1 |]. split; [exact H3 | exact H4].
  Qed.

  Lemma grant_success_checks :
    forall t gs l',
      grant seg seg_eqb path_leb t gs = Granted seg l' ->
      forall g, InE g gs -> defeat_check seg seg_eqb t g = None.
  Proof.
    intros t gs l' Hg g Hin.
    unfold grant in Hg.
    destruct (first_defeat seg _ gs) as [[p r] |] eqn:Hfd; [discriminate |].
    exact (first_defeat_none_all _ _ Hfd g Hin).
  Qed.

  (** The root spelling is always refused: a successful grant contains no
      entry naming the root. *)
  Theorem grant_success_no_root :
    forall t gs l',
      grant seg seg_eqb path_leb t gs = Granted seg l' ->
      forall g, InE g gs -> fst g <> nil.
  Proof.
    intros t gs l' Hg g Hin. apply root_defeat_none.
    exact (proj1 (defeat_check_none_inv t g
                    (grant_success_checks t gs l' Hg g Hin))).
  Qed.

  (** A grant at or beneath a denied path is refused: under a successful
      grant, no granted path lies within any denied root. *)
  Theorem grant_success_avoids_denied :
    forall t gs l',
      grant seg seg_eqb path_leb t gs = Granted seg l' ->
      forall g, InE g gs ->
        find_path seg (fun r => withinb seg seg_eqb r (fst g))
          (denied_roots seg t) = None.
  Proof.
    intros t gs l' Hg g Hin. apply denied_defeat_none.
    exact (proj1 (proj2 (defeat_check_none_inv t g
                           (grant_success_checks t gs l' Hg g Hin)))).
  Qed.

  (** A grant that out-ranks a carveout is refused: under a successful
      grant, no granted entry defeats a lowered clause. *)
  Theorem grant_success_respects_carveouts :
    forall t gs l',
      grant seg seg_eqb path_leb t gs = Granted seg l' ->
      forall g, InE g gs ->
        find_entry seg
          (fun c =>
             andb (withinb seg seg_eqb (fst c) (fst g))
               (acc_gtb (snd g) (snd c)))
          (filter_entries seg (is_carveout seg seg_eqb t) t) = None.
  Proof.
    intros t gs l' Hg g Hin. apply carveout_defeat_none.
    exact (proj2 (proj2 (defeat_check_none_inv t g
                           (grant_success_checks t gs l' Hg g Hin)))).
  Qed.

  (** {1 Locality helpers} *)

  Lemma deepest_none_of_no_cover :
    forall l p,
      (forall g, InE g l -> withinb seg seg_eqb (fst g) p = false) ->
      deepest seg seg_eqb l p = None.
  Proof.
    induction l as [| [q a] rest IH]; simpl; intros p H.
    - reflexivity.
    - pose proof (H (q, a) (or_introl eq_refl)) as Hq. simpl in Hq.
      rewrite Hq. apply IH. intros g Hin. apply H. right. exact Hin.
  Qed.

  Lemma deepest_app_uncovered :
    forall l1 l2 p,
      deepest seg seg_eqb l2 p = None ->
      deepest seg seg_eqb (app l1 l2) p = deepest seg seg_eqb l1 p.
  Proof.
    induction l1 as [| [q a] rest IH]; simpl; intros l2 p H2.
    - exact H2.
    - rewrite (IH l2 p H2). reflexivity.
  Qed.

  (** {1 Arithmetic of depths} *)

  Lemma negb_true_false : forall b, negb b = true -> b = false.
  Proof. intros b H. destruct b. discriminate. reflexivity. Qed.

  Lemma negb_false_true : forall b, negb b = false -> b = true.
  Proof. intros b H. destruct b. reflexivity. discriminate. Qed.

  Lemma nat_leb_refl : forall n, nat_leb n n = true.
  Proof. intros n. induction n as [| n' IH]. reflexivity. simpl. exact IH. Qed.

  Lemma nat_leb_S_negb : forall n m, nat_leb (S m) n = negb (nat_leb n m).
  Proof.
    intros n. induction n as [| n' IH]; intros m.
    - reflexivity.
    - destruct m as [| m'].
      + reflexivity.
      + simpl. exact (IH m').
  Qed.

  Lemma nat_ltb_negb_leb : forall m n, nat_ltb m n = negb (nat_leb n m).
  Proof. intros m n. unfold nat_ltb. exact (nat_leb_S_negb n m). Qed.

  Lemma nat_leb_total : forall m n, nat_leb m n = true \/ nat_leb n m = true.
  Proof.
    intros m. induction m as [| m' IH]; intros n.
    - left. reflexivity.
    - destruct n as [| n'].
      + right. reflexivity.
      + simpl. exact (IH n').
  Qed.

  Lemma nat_leb_antisym :
    forall m n, nat_leb m n = true -> nat_leb n m = true -> m = n.
  Proof.
    intros m. induction m as [| m' IH]; intros n H1 H2; destruct n as [| n'].
    - reflexivity.
    - simpl in H2. discriminate.
    - simpl in H1. discriminate.
    - simpl in H1. simpl in H2. rewrite (IH n' H1 H2). reflexivity.
  Qed.

  Lemma nat_leb_trans :
    forall a b c,
      nat_leb a b = true -> nat_leb b c = true -> nat_leb a c = true.
  Proof.
    intros a. induction a as [| a' IH]; intros b c H1 H2.
    - reflexivity.
    - destruct b as [| b']; [discriminate H1 |].
      destruct c as [| c']; [discriminate H2 |].
      simpl in H1. simpl in H2. simpl. exact (IH b' c' H1 H2).
  Qed.

  Lemma nat_ltb_leb : forall m n, nat_ltb m n = true -> nat_leb m n = true.
  Proof.
    intros m n H. destruct (nat_leb_total m n) as [E | E]; [exact E |].
    rewrite nat_ltb_negb_leb in H. rewrite E in H. discriminate.
  Qed.

  Lemma nat_ltb_irrefl : forall n, nat_ltb n n = false.
  Proof.
    intros n. rewrite nat_ltb_negb_leb. rewrite (nat_leb_refl n). reflexivity.
  Qed.

  Lemma nat_ltb_asym : forall m n, nat_ltb m n = true -> nat_ltb n m = false.
  Proof.
    intros m n H. rewrite nat_ltb_negb_leb. rewrite (nat_ltb_leb m n H).
    reflexivity.
  Qed.

  Lemma nat_ltb_ff_eq :
    forall m n, nat_ltb m n = false -> nat_ltb n m = false -> m = n.
  Proof.
    intros m n H1 H2.
    rewrite nat_ltb_negb_leb in H1. rewrite nat_ltb_negb_leb in H2.
    exact (nat_leb_antisym m n (negb_false_true _ H2) (negb_false_true _ H1)).
  Qed.

  Lemma nat_ltb_trans :
    forall a b c,
      nat_ltb a b = true -> nat_ltb b c = true -> nat_ltb a c = true.
  Proof.
    intros a b c H1 H2. rewrite nat_ltb_negb_leb.
    destruct (nat_leb c a) eqn:E; [| reflexivity].
    rewrite nat_ltb_negb_leb in H2. apply negb_true_false in H2.
    pose proof (nat_leb_trans c a b E (nat_ltb_leb a b H1)) as Hcb.
    rewrite Hcb in H2. discriminate.
  Qed.

  Lemma nat_ltb_leb_trans :
    forall a b c,
      nat_ltb a b = true -> nat_leb b c = true -> nat_ltb a c = true.
  Proof.
    intros a b c H1 H2. rewrite nat_ltb_negb_leb.
    destruct (nat_leb c a) eqn:E; [| reflexivity].
    rewrite nat_ltb_negb_leb in H1. apply negb_true_false in H1.
    pose proof (nat_leb_trans b c a H2 E) as Hba.
    rewrite Hba in H1. discriminate.
  Qed.

  Lemma nat_leb_neq_ltb :
    forall m n, nat_leb m n = true -> m <> n -> nat_ltb m n = true.
  Proof.
    intros m n H Hne. rewrite nat_ltb_negb_leb.
    destruct (nat_leb n m) eqn:E; [| reflexivity].
    exfalso. apply Hne. exact (nat_leb_antisym m n H E).
  Qed.

  (** {1 The access lattice} *)

  Lemma acc_geb_refl : forall a, acc_geb a a = true.
  Proof. intros a. destruct a; reflexivity. Qed.

  Lemma acc_merge_comm : forall a b, acc_merge a b = acc_merge b a.
  Proof. intros a b. destruct a; destruct b; reflexivity. Qed.

  Lemma acc_merge_assoc :
    forall a b c, acc_merge a (acc_merge b c) = acc_merge (acc_merge a b) c.
  Proof. intros a b c. destruct a; destruct b; destruct c; reflexivity. Qed.

  Lemma acc_merge_geb_l : forall a b, acc_geb (acc_merge a b) a = true.
  Proof. intros a b. destruct a; destruct b; reflexivity. Qed.

  Lemma acc_geb_trans :
    forall a b c,
      acc_geb a b = true -> acc_geb b c = true -> acc_geb a c = true.
  Proof.
    intros a b c H1 H2. destruct a; destruct b; destruct c;
      try reflexivity; try discriminate H1; try discriminate H2.
  Qed.

  Lemma acc_merge_nondeny :
    forall a b, a <> Deny -> b <> Deny -> acc_merge a b <> Deny.
  Proof.
    intros a b Ha Hb. destruct a; destruct b; try discriminate;
      try (exfalso; apply Ha; reflexivity);
      try (exfalso; apply Hb; reflexivity).
  Qed.

  (** {1 Paths as prefixes} *)

  Lemma path_eqb_eq :
    forall a b : path seg, path_eqb seg seg_eqb a b = true <-> a = b.
  Proof.
    intros a. induction a as [| x a' IH]; intros b; destruct b as [| y b'];
      simpl; split; intro H; try reflexivity; try discriminate.
    - destruct (seg_eqb x y) eqn:E1; [| discriminate H].
      simpl in H. apply seg_eqb_eq in E1. apply IH in H.
      rewrite E1. rewrite H. reflexivity.
    - injection H as H1 H2. rewrite H1. rewrite seg_eqb_refl. simpl.
      apply IH. exact H2.
  Qed.

  Lemma withinb_length :
    forall r p2,
      withinb seg seg_eqb r p2 = true ->
      nat_leb (depth seg r) (depth seg p2) = true.
  Proof.
    intros r. induction r as [| s r' IH]; intros p2 H; destruct p2 as [| y p2'].
    - reflexivity.
    - reflexivity.
    - simpl in H. discriminate.
    - simpl in H. destruct (seg_eqb s y); [| discriminate H].
      simpl in H. simpl. exact (IH p2' H).
  Qed.

  Lemma prefixes_nested :
    forall p q1 q2,
      withinb seg seg_eqb q1 p = true ->
      withinb seg seg_eqb q2 p = true ->
      nat_leb (depth seg q1) (depth seg q2) = true ->
      withinb seg seg_eqb q1 q2 = true.
  Proof.
    intros p. induction p as [| s p' IH]; intros q1 q2 H1 H2 Hd.
    - destruct q1 as [| x1 q1']; [reflexivity |]. simpl in H1. discriminate.
    - destruct q1 as [| x1 q1']; [reflexivity |].
      destruct q2 as [| x2 q2'].
      + simpl in Hd. discriminate.
      + simpl in H1. destruct (seg_eqb x1 s) eqn:E1; [| discriminate H1].
        simpl in H1.
        simpl in H2. destruct (seg_eqb x2 s) eqn:E2; [| discriminate H2].
        simpl in H2.
        apply seg_eqb_eq in E1. apply seg_eqb_eq in E2.
        rewrite E1. rewrite E2. simpl. rewrite seg_eqb_refl. simpl.
        simpl in Hd. exact (IH q1' q2' H1 H2 Hd).
  Qed.

  Lemma withinb_same_depth_eq :
    forall q1 q2,
      withinb seg seg_eqb q1 q2 = true ->
      depth seg q1 = depth seg q2 ->
      q1 = q2.
  Proof.
    intros q1. induction q1 as [| x1 q1' IH]; intros q2 H Hd;
      destruct q2 as [| x2 q2'].
    - reflexivity.
    - simpl in Hd. discriminate.
    - simpl in H. discriminate.
    - simpl in H. destruct (seg_eqb x1 x2) eqn:E; [| discriminate H].
      simpl in H. apply seg_eqb_eq in E. simpl in Hd.
      injection Hd as Hd. rewrite E. rewrite (IH q2' H Hd). reflexivity.
  Qed.

  Lemma prefixes_same_depth_eq :
    forall p q1 q2,
      withinb seg seg_eqb q1 p = true ->
      withinb seg seg_eqb q2 p = true ->
      depth seg q1 = depth seg q2 ->
      q1 = q2.
  Proof.
    intros p q1 q2 H1 H2 Hd.
    apply (withinb_same_depth_eq q1 q2); [| exact Hd].
    apply (prefixes_nested p q1 q2 H1 H2). rewrite Hd. exact (nat_leb_refl _).
  Qed.

  (* The development below compares depths abstractly; keep the arithmetic
     opaque so its boolean tests stay syntactically stable under [simpl]. *)
  Arguments nat_leb : simpl never.
  Arguments nat_ltb : simpl never.
  Arguments depth : simpl never.
  Arguments acc_merge : simpl never.
  Arguments acc_geb : simpl never.
  Arguments acc_gtb : simpl never.
  Arguments entry_leb : simpl never.

  (** {1 The deepest-clause semilattice}

      [deepest] folds a commutative, associative merge over the clauses seen
      as single-clause results; that algebra is what makes resolution
      indifferent to duplicate collapse and to emission order. *)

  Definition omerge (x y : option (nat * access)) : option (nat * access) :=
    match x, y with
    | None, _ => y
    | Some _, None => x
    | Some (d1, a1), Some (d2, a2) =>
        if nat_ltb d2 d1 then Some (d1, a1)
        else if nat_ltb d1 d2 then Some (d2, a2)
        else Some (d1, acc_merge a1 a2)
    end.

  Definition single (p : path seg) (e : entry seg) : option (nat * access) :=
    if withinb seg seg_eqb (fst e) p
    then Some (depth seg (fst e), snd e)
    else None.

  Arguments omerge : simpl never.
  Arguments single : simpl never.

  Lemma single_covering :
    forall p q a,
      withinb seg seg_eqb q p = true ->
      single p (q, a) = Some (depth seg q, a).
  Proof. intros p q a H. unfold single. simpl. rewrite H. reflexivity. Qed.

  Lemma single_uncovered :
    forall p q a,
      withinb seg seg_eqb q p = false -> single p (q, a) = None.
  Proof. intros p q a H. unfold single. simpl. rewrite H. reflexivity. Qed.

  Lemma omerge_none_r : forall x, omerge x None = x.
  Proof. intros x. destruct x as [[d a] |]; reflexivity. Qed.

  Lemma omerge_ss_lt :
    forall d1 a1 d2 a2,
      nat_ltb d2 d1 = true ->
      omerge (Some (d1, a1)) (Some (d2, a2)) = Some (d1, a1).
  Proof. intros d1 a1 d2 a2 H. unfold omerge. rewrite H. reflexivity. Qed.

  Lemma omerge_ss_gt :
    forall d1 a1 d2 a2,
      nat_ltb d2 d1 = false ->
      nat_ltb d1 d2 = true ->
      omerge (Some (d1, a1)) (Some (d2, a2)) = Some (d2, a2).
  Proof.
    intros d1 a1 d2 a2 H1 H2. unfold omerge. rewrite H1. rewrite H2.
    reflexivity.
  Qed.

  Lemma omerge_ss_eq :
    forall d1 a1 d2 a2,
      nat_ltb d2 d1 = false ->
      nat_ltb d1 d2 = false ->
      omerge (Some (d1, a1)) (Some (d2, a2)) = Some (d1, acc_merge a1 a2).
  Proof.
    intros d1 a1 d2 a2 H1 H2. unfold omerge. rewrite H1. rewrite H2.
    reflexivity.
  Qed.

  Lemma deepest_cons :
    forall q a rest p,
      deepest seg seg_eqb ((q, a) :: rest) p
      = omerge (single p (q, a)) (deepest seg seg_eqb rest p).
  Proof.
    intros q a rest p. simpl. unfold single. simpl.
    destruct (withinb seg seg_eqb q p).
    - destruct (deepest seg seg_eqb rest p) as [[d b] |].
      + unfold omerge.
        destruct (nat_ltb d (depth seg q)) eqn:E1; [reflexivity |].
        destruct (nat_ltb (depth seg q) d) eqn:E2; [reflexivity |].
        rewrite (nat_ltb_ff_eq d (depth seg q) E1 E2). reflexivity.
      + reflexivity.
    - destruct (deepest seg seg_eqb rest p) as [[d b] |]; reflexivity.
  Qed.

  Lemma omerge_comm : forall x y, omerge x y = omerge y x.
  Proof.
    intros x y. destruct x as [[d1 a1] |]; destruct y as [[d2 a2] |];
      try reflexivity.
    destruct (nat_ltb d2 d1) eqn:E21; destruct (nat_ltb d1 d2) eqn:E12.
    - pose proof (nat_ltb_asym d2 d1 E21) as A. rewrite A in E12. discriminate.
    - rewrite (omerge_ss_lt d1 a1 d2 a2 E21).
      rewrite (omerge_ss_gt d2 a2 d1 a1 E12 E21). reflexivity.
    - rewrite (omerge_ss_gt d1 a1 d2 a2 E21 E12).
      rewrite (omerge_ss_lt d2 a2 d1 a1 E12). reflexivity.
    - rewrite (omerge_ss_eq d1 a1 d2 a2 E21 E12).
      rewrite (omerge_ss_eq d2 a2 d1 a1 E12 E21).
      rewrite (nat_ltb_ff_eq d2 d1 E21 E12). rewrite (acc_merge_comm a1 a2).
      reflexivity.
  Qed.

  Lemma omerge_assoc :
    forall x y z, omerge x (omerge y z) = omerge (omerge x y) z.
  Proof.
    intros x y z.
    destruct x as [[d1 a1] |]; [| reflexivity].
    destruct y as [[d2 a2] |]; [| reflexivity].
    destruct z as [[d3 a3] |];
      [| rewrite (omerge_none_r (Some (d2, a2)));
         rewrite (omerge_none_r
                    (omerge (Some (d1, a1)) (Some (d2, a2))));
         reflexivity].
    destruct (nat_ltb d3 d2) eqn:E32.
    - rewrite (omerge_ss_lt d2 a2 d3 a3 E32).
      destruct (nat_ltb d2 d1) eqn:E21.
      + rewrite (omerge_ss_lt d1 a1 d2 a2 E21).
        rewrite (omerge_ss_lt d1 a1 d3 a3 (nat_ltb_trans d3 d2 d1 E32 E21)).
        reflexivity.
      + destruct (nat_ltb d1 d2) eqn:E12.
        * rewrite (omerge_ss_gt d1 a1 d2 a2 E21 E12).
          rewrite (omerge_ss_lt d2 a2 d3 a3 E32). reflexivity.
        * pose proof (nat_ltb_ff_eq d2 d1 E21 E12) as Heq.
          rewrite (omerge_ss_eq d1 a1 d2 a2 E21 E12).
          rewrite Heq in E32.
          rewrite (omerge_ss_lt d1 (acc_merge a1 a2) d3 a3 E32). reflexivity.
    - destruct (nat_ltb d2 d3) eqn:E23.
      + rewrite (omerge_ss_gt d2 a2 d3 a3 E32 E23).
        destruct (nat_ltb d2 d1) eqn:E21.
        * rewrite (omerge_ss_lt d1 a1 d2 a2 E21). reflexivity.
        * destruct (nat_ltb d1 d2) eqn:E12.
          -- rewrite (omerge_ss_gt d1 a1 d2 a2 E21 E12).
             pose proof (nat_ltb_trans d1 d2 d3 E12 E23) as T13.
             rewrite (omerge_ss_gt d1 a1 d3 a3 (nat_ltb_asym d1 d3 T13) T13).
             rewrite (omerge_ss_gt d2 a2 d3 a3 E32 E23). reflexivity.
          -- pose proof (nat_ltb_ff_eq d2 d1 E21 E12) as Heq.
             rewrite (omerge_ss_eq d1 a1 d2 a2 E21 E12).
             rewrite Heq in E32. rewrite Heq in E23.
             rewrite (omerge_ss_gt d1 a1 d3 a3 E32 E23).
             rewrite (omerge_ss_gt d1 (acc_merge a1 a2) d3 a3 E32 E23).
             reflexivity.
      + pose proof (nat_ltb_ff_eq d3 d2 E32 E23) as Heq32.
        rewrite (omerge_ss_eq d2 a2 d3 a3 E32 E23).
        destruct (nat_ltb d2 d1) eqn:E21.
        * rewrite (omerge_ss_lt d1 a1 d2 (acc_merge a2 a3) E21).
          rewrite (omerge_ss_lt d1 a1 d2 a2 E21).
          assert (T : nat_ltb d3 d1 = true) by (rewrite Heq32; exact E21).
          rewrite (omerge_ss_lt d1 a1 d3 a3 T). reflexivity.
        * destruct (nat_ltb d1 d2) eqn:E12.
          -- rewrite (omerge_ss_gt d1 a1 d2 (acc_merge a2 a3) E21 E12).
             rewrite (omerge_ss_gt d1 a1 d2 a2 E21 E12).
             rewrite (omerge_ss_eq d2 a2 d3 a3 E32 E23). reflexivity.
          -- pose proof (nat_ltb_ff_eq d2 d1 E21 E12) as Heq21.
             rewrite (omerge_ss_eq d1 a1 d2 (acc_merge a2 a3) E21 E12).
             rewrite (omerge_ss_eq d1 a1 d2 a2 E21 E12).
             rewrite Heq21 in E32. rewrite Heq21 in E23.
             rewrite (omerge_ss_eq d1 (acc_merge a1 a2) d3 a3 E32 E23).
             rewrite (acc_merge_assoc a1 a2 a3). reflexivity.
  Qed.

  Lemma deepest_app :
    forall l1 l2 p,
      deepest seg seg_eqb (app l1 l2) p
      = omerge (deepest seg seg_eqb l1 p) (deepest seg seg_eqb l2 p).
  Proof.
    intros l1. induction l1 as [| [q a] rest IH]; intros l2 p.
    - reflexivity.
    - change (app ((q, a) :: rest) l2) with ((q, a) :: app rest l2).
      rewrite deepest_cons. rewrite deepest_cons.
      rewrite (IH l2 p). rewrite omerge_assoc. reflexivity.
  Qed.

  Lemma insert_sorted_le :
    forall e x rest,
      entry_leb seg path_leb e x = true ->
      insert_sorted seg path_leb e (x :: rest) = e :: x :: rest.
  Proof. intros e x rest H. simpl. rewrite H. reflexivity. Qed.

  Lemma insert_sorted_gt :
    forall e x rest,
      entry_leb seg path_leb e x = false ->
      insert_sorted seg path_leb e (x :: rest)
      = x :: insert_sorted seg path_leb e rest.
  Proof. intros e x rest H. simpl. rewrite H. reflexivity. Qed.

  Lemma deepest_insert :
    forall e l p,
      deepest seg seg_eqb (insert_sorted seg path_leb e l) p
      = omerge (single p e) (deepest seg seg_eqb l p).
  Proof.
    intros e l p. induction l as [| x rest IH].
    - destruct e as [q a].
      change (insert_sorted seg path_leb (q, a) nil) with ((q, a) :: nil).
      rewrite deepest_cons. reflexivity.
    - destruct (entry_leb seg path_leb e x) eqn:E.
      + rewrite (insert_sorted_le e x rest E). destruct e as [q a].
        rewrite deepest_cons. reflexivity.
      + rewrite (insert_sorted_gt e x rest E). destruct x as [qx ax].
        rewrite deepest_cons. rewrite IH.
        rewrite deepest_cons.
        rewrite (omerge_assoc (single p (qx, ax)) (single p e)
                   (deepest seg seg_eqb rest p)).
        rewrite (omerge_comm (single p (qx, ax)) (single p e)).
        rewrite <- (omerge_assoc (single p e) (single p (qx, ax))
                      (deepest seg seg_eqb rest p)).
        reflexivity.
  Qed.

  Lemma deepest_sort :
    forall l p,
      deepest seg seg_eqb (sort_entries seg path_leb l) p
      = deepest seg seg_eqb l p.
  Proof.
    intros l p. induction l as [| e rest IH].
    - reflexivity.
    - change (sort_entries seg path_leb (e :: rest))
        with (insert_sorted seg path_leb e (sort_entries seg path_leb rest)).
      rewrite deepest_insert. rewrite IH. destruct e as [q a].
      rewrite deepest_cons. reflexivity.
  Qed.

  Lemma upsert_hit :
    forall e q b rest,
      path_eqb seg seg_eqb q (fst e) = true ->
      upsert seg seg_eqb e ((q, b) :: rest)
      = (q, acc_merge b (snd e)) :: rest.
  Proof. intros e q b rest H. simpl. rewrite H. reflexivity. Qed.

  Lemma upsert_miss :
    forall e q b rest,
      path_eqb seg seg_eqb q (fst e) = false ->
      upsert seg seg_eqb e ((q, b) :: rest)
      = (q, b) :: upsert seg seg_eqb e rest.
  Proof. intros e q b rest H. simpl. rewrite H. reflexivity. Qed.

  Lemma deepest_upsert :
    forall e l p,
      deepest seg seg_eqb (upsert seg seg_eqb e l) p
      = omerge (single p e) (deepest seg seg_eqb l p).
  Proof.
    intros e l p. induction l as [| [q b] rest IH].
    - destruct e as [qe ae].
      change (upsert seg seg_eqb (qe, ae) nil) with ((qe, ae) :: nil).
      rewrite deepest_cons. reflexivity.
    - destruct (path_eqb seg seg_eqb q (fst e)) eqn:E.
      + rewrite upsert_hit; [| exact E].
        apply path_eqb_eq in E. destruct e as [qe ae]. simpl in E. subst q.
        rewrite deepest_cons. rewrite deepest_cons.
        rewrite (omerge_assoc (single p (qe, ae)) (single p (qe, b))
                   (deepest seg seg_eqb rest p)).
        assert (Hh : omerge (single p (qe, ae)) (single p (qe, b))
                     = single p (qe, acc_merge b ae)).
        { unfold single. simpl. destruct (withinb seg seg_eqb qe p).
          - rewrite (omerge_ss_eq (depth seg qe) ae (depth seg qe) b
                       (nat_ltb_irrefl _) (nat_ltb_irrefl _)).
            rewrite (acc_merge_comm ae b). reflexivity.
          - reflexivity. }
        rewrite Hh. simpl. reflexivity.
      + rewrite upsert_miss; [| exact E].
        rewrite deepest_cons. rewrite IH. rewrite deepest_cons.
        rewrite (omerge_assoc (single p (q, b)) (single p e)
                   (deepest seg seg_eqb rest p)).
        rewrite (omerge_comm (single p (q, b)) (single p e)).
        rewrite <- (omerge_assoc (single p e) (single p (q, b))
                      (deepest seg seg_eqb rest p)).
        reflexivity.
  Qed.

  Lemma deepest_dedup_into :
    forall l acc p,
      deepest seg seg_eqb (dedup_into seg seg_eqb acc l) p
      = omerge (deepest seg seg_eqb acc p) (deepest seg seg_eqb l p).
  Proof.
    intros l. induction l as [| e rest IH]; intros acc p.
    - simpl. rewrite omerge_none_r. reflexivity.
    - change (dedup_into seg seg_eqb acc (e :: rest))
        with (dedup_into seg seg_eqb (upsert seg seg_eqb e acc) rest).
      rewrite IH. rewrite deepest_upsert. destruct e as [q a].
      rewrite deepest_cons.
      rewrite (omerge_assoc (deepest seg seg_eqb acc p) (single p (q, a))
                 (deepest seg seg_eqb rest p)).
      rewrite (omerge_comm (deepest seg seg_eqb acc p) (single p (q, a))).
      rewrite <- (omerge_assoc (single p (q, a)) (deepest seg seg_eqb acc p)
                    (deepest seg seg_eqb rest p)).
      reflexivity.
  Qed.

  Lemma deepest_normalize :
    forall l p,
      deepest seg seg_eqb (normalize seg seg_eqb path_leb l) p
      = deepest seg seg_eqb l p.
  Proof.
    intros l p. unfold normalize. rewrite deepest_sort. unfold dedup.
    rewrite deepest_dedup_into. reflexivity.
  Qed.

  (** Resolution is invariant under normalization: collapsing duplicates to
      their strongest access and reordering shallowest-first changes no
      path's access. *)
  Theorem resolve_normalize :
    forall l d p,
      resolve seg seg_eqb (normalize seg seg_eqb path_leb l) d p
      = resolve seg seg_eqb l d p.
  Proof.
    intros l d p. unfold resolve. rewrite deepest_normalize. reflexivity.
  Qed.

  (** {1 Emission order}

      Correspondence needs two facts the algebra alone cannot see: the
      normalized list is depth-sorted, and it names each path once. On such
      a list the last covering clause is the deepest one. *)

  Lemma andb_true_split : forall a b, andb a b = true -> a = true /\ b = true.
  Proof.
    intros a b H. destruct a; destruct b; try discriminate H.
    split; reflexivity.
  Qed.

  Lemma path_eqb_refl : forall q, path_eqb seg seg_eqb q q = true.
  Proof. intros q. apply path_eqb_eq. reflexivity. Qed.

  Lemma path_eqb_sym :
    forall a b, path_eqb seg seg_eqb a b = path_eqb seg seg_eqb b a.
  Proof.
    intros a b.
    destruct (path_eqb seg seg_eqb a b) eqn:E1;
      destruct (path_eqb seg seg_eqb b a) eqn:E2; try reflexivity.
    - apply path_eqb_eq in E1. rewrite E1 in E2.
      rewrite (path_eqb_refl b) in E2. discriminate.
    - apply path_eqb_eq in E2. rewrite E2 in E1.
      rewrite (path_eqb_refl a) in E1. discriminate.
  Qed.

  Fixpoint dsorted (l : list (entry seg)) : bool :=
    match l with
    | nil => true
    | a :: rest =>
        andb
          (match rest with
           | nil => true
           | b :: _ => nat_leb (depth seg (fst a)) (depth seg (fst b))
           end)
          (dsorted rest)
    end.

  Lemma dsorted_tail :
    forall a l, dsorted (a :: l) = true -> dsorted l = true.
  Proof.
    intros a l H. destruct l as [| b rest]; [reflexivity |].
    change (dsorted (a :: b :: rest))
      with (andb (nat_leb (depth seg (fst a)) (depth seg (fst b)))
              (dsorted (b :: rest))) in H.
    exact (proj2 (andb_true_split _ _ H)).
  Qed.

  Lemma dsorted_head_leb_all :
    forall x l,
      dsorted (x :: l) = true ->
      forall e, InE e l ->
        nat_leb (depth seg (fst x)) (depth seg (fst e)) = true.
  Proof.
    intros x l. revert x. induction l as [| y rest IH]; intros x H e Hin.
    - destruct Hin.
    - change (dsorted (x :: y :: rest))
        with (andb (nat_leb (depth seg (fst x)) (depth seg (fst y)))
                (dsorted (y :: rest))) in H.
      destruct (andb_true_split _ _ H) as [Hxy Htail].
      destruct Hin as [Heq | Hin].
      + rewrite Heq in Hxy. exact Hxy.
      + exact (nat_leb_trans _ _ _ Hxy (IH y Htail e Hin)).
  Qed.

  Lemma entry_leb_true_leb :
    forall a b,
      entry_leb seg path_leb a b = true ->
      nat_leb (depth seg (fst a)) (depth seg (fst b)) = true.
  Proof.
    intros a b H. unfold entry_leb in H.
    destruct (nat_ltb (depth seg (fst a)) (depth seg (fst b))) eqn:E1.
    - exact (nat_ltb_leb _ _ E1).
    - destruct (nat_ltb (depth seg (fst b)) (depth seg (fst a))) eqn:E2.
      + discriminate H.
      + rewrite (nat_ltb_ff_eq _ _ E1 E2). exact (nat_leb_refl _).
  Qed.

  Lemma entry_leb_false_leb :
    forall a b,
      entry_leb seg path_leb a b = false ->
      nat_leb (depth seg (fst b)) (depth seg (fst a)) = true.
  Proof.
    intros a b H. unfold entry_leb in H.
    destruct (nat_ltb (depth seg (fst a)) (depth seg (fst b))) eqn:E1;
      [discriminate H |].
    destruct (nat_ltb (depth seg (fst b)) (depth seg (fst a))) eqn:E2.
    - exact (nat_ltb_leb _ _ E2).
    - rewrite (nat_ltb_ff_eq _ _ E1 E2). exact (nat_leb_refl _).
  Qed.

  Lemma insert_dsorted :
    forall e l,
      dsorted l = true ->
      dsorted (insert_sorted seg path_leb e l) = true.
  Proof.
    intros e l. induction l as [| x rest IH]; intros H.
    - reflexivity.
    - destruct (entry_leb seg path_leb e x) eqn:E.
      + rewrite insert_sorted_le; [| exact E].
        change (dsorted (e :: x :: rest))
          with (andb (nat_leb (depth seg (fst e)) (depth seg (fst x)))
                  (dsorted (x :: rest))).
        rewrite (entry_leb_true_leb e x E). rewrite H. reflexivity.
      + rewrite insert_sorted_gt; [| exact E].
        destruct rest as [| y rest'].
        * change (insert_sorted seg path_leb e nil) with (e :: nil).
          change (dsorted (x :: e :: nil))
            with (andb (nat_leb (depth seg (fst x)) (depth seg (fst e)))
                    (dsorted (e :: nil))).
          rewrite (entry_leb_false_leb e x E). reflexivity.
        * destruct (entry_leb seg path_leb e y) eqn:E2.
          -- rewrite insert_sorted_le; [| exact E2].
             change (dsorted (x :: e :: y :: rest'))
               with (andb (nat_leb (depth seg (fst x)) (depth seg (fst e)))
                       (andb (nat_leb (depth seg (fst e))
                                (depth seg (fst y)))
                          (dsorted (y :: rest')))).
             rewrite (entry_leb_false_leb e x E).
             rewrite (entry_leb_true_leb e y E2).
             change (dsorted (x :: y :: rest'))
               with (andb (nat_leb (depth seg (fst x)) (depth seg (fst y)))
                       (dsorted (y :: rest'))) in H.
             destruct (andb_true_split _ _ H) as [Hxy Hyr].
             rewrite Hyr. reflexivity.
          -- rewrite insert_sorted_gt; [| exact E2].
             change (dsorted (x :: y :: insert_sorted seg path_leb e rest'))
               with (andb (nat_leb (depth seg (fst x)) (depth seg (fst y)))
                       (dsorted (y :: insert_sorted seg path_leb e rest'))).
             change (dsorted (x :: y :: rest'))
               with (andb (nat_leb (depth seg (fst x)) (depth seg (fst y)))
                       (dsorted (y :: rest'))) in H.
             destruct (andb_true_split _ _ H) as [Hxy Hyr].
             rewrite Hxy.
             pose proof (IH Hyr) as HI.
             rewrite insert_sorted_gt in HI; [| exact E2].
             rewrite HI. reflexivity.
  Qed.

  Lemma sort_dsorted :
    forall l, dsorted (sort_entries seg path_leb l) = true.
  Proof.
    intros l. induction l as [| e rest IH].
    - reflexivity.
    - change (sort_entries seg path_leb (e :: rest))
        with (insert_sorted seg path_leb e (sort_entries seg path_leb rest)).
      exact (insert_dsorted e (sort_entries seg path_leb rest) IH).
  Qed.

  Fixpoint mem_path (q : path seg) (l : list (entry seg)) : bool :=
    match l with
    | nil => false
    | (r, _) :: rest => orb (path_eqb seg seg_eqb r q) (mem_path q rest)
    end.

  Fixpoint no_dup (l : list (entry seg)) : bool :=
    match l with
    | nil => true
    | (q, _) :: rest => andb (negb (mem_path q rest)) (no_dup rest)
    end.

  Lemma InE_mem_path :
    forall q a l, InE (q, a) l -> mem_path q l = true.
  Proof.
    intros q a l. induction l as [| [r c] rest IH]; intros Hin.
    - destruct Hin.
    - destruct Hin as [Heq | Hin].
      + injection Heq as Hr Hc. subst r. simpl. rewrite path_eqb_refl.
        reflexivity.
      + simpl. rewrite (IH Hin).
        destruct (path_eqb seg seg_eqb r q); reflexivity.
  Qed.

  Lemma upsert_mem :
    forall e l r,
      mem_path r (upsert seg seg_eqb e l)
      = orb (path_eqb seg seg_eqb (fst e) r) (mem_path r l).
  Proof.
    intros e l r. induction l as [| [q b] rest IH].
    - destruct e as [qe ae].
      change (upsert seg seg_eqb (qe, ae) nil) with ((qe, ae) :: nil).
      simpl. destruct (path_eqb seg seg_eqb qe r); reflexivity.
    - destruct (path_eqb seg seg_eqb q (fst e)) eqn:E.
      + rewrite upsert_hit; [| exact E].
        apply path_eqb_eq in E. subst q. simpl.
        destruct (path_eqb seg seg_eqb (fst e) r); reflexivity.
      + rewrite upsert_miss; [| exact E].
        simpl. rewrite IH.
        destruct (path_eqb seg seg_eqb q r);
          destruct (path_eqb seg seg_eqb (fst e) r);
          destruct (mem_path r rest); reflexivity.
  Qed.

  Lemma upsert_nodup :
    forall e l, no_dup l = true -> no_dup (upsert seg seg_eqb e l) = true.
  Proof.
    intros e l. induction l as [| [q b] rest IH]; intros H.
    - destruct e as [qe ae].
      change (upsert seg seg_eqb (qe, ae) nil) with ((qe, ae) :: nil).
      reflexivity.
    - change (no_dup ((q, b) :: rest))
        with (andb (negb (mem_path q rest)) (no_dup rest)) in H.
      destruct (andb_true_split _ _ H) as [Hm Hnd].
      destruct (path_eqb seg seg_eqb q (fst e)) eqn:E.
      + rewrite upsert_hit; [| exact E].
        change (no_dup ((q, acc_merge b (snd e)) :: rest))
          with (andb (negb (mem_path q rest)) (no_dup rest)).
        rewrite Hnd.
        destruct (mem_path q rest); [discriminate Hm | reflexivity].
      + rewrite upsert_miss; [| exact E].
        change (no_dup ((q, b) :: upsert seg seg_eqb e rest))
          with (andb (negb (mem_path q (upsert seg seg_eqb e rest)))
                  (no_dup (upsert seg seg_eqb e rest))).
        rewrite (upsert_mem e rest q).
        rewrite (IH Hnd).
        rewrite path_eqb_sym in E. rewrite E.
        destruct (mem_path q rest); [discriminate Hm | reflexivity].
  Qed.

  Lemma dedup_into_nodup :
    forall l acc,
      no_dup acc = true ->
      no_dup (dedup_into seg seg_eqb acc l) = true.
  Proof.
    intros l. induction l as [| e rest IH]; intros acc H.
    - exact H.
    - change (dedup_into seg seg_eqb acc (e :: rest))
        with (dedup_into seg seg_eqb (upsert seg seg_eqb e acc) rest).
      exact (IH (upsert seg seg_eqb e acc) (upsert_nodup e acc H)).
  Qed.

  Lemma insert_mem :
    forall e l r,
      mem_path r (insert_sorted seg path_leb e l)
      = orb (path_eqb seg seg_eqb (fst e) r) (mem_path r l).
  Proof.
    intros e l r. induction l as [| x rest IH].
    - destruct e as [qe ae].
      change (insert_sorted seg path_leb (qe, ae) nil) with ((qe, ae) :: nil).
      simpl. destruct (path_eqb seg seg_eqb qe r); reflexivity.
    - destruct (entry_leb seg path_leb e x) eqn:E.
      + rewrite insert_sorted_le; [| exact E].
        destruct e as [qe ae]. destruct x as [qx ax]. simpl.
        destruct (path_eqb seg seg_eqb qe r);
          destruct (path_eqb seg seg_eqb qx r); reflexivity.
      + rewrite insert_sorted_gt; [| exact E].
        destruct x as [qx ax]. simpl. rewrite IH.
        destruct (path_eqb seg seg_eqb qx r);
          destruct (path_eqb seg seg_eqb (fst e) r);
          destruct (mem_path r rest); reflexivity.
  Qed.

  Lemma insert_nodup :
    forall e l,
      no_dup l = true ->
      mem_path (fst e) l = false ->
      no_dup (insert_sorted seg path_leb e l) = true.
  Proof.
    intros e l. induction l as [| x rest IH]; intros H Hm.
    - destruct e as [qe ae].
      change (insert_sorted seg path_leb (qe, ae) nil) with ((qe, ae) :: nil).
      reflexivity.
    - destruct x as [qx ax].
      change (no_dup ((qx, ax) :: rest))
        with (andb (negb (mem_path qx rest)) (no_dup rest)) in H.
      destruct (andb_true_split _ _ H) as [Hmx Hnd].
      simpl in Hm.
      destruct (path_eqb seg seg_eqb qx (fst e)) eqn:Ex;
        [discriminate Hm |].
      simpl in Hm.
      destruct (entry_leb seg path_leb e (qx, ax)) eqn:E.
      + rewrite insert_sorted_le; [| exact E].
        destruct e as [qe ae].
        change (no_dup ((qe, ae) :: (qx, ax) :: rest))
          with (andb (negb (mem_path qe ((qx, ax) :: rest)))
                  (no_dup ((qx, ax) :: rest))).
        change (no_dup ((qx, ax) :: rest))
          with (andb (negb (mem_path qx rest)) (no_dup rest)).
        rewrite Hnd.
        simpl in Ex |- *.
        rewrite Ex. simpl in Hm. rewrite Hm.
        destruct (mem_path qx rest); [discriminate Hmx | reflexivity].
      + rewrite insert_sorted_gt; [| exact E].
        change (no_dup ((qx, ax) :: insert_sorted seg path_leb e rest))
          with (andb (negb (mem_path qx (insert_sorted seg path_leb e rest)))
                  (no_dup (insert_sorted seg path_leb e rest))).
        rewrite (insert_mem e rest qx).
        rewrite (IH Hnd Hm).
        rewrite path_eqb_sym in Ex. rewrite Ex.
        destruct (mem_path qx rest); [discriminate Hmx | reflexivity].
  Qed.

  Lemma sort_mem :
    forall l r,
      mem_path r (sort_entries seg path_leb l) = mem_path r l.
  Proof.
    intros l r. induction l as [| e rest IH].
    - reflexivity.
    - change (sort_entries seg path_leb (e :: rest))
        with (insert_sorted seg path_leb e (sort_entries seg path_leb rest)).
      rewrite (insert_mem e (sort_entries seg path_leb rest) r).
      rewrite IH. destruct e as [qe ae]. simpl. reflexivity.
  Qed.

  Lemma sort_nodup :
    forall l, no_dup l = true -> no_dup (sort_entries seg path_leb l) = true.
  Proof.
    intros l. induction l as [| [qe ae] rest IH]; intros H.
    - reflexivity.
    - change (no_dup ((qe, ae) :: rest))
        with (andb (negb (mem_path qe rest)) (no_dup rest)) in H.
      destruct (andb_true_split _ _ H) as [Hm Hnd].
      change (sort_entries seg path_leb ((qe, ae) :: rest))
        with (insert_sorted seg path_leb (qe, ae)
                (sort_entries seg path_leb rest)).
      apply (insert_nodup (qe, ae) (sort_entries seg path_leb rest)
               (IH Hnd)).
      simpl. rewrite (sort_mem rest qe).
      destruct (mem_path qe rest); [discriminate Hm | reflexivity].
  Qed.

  Lemma normalize_dsorted :
    forall l, dsorted (normalize seg seg_eqb path_leb l) = true.
  Proof. intros l. unfold normalize. exact (sort_dsorted _). Qed.

  Lemma normalize_nodup :
    forall l, no_dup (normalize seg seg_eqb path_leb l) = true.
  Proof.
    intros l. unfold normalize. apply sort_nodup. unfold dedup.
    exact (dedup_into_nodup l nil eq_refl).
  Qed.

  Lemma deepest_nodup_char :
    forall l p,
      no_dup l = true ->
      forall d b,
        deepest seg seg_eqb l p = Some (d, b) ->
        exists q,
          InE (q, b) l /\ withinb seg seg_eqb q p = true /\ depth seg q = d.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p Hnd d b H.
    - discriminate H.
    - change (no_dup ((q0, a0) :: rest))
        with (andb (negb (mem_path q0 rest)) (no_dup rest)) in Hnd.
      destruct (andb_true_split _ _ Hnd) as [Hm Hnd'].
      rewrite deepest_cons in H.
      destruct (withinb seg seg_eqb q0 p) eqn:W.
      + rewrite (single_covering p q0 a0 W) in H.
        destruct (deepest seg seg_eqb rest p) as [[dr br] |] eqn:Hr.
        * destruct (nat_ltb dr (depth seg q0)) eqn:E1.
          -- rewrite (omerge_ss_lt (depth seg q0) a0 dr br E1) in H.
             injection H as Hd Hb. subst b.
             exists q0. split. left. reflexivity. split. exact W. exact Hd.
          -- destruct (nat_ltb (depth seg q0) dr) eqn:E2.
             ++ rewrite (omerge_ss_gt (depth seg q0) a0 dr br E1 E2) in H.
                injection H as Hd Hb. subst b.
                destruct (IH p Hnd' dr br Hr) as [q' [Hin' [Hw' Hd']]].
                exists q'. split. right. exact Hin'. split. exact Hw'.
                rewrite Hd'. exact Hd.
             ++ pose proof (nat_ltb_ff_eq dr (depth seg q0) E1 E2) as Heq.
                destruct (IH p Hnd' dr br Hr) as [q' [Hin' [Hw' Hd']]].
                assert (Hqq : q' = q0).
                { apply (prefixes_same_depth_eq p q' q0 Hw' W).
                  rewrite Hd'. exact Heq. }
                subst q'.
                pose proof (InE_mem_path q0 br rest Hin') as M.
                rewrite M in Hm. discriminate.
        * rewrite (omerge_none_r (Some (depth seg q0, a0))) in H.
          injection H as Hd Hb. subst b.
          exists q0. split. left. reflexivity. split. exact W. exact Hd.
      + rewrite (single_uncovered p q0 a0 W) in H. simpl in H.
        destruct (IH p Hnd' d b H) as [q' [Hin' [Hw' Hd']]].
        exists q'. split. right. exact Hin'. split. exact Hw'. exact Hd'.
  Qed.

  Lemma last_covering_deepest :
    forall l p,
      dsorted l = true ->
      no_dup l = true ->
      last_covering seg seg_eqb l p
      = match deepest seg seg_eqb l p with
        | None => None
        | Some (_, b) => Some b
        end.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p Hs Hnd.
    - reflexivity.
    - pose proof (dsorted_tail (q0, a0) rest Hs) as Hs'.
      change (no_dup ((q0, a0) :: rest))
        with (andb (negb (mem_path q0 rest)) (no_dup rest)) in Hnd.
      destruct (andb_true_split _ _ Hnd) as [Hm Hnd'].
      change (last_covering seg seg_eqb ((q0, a0) :: rest) p)
        with (match last_covering seg seg_eqb rest p with
              | Some b => Some b
              | None =>
                  if withinb seg seg_eqb q0 p then Some a0 else None
              end).
      rewrite (IH p Hs' Hnd').
      rewrite deepest_cons.
      destruct (deepest seg seg_eqb rest p) as [[dr br] |] eqn:Hr.
      + destruct (withinb seg seg_eqb q0 p) eqn:W.
        * rewrite (single_covering p q0 a0 W).
          destruct (deepest_nodup_char rest p Hnd' dr br Hr)
            as [q' [Hin' [Hw' Hd']]].
          assert (Hle : nat_leb (depth seg q0) (depth seg q') = true).
          { exact (dsorted_head_leb_all (q0, a0) rest Hs (q', br) Hin'). }
          assert (Hne : depth seg q0 <> depth seg q').
          { intro Habs.
            pose proof (prefixes_same_depth_eq p q0 q' W Hw' Habs) as Hqq.
            rewrite <- Hqq in Hin'.
            pose proof (InE_mem_path q0 br rest Hin') as M.
            rewrite M in Hm. discriminate. }
          pose proof (nat_leb_neq_ltb _ _ Hle Hne) as Hlt.
          rewrite Hd' in Hlt.
          rewrite (omerge_ss_gt (depth seg q0) a0 dr br
                     (nat_ltb_asym _ _ Hlt) Hlt).
          reflexivity.
        * rewrite (single_uncovered p q0 a0 W). reflexivity.
      + destruct (withinb seg seg_eqb q0 p) eqn:W.
        * rewrite (single_covering p q0 a0 W).
          rewrite (omerge_none_r (Some (depth seg q0, a0))). reflexivity.
        * rewrite (single_uncovered p q0 a0 W). reflexivity.
  Qed.

  (** The correspondence law — the claim both backends rely on: last clause
      wins over the normalized emission order computes the resolution law. *)
  Theorem emitted_normalize_is_resolve :
    forall l d p,
      emitted seg seg_eqb (normalize seg seg_eqb path_leb l) d p
      = resolve seg seg_eqb l d p.
  Proof.
    intros l d p. unfold emitted.
    rewrite (last_covering_deepest (normalize seg seg_eqb path_leb l) p
               (normalize_dsorted l) (normalize_nodup l)).
    rewrite deepest_normalize. unfold resolve.
    destruct (deepest seg seg_eqb l p) as [[dd bb] |]; reflexivity.
  Qed.

  (** Locality: a successful grant changes nothing outside the granted
      subtrees. *)
  Theorem grant_local :
    forall t gs l' d p,
      grant seg seg_eqb path_leb t gs = Granted seg l' ->
      (forall g, InE g gs -> withinb seg seg_eqb (fst g) p = false) ->
      resolve seg seg_eqb l' d p = resolve seg seg_eqb t d p.
  Proof.
    intros t gs l' d p Hg Hout.
    unfold grant in Hg.
    destruct (first_defeat seg _ gs) as [[q r] |] eqn:Hfd; [discriminate |].
    injection Hg as <-.
    rewrite resolve_normalize.
    unfold resolve.
    rewrite (deepest_app_uncovered t gs p
               (deepest_none_of_no_cover gs p Hout)).
    reflexivity.
  Qed.

  (** {1 Bounds on the winning clause} *)

  Lemma omerge_some_l :
    forall d1 a1 y,
      exists d b,
        omerge (Some (d1, a1)) y = Some (d, b) /\ nat_leb d1 d = true.
  Proof.
    intros d1 a1 y. destruct y as [[d2 a2] |].
    - destruct (nat_ltb d2 d1) eqn:E21.
      + exists d1, a1. split. exact (omerge_ss_lt d1 a1 d2 a2 E21).
        exact (nat_leb_refl d1).
      + destruct (nat_ltb d1 d2) eqn:E12.
        * exists d2, a2. split. exact (omerge_ss_gt d1 a1 d2 a2 E21 E12).
          exact (nat_ltb_leb d1 d2 E12).
        * exists d1, (acc_merge a1 a2). split.
          exact (omerge_ss_eq d1 a1 d2 a2 E21 E12). exact (nat_leb_refl d1).
    - exists d1, a1. split. reflexivity. exact (nat_leb_refl d1).
  Qed.

  Lemma omerge_some_r :
    forall x d2 a2,
      exists d b,
        omerge x (Some (d2, a2)) = Some (d, b) /\ nat_leb d2 d = true.
  Proof.
    intros x d2 a2. rewrite (omerge_comm x (Some (d2, a2))).
    exact (omerge_some_l d2 a2 x).
  Qed.

  Lemma deepest_covering_ge :
    forall l p q a,
      InE (q, a) l ->
      withinb seg seg_eqb q p = true ->
      exists d b,
        deepest seg seg_eqb l p = Some (d, b)
        /\ nat_leb (depth seg q) d = true.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p q a Hin Hw.
    - destruct Hin.
    - rewrite deepest_cons. destruct Hin as [Heq | Hin].
      + injection Heq as Hq Ha. subst q0. subst a0.
        rewrite (single_covering p q a Hw).
        exact (omerge_some_l (depth seg q) a (deepest seg seg_eqb rest p)).
      + destruct (IH p q a Hin Hw) as [dd [bb [Hd Hle]]].
        rewrite Hd.
        destruct (omerge_some_r (single p (q0, a0)) dd bb)
          as [d' [b' [Hm Hle']]].
        exists d', b'. split. exact Hm.
        exact (nat_leb_trans (depth seg q) dd d' Hle Hle').
  Qed.

  Lemma deepest_covering_lt :
    forall l p D,
      (forall q a,
        InE (q, a) l -> withinb seg seg_eqb q p = true ->
        nat_ltb (depth seg q) D = true) ->
      deepest seg seg_eqb l p = None
      \/ exists d b,
            deepest seg seg_eqb l p = Some (d, b) /\ nat_ltb d D = true.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p D H.
    - left. reflexivity.
    - rewrite deepest_cons.
      assert (Hrest : forall q a,
                 InE (q, a) rest -> withinb seg seg_eqb q p = true ->
                 nat_ltb (depth seg q) D = true).
      { intros q a Hin Hw. exact (H q a (or_intror Hin) Hw). }
      destruct (withinb seg seg_eqb q0 p) eqn:W.
      + rewrite (single_covering p q0 a0 W).
        pose proof (H q0 a0 (or_introl eq_refl) W) as Hq0.
        destruct (IH p D Hrest) as [Hn | [dd [bb [Hd Hlt]]]].
        * rewrite Hn. rewrite (omerge_none_r (Some (depth seg q0, a0))).
          right. exists (depth seg q0), a0. split. reflexivity. exact Hq0.
        * rewrite Hd.
          destruct (nat_ltb dd (depth seg q0)) eqn:E1.
          -- rewrite (omerge_ss_lt (depth seg q0) a0 dd bb E1). right.
             exists (depth seg q0), a0. split. reflexivity. exact Hq0.
          -- destruct (nat_ltb (depth seg q0) dd) eqn:E2.
             ++ rewrite (omerge_ss_gt (depth seg q0) a0 dd bb E1 E2). right.
                exists dd, bb. split. reflexivity. exact Hlt.
             ++ rewrite (omerge_ss_eq (depth seg q0) a0 dd bb E1 E2). right.
                exists (depth seg q0), (acc_merge a0 bb). split. reflexivity.
                exact Hq0.
      + rewrite (single_uncovered p q0 a0 W).
        destruct (IH p D Hrest) as [Hn | [dd [bb [Hd Hlt]]]].
        * rewrite Hn. left. reflexivity.
        * rewrite Hd. right. exists dd, bb. split. reflexivity. exact Hlt.
  Qed.

  Lemma deepest_depth_le_path :
    forall l p d b,
      deepest seg seg_eqb l p = Some (d, b) ->
      nat_leb d (depth seg p) = true.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p d b H.
    - discriminate H.
    - rewrite deepest_cons in H.
      destruct (withinb seg seg_eqb q0 p) eqn:W.
      + rewrite (single_covering p q0 a0 W) in H.
        destruct (deepest seg seg_eqb rest p) as [[dr br] |] eqn:Hr.
        * destruct (nat_ltb dr (depth seg q0)) eqn:E1.
          -- rewrite (omerge_ss_lt (depth seg q0) a0 dr br E1) in H.
             injection H as Hd Hb. rewrite <- Hd.
             exact (withinb_length q0 p W).
          -- destruct (nat_ltb (depth seg q0) dr) eqn:E2.
             ++ rewrite (omerge_ss_gt (depth seg q0) a0 dr br E1 E2) in H.
                injection H as Hd Hb. rewrite <- Hd.
                exact (IH p dr br Hr).
             ++ rewrite (omerge_ss_eq (depth seg q0) a0 dr br E1 E2) in H.
                injection H as Hd Hb. rewrite <- Hd.
                exact (withinb_length q0 p W).
        * rewrite (omerge_none_r (Some (depth seg q0, a0))) in H.
          injection H as Hd Hb. rewrite <- Hd.
          exact (withinb_length q0 p W).
      + rewrite (single_uncovered p q0 a0 W) in H. simpl in H.
        exact (IH p d b H).
  Qed.

  Lemma deepest_nondeny :
    forall l p,
      (forall q c,
        InE (q, c) l -> withinb seg seg_eqb q p = true -> c <> Deny) ->
      forall d b, deepest seg seg_eqb l p = Some (d, b) -> b <> Deny.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p H d b Hd.
    - discriminate Hd.
    - rewrite deepest_cons in Hd.
      assert (Hrest : forall q c,
                 InE (q, c) rest -> withinb seg seg_eqb q p = true ->
                 c <> Deny).
      { intros q c Hin Hw. exact (H q c (or_intror Hin) Hw). }
      destruct (withinb seg seg_eqb q0 p) eqn:W.
      + pose proof (H q0 a0 (or_introl eq_refl) W) as H0.
        rewrite (single_covering p q0 a0 W) in Hd.
        destruct (deepest seg seg_eqb rest p) as [[dr br] |] eqn:Hr.
        * pose proof (IH p Hrest dr br Hr) as Hbr.
          destruct (nat_ltb dr (depth seg q0)) eqn:E1.
          -- rewrite (omerge_ss_lt (depth seg q0) a0 dr br E1) in Hd.
             injection Hd as Hd1 Hb. rewrite <- Hb. exact H0.
          -- destruct (nat_ltb (depth seg q0) dr) eqn:E2.
             ++ rewrite (omerge_ss_gt (depth seg q0) a0 dr br E1 E2) in Hd.
                injection Hd as Hd1 Hb. rewrite <- Hb. exact Hbr.
             ++ rewrite (omerge_ss_eq (depth seg q0) a0 dr br E1 E2) in Hd.
                injection Hd as Hd1 Hb. rewrite <- Hb.
                exact (acc_merge_nondeny a0 br H0 Hbr).
        * rewrite (omerge_none_r (Some (depth seg q0, a0))) in Hd.
          injection Hd as Hd1 Hb. rewrite <- Hb. exact H0.
      + rewrite (single_uncovered p q0 a0 W) in Hd. simpl in Hd.
        exact (IH p Hrest d b Hd).
  Qed.

  Lemma deepest_at_self :
    forall l p0 a,
      InE (p0, a) l ->
      exists b,
        deepest seg seg_eqb l p0 = Some (depth seg p0, b)
        /\ acc_geb b a = true.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p0 a Hin.
    - destruct Hin.
    - rewrite deepest_cons. destruct Hin as [Heq | Hin].
      + injection Heq as Hq Ha. subst q0. subst a0.
        rewrite (single_covering p0 p0 a (withinb_refl p0)).
        destruct (deepest seg seg_eqb rest p0) as [[dr br] |] eqn:Hr.
        * pose proof (deepest_depth_le_path rest p0 dr br Hr) as Hle.
          destruct (nat_ltb dr (depth seg p0)) eqn:E1.
          -- exists a. split.
             exact (omerge_ss_lt (depth seg p0) a dr br E1).
             exact (acc_geb_refl a).
          -- assert (E2 : nat_ltb (depth seg p0) dr = false).
             { rewrite nat_ltb_negb_leb. rewrite Hle. reflexivity. }
             exists (acc_merge a br). split.
             exact (omerge_ss_eq (depth seg p0) a dr br E1 E2).
             exact (acc_merge_geb_l a br).
        * exists a. split.
          exact (omerge_none_r (Some (depth seg p0, a))).
          exact (acc_geb_refl a).
      + destruct (IH p0 a Hin) as [b [Hd Hgb]].
        rewrite Hd.
        destruct (withinb seg seg_eqb q0 p0) eqn:W.
        * pose proof (withinb_length q0 p0 W) as Hlen.
          rewrite (single_covering p0 q0 a0 W).
          destruct (nat_ltb (depth seg q0) (depth seg p0)) eqn:E1.
          -- exists b. split; [| exact Hgb].
             exact (omerge_ss_gt (depth seg q0) a0 (depth seg p0) b
                      (nat_ltb_asym (depth seg q0) (depth seg p0) E1) E1).
          -- assert (Hgeq : nat_leb (depth seg p0) (depth seg q0) = true).
             { rewrite nat_ltb_negb_leb in E1.
               exact (negb_false_true _ E1). }
             assert (Heqd : depth seg q0 = depth seg p0).
             { exact (nat_leb_antisym (depth seg q0) (depth seg p0)
                        Hlen Hgeq). }
             assert (F1 : nat_ltb (depth seg p0) (depth seg q0) = false).
             { rewrite Heqd. exact (nat_ltb_irrefl (depth seg p0)). }
             exists (acc_merge a0 b). split.
             ++ rewrite (omerge_ss_eq (depth seg q0) a0 (depth seg p0) b
                           F1 E1).
                rewrite Heqd. reflexivity.
             ++ assert (G : acc_geb (acc_merge a0 b) b = true).
                { rewrite (acc_merge_comm a0 b).
                  exact (acc_merge_geb_l b a0). }
                exact (acc_geb_trans (acc_merge a0 b) b a G Hgb).
        * rewrite (single_uncovered p0 q0 a0 W). simpl.
          exists b. split. reflexivity. exact Hgb.
  Qed.

  (** {1 Membership plumbing} *)

  Lemma InE_app_r :
    forall l1 l2 (e : entry seg), InE e l2 -> InE e (app l1 l2).
  Proof.
    intros l1. induction l1 as [| x rest IH]; intros l2 e H.
    - exact H.
    - simpl. right. exact (IH l2 e H).
  Qed.

  Lemma InE_app_or :
    forall l1 l2 (e : entry seg),
      InE e (app l1 l2) -> InE e l1 \/ InE e l2.
  Proof.
    intros l1. induction l1 as [| x rest IH]; intros l2 e H.
    - right. exact H.
    - simpl in H. destruct H as [Heq | H].
      + left. left. exact Heq.
      + destruct (IH l2 e H) as [Hl | Hr].
        * left. right. exact Hl.
        * right. exact Hr.
  Qed.

  Fixpoint InP (q : path seg) (l : list (path seg)) : Prop :=
    match l with
    | nil => False
    | x :: rest => x = q \/ InP q rest
    end.

  Lemma find_path_none_all :
    forall f l,
      find_path seg f l = None -> forall q, InP q l -> f q = false.
  Proof.
    intros f l. induction l as [| x rest IH]; intros H q Hin.
    - destruct Hin.
    - simpl in H. destruct (f x) eqn:E; [discriminate H |].
      destruct Hin as [Heq | Hin].
      + rewrite <- Heq. exact E.
      + exact (IH H q Hin).
  Qed.

  Lemma denied_roots_in :
    forall t q, InE (q, Deny) t -> InP q (denied_roots seg t).
  Proof.
    intros t. induction t as [| [r c] rest IH]; intros q Hin.
    - destruct Hin.
    - destruct Hin as [Heq | Hin].
      + injection Heq as Hr Hc. subst r. subst c. simpl. left. reflexivity.
      + destruct c; simpl.
        * exact (IH q Hin).
        * exact (IH q Hin).
        * right. exact (IH q Hin).
  Qed.

  (** The deny set is inescapable: at and beneath a denied path, a
      successful grant changes no path's access. *)
  Theorem deny_survives_grant :
    forall t gs l' d q p,
      grant seg seg_eqb path_leb t gs = Granted seg l' ->
      InE (q, Deny) t ->
      withinb seg seg_eqb q p = true ->
      resolve seg seg_eqb l' d p = resolve seg seg_eqb t d p.
  Proof.
    intros t gs l' d q p Hg Hq Hw.
    pose proof (grant_success_avoids_denied t gs l' Hg) as Havoid.
    unfold grant in Hg.
    destruct (first_defeat seg _ gs) as [[pd rd] |] eqn:Hfd; [discriminate |].
    injection Hg as <-.
    unfold resolve. rewrite deepest_normalize. rewrite deepest_app.
    destruct (deepest_covering_ge t p q Deny Hq Hw) as [dt [bt [Hdt Hge]]].
    rewrite Hdt.
    assert (Hgs : forall qg ag,
               InE (qg, ag) gs -> withinb seg seg_eqb qg p = true ->
               nat_ltb (depth seg qg) dt = true).
    { intros qg ag Hin Hwg.
      pose proof (Havoid (qg, ag) Hin) as Hnone.
      pose proof (find_path_none_all _ _ Hnone q (denied_roots_in t q Hq))
        as Hnw.
      simpl in Hnw.
      assert (Hlt : nat_ltb (depth seg qg) (depth seg q) = true).
      { destruct (nat_leb (depth seg q) (depth seg qg)) eqn:El.
        - rewrite (prefixes_nested p q qg Hw Hwg El) in Hnw. discriminate.
        - rewrite nat_ltb_negb_leb. rewrite El. reflexivity. }
      exact (nat_ltb_leb_trans (depth seg qg) (depth seg q) dt Hlt Hge). }
    destruct (deepest_covering_lt gs p dt Hgs)
      as [Hn | [dg [bg [Hdg Hlt]]]].
    - rewrite Hn. rewrite (omerge_none_r (Some (dt, bt))). reflexivity.
    - rewrite Hdg. rewrite (omerge_ss_lt dt bt dg bg Hlt). reflexivity.
  Qed.

  (** No silent loss: a successful grant that asks only for reads and
      writes actually resolves at each granted path — never to a denial,
      never below what was asked. The batch-wide hypothesis is necessary,
      and proving the naive per-entry statement false is what surfaced it: a
      batch may spell a denial of its own, and granting one at or above [p]
      denies [p] by the ordinary resolution law. *)
  Theorem grant_resolves :
    forall t gs l' d p a,
      grant seg seg_eqb path_leb t gs = Granted seg l' ->
      (forall g, InE g gs -> snd g <> Deny) ->
      InE (p, a) gs ->
      resolve seg seg_eqb l' d p <> Deny
      /\ acc_geb (resolve seg seg_eqb l' d p) a = true.
  Proof.
    intros t gs l' d p a Hg Hnd Hin.
    pose proof (grant_success_avoids_denied t gs l' Hg) as Havoid.
    unfold grant in Hg.
    destruct (first_defeat seg _ gs) as [[pd rd] |] eqn:Hfd; [discriminate |].
    injection Hg as <-.
    unfold resolve. rewrite deepest_normalize.
    pose proof (InE_app_r t gs (p, a) Hin) as Hin'.
    destruct (deepest_at_self (app t gs) p a Hin') as [b [Hd Hgb]].
    rewrite Hd.
    assert (Hnodeny : forall qc c,
               InE (qc, c) (app t gs) ->
               withinb seg seg_eqb qc p = true -> c <> Deny).
    { intros qc c Hinc Hwc.
      destruct (InE_app_or t gs (qc, c) Hinc) as [Hint | Hing].
      - intro Hc. subst c.
        pose proof (Havoid (p, a) Hin) as Hnone.
        pose proof (find_path_none_all _ _ Hnone qc
                      (denied_roots_in t qc Hint)) as Hnw.
        simpl in Hnw. rewrite Hwc in Hnw. discriminate.
      - exact (Hnd (qc, c) Hing). }
    split.
    - exact (deepest_nondeny (app t gs) p Hnodeny (depth seg p) b Hd).
    - exact Hgb.
  Qed.

  (** {1 The escalation floor} *)

  Lemma withinb_nil : forall p, withinb seg seg_eqb nil p = true.
  Proof. intros p. reflexivity. Qed.

  Lemma depth_nil : depth seg nil = O.
  Proof. reflexivity. Qed.

  Lemma nat_ltb_zero_r : forall n, nat_ltb n O = false.
  Proof. intros n. reflexivity. Qed.

  Lemma InE_deny_paths_inv :
    forall qs q a,
      InE (q, a) (deny_paths seg qs) -> a = Deny /\ InP q qs.
  Proof.
    intros qs. induction qs as [| q0 rest IH]; intros q a Hin.
    - destruct Hin.
    - simpl in Hin. destruct Hin as [Heq | Hin].
      + injection Heq as Hq Ha. subst q0. subst a.
        split. reflexivity. left. reflexivity.
      + destruct (IH q a Hin) as [Ha Hp]. split. exact Ha. right. exact Hp.
  Qed.

  Lemma InP_deny_paths :
    forall qs q, InP q qs -> InE (q, Deny) (deny_paths seg qs).
  Proof.
    intros qs. induction qs as [| q0 rest IH]; intros q Hin.
    - destruct Hin.
    - simpl. destruct Hin as [Heq | Hin].
      + left. rewrite Heq. reflexivity.
      + right. exact (IH q Hin).
  Qed.

  (* Every covering clause of an all-denied list is a denial, and merges of
     denials are denials, so the winner is a denial. *)
  Lemma deepest_alldeny :
    forall l p,
      (forall q a, InE (q, a) l -> a = Deny) ->
      forall d b, deepest seg seg_eqb l p = Some (d, b) -> b = Deny.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p H d b Hd.
    - discriminate Hd.
    - assert (Ha0 : a0 = Deny) by exact (H q0 a0 (or_introl eq_refl)).
      assert (Hrest : forall q a, InE (q, a) rest -> a = Deny)
        by (intros q a Hin; exact (H q a (or_intror Hin))).
      rewrite deepest_cons in Hd.
      destruct (withinb seg seg_eqb q0 p) eqn:W.
      + rewrite (single_covering p q0 a0 W) in Hd.
        destruct (deepest seg seg_eqb rest p) as [[dr br] |] eqn:Hr.
        * assert (Hbr : br = Deny) by exact (IH p Hrest dr br Hr).
          destruct (nat_ltb dr (depth seg q0)) eqn:E1.
          -- rewrite (omerge_ss_lt (depth seg q0) a0 dr br E1) in Hd.
             injection Hd as _ Hb. rewrite <- Hb. exact Ha0.
          -- destruct (nat_ltb (depth seg q0) dr) eqn:E2.
             ++ rewrite (omerge_ss_gt (depth seg q0) a0 dr br E1 E2) in Hd.
                injection Hd as _ Hb. rewrite <- Hb. exact Hbr.
             ++ rewrite (omerge_ss_eq (depth seg q0) a0 dr br E1 E2) in Hd.
                injection Hd as _ Hb. rewrite <- Hb.
                rewrite Ha0. rewrite Hbr. reflexivity.
        * rewrite (omerge_none_r (Some (depth seg q0, a0))) in Hd.
          injection Hd as _ Hb. rewrite <- Hb. exact Ha0.
      + rewrite (single_uncovered p q0 a0 W) in Hd. simpl in Hd.
        exact (IH p Hrest d b Hd).
  Qed.

  (** A denial survives the escalation floor: at and beneath any path [l]
      denies, the floor still resolves to [Deny] — though its root is
      writable and its default open, the deeper denial outranks the root. *)
  Theorem floor_preserves_denials :
    forall l d q p,
      InE (q, Deny) l ->
      withinb seg seg_eqb q p = true ->
      resolve seg seg_eqb (floor seg l) d p = Deny.
  Proof.
    intros l d q p Hq Hw. unfold resolve, floor.
    rewrite deepest_cons.
    rewrite (single_covering p nil Write (withinb_nil p)).
    assert (Hin : InE (q, Deny) (deny_paths seg (denied_roots seg l)))
      by exact (InP_deny_paths (denied_roots seg l) q (denied_roots_in l q Hq)).
    destruct (deepest_covering_ge (deny_paths seg (denied_roots seg l)) p q
                Deny Hin Hw) as [dd [bb [Hd _]]].
    assert (Hall : forall q' a',
               InE (q', a') (deny_paths seg (denied_roots seg l)) ->
               a' = Deny).
    { intros q' a' Hin'.
      exact (proj1 (InE_deny_paths_inv (denied_roots seg l) q' a' Hin')). }
    assert (Hbb : bb = Deny)
      by exact (deepest_alldeny (deny_paths seg (denied_roots seg l)) p Hall
                  dd bb Hd).
    subst bb. rewrite Hd. rewrite depth_nil.
    destruct (nat_ltb O dd) eqn:E1.
    - rewrite (omerge_ss_gt O Write dd Deny (nat_ltb_zero_r dd) E1).
      reflexivity.
    - rewrite (omerge_ss_eq O Write dd Deny (nat_ltb_zero_r dd) E1).
      reflexivity.
  Qed.

  (** The floor grants the write everywhere the deny set does not reach:
      outside every denied path, the escalated root resolves to [Write]. *)
  Theorem floor_grants_outside :
    forall l d p,
      (forall q, InP q (denied_roots seg l) ->
                 withinb seg seg_eqb q p = false) ->
      resolve seg seg_eqb (floor seg l) d p = Write.
  Proof.
    intros l d p H. unfold resolve, floor.
    rewrite deepest_cons.
    rewrite (single_covering p nil Write (withinb_nil p)).
    assert (Hn : deepest seg seg_eqb (deny_paths seg (denied_roots seg l)) p
                 = None).
    { apply deepest_none_of_no_cover. intros g Hin. destruct g as [gq ga].
      destruct (InE_deny_paths_inv (denied_roots seg l) gq ga Hin)
        as [_ Hgp].
      simpl. exact (H gq Hgp). }
    rewrite Hn. rewrite (omerge_none_r (Some (depth seg nil, Write))).
    reflexivity.
  Qed.

  (** {1 Seatbelt correspondence}

      The premise [emitted_normalize_is_resolve] assumes for seatbelt, now
      proved: the read and write decisions of the SBPL rules emitted for
      [normalize l] agree with the resolution law — a read is admitted exactly
      where the policy does not deny, a write exactly where it resolves to
      [Write]. SBPL resolves per operation, so the two decisions are stated
      separately, matching [clause_rules], which threads no [require-not] and
      lets a later rule win per operation. *)

  Lemma sb_read_of_eq : forall a, sb_read_of a = negb (acc_eqb a Deny).
  Proof. intros a. destruct a; reflexivity. Qed.

  Lemma sb_write_of_eq : forall a, sb_write_of a = acc_eqb a Write.
  Proof. intros a. destruct a; reflexivity. Qed.

  Lemma sb_read_default_eq :
    forall d, sb_read_default d = negb (acc_eqb (default_access d) Deny).
  Proof. intros d. destruct d; reflexivity. Qed.

  Lemma sb_write_default_eq :
    forall d, sb_write_default d = acc_eqb (default_access d) Write.
  Proof. intros d. destruct d; reflexivity. Qed.

  (* The read rule list resolves, per path, to the read projection of the last
     covering clause — the emission is one rule per clause, in clause order. *)
  Lemma sb_read_rules_last :
    forall l p,
      last_match seg seg_eqb (sb_read_rules seg l) p
      = match last_covering seg seg_eqb l p with
        | None => None
        | Some a => Some (sb_read_of a)
        end.
  Proof.
    intros l p. induction l as [| [q a] rest IH].
    - reflexivity.
    - cbn [sb_read_rules last_match last_covering]. rewrite IH.
      destruct (last_covering seg seg_eqb rest p) as [b |].
      + reflexivity.
      + destruct (withinb seg seg_eqb q p); reflexivity.
  Qed.

  Lemma sb_write_rules_last :
    forall l p,
      last_match seg seg_eqb (sb_write_rules seg l) p
      = match last_covering seg seg_eqb l p with
        | None => None
        | Some a => Some (sb_write_of a)
        end.
  Proof.
    intros l p. induction l as [| [q a] rest IH].
    - reflexivity.
    - cbn [sb_write_rules last_match last_covering]. rewrite IH.
      destruct (last_covering seg seg_eqb rest p) as [b |].
      + reflexivity.
      + destruct (withinb seg seg_eqb q p); reflexivity.
  Qed.

  Lemma last_covering_normalize :
    forall l p,
      last_covering seg seg_eqb (normalize seg seg_eqb path_leb l) p
      = match deepest seg seg_eqb l p with
        | None => None
        | Some (_, b) => Some b
        end.
  Proof.
    intros l p.
    rewrite (last_covering_deepest (normalize seg seg_eqb path_leb l) p
               (normalize_dsorted l) (normalize_nodup l)).
    rewrite deepest_normalize. reflexivity.
  Qed.

  (** Seatbelt admits a read of [p] exactly where the policy does not resolve
      to a denial. *)
  Theorem seatbelt_reads_resolve :
    forall l d p,
      sb_reads seg seg_eqb (normalize seg seg_eqb path_leb l) d p
      = negb (acc_eqb (resolve seg seg_eqb l d p) Deny).
  Proof.
    intros l d p. unfold sb_reads, resolve.
    rewrite sb_read_rules_last. rewrite last_covering_normalize.
    destruct (deepest seg seg_eqb l p) as [[dd b] |].
    - exact (sb_read_of_eq b).
    - exact (sb_read_default_eq d).
  Qed.

  (** Seatbelt admits a write of [p] exactly where the policy resolves to
      [Write]. *)
  Theorem seatbelt_writes_resolve :
    forall l d p,
      sb_writes seg seg_eqb (normalize seg seg_eqb path_leb l) d p
      = acc_eqb (resolve seg seg_eqb l d p) Write.
  Proof.
    intros l d p. unfold sb_writes, resolve.
    rewrite sb_write_rules_last. rewrite last_covering_normalize.
    destruct (deepest seg seg_eqb l p) as [[dd b] |].
    - exact (sb_write_of_eq b).
    - exact (sb_write_default_eq d).
  Qed.

  (** {1 Bubblewrap correspondence}

      Bubblewrap's access for a path is the model's [emitted] over its mount
      list: the last covering mount wins. The mount list is the hoisted root
      mount, then the non-root clauses in policy order — [bwrap_mounts]. The
      proof relates that list back to [resolve], using the same [deepest]/
      [omerge] algebra: the root is at depth zero, so any covering deep clause
      wins over it, exactly as in resolution. The root mount is the one place
      bubblewrap can diverge, characterized at the end. *)

  Lemma omerge_none_left : forall y, omerge None y = y.
  Proof. intros y. reflexivity. Qed.

  Lemma omerge_swap12 :
    forall x y z, omerge x (omerge y z) = omerge y (omerge x z).
  Proof.
    intros x y z. rewrite (omerge_assoc x y z). rewrite (omerge_comm x y).
    rewrite <- (omerge_assoc y x z). reflexivity.
  Qed.

  Lemma depth_pos_of_nonnil :
    forall q : path seg, q <> nil -> nat_ltb O (depth seg q) = true.
  Proof.
    intros q H. destruct q as [| s q'].
    - exfalso. apply H. reflexivity.
    - rewrite nat_ltb_negb_leb. reflexivity.
  Qed.

  (* The winning depth is the depth of a covering clause, so a list of clauses
     all deeper than the root has a winner deeper than the root. *)
  Lemma deepest_all_pos :
    forall l p,
      (forall q a, InE (q, a) l -> nat_ltb O (depth seg q) = true) ->
      forall d b, deepest seg seg_eqb l p = Some (d, b) -> nat_ltb O d = true.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p H d b Hd.
    - discriminate Hd.
    - rewrite deepest_cons in Hd.
      assert (Hrest : forall q a,
                 InE (q, a) rest -> nat_ltb O (depth seg q) = true).
      { intros q a Hin. exact (H q a (or_intror Hin)). }
      destruct (withinb seg seg_eqb q0 p) eqn:W.
      + pose proof (H q0 a0 (or_introl eq_refl)) as Hq0.
        rewrite (single_covering p q0 a0 W) in Hd.
        destruct (deepest seg seg_eqb rest p) as [[dr br] |] eqn:Hr.
        * pose proof (IH p Hrest dr br Hr) as Hbr.
          destruct (nat_ltb dr (depth seg q0)) eqn:E1.
          -- rewrite (omerge_ss_lt (depth seg q0) a0 dr br E1) in Hd.
             injection Hd as Hd1 _. rewrite <- Hd1. exact Hq0.
          -- destruct (nat_ltb (depth seg q0) dr) eqn:E2.
             ++ rewrite (omerge_ss_gt (depth seg q0) a0 dr br E1 E2) in Hd.
                injection Hd as Hd1 _. rewrite <- Hd1. exact Hbr.
             ++ rewrite (omerge_ss_eq (depth seg q0) a0 dr br E1 E2) in Hd.
                injection Hd as Hd1 _. rewrite <- Hd1. exact Hq0.
        * rewrite (omerge_none_r (Some (depth seg q0, a0))) in Hd.
          injection Hd as Hd1 _. rewrite <- Hd1. exact Hq0.
      + rewrite (single_uncovered p q0 a0 W) in Hd. simpl in Hd.
        exact (IH p Hrest d b Hd).
  Qed.

  (** {2 The non-root mounts} *)

  Lemma bwrap_rest_root :
    forall a0 rest, bwrap_rest seg ((nil, a0) :: rest) = bwrap_rest seg rest.
  Proof. reflexivity. Qed.

  Lemma bwrap_rest_nonroot_cons :
    forall (s : seg) q0' a0 rest,
      bwrap_rest seg ((s :: q0', a0) :: rest)
      = (s :: q0', a0) :: bwrap_rest seg rest.
  Proof. reflexivity. Qed.

  Lemma bwrap_rest_incl :
    forall l e, InE e (bwrap_rest seg l) -> InE e l.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros e H.
    - destruct H.
    - destruct q0 as [| s q0'].
      + rewrite bwrap_rest_root in H. right. exact (IH e H).
      + rewrite bwrap_rest_nonroot_cons in H. destruct H as [Heq | H].
        * left. exact Heq.
        * right. exact (IH e H).
  Qed.

  Lemma bwrap_rest_nonroot :
    forall l q a, InE (q, a) (bwrap_rest seg l) -> q <> nil.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros q a H.
    - destruct H.
    - destruct q0 as [| s q0'].
      + rewrite bwrap_rest_root in H. exact (IH q a H).
      + rewrite bwrap_rest_nonroot_cons in H. destruct H as [Heq | H].
        * injection Heq as Hq _. rewrite <- Hq. intro C. discriminate C.
        * exact (IH q a H).
  Qed.

  Lemma bwrap_rest_mem :
    forall l r, mem_path r (bwrap_rest seg l) = true -> mem_path r l = true.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros r H.
    - simpl in H. discriminate H.
    - destruct q0 as [| s q0'].
      + rewrite bwrap_rest_root in H.
        change (mem_path r ((nil, a0) :: rest))
          with (orb (path_eqb seg seg_eqb nil r) (mem_path r rest)).
        rewrite (IH r H).
        destruct (path_eqb seg seg_eqb nil r); reflexivity.
      + rewrite bwrap_rest_nonroot_cons in H.
        change (mem_path r ((s :: q0', a0) :: bwrap_rest seg rest))
          with (orb (path_eqb seg seg_eqb (s :: q0') r)
                  (mem_path r (bwrap_rest seg rest))) in H.
        change (mem_path r ((s :: q0', a0) :: rest))
          with (orb (path_eqb seg seg_eqb (s :: q0') r) (mem_path r rest)).
        destruct (path_eqb seg seg_eqb (s :: q0') r).
        * reflexivity.
        * simpl in H. rewrite (IH r H). reflexivity.
  Qed.

  Lemma bwrap_rest_nodup :
    forall l, no_dup l = true -> no_dup (bwrap_rest seg l) = true.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros H.
    - reflexivity.
    - change (no_dup ((q0, a0) :: rest))
        with (andb (negb (mem_path q0 rest)) (no_dup rest)) in H.
      destruct (andb_true_split _ _ H) as [Hm Hnd].
      destruct q0 as [| s q0'].
      + rewrite bwrap_rest_root. exact (IH Hnd).
      + rewrite bwrap_rest_nonroot_cons.
        change (no_dup ((s :: q0', a0) :: bwrap_rest seg rest))
          with (andb (negb (mem_path (s :: q0') (bwrap_rest seg rest)))
                  (no_dup (bwrap_rest seg rest))).
        rewrite (IH Hnd).
        destruct (mem_path (s :: q0') (bwrap_rest seg rest)) eqn:Em.
        * pose proof (bwrap_rest_mem rest (s :: q0') Em) as Mr.
          rewrite Mr in Hm. discriminate Hm.
        * reflexivity.
  Qed.

  Lemma dsorted_cons_head :
    forall a K,
      (forall e,
         InE e K -> nat_leb (depth seg (fst a)) (depth seg (fst e)) = true) ->
      dsorted K = true ->
      dsorted (a :: K) = true.
  Proof.
    intros a K Hle Hd. destruct K as [| b restb].
    - reflexivity.
    - change (dsorted (a :: b :: restb))
        with (andb (nat_leb (depth seg (fst a)) (depth seg (fst b)))
                (dsorted (b :: restb))).
      rewrite (Hle b (or_introl eq_refl)). rewrite Hd. reflexivity.
  Qed.

  Lemma bwrap_rest_dsorted :
    forall l, dsorted l = true -> dsorted (bwrap_rest seg l) = true.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros H.
    - reflexivity.
    - pose proof (dsorted_tail (q0, a0) rest H) as Htail.
      destruct q0 as [| s q0'].
      + rewrite bwrap_rest_root. exact (IH Htail).
      + rewrite bwrap_rest_nonroot_cons.
        apply dsorted_cons_head.
        * intros e Hin.
          exact (dsorted_head_leb_all (s :: q0', a0) rest H e
                   (bwrap_rest_incl rest e Hin)).
        * exact (IH Htail).
  Qed.

  (** {2 The root clause} *)

  Lemma bwrap_root_clause_none :
    forall l, mem_path nil l = false -> bwrap_root_clause seg l = None.
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros H.
    - reflexivity.
    - destruct q0 as [| s q0'].
      + simpl in H. discriminate H.
      + cbn [bwrap_root_clause]. apply IH. simpl in H. exact H.
  Qed.

  Definition root_single (l : list (entry seg)) : option (nat * access) :=
    match bwrap_root_clause seg l with
    | Some a => Some (depth seg nil, a)
    | None => None
    end.

  Lemma root_single_root :
    forall a0 rest, root_single ((nil, a0) :: rest) = Some (depth seg nil, a0).
  Proof. reflexivity. Qed.

  Lemma root_single_nonroot :
    forall (s : seg) q0' a0 rest,
      root_single ((s :: q0', a0) :: rest) = root_single rest.
  Proof. reflexivity. Qed.

  Lemma root_single_none :
    forall l, bwrap_root_clause seg l = None -> root_single l = None.
  Proof. intros l H. unfold root_single. rewrite H. reflexivity. Qed.

  (* [deepest] over the whole list is the root clause's contribution merged
     with [deepest] over the non-root mounts — the merge is order- and
     duplicate-insensitive, so splitting the root out changes nothing. *)
  Lemma deepest_split_root :
    forall l p,
      no_dup l = true ->
      deepest seg seg_eqb l p
      = omerge (root_single l) (deepest seg seg_eqb (bwrap_rest seg l) p).
  Proof.
    intros l. induction l as [| [q0 a0] rest IH]; intros p Hnd.
    - reflexivity.
    - change (no_dup ((q0, a0) :: rest))
        with (andb (negb (mem_path q0 rest)) (no_dup rest)) in Hnd.
      destruct (andb_true_split _ _ Hnd) as [Hm Hnd'].
      destruct q0 as [| s q0'].
      + rewrite bwrap_rest_root. rewrite root_single_root.
        rewrite deepest_cons.
        rewrite (single_covering p nil a0 (withinb_nil p)).
        assert (Hrc : bwrap_root_clause seg rest = None).
        { apply bwrap_root_clause_none. exact (negb_true_false _ Hm). }
        pose proof (IH p Hnd') as IHrest.
        rewrite (root_single_none rest Hrc) in IHrest.
        rewrite (omerge_none_left (deepest seg seg_eqb (bwrap_rest seg rest) p))
          in IHrest.
        rewrite IHrest. reflexivity.
      + rewrite root_single_nonroot. rewrite bwrap_rest_nonroot_cons.
        rewrite deepest_cons. rewrite deepest_cons. rewrite (IH p Hnd').
        exact (omerge_swap12 (single p (s :: q0', a0)) (root_single rest)
                 (deepest seg seg_eqb (bwrap_rest seg rest) p)).
  Qed.

  (** {2 The characterizations} *)

  (* Bubblewrap's access: the deepest non-root mount covering [p], or the root
     mount when none does. *)
  Lemma bwrap_access_char :
    forall l d p,
      bwrap_access seg seg_eqb (normalize seg seg_eqb path_leb l) d p
      = match
          deepest seg seg_eqb
            (bwrap_rest seg (normalize seg seg_eqb path_leb l)) p
        with
        | Some (_, br) => br
        | None =>
            bwrap_root
              (bwrap_root_clause seg (normalize seg seg_eqb path_leb l)) d
        end.
  Proof.
    intros l d p. unfold bwrap_access, emitted, bwrap_mounts.
    cbn [last_covering].
    rewrite (last_covering_deepest
               (bwrap_rest seg (normalize seg seg_eqb path_leb l)) p
               (bwrap_rest_dsorted (normalize seg seg_eqb path_leb l)
                  (normalize_dsorted l))
               (bwrap_rest_nodup (normalize seg seg_eqb path_leb l)
                  (normalize_nodup l))).
    destruct (deepest seg seg_eqb
                (bwrap_rest seg (normalize seg seg_eqb path_leb l)) p)
      as [[dr br] |].
    - reflexivity.
    - rewrite (withinb_nil p). reflexivity.
  Qed.

  (* Resolution, split the same way: the deepest non-root clause, or the root
     clause's own access, or the default. *)
  Lemma resolve_char :
    forall l d p,
      resolve seg seg_eqb l d p
      = match
          deepest seg seg_eqb
            (bwrap_rest seg (normalize seg seg_eqb path_leb l)) p
        with
        | Some (_, br) => br
        | None =>
            match bwrap_root_clause seg (normalize seg seg_eqb path_leb l) with
            | Some a => a
            | None => default_access d
            end
        end.
  Proof.
    intros l d p.
    rewrite <- (resolve_normalize l d p).
    unfold resolve.
    rewrite (deepest_split_root (normalize seg seg_eqb path_leb l) p
               (normalize_nodup l)).
    unfold root_single. rewrite depth_nil.
    destruct (deepest seg seg_eqb
                (bwrap_rest seg (normalize seg seg_eqb path_leb l)) p)
      as [[dr br] |] eqn:HDR.
    - assert (Hhyp : forall q a,
                 InE (q, a) (bwrap_rest seg (normalize seg seg_eqb path_leb l)) ->
                 nat_ltb O (depth seg q) = true).
      { intros q a Hin. apply depth_pos_of_nonnil.
        exact (bwrap_rest_nonroot _ q a Hin). }
      pose proof (deepest_all_pos _ p Hhyp dr br HDR) as Hpos.
      destruct (bwrap_root_clause seg (normalize seg seg_eqb path_leb l))
        as [a |].
      + rewrite (omerge_ss_gt O a dr br (nat_ltb_zero_r dr) Hpos).
        reflexivity.
      + rewrite (omerge_none_left (Some (dr, br))). reflexivity.
    - destruct (bwrap_root_clause seg (normalize seg seg_eqb path_leb l))
        as [a |].
      + rewrite (omerge_none_r (Some (O, a))). reflexivity.
      + rewrite (omerge_none_left (@None (nat * access)%type)). reflexivity.
  Qed.

  (** {2 The correspondence and its one gap} *)

  (** Bubblewrap resolves every path exactly as the policy does, for every
      policy and path. The root mount follows the root clause's own access
      ([bwrap_root]), which is the resolution law's root fallback, so the two
      characterizations differ nowhere: a covering non-root mount wins where one
      exists, the root mount otherwise. *)
  Theorem bubblewrap_resolves :
    forall l d p,
      bwrap_access seg seg_eqb (normalize seg seg_eqb path_leb l) d p
      = resolve seg seg_eqb l d p.
  Proof.
    intros l d p.
    rewrite bwrap_access_char. rewrite resolve_char.
    destruct (deepest seg seg_eqb
                (bwrap_rest seg (normalize seg seg_eqb path_leb l)) p)
      as [[dr br] |].
    - reflexivity.
    - destruct (bwrap_root_clause seg (normalize seg seg_eqb path_leb l));
        reflexivity.
  Qed.

  (** Its write decision therefore matches the resolution law too. *)
  Theorem bubblewrap_writes_resolve :
    forall l d p,
      acc_eqb
        (bwrap_access seg seg_eqb (normalize seg seg_eqb path_leb l) d p) Write
      = acc_eqb (resolve seg seg_eqb l d p) Write.
  Proof. intros l d p. rewrite (bubblewrap_resolves l d p). reflexivity. Qed.

End Laws.
