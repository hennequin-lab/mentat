(** The revert-safety classification laws, machine-checked against the model.

    Statements quantify over the abstract reason type and the [is_superseded]
    predicate; no law constrains a reason beyond that predicate, which is the
    formal counterpart of the claim that the verdict reads only whether a reason
    is a supersession.

    Every law is proved; nothing is admitted. *)

From Corelib Require Import Init.Prelude.

Open Scope list_scope.
From MentatMutation Require Import Revertability.

Section Laws.

  Variable reason : Type.
  Variable is_superseded : reason -> bool.

  (** {1 Boolean scaffolding} *)

  Lemma orb_assoc : forall a b c, orb a (orb b c) = orb (orb a b) c.
  Proof. intros a b c. destruct a; reflexivity. Qed.

  Lemma orb_comm : forall a b, orb a b = orb b a.
  Proof. intros a b. destruct a; destruct b; reflexivity. Qed.

  Lemma orb_true_r : forall b, orb b true = true.
  Proof. intros b. destruct b; reflexivity. Qed.

  (** {1 Reduction of [classify] on a non-empty set}

      On any [cons] the classification is the supersession test; the whole file
      leans on this one definitional fact. *)
  Lemma classify_cons :
    forall r rest,
      classify reason is_superseded (r :: rest)
      = (if any_superseded reason is_superseded (r :: rest)
         then Unavailable reason (r :: rest)
         else Incomplete reason (r :: rest)).
  Proof. reflexivity. Qed.

  (** {1 The three arms} *)

  (** An empty reason set is [Available]. *)
  Theorem classify_nil :
    classify reason is_superseded nil = Available reason.
  Proof. reflexivity. Qed.

  (** A reason set holding a supersession is [Unavailable], carrying the set
      unchanged: supersession is sufficient for unavailability. *)
  Theorem classify_superseded :
    forall reasons,
      any_superseded reason is_superseded reasons = true ->
      classify reason is_superseded reasons = Unavailable reason reasons.
  Proof.
    intros [| r rest] H.
    - simpl in H. discriminate H.
    - rewrite classify_cons. rewrite H. reflexivity.
  Qed.

  (** A non-empty reason set with no supersession is [Incomplete], carrying the
      set unchanged. *)
  Theorem classify_incomplete :
    forall reasons,
      reasons <> nil ->
      any_superseded reason is_superseded reasons = false ->
      classify reason is_superseded reasons = Incomplete reason reasons.
  Proof.
    intros [| r rest] Hne H.
    - exfalso. apply Hne. reflexivity.
    - rewrite classify_cons. rewrite H. reflexivity.
  Qed.

  (** {1 The verdict arm and its precedence} *)

  Inductive answer : Type := AAvailable | AUnavailable | AIncomplete.

  Definition verdict_tag (v : verdict reason) : answer :=
    match v with
    | Available _ => AAvailable
    | Unavailable _ _ => AUnavailable
    | Incomplete _ _ => AIncomplete
    end.

  (** Supersession is the sole proof of unavailability: the answer is
      [Unavailable] exactly when the set holds a supersession — necessary and
      sufficient. *)
  Theorem unavailable_iff_superseded :
    forall reasons,
      verdict_tag (classify reason is_superseded reasons) = AUnavailable
      <-> any_superseded reason is_superseded reasons = true.
  Proof.
    intros [| r rest]; split; intro H.
    - simpl in H. discriminate H.
    - simpl in H. discriminate H.
    - rewrite classify_cons in H.
      destruct (any_superseded reason is_superseded (r :: rest)) eqn:E.
      + reflexivity.
      + simpl in H. discriminate H.
    - rewrite classify_cons. rewrite H. reflexivity.
  Qed.

  Lemma any_superseded_cons_true :
    forall r rest,
      is_superseded r = true ->
      any_superseded reason is_superseded (r :: rest) = true.
  Proof. intros r rest Hr. simpl. rewrite Hr. reflexivity. Qed.

  Lemma any_superseded_app :
    forall l1 l2,
      any_superseded reason is_superseded (app l1 l2)
      = orb (any_superseded reason is_superseded l1)
          (any_superseded reason is_superseded l2).
  Proof.
    induction l1 as [| r rest IH]; intros l2.
    - reflexivity.
    - simpl. rewrite IH. rewrite orb_assoc. reflexivity.
  Qed.

  (** [Unavailable] outranks [Incomplete]: a supersession anywhere in the reason
      set forces [Unavailable], whatever else the set holds. *)
  Theorem superseded_dominates :
    forall pre r post,
      is_superseded r = true ->
      verdict_tag (classify reason is_superseded (app pre (r :: post)))
      = AUnavailable.
  Proof.
    intros pre r post Hr.
    apply (proj2 (unavailable_iff_superseded (app pre (r :: post)))).
    rewrite any_superseded_app.
    rewrite (any_superseded_cons_true r post Hr).
    apply orb_true_r.
  Qed.

  (** {1 Order independence}

      The verdict {e arm} depends on the reason set only through emptiness and
      the presence of a supersession, both invariant under reordering; the
      classification is a genuine worst-case fold. This is the confinement
      resolution's order independence in a new costume — there an [omerge]
      semilattice, here the plain [orb] scan. The carried reason list is {e not}
      permutation invariant — it preserves emission order — but the safety
      answer is. *)

  (* The standard permutation relation, defined here since Corelib's prelude
     does not carry it. *)
  Inductive Perm : list reason -> list reason -> Prop :=
  | perm_nil : Perm nil nil
  | perm_skip : forall x l l', Perm l l' -> Perm (x :: l) (x :: l')
  | perm_swap : forall x y l, Perm (x :: y :: l) (y :: x :: l)
  | perm_trans : forall l l' l'', Perm l l' -> Perm l' l'' -> Perm l l''.

  Lemma perm_refl : forall l, Perm l l.
  Proof.
    induction l as [| x rest IH].
    - exact perm_nil.
    - exact (perm_skip x rest rest IH).
  Qed.

  Definition is_nil (l : list reason) : bool :=
    match l with nil => true | _ :: _ => false end.

  Lemma perm_is_nil :
    forall l l', Perm l l' -> is_nil l = is_nil l'.
  Proof.
    intros l l' H. induction H.
    - reflexivity.
    - reflexivity.
    - reflexivity.
    - rewrite IHPerm1. exact IHPerm2.
  Qed.

  Lemma any_superseded_perm :
    forall l l',
      Perm l l' ->
      any_superseded reason is_superseded l
      = any_superseded reason is_superseded l'.
  Proof.
    intros l l' H. induction H.
    - reflexivity.
    - simpl. rewrite IHPerm. reflexivity.
    - simpl. rewrite orb_assoc.
      rewrite (orb_comm (is_superseded x) (is_superseded y)).
      rewrite <- orb_assoc. reflexivity.
    - rewrite IHPerm1. exact IHPerm2.
  Qed.

  Lemma classify_tag_char :
    forall l,
      verdict_tag (classify reason is_superseded l)
      = (if is_nil l then AAvailable
         else if any_superseded reason is_superseded l then AUnavailable
              else AIncomplete).
  Proof.
    intros [| r rest].
    - reflexivity.
    - rewrite classify_cons. cbn [is_nil].
      destruct (any_superseded reason is_superseded (r :: rest)); reflexivity.
  Qed.

  (** The verdict arm is invariant under any permutation of the reason set. *)
  Theorem classify_tag_perm :
    forall l l',
      Perm l l' ->
      verdict_tag (classify reason is_superseded l)
      = verdict_tag (classify reason is_superseded l').
  Proof.
    intros l l' H.
    rewrite classify_tag_char. rewrite classify_tag_char.
    rewrite (perm_is_nil l l' H).
    rewrite (any_superseded_perm l l' H).
    reflexivity.
  Qed.

  (** {1 Faithfulness of the flip}

      The extracted kernel runs [classify]; [State.revertability] previously
      branched on [(reasons, superseded)] — that is [classify_split]. On every
      list the OCaml builds — [reasons] is the supersession sublist followed by
      reasons that are not supersessions — the two agree, so replacing the
      branch with the kernel is behaviour preserving. *)

  Lemma classify_split_nil_nil :
    classify_split reason nil nil = Available reason.
  Proof. reflexivity. Qed.

  Lemma classify_split_cons_nil :
    forall r rest,
      classify_split reason nil (r :: rest) = Incomplete reason (r :: rest).
  Proof. reflexivity. Qed.

  Lemma classify_split_cons_cons :
    forall s ss r rest,
      classify_split reason (s :: ss) (r :: rest) = Unavailable reason (r :: rest).
  Proof. reflexivity. Qed.

  Theorem classify_split_eq_classify :
    forall superseded others,
      all_superseded reason is_superseded superseded = true ->
      any_superseded reason is_superseded others = false ->
      classify_split reason superseded (app superseded others)
      = classify reason is_superseded (app superseded others).
  Proof.
    intros [| s ss] others Hall Hoth.
    - destruct others as [| o orest].
      + reflexivity.
      + change (app nil (o :: orest)) with (o :: orest).
        rewrite classify_split_cons_nil.
        rewrite classify_cons. rewrite Hoth. reflexivity.
    - assert (Hs : is_superseded s = true).
      { simpl in Hall. destruct (is_superseded s);
          [ reflexivity | discriminate Hall ]. }
      change (app (s :: ss) others) with (s :: app ss others).
      rewrite classify_split_cons_cons.
      rewrite classify_cons.
      rewrite (any_superseded_cons_true s (app ss others) Hs).
      reflexivity.
  Qed.

End Laws.
