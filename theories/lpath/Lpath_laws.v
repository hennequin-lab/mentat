(** The lpath laws, machine-checked against the model.

    Statements quantify over the abstract byte type under one hypothesis: byte
    boolean equality decides propositional equality — the same trust boundary
    the sandbox model draws at its segment type. Every law is proved; nothing is
    admitted.

    The headline law is the bridge: on canonical absolute paths,
    [abs_within root p] computes exactly the segment prefix
    [seg_prefixb (components root) (components p)] — the fact the confinement
    proofs assume of [Lpath.Abs.is_within]. Around it: canonicalization strips
    every [.] and [..] (traversal cannot survive normalization), the components
    round-trip through the canonical string (policy.ml's [render]), and
    normalization is idempotent. *)

From Corelib Require Import Init.Prelude.

Open Scope list_scope.
From MentatLpath Require Import Lpath.

Section Laws.

  Variable byte : Type.
  Variable byte_eqb : byte -> byte -> bool.
  Variable slash dot backslash nul colon : byte.
  Variable is_letter : byte -> bool.

  Hypothesis byte_eqb_eq : forall a b, byte_eqb a b = true <-> a = b.

  (* Every model operation partially applied to this section's alphabet, so the
     proofs read as if the alphabet were fixed. The right-hand sides name the
     [Lpath.]-qualified operations; the left-hand sides shadow them. *)
  Local Abbreviation eqb_chars := (Lpath.eqb_chars byte byte_eqb).
  Local Abbreviation forallb_byte := (Lpath.forallb_byte byte).
  Local Abbreviation is_dot := (Lpath.is_dot byte byte_eqb dot).
  Local Abbreviation is_dotdot := (Lpath.is_dotdot byte byte_eqb dot).
  Local Abbreviation is_component_char :=
    (Lpath.is_component_char byte byte_eqb slash backslash nul).
  Local Abbreviation has_drive_prefix :=
    (Lpath.has_drive_prefix byte byte_eqb colon is_letter).
  Local Abbreviation is_nil := (Lpath.is_nil byte).
  Local Abbreviation malformed :=
    (Lpath.malformed byte byte_eqb slash dot backslash nul colon is_letter).
  Local Abbreviation valid_seg :=
    (Lpath.valid_seg byte byte_eqb slash dot backslash nul colon is_letter).
  Local Abbreviation all_valid :=
    (Lpath.all_valid byte byte_eqb slash dot backslash nul colon is_letter).
  Local Abbreviation flush := (Lpath.flush byte).
  Local Abbreviation split_acc := (Lpath.split_acc byte byte_eqb slash).
  Local Abbreviation split_slash := (Lpath.split_slash byte byte_eqb slash).
  Local Abbreviation join := (Lpath.join byte slash).
  Local Abbreviation canon := (Lpath.canon byte slash).
  Local Abbreviation rev_stack := (Lpath.rev_stack byte).
  Local Abbreviation build_abs := (Lpath.build_abs byte slash).
  Local Abbreviation resolve_onto :=
    (Lpath.resolve_onto byte byte_eqb slash dot backslash nul colon is_letter).
  Local Abbreviation abs_of_string :=
    (Lpath.abs_of_string byte byte_eqb slash dot backslash nul colon is_letter).
  Local Abbreviation starts_with_slash := (Lpath.starts_with_slash byte byte_eqb slash).
  Local Abbreviation is_root := (Lpath.is_root byte byte_eqb slash).
  Local Abbreviation abs_components := (Lpath.abs_components byte byte_eqb slash).
  Local Abbreviation prefixb := (Lpath.prefixb byte byte_eqb).
  Local Abbreviation nth_err := (Lpath.nth_err byte).
  Local Abbreviation below_someb := (Lpath.below_someb byte byte_eqb slash).
  Local Abbreviation abs_within := (Lpath.abs_within byte byte_eqb slash).
  Local Abbreviation abs_strictly_within := (Lpath.abs_strictly_within byte byte_eqb slash).
  Local Abbreviation seg_prefixb := (Lpath.seg_prefixb byte byte_eqb).
  Local Abbreviation InB := (Lpath.InB byte).

  (* Keep the grammar predicates and the renderers folded under [simpl] so the
     fold's guards and the canonical form stay syntactically stable. *)
  Arguments Lpath.is_dot : simpl never.
  Arguments Lpath.is_dotdot : simpl never.
  Arguments Lpath.malformed : simpl never.
  Arguments Lpath.valid_seg : simpl never.
  Arguments Lpath.canon : simpl never.
  Arguments Lpath.build_abs : simpl never.
  Arguments Lpath.is_root : simpl never.
  Arguments Lpath.abs_within : simpl never.
  Arguments Lpath.below_someb : simpl never.
  Arguments Lpath.flush : simpl never.

  (* The parse-result constructors carry the byte parameter; infer it. *)
  Arguments Lpath.Empty {byte}.
  Arguments Lpath.Relative {byte}.
  Arguments Lpath.Malformed {byte} c.
  Arguments Lpath.POk {byte} t.
  Arguments Lpath.PErr {byte} e.

  (** {1 Booleans and lists} *)

  Lemma andb_true_split : forall a b, andb a b = true -> a = true /\ b = true.
  Proof.
    intros a b H. destruct a; destruct b; try discriminate. split; reflexivity.
  Qed.

  Lemma orb_false_split : forall a b, orb a b = false -> a = false /\ b = false.
  Proof.
    intros a b H. destruct a; destruct b; try discriminate. split; reflexivity.
  Qed.

  Lemma bool_iff_eq : forall a b : bool, (a = true <-> b = true) -> a = b.
  Proof.
    intros a b [H1 H2]. destruct a; destruct b; try reflexivity.
    - specialize (H1 eq_refl). discriminate.
    - specialize (H2 eq_refl). discriminate.
  Qed.

  Lemma app_nil_r' : forall {A} (l : list A), app l nil = l.
  Proof. intros A l. induction l as [| x l' IH]; simpl; [reflexivity | rewrite IH; reflexivity]. Qed.

  Lemma app_assoc' : forall {A} (a b c : list A), app a (app b c) = app (app a b) c.
  Proof.
    intros A a b c. induction a as [| x a' IH]; simpl; [reflexivity | rewrite IH; reflexivity].
  Qed.

  (** {1 Byte and byte-string equality} *)

  Lemma byte_eqb_refl : forall a, byte_eqb a a = true.
  Proof. intro a. apply byte_eqb_eq. reflexivity. Qed.

  Lemma eqb_chars_eq : forall a b, eqb_chars a b = true <-> a = b.
  Proof.
    intros a. induction a as [| x a' IH]; intros b; destruct b as [| y b'];
      simpl; split; intro H; try reflexivity; try discriminate.
    - destruct (byte_eqb x y) eqn:E1; [| discriminate H].
      simpl in H. apply byte_eqb_eq in E1. apply IH in H.
      rewrite E1. rewrite H. reflexivity.
    - injection H as H1 H2. rewrite H1. rewrite byte_eqb_refl. simpl.
      apply IH. exact H2.
  Qed.

  Lemma eqb_chars_refl : forall a, eqb_chars a a = true.
  Proof. intro a. apply eqb_chars_eq. reflexivity. Qed.

  Lemma eqb_chars_nil : forall l, eqb_chars l nil = is_nil l.
  Proof. intros l. destruct l; reflexivity. Qed.

  (** {1 List prefixes} *)

  Lemma prefixb_exists : forall p q, prefixb p q = true -> exists r, q = app p r.
  Proof.
    induction p as [| a p' IH]; intros q H.
    - exists q. reflexivity.
    - destruct q as [| b q']; [discriminate |]. simpl in H.
      apply andb_true_split in H. destruct H as [Hb Hp].
      apply byte_eqb_eq in Hb. subst b.
      destruct (IH q' Hp) as [r Hr]. exists r. simpl. rewrite Hr. reflexivity.
  Qed.

  Lemma prefixb_app : forall p r, prefixb p (app p r) = true.
  Proof.
    induction p as [| a p' IH]; intros r; simpl; [reflexivity |].
    rewrite byte_eqb_refl. simpl. apply IH.
  Qed.

  Lemma nth_err_app_len :
    forall (R suf : list byte),
      nth_err (app R suf) (length R)
      = match suf with nil => None | b :: _ => Some b end.
  Proof.
    induction R as [| a R' IH]; intros suf; simpl.
    - destruct suf; reflexivity.
    - apply IH.
  Qed.

  Lemma app_cons_r_nonnil : forall (X : list byte) c tl, app X (cons c tl) <> nil.
  Proof. intros X c tl. destruct X as [| a X']; simpl; discriminate. Qed.

  (** {1 Segment prefixes}

      [seg_prefixb] is the sandbox's [withinb] at [seg := list byte]. *)

  Lemma seg_prefixb_refl : forall l, seg_prefixb l l = true.
  Proof.
    induction l as [| x l' IH]; simpl; [reflexivity |].
    rewrite eqb_chars_refl. simpl. exact IH.
  Qed.

  Lemma seg_prefixb_app :
    forall rs ps, seg_prefixb rs ps = true -> exists qs, ps = app rs qs.
  Proof.
    induction rs as [| r rs' IH]; intros ps H.
    - exists ps. reflexivity.
    - destruct ps as [| p ps']; [discriminate |]. simpl in H.
      apply andb_true_split in H. destruct H as [Hr Hs].
      apply eqb_chars_eq in Hr. subst p.
      destruct (IH ps' Hs) as [qs Hqs]. exists qs. simpl. rewrite Hqs. reflexivity.
  Qed.

  (** {1 Joining segments} *)

  Lemma join_single : forall s, join (s :: nil) = s.
  Proof. intros s. reflexivity. Qed.

  (* The non-singleton join equation, without the tail match: a component with a
     non-empty tail renders as itself, a separator, and the joined tail. *)
  Lemma join_cons_nonnil_tail :
    forall r rs', rs' <> nil -> join (r :: rs') = app r (cons slash (join rs')).
  Proof.
    intros r rs' H. destruct rs' as [| x rs'']; [exfalso; apply H; reflexivity |].
    reflexivity.
  Qed.

  Lemma join_app2 :
    forall xs ys,
      xs <> nil -> ys <> nil ->
      join (app xs ys) = app (join xs) (cons slash (join ys)).
  Proof.
    induction xs as [| x xs' IH]; intros ys Hxs Hys.
    - exfalso. apply Hxs. reflexivity.
    - destruct xs' as [| x2 xs''].
      + (* xs = x :: nil *)
        change (app (x :: nil) ys) with (x :: ys).
        rewrite (join_cons_nonnil_tail x ys Hys).
        rewrite join_single. reflexivity.
      + (* xs = x :: x2 :: xs'' *)
        change (app (x :: x2 :: xs'') ys) with (x :: app (x2 :: xs'') ys).
        assert (Hnn : app (x2 :: xs'') ys <> nil) by (simpl; discriminate).
        rewrite (join_cons_nonnil_tail x (app (x2 :: xs'') ys) Hnn).
        assert (Hne : x2 :: xs'' <> nil) by discriminate.
        rewrite (IH ys Hne Hys).
        rewrite (join_cons_nonnil_tail x (x2 :: xs'') Hne).
        rewrite <- app_assoc'. reflexivity.
  Qed.

  (** {1 The component grammar}

      A surviving component is non-empty, is neither [.] nor [..], and is
      slash-free — the facts the normalization and containment laws depend on.
      Everything below is read off [malformed]'s definition. *)

  Lemma negb_true : forall b, negb b = true -> b = false.
  Proof. intros b H. destruct b; [discriminate | reflexivity]. Qed.

  Lemma negb_false : forall b, negb b = false -> b = true.
  Proof. intros b H. destruct b; [reflexivity | discriminate]. Qed.

  Definition slashfree (l : list byte) : bool :=
    forallb_byte (fun b => negb (byte_eqb b slash)) l.

  Lemma malformed_false_inv :
    forall s,
      malformed s = false ->
      is_nil s = false /\ is_dot s = false /\ is_dotdot s = false
      /\ has_drive_prefix s = false /\ forallb_byte is_component_char s = true.
  Proof.
    intros s H. unfold Lpath.malformed in H.
    apply orb_false_split in H. destruct H as [Hn H].
    apply orb_false_split in H. destruct H as [Hd H].
    apply orb_false_split in H. destruct H as [Hdd H].
    apply orb_false_split in H. destruct H as [Hdr Hfa].
    apply negb_false in Hfa.
    split; [exact Hn |]. split; [exact Hd |]. split; [exact Hdd |].
    split; [exact Hdr | exact Hfa].
  Qed.

  Lemma valid_malformed_false : forall s, valid_seg s = true -> malformed s = false.
  Proof.
    intros s H. unfold Lpath.valid_seg in H. apply negb_true in H. exact H.
  Qed.

  Lemma valid_nonnil : forall s, valid_seg s = true -> is_nil s = false.
  Proof.
    intros s H. apply valid_malformed_false in H.
    exact (proj1 (malformed_false_inv s H)).
  Qed.

  Lemma comp_forall_slashfree :
    forall s, forallb_byte is_component_char s = true -> slashfree s = true.
  Proof.
    unfold slashfree. induction s as [| b s' IH]; intros H; [reflexivity |].
    simpl in H. apply andb_true_split in H. destruct H as [Hc Hrest].
    unfold Lpath.is_component_char in Hc. apply andb_true_split in Hc.
    destruct Hc as [_ Hc]. apply andb_true_split in Hc. destruct Hc as [Hslash _].
    simpl. rewrite Hslash. simpl. apply IH. exact Hrest.
  Qed.

  Lemma valid_slashfree : forall s, valid_seg s = true -> slashfree s = true.
  Proof.
    intros s H. apply valid_malformed_false in H.
    apply comp_forall_slashfree.
    exact (proj2 (proj2 (proj2 (proj2 (malformed_false_inv s H))))).
  Qed.

  Lemma is_nil_app_nonnil :
    forall (s x : list byte), is_nil s = false -> is_nil (app s x) = false.
  Proof.
    intros s x H. destruct s as [| a s']; [discriminate H | reflexivity].
  Qed.

  Lemma flush_nonnil : forall s, is_nil s = false -> flush s = s :: nil.
  Proof.
    intros s H. destruct s as [| a s']; [discriminate H | reflexivity].
  Qed.

  (** {1 Splitting inverts joining}

      [split_slash] on a canonical string recovers exactly the component list,
      so [components] round-trips through [render] (the fact policy.ml's
      [of_kernel] relies on). *)

  Lemma split_acc_slashfree :
    forall w cur, slashfree w = true -> split_acc cur w = flush (app cur w).
  Proof.
    induction w as [| b w' IH]; intros cur H.
    - simpl. rewrite app_nil_r'. reflexivity.
    - unfold slashfree in H. simpl in H. apply andb_true_split in H.
      destruct H as [Hb Hw]. apply negb_true in Hb.
      simpl. rewrite Hb.
      rewrite (IH (app cur (b :: nil)) Hw).
      rewrite <- app_assoc'. reflexivity.
  Qed.

  Lemma split_acc_app_slash :
    forall w cur tl,
      slashfree w = true ->
      split_acc cur (app w (cons slash tl))
      = app (flush (app cur w)) (split_acc nil tl).
  Proof.
    induction w as [| b w' IH]; intros cur tl H.
    - simpl. rewrite byte_eqb_refl. rewrite app_nil_r'. reflexivity.
    - unfold slashfree in H. simpl in H. apply andb_true_split in H.
      destruct H as [Hb Hw]. apply negb_true in Hb.
      change (app (b :: w') (cons slash tl)) with (b :: app w' (cons slash tl)).
      simpl. rewrite Hb.
      rewrite (IH (app cur (b :: nil)) tl Hw).
      rewrite <- app_assoc'. reflexivity.
  Qed.

  Lemma split_join :
    forall fin, all_valid fin = true -> split_acc nil (join fin) = fin.
  Proof.
    induction fin as [| s rest IH]; intros H.
    - reflexivity.
    - simpl in H. apply andb_true_split in H. destruct H as [Hvs Hrest].
      assert (Hsf : slashfree s = true) by (apply valid_slashfree; exact Hvs).
      assert (Hnn : is_nil s = false) by (apply valid_nonnil; exact Hvs).
      destruct rest as [| s2 rest'].
      + rewrite join_single.
        rewrite (split_acc_slashfree s nil Hsf).
        change (app nil s) with s. rewrite (flush_nonnil s Hnn). reflexivity.
      + assert (Hne : s2 :: rest' <> nil) by discriminate.
        rewrite (join_cons_nonnil_tail s (s2 :: rest') Hne).
        rewrite (split_acc_app_slash s nil (join (s2 :: rest')) Hsf).
        change (app nil s) with s. rewrite (flush_nonnil s Hnn).
        rewrite (IH Hrest). reflexivity.
  Qed.

  Lemma split_canon_join :
    forall fin, split_slash (canon fin) = split_acc nil (join fin).
  Proof.
    intros fin. unfold Lpath.canon, Lpath.split_slash. simpl.
    rewrite byte_eqb_refl. reflexivity.
  Qed.

  Lemma split_canon :
    forall fin, all_valid fin = true -> split_slash (canon fin) = fin.
  Proof.
    intros fin H. rewrite split_canon_join. apply split_join. exact H.
  Qed.

  (** {1 The canonical form of a component list} *)

  Lemma is_root_canon : forall fin, is_root (canon fin) = is_nil (join fin).
  Proof.
    intros fin. unfold Lpath.is_root, Lpath.canon. simpl. rewrite byte_eqb_refl.
    simpl. apply eqb_chars_nil.
  Qed.

  Lemma join_cons_nonnil :
    forall s rest, valid_seg s = true -> is_nil (join (s :: rest)) = false.
  Proof.
    intros s rest Hvs. assert (Hnn : is_nil s = false) by (apply valid_nonnil; exact Hvs).
    destruct rest as [| s2 rest'].
    - rewrite join_single. exact Hnn.
    - assert (Hne : s2 :: rest' <> nil) by discriminate.
      rewrite (join_cons_nonnil_tail s (s2 :: rest') Hne).
      apply is_nil_app_nonnil. exact Hnn.
  Qed.

  Lemma join_nil_valid :
    forall fin, all_valid fin = true -> join fin = nil -> fin = nil.
  Proof.
    intros fin H Hj. destruct fin as [| s rest]; [reflexivity |].
    simpl in H. apply andb_true_split in H. destruct H as [Hvs _].
    pose proof (join_cons_nonnil s rest Hvs) as Hf.
    rewrite Hj in Hf. discriminate Hf.
  Qed.

  (** The components round-trip: a validated component list rebuilds and
      re-decomposes to itself. [canon] is policy.ml's [render]. *)
  Lemma components_canon :
    forall fin, all_valid fin = true -> abs_components (canon fin) = fin.
  Proof.
    intros fin H. unfold Lpath.abs_components.
    destruct (is_root (canon fin)) eqn:E.
    - rewrite is_root_canon in E.
      assert (Hjn : join fin = nil).
      { destruct (join fin) as [| a l]; [reflexivity | discriminate E]. }
      rewrite (join_nil_valid fin H Hjn). reflexivity.
    - apply split_canon. exact H.
  Qed.

  Lemma canon_injective :
    forall rs ps,
      all_valid rs = true -> all_valid ps = true ->
      canon rs = canon ps -> rs = ps.
  Proof.
    intros rs ps Hrs Hps Heq.
    pose proof (components_canon rs Hrs) as Hr.
    pose proof (components_canon ps Hps) as Hp.
    rewrite Heq in Hr. rewrite Hp in Hr. symmetry. exact Hr.
  Qed.

  Lemma cons_inj_tail :
    forall (a b : byte) x y, a :: x = b :: y -> x = y.
  Proof. intros a b x y H. injection H as _ Hxy. exact Hxy. Qed.

  (** {1 Normalization produces a canonical form}

      Resolution from the root pushes only validated components and reverses to
      the canonical string, so every successful parse names a canonical path,
      and re-parsing it is the identity. *)

  Lemma resolve_onto_cons :
    forall s rest stack,
      resolve_onto stack (s :: rest)
      = (if is_dot s then resolve_onto stack rest
         else if is_dotdot s then
           match stack with
           | nil => resolve_onto nil rest
           | _ :: stk => resolve_onto stk rest
           end
         else if malformed s then PErr (Malformed s)
         else resolve_onto (s :: stack) rest).
  Proof. intros s rest stack. reflexivity. Qed.

  Lemma all_valid_rev_stack :
    forall l acc, all_valid (rev_stack l acc) = andb (all_valid l) (all_valid acc).
  Proof.
    induction l as [| x l' IH]; intros acc; simpl.
    - reflexivity.
    - rewrite (IH (x :: acc)). simpl.
      destruct (valid_seg x); destruct (all_valid l'); destruct (all_valid acc);
        reflexivity.
  Qed.

  Lemma rev_stack_app :
    forall l acc, rev_stack l acc = app (rev_stack l nil) acc.
  Proof.
    induction l as [| x l' IH]; intros acc; simpl.
    - reflexivity.
    - rewrite (IH (x :: acc)). rewrite (IH (x :: nil)).
      rewrite <- app_assoc'. reflexivity.
  Qed.

  Lemma resolve_onto_ok :
    forall segs stack t,
      all_valid stack = true ->
      resolve_onto stack segs = POk t ->
      exists fin, t = canon fin /\ all_valid fin = true.
  Proof.
    induction segs as [| s rest IH]; intros stack t Hstack Hok.
    - simpl in Hok. injection Hok as Hok.
      exists (rev_stack stack nil). split.
      + rewrite <- Hok. unfold Lpath.build_abs. reflexivity.
      + rewrite all_valid_rev_stack. rewrite Hstack. reflexivity.
    - destruct (is_dot s) eqn:Hd.
      + assert (Hstep : resolve_onto stack (s :: rest) = resolve_onto stack rest).
        { rewrite resolve_onto_cons. rewrite Hd. reflexivity. }
        rewrite Hstep in Hok. apply (IH stack t Hstack Hok).
      + destruct (is_dotdot s) eqn:Hdd.
        * destruct stack as [| s0 stk].
          -- assert (Hstep : resolve_onto nil (s :: rest) = resolve_onto nil rest).
             { rewrite resolve_onto_cons. rewrite Hd. rewrite Hdd. reflexivity. }
             rewrite Hstep in Hok. apply (IH nil t Hstack Hok).
          -- assert (Hstep : resolve_onto (s0 :: stk) (s :: rest)
                             = resolve_onto stk rest).
             { rewrite resolve_onto_cons. rewrite Hd. rewrite Hdd. reflexivity. }
             rewrite Hstep in Hok. simpl in Hstack.
             apply andb_true_split in Hstack. destruct Hstack as [_ Hstk].
             apply (IH stk t Hstk Hok).
        * destruct (malformed s) eqn:Hm.
          -- assert (Hstep : resolve_onto stack (s :: rest) = PErr (Malformed s)).
             { rewrite resolve_onto_cons. rewrite Hd. rewrite Hdd. rewrite Hm.
               reflexivity. }
             rewrite Hstep in Hok. discriminate Hok.
          -- assert (Hstep : resolve_onto stack (s :: rest)
                             = resolve_onto (s :: stack) rest).
             { rewrite resolve_onto_cons. rewrite Hd. rewrite Hdd. rewrite Hm.
               reflexivity. }
             rewrite Hstep in Hok.
             assert (Hvs : valid_seg s = true).
             { unfold Lpath.valid_seg. rewrite Hm. reflexivity. }
             assert (Hstack' : all_valid (s :: stack) = true).
             { simpl. rewrite Hvs. rewrite Hstack. reflexivity. }
             apply (IH (s :: stack) t Hstack' Hok).
  Qed.

  Lemma resolve_onto_canon :
    forall fin stack,
      all_valid fin = true ->
      resolve_onto stack fin = POk (canon (app (rev_stack stack nil) fin)).
  Proof.
    induction fin as [| s rest IH]; intros stack H.
    - simpl. unfold Lpath.build_abs. rewrite app_nil_r'. reflexivity.
    - simpl in H. apply andb_true_split in H. destruct H as [Hvs Hrest].
      apply valid_malformed_false in Hvs.
      destruct (malformed_false_inv s Hvs) as [_ [Hd [Hdd _]]].
      assert (Hstep : resolve_onto stack (s :: rest)
                      = resolve_onto (s :: stack) rest).
      { rewrite resolve_onto_cons. rewrite Hd. rewrite Hdd. rewrite Hvs.
        reflexivity. }
      rewrite Hstep. rewrite (IH (s :: stack) Hrest).
      change (rev_stack (s :: stack) nil) with (rev_stack stack (s :: nil)).
      rewrite (rev_stack_app stack (s :: nil)).
      rewrite <- app_assoc'. reflexivity.
  Qed.

  Lemma of_string_canonical :
    forall s t,
      abs_of_string s = POk t ->
      all_valid (abs_components t) = true /\ t = canon (abs_components t).
  Proof.
    intros s t Hok. unfold Lpath.abs_of_string in Hok.
    destruct (is_nil s) eqn:He.
    - discriminate Hok.
    - destruct (starts_with_slash s) eqn:Hs.
      + destruct (resolve_onto_ok (split_slash s) nil t eq_refl Hok)
          as [fin [Ht Hfin]].
        rewrite Ht. rewrite (components_canon fin Hfin).
        split; [exact Hfin | reflexivity].
      + discriminate Hok.
  Qed.

  (** {1 The three normalization laws} *)

  Lemma all_valid_InB :
    forall l c, all_valid l = true -> InB c l -> valid_seg c = true.
  Proof.
    induction l as [| x rest IH]; intros c Hav Hin.
    - destruct Hin.
    - simpl in Hav. apply andb_true_split in Hav. destruct Hav as [Hx Hrest].
      destruct Hin as [Heq | Hin].
      + rewrite <- Heq. exact Hx.
      + exact (IH c Hrest Hin).
  Qed.

  (** Traversal cannot survive normalization: no component of a normalized
      absolute path is [.] or [..] (or empty), so containment cannot be fooled
      by ["/a/../../etc"]. *)
  Theorem components_no_traversal :
    forall s t,
      abs_of_string s = POk t ->
      forall c,
        InB c (abs_components t) ->
        c <> nil /\ is_dot c = false /\ is_dotdot c = false.
  Proof.
    intros s t Hok c Hin.
    destruct (of_string_canonical s t Hok) as [Hav _].
    pose proof (all_valid_InB (abs_components t) c Hav Hin) as Hvs.
    apply valid_malformed_false in Hvs.
    destruct (malformed_false_inv c Hvs) as [Hn [Hd [Hdd _]]].
    split; [| split; [exact Hd | exact Hdd]].
    intro Hc. rewrite Hc in Hn. discriminate Hn.
  Qed.

  (** The components round-trip through the canonical string: for a validated
      component list [segs], [canon segs] (policy.ml's [render]) re-parses and
      re-decomposes to [segs]. *)
  Theorem render_round_trip :
    forall segs,
      all_valid segs = true ->
      abs_components (canon segs) = segs
      /\ abs_of_string (canon segs) = POk (canon segs).
  Proof.
    intros segs H. split; [apply components_canon; exact H |].
    unfold Lpath.abs_of_string.
    assert (Hnil : is_nil (canon segs) = false) by (unfold Lpath.canon; reflexivity).
    assert (Hsl : starts_with_slash (canon segs) = true).
    { unfold Lpath.canon, Lpath.starts_with_slash. apply byte_eqb_refl. }
    rewrite Hnil. rewrite Hsl. rewrite (split_canon segs H).
    rewrite (resolve_onto_canon segs nil H). reflexivity.
  Qed.

  (** Canonicalization is idempotent: re-parsing a normalized path is the
      identity. *)
  Theorem canonicalization_idempotent :
    forall s t, abs_of_string s = POk t -> abs_of_string t = POk t.
  Proof.
    intros s t Hok.
    destruct (of_string_canonical s t Hok) as [Hav Ht].
    destruct (render_round_trip (abs_components t) Hav) as [_ Hos].
    rewrite <- Ht in Hos. exact Hos.
  Qed.

  (** {1 The bridge lemma}

      On canonical absolute paths, [abs_within] — lpath's string-level
      containment with the [/]-boundary guard — computes exactly the segment
      prefix over the two component lists. This is the fact the sandbox proofs
      assume of [Lpath.Abs.is_within]. *)

  Definition slash_head_nilb (z : list byte) : bool :=
    match z with nil => true | c :: _ => byte_eqb c slash end.

  Lemma slash_head_cons_slash : forall X, slash_head_nilb (cons slash X) = true.
  Proof. intros X. simpl. apply byte_eqb_refl. Qed.

  Lemma slashfree_cons :
    forall b l, slashfree (b :: l) = andb (negb (byte_eqb b slash)) (slashfree l).
  Proof. intros b l. reflexivity. Qed.

  Lemma slashfree_app_slash_false :
    forall w tl, slashfree (app w (cons slash tl)) = false.
  Proof.
    induction w as [| b w' IH]; intros tl.
    - change (app nil (cons slash tl)) with (cons slash tl).
      rewrite slashfree_cons. rewrite byte_eqb_refl. reflexivity.
    - change (app (b :: w') (cons slash tl)) with (b :: app w' (cons slash tl)).
      rewrite slashfree_cons. rewrite (IH tl).
      destruct (negb (byte_eqb b slash)); reflexivity.
  Qed.

  Lemma slashfree_split_unique :
    forall a b x y,
      slashfree a = true -> slashfree b = true ->
      slash_head_nilb x = true -> slash_head_nilb y = true ->
      app a x = app b y -> a = b /\ x = y.
  Proof.
    induction a as [| a0 a' IH]; intros b x y Ha Hb Hx Hy Heq.
    - destruct b as [| b0 b'].
      + simpl in Heq. split; [reflexivity | exact Heq].
      + simpl in Heq. rewrite Heq in Hx. simpl in Hx.
        rewrite slashfree_cons in Hb. apply andb_true_split in Hb.
        destruct Hb as [Hb0 _]. apply negb_true in Hb0.
        rewrite Hx in Hb0. discriminate Hb0.
    - destruct b as [| b0 b'].
      + simpl in Heq. rewrite <- Heq in Hy. simpl in Hy.
        rewrite slashfree_cons in Ha. apply andb_true_split in Ha.
        destruct Ha as [Ha0 _]. apply negb_true in Ha0.
        rewrite Hy in Ha0. discriminate Ha0.
      + simpl in Heq. injection Heq as Hab Hxy.
        rewrite slashfree_cons in Ha. apply andb_true_split in Ha.
        destruct Ha as [_ Ha'].
        rewrite slashfree_cons in Hb. apply andb_true_split in Hb.
        destruct Hb as [_ Hb'].
        destruct (IH b' x y Ha' Hb' Hx Hy Hxy) as [Hab' Hxy'].
        split; [rewrite Hab; rewrite Hab'; reflexivity | exact Hxy'].
  Qed.

  Lemma join_eq_prefix :
    forall rs ps rest,
      all_valid rs = true -> all_valid ps = true ->
      join ps = app (join rs) (cons slash rest) ->
      seg_prefixb rs ps = true.
  Proof.
    induction rs as [| r rs' IH]; intros ps rest Hrs Hps Heq.
    - reflexivity.
    - simpl in Hrs. apply andb_true_split in Hrs. destruct Hrs as [Hvr Hrs'].
      assert (Hsr : slashfree r = true) by (apply valid_slashfree; exact Hvr).
      destruct ps as [| p ps'].
      + exfalso. apply (app_cons_r_nonnil (join (r :: rs')) slash rest).
        rewrite <- Heq. reflexivity.
      + simpl in Hps. apply andb_true_split in Hps. destruct Hps as [Hvp Hps'].
        assert (Hsp : slashfree p = true) by (apply valid_slashfree; exact Hvp).
        destruct ps' as [| q qs].
        * rewrite join_single in Heq. rewrite Heq in Hsp.
          rewrite slashfree_app_slash_false in Hsp. discriminate Hsp.
        * assert (Hpne : q :: qs <> nil) by discriminate.
          rewrite (join_cons_nonnil_tail p (q :: qs) Hpne) in Heq.
          destruct rs' as [| x rs''].
          -- rewrite join_single in Heq.
             destruct (slashfree_split_unique p r (cons slash (join (q :: qs)))
                         (cons slash rest) Hsp Hsr
                         (slash_head_cons_slash (join (q :: qs)))
                         (slash_head_cons_slash rest) Heq) as [Hpr _].
             simpl. rewrite Hpr. rewrite eqb_chars_refl. reflexivity.
          -- assert (Hrne : x :: rs'' <> nil) by discriminate.
             rewrite (join_cons_nonnil_tail r (x :: rs'') Hrne) in Heq.
             rewrite <- app_assoc' in Heq.
             change (app (cons slash (join (x :: rs''))) (cons slash rest))
               with (cons slash (app (join (x :: rs'')) (cons slash rest))) in Heq.
             destruct (slashfree_split_unique p r (cons slash (join (q :: qs)))
                         (cons slash (app (join (x :: rs'')) (cons slash rest)))
                         Hsp Hsr (slash_head_cons_slash (join (q :: qs)))
                         (slash_head_cons_slash _) Heq) as [Hpr Htail].
             pose proof (cons_inj_tail slash slash (join (q :: qs))
                           (app (join (x :: rs'')) (cons slash rest)) Htail) as Hjoin.
             simpl. rewrite Hpr. rewrite eqb_chars_refl. simpl.
             apply (IH (q :: qs) rest Hrs' Hps' Hjoin).
  Qed.

  (* [below_someb] holds exactly when the prefix sits at a component boundary:
     the target is the prefix, a separator, then a remainder. *)
  Lemma below_someb_inv :
    forall R P, below_someb R P = true -> exists rest, P = app R (cons slash rest).
  Proof.
    intros R P H. unfold Lpath.below_someb in H.
    destruct (nth_err P (length R)) as [b |] eqn:Hn; [| discriminate H].
    apply andb_true_split in H. destruct H as [Hb Hp].
    apply byte_eqb_eq in Hb. subst b.
    destruct (prefixb_exists R P Hp) as [suf Hsuf].
    rewrite Hsuf in Hn. rewrite nth_err_app_len in Hn.
    destruct suf as [| c rest]; [discriminate Hn |].
    injection Hn as Hc. subst c. exists rest. rewrite Hsuf. reflexivity.
  Qed.

  Lemma within_canon_iff :
    forall rs ps,
      all_valid rs = true -> all_valid ps = true ->
      (abs_within (canon rs) (canon ps) = true <-> seg_prefixb rs ps = true).
  Proof.
    intros rs ps Hrs Hps. split.
    - intro Hw. unfold Lpath.abs_within in Hw.
      destruct (eqb_chars (canon ps) (canon rs)) eqn:Heq.
      + apply eqb_chars_eq in Heq.
        pose proof (canon_injective ps rs Hps Hrs Heq) as Hpr.
        rewrite Hpr. apply seg_prefixb_refl.
      + destruct (is_root (canon rs)) eqn:Hir.
        * rewrite is_root_canon in Hir.
          assert (Hjn : join rs = nil).
          { destruct (join rs) as [| a l]; [reflexivity | discriminate Hir]. }
          rewrite (join_nil_valid rs Hrs Hjn). reflexivity.
        * destruct (below_someb_inv (canon rs) (canon ps) Hw) as [rest Hrest].
          unfold Lpath.canon in Hrest.
          change (app (cons slash (join rs)) (cons slash rest))
            with (cons slash (app (join rs) (cons slash rest))) in Hrest.
          pose proof (cons_inj_tail slash slash (join ps)
                        (app (join rs) (cons slash rest)) Hrest) as Hjoin.
          apply (join_eq_prefix rs ps rest Hrs Hps Hjoin).
    - intro Hsp. destruct (seg_prefixb_app rs ps Hsp) as [qs Hqs].
      unfold Lpath.abs_within.
      destruct rs as [| r rs'].
      + destruct (eqb_chars (canon ps) (canon nil)); [reflexivity |].
        assert (Hir : is_root (canon nil) = true) by (rewrite is_root_canon; reflexivity).
        rewrite Hir. reflexivity.
      + destruct qs as [| q qs'].
        * rewrite app_nil_r' in Hqs. rewrite Hqs.
          rewrite eqb_chars_refl. reflexivity.
        * assert (Hbelow : below_someb (canon (r :: rs')) (canon ps) = true).
          { unfold Lpath.below_someb.
            assert (Hcp : canon ps
                          = app (canon (r :: rs')) (cons slash (join (q :: qs')))).
            { unfold Lpath.canon. rewrite Hqs.
              assert (Hrne : r :: rs' <> nil) by discriminate.
              assert (Hqne : q :: qs' <> nil) by discriminate.
              rewrite (join_app2 (r :: rs') (q :: qs') Hrne Hqne).
              change (cons slash (app (join (r :: rs')) (cons slash (join (q :: qs')))))
                with (app (cons slash (join (r :: rs'))) (cons slash (join (q :: qs')))).
              reflexivity. }
            rewrite Hcp. rewrite nth_err_app_len. simpl. rewrite byte_eqb_refl.
            rewrite prefixb_app. reflexivity. }
          destruct (eqb_chars (canon ps) (canon (r :: rs'))); [reflexivity |].
          destruct (is_root (canon (r :: rs'))); [reflexivity |].
          exact Hbelow.
  Qed.

  Lemma bridge_canon :
    forall rs ps,
      all_valid rs = true -> all_valid ps = true ->
      abs_within (canon rs) (canon ps) = seg_prefixb rs ps.
  Proof.
    intros rs ps Hrs Hps. apply bool_iff_eq. apply within_canon_iff; assumption.
  Qed.

  (** The bridge, over raw inputs: for any two paths that parse, [abs_within]
      holds iff the first's components are a segment prefix of the second's —
      exactly the fact the confinement proofs assume of [Lpath.Abs.is_within].
      This is what closes the trust gap. *)
  Theorem within_iff_components_prefix :
    forall s1 s2 t1 t2,
      abs_of_string s1 = POk t1 -> abs_of_string s2 = POk t2 ->
      abs_within t1 t2
      = seg_prefixb (abs_components t1) (abs_components t2).
  Proof.
    intros s1 s2 t1 t2 H1 H2.
    destruct (of_string_canonical s1 t1 H1) as [Hv1 Ht1].
    destruct (of_string_canonical s2 t2 H2) as [Hv2 Ht2].
    set (c1 := abs_components t1) in *.
    set (c2 := abs_components t2) in *.
    rewrite Ht1. rewrite Ht2.
    apply (bridge_canon c1 c2 Hv1 Hv2).
  Qed.

  (** The strict predicate is the inclusive bridge minus the equality boundary,
      the same separation the mli draws. *)
  Theorem strictly_within_iff_components_prefix :
    forall s1 s2 t1 t2,
      abs_of_string s1 = POk t1 -> abs_of_string s2 = POk t2 ->
      abs_strictly_within t1 t2
      = andb (seg_prefixb (abs_components t1) (abs_components t2))
             (negb (eqb_chars t1 t2)).
  Proof.
    intros s1 s2 t1 t2 H1 H2. unfold Lpath.abs_strictly_within.
    rewrite (within_iff_components_prefix s1 s2 t1 t2 H1 H2). reflexivity.
  Qed.

End Laws.
