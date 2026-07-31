(** Verified model of the permission decision kernel.

    This is the formal counterpart of the decision rule [policy.mli] states in
    prose. Evaluation is ordered and conservative: the first matching rule
    decides an access, rules are checked before runtime grants, a denied access
    stops the request, and an access matched by no rule and no grant needs
    review. A grouped request proceeds only when every access is allowed, any
    access needing review reviews the whole request, and any denied access
    denies it — the precedence [Denied] over [Review] over [Allowed].

    The model mirrors [policy.ml]'s [explain] and [decide] operation for
    operation. Accesses, matchers, and rules stay abstract: the kernel needs
    only three host predicates — whether a matcher accepts an access ([matches],
    the OCaml [Match.matches]); a rule's matcher and action ([rule_matcher],
    [rule_action], the record projections); and whether a runtime grant allows
    an access ([allows], [Grants.allows]). Everything the matcher variant, its
    classifier, and grant storage do stays in OCaml, exactly as the sandbox
    kernel left path spelling in OCaml.

    The laws follow the model in this same file. Statements quantify over all
    inputs; nothing is admitted. The two fail-closed families of theorems carry
    one hypothesis — a matcher that accepts every access — which the OCaml [Any]
    matcher realizes, so they are not vacuous. *)

From Corelib Require Import Init.Prelude.

Open Scope list_scope.

(* The action a matching rule applies. Kept outside the section so extraction
   emits a plain enum; [rule_action] projects it from a host rule. *)
Inductive action : Type := Allow | Review | Deny.

Section Decision.

  Variable access : Type.
  Variable matcher : Type.
  Variable rule : Type.

  Variable matches : matcher -> access -> bool.
  Variable rule_matcher : rule -> matcher.
  Variable rule_action : rule -> action.

  (** {1 The model} *)

  (** {2 Per-access explanation} *)

  (* Why one access resolved as it did — [policy.ml]'s [explanation]. The [rule]
     carried is the deciding rule; the two grant-side outcomes carry none. *)
  Inductive explanation : Type :=
  | Allowed_by_rule (r : rule)
  | Allowed_by_grant
  | Needs_review
  | Needs_review_by_rule (r : rule)
  | Denied_by_rule (r : rule).

  Definition explanation_of_rule (r : rule) : explanation :=
    match rule_action r with
    | Allow => Allowed_by_rule r
    | Review => Needs_review_by_rule r
    | Deny => Denied_by_rule r
    end.

  Lemma explanation_of_rule_allow :
    forall r, rule_action r = Allow -> explanation_of_rule r = Allowed_by_rule r.
  Proof. intros r H. unfold explanation_of_rule. rewrite H. reflexivity. Qed.

  Lemma explanation_of_rule_review :
    forall r,
      rule_action r = Review -> explanation_of_rule r = Needs_review_by_rule r.
  Proof. intros r H. unfold explanation_of_rule. rewrite H. reflexivity. Qed.

  Lemma explanation_of_rule_deny :
    forall r, rule_action r = Deny -> explanation_of_rule r = Denied_by_rule r.
  Proof. intros r H. unfold explanation_of_rule. rewrite H. reflexivity. Qed.

  (* The first matching rule decides; past the last rule a grant allows and
     otherwise the access needs review. Grants are consulted only after every
     rule has failed to match, which is what makes rules authoritative over
     grants. *)
  Fixpoint explain (allows : access -> bool) (rules : list rule) (a : access) :
      explanation :=
    match rules with
    | nil => if allows a then Allowed_by_grant else Needs_review
    | r :: rest =>
        if matches (rule_matcher r) a then explanation_of_rule r
        else explain allows rest a
    end.

  (** {2 Aggregation over a request} *)

  (* The captured reason an access needs review — [policy.ml]'s [Review.reason].
     [By_rule] retains the deciding review rule. *)
  Inductive reason : Type := Unmatched | By_rule (r : rule).

  (* The decision over a whole request. [Denied] keeps the first denied access
     and the rest, both in request order, as [Decision.Denied (first, rest)]
     does; [Review] keeps every access needing review with its captured reason.
     The OCaml wraps these with the request into [Denial.t] / [Review.t]. *)
  Inductive outcome : Type :=
  | Allowed
  | Reviewed (reasons : list (access * reason))
  | Denied (first : access * rule) (rest : list (access * rule)).

  (* One pass over the request's accesses, collecting the denied accesses with
     their rules and the review accesses with their reasons, both in request
     order — the two lists [decide]'s loop builds with reversed accumulators,
     produced here directly by prepending on return. *)
  Fixpoint gather (allows : access -> bool) (rules : list rule)
      (accesses : list access) :
      (list (access * rule) * list (access * reason)) :=
    match accesses with
    | nil => (nil, nil)
    | a :: rest =>
        let (denials, reviews) := gather allows rules rest in
        match explain allows rules a with
        | Denied_by_rule r => ((a, r) :: denials, reviews)
        | Allowed_by_rule _ => (denials, reviews)
        | Allowed_by_grant => (denials, reviews)
        | Needs_review => (denials, (a, Unmatched) :: reviews)
        | Needs_review_by_rule r => (denials, (a, By_rule r) :: reviews)
        end
    end.

  (* Denied outranks review outranks allowed: any denial denies the request;
     otherwise any review reviews it; otherwise it is allowed. *)
  Definition classify (allows : access -> bool) (rules : list rule)
      (accesses : list access) : outcome :=
    let (denials, reviews) := gather allows rules accesses in
    match denials with
    | (a, r) :: rest => Denied (a, r) rest
    | nil =>
        match reviews with
        | nil => Allowed
        | _ :: _ => Reviewed reviews
        end
    end.

  (** {1 The laws} *)

  (** {2 Boolean and list helpers used only in statements and proofs} *)

  Definition denied1 (allows : access -> bool) (rules : list rule) (a : access) :
      bool :=
    match explain allows rules a with Denied_by_rule _ => true | _ => false end.

  Definition review1 (allows : access -> bool) (rules : list rule) (a : access) :
      bool :=
    match explain allows rules a with
    | Needs_review => true
    | Needs_review_by_rule _ => true
    | _ => false
    end.

  Definition allowed1 (allows : access -> bool) (rules : list rule)
      (a : access) : bool :=
    match explain allows rules a with
    | Allowed_by_rule _ => true
    | Allowed_by_grant => true
    | _ => false
    end.

  Fixpoint lany (f : access -> bool) (l : list access) : bool :=
    match l with nil => false | a :: rest => orb (f a) (lany f rest) end.

  Fixpoint lall (f : access -> bool) (l : list access) : bool :=
    match l with nil => true | a :: rest => andb (f a) (lall f rest) end.

  Fixpoint lfilter (f : access -> bool) (l : list access) : list access :=
    match l with
    | nil => nil
    | a :: rest => if f a then a :: lfilter f rest else lfilter f rest
    end.

  (* Spec projections of [gather]'s two lists as single fixpoints — easier to
     reason about than the paired pass, and proved equal to it below. *)
  Fixpoint denied_of (allows : access -> bool) (rules : list rule)
      (accesses : list access) : list (access * rule) :=
    match accesses with
    | nil => nil
    | a :: rest =>
        match explain allows rules a with
        | Denied_by_rule r => (a, r) :: denied_of allows rules rest
        | Allowed_by_rule _ => denied_of allows rules rest
        | Allowed_by_grant => denied_of allows rules rest
        | Needs_review => denied_of allows rules rest
        | Needs_review_by_rule _ => denied_of allows rules rest
        end
    end.

  Fixpoint reviews_of (allows : access -> bool) (rules : list rule)
      (accesses : list access) : list (access * reason) :=
    match accesses with
    | nil => nil
    | a :: rest =>
        match explain allows rules a with
        | Needs_review => (a, Unmatched) :: reviews_of allows rules rest
        | Needs_review_by_rule r =>
            (a, By_rule r) :: reviews_of allows rules rest
        | Allowed_by_rule _ => reviews_of allows rules rest
        | Allowed_by_grant => reviews_of allows rules rest
        | Denied_by_rule _ => reviews_of allows rules rest
        end
    end.

  Fixpoint denial_paths (l : list (access * rule)) : list access :=
    match l with nil => nil | (a, _) :: rest => a :: denial_paths rest end.

  (* Defining equations, so a step can be exposed without [simpl] unfolding
     [explain] under it. *)
  Lemma denied_of_cons :
    forall allows rules a accs,
      denied_of allows rules (a :: accs) =
      match explain allows rules a with
      | Denied_by_rule r => (a, r) :: denied_of allows rules accs
      | Allowed_by_rule _ => denied_of allows rules accs
      | Allowed_by_grant => denied_of allows rules accs
      | Needs_review => denied_of allows rules accs
      | Needs_review_by_rule _ => denied_of allows rules accs
      end.
  Proof. reflexivity. Qed.

  Lemma reviews_of_cons :
    forall allows rules a accs,
      reviews_of allows rules (a :: accs) =
      match explain allows rules a with
      | Needs_review => (a, Unmatched) :: reviews_of allows rules accs
      | Needs_review_by_rule r =>
          (a, By_rule r) :: reviews_of allows rules accs
      | Allowed_by_rule _ => reviews_of allows rules accs
      | Allowed_by_grant => reviews_of allows rules accs
      | Denied_by_rule _ => reviews_of allows rules accs
      end.
  Proof. reflexivity. Qed.

  Lemma orb_false_split : forall a b, orb a b = false -> a = false /\ b = false.
  Proof.
    intros a b H. destruct a; destruct b; try discriminate H.
    split; reflexivity.
  Qed.

  (** {2 Gather computes the two spec lists} *)

  Lemma gather_denials :
    forall allows rules accesses,
      fst (gather allows rules accesses) = denied_of allows rules accesses.
  Proof.
    intros allows rules accesses.
    induction accesses as [| a rest IH]; simpl; [reflexivity |].
    destruct (gather allows rules rest) as [denials reviews]. simpl in IH.
    destruct (explain allows rules a); simpl; rewrite IH; reflexivity.
  Qed.

  Lemma gather_reviews :
    forall allows rules accesses,
      snd (gather allows rules accesses) = reviews_of allows rules accesses.
  Proof.
    intros allows rules accesses.
    induction accesses as [| a rest IH]; simpl; [reflexivity |].
    destruct (gather allows rules rest) as [denials reviews]. simpl in IH.
    destruct (explain allows rules a); simpl; rewrite IH; reflexivity.
  Qed.

  (* The whole decision, expressed on the spec lists. *)
  Lemma classify_eq :
    forall allows rules accesses,
      classify allows rules accesses =
      match denied_of allows rules accesses with
      | (a, r) :: rest => Denied (a, r) rest
      | nil =>
          match reviews_of allows rules accesses with
          | nil => Allowed
          | _ :: _ => Reviewed (reviews_of allows rules accesses)
          end
      end.
  Proof.
    intros allows rules accesses. unfold classify.
    rewrite <- (gather_denials allows rules accesses).
    rewrite <- (gather_reviews allows rules accesses).
    destruct (gather allows rules accesses) as [denials reviews]. reflexivity.
  Qed.

  (** {2 Totality: every access lands in exactly one category} *)

  Theorem explanation_total :
    forall allows rules a,
      orb (denied1 allows rules a)
        (orb (review1 allows rules a) (allowed1 allows rules a)) = true.
  Proof.
    intros allows rules a. unfold denied1, review1, allowed1.
    destruct (explain allows rules a); reflexivity.
  Qed.

  Theorem explanation_exclusive :
    forall allows rules a,
      (denied1 allows rules a = true ->
       review1 allows rules a = false /\ allowed1 allows rules a = false)
      /\ (review1 allows rules a = true ->
          denied1 allows rules a = false /\ allowed1 allows rules a = false)
      /\ (allowed1 allows rules a = true ->
          denied1 allows rules a = false /\ review1 allows rules a = false).
  Proof.
    intros allows rules a. unfold denied1, review1, allowed1.
    destruct (explain allows rules a);
      (split; [| split]); intro H;
      solve [ discriminate H | split; reflexivity ].
  Qed.

  (** {2 First matching rule wins; rules outrank grants} *)

  Fixpoint first_match (rules : list rule) (a : access) : option rule :=
    match rules with
    | nil => None
    | r :: rest =>
        if matches (rule_matcher r) a then Some r else first_match rest a
    end.

  Theorem explain_first_match_some :
    forall allows rules a r,
      first_match rules a = Some r ->
      explain allows rules a = explanation_of_rule r.
  Proof.
    intros allows rules a r. induction rules as [| r0 rest IH]; simpl.
    - discriminate.
    - destruct (matches (rule_matcher r0) a) eqn:E.
      + intro H. injection H as <-. reflexivity.
      + exact IH.
  Qed.

  Theorem explain_first_match_none :
    forall allows rules a,
      first_match rules a = None ->
      explain allows rules a =
      (if allows a then Allowed_by_grant else Needs_review).
  Proof.
    intros allows rules a. induction rules as [| r0 rest IH]; simpl.
    - reflexivity.
    - destruct (matches (rule_matcher r0) a) eqn:E.
      + discriminate.
      + exact IH.
  Qed.

  (** A matched access is decided without consulting grants: with the same
      rules, its explanation is the same under any two grant sets. *)
  Theorem rules_before_grants :
    forall allows allows' rules a r,
      first_match rules a = Some r ->
      explain allows rules a = explain allows' rules a.
  Proof.
    intros allows allows' rules a r H.
    rewrite (explain_first_match_some allows rules a r H).
    rewrite (explain_first_match_some allows' rules a r H). reflexivity.
  Qed.

  (** {2 Denials do not depend on grants} *)

  Lemma denied1_grant_independent :
    forall allows allows' rules a,
      denied1 allows rules a = denied1 allows' rules a.
  Proof.
    intros allows allows' rules a. unfold denied1.
    destruct (first_match rules a) as [r |] eqn:E.
    - rewrite (explain_first_match_some allows rules a r E).
      rewrite (explain_first_match_some allows' rules a r E). reflexivity.
    - rewrite (explain_first_match_none allows rules a E).
      rewrite (explain_first_match_none allows' rules a E).
      destruct (allows a); destruct (allows' a); reflexivity.
  Qed.

  Lemma denied_of_grant_independent :
    forall allows allows' rules accesses,
      denied_of allows rules accesses = denied_of allows' rules accesses.
  Proof.
    intros allows allows' rules accesses.
    induction accesses as [| a rest IH]; simpl; [reflexivity |].
    destruct (first_match rules a) as [r |] eqn:E.
    - rewrite (explain_first_match_some allows rules a r E).
      rewrite (explain_first_match_some allows' rules a r E).
      destruct (explanation_of_rule r); rewrite IH; reflexivity.
    - rewrite (explain_first_match_none allows rules a E).
      rewrite (explain_first_match_none allows' rules a E).
      destruct (allows a); destruct (allows' a); rewrite IH; reflexivity.
  Qed.

  (** {2 The decision precedence} *)

  Lemma denied_of_nil_iff :
    forall allows rules accesses,
      denied_of allows rules accesses = nil <->
      lany (denied1 allows rules) accesses = false.
  Proof.
    intros allows rules accesses.
    induction accesses as [| a rest IH]; simpl.
    - split; reflexivity.
    - unfold denied1. destruct (explain allows rules a); simpl.
      + (* Allowed_by_rule *) exact IH.
      + (* Allowed_by_grant *) exact IH.
      + (* Needs_review *) exact IH.
      + (* Needs_review_by_rule *) exact IH.
      + (* Denied_by_rule *) split; intro H; discriminate H.
  Qed.

  Lemma reviews_of_nil_iff :
    forall allows rules accesses,
      reviews_of allows rules accesses = nil <->
      lany (review1 allows rules) accesses = false.
  Proof.
    intros allows rules accesses.
    induction accesses as [| a rest IH]; simpl.
    - split; reflexivity.
    - unfold review1. destruct (explain allows rules a); simpl.
      + exact IH.
      + exact IH.
      + split; intro H; discriminate H.
      + split; intro H; discriminate H.
      + exact IH.
  Qed.

  Theorem classify_denied :
    forall allows rules accesses a r rest,
      denied_of allows rules accesses = (a, r) :: rest ->
      classify allows rules accesses = Denied (a, r) rest.
  Proof.
    intros allows rules accesses a r rest H.
    rewrite classify_eq. rewrite H. reflexivity.
  Qed.

  Theorem classify_denied_inv :
    forall allows rules accesses a r rest,
      classify allows rules accesses = Denied (a, r) rest ->
      denied_of allows rules accesses = (a, r) :: rest.
  Proof.
    intros allows rules accesses a r rest H. rewrite classify_eq in H.
    destruct (denied_of allows rules accesses) as [| [a' r'] rest'].
    - destruct (reviews_of allows rules accesses); discriminate H.
    - injection H as <- <- <-. reflexivity.
  Qed.

  (** Any denied access denies the request, whatever the reviews. *)
  Theorem denied_dominates :
    forall allows rules accesses,
      lany (denied1 allows rules) accesses = true ->
      exists a r rest, classify allows rules accesses = Denied (a, r) rest.
  Proof.
    intros allows rules accesses H.
    destruct (denied_of allows rules accesses) as [| [a r] rest] eqn:E.
    - apply (proj1 (denied_of_nil_iff allows rules accesses)) in E.
      rewrite E in H. discriminate H.
    - exists a, r, rest. apply classify_denied. exact E.
  Qed.

  (** Allowed exactly when no access is denied and none needs review. *)
  Theorem classify_allowed_iff :
    forall allows rules accesses,
      classify allows rules accesses = Allowed <->
      lany (denied1 allows rules) accesses = false /\
      lany (review1 allows rules) accesses = false.
  Proof.
    intros allows rules accesses. rewrite classify_eq. split.
    - intro H.
      destruct (denied_of allows rules accesses) as [| [a r] rest] eqn:Ed.
      + destruct (reviews_of allows rules accesses) eqn:Er.
        * split.
          -- apply (denied_of_nil_iff allows rules accesses). exact Ed.
          -- apply (reviews_of_nil_iff allows rules accesses). exact Er.
        * discriminate H.
      + discriminate H.
    - intros [Hd Hr].
      rewrite (proj2 (denied_of_nil_iff allows rules accesses) Hd).
      rewrite (proj2 (reviews_of_nil_iff allows rules accesses) Hr).
      reflexivity.
  Qed.

  (** Review exactly when no access is denied and at least one needs review. *)
  Theorem classify_review_iff :
    forall allows rules accesses,
      (exists reasons, classify allows rules accesses = Reviewed reasons) <->
      lany (denied1 allows rules) accesses = false /\
      lany (review1 allows rules) accesses = true.
  Proof.
    intros allows rules accesses. rewrite classify_eq. split.
    - intros [reasons H].
      destruct (denied_of allows rules accesses) as [| [a r] rest] eqn:Ed.
      + destruct (reviews_of allows rules accesses) eqn:Er.
        * discriminate H.
        * split.
          -- apply (denied_of_nil_iff allows rules accesses). exact Ed.
          -- destruct (lany (review1 allows rules) accesses) eqn:Ha;
               [reflexivity |].
             apply (proj2 (reviews_of_nil_iff allows rules accesses)) in Ha.
             rewrite Ha in Er. discriminate Er.
      + discriminate H.
    - intros [Hd Hr].
      rewrite (proj2 (denied_of_nil_iff allows rules accesses) Hd).
      destruct (reviews_of allows rules accesses) eqn:Er.
      + apply (reviews_of_nil_iff allows rules accesses) in Er.
        rewrite Er in Hr. discriminate Hr.
      + eexists. reflexivity.
  Qed.

  (** {2 Denials name every denied access in request order} *)

  Theorem denials_in_request_order :
    forall allows rules accesses,
      denial_paths (denied_of allows rules accesses) =
      lfilter (denied1 allows rules) accesses.
  Proof.
    intros allows rules accesses.
    induction accesses as [| a rest IH]; simpl; [reflexivity |].
    unfold denied1. destruct (explain allows rules a); simpl; rewrite IH;
      reflexivity.
  Qed.

  (** {2 Grants widen: the monotonicity law} *)

  (* The order the ladder widens along is [Denied] < [Review] < [Allowed]:
     adding grants can only carry an access up it, never down. *)

  (** Grant widening neither introduces nor removes a denial, and keeps the
      exact denied list: the denial set is a function of the rules alone. *)
  Theorem grant_denial_stable :
    forall allows allows' rules accesses a r rest,
      classify allows rules accesses = Denied (a, r) rest <->
      classify allows' rules accesses = Denied (a, r) rest.
  Proof.
    intros allows allows' rules accesses a r rest. split; intro H.
    - apply classify_denied.
      rewrite <- (denied_of_grant_independent allows allows' rules accesses).
      apply classify_denied_inv. exact H.
    - apply classify_denied.
      rewrite (denied_of_grant_independent allows allows' rules accesses).
      apply classify_denied_inv. exact H.
  Qed.

  Lemma allowed1_monotone :
    forall allows allows' rules a,
      (forall x, allows x = true -> allows' x = true) ->
      allowed1 allows rules a = true -> allowed1 allows' rules a = true.
  Proof.
    intros allows allows' rules a Hmono H. unfold allowed1 in *.
    destruct (first_match rules a) as [r |] eqn:E.
    - rewrite (explain_first_match_some allows rules a r E) in H.
      rewrite (explain_first_match_some allows' rules a r E). exact H.
    - rewrite (explain_first_match_none allows rules a E) in H.
      rewrite (explain_first_match_none allows' rules a E).
      destruct (allows a) eqn:Ea.
      + rewrite (Hmono a Ea). reflexivity.
      + discriminate H.
  Qed.

  Lemma review1_antitone :
    forall allows allows' rules a,
      (forall x, allows x = true -> allows' x = true) ->
      review1 allows' rules a = true -> review1 allows rules a = true.
  Proof.
    intros allows allows' rules a Hmono H. unfold review1 in *.
    destruct (first_match rules a) as [r |] eqn:E.
    - rewrite (explain_first_match_some allows' rules a r E) in H.
      rewrite (explain_first_match_some allows rules a r E). exact H.
    - rewrite (explain_first_match_none allows' rules a E) in H.
      rewrite (explain_first_match_none allows rules a E).
      destruct (allows a) eqn:Ea.
      + rewrite (Hmono a Ea) in H. discriminate H.
      + reflexivity.
  Qed.

  Lemma lany_review_false_mono :
    forall allows allows' rules accesses,
      (forall x, allows x = true -> allows' x = true) ->
      lany (review1 allows rules) accesses = false ->
      lany (review1 allows' rules) accesses = false.
  Proof.
    intros allows allows' rules accesses Hmono.
    induction accesses as [| a rest IH]; simpl; [reflexivity |].
    intro H. destruct (orb_false_split _ _ H) as [Ha Hrest].
    destruct (review1 allows' rules a) eqn:Ea'.
    - rewrite (review1_antitone allows allows' rules a Hmono Ea') in Ha.
      discriminate Ha.
    - simpl. rewrite (IH Hrest). reflexivity.
  Qed.

  (** A wider grant set never denies what a narrower one allowed: an allowed
      request stays allowed as grants grow. *)
  Theorem grant_allowed_monotone :
    forall allows allows' rules accesses,
      (forall x, allows x = true -> allows' x = true) ->
      classify allows rules accesses = Allowed ->
      classify allows' rules accesses = Allowed.
  Proof.
    intros allows allows' rules accesses Hmono H.
    apply (classify_allowed_iff allows rules accesses) in H.
    destruct H as [Hd Hr].
    apply (classify_allowed_iff allows' rules accesses). split.
    - apply (denied_of_nil_iff allows' rules accesses).
      rewrite <- (denied_of_grant_independent allows allows' rules accesses).
      apply (denied_of_nil_iff allows rules accesses). exact Hd.
    - exact (lany_review_false_mono allows allows' rules accesses Hmono Hr).
  Qed.

  (** {2 The empty policy} *)

  Lemma denied_of_nil_rules :
    forall allows accesses, denied_of allows nil accesses = nil.
  Proof.
    intros allows accesses. induction accesses as [| a rest IH]; simpl;
      [reflexivity |].
    destruct (allows a); exact IH.
  Qed.

  Theorem empty_policy_never_denies :
    forall allows accesses a r rest,
      classify allows nil accesses <> Denied (a, r) rest.
  Proof.
    intros allows accesses a r rest H. apply classify_denied_inv in H.
    rewrite denied_of_nil_rules in H. discriminate H.
  Qed.

  Lemma review1_nil_rules :
    forall allows a, review1 allows nil a = negb (allows a).
  Proof.
    intros allows a. unfold review1. simpl. destruct (allows a); reflexivity.
  Qed.

  Lemma lany_review1_nil_rules :
    forall allows accesses,
      lany (review1 allows nil) accesses = negb (lall allows accesses).
  Proof.
    intros allows accesses. induction accesses as [| a rest IH]; simpl;
      [reflexivity |].
    rewrite review1_nil_rules. rewrite IH. destruct (allows a); reflexivity.
  Qed.

  Theorem empty_policy_allowed_iff :
    forall allows accesses,
      classify allows nil accesses = Allowed <-> lall allows accesses = true.
  Proof.
    intros allows accesses. split.
    - intro H.
      apply (proj1 (classify_allowed_iff allows nil accesses)) in H.
      destruct H as [_ Hr]. rewrite lany_review1_nil_rules in Hr.
      destruct (lall allows accesses); [reflexivity | discriminate Hr].
    - intro H.
      apply (proj2 (classify_allowed_iff allows nil accesses)). split.
      + apply (denied_of_nil_iff allows nil accesses).
        exact (denied_of_nil_rules allows accesses).
      + rewrite lany_review1_nil_rules. rewrite H. reflexivity.
  Qed.

  (** {2 Fail-closed vocabulary: a rule that matches everything} *)

  (* The OCaml [Any] matcher accepts every access; [deny_all], [always_review],
     and [allow_all_dangerously] are that matcher paired with [Deny], [Review],
     and [Allow]. Instantiated at [Any], these theorems are the exact claims the
     [policy.mli] doc makes for those constructors. *)

  Fixpoint tag (r0 : rule) (accesses : list access) : list (access * rule) :=
    match accesses with
    | nil => nil
    | a :: rest => (a, r0) :: tag r0 rest
    end.

  Fixpoint tag_review (r0 : rule) (accesses : list access) :
      list (access * reason) :=
    match accesses with
    | nil => nil
    | a :: rest => (a, By_rule r0) :: tag_review r0 rest
    end.

  Section All_matching.

    Variable r0 : rule.
    Variable rest : list rule.
    Hypothesis Hmatch : forall a, matches (rule_matcher r0) a = true.

    Lemma explain_head :
      forall allows a, explain allows (r0 :: rest) a = explanation_of_rule r0.
    Proof.
      intros allows a. simpl. rewrite (Hmatch a). reflexivity.
    Qed.

    (** [deny_all]: a leading all-matching deny denies every access of a
        non-empty request, in request order, each by that rule. *)
    Lemma denied_of_head_deny :
      forall allows accesses,
        rule_action r0 = Deny ->
        denied_of allows (r0 :: rest) accesses = tag r0 accesses.
    Proof.
      intros allows accesses Ha.
      induction accesses as [| a more IH]; [reflexivity |].
      rewrite denied_of_cons. rewrite (explain_head allows a).
      rewrite (explanation_of_rule_deny r0 Ha). rewrite IH. reflexivity.
    Qed.

    Theorem all_matching_deny :
      forall allows a0 more,
        rule_action r0 = Deny ->
        classify allows (r0 :: rest) (a0 :: more) =
        Denied (a0, r0) (tag r0 more).
    Proof.
      intros allows a0 more Ha. apply classify_denied.
      rewrite (denied_of_head_deny allows (a0 :: more) Ha). reflexivity.
    Qed.

    (** [allow_all_dangerously]: a leading all-matching allow allows every
        request, whatever the grants. *)
    Lemma denied_of_head_allow :
      forall allows accesses,
        rule_action r0 = Allow ->
        denied_of allows (r0 :: rest) accesses = nil.
    Proof.
      intros allows accesses Ha.
      induction accesses as [| a more IH]; [reflexivity |].
      rewrite denied_of_cons. rewrite (explain_head allows a).
      rewrite (explanation_of_rule_allow r0 Ha). exact IH.
    Qed.

    Lemma reviews_of_head_allow :
      forall allows accesses,
        rule_action r0 = Allow ->
        reviews_of allows (r0 :: rest) accesses = nil.
    Proof.
      intros allows accesses Ha.
      induction accesses as [| a more IH]; [reflexivity |].
      rewrite reviews_of_cons. rewrite (explain_head allows a).
      rewrite (explanation_of_rule_allow r0 Ha). exact IH.
    Qed.

    Theorem all_matching_allow :
      forall allows accesses,
        rule_action r0 = Allow ->
        classify allows (r0 :: rest) accesses = Allowed.
    Proof.
      intros allows accesses Ha. rewrite classify_eq.
      rewrite (denied_of_head_allow allows accesses Ha).
      rewrite (reviews_of_head_allow allows accesses Ha). reflexivity.
    Qed.

    (** [always_review]: a leading all-matching review reviews every access of
        a non-empty request, each by that rule — regardless of grants, so
        grants are defeated. *)
    Lemma denied_of_head_review :
      forall allows accesses,
        rule_action r0 = Review ->
        denied_of allows (r0 :: rest) accesses = nil.
    Proof.
      intros allows accesses Ha.
      induction accesses as [| a more IH]; [reflexivity |].
      rewrite denied_of_cons. rewrite (explain_head allows a).
      rewrite (explanation_of_rule_review r0 Ha). exact IH.
    Qed.

    Lemma reviews_of_head_review :
      forall allows accesses,
        rule_action r0 = Review ->
        reviews_of allows (r0 :: rest) accesses = tag_review r0 accesses.
    Proof.
      intros allows accesses Ha.
      induction accesses as [| a more IH]; [reflexivity |].
      rewrite reviews_of_cons. rewrite (explain_head allows a).
      rewrite (explanation_of_rule_review r0 Ha). rewrite IH. reflexivity.
    Qed.

    Theorem all_matching_review :
      forall allows a0 more,
        rule_action r0 = Review ->
        classify allows (r0 :: rest) (a0 :: more) =
        Reviewed (tag_review r0 (a0 :: more)).
    Proof.
      intros allows a0 more Ha. rewrite classify_eq.
      rewrite (denied_of_head_review allows (a0 :: more) Ha).
      rewrite (reviews_of_head_review allows (a0 :: more) Ha). reflexivity.
    Qed.

    Theorem all_matching_review_defeats_grants :
      forall allows allows' a0 more,
        rule_action r0 = Review ->
        classify allows (r0 :: rest) (a0 :: more) =
        classify allows' (r0 :: rest) (a0 :: more).
    Proof.
      intros allows allows' a0 more Ha.
      rewrite (all_matching_review allows a0 more Ha).
      rewrite (all_matching_review allows' a0 more Ha). reflexivity.
    Qed.

  End All_matching.

End Decision.
