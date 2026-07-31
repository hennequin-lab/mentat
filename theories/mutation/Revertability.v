(** Verified model of the revert-safety classification kernel.

    This is the formal counterpart of the law [revertability.mli] states in
    prose, scoped to the classification branch: given the reason list a
    selection accrues, the verdict is a worst-case fold over it. An empty
    reason set is [Available]; a set that holds a supersession is [Unavailable]
    — supersession is the sole proof of unavailability; any other non-empty set
    is [Incomplete], so [Unavailable] outranks [Incomplete]. The outer
    empty-selection guard above this branch in [State.revertability] (a
    selection resolving nothing answers [Available] regardless) is a separate
    concern, preserved by the flip but not part of this model.

    The model mirrors the classification branch of [State.revertability]
    (state.ml): over the flat reason list that branch builds, it tests the
    supersession sublist for emptiness and carries the whole list into the
    verdict. The reason type stays abstract: the classification inspects a
    reason only through whether it is a supersession, so the kernel takes that
    one predicate — [is_superseded] — exactly as the confinement kernel takes
    only a segment's equality. The reasons' payloads — paths, change ids, claim
    ids — never enter the verdict. *)

From Corelib Require Import Init.Prelude.

Open Scope list_scope.

Section Revertability.

  Variable reason : Type.
  Variable is_superseded : reason -> bool.

  (** The revert-safety answer. Mirrors [Revertability.t]: no degradation
      ([Available]), a proven-unavailable answer, or an evidence-incomplete
      answer. Both degraded arms carry the reason list unchanged. The list order
      is emission order — display-only — and the verdict {e arm} does not depend
      on it (see [classify_tag_perm]). *)
  Inductive verdict : Type :=
  | Available
  | Unavailable (reasons : list reason)
  | Incomplete (reasons : list reason).

  (** Whether any reason is a supersession — [State.revertability]'s
      [superseded] list being non-empty, read off the flat reason list. This is
      an [orb]-fold: the commutative, associative scan that makes the
      classification a worst-case fold indifferent to reason order. *)
  Fixpoint any_superseded (l : list reason) : bool :=
    match l with
    | nil => false
    | r :: rest => orb (is_superseded r) (any_superseded rest)
    end.

  (** The worst-case classification, extracted and run in place of the OCaml
      branch. Over the flat reason list: an empty set is [Available]; a set with
      a supersession is [Unavailable]; any other non-empty set is [Incomplete]. *)
  Definition classify (reasons : list reason) : verdict :=
    match reasons with
    | nil => Available
    | _ :: _ =>
        if any_superseded reasons then Unavailable reasons else Incomplete reasons
    end.

  (** The OCaml classification branch verbatim, for the faithfulness lemma
      ([classify_split_eq_classify] in the laws). [State.revertability] tests
      the separately-built [superseded] list for emptiness and carries the whole
      [reasons] list; the flipped kernel runs [classify] instead, and the lemma
      shows the two agree on every list the OCaml construction can present. *)
  Definition classify_split (superseded reasons : list reason) : verdict :=
    match reasons, superseded with
    | nil, _ => Available
    | _ :: _, _ :: _ => Unavailable reasons
    | _ :: _, nil => Incomplete reasons
    end.

  (** Every reason in the list is a supersession — the shape of the [superseded]
      sublist [State.revertability] concatenates onto the front of [reasons]. *)
  Fixpoint all_superseded (l : list reason) : bool :=
    match l with
    | nil => true
    | r :: rest => andb (is_superseded r) (all_superseded rest)
    end.

End Revertability.
