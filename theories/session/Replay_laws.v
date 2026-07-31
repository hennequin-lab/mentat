(** The replay-fold law, machine-checked against the model.

    One theorem carries the weight: the incremental poll and the full recompute
    agree for every event sequence, whatever the step, state, and reject reason.
    Two append laws feed it and, on their own, justify the two OCaml call sites:
    [full_fold_snoc] licenses [Mentat_session.append] (one [State.apply] per
    appended event), and [full_fold_app] licenses [Mentat_session.append_all]
    (one [State.apply_all] over an appended batch), each equal to a full
    recompute over the extended log.

    Everything quantifies over all event lists under no hypothesis on [S], [E],
    or [A]: the fold never inspects the state, so the theorem is immune to how
    many fields the session projection carries. Nothing is admitted. *)

From Corelib Require Import Init.Prelude.

Open Scope list_scope.
From MentatSession Require Import Replay.

Section Laws.

  Variable S : Type.
  Variable E : Type.
  Variable A : Type.
  Variable f : A -> S -> result S E.

  (** [bind] is left-identity on a success carried by the running poll: binding
      the identity step changes nothing. This is the empty-suffix base case. *)
  Lemma bind_ok_id : forall r : result S E, bind r (fun s => Ok s) = r.
  Proof. intro r. destruct r as [s | e]; reflexivity. Qed.

  (** [bind] respects a step that agrees pointwise — the tool for swapping one
      normalized step for another under a fixed prefix. *)
  Lemma bind_ext :
    forall (r : result S E) (g h : S -> result S E),
      (forall s, g s = h s) -> bind r g = bind r h.
  Proof. intros r g h H. destruct r as [s | e]; simpl; [apply H | reflexivity]. Qed.

  (** A one-event recompute is one step. *)
  Lemma full_fold_single : forall e s, full_fold f s (e :: nil) = f e s.
  Proof. intros e s. simpl. destruct (f e s) as [s' | c]; reflexivity. Qed.

  (** {1 The poll is the recompute} *)

  (* Threading the running poll from an arbitrary carried result equals binding
     that result into a single full recompute of the remaining events. The
     accumulator is generalized so the induction can push one event under it. *)
  Lemma incremental_bind :
    forall (es : list A) (r : result S E),
      incremental f r es = bind r (fun s => full_fold f s es).
  Proof.
    induction es as [| e rest IH]; intro r; simpl.
    - symmetry. apply bind_ok_id.
    - rewrite IH. destruct r as [s0 | e0]; simpl.
      + destruct (f e s0) as [s' | c]; reflexivity.
      + reflexivity.
  Qed.

  (** The refactoring-correctness theorem. Starting from any initial state, the
      incremental poll — one step per appended event, the O(1)-per-append path
      [Mentat_session.append] maintains — equals the full recompute over the
      whole log — [State.of_events] / [State.apply_all]. Two structurally
      different recursions; one value, for every event sequence. *)
  Theorem incremental_eq_full :
    forall (s : S) (es : list A), incremental f (Ok s) es = full_fold f s es.
  Proof. intros s es. rewrite incremental_bind. reflexivity. Qed.

  (** {1 The append laws} *)

  (** Recomputing an appended batch equals recomputing the prefix, then
      recomputing the batch from that state. This is the law
      [Mentat_session.append_all] leans on: fold the new events onto the cached
      state rather than re-folding the whole log. *)
  Lemma full_fold_app :
    forall (es fs : list A) (s : S),
      full_fold f s (es ++ fs)
      = bind (full_fold f s es) (fun s' => full_fold f s' fs).
  Proof.
    induction es as [| e rest IH]; intros fs s; simpl.
    - reflexivity.
    - destruct (f e s) as [s' | c]; simpl; [apply IH | reflexivity].
  Qed.

  (** Recomputing one appended event equals one step on the prefix's recompute.
      This is the law [Mentat_session.append] leans on: [State.apply event
      t.state] rather than re-folding the whole log. *)
  Lemma full_fold_snoc :
    forall (es : list A) (e : A) (s : S),
      full_fold f s (es ++ e :: nil)
      = bind (full_fold f s es) (fun s' => f e s').
  Proof.
    intros es e s. rewrite full_fold_app. apply bind_ext.
    intro s'. apply full_fold_single.
  Qed.

End Laws.
