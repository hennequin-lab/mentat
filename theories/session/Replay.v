(** The replay-fold kernel.

    A session's projected state is a left fold of its durable event log under a
    step that may reject an event. [mentat.session] maintains that projection
    two different ways and requires the two to agree:

    - a {e full recompute} over the whole log — [State.of_events], i.e.
      [State.apply_all events State.empty], the head-recursive loop that threads
      the state and aborts on the first rejected event; and
    - an {e incremental poll} that applies exactly one step per appended event —
      [Mentat_session.append] is [State.apply event t.state], accreting the
      cached state one event at a time as the log grows.

    This model abstracts the step [f], the state [S], the reject reason [E], and
    the event [A], so the agreement is a fact about fold structure alone. It is
    therefore immune to how many fields the session state carries: the theorem
    never inspects [S].

    The step is [f : A -> S -> result S E]; the short-circuit on the first
    rejection is the fold's own, mirroring [State.apply_all]'s loop. The
    diagnostic event index [State.apply_all] tags onto a rejection is a wrapper
    the OCaml boundary keeps {e outside} this fold (it threads the index through
    [S] at the call site), so it does not appear here. *)

From Corelib Require Import Init.Prelude.

Open Scope list_scope.

(** A rejectable result, mirroring OCaml's [('s, 'e) result]. Extraction maps
    it onto the built-in type so the kernel speaks the host's [Ok]/[Error]. *)
Inductive result (S E : Type) : Type :=
  | Ok : S -> result S E
  | Error : E -> result S E.

Arguments Ok {S E} _.
Arguments Error {S E} _.

Section Replay.

  Variable S : Type.   (* the projected state *)
  Variable E : Type.   (* an event's rejection reason *)
  Variable A : Type.   (* a durable event *)

  (* The step: reconstruct one event into the state, or reject it. On the OCaml
     side this is [State.apply]. *)
  Variable f : A -> S -> result S E.

  (** [bind r g] threads a step through a possibly-rejected result: a rejected
      prefix stays rejected, mirroring how a failed [apply] aborts the append
      chain without touching later events. *)
  Definition bind (r : result S E) (g : S -> result S E) : result S E :=
    match r with
    | Ok s => g s
    | Error e => Error e
    end.

  (** The full recompute. Fold the whole event log from [s], threading the
      state and aborting on the first rejected event — the head-recursive
      counterpart of [State.apply_all]'s [loop]. *)
  Fixpoint full_fold (s : S) (es : list A) : result S E :=
    match es with
    | nil => Ok s
    | e :: rest =>
        match f e s with
        | Ok s' => full_fold s' rest
        | Error c => Error c
        end
    end.

  (** The incremental poll. A session accretes one event at a time, each append
      applying exactly one step to the running cached result; threading a
      [result] left-to-right and binding each step is precisely that running
      cache. This is a genuinely different recursion from [full_fold]: it
      threads a [result] and binds, where [full_fold] threads the raw state and
      pattern-matches. Their agreement is the theorem. *)
  Fixpoint incremental (r : result S E) (es : list A) : result S E :=
    match es with
    | nil => r
    | e :: rest => incremental (bind r (fun s => f e s)) rest
    end.

End Replay.

Arguments bind {S E} r g.
Arguments full_fold {S E A} f s es.
Arguments incremental {S E A} f r es.
