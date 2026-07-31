(** Verified model of lpath's absolute-path canonicalization and containment.

    lpath is the trusted computing base beneath the confinement boundary:
    theories/sandbox proves confinement over an abstract [withinb] (segment
    prefix over [list seg]), and lib/sandbox/policy.ml realises that model with
    [Lpath.Abs.is_within], [Abs.components], and [Abs.of_string_exn]. If lpath's
    canonicalization or containment were wrong, those proofs would not transfer
    to real paths. This model mirrors [Abs.of_string]'s lexical normalization
    and [Abs.is_within]'s string-level containment operation for operation, so
    the laws proved in Lpath_laws.v — normalization strips every [.] and [..],
    and [is_within] computes exactly the segment prefix the sandbox assumes —
    close that gap. The model's faithfulness to the real implementation is
    pinned by the differential test in otherlibs/lpath/test.

    A byte is abstract with decidable equality and named constants for the only
    bytes the algorithm inspects: [slash] separates components, [dot] builds the
    [.] and [..] segments, and [backslash], [nul], [colon] together with
    [is_letter] decide the component grammar (a leading ASCII-letter drive
    prefix such as ["C:"]). The extraction instantiates byte with OCaml's
    [char]; the differential test supplies the concrete constants. Keeping the
    alphabet abstract makes the algorithm's genuine dependence — only [/] and
    [.] structure a path; the rest gate which raw strings are legal — visible in
    the type. *)

From Corelib Require Import Init.Prelude.

Open Scope list_scope.

Section Lpath.

  Variable byte : Type.
  Variable byte_eqb : byte -> byte -> bool.
  Variable slash dot backslash nul colon : byte.
  Variable is_letter : byte -> bool.

  (** {1 Bytes, strings, and segments}

      A path string is a [list byte]; a component (segment) is itself a
      [list byte]; a decomposed path is a [list (list byte)]. *)

  (* Byte-string equality, [String.equal] on the OCaml side. Identical in shape
     to the sandbox model's [path_eqb]. *)
  Fixpoint eqb_chars (a b : list byte) : bool :=
    match a, b with
    | nil, nil => true
    | nil, _ :: _ => false
    | _ :: _, nil => false
    | x :: a', y :: b' => andb (byte_eqb x y) (eqb_chars a' b')
    end.

  Fixpoint forallb_byte (f : byte -> bool) (l : list byte) : bool :=
    match l with
    | nil => true
    | b :: l' => andb (f b) (forallb_byte f l')
    end.

  (** {1 The component grammar}

      Mirrors [malformed_component] in lpath.ml exactly: a component is
      malformed iff it is empty, [.], [..], carries a drive prefix, or contains
      a byte outside the component alphabet. [.] and [..] are handled by the
      fold before this check, but the check still excludes them so the grammar
      itself is complete. *)

  Definition is_dot (s : list byte) : bool := eqb_chars s (cons dot nil).

  Definition is_dotdot (s : list byte) : bool :=
    eqb_chars s (cons dot (cons dot nil)).

  (* [is_component_char] in lpath.ml: NUL, [/], and backslash cannot occur in a
     component; every other byte can. *)
  Definition is_component_char (b : byte) : bool :=
    andb (negb (byte_eqb b nul))
      (andb (negb (byte_eqb b slash)) (negb (byte_eqb b backslash))).

  (* [has_windows_drive_prefix]: a leading ASCII letter immediately followed by
     [:], such as ["C:"]. *)
  Definition has_drive_prefix (s : list byte) : bool :=
    match s with
    | c :: d :: _ => andb (is_letter c) (byte_eqb d colon)
    | _ => false
    end.

  Definition is_nil (s : list byte) : bool :=
    match s with nil => true | _ :: _ => false end.

  Definition malformed (s : list byte) : bool :=
    orb (is_nil s)
      (orb (is_dot s)
         (orb (is_dotdot s)
            (orb (has_drive_prefix s)
               (negb (forallb_byte is_component_char s))))).

  Definition valid_seg (s : list byte) : bool := negb (malformed s).

  Fixpoint all_valid (segs : list (list byte)) : bool :=
    match segs with
    | nil => true
    | s :: rest => andb (valid_seg s) (all_valid rest)
    end.

  (** {1 Splitting and joining}

      [split_slash] is [String.split_all ~sep:"/" ~drop:String.is_empty]:
      maximal slash-free runs, empty runs dropped, so it collapses [//] and
      strips leading and trailing separators. [join] is [String.concat "/"].
      [canon] renders a validated component list to the canonical absolute
      string, matching the [Abs] kind's [build]: the leading slash then the
      join. *)

  Definition flush (cur : list byte) : list (list byte) :=
    match cur with nil => nil | _ :: _ => cur :: nil end.

  Fixpoint split_acc (cur : list byte) (bs : list byte) : list (list byte) :=
    match bs with
    | nil => flush cur
    | b :: rest =>
        if byte_eqb b slash then app (flush cur) (split_acc nil rest)
        else split_acc (app cur (cons b nil)) rest
    end.

  Definition split_slash (bs : list byte) : list (list byte) := split_acc nil bs.

  Fixpoint join (segs : list (list byte)) : list byte :=
    match segs with
    | nil => nil
    | s :: nil => s
    | s :: rest => app s (cons slash (join rest))
    end.

  Definition canon (segs : list (list byte)) : list byte := cons slash (join segs).

  (** {1 Normalization}

      [resolve_onto] is lpath's fold from the root: [.] is dropped, [..] pops
      the deepest surviving component and clamps at the root (the [Abs]
      underflow law, [/..] = [/]), a malformed survivor is rejected, and any
      other component is pushed. The stack is reversed, as in lpath, so [..] is
      an O(1) head pop; [build_abs] reverses it back and renders. *)

  Inductive perror : Type :=
  | Empty
  | Relative
  | Malformed (c : list byte).

  Inductive presult : Type :=
  | POk (t : list byte)
  | PErr (e : perror).

  Fixpoint rev_stack (l acc : list (list byte)) : list (list byte) :=
    match l with
    | nil => acc
    | x :: l' => rev_stack l' (cons x acc)
    end.

  Definition build_abs (stack : list (list byte)) : list byte :=
    canon (rev_stack stack nil).

  Fixpoint resolve_onto (stack : list (list byte)) (segs : list (list byte)) :
      presult :=
    match segs with
    | nil => POk (build_abs stack)
    | s :: rest =>
        if is_dot s then resolve_onto stack rest
        else if is_dotdot s then
          match stack with
          | _ :: stk => resolve_onto stk rest
          | nil => resolve_onto nil rest
          end
        else if malformed s then PErr (Malformed s)
        else resolve_onto (cons s stack) rest
    end.

  Definition starts_with_slash (s : list byte) : bool :=
    match s with nil => false | b :: _ => byte_eqb b slash end.

  (** [Abs.of_string]: empty is rejected, a non-slash-rooted string is
      [Relative], otherwise the split is normalized from the root. *)
  Definition abs_of_string (s : list byte) : presult :=
    if is_nil s then PErr Empty
    else if starts_with_slash s then resolve_onto nil (split_slash s)
    else PErr Relative.

  (** {1 Rendering and decomposition} *)

  Definition to_string (t : list byte) : list byte := t.

  Definition is_root (t : list byte) : bool := eqb_chars t (cons slash nil).

  (** [Abs.components]: the root has none; every other path splits on [/]. *)
  Definition abs_components (t : list byte) : list (list byte) :=
    if is_root t then nil else split_slash t.

  (** {1 Containment}

      [below_someb] is [Option.is_some] of lpath's [below_by_component]: the
      component-boundary guard [t.[n] = '/'] together with the string-prefix
      test, which is what stops ["/src-lib"] from lying below ["/src"].
      [abs_within] is [Option.is_some (Abs.relativize ~root p)] in the same
      order lpath tests: equality first, then the root special-case, then the
      boundary-guarded prefix. *)

  Fixpoint prefixb (p q : list byte) : bool :=
    match p with
    | nil => true
    | a :: p' =>
        match q with
        | nil => false
        | b :: q' => andb (byte_eqb a b) (prefixb p' q')
        end
    end.

  Fixpoint nth_err (l : list byte) (n : nat) : option byte :=
    match l, n with
    | nil, _ => None
    | b :: _, O => Some b
    | _ :: l', S n' => nth_err l' n'
    end.

  Definition below_someb (prefix t : list byte) : bool :=
    match nth_err t (length prefix) with
    | Some b => andb (byte_eqb b slash) (prefixb prefix t)
    | None => false
    end.

  Definition abs_within (root t : list byte) : bool :=
    if eqb_chars t root then true
    else if is_root root then true
    else below_someb root t.

  Definition abs_strictly_within (root t : list byte) : bool :=
    andb (abs_within root t) (negb (eqb_chars root t)).

  (** The sandbox's [withinb] instantiated at [seg := list byte],
      [seg_eqb := eqb_chars]: inclusive segment prefix. The bridge lemma proves
      [abs_within] on canonical paths computes exactly this over their
      components. *)
  Fixpoint seg_prefixb (rs ps : list (list byte)) : bool :=
    match rs with
    | nil => true
    | r :: rs' =>
        match ps with
        | nil => false
        | p :: ps' => andb (eqb_chars r p) (seg_prefixb rs' ps')
        end
    end.

  (* Propositional membership, for the no-traversal statement; the model stays
     boolean. Shaped like the sandbox model's [InE]. *)
  Fixpoint InB (c : list byte) (l : list (list byte)) : Prop :=
    match l with
    | nil => False
    | x :: rest => x = c \/ InB c rest
    end.

End Lpath.
