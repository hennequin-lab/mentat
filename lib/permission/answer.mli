(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The authority a reviewer selects when resolving a permission request.

    An answer is the pure, durable value a permission {!Request.t} is resolved
    to: allow the blocked operation with a chosen scope, or deny it. It is the
    reusable half of a resolved permission — the part that is neither session
    identity (prompt id, turn, invocation, tool call) nor a model-visible tool
    result. The session's [Decision.Resolved.t] binds this value to a decision
    and records resolution provenance; [Decision.Answer]'s permission arm
    carries the reviewer's optional denial guidance, and [Decision] derives the
    model-visible result produced by a denial.

    An answer carries no prompt id, no turn or invocation, no LLM tool call or
    result, and no resolution provenance (reviewer vs unattended); those are
    binding metadata owned by the resolving session, not part of the reusable
    value. An answer does not itself grant a runtime capability: applying an
    allow to conversation grants ({!Policy.Review.remember}) or installing a
    family's rules into a durable policy is the runtime's interpretation of the
    selected authority. A [Deny] never grants authority and never installs a
    rule. *)

(** {1:types Types} *)

type lifetime =
  | Conversation
  | User
      (** Where an allowed family of rules is remembered. [Conversation] keeps
          the rules for the current durable conversation only; [User]
          additionally persists them as user configuration. Project is never a
          lifetime: project input can never install a rule (fail-closed).
          Lifetime never widens a rule's scope; it only chooses how long the
          already-scoped rules live. *)

(** The authority selected by an allowed permission. Private so the [Family]
    invariant holds by construction. *)
type allowance = private
  | Once
      (** Allow this one blocked operation. Nothing is remembered; an identical
          later access is reviewed again. *)
  | Exact_for_conversation
      (** Allow, and remember the request's exact reviewed {!Access.t} values as
          conversation grants ({!Policy.Grants.t}). Never broadens to access
          class, path prefix, command family, or network host. *)
  | Family of { lifetime : lifetime; rules : Policy.Rule.t list }
      (** Allow, and install [rules] as visible allow rules with [lifetime].

          Invariant: [rules] is non-empty, has no duplicates
          ({!Policy.Rule.equal}), and every rule's action is
          {!Policy.Rule.Allow}. A family answer may widen only by an explicit,
          visible allow rule the reviewer chose — never by a deny or review rule
          laundered as an allow. *)

type t = private
  | Allow of allowance
  | Deny
      (** A resolved permission answer. [Deny] is bare: the reviewer's optional
          feedback and the model-visible tool result it produces are runtime
          concerns (they carry the LLM dependency), not part of the pure answer.
          Private so the [Family] invariant cannot be bypassed. *)

(** {1:constructing Constructing answers} *)

val once : t
(** [once] is [Allow Once]. *)

val exact_for_conversation : t
(** [exact_for_conversation] is [Allow Exact_for_conversation]. *)

val family : lifetime:lifetime -> rules:Policy.Rule.t list -> t
(** [family ~lifetime ~rules] is [Allow (Family { lifetime; rules })]. Raises
    [Invalid_argument] if [rules] is empty, contains duplicate rules
    ({!Policy.Rule.equal}), or contains a rule whose action is not
    {!Policy.Rule.Allow}. The JSON codec reports the same invalid states as
    decode errors. *)

val deny : t
(** [deny] is [Deny]. *)

(** {1:predicates Predicates, formatting, JSON} *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] select the same authority. Family rule
    order participates in equality. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] for diagnostics only; not stable storage syntax. *)

val jsont : t Jsont.t
(** [jsont] maps answers to version-1 JSON objects. The allow scope is a tagged
    enum ([allow-once] / [allow-exact-for-conversation] / [allow-family]); a
    family additionally carries [lifetime] and its [rules]; [deny] carries no
    payload. The [Allow]/[Deny] tag is exclusive. Unknown object members and
    constructor-invalid family states are decoding errors. Prompt id, resolution
    provenance, and the model-visible tool result are added by the session codec
    that embeds this value — not by [jsont]. *)
