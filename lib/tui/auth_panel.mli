(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Provider authentication and credential-lifecycle panel.

    This is a pure frontend surface over the account cone. Launch-fixed provider
    declarations supply names and declared authentication methods; the client's
    {!Mentat_client.account_readiness} response supplies live readiness. The
    panel keeps those owner values and joins them by
    {!Mentat_llm.Provider.equal}. It does not read an environment, inspect a
    credential store, discover a catalog, or construct account/source
    projections of its own.

    The shell owns effects. It routes keys through {!key} and {!update},
    executes the returned {!event}, and folds client results back with
    {!login_step}, {!login_failed}, {!save_api_key_finished}, or
    {!logout_finished}. Browser opening and clipboard writes are explicitly
    frontend-local events. *)

(** {1:state State} *)

(** The operation whose provider picker is being shown. The namespace keeps
    command-mode constructors distinct from the similarly named shell events. *)
module Mode : sig
  type t =
    | Login
        (** Select a declared login method or enter an API key. Interactive
            methods use [Mentat_client.login]; API keys use
            [Mentat_client.save_api_key] directly. *)
    | Logout  (** Use [Mentat_client.logout]. *)
end

type mode = Mode.t
(** The account-operation mode. *)

type attempt
(** A shell-minted correlation token for one account operation.

    The token has no protocol meaning and is never displayed. It exists only so
    a late step, completion, or browser-open callback from a cancelled or
    superseded operation cannot mutate the currently visible panel. *)

val attempt : int -> attempt
(** [attempt n] is correlation token [n]. The shell must not reuse a token while
    an older operation carrying it may still deliver callbacks. *)

type t
(** The immutable panel state.

    State contains exact provider declarations and exact discovery values. A
    live interactive login retains the current exact
    {!Mentat_provider.Auth.Login.Progress.t} and, when useful, its most recent
    actionable challenge. A successful settlement retains the exact,
    credential-free {!Mentat_provider.Account.t}; terminal and abandoned flows
    discard progress. The API-key editor is deliberately opaque: its contents
    have no accessor or formatter and are rendered only as masked cells. *)

type msg
(** A panel input produced by {!key} or by an interactive Mosaic control in
    {!view}. *)

(** Effects and navigation outcomes interpreted by the shell. Login, logout, and
    API-key effects correspond directly to current client calls; the remaining
    constructors are frontend-local effects. *)
type event =
  | Stay  (** Keep the panel open. *)
  | Close  (** Close without starting another operation. *)
  | Reload_readiness  (** Re-run {!Mentat_client.account_readiness}. *)
  | Save_api_key of { provider : Mentat_llm.Provider.t; key : string }
      (** Call [Mentat_client.save_api_key ~provider ~key]. The key exists only
          in this one-shot payload; the returned panel state has already
          discarded its masked editor. *)
  | Begin_login of {
      provider : Mentat_llm.Provider.t;
      method_ : Mentat_provider.Auth.Login.Id.t;
    }
      (** Call [Mentat_client.login ~provider ~method_], then pull its
          {!Mentat_client.Login.next} steps. API-key and external declarations
          never produce this event. *)
  | Logout of { provider : Mentat_llm.Provider.t }
      (** Call [Mentat_client.logout ~provider]. *)
  | Cancel_login of attempt
      (** Call {!Mentat_client.Login.cancel} for the correlated interactive
          login. Cancellation immediately returns the UI to its preceding picker
          and invalidates later callbacks from that attempt. *)
  | Copy of string
      (** Copy display-safe external instructions, a serialized URI, or the
          exact user code the client yielded. API-key input never produces
          [Copy]. *)
  | Open_url of { attempt : attempt; url : Uri.t }
      (** Open [url] through the frontend's local browser capability. An exact
          [Login.Progress (Progress.Browser_url uri)] step emits the same typed
          URI automatically; Enter can emit it again for retry. The shell
          serializes it only at its presentation boundary. This is never a
          client call. *)
  | Flash of string
      (** Show a non-secret, inert explanation for an unavailable selection. *)

val loading :
  mode:mode ->
  declarations:Mentat_provider.t list ->
  ?provider:Mentat_llm.Provider.t ->
  unit ->
  t
(** [loading ~mode ~declarations ?provider ()] is a newly opened panel waiting
    for account readiness.

    [declarations] is Startup metadata, not a live catalog projection. Duplicate
    provider identities are ignored after the first declaration. [provider]
    implements a typed [/login PROVIDER] or lifecycle fast path without parsing
    an untrusted provider string inside the panel. *)

val readiness_loaded :
  (Mentat_provider.Account.Discovery.t list, Mentat_protocol.Error.t) result ->
  t ->
  t * event
(** [readiness_loaded result t] folds the account-readiness query that [t] is
    currently waiting for. Structured errors are retained and rendered through
    {!Mentat_protocol.Error.diagnostic}; their messages are never parsed.

    A successful response opens the provider picker or follows [provider]'s fast
    path. [Discovery.Known account] rows derive their phase from [account];
    [Discovery.Resolution_failed] remains a provider-local diagnostic. Missing
    rows remain explicitly unavailable; they are not invented as
    [Account.Phase.Missing]. Calls made after the panel has left its
    loading/error state are ignored, so a stale query response cannot reset an
    active editor or login flow. *)

(** {1:input Input} *)

val key : Matrix.Input.Key.event -> msg option
(** [key event] classifies [event] with {!Panel.classify}. Unrecognized modal
    keys return [None]. *)

val update : msg -> t -> t * event
(** [update message t] folds one keyboard action.

    Provider and method pickers support type-to-filter, grapheme-safe backspace,
    wrapping arrows, Enter/Tab confirmation, and digit jump-pick while their
    filter is empty. Escape walks back one drill-down level and closes from the
    provider picker.

    The API-key editor accepts printable input and digits, removes one extended
    grapheme on Backspace, submits on Enter, and discards its contents on either
    submit or Escape. In a live flow, [c] copies the primary value, [u] copies a
    device verification URL, Enter opens/reopens a visible URL, and Escape emits
    [Cancel_login] once an attempt has been stamped. External instructions can
    be copied but never report a completed client login.

    The rendered provider and method {!Mosaic.select} controls own their
    pointer, activation, and viewport behavior and feed their selection back as
    [msg] values. Rendered {!Mosaic.scroll_box} controls own scrolling for long
    instructions and diagnostics; scroll position is presentation state and is
    not mirrored into [t]. *)

val accepts_paste : t -> bool
(** [accepts_paste t] is [true] only while the masked API-key editor is active.
*)

val paste : string -> t -> t
(** [paste text t] appends [text] to the masked API-key editor, omitting CR/LF
    transport line endings, and is a no-op in every other state. The returned
    state exposes no copy or debug representation of the input. *)

(** {1:correlation Correlated client folds} *)

val started : attempt:attempt -> t -> t
(** [started ~attempt t] stamps the account operation that {!update} just
    emitted. It changes only a not-yet-stamped starting state. Re-stamping a
    live operation is ignored. The shell calls this synchronously before
    starting a worker fiber. *)

val active_attempt : t -> attempt option
(** [active_attempt t] is the correlation token of the visible account
    operation, if it has been stamped and has not settled or been cancelled. *)

val cancel_active : t -> (t * event) option
(** [cancel_active t] performs the same transition as Escape for a stamped
    interactive login. It is [None] for direct credential operations and all
    non-flow states, allowing the app's outer abort chord to keep its usual
    meaning there. *)

val login_step : attempt:attempt -> Mentat_client.Login.step -> t -> t * event
(** [login_step ~attempt step t] folds one exact client login [step] when
    [attempt] identifies the visible interactive login; otherwise it is
    [t, Stay].

    [Progress (Browser_url url)] retains the exact progress and returns
    [Open_url] immediately, so browser login auto-opens while preserving a
    visible retry/copy surface. [Progress (Device_challenge challenge)] keeps
    the exact verification URI, user code, and expiry visible: Enter opens the
    URI, [c] copies the code, and [u] copies the URI.
    [Progress (Listening {redirect_uri})] displays the exact callback URI while
    retaining any prior actionable challenge.

    [Saved account] settles visibly, retains the exact credential-free account,
    derives only its readiness phase for display, and invalidates the attempt.
    [Cancelled] settles visibly and also invalidates the attempt. Start and pull
    failures enter through {!login_failed}, not through a synthetic login step.
    Progress is transient UI state rather than a durable login transcript. *)

val login_failed : attempt:attempt -> Mentat_protocol.Error.t -> t -> t
(** [login_failed ~attempt error t] folds a structured failure from starting or
    pulling an interactive login. The exact error value is retained and is never
    classified by parsing its rendered diagnostic. A mismatched attempt is
    ignored. *)

val save_api_key_finished :
  attempt:attempt ->
  (Mentat_provider.Account.t, Mentat_protocol.Error.t) result ->
  t ->
  t
(** [save_api_key_finished ~attempt result t] settles a correlated API-key save.
    A successful result retains and presents the exact account value. Structured
    errors remain exact owner values. A completion for a different operation, or
    a late or superseded completion, is ignored. *)

val logout_finished :
  attempt:attempt ->
  (Mentat_provider.Account.Logout.t, Mentat_protocol.Error.t) result ->
  t ->
  t
(** [logout_finished ~attempt result t] settles a correlated logout. A
    successful result presents the exact current discovery and optional remote
    and conditional-local revocation outcomes. Structured errors remain exact
    owner values. A completion for a different operation, or a late or
    superseded completion, is ignored. *)

val url_opened : attempt:attempt -> t -> t
(** [url_opened ~attempt t] marks the visible browser/device URL as opened when
    the attempt matches. *)

val url_open_failed : attempt:attempt -> message:string -> t -> t
(** [url_open_failed ~attempt ~message t] records a frontend-local browser
    launch failure for the matching flow. [message] is normalized to inert,
    one-line display text; it is not a protocol error and is never parsed. *)

(** {1:view View} *)

val view :
  palette:Theme.Palette.t -> frame:Mosaic.Ansi.Color.t -> t -> msg Mosaic.t
(** [view ~frame t] renders the complete authentication panel.

    Views use shared {!Panel.view} chrome. Provider rows show only declared
    metadata and the exact discovery result; known accounts derive a phase and
    resolution failures remain provider-local diagnostics. Environment variable
    names are described as declarations, never as evidence that the variable is
    set. Login flows visibly distinguish browser URLs, callback listening with
    its redirect URI, device challenges with code/URI/expiry, saved-account
    phase, structured failure, cancellation, save, and logout outcomes. External
    instructions are ANSI-stripped, control-safe, and placed in a scrollable,
    selectable text surface. Device-flow hints name code copy and URL copy
    separately.

    Mosaic owns wrapping, truncation, selection scrolling, and flex allocation;
    the panel does not measure terminal columns, predict visible rows, or keep a
    parallel presentation viewport. The API-key editor uses a non-editing
    {!Mosaic.input} containing bullets only; secret editing remains in [t].
    Shared panel chrome and its parent layout own the viewport constraints. *)
