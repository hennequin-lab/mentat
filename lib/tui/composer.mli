(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Structured editing state and the two-rule composer surface.

    The composer is the pure boundary between Mosaic's controlled textarea and
    {!Draft}. It owns cursor adaptation, prompt-history walking, input-mode
    presentation, and the small set of events that the application shell must
    route. It performs no history I/O, file enumeration, command dispatch, or
    shell execution.

    Shell input is deliberately capability-gated. The caller fixes that gate at
    {!init}; when disabled, a leading [!] is ordinary prompt text and can be
    edited, pasted, restored, and submitted unchanged. This keeps the
    frontend-local executable capability from leaking into the client/session
    waist. File mentions have the same separation: this module always permits
    free-text [@] editing, while the shell alone decides whether enumeration is
    available and whether to show a completion list.

    The state machine follows the Elm shape used by the application: {!render}
    emits {!type-msg} values through [on_msg], and {!update} returns new state
    plus outward {!type-event}s. *)

(** {1:input_modes Input Modes} *)

module Input_mode : sig
  type t =
    | Plain
        (** Ordinary model input, including leading [!] when shell capability
            was disabled at construction. *)
    | Shell
        (** An executable-local shell command. The triggering [!] is represented
            by the marker and is not part of {!draft_text}. This state is
            reachable only when shell capability was enabled at construction. *)
    | History_search
        (** Reverse-history search is active. It takes visual precedence over
            the underlying plain or shell input mode until
            {!End_history_search}. *)
end

(** {1:state State} *)

type t
(** Composer state.

    The value contains one structured draft, a deduplicated newest-first
    prompt-history walk, the launch-fixed shell-capability gate, history-search
    presentation state, and controlled-widget synchronization bookkeeping. *)

val init : shell_enabled:bool -> ?draft:string -> unit -> t
(** [init ~shell_enabled ?draft ()] is a fresh composer whose visible draft is
    [draft], or empty when omitted, with the cursor at its end.

    When [shell_enabled] is [true], an initial draft beginning with a plain [!]
    enters {!Input_mode.Shell} and consumes that trigger. When it is [false],
    [!] remains ordinary prompt text. History is empty until {!with_history}.
    Malformed UTF-8 in [draft] is replaced according to {!Draft.of_text}. *)

val draft : t -> Draft.t
(** [draft t] is [t]'s structured editable draft. *)

val draft_text : t -> string
(** [draft_text t] is the well-formed UTF-8 visible text of {!draft}. In shell
    mode it excludes the leading [!] intent marker. *)

val is_blank : t -> bool
(** [is_blank t] is [true] iff the expanded draft would submit no non-whitespace
    text according to {!Draft.is_blank}. *)

val history_entry : t -> Draft.History_entry.t
(** [history_entry t] is the structured draft suitable for prompt-history
    persistence. A shell command is prefixed with the plain [!] intent marker so
    restoring it into a shell-enabled composer recovers shell mode. *)

val input_mode : t -> Input_mode.t
(** [input_mode t] is the currently presented input mode. History search takes
    precedence over the underlying prompt or shell mode. *)

val active_file_ref_token : t -> string option
(** [active_file_ref_token t] is the active free-text [@]-token containing the
    cursor, including its [@], or [None]. Token recognition is independent of
    whether the executable supplied file-enumeration capability; callers use
    this observation only when they can actually populate a completion list. *)

val with_history : Draft.History_entry.t list -> t -> t
(** [with_history entries t] appends canonical loaded [entries], supplied newest
    first, after entries already remembered by [t]. Exact duplicate structured
    entries are removed while preserving the first occurrence, and an
    in-progress history walk is reset.

    The caller obtains canonical values from {!History.load}; Composer neither
    decodes records nor handles rejected history lines. *)

(** {1:messages Messages} *)

type msg =
  | Edited of string  (** Mosaic reported the textarea's complete new value. *)
  | Cursor_moved of int
      (** Mosaic reported a zero-based extended-grapheme cursor offset. Values
          outside the draft clamp to its nearest end. The view emits this only
          when Mosaic has no active selection; selection replacement is
          reconciled by the following full-value {!Edited} message. *)
  | Paste of string
      (** A bracketed paste was intercepted before the textarea inserted it.
          {!update} delegates line-ending, UTF-8, and large-paste handling to
          {!Draft.insert_paste}. *)
  | Submit of string
      (** Enter was pressed and Mosaic supplied the textarea's complete value.
      *)
  | Help_key
      (** [?] was intercepted on an empty plain draft. Directly dispatching this
          message in another state is a no-op. *)
  | Complete_file_ref of string
      (** Replace the active [@]-token with an atomic reference to this path,
          then add a separating space when needed. The shell emits this only
          when its optional enumeration capability is present. Folding this
          message raises [Invalid_argument] when the path is empty: an
          enumerator returning an empty path violates its programmer-local
          capability contract. *)
  | Set_file_ref_token of string
      (** Replace the active [@]-token's text with [@] followed by this string,
          keeping it an ordinary free-text token with the cursor at its end. The
          shell emits this when completion drills into a directory, so the token
          itself carries the browse location. Without an active token the draft
          is unchanged. *)
  | Clear_file_ref_token
      (** Remove the active [@]-prefixed token containing the cursor, if any.
          The application emits this when a mention resolves to an image, which
          attaches rather than inserting a text reference. *)
  | Insert_image of Draft.Image_ref.t
      (** Insert an atomic [[Image #N]] element at the cursor carrying an
          attached image's media block. The application emits this after the
          runtime's attach flow returns a media block. *)
  | Restore_history of Draft.History_entry.t
      (** Restore a canonical history entry with its cursor at the end. A plain
          leading [!] recovers shell mode only when shell capability is enabled.
      *)
  | History_previous
      (** Move to the next older prompt, saving the initially edited draft for
          restoration by {!History_next}. *)
  | History_next
      (** Move to the next newer prompt, or restore the draft and input mode
          that were active when the history walk began. *)
  | Begin_history_search
      (** Present reverse-history-search mode without changing the draft or its
          underlying plain/shell classification. *)
  | End_history_search  (** Leave reverse-history-search presentation. *)
  | Clear_to_history
      (** Clear a nonblank draft, remember it as the newest history entry, and
          emit {!Draft_discarded}. A blank draft is unchanged. *)
  | Exit_shell
      (** Leave shell mode only when its command draft is blank. In every other
          state this message is a no-op. *)
  | List_key of [ `Up | `Down | `Tab ]
      (** A navigation key intercepted before the textarea default. The shell
          routes it to the open palette/mention/history list, or to the ordinary
          one-line history walk. It has no transition in {!update}. *)

(** {1:events Events} *)

type event =
  | Submitted of {
      text : string;
      media : Mentat_llm.Content.t list;
      entry : Draft.History_entry.t;
    }
      (** A nonblank model prompt, or a prompt carrying only attached images.
          [text] has expanded paste payloads and no image placeholders; [media]
          is the attached images' content blocks in document order; [entry]
          retains structured placeholders for persistence. *)
  | Shell_submitted of { command : string; entry : Draft.History_entry.t }
      (** A nonblank shell command, reachable only for a shell-enabled composer.
          [command] excludes [!], while [entry] includes it for restoration. *)
  | Blank_submitted
      (** Enter on a semantically blank plain draft. Unicode whitespace is
          normalized back to the empty draft; the application decides whether
          the event means resume or no action. Blank shell input emits no event.
      *)
  | Draft_discarded of Draft.History_entry.t
      (** A discarded nonblank draft to remember in prompt history. *)
  | Help_requested
      (** The shortcuts sheet was requested from an empty plain draft. *)

val update : ?submit_enabled:bool -> msg -> t -> t * event list
(** [update ?submit_enabled msg t] folds [msg] into [t] and returns outward
    events in routing order. [submit_enabled] defaults to [true]. When it is
    [false], {!Submit} changes neither state nor events; this is the application
    shell's second line of defence while a selectable list owns Enter. *)

(** {1:view View} *)

val render :
  ?submit_enabled:bool ->
  ?list_open:bool ->
  ?mode:Mentat_session.Contract.Mode.t ->
  ?agent:string ->
  ?turn_running:bool ->
  ?top_margin:int ->
  ?inspect_latest_change:'msg ->
  palette:Theme.Palette.t ->
  on_msg:(msg -> 'msg) ->
  t ->
  'msg Mosaic.t
(** [render ~on_msg t] renders the controlled textarea between two full-width
    rules. Its input grows from one through six rows and then scrolls
    internally. Structured draft elements use {!Theme.atom}; the view never
    mutates [t]. Input-mode changes preserve the mounted editor, so controlled
    value and cursor updates do not reset its widget-local viewport. The frame
    takes the width allocated by its Mosaic parent: its rules are one-sided
    border boxes, and their mode/agent chips and intervening rule compose
    through intrinsic flex layout. No caller predicts or passes terminal
    geometry.

    [submit_enabled] defaults to [true] and controls whether Enter emits
    {!Submit}. A plain leading [!] in an empty shell-enabled prompt enters shell
    mode through Textarea's complete reported value; the trigger is consumed and
    any following bytes in the same report remain in the command. [list_open]
    defaults to [false]; when true, up/down, ctrl+p/n, and tab are intercepted
    as {!List_key}. With no list, unmodified up/down are intercepted only for a
    single-line draft, where the shell uses them for the prompt-history walk.
    When [inspect_latest_change] is present and no list is open, Ctrl+D emits
    that application-owned message before Textarea's ordinary forward-delete
    binding, leaving the controlled draft unchanged.

    [mode] defaults to {!Mentat_session.Contract.Mode.Build}. Plan and Review
    render their owned mode chips and colors; Review remains renderable for
    replay even though the next command palette cannot initiate review. [agent]
    adds a right chip and heats a Build frame to the accent color. Malformed
    UTF-8 in [agent] is replaced, surrounding and line-breaking ASCII whitespace
    is removed or rendered as spaces, and an empty normalized agent is treated
    as absent. Mosaic clips intrinsic labels when their parent cannot allocate
    their full width.

    Placeholder precedence is history search, shell command, addressed agent,
    queued turn, then ordinary Mentat input. [turn_running] defaults to [false].
    [top_margin] defaults to one blank row and may be zero when another surface
    is mounted immediately above the frame.

    Raises [Invalid_argument] if [top_margin < 0]. *)
