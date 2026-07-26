(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Syntax-aware edits to one OCaml implementation or interface.

    [Ocaml_ast_edit] parses a complete [.ml] or [.mli] file with the OCaml
    compiler parser, resolves every requested selector against that one parsed
    tree, validates each supplied fragment in the selected syntactic category,
    and reparses the complete rewritten file before applying a stale-safe
    {!Mentat_edit.t} through {!Mentat_workspace_io.Edit.apply}. It is useful
    when exact text replacement would be fragile; use [edit_file] for a small,
    unique textual replacement.

    {1 Input contract}

    The model input is a strict JSON object with these members:

    - [path], a required non-empty workspace-relative or workspace-contained
      absolute source path;
    - [file_kind], optionally ["implementation"]/["ml"] or
      ["interface"]/["mli"]. When absent, [.ml] selects the implementation
      grammar and [.mli] the interface grammar; another suffix is rejected;
    - [if_identity], optionally a complete-file identity from a previous
      complete [read_file] observation;
    - [edits], a required non-empty array, applied as one atomic full-file
      rewrite.

    Every edit object has [op], [selector], and, except for deletion, [text].
    [op] is one of ["replace"], ["insert_before"], ["insert_after"], or
    ["delete"]. Replacement and insertion text must be non-empty valid UTF-8. As
    in the current tool, [delete] accepts an optional [text] member but ignores
    it. Inserting before or after is valid only for an item selector. Item
    replacement and insertion fragments parse as zero or more structure or
    signature items in the target grammar; expression and type replacements
    parse as exactly one expression or core type. The final complete file must
    also parse.

    A selector is a strict object whose [mode] is:

    - ["item"]: [path] is a non-empty array of non-empty qualified source-name
      components such as [["M","N","answer"]]. Optional [item_kind] narrows the
      namespace to ["value"], ["type"], ["module"], ["module_type"],
      ["exception"], ["external"], ["open"], ["include"], ["class"],
      ["class_type"], ["extension"], or ["eval"]. Optional [occurrence] is a
      one-based index and defaults to [1];
    - ["enclosing"]: [kind] is ["item"], ["expression"], or ["type"], and [line]
      plus [column] identify a position. The smallest matching compiler location
      containing that position is selected;
    - ["exact"]: [kind] has the same vocabulary, and
      [start_line]/[start_column]/[end_line]/[end_column] describe the exact
      half-open compiler range to select.

    [node_item_kind] may narrow [kind = "item"] for [enclosing] and [exact],
    using the item-kind vocabulary above. It is invalid with expression or type
    nodes. Lines are one-based; columns are zero-based byte offsets, not Unicode
    scalar or display-cell offsets. End coordinates are excluded. Every JSON
    integer must be exactly representable in JavaScript's safe integer range;
    values outside that range are rejected rather than rounded. Conditional
    members for the chosen selector mode are required by decoding even where
    JSON Schema cannot express that dependency compactly. Unknown and repeated
    members at every object level are rejected. The current executable contract
    does, however, accept and ignore known members belonging to another mode:
    [item] consumes only [path]/[item_kind]/[occurrence], [enclosing] consumes
    only [kind]/[node_item_kind]/[line]/[column], and [exact] consumes only
    [kind]/[node_item_kind] plus its four range coordinates. This deliberate
    parity rule is not conditional-schema validation; a consumed
    [node_item_kind] is still rejected with expression or type [kind].

    For example, this replaces one nested value declaration:

    {v
    {"path":"lib/x.ml","edits":[{"op":"replace","selector":{"mode":"item","path":["M","answer"],"item_kind":"value"},"text":"let answer = 42"}]}
    v}

    This replaces the exact type node at columns 11 through 14 on line 2:

    {v
    {"path":"lib/x.ml","edits":[{"op":"replace","selector":{"mode":"exact","kind":"type","start_line":2,"start_column":11,"end_line":2,"end_column":14},"text":"string"}]}
    v}

    {1 Selection and rewrite semantics}

    All selectors resolve against the original parse tree, not against the
    result of earlier edits in the array. Qualified item paths follow named
    modules whose bodies are literal structures or signatures. A value binding
    is indexed under every variable bound by its pattern. Opens use the final
    identifier of a simple opened module (or ["open"] when none is available);
    includes, extensions, evaluations, and type extensions use the synthetic
    final components ["include"], ["extension"], ["eval"], and
    ["type_extension"]. Compiler-generated ghost locations are ignored. An item
    occurrence is chosen in parse order. Exact selection accepts compiler nodes
    that duplicate the same requested range as one selection; any defensive
    ambiguity failure reports every competing range.

    One inherited boundary is narrower than the general item rule: the synthetic
    ["type_extension"] item selects the compiler location of the extended type
    path, not the enclosing extension declaration. Its supplied replacement is
    nevertheless validated as an item fragment and the complete splice must
    reparse. Calls that cannot satisfy both conditions are rejected; this tool
    does not silently widen that selector to the whole declaration.

    Selected ranges may not overlap. Two insertions at the same byte boundary
    are allowed; under the legacy bottom-up splice rule, a later input insertion
    at that exact boundary appears before an earlier one. An insertion at the
    boundary of a non-empty replacement is allowed, but an insertion and a
    replacement starting at the same boundary conflict. Source bytes,
    formatting, and comments outside selected compiler locations are preserved
    exactly. Supplied fragment bytes replace or adjoin locations exactly; the
    tool does not run an OCaml formatter, normalize line endings, or move or
    reattach comments outside compiler locations. In particular, CRLF outside a
    splice remains CRLF. A leading UTF-8 byte-order mark is not stripped before
    parsing; the OCaml grammar's rejection of that mark is a source parse error.

    The target must exist, be a regular file no larger than {!max_file_bytes},
    contain valid non-binary UTF-8, and parse with the chosen grammar. A
    supplied [if_identity] is compared with the complete bytes before parsing. A
    mismatch, or a change after observation but before application, fails
    [`Stale] without overwriting the newer file. Read resolution follows
    in-workspace symlinks, while the native write boundary is deliberately
    no-follow; consequently a readable symlink target is still refused at
    application.

    {1 Permissions, output, and errors}

    A decoded call requests one modify access for the resolved path. Its
    {!Mentat_permission.Request.Change} is computed from decoded edits only and
    never reads the target. [additions] is the sum of logical line counts of all
    replace and insert fragments; deletion contributes zero. [removals] is
    exactly zero only when every edit is an insertion and is otherwise unknown,
    because replacement and deletion source text has not yet been selected. Thus
    two insertions of ["let a = 1\n"] and ["let b = 2"] report two additions and
    zero removals; a replacement plus an insertion reports their summed
    additions and unknown removals; a delete-only call reports zero additions
    and unknown removals. No fabricated before/after diff is attached. An
    unresolvable path produces no request and then fails before file access.

    A changed success returns one human summary line containing the display
    path. Its durable semantic {!Mentat_tools_output.Update.t} carries only
    applied disposition, one changed file, the decoded-fragment line counts, and
    zero skipped files. A byte-identical rewrite returns the [unchanged] summary
    with zero changed files and zero for both counts without applying a
    mutation. Semantic JSON omits paths, selected source text, source ranges,
    complete before/after bytes, content identities, diffs, edit results,
    receipts, and mutation evidence. The capability's claim scope owns the
    authoritative result, which this tool discards.

    Malformed JSON, invalid coordinates or action combinations, invalid UTF-8
    fragments, source or fragment parse errors, a missing or ambiguous
    selection, and overlapping edits fail [`Invalid_input]. A missing path fails
    [`Not_found]; file-size, non-regular, binary, protected-path, or read-only
    refusals fail [`Invalid_input]; freshness conflicts fail [`Stale]; native
    I/O failures fail [`Failed]. Compiler parse diagnostics name the phase and
    include the compiler range when available. Their prose is meant for humans
    and is not a stable matching surface. Cancellation is reported as a
    cancelled interruption. *)

val name : string
(** [name] is ["ocaml_ast_edit"]. *)

val max_file_bytes : int
(** [max_file_bytes] is the one-MiB bound on the complete observed and final
    source file. The native edit boundary enforces the same final bound. *)

val make : Mentat_workspace_io.t -> Mentat_tool.t
(** [make workspace_io] is the immutable model-facing tool definition over
    [workspace_io]. Construction performs no I/O.

    The run callback polls cancellation before path resolution and again after
    complete-file observation and planning, immediately before a changed plan is
    applied. Cancellation at either poll prevents mutation. Once
    {!Mentat_workspace_io.Edit.apply} begins, it completes inside the
    capability's protected commit-and-attribution boundary. *)
