(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Bounded, credential-free public web fetch tool.

    [web_fetch] accepts a strict JSON object with required string [url] and
    optional [format] (["markdown"], ["text"], or ["html"]) and positive integer
    [timeout_ms]. Unknown and duplicate members are rejected. URLs are at most
    2,000 bytes, use HTTP or HTTPS, have a host, and contain neither user
    information nor a fragment. HTTP is always upgraded to HTTPS before
    permission planning and execution.

    Each decoded call requests HTTPS network access for the exact lower-case
    host and effective port that execution consumes. The tool generates only
    [User-Agent], format-specific [Accept], and [Accept-Language] headers. A
    call-scoped {!Eio.Switch.run} contains the injected transport. The transport
    remains responsible for whole-call timeout enforcement, DNS address
    admission and pinning, TLS verification, at most ten exact-authority
    redirects, streaming body limits, and cancellation.

    Successful 2xx text responses complete with bounded content. Non-2xx
    responses fail with a bounded partial output. The policy character bound
    applies to the projected body or preview; fixed response headings and a
    truncation note are additional. Cross-authority redirects complete without
    contacting the target and instruct the model to make a new call, which
    requires a new permission decision. Non-text MIME types, unsupported
    charsets, invalid UTF-8, oversized responses, and protocol violations fail.
    External diagnostics are repaired to UTF-8, stripped of ANSI CSI/OSC
    sequences, trimmed, and bounded to 4,096 bytes.

    HTML projection removes active and metadata elements before emitting
    Markdown, visible text, or sanitized HTML. The sanitizer is for inert model
    output and is not a browser-grade security boundary. Compact durable output
    retains only disposition, status, and final-response bytes; URLs, bodies,
    MIME types, redirect targets, and diagnostics remain in authoritative text.
*)

val name : string
(** [name] is ["web_fetch"]. *)

val make : policy:Policy.t -> fetch:Transport.t -> Mentat_tool.t
(** [make ~policy ~fetch] is the immutable [web_fetch] definition. Construction
    starts no work and opens no network resource.

    A malformed URL or timeout produces no permission request and fails as
    [`Invalid_input]. A private resolved address fails as [`Permission_denied];
    timeout as [`Timed_out]; backend unavailability as [`Unavailable]; and
    response-limit, redirect-limit, protocol, MIME, charset, and UTF-8 failures
    as [`Failed]. Cooperative cancellation returns a cancelled interruption.

    [fetch] is invoked once beneath a fresh call-scoped switch. It receives the
    same normalized URL, generated headers, timeout, byte limit, private-network
    setting, and fixed redirect limit from which the tool's behavior was
    derived. *)
