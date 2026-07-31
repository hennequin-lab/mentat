(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Checkpoint-hash cost: capturing and comparing file content by identity.

    Before a turn's edits, a conservative checkpoint hashes each file's bytes
    into a content reference; after, a re-read is checked against that reference
    to decide whether the content actually changed. The felt moment is the pause
    a user waits through before a tool turn's edits begin — dominated by hashing
    the files in scope — and the dedup check that decides whether a re-read is a
    fresh blob or the same one. The capture axis (1K/64K/1M) says how the pause
    scales with file size; the dedup check must stay near-free because it runs
    on every re-read, and its cheapest arm is a length comparison that never
    hashes. *)

module Digest = Mentat_digest
module Content_ref = Digest.Content_ref
module Bytes = Bench_support.Bytes

let medium_ref = Content_ref.of_contents Bytes.medium
let medium_token = Content_ref.to_token medium_ref

(* Same length as [medium], differing in the first byte: forces the dedup check
   past its length fast-path into a full content comparison. *)
let same_size_other =
  String.mapi (fun i c -> if i = 0 then '\001' else c) Bytes.medium

(* One byte longer than [medium]: the dedup check rejects on length alone. *)
let wrong_size = Bytes.medium ^ "x"

let () =
  Thumper.run "digest"
    ~budgets:
      [
        Thumper.Budget.no_more_alloc_than 0.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
      ]
    Thumper.
      [
        group "capture"
          (List.map
             (fun (label, bytes) ->
               bench label (fun () -> Content_ref.of_contents bytes))
             [ ("1K", Bytes.small); ("64K", Bytes.medium); ("1M", Bytes.large) ]);
        group "token"
          [
            bench "to-token" (fun () -> Content_ref.to_token medium_ref);
            bench "of-token" (fun () -> Content_ref.of_token medium_token);
          ];
        group "dedup-check"
          [
            bench "equal-ref" (fun () ->
                Content_ref.equal medium_ref medium_ref);
            bench "matches/same-bytes" (fun () ->
                Content_ref.matches medium_ref Bytes.medium);
            bench "matches/same-length" (fun () ->
                Content_ref.matches medium_ref same_size_other);
            bench "matches/wrong-length" (fun () ->
                Content_ref.matches medium_ref wrong_size);
          ];
      ]
