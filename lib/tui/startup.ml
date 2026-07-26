(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type input = Empty | Draft of string | Submit of string

type t = {
  snapshot : Snapshot.t;
  mode : Mentat_session.Contract.Mode.t;
  session : Mentat_session.Id.t option;
  input : input;
  providers : Mentat_provider.t list;
  permission_review : Mentat_permission.Review_behavior.t;
  launch_review : bool;
}

let make ?(launch_review = false) ~snapshot ~mode ~session ~input ~providers
    ~permission_review () =
  {
    snapshot;
    mode;
    session;
    input;
    providers;
    permission_review;
    launch_review;
  }

let snapshot t = t.snapshot
let mode t = t.mode
let session t = t.session
let input t = t.input
let providers t = t.providers
let permission_review t = t.permission_review
let launch_review t = t.launch_review
