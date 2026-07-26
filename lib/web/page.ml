(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mentat_session
open Mentat_protocol

(* The in-page copy of the strict policy the network edge sets as the
   authoritative header. [frame-ancestors] is header-only and a browser
   ignores it here, but the remaining directives take effect from the meta and
   document the contract at the point the page is served: same-origin script and
   style only, no inline or eval, connect (the event stream) and form posts to
   the same origin only. *)
let content_security_policy =
  "default-src 'none'; script-src 'self'; style-src 'self'; img-src 'self' \
   data:; connect-src 'self'; form-action 'self'; base-uri 'none'; \
   frame-ancestors 'none'"

let meta ?at () = Html.El.v ?at "meta" []
let link ?at () = Html.El.v ?at "link" []

(* The document shell. [head_title] is escaped owner text; the rest is fixed
   author chrome. *)
let document ~head_title ~body =
  Html.El.splice
    [
      Html.El.unsafe_raw "<!doctype html>";
      Html.El.v
        ~at:[ Html.At.v "lang" "en" ]
        "html"
        [
          Html.El.v "head"
            [
              meta ~at:[ Html.At.v "charset" "utf-8" ] ();
              meta
                ~at:
                  [
                    Html.At.name "viewport";
                    Html.At.v "content" "width=device-width, initial-scale=1";
                  ]
                ();
              meta
                ~at:
                  [
                    Html.At.v "http-equiv" "Content-Security-Policy";
                    Html.At.v "content" content_security_policy;
                  ]
                ();
              meta
                ~at:
                  [
                    Html.At.name "color-scheme";
                    Html.At.v "content" "light dark";
                  ]
                ();
              Html.El.v "title" [ Html.El.txt head_title ];
              link
                ~at:
                  [
                    Html.At.v "rel" "stylesheet"; Html.At.href "/static/app.css";
                  ]
                ();
              Html.El.v
                ~at:[ Html.At.type' "module"; Html.At.v "src" "/static/app.js" ]
                "script" [];
            ];
          Html.El.v "body" body;
        ];
    ]

(* ── Session index ──────────────────────────────────────────────────────── *)

let post_button ~action ~label ~cls =
  Html.El.form
    ~at:
      [ Html.At.method' "post"; Html.At.action action; Html.At.class_ "action" ]
    [
      Html.El.button
        ~at:[ Html.At.type' "submit"; Html.At.class_ cls ]
        [ Html.El.txt label ];
    ]

let lifecycle_actions summary =
  let id = Id.to_string (Summary.id summary) in
  let base = "/session/" ^ id in
  let status = Summary.lifecycle summary in
  let archive_or_restore =
    if Metadata.Status.is_archived status then
      [
        post_button ~action:(base ^ "/restore") ~label:"Restore" ~cls:"restore";
      ]
    else if Metadata.Status.is_active status then
      [
        post_button ~action:(base ^ "/archive") ~label:"Archive" ~cls:"archive";
      ]
    else []
  in
  let delete =
    if Metadata.Status.is_deleted status then []
    else
      [ post_button ~action:(base ^ "/delete") ~label:"Delete" ~cls:"delete" ]
  in
  archive_or_restore @ delete

let session_row summary =
  let id = Id.to_string (Summary.id summary) in
  let preview =
    match Summary.preview summary with
    | Some text ->
        [ Html.El.p ~at:[ Html.At.class_ "preview" ] [ Html.El.txt text ] ]
    | None -> []
  in
  Html.El.li
    ~at:[ Html.At.class_ "session-row" ]
    [
      Html.El.a
        ~at:[ Html.At.href ("/session/" ^ id); Html.At.class_ "session-link" ]
        (Html.El.span
           ~at:[ Html.At.class_ "title" ]
           [ Html.El.txt (Summary.display_title summary) ]
        :: Html.El.span
             ~at:
               [
                 Html.At.class_
                   ("phase " ^ Summary.Phase.to_string (Summary.phase summary));
               ]
             [ Html.El.txt (Summary.Phase.to_string (Summary.phase summary)) ]
        :: Html.El.span
             ~at:[ Html.At.class_ "turns" ]
             [ Html.El.txt (string_of_int (Summary.turns summary) ^ " turns") ]
        :: preview);
      Html.El.div ~at:[ Html.At.class_ "actions" ] (lifecycle_actions summary);
    ]

let new_session_form =
  Html.El.form
    ~at:
      [
        Html.At.method' "post";
        Html.At.action "/sessions";
        Html.At.class_ "new-session";
      ]
    [
      Html.El.button
        ~at:[ Html.At.type' "submit"; Html.At.class_ "primary" ]
        [ Html.El.txt "New session" ];
    ]

let unreadable_notice = function
  | 0 -> Html.El.void
  | n ->
      Html.El.p
        ~at:[ Html.At.class_ "notice failure" ]
        [
          Html.El.txt
            (string_of_int n ^ " saved session"
            ^ (if n = 1 then "" else "s")
            ^ " could not be read.");
        ]

let session_list ~sessions ~unreadable =
  let rows =
    match sessions with
    | [] ->
        [
          Html.El.p
            ~at:[ Html.At.class_ "empty" ]
            [ Html.El.txt "No sessions yet." ];
        ]
    | _ ->
        [
          Html.El.ul
            ~at:[ Html.At.class_ "sessions" ]
            (List.map session_row sessions);
        ]
  in
  document ~head_title:"Mentat"
    ~body:
      [
        Html.El.v
          ~at:[ Html.At.class_ "top" ]
          "header"
          [ Html.El.h 1 [ Html.El.txt "Mentat" ]; new_session_form ];
        Html.El.v
          ~at:[ Html.At.id "sessions" ]
          "nav"
          (unreadable_notice unreadable :: rows);
      ]

(* ── One session's transcript ───────────────────────────────────────────── *)

let earlier_control ~session ~before =
  let attrs, children =
    match before with
    | None -> ([ Html.At.id "earlier" ], [])
    | Some position ->
        let base = "/session/" ^ Id.to_string session in
        ( [
            Html.At.id "earlier";
            Html.At.data "before" (string_of_int (Position.seq position));
          ],
          [
            Html.El.button
              ~at:
                [
                  Html.At.type' "button";
                  Html.At.class_ "load-earlier";
                  Html.At.data "src"
                    (base ^ "/before?p=" ^ string_of_int (Position.seq position));
                ]
              [ Html.El.txt "Load earlier" ];
          ] )
  in
  Html.El.div ~at:attrs children

let composer ~session =
  let base = "/session/" ^ Id.to_string session in
  Html.El.form
    ~at:
      [
        Html.At.id "composer";
        Html.At.method' "post";
        Html.At.action (base ^ "/prompt");
      ]
    [
      Html.El.textarea
        ~at:
          [
            Html.At.name "prompt";
            Html.At.v "rows" "3";
            Html.At.v "placeholder" "Message Mentat…";
          ]
        [];
      Html.El.div
        ~at:[ Html.At.class_ "composer-actions" ]
        [
          Html.El.button
            ~at:[ Html.At.type' "submit"; Html.At.class_ "primary" ]
            [ Html.El.txt "Send" ];
          Html.El.button
            ~at:
              [
                Html.At.type' "submit";
                Html.At.v "formaction" (base ^ "/queue");
                Html.At.class_ "queue";
              ]
            [ Html.El.txt "Queue" ];
        ];
    ]

let session_controls ~session =
  let base = "/session/" ^ Id.to_string session in
  Html.El.div
    ~at:[ Html.At.class_ "controls" ]
    [
      post_button ~action:(base ^ "/interrupt") ~label:"Interrupt"
        ~cls:"interrupt";
      post_button ~action:(base ^ "/compact") ~label:"Compact" ~cls:"compact";
    ]

let session ~title ~session ~resume ~earlier ~body =
  let resume_token =
    match resume with Some p -> string_of_int (Position.seq p) | None -> ""
  in
  document ~head_title:("Mentat — " ^ title)
    ~body:
      [
        Html.El.v
          ~at:[ Html.At.class_ "top" ]
          "header"
          [
            Html.El.a
              ~at:[ Html.At.href "/"; Html.At.class_ "back" ]
              [ Html.El.txt "← Sessions" ];
            Html.El.h 1
              ~at:[ Html.At.class_ "session-title" ]
              [ Html.El.txt title ];
            session_controls ~session;
          ];
        Html.El.v
          ~at:
            [
              Html.At.id "main";
              Html.At.data "session" (Id.to_string session);
              Html.At.data "resume" resume_token;
            ]
          "main"
          [ earlier_control ~session ~before:earlier; body ];
        composer ~session;
      ]

(* ── Error document ─────────────────────────────────────────────────────── *)

let error ~status ~message =
  document ~head_title:"Mentat — error"
    ~body:
      [
        Html.El.v
          ~at:[ Html.At.class_ "error-page" ]
          "main"
          [
            Html.El.h 1 [ Html.El.txt (string_of_int status) ];
            Html.El.p [ Html.El.txt message ];
            Html.El.p
              [
                Html.El.a ~at:[ Html.At.href "/" ] [ Html.El.txt "← Sessions" ];
              ];
          ];
      ]
