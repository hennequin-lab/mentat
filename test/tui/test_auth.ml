(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Provider = Mentat_llm.Provider
module Declaration = Mentat_provider
module Auth = Declaration.Auth
module Login = Auth.Login
module Account = Declaration.Account
module Credential = Declaration.Credential
module Client_login = Mentat_client.Login
module Protocol = Mentat_protocol

let submit t text =
  Tui.keys t text;
  Tui.enter t;
  Tui.settle t

let provider id = Provider.make id
let oauth_client = Oauth2.Client.make ~id:"tui-test" ()

let declaration_with_logins provider logins =
  Declaration.make provider ~display_name:(Provider.id provider)
    ~auth:(Auth.make ~login:logins ())
    []

let declaration provider login = declaration_with_logins provider [ login ]

let browser_login () =
  Login.oauth2_authorization_code ~id:(Login.Id.make "browser") ~label:"Browser"
    ~client:oauth_client
    ~authorization_endpoint:(Uri.of_string "https://auth.example/authorize")
    ~token_endpoint:(Uri.of_string "https://auth.example/token")
    ()

let device_login () =
  Login.oauth2_device_code
    ~id:(Login.Id.make "device-code")
    ~label:"Device code" ~client:oauth_client
    ~device_endpoint:(Uri.of_string "https://auth.example/device")
    ~token_endpoint:(Uri.of_string "https://auth.example/token")
    ()

let external_login () =
  Login.make
    ~id:(Login.Id.make "corporate-sso")
    ~label:"Corporate SSO"
    (Login.Protocol.External
       {
         instructions =
           Some
             "Open the company identity portal and approve Mentat for this \n\
              workspace.";
         credential_kind = Some Credential.Kind.OAuth;
       })

let credential provider secret =
  Credential.make ~provider ~source:Credential.Source.process
    (Credential.Secret.api_key secret)

let account ?(problems = []) provider =
  Account.checked (credential provider "sk-test-account") ~problems ()

let unavailable text =
  Protocol.Error.Unavailable (Mentat_diagnostic.of_text text)

(* The provider owner's effective model readiness for the post-login handoff:
   [route] declares one model and its checked account lists that id, so the
   handed-off model picker renders one eligible, available row. *)
let one_model_readiness ?(problems = []) ?(id = "primary") route =
  let catalog_model =
    Mentat_provider.Model.make
      (Mentat_llm.Model.make ~provider:route
         ~api:(Mentat_llm.Model.Api.make "responses")
         ~id)
      ()
  in
  let declaration =
    Declaration.make route ~display_name:(Provider.id route) [ catalog_model ]
  in
  let discovery =
    Account.Discovery.Known
      (Account.checked
         (credential route "sk-handoff")
         ~problems ~models:[ id ] ())
  in
  Tui.Model_readiness.of_providers [ declaration ] ~discoveries:[ discovery ]

let require_opened_urls t expected =
  let to_string uri = Uri.to_string uri in
  let expected = List.map to_string expected in
  let actual = Tui.opened_urls t |> List.map to_string in
  if not (List.equal String.equal expected actual) then
    failwith
      (Printf.sprintf "expected opened URLs [%s], observed [%s]"
         (String.concat "; " expected)
         (String.concat "; " actual))

(* Every checkpoint is a complete numbered terminal. Client operations remain
   held until the journey releases their one typed result, so pending and
   settled ownership stay available for human review. *)

let%expect_test "external login names its provider once in the complete panel" =
  let route = provider "external-provider" in
  let login = external_login () in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  Tui.run ~name:"auth-external" ~size:(100, 40)
    ~providers:[ declaration route login ]
    ~readiness_responses:[ [ missing ]; [ missing ] ]
  @@ fun t ->
  Tui.settle t;
  submit t "/login external-provider";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Log in to external-provider · Corporate SSO
    29 |
    30 |   Open the company identity portal and approve Mentat for this
    31 |   workspace.
    32 |
    33 |   This declared method cannot be completed by the Mentat client.
    34 |
    35 |   Press c to copy the instructions. Escape goes back.
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.keys t "c";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Log in to external-provider · Corporate SSO
    29 |
    30 |   Open the company identity portal and approve Mentat for this
    31 |   workspace.
    32 |
    33 |   This declared method cannot be completed by the Mentat client.
    34 |
    35 |   Instructions copied. Escape goes back.
    36 |
    37 |
    38 |
    39 |
    40 |
    |}]

let%expect_test
    "browser progress crosses the exact URL boundary and saved login refreshes \
     discovery" =
  let route = provider "browser-provider" in
  let login = browser_login () in
  let saved = account ~problems:[ Account.Problem.Revoked ] route in
  let script =
    Tui.Login_script.make ~provider:route ~method_:(Login.id login)
      [
        Ok
          (Client_login.Progress
             (Login.Progress.Browser_url
                (Uri.of_string "https://auth.example/start?attempt=exact")));
        Ok
          (Client_login.Progress
             (Login.Progress.Listening
                {
                  redirect_uri = Uri.of_string "http://127.0.0.1:1717/callback";
                }));
        Ok (Client_login.Saved saved);
      ]
  in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  Tui.run ~name:"auth-browser" ~size:(100, 40) ~browser:true
    ~providers:[ declaration route login ]
    ~readiness_responses:
      [ [ missing ]; [ missing ]; [ Account.Discovery.Known saved ] ]
    ~login_scripts:[ script ]
  @@ fun t ->
  Tui.settle t;
  submit t "/login browser-provider";
  Tui.next_login_step t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Log in to browser-provider · browser
    29 |
    30 |   Sign-in URL
    31 |   https://auth.example/start?attempt=exact
    32 |
    33 |   Browser opened. Complete sign-in there.
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.next_login_step t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Log in to browser-provider · browser
    29 |
    30 |   Sign-in URL
    31 |   https://auth.example/start?attempt=exact
    32 |
    33 |   Listening for the authorization callback at
    34 |
    35 |   http://127.0.0.1:$PORT/callback
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.next_login_step t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   browser-provider
    29 |
    30 |   Credential saved.
    31 |
    32 |   Account readiness: blocked
    33 |
    34 |   Enter or Escape closes.
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}]

let%expect_test
    "browser copy, reopen, and Escape cancellation remain visibly correlated" =
  let route = provider "browser-controls" in
  let login = browser_login () in
  let url = Uri.of_string "https://auth.example/control?attempt=browser" in
  let script =
    Tui.Login_script.make ~provider:route ~method_:(Login.id login)
      [
        Ok (Client_login.Progress (Login.Progress.Browser_url url));
        Ok Client_login.Cancelled;
      ]
  in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  Tui.run ~name:"auth-browser-controls" ~size:(100, 40) ~browser:true
    ~providers:[ declaration route login ]
    ~readiness_responses:[ [ missing ]; [ missing ] ]
    ~login_scripts:[ script ]
  @@ fun t ->
  Tui.settle t;
  submit t "/login browser-controls";
  Tui.next_login_step t;
  Tui.settle t;
  require_opened_urls t [ url ];
  Tui.keys t "c";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Log in to browser-controls · browser
    29 |
    30 |   Sign-in URL
    31 |   https://auth.example/control?attempt=browser · copied
    32 |
    33 |   Browser opened. Complete sign-in there.
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.enter t;
  Tui.settle t;
  require_opened_urls t [ url; url ];
  Tui.keys t Key.escape;
  Tui.settle t;
  (* The UI has already cancelled and left the attempt. The terminal callback
     now proves that its result is stale while releasing the scripted owner. *)
  Tui.next_login_step t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Choose a provider.
    29 |
    30 |    ▶ 1  browser-controls
    31 |      missing
    32 |
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}]

let%expect_test
    "cancelled and failed device logins settle without refreshing discovery" =
  let route = provider "device-provider" in
  let login = device_login () in
  let cancelled =
    Tui.Login_script.make ~provider:route ~method_:(Login.id login)
      [
        Ok
          (Client_login.Progress
             (Login.Progress.Device_challenge
                {
                  url = Uri.of_string "https://auth.example/activate";
                  user_code = "ABCD-EFGH";
                  expires_in = 600;
                }));
        Ok Client_login.Cancelled;
      ]
  in
  let failed =
    Tui.Login_script.make ~provider:route ~method_:(Login.id login)
      [ Error (unavailable "device exchange failed") ]
  in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  Tui.run ~name:"auth-device" ~size:(100, 40)
    ~providers:[ declaration route login ]
    ~readiness_responses:[ [ missing ]; [ missing ]; [ missing ] ]
    ~login_scripts:[ cancelled; failed ]
  @@ fun t ->
  Tui.settle t;
  submit t "/login device-provider";
  Tui.next_login_step t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Log in to device-provider · device code                                                          █
    29 |                                                                                                    █
    30 |   Verification URL                                                                                 █
    31 |   https://auth.example/activate                                                                    █
    32 |                                                                                                    █
    33 |   Device code                                                                                      ▀
    34 |   ABCD-EFGH
    35 |
    36 |   Challenge expires in 600 seconds.
    37 |
    38 |   Open the verification page, then enter the device code.
    39 |
    40 |
    |}];
  Tui.next_login_step t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   device-provider
    29 |
    30 |   Login cancelled.
    31 |
    32 |   Enter or Escape closes.
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  submit t "/login device-provider";
  Tui.next_login_step t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   device-provider
    29 |
    30 |   device exchange failed
    31 |
    32 |   Enter or Escape closes.
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}]

let%expect_test
    "device code and URL copy, open, and control-c cancellation stay visible" =
  let route = provider "device-controls" in
  let login = device_login () in
  let url = Uri.of_string "https://auth.example/device-controls" in
  let script =
    Tui.Login_script.make ~provider:route ~method_:(Login.id login)
      [
        Ok
          (Client_login.Progress
             (Login.Progress.Device_challenge
                { url; user_code = "SAFE-CODE"; expires_in = 300 }));
        Ok Client_login.Cancelled;
      ]
  in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  Tui.run ~name:"auth-device-controls" ~size:(100, 40) ~browser:true
    ~providers:[ declaration route login ]
    ~readiness_responses:[ [ missing ]; [ missing ] ]
    ~login_scripts:[ script ]
  @@ fun t ->
  Tui.settle t;
  submit t "/login device-controls";
  Tui.next_login_step t;
  Tui.settle t;
  Tui.keys t "c";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Log in to device-controls · device code                                                          █
    29 |                                                                                                    █
    30 |   Verification URL                                                                                 █
    31 |   https://auth.example/device-controls                                                             █
    32 |                                                                                                    █
    33 |   Device code                                                                                      ▀
    34 |   SAFE-CODE · copied
    35 |
    36 |   Challenge expires in 300 seconds.
    37 |
    38 |   Open the verification page, then enter the device code.
    39 |
    40 |
    |}];
  Tui.keys t "u";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Log in to device-controls · device code                                                          █
    29 |                                                                                                    █
    30 |   Verification URL                                                                                 █
    31 |   https://auth.example/device-controls · copied                                                    █
    32 |                                                                                                    █
    33 |   Device code                                                                                      ▀
    34 |   SAFE-CODE
    35 |
    36 |   Challenge expires in 300 seconds.
    37 |
    38 |   Open the verification page, then enter the device code.
    39 |
    40 |
    |}];
  Tui.enter t;
  Tui.settle t;
  require_opened_urls t [ url ];
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Log in to device-controls · device code                                                          █
    29 |                                                                                                    █
    30 |   Verification URL                                                                                 █
    31 |   https://auth.example/device-controls · copied                                                    █
    32 |                                                                                                    █
    33 |   Device code                                                                                      ▀
    34 |   SAFE-CODE
    35 |
    36 |   Challenge expires in 300 seconds.
    37 |
    38 |   Verification page opened. Enter the device code there.
    39 |
    40 |
    |}];
  Tui.keys t Key.ctrl_c;
  Tui.settle t;
  (* The UI has already cancelled and left the attempt. The terminal callback
     now proves that its result is stale while releasing the scripted owner. *)
  Tui.next_login_step t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Choose a provider.
    29 |
    30 |    ▶ 1  device-controls
    31 |      missing
    32 |
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}]

let%expect_test
    "API-key save stays pending until one typed completion and refresh" =
  let route = provider "key-provider" in
  let login = Login.api_key () in
  let saved = account ~problems:[ Account.Problem.Network ] route in
  let save provider ~key =
    let credential =
      Credential.make ~provider ~source:Credential.Source.process
        (Credential.Secret.api_key key)
    in
    Ok (Account.checked credential ~problems:[ Account.Problem.Network ] ())
  in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  Tui.run ~name:"auth-api-key" ~size:(100, 40)
    ~providers:[ declaration route login ]
    ~readiness_responses:
      [ [ missing ]; [ missing ]; [ Account.Discovery.Known saved ] ]
    ~model_readiness_results:
      [ Ok (one_model_readiness ~problems:[ Account.Problem.Network ] route) ]
    ~save_api_key:save
  @@ fun t ->
  Tui.settle t;
  submit t "/login key-provider";
  Tui.paste t "sk-panel-secret";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Enter an API key for key-provider
    29 |
    30 |   ❯ •••••••••••••••
    31 |
    32 |   The key is masked and leaves this panel only when saved.
    33 |
    34 |   Enter saves. Escape discards and goes back.
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Saving API key…
    29 |
    30 |
    31 |
    32 |
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.finish_save_api_key t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    model
    27 |
    28 | Current  openai/gpt-5.5 medium
    29 |
    30 | 1 providers · 1 models
    31 |
    32 |  ❯   key-provider         primary
    33 |
    34 |
    35 | ○ Reasoning effort is not adjustable for this model
    36 |
    37 | Filter or set by name
    38 |
    39 |
    40 |
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    15 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    16 |
    17 |                                      dev · openai/gpt-5.5 medium
    18 |
    19 |                ▎ welcome — and thanks for trying mentat this early.
    20 |                ▎ it's experimental: sessions and config may change without migration.
    21 |
    22 |
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    24 | ❯ message mentat
    25 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    26 |
    27 |                                         ∅ no recent sessions
    28 |
    29 |
    30 |
    31 |
    32 |
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |   ~/mentat-tui-auth4c7ab92a · openai/gpt-5.5 medium · ! full access                ? for shortcuts
    |}]

(* The post-login model handoff is uniform across entry paths: a first browser
   login that saves a usable credential hands off to the model picker, the same
   "now what?" answer the API-key path gives. The launch model still names a
   provider that is not signed in, so the picker opens on the freshly usable
   route. *)
let%expect_test "a first browser login hands off to the model picker" =
  let route = provider "browser-provider" in
  let login = browser_login () in
  let saved = account route in
  let script =
    Tui.Login_script.make ~provider:route ~method_:(Login.id login)
      [
        Ok
          (Client_login.Progress
             (Login.Progress.Browser_url
                (Uri.of_string "https://auth.example/start")));
        Ok (Client_login.Saved saved);
      ]
  in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  Tui.run ~name:"auth-browser-handoff" ~size:(100, 40) ~browser:true
    ~providers:[ declaration route login ]
    ~readiness_responses:
      [ [ missing ]; [ missing ]; [ Account.Discovery.Known saved ] ]
    ~model_readiness_results:[ Ok (one_model_readiness route) ]
    ~login_scripts:[ script ]
  @@ fun t ->
  Tui.settle t;
  submit t "/login browser-provider";
  Tui.next_login_step t;
  Tui.settle t;
  Tui.next_login_step t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    model
    27 |
    28 | Current  openai/gpt-5.5 medium
    29 |
    30 | 1 providers · 1 models
    31 |
    32 |  ❯   browser-provider     primary
    33 |
    34 |
    35 | ○ Reasoning effort is not adjustable for this model
    36 |
    37 | Filter or set by name
    38 |
    39 |
    40 |
    |}]

(* A login on a workspace that already has a usable account does not hand off:
   the running model works, so opening the picker on every login would be noise.
   Here the launch provider is already signed in, so signing a second provider
   in settles on the auth record and leaves the surface on the auth panel. *)
let%expect_test "a login keeps a configured account on its surface" =
  let configured = provider "openai" in
  let newcomer = provider "extra-provider" in
  let login = Login.api_key () in
  let save provider ~key = Ok (Account.checked (credential provider key) ()) in
  let ready = Account.Discovery.Known (account configured) in
  let missing = Account.Discovery.Known (Account.missing ~provider:newcomer) in
  let saved = Account.Discovery.Known (account newcomer) in
  Tui.run ~name:"auth-no-handoff" ~size:(100, 40)
    ~providers:[ declaration configured login; declaration newcomer login ]
    ~readiness_responses:
      [ [ ready; missing ]; [ ready; missing ]; [ ready; saved ] ]
    ~save_api_key:save
  @@ fun t ->
  Tui.settle t;
  submit t "/login extra-provider";
  Tui.paste t "sk-extra-secret";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.finish_save_api_key t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   extra-provider
    29 |
    30 |   API key saved.
    31 |
    32 |   Account readiness: ready
    33 |
    34 |   Enter or Escape closes.
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}]

let%expect_test "logout stays pending until one typed completion and refresh" =
  let route = provider "logout-provider" in
  let login = Login.api_key () in
  let readiness = account route in
  let resolution_error =
    Declaration.Credential_error.Invalid_credential_text
      {
        source = Credential.Source.env "LOGOUT_PROVIDER_KEY";
        kind = Credential.Kind.Api_key;
      }
  in
  let logout provider =
    Ok
      {
        Account.Logout.current =
          Account.Discovery.Resolution_failed
            { provider; error = resolution_error };
        revocation = None;
      }
  in
  let discovered = Account.Discovery.Known readiness in
  let current =
    Account.Discovery.Resolution_failed
      { provider = route; error = resolution_error }
  in
  Tui.run ~name:"auth-logout" ~size:(100, 40)
    ~providers:[ declaration route login ]
    ~readiness_responses:[ [ discovered ]; [ discovered ]; [ current ] ]
    ~logout
  @@ fun t ->
  Tui.settle t;
  submit t "/logout logout-provider";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log out
    27 |
    28 |   Removing credentials…
    29 |
    30 |
    31 |
    32 |
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.finish_logout t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log out
    27 |
    28 |   logout-provider
    29 |
    30 |   Local credential removal settled.
    31 |
    32 |   Current account readiness: credential from env(LOGOUT_PROVIDER_KEY) has kind api_key with
    33 |   invalid text
    34 |
    35 |   Enter or Escape closes.
    36 |
    37 |
    38 |
    39 |
    40 |
    |}]

let%expect_test
    "account-readiness failure retries through the typed query boundary" =
  let route = provider "readiness-provider" in
  let login = Login.api_key () in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  let failure = unavailable "account discovery is temporarily unavailable" in
  Tui.run ~name:"auth-readiness-retry" ~size:(100, 40)
    ~providers:[ declaration route login ]
    ~readiness_results:[ Ok [ missing ]; Error failure; Ok [ missing ] ]
  @@ fun t ->
  Tui.settle t;
  submit t "/login readiness-provider";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   account discovery is temporarily unavailable
    29 |
    30 |   Enter retries. Escape closes.
    31 |
    32 |
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Enter an API key for readiness-provider
    29 |
    30 |   ❯ Paste or type an API key
    31 |
    32 |   The key is masked and leaves this panel only when saved.
    33 |
    34 |   Enter saves. Escape discards and goes back.
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}]

let%expect_test
    "multiple login methods preserve API-key discard back-navigation" =
  let route = provider "multi-provider" in
  let methods = [ browser_login (); Login.api_key () ] in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  Tui.run ~name:"auth-methods-discard" ~size:(100, 40)
    ~providers:[ declaration_with_logins route methods ]
    ~readiness_responses:[ [ missing ]; [ missing ] ]
  @@ fun t ->
  Tui.settle t;
  submit t "/login multi-provider";
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Choose how to log in to multi-provider.
    29 |
    30 |    ▶ 1  Browser
    31 |      Browser
    32 |      2  API key
    33 |      API key
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.paste t "sk-discard-this-secret";
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Enter an API key for multi-provider
    29 |
    30 |   ❯ ••••••••••••••••••••••
    31 |
    32 |   The key is masked and leaves this panel only when saved.
    33 |
    34 |   Enter saves. Escape discards and goes back.
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Choose how to log in to multi-provider.
    29 |
    30 |    ▶ 1  Browser
    31 |      Browser
    32 |      2  API key
    33 |      API key
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}];
  Tui.keys t Key.down;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |
    08 |
    09 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                      dev · openai/gpt-5.5 medium
    13 |
    14 |                ▎ welcome — and thanks for trying mentat this early.
    15 |                ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    log in
    27 |
    28 |   Enter an API key for multi-provider
    29 |
    30 |   ❯ Paste or type an API key
    31 |
    32 |   The key is masked and leaves this panel only when saved.
    33 |
    34 |   Enter saves. Escape discards and goes back.
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |
    |}]

let%expect_test "mixed account discovery retains healthy and failed rows" =
  let healthy = provider "healthy-provider" in
  let failed = provider "failed-provider" in
  let error =
    Declaration.Credential_error.Invalid_credential_text
      {
        source = Credential.Source.env "FAILED_PROVIDER_KEY";
        kind = Credential.Kind.Api_key;
      }
  in
  let mixed =
    [
      Account.Discovery.Known (account healthy);
      Account.Discovery.Resolution_failed { provider = failed; error };
    ]
  in
  Tui.run ~name:"auth-readiness" ~size:(100, 40)
    ~readiness_responses:[ mixed; mixed ]
  @@ fun t ->
  Tui.settle t;
  submit t "/status";
  Tui.print t;
  [%expect
    {|
    01 |  settings ────────────────────────────────────────────────────────────────────────────── 2 providers
    02 |
    03 | config  status  usage
    04 |
    05 | Runtime
    06 |   version         dev
    07 |   current model   openai/gpt-5.5 medium
    08 |   workspace       ~/mentat-tui-auth-rbe72fedf
    09 |   context window  128,000 tokens
    10 |   launch sandbox  danger-full-access
    11 |
    12 | Session
    13 |   No active session.
    14 |
    15 | Providers
    16 |   failed-provider   credential from env(FAILED_PROVIDER_KEY) has kind api_key with invalid text
    17 |   healthy-provider  ready
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 |
    26 |
    27 |
    28 |
    29 |
    30 |
    31 |
    32 |
    33 |
    34 |
    35 |
    36 |
    37 |
    38 |
    39 |
    40 |   tab page · esc back
    |}]

let%expect_test "API-key entry surfaces the terminal caret at the masked tail" =
  let route = provider "caret-provider" in
  let login = Login.api_key () in
  let missing = Account.Discovery.Known (Account.missing ~provider:route) in
  Tui.run ~name:"auth-caret" ~size:(100, 40)
    ~providers:[ declaration route login ]
    ~readiness_responses:[ [ missing ]; [ missing ] ]
  @@ fun t ->
  Tui.settle t;
  submit t "/login caret-provider";
  Tui.settle t;
  let caret t =
    match Tui.cursor t with
    | None -> print_string "hidden"
    | Some (row, col) -> Printf.printf "row %d col %d" row col
  in
  caret t;
  [%expect {| row 30 col 5 |}];
  Tui.keys t "abc";
  Tui.settle t;
  caret t;
  [%expect {| row 30 col 8 |}]
