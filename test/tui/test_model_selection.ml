(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Tui_harness
module Session = Mentat_session
module Protocol = Mentat_protocol
module Reasoning = Mentat_llm.Request.Options.Reasoning_effort
module Provider = Mentat_llm.Provider
module Account = Mentat_provider.Account
module Auth = Mentat_provider.Auth
module Catalog_model = Mentat_provider.Model
module Credential = Mentat_provider.Credential
module Credential_error = Mentat_provider.Credential_error

let failf format = Format.kasprintf failwith format

let selector text =
  match Mentat_provider.Selector.of_string text with
  | Ok selector -> selector
  | Error error ->
      failf "invalid test selector: %a" Mentat_provider.Selector.Error.pp error

let model ~provider id =
  Mentat_llm.Model.make
    ~provider:(Mentat_llm.Provider.make provider)
    ~api:(Mentat_llm.Model.Api.make "responses")
    ~id

let unavailable message =
  Protocol.Error.Unavailable (Mentat_diagnostic.of_text message)

let submit t text =
  Tui.paste t text;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let open_model t = submit t "/model"

let select_model t text =
  open_model t;
  Tui.paste t text;
  Tui.settle t;
  Tui.enter t;
  Tui.settle t

let usage = Mentat_llm.Usage.make ~input:1_280 ~output:20 ()
let provider = Provider.make

let credential route =
  Credential.make ~provider:route ~source:Credential.Source.process
    (Credential.Secret.api_key "sk-model-panel-test")

let catalog_model ?display_name ?status ?context_window ?default_reasoning
    ?supported_reasoning route id =
  Catalog_model.make
    (model ~provider:(Provider.id route) id)
    ?display_name ?status ?context_window ?default_reasoning
    ?supported_reasoning ()

let required_auth name = Auth.make ~env:[ Auth.Env.api_key name ] ()

let rich_model_readiness () =
  let anthropic = provider "anthropic" in
  let openai = provider "openai" in
  let local = provider "local" in
  let broken = provider "broken-provider" in
  let anthropic_models =
    [
      catalog_model ~display_name:"Claude Sonnet 4.5" ~context_window:200_000
        anthropic "claude-sonnet-4-5";
      catalog_model ~display_name:"Claude Opus 3" ~context_window:200_000
        ~status:Catalog_model.Deprecated anthropic "claude-opus-3";
      catalog_model ~display_name:"Claude Instant"
        ~status:(Catalog_model.Unavailable "retired by provider") anthropic
        "claude-instant";
    ]
  in
  let openai_models =
    [
      catalog_model ~display_name:"GPT-5.5" ~context_window:1_050_000 openai
        "gpt-5.5";
      catalog_model ~display_name:"GPT-5.5 Mini" ~context_window:400_000
        ~status:Catalog_model.Preview openai "gpt-5.5-mini";
      catalog_model ~display_name:"GPT-5 Pro" ~context_window:400_000 openai
        "gpt-5-pro";
    ]
  in
  let local_models =
    [
      catalog_model ~display_name:"Llama 4 Scout" ~context_window:128_000 local
        "llama-4-scout";
    ]
  in
  let broken_models =
    [ catalog_model ~display_name:"Broken Model" broken "broken-model" ]
  in
  let providers =
    [
      Mentat_provider.make anthropic ~display_name:"Anthropic"
        ~auth:(required_auth "ANTHROPIC_API_KEY")
        anthropic_models;
      Mentat_provider.make openai ~display_name:"OpenAI"
        ~auth:(required_auth "OPENAI_API_KEY")
        openai_models;
      Mentat_provider.make local ~display_name:"Local Models" local_models;
      Mentat_provider.make broken ~display_name:"Broken Provider"
        ~auth:(required_auth "BROKEN_PROVIDER_TOKEN")
        broken_models;
    ]
  in
  let resolution_error =
    Credential_error.Unsupported_credential_kind
      {
        source = Credential.Source.env "BROKEN_PROVIDER_TOKEN";
        kind = Credential.Kind.Bearer;
        accepted = [ Credential.Kind.Api_key ];
      }
  in
  let discoveries =
    [
      Account.Discovery.Known (Account.missing ~provider:anthropic);
      Account.Discovery.Known
        (Account.checked (credential openai)
           ~models:[ "gpt-5.5"; "gpt-5.5-mini" ]
           ());
      Account.Discovery.Known (Account.missing ~provider:local);
      Account.Discovery.Resolution_failed
        { provider = broken; error = resolution_error };
    ]
  in
  Tui.Model_readiness.of_providers providers ~discoveries

let empty_model_readiness () =
  let empty = provider "empty" in
  let providers = [ Mentat_provider.make empty ~display_name:"Empty" [] ] in
  let discoveries =
    [ Account.Discovery.Known (Account.missing ~provider:empty) ]
  in
  Tui.Model_readiness.of_providers providers ~discoveries

(* One actionable model whose owner declaration supports several reasoning
   levels, so the effort line under the table adjusts with [←]/[→]. Its account
   is checked and lists the id, so the row is eligible, available, and route
   ready — the effort control is never gated behind a blocked route. *)
let effort_model_readiness () =
  let openai = provider "openai" in
  let models =
    [
      catalog_model ~display_name:"GPT-5.5" ~default_reasoning:Reasoning.Medium
        ~supported_reasoning:[ Reasoning.Low; Reasoning.Medium; Reasoning.High ]
        openai "gpt-5.5";
    ]
  in
  let providers =
    [
      Mentat_provider.make openai ~display_name:"OpenAI"
        ~auth:(required_auth "OPENAI_API_KEY")
        models;
    ]
  in
  let discoveries =
    [
      Account.Discovery.Known
        (Account.checked (credential openai) ~models:[ "gpt-5.5" ] ());
    ]
  in
  Tui.Model_readiness.of_providers providers ~discoveries

let discovery_rows () =
  [
    Account.Discovery.Known (Account.missing ~provider:(provider "openai"));
    Account.Discovery.Known
      (Account.present (credential (provider "anthropic")));
  ]

(* Every assertion below is a complete numbered terminal frame. The fixtures
   construct only provider-owner values; the panel receives no frontend catalog
   or model mirror. *)

let%expect_test
    "live model catalog renders owner facts and refuses a blocked route" =
  Tui.run ~name:"model-discovery" ~size:(100, 48)
    ~model_readiness_results:[ Ok (rich_model_readiness ()) ]
  @@ fun t ->
  open_model t;
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
    11 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    12 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    13 |
    14 |                                      dev · openai/gpt-5.5 medium
    15 |
    16 |                ▎ welcome — and thanks for trying mentat this early.
    17 |                ▎ it's experimental: sessions and config may change without migration.
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
    29 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    30 |    model
    31 |
    32 | Current  openai/gpt-5.5 medium
    33 |
    34 | 4 providers · 7 models
    35 |
    36 |      Anthropic           Claude Sonnet 4.5             sign-in required                 200K       │
    37 |                          Claude Opus 3                 deprecated                       200K       │
    38 |  ❯   OpenAI              GPT-5.5                                                       1.05M   ✓   │
    39 |                          GPT-5.5 Mini                  preview                          400K       │
    40 |                          GPT-5 Pro                     unavailable                      400K       ↓
    41 |
    42 |
    43 | ○ Reasoning effort is not adjustable for this model
    44 |
    45 | Filter or set by name
    46 |
    47 |
    48 |
    |}];
  Tui.paste t "broken";
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
    11 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    12 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    13 |
    14 |                                      dev · openai/gpt-5.5 medium
    15 |
    16 |                ▎ welcome — and thanks for trying mentat this early.
    17 |                ▎ it's experimental: sessions and config may change without migration.
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
    29 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    30 |    model   broken
    31 |
    32 | Current  openai/gpt-5.5 medium
    33 |
    34 | 1 of 7 models
    35 |
    36 |  ❯   Broken Provider      Broken Model                   credential · credential f...
    37 |
    38 |
    39 |
    40 |
    41 |
    42 |
    43 | ○ Reasoning effort is not adjustable for this model
    44 |
    45 | Filter or set by name
    46 |
    47 |
    48 |
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
    09 |
    10 |
    11 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    12 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    13 |
    14 |                                      dev · openai/gpt-5.5 medium
    15 |
    16 |                ▎ welcome — and thanks for trying mentat this early.
    17 |                ▎ it's experimental: sessions and config may change without migration.
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
    29 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    30 |    model   broken
    31 |
    32 | Current  openai/gpt-5.5 medium
    33 |
    34 | 1 of 7 models
    35 |
    36 |  ❯   Broken Provider      Broken Model                   credential · credential f...
    37 |
    38 |
    39 |
    40 | ○ Reasoning effort is not adjustable for this model
    41 |
    42 | broken-provider/broken-model cannot be selected · credential · credential from env(
    43 | BROKEN_PROVIDER_TOKEN) has kind bearer, which the provider does not accept (accepted: api_key)
    44 |
    45 | Filter or set by name
    46 |
    47 |
    48 |
    |}]

let readiness_page () =
  let routes =
    provider "openai"
    :: List.init 12 (fun index ->
        provider (Printf.sprintf "provider-%02d" (index + 1)))
  in
  let providers =
    List.map
      (fun route ->
        let id = Provider.id route ^ "-model" in
        Mentat_provider.make route
          [
            catalog_model ~display_name:("Model " ^ Provider.id route) route id;
          ])
      routes
  in
  let discoveries =
    List.map
      (fun route -> Account.Discovery.Known (Account.missing ~provider:route))
      routes
  in
  Tui.Model_readiness.of_providers providers ~discoveries

let%expect_test "model catalog pages within Mosaic's measured table body" =
  Tui.run ~name:"model-readiness-page" ~size:(80, 40)
    ~model_readiness_results:[ Ok (readiness_page ()) ]
  @@ fun t ->
  open_model t;
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
    09 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                            dev · openai/gpt-5.5 medium
    13 |
    14 |      ▎ welcome — and thanks for trying mentat this early.
    15 |      ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    model
    27 |
    28 | Current  openai/gpt-5.5 medium
    29 |
    30 | 13 providers · 13 models
    31 |
    32 |  ❯   openai          Model openai                                              ↓
    33 |
    34 |
    35 | ○ Reasoning effort is not adjustable for this model
    36 |
    37 | Filter or set by name
    38 |
    39 |
    40 |
    |}];
  Tui.keys t Key.page_down;
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
    09 |                           █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                           █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                            dev · openai/gpt-5.5 medium
    13 |
    14 |      ▎ welcome — and thanks for trying mentat this early.
    15 |      ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    26 |    model
    27 |
    28 | Current  openai/gpt-5.5 medium
    29 |
    30 | 13 providers · 13 models
    31 |
    32 |  ❯   provider-01     Model provider-01                                         ↕
    33 |
    34 |
    35 | ○ Reasoning effort is not adjustable for this model
    36 |
    37 | Filter or set by name
    38 |
    39 |
    40 |
    |}]

let%expect_test "model catalog loading keeps set-by-name visible" =
  Tui.run ~name:"model-catalog-loading" ~size:(96, 30)
    ~hold_settings_queries:true
    ~model_readiness_results:[ Ok (rich_model_readiness ()) ]
  @@ fun t ->
  open_model t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |                                   █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    08 |                                   █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    09 |
    10 |                                    dev · openai/gpt-5.5 medium
    11 |
    12 |              ▎ welcome — and thanks for trying mentat this early.
    13 |              ▎ it's experimental: sessions and config may change without migration.
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    21 |    model
    22 |
    23 | ⠋ loading the live model catalog… You can set a model by name below.
    24 |
    25 |
    26 |
    27 | Filter or set by name
    28 |
    29 |
    30 |
    |}];
  Tui.finish_settings_queries t;
  Tui.settle t

let%expect_test "model catalog failure keeps set-by-name available" =
  Tui.run ~name:"model-catalog-unavailable" ~size:(96, 30)
    ~model_readiness_results:
      [ Error (unavailable "live model catalog is temporarily unavailable") ]
  @@ fun t ->
  open_model t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |
    06 |
    07 |                                   █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    08 |                                   █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    09 |
    10 |                                    dev · openai/gpt-5.5 medium
    11 |
    12 |              ▎ welcome — and thanks for trying mentat this early.
    13 |              ▎ it's experimental: sessions and config may change without migration.
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    21 |    model
    22 |
    23 | ! live model catalog is temporarily unavailable · set by name or press ctrl+r to retry
    24 |
    25 |
    26 |
    27 | Filter or set by name
    28 |
    29 |
    30 |
    |}]

let%expect_test
    "filter navigation activates the exact actionable owner selector" =
  let selected = selector "openai/gpt-5.5-mini" in
  let setup =
    Tui.Turn_script.complete ~prompt:"start model selection" "Ready."
  in
  Tui.run ~name:"model-filter-activate" ~size:(100, 36)
    ~model_readiness_results:[ Ok (rich_model_readiness ()) ]
    ~model_selections:[ selected ] ~turns:[ setup ]
  @@ fun t ->
  submit t "start model selection";
  Tui.finish_turn t;
  Tui.settle t;
  open_model t;
  Tui.paste t "gpt-5";
  Tui.settle t;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-filter-15329cde
    04 |
    05 | ❯ start model selection
    06 |
    07 | ⏺ Ready.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    20 |    model   gpt-5
    21 |
    22 | Current  openai/gpt-5.5
    23 |
    24 | 3 of 7 models
    25 |
    26 |      OpenAI               GPT-5.5                                                         1.05M   ✓
    27 |  ❯                        GPT-5.5 Mini                   preview                           400K
    28 |                           GPT-5 Pro                      unavailable                       400K
    29 |
    30 | ○ Reasoning effort is not adjustable for this model
    31 |
    32 | Filter or set by name
    33 |
    34 | ❯ gpt-5
    35 |
    36 |   ↑↓/pgup/pgdn bro… · ←→ effo… · ↵ choose/… · ctrl+r refre… · type filter or provider/… · esc clo…
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.finish_model_selection t (Ok ());
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-filter-15329cde
    04 |
    05 | ❯ start model selection
    06 |
    07 | ⏺ Ready.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
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
    33 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    34 | ❯ message mentat
    35 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    36 |   ! not logged in · /login · ~/mentat-tui-model-fi… · openai/gpt-5.5-m… · ! full access ? for sho…
    |}]

(* The effort control (05-overlays-pickers.md §Model picker) reads the selected
   row model's owner-declared levels. The frames are promoted once the shared
   harness rebuilds; the assertions exercise the default level, a step up, a
   wrap back down through the default to the low end, and activation carrying the
   chosen effort into the selection. *)
let%expect_test "effort control walks the selected model's reasoning levels" =
  let selected = selector "openai/gpt-5.5" in
  let setup =
    Tui.Turn_script.complete ~prompt:"start model selection" "Ready."
  in
  Tui.run ~name:"model-effort" ~size:(100, 30)
    ~model_readiness_results:[ Ok (effort_model_readiness ()) ]
    ~model_selections:[ selected ] ~turns:[ setup ]
  @@ fun t ->
  submit t "start model selection";
  Tui.finish_turn t;
  Tui.settle t;
  open_model t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-mode562a1257
    04 |
    05 | ❯ start model selection
    06 |
    07 | ⏺ Ready.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    17 |    model
    18 |
    19 | 1 providers · 1 models
    20 |
    21 |  ❯   OpenAI               GPT-5.5                                                                 ✓
    22 |
    23 |
    24 | ◐ Medium effort (default) ← → adjust
    25 |
    26 | Filter or set by name
    27 |
    28 | ❯ provider/model
    29 |
    30 |   ↑↓/pgup/pgdn bro… · ←→ effo… · ↵ choose/… · ctrl+r refre… · type filter or provider/… · esc clo…
    |}];
  (* Right steps up from the model default (medium) to high. *)
  Tui.keys t Key.right;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-mode562a1257
    04 |
    05 | ❯ start model selection
    06 |
    07 | ⏺ Ready.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    17 |    model
    18 |
    19 | 1 providers · 1 models
    20 |
    21 |  ❯   OpenAI               GPT-5.5                                                                 ✓
    22 |
    23 |
    24 | ● High effort ← → adjust
    25 |
    26 | Filter or set by name
    27 |
    28 | ❯ provider/model
    29 |
    30 |   ↑↓/pgup/pgdn bro… · ←→ effo… · ↵ choose/… · ctrl+r refre… · type filter or provider/… · esc clo…
    |}];
  (* Two lefts walk high → medium (the default) → low. *)
  Tui.keys t (Key.left ^ Key.left);
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-mode562a1257
    04 |
    05 | ❯ start model selection
    06 |
    07 | ⏺ Ready.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    17 |    model
    18 |
    19 | 1 providers · 1 models
    20 |
    21 |  ❯   OpenAI               GPT-5.5                                                                 ✓
    22 |
    23 |
    24 | ○ Low effort ← → adjust
    25 |
    26 | Filter or set by name
    27 |
    28 | ❯ provider/model
    29 |
    30 |   ↑↓/pgup/pgdn bro… · ←→ effo… · ↵ choose/… · ctrl+r refre… · type filter or provider/… · esc clo…
    |}];
  (* Enter activates the exact owner selector with the chosen effort. *)
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-mode562a1257
    04 |
    05 | ❯ start model selection
    06 |
    07 | ⏺ Ready.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |
    25 |
    26 |
    27 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    28 | ❯ message mentat
    29 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    30 |   ! not logged in · /login · ~/mentat-tui-mode562a… · openai/gpt-5.5 · ! full access ? for shortc…
    |}]

let%expect_test "refresh keeps rows visible and retains a structured failure" =
  Tui.run ~name:"model-refresh-retained" ~size:(108, 38)
    ~hold_settings_queries:true
    ~model_readiness_results:
      [
        Ok (rich_model_readiness ());
        Error (unavailable "refresh could not reach the provider catalog");
      ]
  @@ fun t ->
  open_model t;
  Tui.finish_settings_queries t;
  Tui.settle t;
  Tui.keys t Key.ctrl_r;
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
    09 |                                         █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                         █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                          dev · openai/gpt-5.5 medium
    13 |
    14 |                    ▎ welcome — and thanks for trying mentat this early.
    15 |                    ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    25 |    model
    26 |
    27 | Current  openai/gpt-5.5 medium
    28 |
    29 | 4 providers · 7 models
    30 |
    31 | ⠋ refreshing the live model catalog…
    32 |
    33 |
    34 | ○ Reasoning effort is not adjustable for this model
    35 |
    36 | Filter or set by name
    37 |
    38 |
    |}];
  Tui.finish_settings_queries t;
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
    09 |                                         █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    10 |                                         █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    11 |
    12 |                                          dev · openai/gpt-5.5 medium
    13 |
    14 |                    ▎ welcome — and thanks for trying mentat this early.
    15 |                    ▎ it's experimental: sessions and config may change without migration.
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    25 |    model
    26 |
    27 | Current  openai/gpt-5.5 medium
    28 |
    29 | 4 providers · 7 models
    30 |
    31 | ! refresh could not reach the provider catalog
    32 |
    33 |
    34 | ○ Reasoning effort is not adjustable for this model
    35 |
    36 | Filter or set by name
    37 |
    38 |
    |}]

let%expect_test "Mosaic allocates the complete catalog in narrow short frames" =
  Tui.run ~name:"model-narrow-short" ~size:(58, 18)
    ~model_readiness_results:[ Ok (rich_model_readiness ()) ]
  @@ fun t ->
  open_model t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |                █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    05 |                █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    06 |
    07 |                 dev · openai/gpt-5.5 medium
    08 |
    09 | ▎ welcome — and thanks for trying mentat this early.
    10 | ▎ it's experimental: sessions and config may change withou
    11 |
    12 |
    13 |
    14 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    15 |    model
    16 |
    17 | 4 providers · 7 models
    18 |
    |}];
  Tui.resize t ~width:42 ~height:14;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |        █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    04 |        █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    05 |
    06 |         dev · openai/gpt-5.5 medium
    07 |
    08 | ▎ welcome — and thanks for trying mentat t
    09 | ▎ it's experimental: sessions and config m
    10 |
    11 |
    12 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    13 |    model
    14 |
    |}]

let%expect_test
    "a provider route with no models renders an honest empty catalog" =
  Tui.run ~name:"model-empty-catalog" ~size:(76, 24)
    ~model_readiness_results:[ Ok (empty_model_readiness ()) ]
  @@ fun t ->
  open_model t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |                         █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    06 |                         █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    07 |
    08 |                          dev · openai/gpt-5.5 medium
    09 |
    10 |    ▎ welcome — and thanks for trying mentat this early.
    11 |    ▎ it's experimental: sessions and config may change without migration.
    12 |
    13 |
    14 |
    15 |
    16 |
    17 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    18 |    model
    19 |
    20 | 1 providers · 0 models
    21 |
    22 | No models are currently reported by the catalog.
    23 |
    24 |
    |}]

let%expect_test "escape from a settings-opened model panel restores settings" =
  let setup = Tui.Turn_script.complete ~prompt:"start settings" "Ready." in
  Tui.run ~name:"model-settings-return" ~size:(100, 24)
    ~readiness:(discovery_rows ()) ~turns:[ setup ]
  @@ fun t ->
  submit t "start settings";
  Tui.finish_turn t;
  Tui.settle t;
  submit t "/config";
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-setting87055c84
    04 |
    05 | ❯ start settings
    06 |
    07 | ⏺ Ready.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    model
    15 |
    16 | ! model readiness unavailable in the visual harness · set by name or press ctrl+r to retry
    17 |
    18 |
    19 |
    20 | Filter or set by name
    21 |
    22 | ❯ provider/model
    23 |
    24 |   ↵ choose/set · ctrl+r refresh · type filter or provider/model · esc close
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |  settings ────────────────────────────────────────────────────────────────────────────── unavailable
    02 |
    03 | config  status  usage
    04 |
    05 | Session controls below apply to the next turn only.
    06 |
    07 | !  configuration unavailable in the visual harness
    08 |
    09 |      setting                    value                                    source
    10 |  ›   model                      openai/gpt-5.5                           next turn
    11 |      permission review          choose a next-turn request               next turn
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↑↓ move · ←→ change · ↵ apply · tab page · / filter · esc back
    |}]

let%expect_test "model panel preserves a short transcript when it closes" =
  let turn =
    Tui.Turn_script.complete ~prompt:"show the model panel"
      "The complete answer remains visible around the panel."
  in
  Tui.run ~name:"model-short-transcript" ~size:(80, 24)
    ~readiness:(discovery_rows ()) ~turns:[ turn ]
  @@ fun t ->
  submit t "show the model panel";
  Tui.finish_turn t;
  Tui.settle t;
  open_model t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-short-trdfc4b587
    04 |
    05 | ❯ show the model panel
    06 |
    07 | ⏺ The complete answer remains visible around the panel.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    model
    15 |
    16 | ! model readiness unavailable in the visual harness · set by name or press
    17 | ctrl+r to retry
    18 |
    19 |
    20 | Filter or set by name
    21 |
    22 | ❯ provider/model
    23 |
    24 |   ↵ choose/set · ctrl+r refresh · type filter or provider/model · esc close
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-short-trdfc4b587
    04 |
    05 | ❯ show the model panel
    06 |
    07 | ⏺ The complete answer remains visible around the panel.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────
    24 |   ~/mentat-tui-model-short-trdf… · openai/gpt-5… · ! full access ? for shortc…
    |}]

let overflowing_answer =
  List.init 20 (fun index ->
      Printf.sprintf
        "Tail marker %02d keeps the overflowing transcript anchored." (index + 1))
  |> String.concat "\n\n"

let%expect_test "wide model panel preserves the overflowing transcript tail" =
  let turn =
    Tui.Turn_script.complete ~prompt:"show the wide model panel"
      overflowing_answer
  in
  Tui.run ~name:"model-wide-transcript" ~size:(120, 32)
    ~readiness:(discovery_rows ()) ~turns:[ turn ]
  @@ fun t ->
  submit t "show the wide model panel";
  Tui.finish_turn t;
  Tui.settle t;
  open_model t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |   Tail marker 13 keeps the overflowing transcript anchored.
    03 |
    04 |   Tail marker 14 keeps the overflowing transcript anchored.
    05 |
    06 |   Tail marker 15 keeps the overflowing transcript anchored.
    07 |
    08 |   Tail marker 16 keeps the overflowing transcript anchored.
    09 |
    10 |   Tail marker 17 keeps the overflowing transcript anchored.
    11 |
    12 |   Tail marker 18 keeps the overflowing transcript anchored.
    13 |
    14 |   Tail marker 19 keeps the overflowing transcript anchored.
    15 |
    16 |   Tail marker 20 keeps the overflowing transcript anchored.
    17 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    18 |    model
    19 |
    20 | ! model readiness unavailable in the visual harness · set by name or press ctrl+r to retry
    21 |
    22 |
    23 |
    24 |
    25 |
    26 |
    27 |
    28 | Filter or set by name
    29 |
    30 | ❯ provider/model
    31 |
    32 |   ↵ choose/set · ctrl+r refresh · type filter or provider/model · esc close
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |   Tail marker 07 keeps the overflowing transcript anchored.
    02 |
    03 |   Tail marker 08 keeps the overflowing transcript anchored.
    04 |
    05 |   Tail marker 09 keeps the overflowing transcript anchored.
    06 |
    07 |   Tail marker 10 keeps the overflowing transcript anchored.
    08 |
    09 |   Tail marker 11 keeps the overflowing transcript anchored.
    10 |
    11 |   Tail marker 12 keeps the overflowing transcript anchored.
    12 |
    13 |   Tail marker 13 keeps the overflowing transcript anchored.
    14 |
    15 |   Tail marker 14 keeps the overflowing transcript anchored.
    16 |
    17 |   Tail marker 15 keeps the overflowing transcript anchored.
    18 |
    19 |   Tail marker 16 keeps the overflowing transcript anchored.
    20 |
    21 |   Tail marker 17 keeps the overflowing transcript anchored.
    22 |
    23 |   Tail marker 18 keeps the overflowing transcript anchored.
    24 |
    25 |   Tail marker 19 keeps the overflowing transcript anchored.
    26 |
    27 |   Tail marker 20 keeps the overflowing transcript anchored.
    28 |
    29 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    30 | ❯ message mentat
    31 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    32 |   ~/mentat-tui-model-wide-tr413c4364 · openai/gpt-5.5 · ! full access                                  ? for shortcuts
    |}]

(* Correlation laws are asserted through complete terminal frames. Inactive
   completions are delivered through held client operations after a real
   session replacement, without synthesizing duplicate callback delivery. *)

let%expect_test
    "model selection is single-flight and facts own effective presentation" =
  let selected_text = "anthropic/claude-sonnet-4-5" in
  let selected = selector selected_text in
  let setup =
    Tui.Turn_script.complete ~prompt:"start session"
      ~reasoning_effort:Reasoning.Medium ~usage "Ready."
  in
  let changed =
    Tui.Turn_script.complete ~prompt:"use the selected model"
      ~model:(model ~provider:"anthropic" "claude-sonnet-4-5")
      ~reasoning_effort:Reasoning.High ~usage "Selected."
  in
  Tui.run ~name:"model-selection" ~size:(120, 24) ~model_selections:[ selected ]
    ~turns:[ setup; changed ]
  @@ fun t ->
  submit t "start session";
  Tui.finish_turn t;
  Tui.settle t;
  submit t "/status";
  Tui.print t;
  [%expect
    {|
    01 |  settings ────────────────────────────────────────────────────────────────────────────────────────────────── 1 providers
    02 |
    03 | config  status  usage
    04 |
    05 | Runtime
    06 |   version         dev
    07 |   current model   openai/gpt-5.5 medium
    08 |   workspace       ~/mentat-tui-model-sec766f65
    09 |   context window  128,000 tokens
    10 |   launch sandbox  danger-full-access
    11 |
    12 | Session
    13 |   id            session-visual-00001
    14 |   lifecycle     active
    15 |   phase         idle
    16 |   active model  openai/gpt-5.5
    17 |   waiting       none
    18 |   workflow      build
    19 |   last outcome  completed
    20 |
    21 | Providers
    22 |   openai  missing
    23 |
    24 |   tab page · esc back
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;

  select_model t selected_text;
  Tui.print t;
  [%expect
    {|
    01 |                                                                                 │ context
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   1,300 tokens
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-sec766f65                    │   1% used
    04 |                                                                                 │
    05 | ❯ start session                                                                 │
    06 |                                                                                 │
    07 | ⏺ Ready.                                                                        │
    08 |                                                                                 │
    09 |                                                                                 │
    10 |                                                                                 │
    11 |                                                                                 │
    12 |                                                                                 │
    13 |                                                                                 │
    14 |                                                                                 │
    15 |                                                                                 │
    16 |                                                                                 │
    17 |                                                                                 │
    18 |                                                                                 │
    19 |                                                                                 │
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-model-sec76… · openai/gpt-5.5 medium · ! full access · 1.3K … ? for shortcu…
    |}];

  select_model t "openai/gpt-6";
  Tui.print t;
  [%expect
    {|
    01 |                                                                                 │ context
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   1,300 tokens
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-sec766f65                    │   1% used
    04 |                                                                                 │
    05 | ❯ start session                                                                 │
    06 |                                                                                 │
    07 | ⏺ Ready.                                                                        │
    08 |                                                                                 │
    09 |                                                                                 │
    10 |                                                                                 │
    11 |                                                                                 │
    12 |                                                                                 │
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    model   openai/gpt-6
    15 |
    16 | ! model readiness unavailable in the visual harness · set by name or press ctrl+r to retry
    17 |
    18 | model selection is still being applied
    19 |
    20 | Filter or set by name
    21 |
    22 | ❯ openai/gpt-6
    23 |
    24 |   ↵ choose/set · ctrl+r refresh · type filter or provider/model · esc close
    |}];
  Tui.keys t Key.escape;
  Tui.settle t;

  Tui.finish_model_selection t (Ok ());
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |                                                                                 │ context
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   1,300 tokens
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-sec766f65                    │
    04 |                                                                                 │
    05 | ❯ start session                                                                 │
    06 |                                                                                 │
    07 | ⏺ Ready.                                                                        │
    08 |                                                                                 │
    09 |                                                                                 │
    10 |                                                                                 │
    11 |                                                                                 │
    12 |                                                                                 │
    13 |                                                                                 │
    14 |                                                                                 │
    15 |                                                                                 │
    16 |                                                                                 │
    17 |                                                                                 │
    18 |                                                                                 │
    19 |                                                                                 │
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-model-sec7… · anthropic/claude-sonnet-4… · ! full access · 1.… ? for shortc…
    |}];

  submit t "use the selected model";
  Tui.print t;
  [%expect
    {|
    01 |                                                                                 │ context
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium                     │   1,300 tokens
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-sec766f65                    │
    04 |                                                                                 │
    05 | ❯ start session                                                                 │
    06 |                                                                                 │
    07 | ⏺ Ready.                                                                        │
    08 |                                                                                 │
    09 | ❯ use the selected model                                                        │
    10 |                                                                                 │
    11 | ⠋ Working… (0s · ↓ 20 tokens · esc to interrupt)                                │
    12 |                                                                                 │
    13 |                                                                                 │
    14 |                                                                                 │
    15 |                                                                                 │
    16 |                                                                                 │
    17 |                                                                                 │
    18 |                                                                                 │
    19 |                                                                                 │
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ queue a message — sends after this turn
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-model-se… · anthropic/claude-sonnet-4-5 h… · ! full access · 1… ? for short…
    |}];
  submit t "/status";
  Tui.print t;
  [%expect
    {|
    01 |  settings ────────────────────────────────────────────────────────────────────────────────────────────────── 1 providers
    02 |
    03 | config  status  usage
    04 |
    05 | Runtime
    06 |   version         dev
    07 |   current model   anthropic/claude-sonnet-4-5 high
    08 |   workspace       ~/mentat-tui-model-sec766f65
    09 |   context window  —
    10 |   launch sandbox  danger-full-access
    11 |
    12 | Session
    13 |   id            session-visual-00001
    14 |   lifecycle     active
    15 |   phase         waiting
    16 |   active model  anthropic/claude-sonnet-4-5
    17 |   waiting       awaiting_provider
    18 |   workflow      build
    19 |   last outcome  completed
    20 |
    21 | Providers
    22 |   openai  missing
    23 |
    24 |   tab page · esc back
    |}]

let completed_session ~id ~turn_id ~input ~created_at project =
  let id = Session.Id.of_string id in
  let session =
    Session.create ~id
      ~cwd:(Lpath.Abs.of_string_exn (Project.root project))
      ~created_at:(Session.Time.of_unix_ms created_at)
      ()
  in
  let contract =
    Session.Contract.make ~mode:Session.Contract.Mode.Build
      ~model:(model ~provider:"openai" "gpt-5.5")
      ~options:
        (Mentat_llm.Request.Options.make ~reasoning_effort:Reasoning.Medium ())
      ~declarations:[] ~policy:Mentat_permission.Policy.default
      ~review:Mentat_permission.Review_behavior.Enforce
      ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
      ()
  in
  let turn =
    Session.Turn.make
      ~id:(Session.Turn.Id.of_string turn_id)
      ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text input)
      ~max_steps:8 ~contract ()
  in
  match
    Session.append_all
      [
        Session.Event.turn_started turn;
        Session.Event.turn_finished ~turn:(Session.Turn.id turn)
          Session.Turn.Outcome.completed;
      ]
      session
  with
  | Ok session -> session
  | Error error ->
      failf "invalid saved session: %s" (Session.Error.message error)

let%expect_test "model completions render only for their exact active session" =
  let rejected_text = "openai/gpt-5.5-mini" in
  let inactive_failure_text = "anthropic/claude-haiku-4-5" in
  let inactive_success_text = "anthropic/claude-opus-4-5" in
  let after_success_text = "openai/gpt-5.5" in
  let setup = Tui.Turn_script.complete ~prompt:"start" "Ready." in
  Tui.run ~name:"model-correlation" ~size:(100, 24)
    ~sessions:(fun project ->
      [
        completed_session ~id:"session-newer" ~turn_id:"turn-newer"
          ~input:"newer saved session" ~created_at:900_000L project;
        completed_session ~id:"session-older" ~turn_id:"turn-older"
          ~input:"older saved session" ~created_at:800_000L project;
      ])
    ~model_selections:
      [
        selector rejected_text;
        selector inactive_failure_text;
        selector inactive_success_text;
        selector after_success_text;
      ]
    ~turns:[ setup ]
  @@ fun t ->
  submit t "start";
  Tui.finish_turn t;
  Tui.settle t;

  select_model t rejected_text;
  Tui.finish_model_selection t (Error (unavailable "model write rejected"));
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-cora3da6179
    04 |
    05 | ❯ start
    06 |
    07 | ⏺ Ready.
    08 |
    09 | ✗ model write rejected
    10 |   retry when ready
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   model write rejected
    |}];

  select_model t inactive_failure_text;
  submit t "/sessions";
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-cora3da6179
    04 |
    05 | ❯ start
    06 |
    07 | ⏺ Ready.
    08 |
    09 | ✗ model write rejected
    10 |   retry when ready
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    sessions
    15 |
    16 |      session-visual-00001                                                         idle     just now
    17 |  ❯   session-newer                                                                idle       1m ago
    18 |      session-older                                                                idle       3m ago
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · tab browse · type to filter · ↑↓ select · esc close
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.finish_model_selection t
    (Error (unavailable "inactive completion must be invisible"));
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-cora3da6179
    04 |
    05 | ❯ newer saved session
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-model-… · openai/gpt-5.5 med… · ! full access ? for sho…
    |}];

  (* A later selection can escape only if the invisible failure retired the
     earlier request. Hold it, switch from the newer saved session to the
     older one, and settle its success while its target is inactive. *)
  select_model t inactive_success_text;
  submit t "/sessions";
  Tui.keys t Key.down;
  Tui.keys t Key.down;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-cora3da6179
    04 |
    05 | ❯ newer saved session
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 | ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
    14 |    sessions
    15 |
    16 |      session-visual-00001                                                         idle     just now
    17 |      session-newer                                                                idle       1m ago
    18 |  ❯   session-older                                                                idle       3m ago
    19 |
    20 |
    21 |
    22 |
    23 |
    24 |   ↵ resume · tab browse · type to filter · ↑↓ select · esc close
    |}];
  Tui.enter t;
  Tui.settle t;
  Tui.finish_model_selection t (Ok ());
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-cora3da6179
    04 |
    05 | ❯ older saved session
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-model-… · openai/gpt-5.5 med… · ! full access ? for sho…
    |}];

  (* A final selection proves that inactive success retired its request and
     reopened the single flight. *)
  select_model t after_success_text;
  Tui.finish_model_selection t (Ok ());
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-cora3da6179
    04 |
    05 | ❯ older saved session
    06 |
    07 |
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-model-cora… · openai/gpt-5… · ! full access ? for short…
    |}]

let session_with_child project =
  let id = Session.Id.of_string "session-parent" in
  let session =
    Session.create ~id
      ~cwd:(Lpath.Abs.of_string_exn (Project.root project))
      ~created_at:(Session.Time.of_unix_ms 1_000_000L)
      ()
  in
  let contract =
    Session.Contract.make ~mode:Session.Contract.Mode.Build
      ~model:(model ~provider:"openai" "gpt-5.5")
      ~declarations:[] ~policy:Mentat_permission.Policy.default
      ~review:Mentat_permission.Review_behavior.Enforce
      ~sandbox:(Mentat_sandbox.identity Mentat_sandbox.direct)
      ()
  in
  let turn =
    Session.Turn.make
      ~id:(Session.Turn.Id.of_string "turn-parent")
      ~origin:Session.Turn.Origin.User
      ~input:(Session.Turn.Input.user_text "delegate")
      ~max_steps:8 ~contract ()
  in
  let edge =
    Session.Delegation.make
      ~child:(Session.Id.of_string "session-child")
      ~source_turn:(Session.Turn.id turn) ~source_call:"call-child"
      ~task:[ Mentat_llm.Content.text "inspect the child path" ]
      ()
  in
  match
    Session.append_all
      [
        Session.Event.turn_started turn; Session.Event.delegation_recorded edge;
      ]
      session
  with
  | Ok session -> session
  | Error error ->
      failf "invalid parent session: %s" (Session.Error.message error)

let%expect_test
    "a tracked child command failure clears and renders its child row" =
  Tui.run ~name:"child-correlation" ~size:(100, 24)
    ~sessions:(fun project -> [ session_with_child project ])
    ~child_observation_error:(unavailable "child feed failed")
  @@ fun t ->
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-child-corac2b3aaf
    04 |
    05 | ❯ delegate
    06 |
    07 | ✗ child feed failed
    08 |   retry when ready
    09 |
    10 | ⠋ Working… (0s · esc to interrupt)
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    19 | ❯ queue a message — sends after this turn
    20 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    21 |   ◯ main
    22 |   ! subagent inspect the child path                                                 feed unavailable
    23 |
    24 |   child feed failed
    |}]

(* A confirmed selection is the model line's second owner alongside
   [Turn_started]: the moment the engine accepts the selector for the active
   session, the footer and home banner show the model the next turn will use.
   The visible model change is the acknowledgement, so no flash covers it. *)
let%expect_test
    "a confirmed selection updates the footer to the next-turn model" =
  let selected_text = "anthropic/claude-sonnet-4-5" in
  let selected = selector selected_text in
  let setup = Tui.Turn_script.complete ~prompt:"start session" "Ready." in
  Tui.run ~name:"model-next-turn-footer" ~size:(120, 24)
    ~model_selections:[ selected ] ~turns:[ setup ]
  @@ fun t ->
  submit t "start session";
  Tui.finish_turn t;
  Tui.settle t;
  select_model t selected_text;
  Tui.finish_model_selection t (Ok ());
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-next-turb4b381a2
    04 |
    05 | ❯ start session
    06 |
    07 | ⏺ Ready.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-model-next-turb4… · anthropic/claude-sonnet-4… · ! full access ? for shortc…
    |}]

(* The opening record banner keeps the model the transcript began with, while
   the footer tracks the next turn. A mid-session change therefore shows the two
   models honestly: the banner records what served the first message and the
   footer names what the next message will use, which [Turn_started] then
   confirms with the exact sealed effort. *)
let%expect_test
    "a mid-session change keeps the opening banner and labels the next reply" =
  let selected_text = "anthropic/claude-sonnet-4-5" in
  let selected = selector selected_text in
  let setup = Tui.Turn_script.complete ~prompt:"start session" "Ready." in
  let changed =
    Tui.Turn_script.complete ~prompt:"use the selected model"
      ~model:(model ~provider:"anthropic" "claude-sonnet-4-5")
      ~reasoning_effort:Reasoning.High "Selected."
  in
  Tui.run ~name:"model-banner-vs-footer" ~size:(120, 24)
    ~model_selections:[ selected ] ~turns:[ setup; changed ]
  @@ fun t ->
  submit t "start session";
  Tui.finish_turn t;
  Tui.settle t;
  select_model t selected_text;
  Tui.finish_model_selection t (Ok ());
  Tui.settle t;
  submit t "use the selected model";
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · openai/gpt-5.5 medium
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-banner-v56abd910
    04 |
    05 | ❯ start session
    06 |
    07 | ⏺ Ready.
    08 |
    09 | ❯ use the selected model
    10 |
    11 | ⏺ Selected.
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-model-banner-… · anthropic/claude-sonnet-4-5 h… · ! full access ? for short…
    |}]

(* Sessions are minted lazily by the first prompt, so a pre-session activation
   has nothing to bind — the selection stages for the session that prompt will
   start. The panel closes and the home banner adopts the staged model at once:
   the model line always names the model the next turn will use. *)
let%expect_test
    "activation with no session stages the model for the first prompt" =
  Tui.run ~name:"model-no-session" ~size:(100, 24) @@ fun t ->
  open_model t;
  Tui.paste t "anthropic/claude-sonnet-4-5";
  Tui.settle t;
  Tui.enter t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |
    03 |
    04 |
    05 |                                     █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·
    06 |                                     █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂
    07 |
    08 |                                   dev · anthropic/claude-sonnet-4-5
    09 |
    10 |                ▎ welcome — and thanks for trying mentat this early.
    11 |                ▎ it's experimental: sessions and config may change without migration.
    12 |
    13 |
    14 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    15 | ❯ message mentat
    16 | ────────────────────────────────────────────────────────────────────────────────────────────────────
    17 |
    18 |                                    ! /login — no connected account
    19 |                                         ∅ no recent sessions
    20 |
    21 |
    22 |
    23 |
    24 |   ! not logged in · /login · ~/mentat-tui-mo… · anthropic/claude-sonnet… · ! full access ? for sh…
    |}]

(* The staged selection rides [Start_session]: the runtime applies it between
   create and the first submit, so the very first turn already runs the chosen
   model, which [Turn_started] then confirms with the exact sealed effort. *)
let%expect_test "a staged pre-session selection seals the first turn" =
  let selected_text = "anthropic/claude-sonnet-4-5" in
  let selected = selector selected_text in
  let first =
    Tui.Turn_script.complete ~prompt:"start on the staged model"
      ~model:(model ~provider:"anthropic" "claude-sonnet-4-5")
      ~reasoning_effort:Reasoning.High "Staged."
  in
  Tui.run ~name:"model-staged-first-turn" ~size:(120, 24)
    ~model_selections:[ selected ] ~turns:[ first ]
  @@ fun t ->
  select_model t selected_text;
  submit t "start on the staged model";
  Tui.finish_model_selection t (Ok ());
  Tui.settle t;
  Tui.finish_turn t;
  Tui.settle t;
  Tui.print t;
  [%expect
    {|
    01 |
    02 |  █▄█ ██▀ █▀▄ ▀█▀ ▄▀█ ▀█▀   ·    dev · anthropic/claude-sonnet-4-5
    03 |  █ █ █▄▄ █ █  █  █▀█  █  ▂▄▆▄▂  ~/mentat-tui-model-staged-fi8106001b
    04 |
    05 | ❯ start on the staged model
    06 |
    07 | ⏺ Staged.
    08 |
    09 |
    10 |
    11 |
    12 |
    13 |
    14 |
    15 |
    16 |
    17 |
    18 |
    19 |
    20 |
    21 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    22 | ❯ message mentat
    23 | ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    24 |   ! not logged in · /login · ~/mentat-tui-model-staged-… · anthropic/claude-sonnet-4-5 h… · ! full access ? for short…
    |}]
