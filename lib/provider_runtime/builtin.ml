(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Provider = Mentat_provider
module Auth = Provider.Auth
module Env = Auth.Env
module Login = Auth.Login
module Protocol = Login.Protocol
module Model = Provider.Model
module Listing = Provider.Listing
module Capability = Model.Capability
module Date = Model.Date
module Month = Model.Month
module Modality = Model.Modality
module Pricing = Model.Pricing
module Options = Mentat_llm.Request.Options
module Credential = Provider.Credential
module Secret = Credential.Secret
module Kind = Credential.Kind
module Problem = Provider.Account.Problem

let invalid message = invalid_arg ("Mentat_provider_runtime.Builtin: " ^ message)

let date text =
  match Date.of_string text with
  | Some date -> date
  | None -> invalid ("invalid built-in model release date: " ^ text)

(* Provider-documented knowledge cutoffs (YYYY-MM). Anthropic values are the
   "reliable knowledge cutoff" column of the models overview; OpenAI and Google
   values are the per-model knowledge cutoff each publishes. *)
let cutoff text =
  match Month.of_string text with
  | Some month -> month
  | None -> invalid ("invalid built-in model knowledge cutoff: " ^ text)

(* Model metadata. *)

let text_image = [ Modality.text; Modality.image ]
let text_image_pdf = [ Modality.text; Modality.image; Modality.pdf ]

let text_image_audio_video_pdf =
  [
    Modality.text; Modality.image; Modality.audio; Modality.video; Modality.pdf;
  ]

let tools_only = [ Capability.tools ]
let tools_reasoning = [ Capability.tools; Capability.reasoning ]

let tools_reasoning_json_schema =
  [ Capability.tools; Capability.reasoning; Capability.json_schema ]

let gpt_coding_capabilities =
  tools_reasoning_json_schema @ [ Capability.extension "apply-patch" ]

let gpt5_efforts =
  Options.Reasoning_effort.[ Disabled; Low; Medium; High; Extra_high ]

let gpt56_efforts =
  Options.Reasoning_effort.[ Disabled; Low; Medium; High; Extra_high; Max ]

let gpt_pro_efforts = Options.Reasoning_effort.[ Medium; High; Extra_high ]

let claude_47_efforts =
  Options.Reasoning_effort.[ Low; Medium; High; Extra_high; Max ]

let gemini_3_pro_efforts = Options.Reasoning_effort.[ Low; High ]

let gemini_3_flash_efforts =
  Options.Reasoning_effort.[ Minimal; Low; Medium; High ]

let local_efforts = Options.Reasoning_effort.[ Disabled; Low; Medium; High ]
let tools_json_schema = [ Capability.tools; Capability.json_schema ]

let rate ?cache_read ?cache_write ~input ~output () =
  Pricing.Rate.make ~input_per_million:input
    ?cached_input_per_million:cache_read ~output_per_million:output
    ?cache_write_5m_per_million:cache_write ()

let tier ?cache_read ?cache_write ~context_over ~input ~output () =
  (context_over, rate ?cache_read ?cache_write ~input ~output ())

let pricing ?(tiers = []) ?cache_read ?cache_write ~input ~output () =
  Pricing.make ~context_over:tiers
    (rate ?cache_read ?cache_write ~input ~output ())

let gpt_5_chat_unavailable =
  Model.Unavailable "OpenAI Responses does not support this chat alias"

(* One model per family: each entry is the newest member of its lineage.
   [gpt-5.6-sol] is the listed id for the frontier model: the bare [gpt-5.6]
   alias resolves to the same model on the API-key route but the ChatGPT Codex
   backend rejects it, while every route accepts the explicit id. *)
let openai_models =
  let llm = Mentat_llm_openai.model in
  [
    Model.make (llm "gpt-5.6-sol") ~display_name:"GPT-5.6 Sol" ~family:"gpt-sol"
      ~released_on:(date "2026-07-09") ~knowledge_cutoff:(cutoff "2026-02")
      ~context_window:1_050_000 ~max_output_tokens:128_000
      ~input_modalities:text_image_pdf ~capabilities:gpt_coding_capabilities
      ~default_reasoning:Options.Reasoning_effort.Medium
      ~supported_reasoning:gpt56_efforts
      ~pricing:
        (pricing ~input:5. ~output:30. ~cache_read:0.5 ~cache_write:6.25
           ~tiers:
             [
               tier ~context_over:272_000 ~input:10. ~output:45. ~cache_read:1.
                 ~cache_write:12.5 ();
             ]
           ())
      ();
    Model.make (llm "gpt-5.6-terra") ~display_name:"GPT-5.6 Terra"
      ~family:"gpt-terra" ~released_on:(date "2026-07-09")
      ~knowledge_cutoff:(cutoff "2026-02") ~context_window:1_050_000
      ~max_output_tokens:128_000 ~input_modalities:text_image_pdf
      ~capabilities:gpt_coding_capabilities
      ~default_reasoning:Options.Reasoning_effort.Medium
      ~supported_reasoning:gpt56_efforts
      ~pricing:
        (pricing ~input:2.5 ~output:15. ~cache_read:0.25 ~cache_write:3.125
           ~tiers:
             [
               tier ~context_over:272_000 ~input:5. ~output:22.5 ~cache_read:0.5
                 ~cache_write:6.25 ();
             ]
           ())
      ();
    Model.make (llm "gpt-5.6-luna") ~display_name:"GPT-5.6 Luna"
      ~family:"gpt-luna" ~released_on:(date "2026-07-09")
      ~knowledge_cutoff:(cutoff "2026-02") ~context_window:1_050_000
      ~max_output_tokens:128_000 ~input_modalities:text_image_pdf
      ~capabilities:gpt_coding_capabilities
      ~default_reasoning:Options.Reasoning_effort.Medium
      ~supported_reasoning:gpt56_efforts
      ~pricing:
        (pricing ~input:1. ~output:6. ~cache_read:0.1 ~cache_write:1.25
           ~tiers:
             [
               tier ~context_over:272_000 ~input:2. ~output:9. ~cache_read:0.2
                 ~cache_write:2.5 ();
             ]
           ())
      ();
    Model.make (llm "gpt-5.5-pro") ~display_name:"GPT-5.5 Pro" ~family:"gpt-pro"
      ~released_on:(date "2026-04-23") ~knowledge_cutoff:(cutoff "2025-12")
      ~context_window:1_050_000 ~max_output_tokens:128_000
      ~input_modalities:text_image_pdf ~capabilities:gpt_coding_capabilities
      ~default_reasoning:Options.Reasoning_effort.Medium
      ~supported_reasoning:gpt_pro_efforts
      ~pricing:
        (pricing ~input:30. ~output:180.
           ~tiers:[ tier ~context_over:272_000 ~input:60. ~output:270. () ]
           ())
      ();
    Model.make (llm "gpt-5.4-mini") ~display_name:"GPT-5.4 mini"
      ~family:"gpt-mini" ~released_on:(date "2026-03-17")
      ~knowledge_cutoff:(cutoff "2025-08") ~context_window:400_000
      ~max_output_tokens:128_000 ~input_modalities:text_image
      ~capabilities:gpt_coding_capabilities
      ~default_reasoning:Options.Reasoning_effort.Medium
      ~supported_reasoning:gpt5_efforts
      ~pricing:(pricing ~input:0.75 ~output:4.5 ~cache_read:0.075 ())
      ();
    Model.make (llm "gpt-5.4-nano") ~display_name:"GPT-5.4 nano"
      ~family:"gpt-nano" ~released_on:(date "2026-03-17")
      ~knowledge_cutoff:(cutoff "2025-08") ~context_window:400_000
      ~max_output_tokens:128_000 ~input_modalities:text_image
      ~capabilities:gpt_coding_capabilities
      ~default_reasoning:Options.Reasoning_effort.Medium
      ~supported_reasoning:gpt5_efforts
      ~pricing:(pricing ~input:0.2 ~output:1.25 ~cache_read:0.02 ())
      ();
    Model.make (llm "gpt-5.3-codex") ~display_name:"GPT-5.3 Codex"
      ~family:"gpt-codex" ~released_on:(date "2026-02-05")
      ~knowledge_cutoff:(cutoff "2025-08") ~context_window:400_000
      ~max_output_tokens:128_000 ~input_modalities:text_image_pdf
      ~capabilities:gpt_coding_capabilities
      ~default_reasoning:Options.Reasoning_effort.Medium
      ~supported_reasoning:gpt5_efforts
      ~pricing:(pricing ~input:1.75 ~output:14. ~cache_read:0.175 ())
      ();
    Model.make (llm "gpt-image-2") ~display_name:"gpt-image-2"
      ~family:"gpt-image" ~released_on:(date "2026-04-21")
      ~input_modalities:text_image ~output_modalities:[ Modality.image ]
      ~pricing:(pricing ~input:5. ~output:30. ~cache_read:1.25 ())
      ();
    Model.make
      (llm "gpt-5.3-chat-latest")
      ~display_name:"GPT-5.3 Chat (latest)" ~family:"gpt"
      ~released_on:(date "2026-03-03") ~knowledge_cutoff:(cutoff "2025-08")
      ~context_window:128_000 ~max_output_tokens:16_384
      ~input_modalities:text_image ~capabilities:tools_json_schema
      ~pricing:(pricing ~input:1.75 ~output:14. ~cache_read:0.175 ())
      ~status:gpt_5_chat_unavailable ();
  ]

let anthropic_models =
  let llm = Mentat_llm_anthropic.model in
  [
    Model.make (llm "claude-sonnet-5") ~display_name:"Claude Sonnet 5"
      ~family:"claude-sonnet" ~released_on:(date "2026-06-29")
      ~knowledge_cutoff:(cutoff "2026-01") ~context_window:1_000_000
      ~max_output_tokens:128_000 ~input_modalities:text_image_pdf
      ~capabilities:tools_reasoning ~supported_reasoning:claude_47_efforts
      ~pricing:
        (pricing ~input:2. ~output:10. ~cache_read:0.2 ~cache_write:2.5 ())
      ();
    Model.make (llm "claude-fable-5") ~display_name:"Claude Fable 5"
      ~family:"claude-fable" ~released_on:(date "2026-06-07")
      ~knowledge_cutoff:(cutoff "2026-01") ~context_window:1_000_000
      ~max_output_tokens:128_000 ~input_modalities:text_image_pdf
      ~capabilities:tools_reasoning ~supported_reasoning:claude_47_efforts
      ~pricing:
        (pricing ~input:10. ~output:50. ~cache_read:1. ~cache_write:12.5 ())
      ();
    Model.make (llm "claude-opus-5") ~display_name:"Claude Opus 5"
      ~family:"claude-opus" ~released_on:(date "2026-07-21")
      ~knowledge_cutoff:(cutoff "2026-01") ~context_window:1_000_000
      ~max_output_tokens:128_000 ~input_modalities:text_image_pdf
      ~capabilities:tools_reasoning ~supported_reasoning:claude_47_efforts
      ~pricing:
        (pricing ~input:5. ~output:25. ~cache_read:0.5 ~cache_write:6.25 ())
      ();
    (* Haiku 4.5 predates adaptive thinking: it reasons only under the manual
       token budget this provider no longer sends, and rejects the effort
       parameter that replaced it. Declaring no reasoning capability is what
       makes that true rather than merely unreached, and it keeps the selection
       gate from offering an effort the encoder could not express. *)
    Model.make (llm "claude-haiku-4-5") ~display_name:"Claude Haiku 4.5"
      ~family:"claude-haiku" ~released_on:(date "2025-10-15")
      ~knowledge_cutoff:(cutoff "2025-02") ~context_window:200_000
      ~max_output_tokens:64_000 ~input_modalities:text_image_pdf
      ~capabilities:tools_only
      ~pricing:
        (pricing ~input:1. ~output:5. ~cache_read:0.1 ~cache_write:1.25 ())
      ();
  ]

let google_models =
  let llm = Mentat_llm_google.model in
  [
    Model.make (llm "gemini-3.6-flash") ~display_name:"Gemini 3.6 Flash"
      ~family:"gemini-flash" ~released_on:(date "2026-07-21")
      ~knowledge_cutoff:(cutoff "2026-03") ~context_window:1_048_576
      ~max_output_tokens:65_536 ~input_modalities:text_image_audio_video_pdf
      ~capabilities:tools_reasoning ~supported_reasoning:gemini_3_flash_efforts
      ~pricing:(pricing ~input:1.5 ~output:7.5 ~cache_read:0.15 ())
      ();
    Model.make
      (llm "gemini-3.5-flash-lite")
      ~display_name:"Gemini 3.5 Flash Lite" ~family:"gemini-flash-lite"
      ~released_on:(date "2026-07-21") ~knowledge_cutoff:(cutoff "2026-03")
      ~context_window:1_048_576 ~max_output_tokens:65_536
      ~input_modalities:text_image_audio_video_pdf ~capabilities:tools_reasoning
      ~supported_reasoning:gemini_3_flash_efforts
      ~pricing:(pricing ~input:0.3 ~output:2.5 ~cache_read:0.03 ())
      ();
    Model.make
      (llm "gemini-3.1-pro-preview")
      ~display_name:"Gemini 3.1 Pro Preview" ~family:"gemini-pro"
      ~released_on:(date "2026-02-19") ~knowledge_cutoff:(cutoff "2025-01")
      ~context_window:1_048_576 ~max_output_tokens:65_536
      ~input_modalities:text_image_audio_video_pdf ~capabilities:tools_reasoning
      ~supported_reasoning:gemini_3_pro_efforts ~status:Model.Preview
      ~pricing:
        (pricing ~input:2. ~output:12. ~cache_read:0.2
           ~tiers:
             [
               tier ~context_over:200_000 ~input:4. ~output:18. ~cache_read:0.4
                 ();
             ]
           ())
      ();
  ]

let local_models =
  let local_model entry =
    let llm = Mentat_llm_local.model (Mentat_llm_local.Manifest.id entry) in
    let display_name = Mentat_llm_local.Manifest.display_name entry in
    let family = Mentat_llm_local.Manifest.family entry in
    let context_window = Mentat_llm_local.Manifest.context_length entry in
    if Mentat_llm_local.Manifest.reasoning entry then
      Model.make llm ~display_name ~family ~context_window
        ~max_output_tokens:16_384 ~capabilities:tools_reasoning_json_schema
        ~default_reasoning:Options.Reasoning_effort.Medium
        ~supported_reasoning:local_efforts ()
    else
      Model.make llm ~display_name ~family ~context_window
        ~max_output_tokens:16_384 ~capabilities:tools_json_schema ()
  in
  List.map local_model Mentat_llm_local.Manifest.all

(* Auth declarations. *)

module Openai_auth = struct
  let issuer = Uri.of_string "https://auth.openai.com"
  let client_id = "app_EMoamEEZ73f0CkXaXp7hrann"
  let browser_redirect_uri = Uri.of_string "http://localhost:1455/auth/callback"

  let trim_right_slashes path =
    let rec loop i =
      if i <= 0 then ""
      else if Char.equal (String.unsafe_get path (i - 1)) '/' then loop (i - 1)
      else String.sub path 0 i
    in
    loop (String.length path)

  let append_path suffix =
    let base = trim_right_slashes (Uri.path issuer) in
    let suffix =
      if String.starts_with ~prefix:"/" suffix then suffix else "/" ^ suffix
    in
    let uri = Uri.with_path issuer (base ^ suffix) in
    Uri.with_fragment (Uri.with_query' uri []) None

  let authorization_endpoint = append_path "/oauth/authorize"
  let oauth_token_endpoint = append_path "/oauth/token"
end

let openai_chatgpt_protocol_id = Login.Id.make "openai_chatgpt"

let openai_auth =
  let client = Oauth2.Client.make ~id:Openai_auth.client_id () in
  let browser =
    Login.oauth2_authorization_code ~client
      ~authorization_endpoint:Openai_auth.authorization_endpoint
      ~token_endpoint:Openai_auth.oauth_token_endpoint
      ~redirect_uri:Openai_auth.browser_redirect_uri
      ~scope:[ "openid"; "profile"; "email"; "offline_access" ]
        (* [originator] names the client making the request; the issuer accepts
         any value and uses it for attribution, so Mentat names itself. The
         client id above is still Codex CLI's public OAuth client, which
         Mentat has no registered equivalent of. *)
      ~extra:
        [
          ("id_token_add_organizations", "true");
          ("codex_cli_simplified_flow", "true");
          ("originator", "mentat");
        ]
      ()
  in
  let device_code =
    Login.make
      ~id:(Login.Id.make "device-code")
      ~label:"OpenAI ChatGPT device code"
      (Protocol.Provider_defined
         { protocol = openai_chatgpt_protocol_id; credential_kind = Kind.OAuth })
  in
  Auth.make
    ~env:[ Env.api_key "OPENAI_API_KEY" ]
    ~login:[ browser; device_code; Login.api_key () ]
    ()

let anthropic_auth =
  Auth.make
    ~env:[ Env.api_key "ANTHROPIC_API_KEY" ]
    ~login:[ Login.api_key () ]
    ()

let google_auth =
  Auth.make
    ~env:
      [
        Env.api_key "GOOGLE_API_KEY";
        Env.api_key "GOOGLE_GENERATIVE_AI_API_KEY";
        Env.api_key "GEMINI_API_KEY";
      ]
    ~login:[ Login.api_key () ]
    ()

let ollama_auth =
  Auth.make ~required:false
    ~env:[ Env.api_key "OLLAMA_API_KEY" ]
    ~login:[ Login.api_key () ]
    ()

(* [default_model] is matched to a declared model by its [Mentat_llm.Model.t]
   identity, so a bare annotation of the identity suffices. *)
let default_model llm_model = Model.make llm_model ()

let gguf_dynamic_model llm capabilities id =
  if String.ends_with ~suffix:".gguf" id then
    Some
      (Model.make (llm id) ~display_name:(Filename.basename id) ~family:"gguf"
         ~capabilities ())
  else None

let openai_declaration =
  Provider.make Mentat_llm_openai.provider ~display_name:"OpenAI"
    ~default_model:(default_model (Mentat_llm_openai.model "gpt-5.6-sol"))
    ~auth:openai_auth openai_models

let anthropic_declaration =
  Provider.make Mentat_llm_anthropic.provider ~display_name:"Anthropic"
    ~default_model:
      (default_model (Mentat_llm_anthropic.model "claude-sonnet-5"))
    ~auth:anthropic_auth anthropic_models

let google_declaration =
  Provider.make Mentat_llm_google.provider ~display_name:"Google"
    ~default_model:(default_model (Mentat_llm_google.model "gemini-3.6-flash"))
    ~auth:google_auth google_models

let local_declaration =
  Provider.make Mentat_llm_local.provider ~display_name:"Local"
    ~default_model:(default_model (Mentat_llm_local.model "qwen3-coder-30b"))
    ~dynamic:(gguf_dynamic_model Mentat_llm_local.model tools_json_schema)
    local_models

let ollama_declaration =
  Provider.make Mentat_llm_ollama.provider ~display_name:"Ollama"
    ~auth:ollama_auth
    ~dynamic:(fun id ->
      if String.equal id "" then None
      else
        Some
          (Model.make
             (Mentat_llm_ollama.model id)
             ~display_name:id ~family:"ollama" ~capabilities:tools_json_schema
             ()))
    []

(* OpenCode Go: a subscription gateway whose model set is server-owned. The
   declaration curates the models mentat vouches for — pricing, family
   naming, the default — and the server listing supplies every other model at
   runtime through the listed-model rule below. Context windows and cutoffs
   are filled from server metadata as it is verified, never invented. *)

(* Context windows, output limits, and current prices come from the vendor's
   published model catalog; reasoning efforts are declared only for models a
   live probe confirmed accept [reasoning_effort] (the chat-completions codec
   encodes low/medium/high). The DeepSeek pair is region-gated pending a
   console opt-in, so its efforts stay undeclared and its rates are the
   published peak — the spend row errs high, never low. *)
let opencode_efforts = Options.Reasoning_effort.[ Low; Medium; High ]

let opencode_models =
  let llm = Mentat_llm_opencode.chat_model in
  [
    Model.make (llm "kimi-k2.7-code") ~display_name:"Kimi K2.7 Code"
      ~family:"kimi-code" ~context_window:262_144 ~max_output_tokens:262_144
      ~capabilities:tools_reasoning ~supported_reasoning:opencode_efforts
      ~pricing:(pricing ~input:0.95 ~output:4. ~cache_read:0.19 ())
      ();
    Model.make (llm "kimi-k3") ~display_name:"Kimi K3" ~family:"kimi"
      ~context_window:1_048_576 ~max_output_tokens:131_072
      ~capabilities:tools_reasoning ~supported_reasoning:opencode_efforts
      ~pricing:(pricing ~input:3. ~output:15. ~cache_read:0.3 ())
      ();
    Model.make (llm "glm-5.3") ~display_name:"GLM-5.3" ~family:"glm"
      ~context_window:1_000_000 ~max_output_tokens:131_072
      ~capabilities:tools_reasoning ~supported_reasoning:opencode_efforts
      ~pricing:(pricing ~input:1.4 ~output:4.4 ~cache_read:0.26 ())
      ();
    Model.make (llm "deepseek-v4-pro") ~display_name:"DeepSeek V4 Pro"
      ~family:"deepseek-pro" ~context_window:1_000_000
      ~max_output_tokens:384_000 ~capabilities:tools_only
      ~pricing:(pricing ~input:1.32 ~output:3.96 ~cache_read:0.044 ())
      ();
    Model.make (llm "deepseek-v4-flash") ~display_name:"DeepSeek V4 Flash"
      ~family:"deepseek-flash" ~context_window:1_000_000
      ~max_output_tokens:384_000 ~capabilities:tools_only
      ~pricing:(pricing ~input:0.44 ~output:1.32 ~cache_read:0.014 ())
      ();
    Model.make (llm "mimo-v2.5-pro") ~display_name:"MiMo-V2.5-Pro"
      ~family:"mimo-pro" ~context_window:1_048_576 ~max_output_tokens:128_000
      ~capabilities:tools_reasoning ~supported_reasoning:opencode_efforts
      ~pricing:(pricing ~input:0.435 ~output:0.87 ~cache_read:0.003625 ())
      ();
    Model.make (llm "mimo-v2.5") ~display_name:"MiMo-V2.5" ~family:"mimo"
      ~context_window:1_000_000 ~max_output_tokens:128_000
      ~capabilities:tools_reasoning ~supported_reasoning:opencode_efforts
      ~pricing:(pricing ~input:0.14 ~output:0.28 ~cache_read:0.0028 ())
      ();
    Model.make (llm "hy3") ~display_name:"Hy3" ~family:"hy"
      ~context_window:256_000 ~max_output_tokens:64_000
      ~capabilities:tools_reasoning ~supported_reasoning:opencode_efforts
      ~pricing:(pricing ~input:0.0175 ~output:0.0725 ~cache_read:0.004375 ())
      ();
    (* The messages-protocol lines. Their thinking dialect is unverified on
       the gateway, so no reasoning efforts are declared. *)
    Model.make
      (Mentat_llm_opencode.messages_model "minimax-m3")
      ~display_name:"MiniMax M3" ~family:"minimax" ~context_window:1_000_000
      ~max_output_tokens:131_072 ~capabilities:tools_only
      ~pricing:
        (pricing ~input:0.3 ~output:1.2 ~cache_read:0.06
           ~tiers:
             [
               tier ~context_over:512_000 ~input:0.6 ~output:2.4 ~cache_read:0.12
                 ();
             ]
           ())
      ();
    Model.make
      (Mentat_llm_opencode.messages_model "qwen3.8-max")
      ~display_name:"Qwen3.8 Max" ~family:"qwen-max" ~context_window:1_000_000
      ~max_output_tokens:131_072 ~capabilities:tools_only
      ~pricing:
        (pricing ~input:2. ~output:6. ~cache_read:0.25 ~cache_write:2.5 ())
      ();
    Model.make
      (Mentat_llm_opencode.messages_model "qwen3.7-plus")
      ~display_name:"Qwen3.7 Plus" ~family:"qwen-plus"
      ~context_window:1_000_000 ~max_output_tokens:65_536
      ~capabilities:tools_only
      ~pricing:
        (pricing ~input:0.4 ~output:1.6 ~cache_read:0.04 ~cache_write:0.5
           ~tiers:
             [
               tier ~context_over:256_000 ~input:1.2 ~output:4.8 ~cache_read:0.12
                 ~cache_write:1.5 ();
             ]
           ())
      ();
  ]

let opencode_auth =
  Auth.make
    ~env:[ Env.api_key "OPENCODE_API_KEY" ]
    ~login:[ Login.api_key () ]
    ()

(* The gateway assigns each model one of several wire protocols — a
   per-model, server-decided fact no pure function of the id can recover —
   so the listed-model rule reads the protocol the check lowered into the
   listing. The protocol families are the existing adapters' exported api
   values, never re-minted spellings of them; only the truly unrecognized
   case gets its own id. Models on protocols this provider does not serve
   surface as [Unavailable] with the reason: inspectable, never a dead
   pick. *)
let opencode_unknown_api = Mentat_llm.Model.Api.make "unknown"

let opencode_dynamic_listed listed =
  let id = Listing.Model.id listed in
  let unroutable api reason =
    Model.make
      (Mentat_llm.Model.make ~provider:Mentat_llm_opencode.provider ~api ~id)
      ?display_name:(Listing.Model.display_name listed)
      ?family:(Listing.Model.family listed)
      ~status:(Model.Unavailable reason) ()
  in
  match Listing.Model.api listed with
  | Some api
    when Mentat_llm.Model.Api.equal api Mentat_llm_http.Chat_completions.api ->
      Some
        (Model.make
           (Mentat_llm_opencode.chat_model id)
           ?display_name:(Listing.Model.display_name listed)
           ?family:(Listing.Model.family listed)
           ?context_window:(Listing.Model.context_window listed)
           ?max_output_tokens:(Listing.Model.max_output_tokens listed)
           ?pricing:(Listing.Model.pricing listed)
           ?input_modalities:(Listing.Model.input_modalities listed)
           ~capabilities:
             (Option.value ~default:tools_only
                (Listing.Model.capabilities listed))
           ?status:(Listing.Model.status listed) ())
  | Some api when Mentat_llm.Model.Api.equal api Mentat_llm_http.Messages.api ->
      Some
        (Model.make
           (Mentat_llm_opencode.messages_model id)
           ?display_name:(Listing.Model.display_name listed)
           ?family:(Listing.Model.family listed)
           ?context_window:(Listing.Model.context_window listed)
           ?max_output_tokens:(Listing.Model.max_output_tokens listed)
           ?pricing:(Listing.Model.pricing listed)
           ?input_modalities:(Listing.Model.input_modalities listed)
           ~capabilities:
             (Option.value ~default:tools_only
                (Listing.Model.capabilities listed))
           ?status:(Listing.Model.status listed) ())
  | Some api ->
      Some (unroutable api "this wire protocol is not served by this provider")
  | None -> Some (unroutable opencode_unknown_api "unrecognized wire protocol")

let opencode_declaration =
  Provider.make Mentat_llm_opencode.provider ~display_name:"OpenCode Go"
    ~default_model:
      (default_model (Mentat_llm_opencode.chat_model "kimi-k2.7-code"))
    ~auth:opencode_auth ~dynamic_listed:opencode_dynamic_listed opencode_models

(* Live account checks. *)

let drop_prefix ~prefix s =
  if String.starts_with ~prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else s

module Check = struct
  let problem ~status ~body =
    if status = 401 || status = 403 then Problem.Invalid_credential
    else if status = 402 then Problem.Quota_exceeded
    else if status = 429 then
      if String.includes ~affix:"quota" (String.lowercase_ascii body) then
        Problem.Quota_exceeded
      else Problem.Rate_limited
    else if status >= 500 then Problem.Network
    else Problem.other "unknown_provider_response"

  let json_field name = function
    | Jsont.Object (fields, _) ->
        Option.map snd (Jsont.Json.find_mem name fields)
    | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
    | Jsont.Array _ ->
        None

  let json_string_field name json =
    match json_field name json with
    | Some (Jsont.String (value, _)) -> Some value
    | Some _ | None -> None

  let entry_ids ~id values =
    let ids = List.filter_map (json_string_field id) values in
    if List.length ids = List.length values then Some ids else None

  let models body =
    match Jsont_bytesrw.decode_string Jsont.json body with
    | Error _ -> None
    | Ok json -> (
        match json_field "data" json with
        | Some (Jsont.Array (values, _)) -> entry_ids ~id:"id" values
        | Some _ -> None
        | None -> (
            match json_field "models" json with
            | Some (Jsont.Array (values, _)) -> (
                (* The ChatGPT Codex backend keys each entry by [slug]; Google's
                   [/models] keys them by a [models/]-prefixed [name]. *)
                match entry_ids ~id:"slug" values with
                | Some _ as ids -> ids
                | None ->
                    entry_ids ~id:"name" values
                    |> Option.map (List.map (drop_prefix ~prefix:"models/")))
            | Some _ | None -> None))
end

let observation ?(problems = []) ?listing () =
  { Driver.problems; profile = None; org = None; listing }

let unsupported_route = observation ~problems:[ Problem.Unsupported ] ()
let network_problem = observation ~problems:[ Problem.Network ] ()
let unknown_response = Problem.other "unknown_provider_response"

(* Server data is untrusted: a malformed id set lowers to a problem rather
   than escaping the check as a fault. *)
let listing_of_ids ids =
  match Provider.Listing.of_ids ids with
  | listing -> observation ~listing ()
  | exception Invalid_argument _ -> observation ~problems:[ unknown_response ] ()

let observe ~sw ~env ~headers url =
  match Tls_setup.get ~sw ~env ~headers url with
  | Error () -> network_problem
  | Ok (status, body) ->
      if status >= 200 && status < 300 then
        match Check.models body with
        | Some models -> listing_of_ids models
        | None -> observation ~problems:[ unknown_response ] ()
      else observation ~problems:[ Check.problem ~status ~body ] ()

let effective_base_url ~default = function
  | None -> default
  | Some base_url -> base_url

let chatgpt_base_url = "https://chatgpt.com/backend-api/codex"

(* The ChatGPT Codex backend rejects a [/models] request that omits a
   [client_version] query parameter with HTTP 400, and requires the value to
   parse as a semantic version (a bare integer is rejected). The magnitude does
   not gate the response: the endpoint returns the full model list regardless, so
   a well-formed sentinel is sufficient to observe readiness. The standard
   [api.openai.com/v1/models] route imposes no such requirement. *)
let chatgpt_client_version = "0.0.0"

let chatgpt_account_headers = function
  | None -> []
  | Some account_id -> [ ("chatgpt-account-id", account_id) ]

let openai_check ~sw ~env ?base_url ?auth_base_url:_ credential =
  match credential with
  | None -> unsupported_route
  | Some credential ->
      let models ~default ~path ~headers =
        observe ~sw ~env ~headers (effective_base_url ~default base_url ^ path)
      in
      let api_route token =
        models ~default:"https://api.openai.com/v1" ~path:"/models"
          ~headers:[ ("authorization", "Bearer " ^ token) ]
      in
      Secret.expose
        (Credential.secret credential)
        ~api_key:(fun ~key -> api_route key)
        ~bearer:(fun ~token -> api_route token)
        ~oauth:(fun ~access_token ~refresh_token:_ ~expires_at:_ ~account_id ->
          models ~default:chatgpt_base_url
            ~path:("/models?client_version=" ^ chatgpt_client_version)
            ~headers:
              (("authorization", "Bearer " ^ access_token)
              :: chatgpt_account_headers account_id))

let anthropic_headers secret =
  Secret.expose secret
    ~api_key:(fun ~key ->
      Some [ ("x-api-key", key); ("anthropic-version", "2023-06-01") ])
    ~bearer:(fun ~token ->
      Some
        [
          ("authorization", "Bearer " ^ token);
          ("anthropic-version", "2023-06-01");
        ])
    ~oauth:(fun ~access_token:_ ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
      None)

let anthropic_check ~sw ~env ?base_url ?auth_base_url:_ credential =
  match Option.map Credential.secret credential with
  | None -> unsupported_route
  | Some secret -> (
      match anthropic_headers secret with
      | None -> unsupported_route
      | Some headers ->
          let base_url =
            effective_base_url ~default:"https://api.anthropic.com/v1" base_url
          in
          observe ~sw ~env ~headers (base_url ^ "/models"))

let google_key secret =
  Secret.expose secret
    ~api_key:(fun ~key -> Some key)
    ~bearer:(fun ~token:_ -> None)
    ~oauth:(fun ~access_token:_ ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
      None)

let google_check ~sw ~env ?base_url ?auth_base_url:_ credential =
  match Option.bind credential (fun c -> google_key (Credential.secret c)) with
  | None -> unsupported_route
  | Some key ->
      let base_url =
        effective_base_url
          ~default:"https://generativelanguage.googleapis.com/v1beta" base_url
      in
      let url =
        Uri.of_string (base_url ^ "/models")
        |> (fun uri -> Uri.add_query_param' uri ("key", key))
        |> Uri.to_string
      in
      observe ~sw ~env ~headers:[] url

(* Ollama's listing endpoint needs no credential; a resolved key still rides
   as a bearer for proxied daemons. *)
let ollama_check ~sw ~env ?base_url ?auth_base_url:_ credential =
  let base_url =
    effective_base_url ~default:"http://127.0.0.1:11434" base_url
  in
  let headers =
    match Option.map Credential.secret credential with
    | None -> []
    | Some secret ->
        Secret.expose secret
          ~api_key:(fun ~key -> [ ("authorization", "Bearer " ^ key) ])
          ~bearer:(fun ~token -> [ ("authorization", "Bearer " ^ token) ])
          ~oauth:(fun
              ~access_token:_ ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
            [])
  in
  observe ~sw ~env ~headers (base_url ^ "/api/tags")

(* The OpenCode Go check reads the console's [/api/config] — the one endpoint
   that publishes per-model metadata, including the wire-protocol package the
   listed-model rule needs — and lowers it into a listing. The console is an
   enrichment source, not the account authority: whenever it yields no
   listing (down, rejecting the key, unrecognized shape), the check falls
   back to the gateway's bare-ids [/v1/models], whose verdict on the
   credential stands. Server data is untrusted throughout: an unusable entry
   degrades to a bare id, never a fault. *)

module Opencode_config = struct
  let api_of_npm = function
    | "@ai-sdk/openai-compatible" -> Some Mentat_llm_http.Chat_completions.api
    | "@ai-sdk/anthropic" -> Some Mentat_llm_http.Messages.api
    | "@ai-sdk/openai" -> Some Mentat_llm_openai.api
    | _ -> None

  let json_number_field name json =
    match Check.json_field name json with
    | Some (Jsont.Number (value, _)) -> Some value
    | Some _ | None -> None

  let json_bool_field name json =
    match Check.json_field name json with
    | Some (Jsont.Bool (value, _)) -> Some value
    | Some _ | None -> None

  let non_empty = function Some "" -> None | value -> value

  let positive_int_field name json =
    match json_number_field name json with
    | Some value
      when Float.is_finite value && value >= 1. && value <= 1e9
           && Float.is_integer value ->
        Some (int_of_float value)
    | Some _ | None -> None

  let rate_lane name json =
    match json_number_field name json with
    | Some value when Float.is_finite value && value >= 0. -> Some value
    | Some _ | None -> None

  let rate_of json =
    match (rate_lane "input" json, rate_lane "output" json) with
    | Some input, Some output ->
        Some
          (rate
             ?cache_read:(rate_lane "cache_read" json)
             ?cache_write:(rate_lane "cache_write" json)
             ~input ~output ())
    | (Some _ | None), _ -> None

  let pricing_of json =
    Option.bind (Check.json_field "cost" json) (fun cost ->
        rate_of cost
        |> Option.map (fun default_rate ->
            let tiers =
              Option.bind (Check.json_field "context_over_200k" cost) rate_of
              |> Option.map (fun tier_rate -> [ (200_000, tier_rate) ])
              |> Option.value ~default:[]
            in
            Pricing.make ~context_over:tiers default_rate))

  let status_of = function
    | "active" -> Some Model.Stable
    | "beta" -> Some Model.Preview
    | "alpha" -> Some (Model.Unavailable "alpha")
    | "deprecated" -> Some Model.Deprecated
    | _ -> None

  let input_modalities_of json =
    Option.bind (Check.json_field "modalities" json) (fun modalities ->
        match Check.json_field "input" modalities with
        | Some (Jsont.Array (values, _)) ->
            Some
              (values
              |> List.filter_map (function
                | Jsont.String (value, _) -> Modality.of_string value
                | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.Array _
                | Jsont.Object _ ->
                    None)
              |> List.sort_uniq Modality.compare)
        | Some _ | None -> None)

  let listed_model ~item_npm (id, json) =
    let npm =
      match
        Option.bind (Check.json_field "provider" json)
          (Check.json_string_field "npm")
      with
      | Some _ as override -> override
      | None -> item_npm
    in
    match
      Listing.Model.make ~id
        ?api:(Option.bind npm api_of_npm)
        ?display_name:(non_empty (Check.json_string_field "name" json))
        ?family:(non_empty (Check.json_string_field "family" json))
        ?context_window:
          (Option.bind (Check.json_field "limit" json)
             (positive_int_field "context"))
        ?max_output_tokens:
          (Option.bind (Check.json_field "limit" json)
             (positive_int_field "output"))
        ?pricing:(pricing_of json)
        ?input_modalities:(input_modalities_of json)
        ?capabilities:
          (Option.map
             (fun tool_call -> if tool_call then tools_only else [])
             (json_bool_field "tool_call" json))
        ?status:
          (Option.bind (Check.json_string_field "status" json) status_of)
        ()
    with
    | listed -> Some listed
    | exception Invalid_argument _ -> (
        (* An unusable entry keeps its id in the listing rather than
           vanishing into unavailability. *)
        match Listing.Model.of_id id with
        | listed -> Some listed
        | exception Invalid_argument _ -> None)

  let listing body =
    match Jsont_bytesrw.decode_string Jsont.json body with
    | Error _ -> None
    | Ok json ->
        Option.bind (Check.json_field "config" json) (fun config ->
            Option.bind (Check.json_field "provider" config) (fun providers ->
                Option.bind (Check.json_field "opencode-go" providers)
                  (fun item ->
                    let item_npm = Check.json_string_field "npm" item in
                    match Check.json_field "models" item with
                    | Some (Jsont.Object (members, _)) -> (
                        let models =
                          List.filter_map
                            (fun ((id, _), value) ->
                              listed_model ~item_npm (id, value))
                            members
                        in
                        (* An empty parse is shape drift, not an empty
                           catalog: it must not become the availability
                           authority that empties the provider. *)
                        match models with
                        | [] -> None
                        | models -> (
                            match Listing.make models with
                            | listing -> Some listing
                            | exception Invalid_argument _ -> None))
                    | Some _ | None -> None)))
end

let opencode_console_default = "https://console.opencode.ai"
let opencode_gateway_default = "https://opencode.ai/zen/go"

let opencode_check ~sw ~env ?base_url ?auth_base_url credential =
  match Option.map Credential.secret credential with
  | None -> unsupported_route
  | Some secret ->
      let token =
        Secret.expose secret
          ~api_key:(fun ~key -> key)
          ~bearer:(fun ~token -> token)
          ~oauth:(fun
              ~access_token ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
            access_token)
      in
      let headers = [ ("authorization", "Bearer " ^ token) ] in
      let console =
        effective_base_url ~default:opencode_console_default auth_base_url
      in
      let gateway_models () =
        let base_url =
          effective_base_url ~default:opencode_gateway_default base_url
        in
        observe ~sw ~env ~headers (base_url ^ "/v1/models")
      in
      (match Tls_setup.get ~sw ~env ~headers (console ^ "/api/config") with
      | Ok (status, body) when status >= 200 && status < 300 -> (
          match Opencode_config.listing body with
          | Some listing -> observation ~listing ()
          | None -> gateway_models ())
      | Ok _ | Error () -> gateway_models ())

let openai_chatgpt_config auth_base_url =
  match auth_base_url with
  | None -> Ok Oauth_flow.Openai_chatgpt.Config.default
  | Some root ->
      Oauth_flow.Openai_chatgpt.Config.make ~issuer:(Uri.of_string root) ()

let openai_auth_problem = function
  | Oauth_flow.Error.Rejected _ -> Problem.Refresh_failed
  | Oauth_flow.Error.Network _ | Oauth_flow.Error.Timeout _ -> Problem.Network
  | Oauth_flow.Error.Protocol _ -> Problem.other "unknown_provider_response"
  | Oauth_flow.Error.Not_refreshable | Oauth_flow.Error.Invalid_request _ ->
      Problem.Unsupported

let with_openai_auth ~env ?auth_base_url run =
  match openai_chatgpt_config auth_base_url with
  | Error error -> Error (openai_auth_problem error)
  | Ok config -> (
      match Oauth_flow.Http.tls_client ~stdenv:env with
      | Error _ -> Error Problem.Network
      | Ok http -> (
          match run http config with
          | Ok _ as ok -> ok
          | Error error -> Error (openai_auth_problem error)))

let openai_refresh ~sw ~env ~now ?auth_base_url secret =
  with_openai_auth ~env ?auth_base_url (fun http config ->
      Oauth_flow.Openai_chatgpt.refresh ~http ~sw ~now config secret)

let openai_revoke ~sw ~env ?auth_base_url secret =
  with_openai_auth ~env ?auth_base_url (fun http config ->
      Oauth_flow.Openai_chatgpt.revoke ~http ~sw config secret)

let openai_device_flow ~http ~sw ~now ~auth_base_url =
  match openai_chatgpt_config auth_base_url with
  | Error error -> Error error
  | Ok config ->
      Oauth_flow.Device_code.start_openai_chatgpt ~http ~sw ~now config

let openai_provider_defined id =
  if Login.Id.equal id openai_chatgpt_protocol_id then Some openai_device_flow
  else None

let no_provider_defined _ = None

(* Build clients. *)

let missing provider = Error (Error.Missing_credential provider)

let http_config ~provider make =
  match make () with
  | config -> Ok config
  | exception Invalid_argument message ->
      Error (Error.Invalid_base_url { provider; message })

let openai_build ~sw ~env ?base_url credential =
  Eio.Switch.check sw;
  match credential with
  | None -> missing Mentat_llm_openai.provider
  | Some credential ->
      let api_route credential =
        Result.map
          (fun config -> Mentat_llm_openai.client ~env ~config ~credential ())
          (http_config ~provider:Mentat_llm_openai.provider (fun () ->
               Mentat_llm_openai.Config.make ?base_url ()))
      in
      Secret.expose
        (Credential.secret credential)
        ~api_key:(fun ~key ->
          api_route (Mentat_llm_openai.Credential.api_key key))
        ~bearer:(fun ~token ->
          api_route (Mentat_llm_openai.Credential.bearer token))
        ~oauth:(fun ~access_token ~refresh_token:_ ~expires_at:_ ~account_id ->
          let base_url =
            effective_base_url ~default:chatgpt_base_url base_url
          in
          Result.map
            (fun config ->
              Mentat_llm_openai.client ~env ~config
                ~credential:(Mentat_llm_openai.Credential.bearer access_token)
                ())
            (http_config ~provider:Mentat_llm_openai.provider (fun () ->
                 Mentat_llm_openai.Config.make ~base_url
                   ~headers:(chatgpt_account_headers account_id)
                   ())))

let anthropic_build ~sw ~env ?base_url credential =
  Eio.Switch.check sw;
  match credential with
  | None -> missing Mentat_llm_anthropic.provider
  | Some credential ->
      (* No Anthropic OAuth flow exists: an OAuth secret would ride as a static
         bearer that nothing refreshes, so the build refuses it. *)
      Secret.expose
        (Credential.secret credential)
        ~api_key:(fun ~key ->
          Result.map
            (fun config ->
              Mentat_llm_anthropic.client ~env ~config
                ~credential:(Mentat_llm_anthropic.Credential.api_key key)
                ())
            (http_config ~provider:Mentat_llm_anthropic.provider (fun () ->
                 Mentat_llm_anthropic.Config.make ?base_url ())))
        ~bearer:(fun ~token ->
          Result.map
            (fun config ->
              Mentat_llm_anthropic.client ~env ~config
                ~credential:(Mentat_llm_anthropic.Credential.bearer token)
                ())
            (http_config ~provider:Mentat_llm_anthropic.provider (fun () ->
                 Mentat_llm_anthropic.Config.make ?base_url ())))
        ~oauth:(fun
            ~access_token:_ ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
          missing Mentat_llm_anthropic.provider)

let google_build ~sw ~env ?base_url credential =
  Eio.Switch.check sw;
  match credential with
  | None -> missing Mentat_llm_google.provider
  | Some credential ->
      Secret.expose
        (Credential.secret credential)
        ~api_key:(fun ~key ->
          Result.map
            (fun config ->
              Mentat_llm_google.client ~env ~config
                ~credential:(Mentat_llm_google.Credential.api_key key)
                ())
            (http_config ~provider:Mentat_llm_google.provider (fun () ->
                 Mentat_llm_google.Config.make ?base_url ())))
        ~bearer:(fun ~token:_ -> missing Mentat_llm_google.provider)
        ~oauth:(fun
            ~access_token:_ ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
          missing Mentat_llm_google.provider)

(* Local runners have no HTTP endpoint, so a base-URL override is meaningless and
   is ignored rather than erroring; they are credentialless. *)
let local_build ~sw ~env ?base_url:_ _credential =
  Ok (Mentat_llm_local.client ~sw ~env ~http:(Tls_setup.web_http_client env) ())

let ollama_build ~sw ~env ?base_url credential =
  Eio.Switch.check sw;
  let config =
    http_config ~provider:Mentat_llm_ollama.provider (fun () ->
        Mentat_llm_ollama.Config.make ?base_url ())
  in
  let credential =
    match credential with
    | None -> Ok None
    | Some credential ->
        Secret.expose
          (Credential.secret credential)
          ~api_key:(fun ~key ->
            Ok (Some (Mentat_llm_ollama.Credential.api_key key)))
          ~bearer:(fun ~token ->
            Ok (Some (Mentat_llm_ollama.Credential.bearer token)))
          ~oauth:(fun
              ~access_token:_ ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
            missing Mentat_llm_ollama.provider)
  in
  match (config, credential) with
  | (Error _ as error), _ | _, (Error _ as error) -> error
  | Ok config, Ok credential ->
      Ok (Mentat_llm_ollama.client ~env ~config ?credential ())

let opencode_build ~sw ~env ?base_url credential =
  Eio.Switch.check sw;
  match credential with
  | None -> missing Mentat_llm_opencode.provider
  | Some credential ->
      (* Every accepted credential kind rides one bearer header; kind drives
         store and refresh semantics, not header spelling. *)
      let route mk =
        Result.map
          (fun config ->
            Mentat_llm_opencode.client ~env ~config ~credential:mk ())
          (http_config ~provider:Mentat_llm_opencode.provider (fun () ->
               Mentat_llm_opencode.Config.make ?base_url ()))
      in
      Secret.expose
        (Credential.secret credential)
        ~api_key:(fun ~key -> route (Mentat_llm_opencode.Credential.api_key key))
        ~bearer:(fun ~token -> route (Mentat_llm_opencode.Credential.bearer token))
        ~oauth:(fun ~access_token ~refresh_token:_ ~expires_at:_ ~account_id:_ ->
          route (Mentat_llm_opencode.Credential.bearer access_token))

(* Local model artifacts. *)

let local_artifact =
  let status model =
    let id = Mentat_llm.Model.id model in
    match Mentat_llm_local.Artifact.status id with
    | Error message -> Some (Artifact.Status.Unavailable { message })
    | Ok (Mentat_llm_local.Artifact.Installed { path }) ->
        Some (Artifact.Status.Installed { path })
    | Ok (Mentat_llm_local.Artifact.Missing { path; url; size }) ->
        Some
          (Artifact.Status.Missing { path; size = Some size; source = Some url })
    | Ok (Mentat_llm_local.Artifact.Explicit_path { exists = true; path }) ->
        Some (Artifact.Status.Installed { path })
    | Ok (Mentat_llm_local.Artifact.Explicit_path { exists = false; path }) ->
        Some (Artifact.Status.Missing { path; size = None; source = None })
  in
  let map_progress observe (progress : Mentat_llm_local.Download.progress) =
    let phase =
      match progress.Mentat_llm_local.Download.phase with
      | Mentat_llm_local.Download.Checking -> Artifact.Progress.Checking
      | Mentat_llm_local.Download.Downloading -> Artifact.Progress.Downloading
      | Mentat_llm_local.Download.Verifying -> Artifact.Progress.Verifying
      | Mentat_llm_local.Download.Installed -> Artifact.Progress.Ready
    in
    observe
      {
        Artifact.Progress.provider = Mentat_llm_local.provider;
        model = progress.Mentat_llm_local.Download.model;
        label = progress.Mentat_llm_local.Download.label;
        received = progress.Mentat_llm_local.Download.received;
        total = progress.Mentat_llm_local.Download.total;
        phase;
      }
  in
  let prepare ~sw ~env ~cancelled ~observe model =
    let id = Mentat_llm.Model.id model in
    Mentat_llm_local.Artifact.prepare ~sw ~env
      ~http:(Tls_setup.web_http_client env)
      ~cancelled ~observe_download:(map_progress observe) id
  in
  let download ~sw ~env ~force ~observe model =
    let id = Mentat_llm.Model.id model in
    match Mentat_llm_local.Artifact.status id with
    | Error message ->
        Artifact.Download_outcome.Refused { message; force_hint = false }
    | Ok (Mentat_llm_local.Artifact.Installed { path }) ->
        Artifact.Download_outcome.Already_installed path
    | Ok (Mentat_llm_local.Artifact.Explicit_path _) ->
        Artifact.Download_outcome.Not_downloadable
    | Ok (Mentat_llm_local.Artifact.Missing _) -> (
        match
          Mentat_llm_local.Artifact.prepare ~sw ~env
            ~http:(Tls_setup.web_http_client env)
            ~cancelled:(fun () -> false)
            ~observe_download:(map_progress observe) ~force id
        with
        | Ok () -> Artifact.Download_outcome.Downloaded
        | Error error ->
            (* The memory-budget guard is the only force-recoverable failure. *)
            let force_hint =
              match Mentat_llm.Error.kind error with
              | Mentat_llm.Error.Unsupported -> not force
              | _ -> false
            in
            Artifact.Download_outcome.Refused
              { message = Mentat_llm.Error.message error; force_hint })
  in
  { Driver.status; prepare; download }

(* The registration list. *)

let registrations =
  [
    {
      Driver.declaration = openai_declaration;
      driver =
        {
          Driver.build_client = openai_build;
          check = Some openai_check;
          refresh = Some openai_refresh;
          revoke = Some openai_revoke;
          provider_defined = openai_provider_defined;
          artifact = None;
        };
    };
    {
      Driver.declaration = anthropic_declaration;
      driver =
        {
          Driver.build_client = anthropic_build;
          check = Some anthropic_check;
          refresh = None;
          revoke = None;
          provider_defined = no_provider_defined;
          artifact = None;
        };
    };
    {
      Driver.declaration = google_declaration;
      driver =
        {
          Driver.build_client = google_build;
          check = Some google_check;
          refresh = None;
          revoke = None;
          provider_defined = no_provider_defined;
          artifact = None;
        };
    };
    {
      Driver.declaration = local_declaration;
      driver =
        {
          Driver.build_client = local_build;
          check = None;
          refresh = None;
          revoke = None;
          provider_defined = no_provider_defined;
          artifact = Some local_artifact;
        };
    };
    {
      Driver.declaration = ollama_declaration;
      driver =
        {
          Driver.build_client = ollama_build;
          check = Some ollama_check;
          refresh = None;
          revoke = None;
          provider_defined = no_provider_defined;
          artifact = None;
        };
    };
    {
      Driver.declaration = opencode_declaration;
      driver =
        {
          Driver.build_client = opencode_build;
          check = Some opencode_check;
          refresh = None;
          revoke = None;
          provider_defined = no_provider_defined;
          artifact = None;
        };
    };
  ]
