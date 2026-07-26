(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The two client-owned flow codecs deferred "as unrepresentable" until a
   transport carried them. This is that transport, so
   they are minted here: [compaction_result] (the [compact] flow's outcome) and
   [login_step] (each step of the interactive login stream). They are the
   library's own wire deliverable, pinned by the golden corpus. *)

let compaction_result : Mentat_client.compaction_result Jsont.t =
  Jsont.enum ~kind:"compaction result"
    [
      ("installed", Mentat_client.Installed); ("skipped", Mentat_client.Skipped);
    ]

let login_progress : Mentat_provider.Auth.Login.Progress.t Jsont.t =
  let browser_url =
    Jsont.Object.map ~kind:"browser url" (fun url ->
        Mentat_provider.Auth.Login.Progress.Browser_url url)
    |> Jsont.Object.mem "url" Codecs.uri ~enc:(function
      | Mentat_provider.Auth.Login.Progress.Browser_url url -> url
      | _ -> assert false)
    |> Jsont.Object.finish
    |> Jsont.Object.Case.map "browser_url" ~dec:Fun.id
  in
  let listening =
    Jsont.Object.map ~kind:"listening" (fun redirect_uri ->
        Mentat_provider.Auth.Login.Progress.Listening { redirect_uri })
    |> Jsont.Object.mem "redirect_uri" Codecs.uri ~enc:(function
      | Mentat_provider.Auth.Login.Progress.Listening { redirect_uri } ->
          redirect_uri
      | _ -> assert false)
    |> Jsont.Object.finish
    |> Jsont.Object.Case.map "listening" ~dec:Fun.id
  in
  let device_challenge =
    Jsont.Object.map ~kind:"device challenge" (fun url user_code expires_in ->
        Mentat_provider.Auth.Login.Progress.Device_challenge
          { url; user_code; expires_in })
    |> Jsont.Object.mem "url" Codecs.uri ~enc:(function
      | Mentat_provider.Auth.Login.Progress.Device_challenge { url; _ } -> url
      | _ -> assert false)
    |> Jsont.Object.mem "user_code" Jsont.string ~enc:(function
      | Mentat_provider.Auth.Login.Progress.Device_challenge { user_code; _ } ->
          user_code
      | _ -> assert false)
    |> Jsont.Object.mem "expires_in" Jsont.int ~enc:(function
      | Mentat_provider.Auth.Login.Progress.Device_challenge { expires_in; _ }
        ->
          expires_in
      | _ -> assert false)
    |> Jsont.Object.finish
    |> Jsont.Object.Case.map "device_challenge" ~dec:Fun.id
  in
  let cases =
    List.map Jsont.Object.Case.make [ browser_url; listening; device_challenge ]
  in
  let enc_case = function
    | Mentat_provider.Auth.Login.Progress.Browser_url _ as p ->
        Jsont.Object.Case.value browser_url p
    | Mentat_provider.Auth.Login.Progress.Listening _ as p ->
        Jsont.Object.Case.value listening p
    | Mentat_provider.Auth.Login.Progress.Device_challenge _ as p ->
        Jsont.Object.Case.value device_challenge p
  in
  Jsont.Object.map ~kind:"login progress" Fun.id
  |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.finish

let login_step : Mentat_client.Login.step Jsont.t =
  let progress =
    Jsont.Object.map ~kind:"login progress step" (fun p ->
        Mentat_client.Login.Progress p)
    |> Jsont.Object.mem "progress" login_progress ~enc:(function
      | Mentat_client.Login.Progress p -> p
      | _ -> assert false)
    |> Jsont.Object.finish
    |> Jsont.Object.Case.map "progress" ~dec:Fun.id
  in
  let saved =
    Jsont.Object.map ~kind:"login saved step" (fun a ->
        Mentat_client.Login.Saved a)
    |> Jsont.Object.mem "account" Mentat_provider.Account.jsont ~enc:(function
      | Mentat_client.Login.Saved a -> a
      | _ -> assert false)
    |> Jsont.Object.finish
    |> Jsont.Object.Case.map "saved" ~dec:Fun.id
  in
  let cancelled =
    Jsont.Object.map ~kind:"login cancelled step" Mentat_client.Login.Cancelled
    |> Jsont.Object.finish
    |> Jsont.Object.Case.map "cancelled" ~dec:Fun.id
  in
  let cases = List.map Jsont.Object.Case.make [ progress; saved; cancelled ] in
  let enc_case = function
    | Mentat_client.Login.Progress _ as s -> Jsont.Object.Case.value progress s
    | Mentat_client.Login.Saved _ as s -> Jsont.Object.Case.value saved s
    | Mentat_client.Login.Cancelled as s -> Jsont.Object.Case.value cancelled s
  in
  Jsont.Object.map ~kind:"login step" Fun.id
  |> Jsont.Object.case_mem "step" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.finish
