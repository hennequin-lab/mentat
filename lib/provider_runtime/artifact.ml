(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Status = struct
  type t =
    | Installed of { path : string }
    | Missing of { path : string; size : int64 option; source : string option }
    | Unavailable of { message : string }

  let bytes_text bytes =
    let value = Int64.to_float bytes in
    if value >= 1_000_000_000. then
      Printf.sprintf "%.1f GB" (value /. 1_000_000_000.)
    else if value >= 1_000_000. then
      Printf.sprintf "%.1f MB" (value /. 1_000_000.)
    else if value >= 1_000. then Printf.sprintf "%.1f KB" (value /. 1_000.)
    else Printf.sprintf "%Ld B" bytes

  let summary = function
    | Installed { path } -> "installed: " ^ path
    | Missing { size = None; _ } -> "missing - auto-download"
    | Missing { size = Some size; _ } ->
        "missing - auto-download " ^ bytes_text size
    | Unavailable { message } -> "unavailable: " ^ message

  let jsont =
    let installed =
      Jsont.Object.map ~kind:"installed artifact" (fun path ->
          Installed { path })
      |> Jsont.Object.mem "path" Jsont.string ~enc:(function
        | Installed { path } -> path
        | Missing _ | Unavailable _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "installed" ~dec:Fun.id
    in
    let missing =
      Jsont.Object.map ~kind:"missing artifact" (fun path size source ->
          Missing { path; size; source })
      |> Jsont.Object.mem "path" Jsont.string ~enc:(function
        | Missing { path; _ } -> path
        | Installed _ | Unavailable _ -> assert false)
      |> Jsont.Object.opt_mem "size" Jsont.int64 ~enc:(function
        | Missing { size; _ } -> size
        | Installed _ | Unavailable _ -> assert false)
      |> Jsont.Object.opt_mem "source" Jsont.string ~enc:(function
        | Missing { source; _ } -> source
        | Installed _ | Unavailable _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "missing" ~dec:Fun.id
    in
    let unavailable =
      Jsont.Object.map ~kind:"unavailable artifact" (fun message ->
          Unavailable { message })
      |> Jsont.Object.mem "message" Jsont.string ~enc:(function
        | Unavailable { message } -> message
        | Installed _ | Missing _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "unavailable" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make [ installed; missing; unavailable ]
    in
    let enc_case = function
      | Installed _ as s -> Jsont.Object.Case.value installed s
      | Missing _ as s -> Jsont.Object.Case.value missing s
      | Unavailable _ as s -> Jsont.Object.Case.value unavailable s
    in
    Jsont.Object.map ~kind:"artifact status" Fun.id
    |> Jsont.Object.case_mem "state" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Progress = struct
  type phase = Checking | Downloading | Verifying | Ready

  type t = {
    provider : Mentat_llm.Provider.t;
    model : string;
    label : string;
    received : int64;
    total : int64 option;
    phase : phase;
  }

  let phase_jsont =
    Jsont.enum ~kind:"artifact phase"
      [
        ("checking", Checking);
        ("downloading", Downloading);
        ("verifying", Verifying);
        ("ready", Ready);
      ]

  let jsont =
    Jsont.Object.map ~kind:"artifact progress"
      (fun provider model label received total phase ->
        { provider; model; label; received; total; phase })
    |> Jsont.Object.mem "provider" Mentat_llm.Provider.jsont ~enc:(fun p ->
        p.provider)
    |> Jsont.Object.mem "model" Jsont.string ~enc:(fun p -> p.model)
    |> Jsont.Object.mem "label" Jsont.string ~enc:(fun p -> p.label)
    |> Jsont.Object.mem "received" Jsont.int64 ~enc:(fun p -> p.received)
    |> Jsont.Object.opt_mem "total" Jsont.int64 ~enc:(fun p -> p.total)
    |> Jsont.Object.mem "phase" phase_jsont ~enc:(fun p -> p.phase)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Download_outcome = struct
  type t =
    | Already_installed of string
    | Not_downloadable
    | Downloaded
    | Refused of { message : string; force_hint : bool }

  let jsont =
    let already =
      Jsont.Object.map ~kind:"already installed" (fun path ->
          Already_installed path)
      |> Jsont.Object.mem "path" Jsont.string ~enc:(function
        | Already_installed path -> path
        | Not_downloadable | Downloaded | Refused _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "already_installed" ~dec:Fun.id
    in
    let not_downloadable =
      Jsont.Object.map ~kind:"not downloadable" Not_downloadable
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "not_downloadable" ~dec:Fun.id
    in
    let downloaded =
      Jsont.Object.map ~kind:"downloaded" Downloaded
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "downloaded" ~dec:Fun.id
    in
    let refused =
      Jsont.Object.map ~kind:"refused download" (fun message force_hint ->
          Refused { message; force_hint })
      |> Jsont.Object.mem "message" Jsont.string ~enc:(function
        | Refused { message; _ } -> message
        | Already_installed _ | Not_downloadable | Downloaded -> assert false)
      |> Jsont.Object.mem "force_hint" Jsont.bool ~enc:(function
        | Refused { force_hint; _ } -> force_hint
        | Already_installed _ | Not_downloadable | Downloaded -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "refused" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make
        [ already; not_downloadable; downloaded; refused ]
    in
    let enc_case = function
      | Already_installed _ as o -> Jsont.Object.Case.value already o
      | Not_downloadable as o -> Jsont.Object.Case.value not_downloadable o
      | Downloaded as o -> Jsont.Object.Case.value downloaded o
      | Refused _ as o -> Jsont.Object.Case.value refused o
    in
    Jsont.Object.map ~kind:"artifact download outcome" Fun.id
    |> Jsont.Object.case_mem "outcome" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end
