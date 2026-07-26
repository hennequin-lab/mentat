(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

type reason =
  | Superseded of { path : Mentat_workspace.Path.t; by : Change.Id.t }
  | Non_contiguous of { path : Mentat_workspace.Path.t }
  | Observed of { claim : Mentat_session.Tool_claim.Id.t }
  | Uncertain of {
      claim : Mentat_session.Tool_claim.Id.t;
      path : Mentat_workspace.Path.t;
    }
  | Unresolved_revert of Revert_data.Id.t

type t = Available | Unavailable of reason list | Incomplete of reason list

let reason_equal a b =
  match (a, b) with
  | Superseded a, Superseded b ->
      Mentat_workspace.Path.equal a.path b.path && Change.Id.equal a.by b.by
  | Non_contiguous a, Non_contiguous b ->
      Mentat_workspace.Path.equal a.path b.path
  | Observed a, Observed b -> Mentat_session.Tool_claim.Id.equal a.claim b.claim
  | Uncertain a, Uncertain b ->
      Mentat_session.Tool_claim.Id.equal a.claim b.claim
      && Mentat_workspace.Path.equal a.path b.path
  | Unresolved_revert a, Unresolved_revert b -> Revert_data.Id.equal a b
  | ( ( Superseded _ | Non_contiguous _ | Observed _ | Uncertain _
      | Unresolved_revert _ ),
      _ ) ->
      false

let reason_message = function
  | Superseded { path; by } ->
      Format.asprintf "%a was later changed by %a" Mentat_workspace.Path.pp path
        Change.Id.pp by
  | Non_contiguous { path } ->
      Format.asprintf "%a has unrecorded interleaved changes"
        Mentat_workspace.Path.pp path
  | Observed { claim } ->
      Format.asprintf
        "paths changed during tool claim %a without recorded transitions"
        Mentat_session.Tool_claim.Id.pp claim
  | Uncertain { claim; path } ->
      Format.asprintf
        "a recorded edit on %a under tool claim %a has an uncertain outcome"
        Mentat_workspace.Path.pp path Mentat_session.Tool_claim.Id.pp claim
  | Unresolved_revert id ->
      Format.asprintf "revert %a started but never settled" Revert_data.Id.pp id

let message = function
  | Available -> "revert available"
  | Unavailable reasons ->
      "revert unavailable: "
      ^ String.concat "; " (List.map reason_message reasons)
  | Incomplete reasons ->
      "revert evidence incomplete: "
      ^ String.concat "; " (List.map reason_message reasons)

let equal a b =
  match (a, b) with
  | Available, Available -> true
  | Unavailable a, Unavailable b -> List.equal reason_equal a b
  | Incomplete a, Incomplete b -> List.equal reason_equal a b
  | (Available | Unavailable _ | Incomplete _), _ -> false

let pp ppf t = Format.pp_print_string ppf (message t)

let reason_jsont =
  let superseded_case =
    Jsont.Object.map ~kind:"superseded reason" (fun path by ->
        Superseded { path; by })
    |> Jsont.Object.mem "path" Mentat_workspace.Path.jsont ~enc:(function
      | Superseded { path; _ } -> path
      | Non_contiguous _ | Observed _ | Uncertain _ | Unresolved_revert _ ->
          assert false)
    |> Jsont.Object.mem "by" Change.Id.jsont ~enc:(function
      | Superseded { by; _ } -> by
      | Non_contiguous _ | Observed _ | Uncertain _ | Unresolved_revert _ ->
          assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "superseded" ~dec:Fun.id
  in
  let non_contiguous_case =
    Jsont.Object.map ~kind:"non-contiguous reason" (fun path ->
        Non_contiguous { path })
    |> Jsont.Object.mem "path" Mentat_workspace.Path.jsont ~enc:(function
      | Non_contiguous { path } -> path
      | Superseded _ | Observed _ | Uncertain _ | Unresolved_revert _ ->
          assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "non_contiguous" ~dec:Fun.id
  in
  let observed_case =
    Jsont.Object.map ~kind:"observed reason" (fun claim -> Observed { claim })
    |> Jsont.Object.mem "claim" Mentat_session.Tool_claim.Id.jsont
         ~enc:(function
         | Observed { claim } -> claim
         | Superseded _ | Non_contiguous _ | Uncertain _ | Unresolved_revert _
           ->
             assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "observed" ~dec:Fun.id
  in
  let uncertain_case =
    Jsont.Object.map ~kind:"uncertain reason" (fun claim path ->
        Uncertain { claim; path })
    |> Jsont.Object.mem "claim" Mentat_session.Tool_claim.Id.jsont
         ~enc:(function
         | Uncertain { claim; _ } -> claim
         | Superseded _ | Non_contiguous _ | Observed _ | Unresolved_revert _ ->
             assert false)
    |> Jsont.Object.mem "path" Mentat_workspace.Path.jsont ~enc:(function
      | Uncertain { path; _ } -> path
      | Superseded _ | Non_contiguous _ | Observed _ | Unresolved_revert _ ->
          assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "uncertain" ~dec:Fun.id
  in
  let unresolved_case =
    Jsont.Object.map ~kind:"unresolved revert reason" (fun revert ->
        Unresolved_revert revert)
    |> Jsont.Object.mem "revert" Revert_data.Id.jsont ~enc:(function
      | Unresolved_revert revert -> revert
      | Superseded _ | Non_contiguous _ | Observed _ | Uncertain _ ->
          assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "unresolved_revert" ~dec:Fun.id
  in
  let cases =
    List.map Jsont.Object.Case.make
      [
        superseded_case;
        non_contiguous_case;
        observed_case;
        uncertain_case;
        unresolved_case;
      ]
  in
  let enc_case = function
    | Superseded _ as reason -> Jsont.Object.Case.value superseded_case reason
    | Non_contiguous _ as reason ->
        Jsont.Object.Case.value non_contiguous_case reason
    | Observed _ as reason -> Jsont.Object.Case.value observed_case reason
    | Uncertain _ as reason -> Jsont.Object.Case.value uncertain_case reason
    | Unresolved_revert _ as reason ->
        Jsont.Object.Case.value unresolved_case reason
  in
  Jsont.Object.map ~kind:"revertability reason" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let jsont =
  let non_empty reasons =
    match reasons with
    | [] -> decode_error "reasons must not be empty"
    | _ :: _ -> reasons
  in
  let available_case =
    Jsont.Object.map ~kind:"available revertability" Available
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "available" ~dec:Fun.id
  in
  let unavailable_case =
    Jsont.Object.map ~kind:"unavailable revertability" (fun reasons ->
        Unavailable (non_empty reasons))
    |> Jsont.Object.mem "reasons" (Jsont.list reason_jsont) ~enc:(function
      | Unavailable reasons -> reasons
      | Available | Incomplete _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "unavailable" ~dec:Fun.id
  in
  let incomplete_case =
    Jsont.Object.map ~kind:"incomplete revertability" (fun reasons ->
        Incomplete (non_empty reasons))
    |> Jsont.Object.mem "reasons" (Jsont.list reason_jsont) ~enc:(function
      | Incomplete reasons -> reasons
      | Available | Unavailable _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "incomplete" ~dec:Fun.id
  in
  let cases =
    List.map Jsont.Object.Case.make
      [ available_case; unavailable_case; incomplete_case ]
  in
  let enc_case = function
    | Available -> Jsont.Object.Case.value available_case Available
    | Unavailable _ as answer -> Jsont.Object.Case.value unavailable_case answer
    | Incomplete _ as answer -> Jsont.Object.Case.value incomplete_case answer
  in
  Jsont.Object.map ~kind:"revertability" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
