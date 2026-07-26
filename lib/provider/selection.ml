(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let reasoning_rank effort =
  let open Mentat_llm.Request.Options.Reasoning_effort in
  match effort with
  | Disabled -> 0
  | Minimal -> 1
  | Low -> 2
  | Medium -> 3
  | High -> 4
  | Extra_high -> 5
  | Max -> 6

let reasoning_equal a b = reasoning_rank a = reasoning_rank b

module Requirement = struct
  type t = {
    capabilities : Model.Capability.t list;
    reasoning_effort : Mentat_llm.Request.Options.Reasoning_effort.t option;
  }

  module Mismatch = struct
    type t =
      | Lifecycle of Model.status
      | Missing_capability of Model.Capability.t
      | Unsupported_reasoning of {
          requested : Mentat_llm.Request.Options.Reasoning_effort.t;
          supported : Mentat_llm.Request.Options.Reasoning_effort.t list;
        }

    let status_label = function
      | Model.Stable -> "stable"
      | Model.Preview -> "preview"
      | Model.Deprecated -> "deprecated"
      | Model.Unavailable reason -> Printf.sprintf "unavailable: %s" reason

    let message = function
      | Lifecycle status ->
          Printf.sprintf "model is not selectable (%s)" (status_label status)
      | Missing_capability capability ->
          Printf.sprintf "model does not support capability %s"
            (Model.Capability.to_string capability)
      | Unsupported_reasoning { requested; supported } ->
          let supported =
            match supported with
            | [] -> "none"
            | efforts ->
                String.concat ", "
                  (List.map
                     Mentat_llm.Request.Options.Reasoning_effort.to_string
                     efforts)
          in
          Printf.sprintf
            "model does not support reasoning effort %s (supported: %s)"
            (Mentat_llm.Request.Options.Reasoning_effort.to_string requested)
            supported

    let pp ppf mismatch = Format.pp_print_string ppf (message mismatch)
  end

  let none = { capabilities = []; reasoning_effort = None }

  let make ?(capabilities = []) ?reasoning_effort () =
    { capabilities; reasoning_effort }

  let check t model =
    if not (Model.selectable model) then
      Error (Mismatch.Lifecycle (Model.status model))
    else
      match
        List.find_opt
          (fun c -> not (Model.has_capability c model))
          t.capabilities
      with
      | Some capability -> Error (Mismatch.Missing_capability capability)
      | None -> (
          match t.reasoning_effort with
          | None -> Ok ()
          | Some effort ->
              let supported = Model.supported_reasoning model in
              if
                Model.has_capability Model.Capability.reasoning model
                && List.exists (reasoning_equal effort) supported
              then Ok ()
              else
                Error
                  (Mismatch.Unsupported_reasoning
                     { requested = effort; supported }))
end

module Error = struct
  type t =
    | No_model
    | Requirement_mismatch of {
        selector : Selector.t;
        mismatch : Requirement.Mismatch.t;
        alternative : Selector.t option;
      }

  let message = function
    | No_model -> "no model is available"
    | Requirement_mismatch { selector; mismatch; alternative } -> (
        let base =
          Printf.sprintf "model %s: %s"
            (Selector.to_string selector)
            (Requirement.Mismatch.message mismatch)
        in
        match alternative with
        | None -> base
        | Some alternative ->
            Printf.sprintf "%s; try %s" base (Selector.to_string alternative))

  let pp ppf error = Format.pp_print_string ppf (message error)
end

let eligible requirements model =
  Result.is_ok (Requirement.check requirements model)

(* The same-provider default (else first eligible same-provider model) that
   satisfies the entire requirement set. *)
let same_provider_alternative catalog requirements model =
  match Catalog.declaration catalog (Model.provider model) with
  | None -> None
  | Some declaration ->
      let candidate =
        match Declaration.default_model declaration with
        | Some default when eligible requirements default -> Some default
        | Some _ | None ->
            List.find_opt (eligible requirements)
              (Declaration.models declaration)
      in
      Option.map Model.selector candidate

let resolve_preferred catalog requirements model =
  match Requirement.check requirements model with
  | Ok () -> Ok model
  | Error mismatch ->
      Error
        (Error.Requirement_mismatch
           {
             selector = Model.selector model;
             mismatch;
             alternative = same_provider_alternative catalog requirements model;
           })

let first_eligible_default catalog requirements ~keep =
  Catalog.declarations catalog
  |> List.filter (fun declaration -> keep (Declaration.id declaration))
  |> List.filter_map Declaration.default_model
  |> List.find_opt (eligible requirements)

(* The small role never blocks a run, so an ineligible or absent preferred small
   model silently yields the already-resolved main model rather than an error. *)
let small ~main ?preferred ?requirements () =
  let requirements = Option.value requirements ~default:Requirement.none in
  match preferred with
  | Some model when eligible requirements model -> model
  | Some _ | None -> main

let main ~catalog ~provider_preferred ?preferred ?requirements () =
  let requirements = Option.value requirements ~default:Requirement.none in
  match preferred with
  | Some model -> resolve_preferred catalog requirements model
  | None -> (
      match
        first_eligible_default catalog requirements ~keep:provider_preferred
      with
      | Some model -> Ok model
      | None -> (
          match
            first_eligible_default catalog requirements ~keep:(fun _ -> true)
          with
          | Some model -> Ok model
          | None -> (
              match
                List.find_opt (eligible requirements)
                  (Catalog.models ~include_hidden:true catalog)
              with
              | Some model -> Ok model
              | None -> Error Error.No_model)))
