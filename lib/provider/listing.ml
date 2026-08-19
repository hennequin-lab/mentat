(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message =
  invalid_arg ("Mentat_provider.Listing." ^ fn ^ ": " ^ message)

let check_non_empty fn field value =
  if String.is_empty value then invalid fn (field ^ " must not be empty")

let check_optional_non_empty fn field = function
  | None -> ()
  | Some value -> check_non_empty fn field value

let check_optional_positive fn field = function
  | None -> ()
  | Some value -> if value <= 0 then invalid fn (field ^ " must be positive")

let check_optional_no_duplicates fn field compare = function
  | None -> ()
  | Some values ->
      let rec loop = function
        | first :: second :: rest ->
            if compare first second = 0 then
              invalid fn (field ^ " contain duplicates");
            loop (second :: rest)
        | [] | [ _ ] -> ()
      in
      loop (List.sort compare values)

module Model = struct
  type t = {
    id : string;
    api : Mentat_llm.Model.Api.t option;
    display_name : string option;
    family : string option;
    context_window : int option;
    max_output_tokens : int option;
    pricing : Model.Pricing.t option;
    input_modalities : Model.Modality.t list option;
    capabilities : Model.Capability.t list option;
    status : Model.status option;
  }

  let make ~id ?api ?display_name ?family ?context_window ?max_output_tokens
      ?pricing ?input_modalities ?capabilities ?status () =
    check_non_empty "Model.make" "id" id;
    check_optional_non_empty "Model.make" "display_name" display_name;
    check_optional_non_empty "Model.make" "family" family;
    check_optional_positive "Model.make" "context_window" context_window;
    check_optional_positive "Model.make" "max_output_tokens" max_output_tokens;
    check_optional_no_duplicates "Model.make" "input_modalities"
      Model.Modality.compare input_modalities;
    check_optional_no_duplicates "Model.make" "capabilities"
      Model.Capability.compare capabilities;
    {
      id;
      api;
      display_name;
      family;
      context_window;
      max_output_tokens;
      pricing;
      input_modalities;
      capabilities;
      status;
    }

  let of_id id = make ~id ()
  let id t = t.id
  let api t = t.api
  let display_name t = t.display_name
  let family t = t.family
  let context_window t = t.context_window
  let max_output_tokens t = t.max_output_tokens
  let pricing t = t.pricing
  let input_modalities t = t.input_modalities
  let capabilities t = t.capabilities
  let status t = t.status

  let equal_status a b =
    match (a, b) with
    | Model.Stable, Model.Stable
    | Model.Preview, Model.Preview
    | Model.Deprecated, Model.Deprecated ->
        true
    | Model.Unavailable a, Model.Unavailable b -> String.equal a b
    | ( (Model.Stable | Model.Preview | Model.Deprecated | Model.Unavailable _),
        _ ) ->
        false

  let equal a b =
    String.equal a.id b.id
    && Option.equal Mentat_llm.Model.Api.equal a.api b.api
    && Option.equal String.equal a.display_name b.display_name
    && Option.equal String.equal a.family b.family
    && Option.equal Int.equal a.context_window b.context_window
    && Option.equal Int.equal a.max_output_tokens b.max_output_tokens
    && Option.equal Model.Pricing.equal a.pricing b.pricing
    && Option.equal
         (List.equal Model.Modality.equal)
         a.input_modalities b.input_modalities
    && Option.equal
         (List.equal Model.Capability.equal)
         a.capabilities b.capabilities
    && Option.equal equal_status a.status b.status

  let pp ppf t =
    Format.fprintf ppf "@[<2>{id=%s%a}@]" t.id
      Format.(
        pp_print_option (fun ppf api ->
            fprintf ppf "; api=%a" Mentat_llm.Model.Api.pp api))
      t.api
end

type t = { models : Model.t list }

let make models =
  let rec check_unique seen = function
    | [] -> ()
    | model :: rest ->
        let id = Model.id model in
        if List.exists (String.equal id) seen then
          invalid "make" ("duplicate model id " ^ id);
        check_unique (id :: seen) rest
  in
  check_unique [] models;
  { models }

let of_ids ids = make (List.map Model.of_id ids)
let models t = t.models
let ids t = List.map Model.id t.models
let mem t id = List.exists (fun model -> String.equal id (Model.id model)) t.models
let find t id = List.find_opt (fun model -> String.equal id (Model.id model)) t.models
let equal a b = List.equal Model.equal a.models b.models

let pp ppf t =
  Format.fprintf ppf "@[<2>{models=[%a]}@]"
    Format.(
      pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf "; ") Model.pp)
    t.models
