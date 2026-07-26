(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Provider_map = Map.Make (struct
  type t = Mentat_llm.Provider.t

  let compare = Mentat_llm.Provider.compare
end)

module Error = struct
  type t =
    | Invalid_selector of {
        input : string;
        error : Selector.Error.t;
        candidates : Selector.t list;
      }
    | Unknown_provider of {
        provider : Mentat_llm.Provider.t;
        known : Mentat_llm.Provider.t list;
      }
    | Unknown_model of {
        provider : Mentat_llm.Provider.t;
        model : string;
        known : string list;
      }

  let join to_string = function
    | [] -> "none"
    | values -> String.concat ", " (List.map to_string values)

  let message = function
    | Invalid_selector { input; error; candidates } ->
        Printf.sprintf "model selector %S is invalid: %s; known models: %s"
          input
          (Selector.Error.message error)
          (join Selector.to_string candidates)
    | Unknown_provider { provider; known } ->
        Printf.sprintf "unknown provider %S; known providers: %s"
          (Mentat_llm.Provider.id provider)
          (join Mentat_llm.Provider.id known)
    | Unknown_model { provider; model; known } ->
        Printf.sprintf "unknown model %S for provider %S; known models: %s"
          model
          (Mentat_llm.Provider.id provider)
          (join Fun.id known)

  let pp ppf error = Format.pp_print_string ppf (message error)
end

type t = {
  declarations : Declaration.t list;
  by_id : Declaration.t Provider_map.t;
}

let make declarations =
  let by_id =
    List.fold_left
      (fun by_id declaration ->
        let id = Declaration.id declaration in
        if Provider_map.mem id by_id then
          invalid_arg
            ("Mentat_provider.Catalog.make: duplicate provider "
           ^ Mentat_llm.Provider.id id);
        Provider_map.add id declaration by_id)
      Provider_map.empty declarations
  in
  { declarations; by_id }

let declarations t = t.declarations
let declaration t provider = Provider_map.find_opt provider t.by_id
let known_providers t = List.map Declaration.id t.declarations

let visible_models ~include_hidden models =
  if include_hidden then models else List.filter Model.visible models

let models ?(include_hidden = false) t =
  t.declarations
  |> List.concat_map Declaration.models
  |> visible_models ~include_hidden

let models_for ?(include_hidden = false) t provider =
  match declaration t provider with
  | None ->
      Error (Error.Unknown_provider { provider; known = known_providers t })
  | Some declaration ->
      Ok (visible_models ~include_hidden (Declaration.models declaration))

let model_by_id declaration id =
  List.find_opt
    (fun model -> String.equal id (Model.id model))
    (Declaration.models declaration)

let find t selector =
  let provider = Selector.provider selector in
  match declaration t provider with
  | None ->
      Error (Error.Unknown_provider { provider; known = known_providers t })
  | Some declaration -> (
      let model_id = Selector.id selector in
      match model_by_id declaration model_id with
      | Some model -> Ok model
      | None -> (
          match Declaration.resolve_dynamic declaration model_id with
          | Some model -> Ok model
          | None ->
              Error
                (Error.Unknown_model
                   {
                     provider;
                     model = model_id;
                     known = List.map Model.id (Declaration.models declaration);
                   })))

let model_selectors t = List.map Model.selector (models ~include_hidden:true t)

let invalid_selector_candidates t input = function
  | Selector.Error.Empty -> []
  | Selector.Error.Missing_separator ->
      let model_id = String.trim input in
      let exact =
        models ~include_hidden:true t
        |> List.filter (fun model -> String.equal model_id (Model.id model))
        |> List.map Model.selector
      in
      if List.is_empty exact then model_selectors t else exact
  | Selector.Error.Empty_provider | Selector.Error.Empty_model
  | Selector.Error.Invalid_provider _ ->
      model_selectors t

let resolve t input =
  match Selector.of_string input with
  | Ok selector -> find t selector
  | Error error ->
      Error
        (Error.Invalid_selector
           {
             input;
             error;
             candidates = invalid_selector_candidates t input error;
           })
