(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let rec of_json = function
  | Jsont.Object (members, meta) ->
      let compare_member ((a, _), _) ((b, _), _) = String.compare a b in
      let members = List.stable_sort compare_member members in
      let members =
        List.map (fun (name, value) -> (name, of_json value)) members
      in
      Jsont.Object (members, meta)
  | Jsont.Array (elements, meta) -> Jsont.Array (List.map of_json elements, meta)
  | (Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _) as leaf ->
      leaf

let rec equal a b =
  match (a, b) with
  | Jsont.Null _, Jsont.Null _ -> true
  | Jsont.Bool (a, _), Jsont.Bool (b, _) -> Bool.equal a b
  | Jsont.Number (a, _), Jsont.Number (b, _) -> Float.equal a b
  | Jsont.String (a, _), Jsont.String (b, _) -> String.equal a b
  | Jsont.Array (a, _), Jsont.Array (b, _) -> List.equal equal a b
  | Jsont.Object (a, _), Jsont.Object (b, _) ->
      let equal_member ((na, _), va) ((nb, _), vb) =
        String.equal na nb && equal va vb
      in
      List.equal equal_member a b
  | ( ( Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
      | Jsont.Array _ | Jsont.Object _ ),
      _ ) ->
      false
