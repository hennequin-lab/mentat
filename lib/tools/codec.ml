(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let decode_error message = Jsont.Error.msg Jsont.Meta.none message

let strict_object ~kind object_codec =
  let reject_duplicates = function
    | Jsont.Object (members, _) ->
        let names =
          List.map (fun ((name, _), _) -> name) members
          |> List.sort String.compare
        in
        let rec loop = function
          | left :: (right :: _ as names) ->
              if String.equal left right then
                decode_error ("duplicate input member " ^ left)
              else loop names
          | [] | [ _ ] -> ()
        in
        loop names
    | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
    | Jsont.Array _ ->
        ()
  in
  let decode json =
    reject_duplicates json;
    match Jsont.Json.decode object_codec json with
    | Ok value -> value
    | Error diagnostic -> decode_error diagnostic
  in
  let encode value =
    match Jsont.Json.encode object_codec value with
    | Ok json -> json
    | Error diagnostic -> decode_error diagnostic
  in
  Jsont.map ~kind ~dec:decode ~enc:encode Jsont.json

let obj fields =
  Jsont.Json.object'
    (List.map
       (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value)
       fields)
