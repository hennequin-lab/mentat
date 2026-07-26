(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* House helpers — keep the decode pair byte-identical across lib/*/import.ml
   copies. *)

let decode_error message = Jsont.Error.msg Jsont.Meta.none message

let decode_invalid_arg f =
  match f () with
  | value -> value
  | exception Invalid_argument message -> decode_error message

module String_map = Map.Make (String)

let strict_string_map ~kind codec =
  let dec_empty () = String_map.empty in
  let dec_add _meta name value members =
    if String_map.mem name members then
      decode_error (kind ^ " has duplicate member " ^ name);
    String_map.add name value members
  in
  let dec_finish _meta members = String_map.bindings members in
  let enc =
    {
      Jsont.Object.Mems.enc =
        (fun add members acc ->
          List.fold_left
            (fun acc (name, value) -> add Jsont.Meta.none name value acc)
            acc members);
    }
  in
  let mems =
    Jsont.Object.Mems.map ~kind ~dec_empty ~dec_add ~dec_finish ~enc codec
  in
  Jsont.Object.map ~kind Fun.id
  |> Jsont.Object.keep_unknown mems ~enc:Fun.id
  |> Jsont.Object.finish
