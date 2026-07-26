(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message =
  invalid_arg ("Mentat_tool.Prepared." ^ fn ^ ": " ^ message)

type t = {
  tool : string;
  input : Jsont.json;
  payload : Jsont.json;
  requests : Mentat_permission.Request.t list;
  description : string;
}

let tool t = t.tool
let description t = t.description
let input t = t.input
let requests t = t.requests

type reconstruction = Undecodable of string | Drift

let reconstruct t jsont =
  match Jsont.Json.decode jsont t.payload with
  | Error message -> Error (Undecodable message)
  | Ok plan -> (
      match Jsont.Json.encode jsont plan with
      | Error message -> Error (Undecodable message)
      | Ok raw ->
          (* Order-significant: a payload that decodes but is not canonical
             (a reordering of the stored form) is drift, and [Jsont.Json.equal]
             would not see it. *)
          if Canonical_json.equal (Canonical_json.of_json raw) t.payload then
            Ok plan
          else Error Drift)

let of_payload ~tool ~input jsont ~describe ~requests plan =
  let ( let* ) = Stdlib.Result.bind in
  let encode value = Jsont.Json.encode jsont value in
  let* raw = encode plan in
  let* reconstructed = Jsont.Json.decode jsont (Canonical_json.of_json raw) in
  let* raw' = encode reconstructed in
  let payload = Canonical_json.of_json raw' in
  let requests_of_plan = requests reconstructed in
  let description = describe reconstructed in
  let candidate =
    { tool; input; payload; requests = requests_of_plan; description }
  in
  (* Prove now what resume will demand later: the stored payload decodes to a
     canonical value with the same requests. A codec that is not decode-stable
     fails this at prepare time — before the user approves — rather than as an
     unexplained [Prepared_drift] after approval. *)
  match reconstruct candidate jsont with
  | Error (Undecodable message) ->
      Error ("prepared payload does not reconstruct: " ^ message)
  | Error Drift ->
      Error "prepared payload does not reconstruct to a canonical value"
  | Ok plan' ->
      if
        List.equal Mentat_permission.Request.equal (requests plan')
          requests_of_plan
      then Ok candidate
      else
        Error "prepared payload reconstructs to different permission requests"

let equal a b =
  String.equal a.tool b.tool
  && Canonical_json.equal a.input b.input
  && Canonical_json.equal a.payload b.payload
  && List.equal Mentat_permission.Request.equal a.requests b.requests
  && String.equal a.description b.description

let version = 1

(* [Jsont.int] accepts truncating floats ([1.9]) and numeric strings (["1"]), so
   the version gate reads the member as raw JSON and requires an exact integer
   equal to [version]. *)
let version_of_json = function
  | Jsont.Number (f, _) when Float.is_integer f -> int_of_float f
  | Jsont.Number _ | Jsont.Null _ | Jsont.Bool _ | Jsont.String _
  | Jsont.Array _ | Jsont.Object _ ->
      invalid "jsont" "version must be an integer"

let jsont =
  let make version_json tool input payload requests description =
    Codec.decode_invalid_arg (fun () ->
        let stored_version = version_of_json version_json in
        if stored_version <> version then
          invalid "jsont"
            ("unknown prepared envelope version: "
            ^ string_of_int stored_version);
        { tool; input; payload; requests; description })
  in
  Jsont.Object.map ~kind:"prepared tool payload" make
  |> Jsont.Object.mem "version" Jsont.json ~enc:(fun _ ->
      Jsont.Json.number (float_of_int version))
  |> Jsont.Object.mem "tool" Jsont.string ~enc:tool
  |> Jsont.Object.mem "input" Jsont.json ~enc:input
  |> Jsont.Object.mem "payload" Jsont.json ~enc:(fun t -> t.payload)
  |> Jsont.Object.mem "requests"
       (Jsont.list Mentat_permission.Request.jsont)
       ~enc:requests
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun t -> t.description)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
