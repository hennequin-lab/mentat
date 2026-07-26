(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

let invalid fn message = invalid_arg' "Mentat_llm.Content" fn message

let reject_empty fn field value =
  if String.is_empty value then invalid fn (field ^ " must not be empty")

type media_source =
  [ `Uri of string | `Base64 of string | `Ref of Mentat_digest.Content_ref.t ]

type t =
  | Text of string
  | Media of { media_type : string; source : media_source }

let text value =
  reject_empty "text" "text" value;
  Text value

let media ~media_type source =
  reject_empty "media" "media_type" media_type;
  begin match source with
  | `Uri value -> reject_empty "media" "uri" value
  | `Base64 value -> reject_empty "media" "base64" value
  | `Ref _ -> ()
  end;
  Media { media_type; source }

let equal_source a b =
  match (a, b) with
  | `Uri x, `Uri y | `Base64 x, `Base64 y -> String.equal x y
  | `Ref x, `Ref y -> Mentat_digest.Content_ref.equal x y
  | (`Uri _ | `Base64 _ | `Ref _), _ -> false

let equal a b =
  match (a, b) with
  | Text x, Text y -> String.equal x y
  | ( Media { media_type = mt_a; source = source_a },
      Media { media_type = mt_b; source = source_b } ) ->
      String.equal mt_a mt_b && equal_source source_a source_b
  | (Text _ | Media _), _ -> false

let source_jsont =
  let base64 =
    Jsont.Object.map ~kind:"base64 media source" (fun data -> `Base64 data)
    |> Jsont.Object.mem "data" Jsont.string ~enc:(function
      | `Base64 data -> data
      | `Uri _ | `Ref _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "base64" ~dec:Fun.id
  in
  let uri =
    Jsont.Object.map ~kind:"URI media source" (fun uri -> `Uri uri)
    |> Jsont.Object.mem "uri" Jsont.string ~enc:(function
      | `Uri uri -> uri
      | `Base64 _ | `Ref _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "uri" ~dec:Fun.id
  in
  let ref' =
    Jsont.Object.map ~kind:"content-reference media source" (fun r -> `Ref r)
    |> Jsont.Object.mem "ref" Mentat_digest.Content_ref.jsont ~enc:(function
      | `Ref r -> r
      | `Uri _ | `Base64 _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "ref" ~dec:Fun.id
  in
  let cases = List.map Jsont.Object.Case.make [ base64; uri; ref' ] in
  let enc_case = function
    | `Base64 _ as source -> Jsont.Object.Case.value base64 source
    | `Uri _ as source -> Jsont.Object.Case.value uri source
    | `Ref _ as source -> Jsont.Object.Case.value ref' source
  in
  Jsont.Object.map ~kind:"media source" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let jsont =
  let text_case =
    Jsont.Object.map ~kind:"text content" (fun value ->
        decode_invalid_arg (fun () -> text value))
    |> Jsont.Object.mem "text" Jsont.string ~enc:(function
      | Text value -> value
      | Media _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "text" ~dec:Fun.id
  in
  let media_case =
    Jsont.Object.map ~kind:"media content" (fun media_type source ->
        decode_invalid_arg (fun () -> media ~media_type source))
    |> Jsont.Object.mem "media_type" Jsont.string ~enc:(function
      | Media { media_type; _ } -> media_type
      | Text _ -> assert false)
    |> Jsont.Object.mem "source" source_jsont ~enc:(function
      | Media { source; _ } -> source
      | Text _ -> assert false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "media" ~dec:Fun.id
  in
  let cases = List.map Jsont.Object.Case.make [ text_case; media_case ] in
  let enc_case = function
    | Text _ as content -> Jsont.Object.Case.value text_case content
    | Media _ as content -> Jsont.Object.Case.value media_case content
  in
  Jsont.Object.map ~kind:"message content" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
