(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Exa | Parallel

let of_string = function
  | "exa" -> Some Exa
  | "parallel" -> Some Parallel
  | _ -> None

let to_string = function Exa -> "exa" | Parallel -> "parallel"

(* The scouted keyless MCP endpoints (opencode packages/opencode/src/tool). Exa
   admits an optional key as a query parameter; Parallel's key rides an
   Authorization header instead, so its endpoint is key-free. *)
let endpoint t ?api_key () =
  match t with
  | Exa ->
      let base = Uri.of_string "https://mcp.exa.ai/mcp" in
      Option.fold ~none:base
        ~some:(fun key -> Uri.add_query_param' base ("exaApiKey", key))
        api_key
  | Parallel -> Uri.of_string "https://search.parallel.ai/mcp"

let headers t ?api_key ~user_agent () =
  let base =
    [
      ("user-agent", user_agent);
      ("accept", "application/json, text/event-stream");
      ("content-type", "application/json");
    ]
  in
  match (t, api_key) with
  | Parallel, Some key -> base @ [ ("authorization", "Bearer " ^ key) ]
  | Parallel, None | Exa, _ -> base

let tool_name = function Exa -> "web_search_exa" | Parallel -> "web_search"

(* Backend-shaped call arguments. Exa takes a flat query with search knobs;
   Parallel takes an objective plus a query list. Defaults mirror the scouted
   clients ([type=auto], [livecrawl=fallback]). *)
let arguments t ~query ~num_results =
  match t with
  | Exa ->
      Codec.obj
        [
          ("query", Jsont.Json.string query);
          ("type", Jsont.Json.string "auto");
          ("numResults", Jsont.Json.int num_results);
          ("livecrawl", Jsont.Json.string "fallback");
        ]
  | Parallel ->
      Codec.obj
        [
          ("objective", Jsont.Json.string query);
          ("search_queries", Jsont.Json.list [ Jsont.Json.string query ]);
        ]

let request_body t ~query ~num_results =
  let envelope =
    Codec.obj
      [
        ("jsonrpc", Jsont.Json.string "2.0");
        ("id", Jsont.Json.int 1);
        ("method", Jsont.Json.string "tools/call");
        ( "params",
          Codec.obj
            [
              ("name", Jsont.Json.string (tool_name t));
              ("arguments", arguments t ~query ~num_results);
            ] );
      ]
  in
  match Jsont_bytesrw.encode_string Jsont.json envelope with
  | Ok text -> text
  | Error message ->
      (* The envelope is built from validated leaves, so a failed encode is a
         programmer bug, not backend input. *)
      invalid_arg ("Search_service.request_body: " ^ message)

let json_mem name = function
  | Jsont.Object (fields, _) -> Option.map snd (Jsont.Json.find_mem name fields)
  | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
  | Jsont.Array _ ->
      None

let json_string = function Jsont.String (value, _) -> Some value | _ -> None

(* An MCP tools/call result is {"result":{"content":[{"type","text"},…]}}; the
   first content item carrying text is the search answer. *)
let extract_text json =
  match json_mem "result" json with
  | Some result -> (
      match json_mem "content" result with
      | Some (Jsont.Array (items, _)) ->
          List.find_map
            (fun item -> Option.bind (json_mem "text" item) json_string)
            items
      | Some _ | None -> None)
  | None -> None

let parse_payload payload =
  let payload = String.trim payload in
  if String.length payload = 0 || not (Char.equal payload.[0] '{') then None
  else
    match Jsont_bytesrw.decode_string Jsont.json payload with
    | Ok json -> extract_text json
    | Error _ -> None

let parse_response body =
  match parse_payload body with
  | Some text -> Ok text
  | None -> (
      (* Server-sent events: each event's payload is on a [data:] line. *)
      let from_line line =
        match String.starts_with ~prefix:"data: " line with
        | true -> parse_payload (String.sub line 6 (String.length line - 6))
        | false -> None
      in
      let lines = String.split_on_char '\n' body in
      match List.find_map from_line lines with
      | Some text -> Ok text
      | None -> Error "response carried no search result text")
