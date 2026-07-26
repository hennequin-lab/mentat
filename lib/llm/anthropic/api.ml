(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Http = Mentat_llm_http

let api = "messages"
let default_base_url = "https://api.anthropic.com/v1"
let default_max_retries = 2
let default_stream_max_retries = 5

module Error = struct
  type response = Http.response = {
    status : int;
    headers : (string * string) list;
    body : string;
  }

  type t = Response of response | Transport of string | Decode of string
end

module Client = struct
  type auth = Api_key of string | Bearer of string

  type t = {
    config : Config.t;
    auth : auth;
    sw : Eio.Switch.t;
    env : Eio_unix.Stdenv.base;
  }

  let make config ~sw ~env ~auth () = { config; auth; sw; env }
  let config t = t.config
  let sw t = t.sw
  let env t = t.env

  let auth_header t =
    match t.auth with
    | Api_key key -> ("x-api-key", key)
    | Bearer token -> ("authorization", "Bearer " ^ token)
end

let headers t attempt =
  [
    Client.auth_header t;
    ("content-type", "application/json");
    ("accept", "text/event-stream");
    ("anthropic-version", "2023-06-01");
    ("user-agent", "mentat-llm-anthropic/0");
    ("x-stainless-retry-count", string_of_int attempt);
  ]

let error_of_http = function
  | Http.Response response -> Error.Response response
  | Http.Transport message | Http.Unresolved_host message ->
      Error.Transport message

let stream_post t ~on_retry ~path ~body =
  let config = Client.config t in
  let base_url =
    Option.value (Config.base_url config) ~default:default_base_url
  in
  let max_retries =
    Option.value (Config.max_retries config) ~default:default_max_retries
  in
  Http.Retry.pre_stream ~clock:(Client.env t)#clock ~max_retries ~on_retry
    (fun ~attempt ->
      Http.post_stream ~sw:(Client.sw t) ~env:(Client.env t)
        ~url:(base_url ^ path) ~headers:(headers t attempt) ~body)
  |> Result.map_error error_of_http

let json_member name value = Jsont.Json.mem (Jsont.Json.name name) value
let string_member name value = json_member name (Jsont.Json.string value)
let bool_member name value = json_member name (Jsont.Json.bool value)
let int_member name value = json_member name (Jsont.Json.int value)
let number_member name value = json_member name (Jsont.Json.number value)
let list_member name value = json_member name (Jsont.Json.list value)

let add_opt member name value fields =
  match value with None -> fields | Some value -> member name value :: fields

let json_string json =
  match Jsont_bytesrw.encode_string Jsont.json json with
  | Ok value -> Ok value
  | Error message -> Error (Error.Decode ("JSON encode failed: " ^ message))

module Messages = struct
  type request = {
    model : string;
    system : Jsont.json list;
    messages : Jsont.json list;
    tools : Jsont.json list;
    tool_choice : Jsont.json option;
    thinking : Jsont.json option;
    max_tokens : int;
    temperature : float option;
    stream : bool;
  }

  type event = { name : string; data : Jsont.json }

  type stream = {
    next : unit -> (event, Error.t) result option;
    close : unit -> unit;
  }

  let next stream = stream.next ()
  let close stream = stream.close ()

  let body request =
    let fields =
      [
        string_member "model" request.model;
        list_member "messages" request.messages;
        int_member "max_tokens" request.max_tokens;
        bool_member "stream" request.stream;
      ]
    in
    let fields =
      match request.system with
      | [] -> fields
      | system -> list_member "system" system :: fields
    in
    let fields =
      match request.tools with
      | [] -> fields
      | tools -> list_member "tools" tools :: fields
    in
    let fields = add_opt json_member "tool_choice" request.tool_choice fields in
    let fields = add_opt json_member "thinking" request.thinking fields in
    let fields =
      add_opt number_member "temperature" request.temperature fields
    in
    Jsont.Json.object' (List.rev fields)

  let event_type = function
    | Jsont.Object (fields, _) -> (
        match Jsont.Json.find_mem "type" fields with
        | Some (_, Jsont.String (value, _)) -> Some value
        | Some _ | None -> None)
    | Jsont.Null _ | Jsont.Bool _ | Jsont.Number _ | Jsont.String _
    | Jsont.Array _ ->
        None

  let decode_sse_event name raw_data =
    match Jsont_bytesrw.decode_string Jsont.json raw_data with
    | Error message ->
        Error (Error.Decode ("Anthropic stream JSON decode failed: " ^ message))
    | Ok data -> (
        match (name, event_type data) with
        | "", None -> Error (Error.Decode "Anthropic stream event missing type")
        | "", Some name | name, _ -> Ok { name; data })

  let stream_of_flow body =
    let reader = Http.Sse.make body in
    let closed = ref false in
    {
      next =
        (fun () ->
          if !closed then None
          else
            try
              Option.map
                (fun (event : Http.Sse.event) ->
                  decode_sse_event event.Http.Sse.name event.Http.Sse.data)
                (Http.Sse.next reader)
            with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | exn -> Some (Error (Error.Transport (Printexc.to_string exn))));
      close = (fun () -> closed := true);
    }

  let create_stream
      ?(on_retry = fun ~attempt:_ ~limit:_ ~delay:_ ~reason:_ -> ()) client
      request =
    match json_string (body request) with
    | Error error -> Error error
    | Ok body ->
        Result.map stream_of_flow
          (stream_post client ~on_retry ~path:"/messages" ~body)
end
