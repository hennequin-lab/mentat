(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The request/response envelope and the SSE frame vocabulary. The envelope
   extends the landed protocol [v] discipline — a required
   integer version, strict-decoded — with only a narrow client-minted request-id
   and no routing member: one connection binds one workspace at handshake, so
   there is nothing to select per request. The endpoint-specific payload and
   result cross as generic JSON subtrees, decoded against the resolved
   descriptor's codec by the dispatcher and the remote driver. *)

(* [mentat_protocol]'s [Wire] module is private, so the envelope version is
   restated here; it names the same v1 floor the inner values carry. *)
let version = 1

let check v =
  if not (Int.equal v version) then
    Jsont.Error.msg Jsont.Meta.none
      (Printf.sprintf "unsupported envelope version %d (expected %d)" v version)

let v_mem map = Jsont.Object.mem "v" Jsont.int ~enc:(fun _ -> version) map

(* Envelopes. *)

type request = {
  request_id : string option;
  endpoint : string;
  payload : Jsont.json;
}

let request_jsont : request Jsont.t =
  Jsont.Object.map ~kind:"wire request" (fun v request_id endpoint payload ->
      check v;
      { request_id; endpoint; payload })
  |> v_mem
  |> Jsont.Object.opt_mem "request_id" Jsont.string ~enc:(fun r -> r.request_id)
  |> Jsont.Object.mem "endpoint" Jsont.string ~enc:(fun r -> r.endpoint)
  |> Jsont.Object.mem "payload" Jsont.json ~enc:(fun r -> r.payload)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

type response = {
  resp_request_id : string option;
  outcome : (Jsont.json, Mentat_protocol.Error.t) result;
}

let response_jsont : response Jsont.t =
  Jsont.Object.map ~kind:"wire response" (fun v resp_request_id result error ->
      check v;
      let outcome =
        match (result, error) with
        | Some json, None -> Ok json
        | None, Some error -> Error error
        | Some _, Some _ | None, None ->
            Jsont.Error.msg Jsont.Meta.none
              "response must carry exactly one of result or error"
      in
      { resp_request_id; outcome })
  |> v_mem
  |> Jsont.Object.opt_mem "request_id" Jsont.string ~enc:(fun r ->
      r.resp_request_id)
  |> Jsont.Object.opt_mem "result" Jsont.json ~enc:(fun r ->
      match r.outcome with Ok json -> Some json | Error _ -> None)
  |> Jsont.Object.opt_mem "error" Mentat_protocol.Error.jsont ~enc:(fun r ->
      match r.outcome with Error error -> Some error | Ok _ -> None)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

(* The handshake — the connection's first exchange and the only version
   negotiation. *)

type handshake_request = {
  v_max : int;
  requested_workspace : string option;
  environment : (string * string) list option;
      (* The invoking client's process environment, captured at its snapshot.
         The daemon resolves a freshly booted workspace instance against it —
         config env overrides, toolchain discovery, and the exact child
         environment — so a confined command is configured from the shell that
         asked for the run, not the shell that spawned the daemon. Carried on
         every handshake because any dial may be the one that (re)boots an
         evicted instance; a live instance keeps the environment it booted
         with. Absent means the client offered none (a raw wire client) and
         the daemon falls back to its own. *)
}

let binding_jsont : (string * string) Jsont.t =
  Jsont.t2 ~kind:"environment binding"
    ~dec:(fun name value -> (name, value))
    ~enc:(fun (name, value) i -> if i = 0 then name else value)
    Jsont.string

let handshake_request_jsont : handshake_request Jsont.t =
  Jsont.Object.map ~kind:"handshake request"
    (fun v_max requested_workspace environment ->
      { v_max; requested_workspace; environment })
  |> Jsont.Object.mem "v_max" Jsont.int ~enc:(fun (r : handshake_request) ->
      r.v_max)
  |> Jsont.Object.opt_mem "workspace" Jsont.string
       ~enc:(fun (r : handshake_request) -> r.requested_workspace)
  |> Jsont.Object.opt_mem "environment" (Jsont.list binding_jsont)
       ~enc:(fun (r : handshake_request) -> r.environment)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

type handshake_response = { negotiated : int; workspace : string option }

let handshake_response_jsont : handshake_response Jsont.t =
  Jsont.Object.map ~kind:"handshake response" (fun negotiated workspace ->
      { negotiated; workspace })
  |> Jsont.Object.mem "v" Jsont.int ~enc:(fun (r : handshake_response) ->
      r.negotiated)
  |> Jsont.Object.opt_mem "workspace" Jsont.string
       ~enc:(fun (r : handshake_response) -> r.workspace)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

(* String bridges over the codecs above. A decode failure is a structured wire
   fault, surfaced by the caller as the one protocol error type. *)

let decode : 'a Jsont.t -> string -> ('a, string) result =
 fun codec body -> Jsont_bytesrw.decode_string codec body

let encode : 'a Jsont.t -> 'a -> string =
 fun codec value ->
  match Jsont_bytesrw.encode_string codec value with
  | Ok text -> text
  | Error message ->
      (* Encoding an owner value the codec accepts cannot fail on well-formed
         input; a failure is a programming error, not a wire condition. *)
      invalid_arg ("mentat_server.wire: encode: " ^ message)

let decode_json : 'a Jsont.t -> Jsont.json -> ('a, string) result =
 fun codec json -> Jsont.Json.decode codec json

let encode_json : 'a Jsont.t -> 'a -> Jsont.json =
 fun codec value ->
  match Jsont.Json.encode codec value with
  | Ok json -> json
  | Error message -> invalid_arg ("mentat_server.wire: encode_json: " ^ message)
