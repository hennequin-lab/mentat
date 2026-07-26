(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Decode_error = struct
  type t =
    | Name_mismatch of { declaration : string; call : string }
    | Invalid_input of { tool : string; diagnostic : string }

  let message = function
    | Name_mismatch { declaration; call } ->
        Printf.sprintf "tool call name %S does not match declaration %S" call
          declaration
    | Invalid_input { tool; diagnostic } ->
        Printf.sprintf "tool %S rejected its input: %s" tool diagnostic

  let pp ppf e = Format.pp_print_string ppf (message e)
end

module Resume_error = struct
  type t =
    | Not_staged
    | Tool_mismatch
    | Input_mismatch
    | Invalid_prepared of string
    | Prepared_drift
    | Permission_drift

  let message = function
    | Not_staged -> "resume of a call that is not awaiting preparation"
    | Tool_mismatch -> "prepared value belongs to a different tool"
    | Input_mismatch -> "prepared value was prepared from different input"
    | Invalid_prepared message -> "prepared payload does not decode: " ^ message
    | Prepared_drift -> "prepared payload is not canonical"
    | Permission_drift -> "final permission requests changed since preparation"

  let pp ppf e = Format.pp_print_string ppf (message e)
end

type outcome = Finished of Output.t Result.t | Prepared of Prepared.t

type t =
  | Immediate : {
      declaration : Mentat_llm.Tool.t;
      source : Mentat_llm.Tool.Call.t;
      stage : Stage.t;
      canonical_input : Jsont.json;
      requests : Mentat_permission.Request.t list;
      value : 'value;
      output : 'output Output.encoder;
      run : cancelled:(unit -> bool) -> 'value -> 'output Result.t;
    }
      -> t
  | Staged : {
      declaration : Mentat_llm.Tool.t;
      source : Mentat_llm.Tool.Call.t;
      canonical_input : Jsont.json;
      requests : Mentat_permission.Request.t list;
      value : 'input;
      prepared : 'prepared Jsont.t;
      describe : 'prepared -> string;
      output : 'output Output.encoder;
      prepare :
        cancelled:(unit -> bool) ->
        'input ->
        [ `Finished of 'output Result.t | `Prepared of 'prepared ];
      permissions : 'prepared -> Mentat_permission.Request.t list;
      run : cancelled:(unit -> bool) -> 'prepared -> 'output Result.t;
    }
      -> t

let canonical_input input value =
  Canonical_json.of_json (Input.encode input value)

let decode definition source =
  let source_name = Mentat_llm.Tool.Call.name source in
  let source_input = Mentat_llm.Tool.Call.input source in
  let declaration_name =
    Mentat_llm.Tool.name (Definition.declaration definition)
  in
  if not (String.equal source_name declaration_name) then
    Error
      (Decode_error.Name_mismatch
         { declaration = declaration_name; call = source_name })
  else
    match (definition : Definition.t) with
    | Definition.Tool d -> (
        match Input.decode d.input source_input with
        | Error diagnostic ->
            Error
              (Decode_error.Invalid_input
                 { tool = declaration_name; diagnostic })
        | Ok value ->
            Ok
              (Immediate
                 {
                   declaration = d.declaration;
                   source;
                   stage = Stage.Direct;
                   canonical_input = canonical_input d.input value;
                   requests = d.permissions value;
                   value;
                   output = d.output;
                   run = d.run;
                 }))
    | Definition.Staged d -> (
        match Input.decode d.input source_input with
        | Error diagnostic ->
            Error
              (Decode_error.Invalid_input
                 { tool = declaration_name; diagnostic })
        | Ok value ->
            Ok
              (Staged
                 {
                   declaration = d.declaration;
                   source;
                   canonical_input = canonical_input d.input value;
                   requests = d.prepare_permissions value;
                   value;
                   prepared = d.prepared;
                   describe = d.describe;
                   output = d.output;
                   prepare = d.prepare;
                   permissions = d.permissions;
                   run = d.run;
                 }))

let declaration = function
  | Immediate t -> t.declaration
  | Staged t -> t.declaration

let name t = Mentat_llm.Tool.name (declaration t)
let source = function Immediate t -> t.source | Staged t -> t.source

let input = function
  | Immediate t -> t.canonical_input
  | Staged t -> t.canonical_input

let stage = function Immediate t -> t.stage | Staged _ -> Stage.Prepare
let permissions = function Immediate t -> t.requests | Staged t -> t.requests

let run call ~cancelled =
  match call with
  | Immediate t -> Finished (Result.map t.output (t.run ~cancelled t.value))
  | Staged t -> (
      match t.prepare ~cancelled t.value with
      | `Finished result -> Finished (Result.map t.output result)
      | `Prepared plan -> (
          match
            Prepared.of_payload
              ~tool:(Mentat_llm.Tool.name t.declaration)
              ~input:t.canonical_input t.prepared ~describe:t.describe
              ~requests:t.permissions plan
          with
          | Ok prepared -> Prepared prepared
          | Error message -> Finished (Result.failed `Failed message)))

let resume call prepared =
  match call with
  | Immediate _ -> Error Resume_error.Not_staged
  | Staged t -> (
      let name = Mentat_llm.Tool.name t.declaration in
      if not (String.equal (Prepared.tool prepared) name) then
        Error Resume_error.Tool_mismatch
      else if
        not (Canonical_json.equal (Prepared.input prepared) t.canonical_input)
      then Error Resume_error.Input_mismatch
      else
        match Prepared.reconstruct prepared t.prepared with
        | Error (Prepared.Undecodable message) ->
            Error (Resume_error.Invalid_prepared message)
        | Error Prepared.Drift -> Error Resume_error.Prepared_drift
        | Ok plan ->
            let requests = t.permissions plan in
            if
              not
                (List.equal Mentat_permission.Request.equal requests
                   (Prepared.requests prepared))
            then Error Resume_error.Permission_drift
            else
              Ok
                (Immediate
                   {
                     declaration = t.declaration;
                     source = t.source;
                     stage = Stage.Run;
                     canonical_input = t.canonical_input;
                     requests;
                     value = plan;
                     output = t.output;
                     run = t.run;
                   }))
