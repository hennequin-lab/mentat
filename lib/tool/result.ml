(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message =
  invalid_arg ("Mentat_tool.Result." ^ fn ^ ": " ^ message)

let reject_empty fn field value =
  if String.is_empty value then invalid fn (field ^ " must not be empty")

type failure =
  [ `Invalid_input
  | `Permission_denied
  | `Not_found
  | `Stale
  | `Unavailable
  | `Timed_out
  | `Failed ]

type status =
  | Completed
  | Failed of { kind : failure; message : string; metadata : Jsont.json option }
  | Interrupted of { reason : string; cancelled : bool }

(* The output lives inside each arm, so "completed without output" is
   unrepresentable. The internal constructors are suffixed to stay distinct from
   the public {!status} projection above, which carries no output. *)
type 'a t =
  | Completed_ of 'a
  | Failed_ of {
      kind : failure;
      message : string;
      metadata : Jsont.json option;
      output : 'a option;
    }
  | Interrupted_ of { reason : string; cancelled : bool; output : 'a option }

let completed ~output () = Completed_ output

let failed ?output ?metadata kind message =
  reject_empty "failed" "message" message;
  Failed_ { kind; message; metadata; output }

let interrupted ?output ~reason ~cancelled () =
  reject_empty "interrupted" "reason" reason;
  Interrupted_ { reason; cancelled; output }

let cancelled ?output () =
  interrupted ?output ~reason:"tool call cancelled" ~cancelled:true ()

let status = function
  | Completed_ _ -> Completed
  | Failed_ { kind; message; metadata; output = _ } ->
      Failed { kind; message; metadata }
  | Interrupted_ { reason; cancelled; output = _ } ->
      Interrupted { reason; cancelled }

let output = function
  | Completed_ output -> Some output
  | Failed_ { output; _ } -> output
  | Interrupted_ { output; _ } -> output

let map f = function
  | Completed_ output -> Completed_ (f output)
  | Failed_ { kind; message; metadata; output } ->
      Failed_ { kind; message; metadata; output = Option.map f output }
  | Interrupted_ { reason; cancelled; output } ->
      Interrupted_ { reason; cancelled; output = Option.map f output }

let equal eq a b =
  match (a, b) with
  | Completed_ a, Completed_ b -> eq a b
  | Failed_ a, Failed_ b ->
      a.kind = b.kind
      && String.equal a.message b.message
      && Option.equal Jsont.Json.equal a.metadata b.metadata
      && Option.equal eq a.output b.output
  | Interrupted_ a, Interrupted_ b ->
      String.equal a.reason b.reason
      && Bool.equal a.cancelled b.cancelled
      && Option.equal eq a.output b.output
  | (Completed_ _ | Failed_ _ | Interrupted_ _), _ -> false

let failure_jsont =
  Jsont.enum ~kind:"tool failure"
    [
      ("invalid_input", `Invalid_input);
      ("permission_denied", `Permission_denied);
      ("not_found", `Not_found);
      ("stale", `Stale);
      ("unavailable", `Unavailable);
      ("timed_out", `Timed_out);
      ("failed", `Failed);
    ]

(* One case object per status. Each declares only its own members and rejects
   unknown ones, so a result carrying members from another status (a "failed"
   with "reason"/"cancelled") is a decode error rather than a silent drop. *)
let jsont value_jsont =
  let completed_case =
    Jsont.Object.map ~kind:"completed tool result" (fun output ->
        Codec.decode_invalid_arg (fun () ->
            match output with
            | Some output -> completed ~output ()
            | None -> invalid "jsont" "completed result requires output"))
    |> Jsont.Object.opt_mem "output" value_jsont ~enc:output
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "completed" ~dec:Fun.id
  in
  let failed_case =
    Jsont.Object.map ~kind:"failed tool result"
      (fun failure message metadata output ->
        Codec.decode_invalid_arg (fun () ->
            failed ?output ?metadata failure message))
    |> Jsont.Object.mem "failure" failure_jsont ~enc:(fun t ->
        match t with Failed_ f -> f.kind | _ -> assert false)
    |> Jsont.Object.mem "message" Jsont.string ~enc:(fun t ->
        match t with Failed_ f -> f.message | _ -> assert false)
    |> Jsont.Object.opt_mem "metadata" Jsont.json ~enc:(fun t ->
        match t with Failed_ f -> f.metadata | _ -> assert false)
    |> Jsont.Object.opt_mem "output" value_jsont ~enc:output
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "failed" ~dec:Fun.id
  in
  let interrupted_case =
    Jsont.Object.map ~kind:"interrupted tool result"
      (fun reason cancelled output ->
        Codec.decode_invalid_arg (fun () ->
            interrupted ?output ~reason ~cancelled ()))
    |> Jsont.Object.mem "reason" Jsont.string ~enc:(fun t ->
        match t with Interrupted_ i -> i.reason | _ -> assert false)
    |> Jsont.Object.mem "cancelled" Jsont.bool ~enc:(fun t ->
        match t with Interrupted_ i -> i.cancelled | _ -> assert false)
    |> Jsont.Object.opt_mem "output" value_jsont ~enc:output
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
    |> Jsont.Object.Case.map "interrupted" ~dec:Fun.id
  in
  let cases =
    List.map Jsont.Object.Case.make
      [ completed_case; failed_case; interrupted_case ]
  in
  let enc_case t =
    match t with
    | Completed_ _ -> Jsont.Object.Case.value completed_case t
    | Failed_ _ -> Jsont.Object.Case.value failed_case t
    | Interrupted_ _ -> Jsont.Object.Case.value interrupted_case t
  in
  Jsont.Object.map ~kind:"tool result" Fun.id
  |> Jsont.Object.case_mem "status" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

(* Lower [text] plus any media the output carries. Version-pinned: an output
   with no media lowers to a plain text result, byte-identical to a pre-media
   result; media appends model-visible blocks after the text. *)
let lower ?error call ~text media =
  match media with
  | [] -> Mentat_llm.Tool.Result.text ?error call text
  | media ->
      Mentat_llm.Tool.Result.make ?error call
        (Mentat_llm.Content.text text :: media)

let to_model ~call result =
  match result with
  | Completed_ output ->
      lower call ~text:(Output.text output) (Output.media output)
  | Failed_ { message; output = Some output; _ } ->
      lower ~error:true call
        ~text:(message ^ "\n\n" ^ Output.text output)
        (Output.media output)
  | Failed_ { message; output = None; _ } ->
      Mentat_llm.Tool.Result.text ~error:true call message
  | Interrupted_ { reason; output = Some output; _ } ->
      lower ~error:true call
        ~text:(reason ^ "\n\n" ^ Output.text output)
        (Output.media output)
  | Interrupted_ { reason; output = None; _ } ->
      Mentat_llm.Tool.Result.text ~error:true call reason
