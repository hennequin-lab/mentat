(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Options carry [Jsont.json] (structured response formats) and expose no
   [equal]; comparing over the JSON value ignores parse metadata, so a decoded
   contract equals an otherwise-identical constructed one. *)
let options_equal a b =
  match
    ( Jsont.Json.encode Mentat_llm.Request.Options.jsont a,
      Jsont.Json.encode Mentat_llm.Request.Options.jsont b )
  with
  | Ok a, Ok b -> Jsont.Json.equal a b
  | _ -> false

module Mode = struct
  type t = Build | Plan | Review

  let equal a b = a = b

  let pp ppf = function
    | Build -> Format.pp_print_string ppf "build"
    | Plan -> Format.pp_print_string ppf "plan"
    | Review -> Format.pp_print_string ppf "review"

  let jsont =
    Jsont.enum ~kind:"turn mode"
      [ ("build", Build); ("plan", Plan); ("review", Review) ]
end

type t = {
  mode : Mode.t;
  model : Mentat_llm.Model.t;
  options : Mentat_llm.Request.Options.t;
  declarations : Mentat_llm.Tool.t list;
  output_tool : Mentat_llm.Tool.t option;
  policy : Mentat_permission.Policy.t;
  review : Mentat_permission.Review_behavior.t;
  sandbox : Mentat_sandbox.Identity.t;
}

let make ~mode ~model ?(options = Mentat_llm.Request.Options.default)
    ~declarations ?output_tool ~policy ~review ~sandbox () =
  { mode; model; options; declarations; output_tool; policy; review; sandbox }

let mode t = t.mode
let model t = t.model
let options t = t.options
let declarations t = t.declarations
let output_tool t = t.output_tool
let policy t = t.policy
let review t = t.review
let sandbox t = t.sandbox

let equal a b =
  Mode.equal a.mode b.mode
  && Mentat_llm.Model.equal a.model b.model
  && options_equal a.options b.options
  && List.equal Mentat_llm.Tool.equal a.declarations b.declarations
  && Option.equal Mentat_llm.Tool.equal a.output_tool b.output_tool
  && Mentat_permission.Policy.equal a.policy b.policy
  && Mentat_permission.Review_behavior.equal a.review b.review
  && Mentat_sandbox.Identity.equal a.sandbox b.sandbox

let pp ppf t =
  Format.fprintf ppf "@[<hov>{ mode = %a; model = %a; sandbox = %a }@]" Mode.pp
    t.mode Mentat_llm.Model.pp t.model Mentat_sandbox.Identity.pp t.sandbox

let jsont =
  Jsont.Object.map ~kind:"turn contract"
    (fun mode model options declarations output_tool policy review sandbox ->
      make ~mode ~model ~options ~declarations ?output_tool ~policy ~review
        ~sandbox ())
  |> Jsont.Object.mem "mode" Mode.jsont ~enc:mode
  |> Jsont.Object.mem "model" Mentat_llm.Model.jsont ~enc:model
  |> Jsont.Object.mem "options" Mentat_llm.Request.Options.jsont ~enc:options
  |> Jsont.Object.mem "declarations"
       (Jsont.list Mentat_llm.Tool.jsont)
       ~enc:declarations
  |> Jsont.Object.opt_mem "output_tool" Mentat_llm.Tool.jsont ~enc:output_tool
  |> Jsont.Object.mem "policy" Mentat_permission.Policy.jsont ~enc:policy
  |> Jsont.Object.mem "review" Mentat_permission.Review_behavior.jsont
       ~enc:review
  |> Jsont.Object.mem "sandbox" Mentat_sandbox.Identity.jsont ~enc:sandbox
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
