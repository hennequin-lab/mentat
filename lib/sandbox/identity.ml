(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Enforced of { backend : Backend.t; profile : Mentat_digest.t }
  | Not_requested
  | Declared_external
  | Refused

let not_requested = Not_requested
let declared_external = Declared_external
let refused = Refused
let enforced ~backend ~profile = Enforced { backend; profile }
let domain = "mentat.sandbox.identity.v1"

let parts = function
  | Enforced { backend; profile } ->
      [ "enforced"; Backend.id backend; Mentat_digest.to_hex profile ]
  | Not_requested -> [ "not_requested" ]
  | Declared_external -> [ "declared_external" ]
  | Refused -> [ "refused" ]

let digest t = Mentat_digest.derive ~domain (parts t)
let equal a b = Mentat_digest.equal (digest a) (digest b)

let pp ppf = function
  | Enforced { backend; _ } ->
      Format.fprintf ppf "enforced (%s)" (Backend.id backend)
  | Not_requested -> Format.pp_print_string ppf "not requested"
  | Declared_external -> Format.pp_print_string ppf "declared external"
  | Refused -> Format.pp_print_string ppf "refused"

let decode_error message = Jsont.Error.msg Jsont.Meta.none message

let jsont =
  let make cls backend profile =
    match (cls, backend, profile) with
    | "enforced", Some backend, Some profile -> (
        match Backend.of_id backend with
        | Some backend -> Enforced { backend; profile }
        | None -> decode_error ("unknown sandbox backend: " ^ backend))
    | "enforced", _, _ ->
        decode_error "enforced sandbox identity requires a backend and profile"
    | "not_requested", None, None -> Not_requested
    | "declared_external", None, None -> Declared_external
    | "refused", None, None -> Refused
    | ("not_requested" | "declared_external" | "refused"), _, _ ->
        decode_error
          (cls ^ " sandbox identity must not carry a backend or profile")
    | cls, _, _ -> decode_error ("unknown sandbox identity class: " ^ cls)
  in
  let class_of = function
    | Enforced _ -> "enforced"
    | Not_requested -> "not_requested"
    | Declared_external -> "declared_external"
    | Refused -> "refused"
  in
  let backend_of = function
    | Enforced { backend; _ } -> Some (Backend.id backend)
    | Not_requested | Declared_external | Refused -> None
  in
  let profile_of = function
    | Enforced { profile; _ } -> Some profile
    | Not_requested | Declared_external | Refused -> None
  in
  Jsont.Object.map ~kind:"sandbox identity" make
  |> Jsont.Object.mem "class" Jsont.string ~enc:class_of
  |> Jsont.Object.opt_mem "backend" Jsont.string ~enc:backend_of
  |> Jsont.Object.opt_mem "profile" Mentat_digest.jsont ~enc:profile_of
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
