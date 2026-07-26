(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Disconnected | Clean | Failing of int | Unknown

let equal a b =
  match (a, b) with
  | Disconnected, Disconnected | Clean, Clean | Unknown, Unknown -> true
  | Failing a, Failing b -> Int.equal a b
  | (Disconnected | Clean | Failing _ | Unknown), _ -> false

let pp ppf = function
  | Disconnected -> Format.pp_print_string ppf "disconnected"
  | Clean -> Format.pp_print_string ppf "clean"
  | Failing n -> Format.fprintf ppf "failing(%d)" n
  | Unknown -> Format.pp_print_string ppf "unknown"

(* The verdict crosses the wire as an object: a closed-vocabulary [status] tag
   and an [errors] count that is the outstanding-diagnostic total for [Failing]
   and 0 for every other case. The tag enum rejects an unknown status loudly, so
   the reconstruction below is total. *)
let status_jsont =
  Jsont.enum ~kind:"workspace tooling health status"
    [
      ("disconnected", `Disconnected);
      ("clean", `Clean);
      ("failing", `Failing);
      ("unknown", `Unknown);
    ]

let jsont =
  Jsont.Object.map ~kind:"workspace tooling health" (fun status errors ->
      match status with
      | `Disconnected -> Disconnected
      | `Clean -> Clean
      | `Failing -> Failing errors
      | `Unknown -> Unknown)
  |> Jsont.Object.mem "status" status_jsont ~enc:(function
    | Disconnected -> `Disconnected
    | Clean -> `Clean
    | Failing _ -> `Failing
    | Unknown -> `Unknown)
  |> Jsont.Object.mem "errors" Jsont.int ~enc:(function
    | Failing n -> n
    | Disconnected | Clean | Unknown -> 0)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
