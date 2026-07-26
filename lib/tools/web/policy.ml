(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let default_max_fetch_bytes = 5 * 1024 * 1024
let default_max_output_chars = 100_000
let default_default_timeout_ms = 30_000
let default_max_timeout_ms = 120_000

module Error = struct
  type field =
    | Max_fetch_bytes
    | Max_output_chars
    | Default_timeout_ms
    | Max_timeout_ms

  type t =
    | Non_positive of { field : field; value : int }
    | Default_timeout_exceeds_max of {
        default_timeout_ms : int;
        max_timeout_ms : int;
      }

  let field_name = function
    | Max_fetch_bytes -> "max_fetch_bytes"
    | Max_output_chars -> "max_output_chars"
    | Default_timeout_ms -> "default_timeout_ms"
    | Max_timeout_ms -> "max_timeout_ms"

  let message = function
    | Non_positive { field; value } ->
        Printf.sprintf "%s must be positive (got %d)" (field_name field) value
    | Default_timeout_exceeds_max { default_timeout_ms; max_timeout_ms } ->
        Printf.sprintf
          "default_timeout_ms (%d) must not exceed max_timeout_ms (%d)"
          default_timeout_ms max_timeout_ms

  let pp ppf error = Format.pp_print_string ppf (message error)
end

module Timeout_error = struct
  type t =
    | Non_positive of int
    | Exceeds_max of { requested : int; maximum : int }

  let message = function
    | Non_positive requested ->
        Printf.sprintf "timeout_ms must be positive (got %d)" requested
    | Exceeds_max { requested; maximum } ->
        Printf.sprintf "timeout_ms must not exceed %d (got %d)" maximum
          requested

  let pp ppf error = Format.pp_print_string ppf (message error)
end

type t = {
  allow_private_network : bool;
  max_fetch_bytes : int;
  max_output_chars : int;
  default_timeout_ms : int;
  max_timeout_ms : int;
}

let make ?(allow_private_network = false)
    ?(max_fetch_bytes = default_max_fetch_bytes)
    ?(max_output_chars = default_max_output_chars)
    ?(default_timeout_ms = default_default_timeout_ms)
    ?(max_timeout_ms = default_max_timeout_ms) () =
  let open Error in
  if max_fetch_bytes <= 0 then
    Error (Non_positive { field = Max_fetch_bytes; value = max_fetch_bytes })
  else if max_output_chars <= 0 then
    Error (Non_positive { field = Max_output_chars; value = max_output_chars })
  else if default_timeout_ms <= 0 then
    Error
      (Non_positive { field = Default_timeout_ms; value = default_timeout_ms })
  else if max_timeout_ms <= 0 then
    Error (Non_positive { field = Max_timeout_ms; value = max_timeout_ms })
  else if default_timeout_ms > max_timeout_ms then
    Error (Default_timeout_exceeds_max { default_timeout_ms; max_timeout_ms })
  else
    Ok
      {
        allow_private_network;
        max_fetch_bytes;
        max_output_chars;
        default_timeout_ms;
        max_timeout_ms;
      }

let allow_private_network t = t.allow_private_network
let max_fetch_bytes t = t.max_fetch_bytes
let max_output_chars t = t.max_output_chars
let default_timeout_ms t = t.default_timeout_ms
let max_timeout_ms t = t.max_timeout_ms

let resolve_timeout_ms t = function
  | None -> Ok t.default_timeout_ms
  | Some requested when requested <= 0 ->
      Error (Timeout_error.Non_positive requested)
  | Some requested when requested > t.max_timeout_ms ->
      Error
        (Timeout_error.Exceeds_max { requested; maximum = t.max_timeout_ms })
  | Some requested -> Ok requested
