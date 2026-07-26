(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Status = struct
  type t = Running | Exited of int | Signaled of int | Terminated

  let pp ppf = function
    | Running -> Format.pp_print_string ppf "running"
    | Exited code -> Format.fprintf ppf "exited %d" code
    | Signaled signal -> Format.fprintf ppf "signaled %d" signal
    | Terminated -> Format.pp_print_string ppf "terminated"

  let equal a b =
    match (a, b) with
    | Running, Running | Terminated, Terminated -> true
    | Exited a, Exited b | Signaled a, Signaled b -> Int.equal a b
    | (Running | Exited _ | Signaled _ | Terminated), _ -> false
end

module View = struct
  type t = {
    handle : string;
    command : string;
    status : Status.t;
    age_ms : int;
  }

  let make ~handle ~command ~status ~age_ms =
    if age_ms < 0 then
      invalid_arg "Process.View.make: age_ms must be non-negative";
    { handle; command; status; age_ms }

  let handle t = t.handle
  let command t = t.command
  let status t = t.status
  let age_ms t = t.age_ms

  let equal a b =
    String.equal a.handle b.handle
    && String.equal a.command b.command
    && Status.equal a.status b.status
    && Int.equal a.age_ms b.age_ms
end
