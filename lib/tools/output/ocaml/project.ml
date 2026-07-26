(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid message =
  invalid_arg ("Mentat_tools_output.Ocaml.Project.make: " ^ message)

type t = { components : int; tests : int }

let make ~components ~tests =
  if components < 0 then invalid "components must be non-negative";
  if tests < 0 then invalid "tests must be non-negative";
  { components; tests }

let components t = t.components
let tests t = t.tests

let jsont =
  let make version components tests =
    Mentat_tool.Codec.decode_invalid_arg (fun () ->
        if version <> 1 then invalid "unsupported version";
        make ~components ~tests)
  in
  Jsont.Object.map ~kind:"OCaml Dune project output" make
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "components" Jsont.int ~enc:components
  |> Jsont.Object.mem "tests" Jsont.int ~enc:tests
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
