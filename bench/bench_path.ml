(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Path-normalization floor: the guard under every workspace path.

    Lpath normalizes and resolves every path the agent touches — a tool
    argument, a mention, a diff label, a checkpoint target. This is not a
    latency a user waits on; it is a floor that must stay cheap because it runs
    on the hot edge of everything else. A regression here never shows up as one
    slow screen — it taxes every path operation uniformly, which is exactly why
    it is guarded on its own rather than folded into a feature's number. The
    cases exercise both string parsing and root-relative resolution, the two
    shapes the workspace layer drives, over a fixed set of realistic path
    inputs. *)

module Path = Lpath

let ok label = function
  | Ok value -> value
  | Error error -> failwith (label ^ ": " ^ Path.Error.message error)

let rel_inputs =
  Thumper.black_box
    [|
      "lib/path/../diff/mentat_diff.ml";
      "./test//test_diff.ml";
      "src/a/./b/../c/d.ml";
      "doc/design/../design/path.md";
      "a/b/c/d/e/f/../../g";
    |]

let abs_inputs =
  Thumper.black_box
    [|
      "/workspace/mentat/lib/path/../diff/mentat_diff.ml";
      "/tmp//mentat/./test/test_diff.ml";
      "/a/b/c/../../d/e";
      "/../workspace/mentat";
    |]

let rel_root = ok "rel root" (Path.Rel.of_string "workspace/src/lib")
let abs_root = ok "abs root" (Path.Abs.of_string "/workspace/mentat/lib")

let () =
  Thumper.run "path"
    ~budgets:
      [
        Thumper.Budget.no_more_alloc_than 0.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.wall_time 1000.0;
        Thumper.Budget.no_slower_than ~metric:Thumper.Metric.cpu_time 1000.0;
      ]
    Thumper.
      [
        group "guard"
          [
            bench "parse/rel" (fun () ->
                Array.iter
                  (fun input ->
                    ignore
                      (Sys.opaque_identity
                         (Path.Rel.of_string input |> ok "rel" |> Path.Rel.hash)))
                  rel_inputs);
            bench "parse/abs" (fun () ->
                Array.iter
                  (fun input ->
                    ignore
                      (Sys.opaque_identity
                         (Path.Abs.of_string input |> ok "abs" |> Path.Abs.hash)))
                  abs_inputs);
            bench "resolve/rel" (fun () ->
                Array.iter
                  (fun input ->
                    ignore
                      (Sys.opaque_identity
                         (Path.Rel.resolve rel_root input
                         |> ok "resolve" |> Path.Rel.hash)))
                  rel_inputs);
            bench "resolve/abs" (fun () ->
                Array.iter
                  (fun input ->
                    ignore
                      (Sys.opaque_identity
                         (Path.Abs.resolve abs_root input
                         |> ok "abs resolve" |> Path.Abs.hash)))
                  rel_inputs);
            bench "resolve_any" (fun () ->
                Array.iteri
                  (fun i _ ->
                    let input =
                      if i land 1 = 0 then
                        abs_inputs.(i mod Array.length abs_inputs)
                      else rel_inputs.(i mod Array.length rel_inputs)
                    in
                    ignore
                      (Sys.opaque_identity
                         (Path.Abs.resolve_any ~base:abs_root input
                         |> ok "abs resolve_any" |> Path.Abs.hash)))
                  rel_inputs);
          ];
      ]
