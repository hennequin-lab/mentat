(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
module Health = Mentat_ocaml_dune_rpc.Instance.Health

let realpath_or path =
  match Unix.realpath path with p -> p | exception Unix.Unix_error _ -> path

(* Canonicalise: Dune registers its RPC endpoint under the realpath of the
   root, and the instance matches endpoints against the workspace root after
   [realpath]; on macOS the temp directory is a [/var/...] path that is a
   symlink to [/private/var/...]. *)
let with_temp_dir f =
  f (realpath_or (temp_dir ~prefix:"mentat-dune-rpc-test" ()))

let write_file path contents =
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc contents)

let make_project ~root ~broken =
  Unix.mkdir (Filename.concat root "lib") 0o755;
  write_file (Filename.concat root "dune-project") "(lang dune 3.22)\n";
  write_file (Filename.concat root "lib/dune") "(library\n (name probe_lib))\n";
  write_file
    (Filename.concat root "lib/probe_lib.ml")
    (if broken then "let (x : int) = \"deliberate type error\"\n"
     else "let x = 42\nlet () = ignore x\n")

let workspace_at root =
  Mentat_workspace.single
    (Mentat_workspace.Root.of_dir (Lpath.Abs.of_string_exn root))

(* Spawn a real [dune build --root <root> --watch @check], killed when [sw]
   ends. Stdout/stderr are shell-redirected to /dev/null so the watch's chatter
   stays out of the test log. [ocamlc]'s directory is placed on the child's
   PATH so the watch has a compiler regardless of how it was discovered. *)
let spawn_watch ~sw ~env ~toolchain ~dune ~root =
  let child_env = Mentat_ocaml_toolchain.env toolchain ~program:"ocamlc" in
  let command =
    [
      "/bin/sh";
      "-c";
      "exec \"$1\" build --root \"$2\" --watch @check >/dev/null 2>&1";
      "sh";
      dune;
      root;
    ]
  in
  let process =
    Eio.Process.spawn ~sw
      (Eio.Stdenv.process_mgr env)
      ~cwd:(Eio.Path.( / ) (Eio.Stdenv.fs env) root)
      ~stdin:(Eio.Flow.string_source "")
      ~env:child_env command
  in
  Eio.Switch.on_release sw (fun () ->
      (try Eio.Process.signal process Sys.sigkill with _ -> ());
      match Unix.kill (Eio.Process.pid process) Sys.sigkill with
      | () | (exception Unix.Unix_error _) -> ());
  process

(* Poll [build_health] until it settles on a verdict [wanted] accepts, or the
   deadline passes; return the last verdict seen. A found-but-not-yet-built
   watch reports [Disconnected]/[Clean] transiently, so we wait for the target
   rather than trusting the first concrete answer. *)
let await_verdict ~clock instance ~wanted ~timeout_s =
  let deadline = Eio.Time.now clock +. timeout_s in
  let rec loop () =
    let verdict =
      Mentat_ocaml_dune_rpc.Instance.build_health instance ~clock ()
    in
    if wanted verdict || Eio.Time.now clock >= deadline then verdict
    else begin
      Eio.Time.sleep clock 0.1;
      loop ()
    end
  in
  loop ()

(* Hermetic: with no watch registered for a fresh workspace, [build_health]
   returns the concrete [Disconnected] verdict promptly — it never blocks. This
   is the registry-only path (no connection attempt). *)
let build_health_disconnected () =
  with_temp_dir @@ fun root ->
  make_project ~root ~broken:true;
  Eio_main.run @@ fun eio_env ->
  let instance =
    Mentat_ocaml_dune_rpc.Instance.create ~fs:(Eio.Stdenv.fs eio_env)
      ~net:(Eio.Stdenv.net eio_env) ~workspace:(workspace_at root) ()
  in
  let clock = Eio.Stdenv.clock eio_env in
  let verdict =
    Eio.Time.with_timeout_exn clock 5.0 (fun () ->
        Mentat_ocaml_dune_rpc.Instance.build_health instance ~clock ())
  in
  is_true
    ~msg:
      (Format.asprintf "no watch registered => Disconnected, got %a" Health.pp
         verdict)
    (Health.equal verdict Health.Disconnected)

(* Live end-to-end: connect the RPC client to a real running Dune and confirm
   the handshake completes and the verdict is truthful — [Failing] for a
   type-erroring project, [Clean] for a good one.

   Opt-in via [MENTAT_DUNE_RPC_LIVE_TEST]: it spawns real [dune build --watch]
   processes and reads Dune's XDG RPC registry, which the [dune runtest] sandbox
   isolates (registry entries a spawned watch writes are not observable from
   inside it). Left ungated it would flake or fail there, so the default suite
   skips it; a developer runs it directly with a compiler on PATH
   ([MENTAT_DUNE_RPC_LIVE_TEST=1 dune exec test/unit/test_ocaml_dune.exe]) to
   exercise the real client handshake — the concern the reducible unit tests
   cannot reach. *)
let build_health_live () =
  match Sys.getenv_opt "MENTAT_DUNE_RPC_LIVE_TEST" with
  | None | Some "" | Some "0" ->
      Printf.eprintf
        "[skip] live Dune RPC build-health: set MENTAT_DUNE_RPC_LIVE_TEST=1 \
         (with dune + ocamlc discoverable) to exercise the live handshake\n\
         %!"
  | Some _ -> (
      let toolchain =
        Mentat_ocaml_toolchain.discover ~env:(Unix.environment ())
          ~workspace_root:None
      in
      match
        ( Mentat_ocaml_toolchain.find toolchain "dune",
          Mentat_ocaml_toolchain.find toolchain "ocamlc" )
      with
      | Some (dune, _), Some (_ocamlc, _) ->
          Eio_main.run @@ fun eio_env ->
          let fs = Eio.Stdenv.fs eio_env in
          let net = Eio.Stdenv.net eio_env in
          let clock = Eio.Stdenv.clock eio_env in
          let check ~broken ~wanted ~label =
            with_temp_dir @@ fun root ->
            make_project ~root ~broken;
            Eio.Switch.run @@ fun sw ->
            let _watch = spawn_watch ~sw ~env:eio_env ~toolchain ~dune ~root in
            let instance =
              Mentat_ocaml_dune_rpc.Instance.create ~fs ~net
                ~workspace:(workspace_at root) ()
            in
            let verdict =
              await_verdict ~clock instance ~wanted ~timeout_s:20.0
            in
            is_true
              ~msg:
                (Format.asprintf "%s: expected %s, got %a" label
                   (if broken then "Failing" else "Clean")
                   Health.pp verdict)
              (wanted verdict)
          in
          check ~broken:true ~label:"broken project" ~wanted:(function
            | Health.Failing n -> n >= 1
            | _ -> false);
          check ~broken:false ~label:"clean project" ~wanted:(function
            | Health.Clean -> true
            | _ -> false)
      | _ ->
          failf
            "MENTAT_DUNE_RPC_LIVE_TEST is set but no dune + ocamlc toolchain \
             is discoverable; put a compiler on PATH or set OPAM_SWITCH_PREFIX")

let () =
  run "mentat.ocaml.dune_rpc"
    [
      test "build_health is Disconnected without a watch"
        build_health_disconnected;
      test "build_health against a live dune watch" build_health_live;
    ]
