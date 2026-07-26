(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* A hermetic fake of Dune's RPC server, for the notices blackbox suite.

   mentat's build-health probe is registry-first: it reads Dune's XDG RPC
   registry to find an already-running [dune build --watch], connects to its
   socket, and requests diagnostics. This fake stands in for that watch WITHOUT
   any dune or compiler: it writes a registry entry pointing at its own Unix
   socket and answers the real dune-rpc client handshake (initialize /
   version_menu / diagnostics) with canned diagnostics, using dune-rpc's own
   wire types so the exchange stays faithful to the protocol mentat's client
   speaks. It exercises the fixed client end-to-end; it is not a general Dune
   server (it only implements what the health probe calls).

   Usage: mentat_fake_dune_rpc_server --root DIR --scenario failing|clean
                                      --ready FILE
   Discovery shares the process XDG environment, so the caller must run it under
   the same HOME/XDG_* as the mentat it should be visible to. *)

module Drpc = Dune_rpc.Private
module Conv = Drpc.Conv

type scenario = Failing | Clean

(* The negotiated dune version, learned from the client's initialize request and
   used for every subsequent Conv en/decode. dune-rpc.3.24 advertises (3, 24);
   reading it rather than pinning keeps the fake correct if the client changes
   what it sends. *)
let session_version = ref (3, 24)

let canned_diagnostic () =
  {
    Drpc.Diagnostic.targets = [];
    id = Drpc.Diagnostic.Id.create 1;
    message =
      Pp.verbatim
        "This expression has type string but an expression was expected of \
         type int";
    loc = None;
    severity = Some Drpc.Diagnostic.Error;
    promotion = [];
    directory = None;
    related = [];
  }

let diagnostics_for = function
  | Failing -> [ canned_diagnostic () ]
  | Clean -> []

(* --- Registry advertisement ------------------------------------------------ *)

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    let parent = Filename.dirname dir in
    if not (String.equal parent dir) then mkdir_p parent;
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path contents =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let advertise ~root ~socket =
  let xdg = Xdg.create ~env:Sys.getenv_opt () in
  let config = Drpc.Registry.Config.create xdg in
  let entry =
    Drpc.Registry.Dune.create ~where:(`Unix socket) ~root ~pid:(Unix.getpid ())
  in
  let (`Caller_should_write file) =
    Drpc.Registry.Config.register config entry
  in
  write_file file.Drpc.Registry.File.path file.Drpc.Registry.File.contents;
  file.Drpc.Registry.File.path

(* --- Handshake ------------------------------------------------------------- *)

let respond oc id result =
  Csexp.to_channel oc
    (Conv.to_sexp Drpc.Packet.sexp (Drpc.Packet.Response (id, result)));
  flush oc

let error ~message =
  Error
    (Drpc.Response.Error.create ~kind:Drpc.Response.Error.Invalid_request
       ~message ())

let handle_request scenario oc id (call : Drpc.Call.t) =
  let method_ = Drpc.Method.Name.to_string call.Drpc.Call.method_ in
  if String.equal method_ "initialize" then begin
    (match Drpc.Initialize.Request.of_call call ~version:!session_version with
    | Ok req -> session_version := Drpc.Initialize.Request.dune_version req
    | Error _ -> ());
    respond oc id
      (Ok
         (Drpc.Initialize.Response.to_response
            (Drpc.Initialize.Response.create ())))
  end
  else if String.equal method_ "version_menu" then
    match
      Drpc.Version_negotiation.Request.of_call call ~version:!session_version
    with
    | Ok (Drpc.Version_negotiation.Request.Menu offered) ->
        (* Select the highest version the client offered for each method: the
           client offered them, so it supports them, and this keeps diagnostics
           at its current generation (v2 -> Conv.list Diagnostic.sexp). *)
        let selected =
          List.map
            (fun (name, versions) -> (name, List.fold_left max 0 versions))
            offered
        in
        respond oc id
          (Ok
             (Drpc.Version_negotiation.Response.to_response
                (Drpc.Version_negotiation.Response.create selected)))
    | Error _ -> respond oc id (error ~message:"malformed version_menu request")
  else if String.equal method_ "diagnostics" then
    let payload =
      Conv.to_sexp (Conv.list Drpc.Diagnostic.sexp) (diagnostics_for scenario)
    in
    respond oc id (Ok payload)
  else respond oc id (error ~message:("unsupported method: " ^ method_))

(* One connection: [ic]/[oc] share [fd], so the caller owns closing [fd] once;
   here we only read requests, answer them, and flush. *)
let serve scenario fd =
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  let rec loop () =
    match Csexp.input ic with
    | Error _ -> () (* client closed the connection *)
    | Ok sexp -> (
        match Conv.of_sexp Drpc.Packet.sexp ~version:!session_version sexp with
        | Ok (Drpc.Packet.Request (id, call)) ->
            handle_request scenario oc id call;
            loop ()
        | Ok (Drpc.Packet.Notification _) | Ok (Drpc.Packet.Response _) ->
            loop ()
        | Error _ -> ())
  in
  loop ();
  try flush oc with Sys_error _ -> ()

(* --- Entry point ----------------------------------------------------------- *)

let realpath path =
  match Unix.realpath path with p -> p | exception Unix.Unix_error _ -> path

let () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let root = ref "" and scenario = ref Failing and ready = ref "" in
  let spec =
    [
      ("--root", Arg.Set_string root, "workspace root the endpoint advertises");
      ( "--scenario",
        Arg.String
          (function
          | "failing" -> scenario := Failing
          | "clean" -> scenario := Clean
          | other -> raise (Arg.Bad ("unknown scenario: " ^ other))),
        "failing|clean" );
      ("--ready", Arg.Set_string ready, "file to touch once listening");
    ]
  in
  Arg.parse spec
    (fun a -> raise (Arg.Bad ("unexpected argument: " ^ a)))
    "mentat_fake_dune_rpc_server";
  if String.equal !root "" then failwith "--root is required";
  let root = realpath !root in
  (* A short socket path under [/tmp]: a Unix socket cannot bind a long path (the
     ~104-byte sun_path limit), which rules out both the cram's deep [$PWD] and
     macOS's [/var/folders/...] [$TMPDIR]. The client discovers the path from the
     registry regardless of where it lives. *)
  let socket = Filename.temp_file ~temp_dir:"/tmp" "mentat-fake-rpc-" ".sock" in
  Unix.unlink socket;
  let registry_file = advertise ~root ~socket in
  let listen = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind listen (Unix.ADDR_UNIX socket);
  Unix.listen listen 8;
  let cleanup () =
    (try Unix.unlink socket with Unix.Unix_error _ -> ());
    try Unix.unlink registry_file with Unix.Unix_error _ -> ()
  in
  at_exit cleanup;
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle (fun _ -> exit 0)))
    [ Sys.sigterm; Sys.sigint ];
  if not (String.equal !ready "") then write_file !ready "ready\n";
  let rec accept_loop () =
    let fd, _ = Unix.accept listen in
    serve !scenario fd;
    (try Unix.close fd with Unix.Unix_error _ -> ());
    accept_loop ()
  in
  accept_loop ()
