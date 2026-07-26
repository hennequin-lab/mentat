(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Workspace = Mentat_workspace

let ( let* ) = Result.bind

module Drpc = Dune_rpc.Private

let log_src =
  Logs.Src.create "mentat.ocaml.dune.rpc" ~doc:"Dune RPC connection lifecycle"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* A [Dune_rpc.Fiber_intf.S] instance over direct-style Eio.

   The fiber type is a suspended computation ([unit -> 'a]), not the bare
   value ['a]. This deferral is load-bearing: [Dune_rpc.Private.Client] stores
   an unforced [Ivar.read] of its still-empty handler ivar in the client
   record and only forces it after version negotiation fills the ivar (see the
   [handler : _ Fiber.t] field). With the identity representation ['a t = 'a]
   that read is forced eagerly at client-creation time, [Eio.Promise.await]
   blocks the connecting fiber before the handshake fibers are even forked,
   and the whole connection deadlocks. Representing a fiber as a thunk keeps
   [Ivar.read] on an empty ivar a value, forced (and awaited) only when the
   library binds it. A fiber is forced with {!run}. *)
module Dune_rpc_fiber = struct
  type 'a t = unit -> 'a

  let run (t : 'a t) : 'a = t ()
  let return x () = x

  let fork_and_join_unit f g () =
    let result = ref None in
    Eio.Fiber.both
      (fun () -> run (f ()))
      (fun () -> result := Some (run (g ())));
    match !result with
    | Some value -> value
    | None -> invalid_arg "second fiber did not return"

  let parallel_iter next ~f () =
    let rec loop () =
      match run (next ()) with
      | None -> ()
      | Some value ->
          run (f value);
          loop ()
    in
    loop ()

  let finalize f ~finally () =
    match run (f ()) with
    | value ->
        run (finally ());
        value
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        run (finally ());
        Printexc.raise_with_backtrace exn backtrace

  let collect_errors f () = try Ok (run (f ())) with exn -> Error [ exn ]

  module O = struct
    let ( let* ) value f () = run (f (run value))
    let ( let+ ) value f () = f (run value)
  end

  module Ivar = struct
    type 'a t = 'a Eio.Promise.t * 'a Eio.Promise.u

    let create () = Eio.Promise.create ()
    let read (promise, _resolver) () = Eio.Promise.await promise
    let fill (_promise, resolver) value () = Eio.Promise.resolve resolver value
  end
end

module Dune_rpc_chan = struct
  (* Dune's RPC client drives the transport as two joined fibers: a reader
     loop and the request/notification driver (see [Client.connect_raw]). The
     driver ends a session by calling {!close}, and the reader loop only stops
     once a {!read} yields [None]. Closing the underlying Eio flow does not
     wake a fiber already blocked in a socket read, so a bare [Eio.Flow.close]
     leaves the reader parked forever and the joining [fork_and_join_unit]
     never returns. [close] therefore also resolves a [closed] promise that
     every [read] races against, delivering the [None] the client's contract
     expects when the connection is closed locally. *)
  type t =
    | Chan : {
        flow : _ Eio.Net.stream_socket;
        reader : Eio.Buf_read.t;
        closed : unit Eio.Promise.t;
        resolve_closed : unit Eio.Promise.u;
        mutable is_closed : bool;
      }
        -> t

  let of_flow flow =
    let closed, resolve_closed = Eio.Promise.create () in
    Chan
      {
        flow;
        reader = Eio.Buf_read.of_flow ~max_size:16_777_216 flow;
        closed;
        resolve_closed;
        is_closed = false;
      }

  let write (Chan { flow; _ }) sexps () =
    List.iter
      (fun sexp -> Eio.Flow.copy_string (Csexp.to_string sexp) flow)
      sexps

  let close (Chan c) () =
    if not c.is_closed then begin
      c.is_closed <- true;
      Eio.Promise.resolve c.resolve_closed ();
      Eio.Flow.close c.flow
    end

  (* One csexp, consumed incrementally from the shared buffered reader with
     Csexp's streaming lexer. Csexp is length-prefixed, so an atom body is
     taken in a single bulk read; the message costs O(size) rather than the
     quadratic re-parse of the whole accumulated buffer per byte, and each
     [Buf_read] refill is a scheduler yield, so a large message (e.g. a big
     Dune diagnostic set) never stalls the domain nor outruns a wrapping
     timeout. Only this sexp's bytes are consumed; any trailing bytes stay
     buffered for the next call. [End_of_file] (peer closed) yields [None]. *)
  let read_one reader =
    let module Parser = Csexp.Parser in
    let lexer = Parser.Lexer.create () in
    let rec loop stack =
      match Parser.Lexer.feed lexer (Eio.Buf_read.any_char reader) with
      | Parser.Lexer.Atom length ->
          let atom = Eio.Buf_read.take length reader in
          settle (Parser.Stack.add_atom atom stack)
      | (Parser.Lexer.Await | Parser.Lexer.Lparen | Parser.Lexer.Rparen) as
        token ->
          settle (Parser.Stack.add_token token stack)
    and settle stack =
      match stack with
      | Parser.Stack.Sexp (sexp, Parser.Stack.Empty) -> Some sexp
      | stack -> loop stack
    in
    try loop Parser.Stack.Empty with End_of_file -> None

  let read (Chan { reader; closed; is_closed; _ }) () =
    if is_closed then None
    else
      (* Race the next message against a local close: whichever settles first
         wins, and the loser (a parked read, or the close waiter) is
         cancelled. A local close thus surfaces as [None], exactly as a peer
         EOF does. *)
      Eio.Fiber.first
        (fun () ->
          Eio.Promise.await closed;
          None)
        (fun () -> read_one reader)
end

module Dune_rpc_client = Drpc.Client.Make (Dune_rpc_fiber) (Dune_rpc_chan)

let csexp_text sexp = Csexp.to_string sexp

let protocol_error ?payload message =
  Error
    (Error.Protocol_error { message; payload = Option.map csexp_text payload })

let response_error error =
  Error
    (Error.Protocol_error
       {
         message = Drpc.Response.Error.message error;
         payload = Option.map csexp_text (Drpc.Response.Error.payload error);
       })

let version_error error =
  Error
    (Error.Protocol_error
       {
         message = Drpc.Version_error.message error;
         payload = Option.map csexp_text (Drpc.Version_error.payload error);
       })

module Endpoint = struct
  type address = Unix of string | Tcp of { host : string; port : int }
  type t = { root : string; address : address }

  let make ~root address =
    if String.equal root "" then invalid_arg "root must not be empty";
    { root; address }

  let root t = t.root
  let address t = t.address

  let to_string t =
    match t.address with
    | Unix path -> t.root ^ " unix://" ^ path
    | Tcp { host; port } -> t.root ^ " tcp://" ^ host ^ ":" ^ string_of_int port

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Registry = struct
  module Dune_registry = Drpc.Registry

  type t = {
    config : Dune_registry.Config.t;
    mutable registry : Dune_registry.t;
  }

  exception Missing_registry_dir

  module Registry_fiber = struct
    include Dune_rpc_fiber

    let parallel_map xs ~f () = List.map (fun x -> run (f x)) xs
  end

  let create ~env () =
    let config = Dune_registry.Config.create (Xdg.create ~env ()) in
    { config; registry = Dune_registry.create config }

  let reset t = t.registry <- Dune_registry.create t.config
  let current t = Dune_registry.current t.registry
  let root = Dune_registry.Dune.root
  let pid = Dune_registry.Dune.pid

  let endpoint entry =
    let address =
      match Dune_registry.Dune.where entry with
      | `Unix path -> Endpoint.Unix path
      | `Ip (`Host host, `Port port) -> Endpoint.Tcp { host; port }
    in
    Endpoint.make ~root:(root entry) address

  let poll ~fs t =
    let module Poll =
      Dune_registry.Poll
        (Registry_fiber)
        (struct
          let with_error f = try Ok (f ()) with exn -> Error exn
          let path raw = Eio.Path.( / ) fs raw

          let scandir raw =
            Dune_rpc_fiber.return
              (match Eio.Path.kind ~follow:true (path raw) with
              | `Not_found -> Ok []
              | _ -> with_error (fun () -> Eio.Path.read_dir (path raw)))

          let stat raw =
            Dune_rpc_fiber.return
              (match Eio.Path.kind ~follow:true (path raw) with
              | `Not_found -> Error Missing_registry_dir
              | _ ->
                  with_error (fun () ->
                      `Mtime
                        (Eio.Path.stat ~follow:true (path raw))
                          .Eio.File.Stat.mtime))

          let read_file raw =
            Dune_rpc_fiber.return
              (with_error (fun () -> Eio.Path.load (path raw)))
        end) in
    reset t;
    match Dune_rpc_fiber.run (Poll.poll t.registry) with
    | Ok refresh -> (
        match Dune_registry.Refresh.errored refresh with
        | [] -> Ok (current t)
        | (path, exn) :: remaining ->
            Error
              (Error.Connection_failed
                 {
                   endpoint = path;
                   message =
                     Printexc.to_string exn ^ " ("
                     ^ string_of_int (List.length remaining + 1)
                     ^ " registry error(s))";
                 }))
    | Error Missing_registry_dir -> Ok (current t)
    | Error exn ->
        Error
          (Error.Connection_failed
             {
               endpoint = "dune rpc registry";
               message = Printexc.to_string exn;
             })
end

module Diagnostic = struct
  module Id = struct
    type t = string

    let of_string value =
      if String.equal value "" then
        invalid_arg "diagnostic id must not be empty";
      value
  end

  type id = Id.t

  module Store = struct
    (* Diagnostics arrive only as a full one-shot snapshot from
       [request_diagnostics], so the store is built purely by adding each
       entry (deduping by id). The deleted streaming subscription applied
       incremental [Remove] events; a full snapshot has no analogue — an entry
       absent from the new set is simply never added — so dropping [Remove]
       changes nothing observable here. A live-diagnostics panel restoring the
       subscription would reintroduce it. *)
    type t = (id * Mentat_ocaml.Diagnostic.t) list

    let empty = []
    let add id diagnostic t = (id, diagnostic) :: List.remove_assoc id t
    let to_list t = List.rev t
  end
end

module Connection = struct
  type t = { workspace : Workspace.t option; client : Dune_rpc_client.t }

  let make ~client ?workspace () = { workspace; client }

  let init_request =
    Drpc.Initialize.Request.create
      ~id:
        (Drpc.Id.make
           (Csexp.List [ Csexp.Atom "mentat"; Csexp.Atom "ocaml-dune" ]))

  let unix_socket_path_limit = 100

  let is_dir path =
    match Sys.is_directory path with
    | true -> true
    | false -> false
    | exception Sys_error _ -> false

  let temp_dir () =
    if is_dir "/tmp" then "/tmp" else Filename.get_temp_dir_name ()

  let realpath path =
    match Unix.realpath path with
    | path -> Some path
    | exception Unix.Unix_error _ -> None

  let normalize_dir path =
    match Filename.chop_suffix_opt ~suffix:Filename.dir_sep path with
    | Some path -> path
    | None -> path

  let drop_root ~root path =
    let root = normalize_dir root in
    if String.equal path root then Some ""
    else
      let prefix = root ^ Filename.dir_sep in
      if String.starts_with ~prefix path then
        Some (String.drop_first (String.length prefix) path)
      else None

  let realpath_with_basename path =
    let dir = Filename.dirname path in
    let base = Filename.basename path in
    Option.map (fun dir -> Filename.concat dir base) (realpath dir)

  let workspace_relative_socket endpoint path =
    let roots =
      Endpoint.root endpoint
      :: Option.to_list (realpath (Endpoint.root endpoint))
    in
    let paths = path :: Option.to_list (realpath_with_basename path) in
    List.find_map
      (fun root ->
        List.find_map
          (fun path ->
            Option.map (fun rel -> (root, rel)) (drop_root ~root path))
          paths)
      roots

  let with_short_workspace_socket endpoint path f =
    match workspace_relative_socket endpoint path with
    | None -> f path
    | Some (root, rel) -> (
        let link =
          Filename.temp_file ~temp_dir:(temp_dir ()) "mentat-dune-rpc-" ""
        in
        match
          Unix.unlink link;
          Unix.symlink root link
        with
        | () ->
            Fun.protect
              ~finally:(fun () ->
                match Unix.unlink link with
                | () -> ()
                | exception Unix.Unix_error _ -> ())
              (fun () -> f (Filename.concat link rel))
        | exception Unix.Unix_error _ -> f path)

  let connect_unix ~sw net endpoint path f =
    if String.length path <= unix_socket_path_limit then
      let flow = Eio.Net.connect ~sw net (`Unix path) in
      f flow
    else
      with_short_workspace_socket endpoint path @@ fun path ->
      let flow = Eio.Net.connect ~sw net (`Unix path) in
      f flow

  let connect_flow ~sw ~net endpoint f =
    match Endpoint.address endpoint with
    | Endpoint.Unix path -> connect_unix ~sw net endpoint path f
    | Endpoint.Tcp { host; port } ->
        Eio.Net.with_tcp_connect ~host ~service:(string_of_int port) net f

  let with_connection ~sw ~net ?workspace endpoint ~f =
    let endpoint_text = Endpoint.to_string endpoint in
    try
      connect_flow ~sw ~net endpoint @@ fun flow ->
      let chan = Dune_rpc_chan.of_flow flow in
      Dune_rpc_fiber.run
        (Dune_rpc_client.connect chan init_request ~f:(fun client ->
             let t = make ~client ?workspace () in
             Dune_rpc_fiber.return (f t)))
    with
    | Drpc.Response.Error.E error -> response_error error
    | Drpc.Version_error.E error -> version_error error
    | exn ->
        Error
          (Error.Connection_failed
             { endpoint = endpoint_text; message = Printexc.to_string exn })

  let prepare_request client request =
    match
      Dune_rpc_fiber.run
        (Dune_rpc_client.Versioned.prepare_request client request)
    with
    | Ok request -> Ok request
    | Error error -> version_error error

  let request ?id client request params =
    let* request = prepare_request client request in
    match
      Dune_rpc_fiber.run (Dune_rpc_client.request ?id client request params)
    with
    | Ok value -> Ok value
    | Error error -> response_error error

  let pp_text pp = String.trim (Format.asprintf "%a@." Drpc.Pp.to_fmt pp)

  let severity diagnostic =
    match Drpc.Diagnostic.severity diagnostic with
    | Some Drpc.Diagnostic.Error -> Mentat_ocaml.Diagnostic.Severity.Error
    | Some Drpc.Diagnostic.Warning -> Mentat_ocaml.Diagnostic.Severity.Warning
    | None -> Mentat_ocaml.Diagnostic.Severity.Information

  let diagnostic_payload_error message =
    protocol_error ("invalid Dune RPC diagnostic payload: " ^ message)

  let construct_diagnostic f =
    try Ok (f ())
    with Invalid_argument message -> diagnostic_payload_error message

  let position (position : Lexing.position) =
    let column = max 0 (position.Lexing.pos_cnum - position.Lexing.pos_bol) in
    Mentat_ocaml.Position.make ~line:(max 1 position.Lexing.pos_lnum) ~column

  let location_of_dune t loc =
    let path = loc.Drpc.Loc.start.Lexing.pos_fname in
    match t.workspace with
    | None -> Ok None
    | Some workspace -> (
        match Workspace.resolve_string workspace path with
        | Ok path ->
            let start = position (Drpc.Loc.start loc) in
            let end_ = position (Drpc.Loc.stop loc) in
            construct_diagnostic (fun () ->
                Some
                  (Mentat_ocaml.Location.make ~path
                     ~range:(Mentat_ocaml.Range.make ~start ~end_)))
        | Error _ -> Ok None)

  let related_of_dune t related =
    let* location = location_of_dune t (Drpc.Diagnostic.Related.loc related) in
    construct_diagnostic (fun () ->
        Mentat_ocaml.Diagnostic.Related.make ?location
          (pp_text (Drpc.Diagnostic.Related.message related)))

  let diagnostic_of_dune t diagnostic =
    let message = pp_text (Drpc.Diagnostic.message diagnostic) in
    let rec related_loop acc = function
      | [] -> Ok (List.rev acc)
      | related :: rest -> (
          match related_of_dune t related with
          | Ok related -> related_loop (related :: acc) rest
          | Error _ as error -> error)
    in
    let* related = related_loop [] (Drpc.Diagnostic.related diagnostic) in
    let* location =
      match Drpc.Diagnostic.loc diagnostic with
      | None -> Ok None
      | Some location -> location_of_dune t location
    in
    construct_diagnostic (fun () ->
        Mentat_ocaml.Diagnostic.make ?location ~related
          ~source:Mentat_ocaml.Diagnostic.Source.dune
          ~severity:(severity diagnostic) message)

  let diagnostic_id diagnostic =
    Drpc.Diagnostic.id diagnostic
    |> Drpc.Diagnostic.Id.hash |> string_of_int |> Diagnostic.Id.of_string

  let request_diagnostics t =
    let* diagnostics = request t.client Drpc.Public.Request.diagnostics () in
    let rec loop store = function
      | [] -> Ok store
      | dune_diagnostic :: rest -> (
          match diagnostic_of_dune t dune_diagnostic with
          | Ok diagnostic ->
              loop
                (Diagnostic.Store.add
                   (diagnostic_id dune_diagnostic)
                   diagnostic store)
                rest
          | Error _ as error -> error)
    in
    let* store = loop Diagnostic.Store.empty diagnostics in
    Log.debug (fun m ->
        m "fetched dune diagnostics count=%d"
          (List.length (Diagnostic.Store.to_list store)));
    Ok store
end

module Instance = struct
  type fs = Fs : _ Eio.Path.t -> fs
  type net = Net : _ Eio.Net.t -> net

  type t = {
    fs : fs;
    net : net;
    workspace : Workspace.t;
    registry : Registry.t;
    mutex : Eio.Mutex.t;
    mutable endpoint : Endpoint.t option;
    mutable diagnostics : Diagnostic.Store.t;
  }

  let create ~fs ~net ~workspace ?(env = Sys.getenv_opt) () =
    {
      fs = Fs fs;
      net = Net net;
      workspace;
      registry = Registry.create ~env ();
      mutex = Eio.Mutex.create ();
      endpoint = None;
      diagnostics = Diagnostic.Store.empty;
    }

  let with_lock t f = Eio.Mutex.use_rw ~protect:true t.mutex f
  let diagnostics t = with_lock t (fun () -> t.diagnostics)

  let workspace_root_strings workspace =
    List.map
      (fun root -> Lpath.Abs.to_string (Workspace.Root.dir root))
      (Workspace.roots workspace)

  let normalize_abs path =
    match Lpath.Abs.of_string path with
    | Ok abs ->
        let path = Lpath.Abs.to_string abs in
        Some
          (match Unix.realpath path with
          | path -> path
          | exception Unix.Unix_error _ -> path)
    | Error _ -> None

  let same_root a b =
    String.equal a b
    ||
    match (normalize_abs a, normalize_abs b) with
    | Some a, Some b -> String.equal a b
    | Some _, None | None, Some _ | None, None -> false

  let process_alive pid =
    if pid <= 0 then false
    else
      match Unix.kill pid 0 with
      | () -> true
      | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
      | exception Unix.Unix_error (Unix.EPERM, _, _) -> true
      | exception Unix.Unix_error _ -> false

  let choose_registry_entry ~workspace entries =
    let roots = workspace_root_strings workspace in
    List.find_opt
      (fun entry ->
        process_alive (Registry.pid entry)
        && List.exists (same_root (Registry.root entry)) roots)
      entries

  let refresh_unlocked t =
    let (Fs fs) = t.fs in
    let previous = t.endpoint in
    let note_lost () =
      if Option.is_some previous then
        Log.info (fun m -> m "dune rpc endpoint lost")
    in
    let* entries = Registry.poll ~fs t.registry in
    match choose_registry_entry ~workspace:t.workspace entries with
    | None ->
        note_lost ();
        t.endpoint <- None;
        Ok None
    | Some entry ->
        let endpoint = Registry.endpoint entry in
        (match previous with
        | Some prev
          when String.equal (Endpoint.to_string prev)
                 (Endpoint.to_string endpoint) ->
            ()
        | _ ->
            Log.info (fun m ->
                m "dune rpc endpoint found endpoint=%a" Endpoint.pp endpoint));
        t.endpoint <- Some endpoint;
        Ok (Some endpoint)

  let refresh t =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> refresh_unlocked t)

  module Health = struct
    type t = Disconnected | Clean | Failing of int | Unknown

    let equal (a : t) (b : t) =
      match (a, b) with
      | Disconnected, Disconnected | Clean, Clean | Unknown, Unknown -> true
      | Failing a, Failing b -> Int.equal a b
      | (Disconnected | Clean | Failing _ | Unknown), _ -> false

    let pp ppf : t -> unit = function
      | Disconnected -> Format.pp_print_string ppf "disconnected"
      | Clean -> Format.pp_print_string ppf "clean"
      | Failing n -> Format.fprintf ppf "failing %d" n
      | Unknown -> Format.pp_print_string ppf "unknown"
  end

  (* A registry-first, no-spawn health probe: [refresh] only observes an
     already-running instance, and the current-diagnostics request is bounded
     so a slow Dune build cannot stall a frontend at launch. A found endpoint
     whose request times out or fails is still connected — {!Health.Unknown},
     not {!Health.Disconnected}. *)
  let build_health t ~clock ?(timeout_s = 0.5) () =
    match refresh t with
    | Error _ | Ok None -> Health.Disconnected
    | Ok (Some endpoint) -> (
        let (Net net) = t.net in
        let query () =
          Eio.Switch.run @@ fun sw ->
          Connection.with_connection ~sw ~net ~workspace:t.workspace endpoint
            ~f:(fun connection -> Connection.request_diagnostics connection)
        in
        match Eio.Time.with_timeout_exn clock timeout_s query with
        | exception Eio.Time.Timeout -> Health.Unknown
        | Error _ -> Health.Unknown
        | Ok store ->
            with_lock t (fun () ->
                t.endpoint <- Some endpoint;
                t.diagnostics <- store);
            let count = List.length (Diagnostic.Store.to_list store) in
            if count = 0 then Health.Clean else Health.Failing count)
end
