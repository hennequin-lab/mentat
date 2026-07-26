(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

module Store = Mentat_provider.Credential.Store
module Name = Mentat_provider.Credential.Name

let log_src =
  Logs.Src.create "mentat.provider_runtime.store" ~doc:"Credential store access"

module Log = (val Logs.src_log log_src : Logs.LOG)

type t = {
  config_dir : Eio.Fs.dir_ty Eio.Path.t;
  store_native : string; (* Native path of [auth.json], for the [.lock] files. *)
}

let make ~config_dir =
  let native =
    match Eio.Path.native config_dir with
    | Some dir -> dir
    | None ->
        invalid_arg
          "Credential_store.make: config_dir must be a native filesystem path"
  in
  { config_dir; store_native = Filename.concat native "auth.json" }

let store_file t = Eio.Path.( / ) t.config_dir "auth.json"
let io_error message = Error (Store_error.Io message)
let locked_error message = Error (Store_error.Locked message)

let ensure_dir t =
  match Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 t.config_dir with
  | () -> Ok ()
  | exception exn -> io_error (Printexc.to_string exn)

let tmp_counter = Atomic.make 0

let tmp_name env =
  let counter = Atomic.fetch_and_add tmp_counter 1 in
  let stamp =
    Eio.Time.now (Eio.Stdenv.clock env)
    |> Int64.bits_of_float |> Int64.to_string
  in
  Printf.sprintf "auth.json.tmp.%d.%s.%d" (Unix.getpid ()) stamp counter

(* In-process + filesystem locking. *)

type lock_mutex = { mutex : Eio.Mutex.t; mutable users : int }

let lock_mutexes : (string, lock_mutex) Hashtbl.t = Hashtbl.create 8
let lock_mutexes_guard = Mutex.create ()

let acquire_lock_mutex key =
  Mutex.protect lock_mutexes_guard (fun () ->
      match Hashtbl.find_opt lock_mutexes key with
      | Some entry ->
          entry.users <- entry.users + 1;
          entry
      | None ->
          let entry = { mutex = Eio.Mutex.create (); users = 1 } in
          Hashtbl.add lock_mutexes key entry;
          entry)

let release_lock_mutex key entry =
  Mutex.protect lock_mutexes_guard (fun () ->
      match Hashtbl.find_opt lock_mutexes key with
      | Some current when current == entry ->
          assert (entry.users > 0);
          entry.users <- entry.users - 1;
          if entry.users = 0 then Hashtbl.remove lock_mutexes key
      | Some _ | None -> assert false)

let canonical_lock_key path =
  try
    let dir =
      Eio_unix.run_in_systhread ~label:"mentat-auth-lock-realpath" (fun () ->
          Unix.realpath (Filename.dirname path))
    in
    dir ^ "/" ^ Filename.basename path
  with Unix.Unix_error _ -> path

exception Lock_failure of exn

let lock_operation f =
  match f () with
  | value -> value
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception (Lock_failure _ as exn) -> raise exn
  | exception exn -> raise (Lock_failure exn)

let preserving_cleanup ~path cleanup f =
  match f () with
  | value ->
      cleanup ();
      value
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      (match cleanup () with
      | () -> ()
      | exception Lock_failure cleanup_error ->
          Log.err (fun m ->
              m "account lock cleanup failed path=%s error=%s" path
                (Printexc.to_string cleanup_error)));
      Printexc.raise_with_backtrace exn backtrace

let with_lock ~env path f =
  let rec lockf fd command =
    match Unix.lockf fd command 0 with
    | () -> ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> lockf fd command
  in
  let lock_path = path ^ ".lock" in
  let key = canonical_lock_key lock_path in
  let entry = acquire_lock_mutex key in
  Fun.protect
    ~finally:(fun () -> release_lock_mutex key entry)
    (fun () ->
      Eio.Mutex.use_ro entry.mutex (fun () ->
          let fd =
            lock_operation (fun () ->
                Unix.openfile lock_path
                  [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ]
                  0o600)
          in
          preserving_cleanup ~path:lock_path
            (fun () -> lock_operation (fun () -> Unix.close fd))
            (fun () ->
              let rec acquire backoff =
                match Unix.lockf fd Unix.F_TLOCK 0 with
                | () -> ()
                | exception Unix.Unix_error (Unix.EINTR, _, _) ->
                    acquire backoff
                | exception Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _)
                  ->
                    Eio.Time.sleep (Eio.Stdenv.clock env) backoff;
                    acquire (Float.min (backoff *. 2.) 0.1)
                | exception exn -> raise (Lock_failure exn)
              in
              acquire 0.001;
              preserving_cleanup ~path:lock_path
                (fun () -> lock_operation (fun () -> lockf fd Unix.F_ULOCK))
                f)))

let with_store_lock t ~env f =
  let* () = ensure_dir t in
  match with_lock ~env t.store_native f with
  | result -> result
  | exception Lock_failure exn ->
      locked_error (t.store_native ^ ".lock: " ^ Printexc.to_string exn)

(* The lock slot is a filesystem-safe, collision-free key over the constrained
   provider-id and credential-name identifier charsets; it needs no digest. *)
let credential_lock_native t provider name =
  let provider = Mentat_llm.Provider.id provider in
  let name = Name.to_string name in
  Printf.sprintf "%s.credential-%d-%s-%s" t.store_native
    (String.length provider) provider name

let with_credential_lock t ~env ~provider ~name f =
  let* () = ensure_dir t in
  let path = credential_lock_native t provider name in
  match with_lock ~env path f with
  | result -> result
  | exception Lock_failure exn ->
      locked_error (path ^ ".lock: " ^ Printexc.to_string exn)

(* File load and save. *)

let load t =
  let path = store_file t in
  if Eio.Path.is_file path then
    match Eio.Path.load path with
    | exception exn -> io_error (t.store_native ^ ": " ^ Printexc.to_string exn)
    | text -> (
        match Jsont_bytesrw.decode_string Store.jsont text with
        | Ok store -> Ok store
        | Error _ ->
            (* The public message is generic so the secret-confinement guarantee
               rests on this library's own code, not on the decoder's rendering
               over a secret-bearing file. The log is deliberately payload-free
               for the same reason. *)
            Log.debug (fun m -> m "auth.json decode failed");
            Error
              (Store_error.Decode
                 (t.store_native
                ^ ": not a valid credential store (corrupt or obsolete format)"
                 )))
  else if Eio.Path.is_directory path then
    io_error (t.store_native ^ ": is a directory")
  else Ok Store.empty

let save t ~env store =
  let* () = ensure_dir t in
  match Jsont_bytesrw.encode_string Store.jsont store with
  | Error _ -> failwith "credential store encoding invariant violated"
  | Ok text -> (
      let tmp = Eio.Path.( / ) t.config_dir (tmp_name env) in
      match Eio.Path.save ~create:(`Exclusive 0o600) tmp (text ^ "\n") with
      | exception exn ->
          io_error (t.store_native ^ ": " ^ Printexc.to_string exn)
      | () -> (
          match Eio.Path.rename tmp (store_file t) with
          | () ->
              Log.debug (fun m -> m "auth store written");
              Ok ()
          | exception exn ->
              (* Remove the temp under [Cancel.protect]: on a cancel raised at the
                 rename the unlink would itself be cancelled and leave the temp
                 orphaned. The real [auth.json] is untouched (atomic rename). *)
              Eio.Cancel.protect (fun () ->
                  try Eio.Path.unlink ~missing_ok:true tmp
                  with cleanup ->
                    Log.debug (fun m ->
                        m "auth store temp cleanup failed: %s"
                          (Printexc.to_string cleanup)));
              io_error (t.store_native ^ ": " ^ Printexc.to_string exn)))

let edit t ~env ~f =
  with_store_lock t ~env (fun () ->
      let* store = load t in
      let store, value = f store in
      let* () = save t ~env store in
      Ok value)
