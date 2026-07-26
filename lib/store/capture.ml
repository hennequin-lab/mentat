(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Content_ref = Mentat_digest.Content_ref
module Rel = Lpath.Rel

(* Errors. *)

module Error = struct
  type t = { path : string; detail : string }

  let message t = Printf.sprintf "%s: %s" t.path t.detail
  let pp ppf t = Format.pp_print_string ppf (message t)
end

exception Failure_at of Error.t

let fail ~path detail = raise (Failure_at { Error.path; detail })
let failf ~path fmt = Printf.ksprintf (fun detail -> fail ~path detail) fmt

let rendered = function
  | Eio.Io _ as exn -> Format.asprintf "%a" Eio.Exn.pp exn
  | Unix.Unix_error (e, _, _) -> Unix.error_message e
  | exn -> Printexc.to_string exn

let guard ~label f =
  match f () with
  | value -> Ok value
  | exception Failure_at e -> Error e
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn -> Error { Error.path = label; detail = rendered exn }

(* Durable filesystem mechanics.

   The same temp then fsync then rename then directory-fsync shape as
   [Disk.replace], deliberately restated rather than shared: this view fails on
   a presentation-only channel — a capture failure degrades a checkpoint to
   [Degraded] — so it raises {!Failure_at} with a flat [Error.t] instead of
   threading the domain views' [Io]-typed [Disk] primitives. *)

let fd_of resource =
  match Eio_unix.Resource.fd_opt resource with
  | Some fd -> fd
  | None -> invalid_arg "Mentat_store.Capture: opened capability has no fd"

let rec fsync_fd fd =
  match Unix.fsync fd with
  | () -> ()
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> fsync_fd fd

let rec write_at fd ~path text offset =
  if offset < String.length text then
    match Unix.write_substring fd text offset (String.length text - offset) with
    | 0 -> fail ~path "no bytes written"
    | written -> write_at fd ~path text (offset + written)
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        write_at fd ~path text offset

let write_all fd ~path text = write_at fd ~path text 0

let fsync_dir ~path dir =
  Eio.Switch.run @@ fun sw ->
  (* [dir / "."], never the bare capability: an opened directory's relative
     path is empty, and openat2 rejects an empty path with ENOENT (the
     io_uring backend); "." names the directory on every backend. *)
  let resource =
    try Eio.Path.open_in ~sw (Eio.Path.( / ) dir ".")
    with exn -> failf ~path "open for sync: %s" (rendered exn)
  in
  Eio_unix.Fd.use_exn "snapshot dir sync" (fd_of resource) (fun ufd ->
      Eio_unix.run_in_systhread ~label:"snapshot dir sync" (fun () ->
          fsync_fd ufd))

let tmp_counter = Atomic.make 0

let tmp_name name =
  let count = Atomic.fetch_and_add tmp_counter 1 + 1 in
  Printf.sprintf "%s.tmp.%d.%d" name (Unix.getpid ()) count

let mkdirs ~path dir =
  try Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir
  with exn -> failf ~path "mkdir: %s" (rendered exn)

let durable_replace ~dir ~dir_path ~name bytes =
  Eio.Switch.run @@ fun sw ->
  let tmp = tmp_name name in
  let tmp_cap = Eio.Path.( / ) dir tmp in
  let tmp_path = dir_path ^ "/" ^ tmp in
  let target_cap = Eio.Path.( / ) dir name in
  let target_path = dir_path ^ "/" ^ name in
  let renamed = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not !renamed then
        try Eio.Path.unlink ~missing_ok:true tmp_cap with _ -> ())
    (fun () ->
      let resource =
        try Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) tmp_cap
        with exn -> failf ~path:tmp_path "open tmp: %s" (rendered exn)
      in
      Eio_unix.Fd.use_exn "snapshot write" (fd_of resource) (fun fd ->
          Eio_unix.run_in_systhread ~label:"snapshot write" (fun () ->
              write_all fd ~path:tmp_path bytes;
              fsync_fd fd));
      (try Eio.Path.rename tmp_cap target_cap
       with exn -> failf ~path:target_path "rename: %s" (rendered exn));
      renamed := true;
      (* After the rename the bytes are published; a directory-sync failure now
         leaves durable state changed, which the write-once read path tolerates
         (the bytes match their content-addressed name either way). *)
      fsync_dir ~path:dir_path dir)

let path_present cap =
  match Eio.Path.kind ~follow:false cap with
  | `Regular_file -> true
  | _ -> false
  | exception _ -> false

let load_bytes ~path cap =
  try Eio.Path.load cap with exn -> failf ~path "read: %s" (rendered exn)

(* The store.

   The store resolves under the opened store root: it holds the root handle and
   its [snapshots/<key>] base, and every blob capability is reached through
   [Handle.path] — the same [openat]-based path every store view uses, so a root
   rename can never split a capture across two physical trees. *)

module Store = struct
  type t = { root : Handle.t; base : string }

  let create root ~key =
    if String.length key = 0 then
      invalid_arg "Mentat_store.Capture.Store.create: key must not be empty";
    { root; base = Layout.snapshots_dir ^ "/" ^ key }
end

(* A blob's on-disk location: a two-hex shard from the digest, and a filename
   that is the content token with the token separator mapped to a portable path
   character (mirrors [Layout.blob_name]). *)

let blob_shard ref =
  String.sub (Mentat_digest.to_hex (Content_ref.digest ref)) 0 2

let blob_filename ref =
  String.map (function ':' -> '-' | c -> c) (Content_ref.to_token ref)

let shard_rel store shard = store.Store.base ^ "/blobs/" ^ shard

let write_blob store ref bytes =
  let shard = blob_shard ref in
  let name = blob_filename ref in
  let shard_rel = shard_rel store shard in
  let shard_dir = Handle.path store.Store.root shard_rel in
  let target = Eio.Path.( / ) shard_dir name in
  (* Write-once: an identical content-addressed blob is already durable. *)
  if not (path_present target) then begin
    mkdirs ~path:shard_rel shard_dir;
    durable_replace ~dir:shard_dir ~dir_path:shard_rel ~name bytes
  end

let read_blob store ref =
  let shard = blob_shard ref in
  let name = blob_filename ref in
  let shard_rel = shard_rel store shard in
  let path = shard_rel ^ "/" ^ name in
  let target = Eio.Path.( / ) (Handle.path store.Store.root shard_rel) name in
  if not (path_present target) then fail ~path "snapshot blob is absent";
  let bytes = load_bytes ~path target in
  if Content_ref.matches ref bytes then bytes
  else fail ~path "snapshot blob does not match its reference"

(* Manifest codec. *)

let decode_error message = Jsont.Error.msg Jsont.Meta.none message

let rel_jsont =
  Jsont.map ~kind:"snapshot path"
    ~dec:(fun s ->
      match Rel.of_string s with
      | Ok rel -> rel
      | Error e -> decode_error (Lpath.Error.message e))
    ~enc:Rel.to_string Jsont.string

type manifest = (Rel.t * Content_ref.t) list

let entry_jsont =
  Jsont.Object.map ~kind:"snapshot entry" (fun path content -> (path, content))
  |> Jsont.Object.mem "path" rel_jsont ~enc:(fun (path, _) -> path)
  |> Jsont.Object.mem "content" Content_ref.jsont ~enc:(fun (_, content) ->
      content)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let manifest_jsont : manifest Jsont.t =
  Jsont.Object.map ~kind:"snapshot manifest" (fun version entries ->
      if version <> 1 then decode_error "unsupported snapshot manifest version"
      else entries)
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun _ -> 1)
  |> Jsont.Object.mem "entries" (Jsont.list entry_jsont) ~enc:Fun.id
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let load_manifest store reference =
  let ref =
    match Content_ref.of_token reference with
    | Ok ref -> ref
    | Error e -> fail ~path:store.Store.base (Mentat_digest.Error.message e)
  in
  let bytes = read_blob store ref in
  match Jsont_bytesrw.decode_string manifest_jsont bytes with
  | Ok manifest -> manifest
  | Error message ->
      fail
        ~path:(store.Store.base ^ "/" ^ reference)
        ("manifest decode: " ^ message)

(* Capturing. *)

type loaded = Loaded of string | Skip | Excluded
type t = { reference : string; excluded : int }

let capture store ~paths ~load =
  guard ~label:store.Store.base @@ fun () ->
  let excluded = ref 0 in
  let entries =
    List.filter_map
      (fun rel ->
        match load rel with
        | Skip -> None
        | Excluded ->
            incr excluded;
            None
        | Loaded bytes ->
            let ref = Content_ref.of_contents bytes in
            write_blob store ref bytes;
            Some (rel, ref))
      paths
  in
  let entries = List.sort (fun (a, _) (b, _) -> Rel.compare a b) entries in
  let manifest_bytes =
    match Jsont_bytesrw.encode_string manifest_jsont entries with
    | Ok bytes -> bytes
    | Error message ->
        fail ~path:store.Store.base ("manifest encode: " ^ message)
  in
  let manifest_ref = Content_ref.of_contents manifest_bytes in
  write_blob store manifest_ref manifest_bytes;
  { reference = Content_ref.to_token manifest_ref; excluded = !excluded }

(* Reading a capture. *)

type observed = File of string | Absent

let read store ~reference path =
  guard ~label:store.Store.base @@ fun () ->
  let manifest = load_manifest store reference in
  match List.find_opt (fun (p, _) -> Rel.equal p path) manifest with
  | None -> Absent
  | Some (_, ref) -> File (read_blob store ref)
