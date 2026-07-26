(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type kind = Regular | Directory | Other
type observation = Missing | Found of kind | Escapes | Failed of string

let fs_path ~fs abs = Eio.Path.( / ) fs (Lpath.Abs.to_string abs)

let kind_of_eio = function
  | `Regular_file -> Regular
  | `Directory -> Directory
  | `Unknown | `Fifo | `Character_special | `Block_device | `Symbolic_link
  | `Socket ->
      Other

(* [realpath abs] canonicalizes [abs] through the platform realpath, resolving
   every symbolic link, or fails with a message. The target must exist. *)
let realpath abs =
  match Unix.realpath (Lpath.Abs.to_string abs) with
  | resolved -> (
      match Lpath.Abs.of_string resolved with
      | Ok abs -> Ok abs
      | Error error -> Error (Lpath.Error.message error))
  | exception Unix.Unix_error (error, _, _) -> Error (Unix.error_message error)
  | exception exn -> Error (Printexc.to_string exn)

let contained ~root path =
  match realpath root with
  | Error message -> Error message
  | Ok root -> (
      match realpath path with
      | Error message -> Error message
      | Ok target -> Ok (Lpath.Abs.is_within ~root target))

let is_not_directory_error exn =
  String.includes ~affix:"Not a directory" (Format.asprintf "%a" Eio.Exn.pp exn)

let observe ~fs ~root path =
  match Eio.Path.stat ~follow:true (fs_path ~fs path) with
  | stat -> (
      match contained ~root path with
      | Error message -> Failed message
      | Ok false -> Escapes
      | Ok true -> Found (kind_of_eio stat.Eio.File.Stat.kind))
  | exception Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Missing
  | exception exn when is_not_directory_error exn -> Missing
  | exception exn -> Failed (Format.asprintf "%a" Eio.Exn.pp exn)

(* A memory guard well above any legitimate instruction, skill, or resource
   file, and distinct from the far smaller display budgets callers apply to what
   they keep. It exists so a project-owned file — which may be large or
   adversarial — cannot be loaded whole and exhaust memory. *)
let read_limit = 4 * 1024 * 1024

let read_file ~fs ~max_bytes path =
  match
    Eio.Path.with_open_in (fs_path ~fs path) (fun flow ->
        Eio.Buf_read.(take_all (of_flow ~max_size:max_bytes flow)))
  with
  | contents -> Ok contents
  | exception Eio.Buf_read.Buffer_limit_exceeded ->
      Error
        (Printf.sprintf "%s exceeds the %d-byte read limit"
           (Lpath.Abs.to_string path) max_bytes)
  | exception exn -> Error (Format.asprintf "%a" Eio.Exn.pp exn)

let read_dir_names ~fs dir =
  match Eio.Path.read_dir (fs_path ~fs dir) with
  | names -> Ok names
  | exception exn -> Error (Format.asprintf "%a" Eio.Exn.pp exn)

let entry_kind ~fs path =
  match Eio.Path.stat ~follow:false (fs_path ~fs path) with
  | stat -> Some (kind_of_eio stat.Eio.File.Stat.kind)
  | exception Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  | exception exn when is_not_directory_error exn -> None
  | exception _ -> None

let is_file_or_directory ~fs path =
  let path = fs_path ~fs path in
  Eio.Path.is_file path || Eio.Path.is_directory path
