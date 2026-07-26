(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
module N = Nofollow

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stats -> (
      match stats.Unix.st_kind with
      | Unix.S_DIR ->
          Array.iter
            (fun name ->
              if (not (String.equal name ".")) && not (String.equal name "..")
              then rm_rf (Filename.concat path name))
            (Sys.readdir path);
          Unix.rmdir path
      | _ -> Unix.unlink path)

let with_temp_dir f =
  let dir = Filename.temp_file "nofollow-test-" "" in
  Unix.unlink dir;
  Unix.mkdir dir 0o755;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

(* Every test anchors a fresh root directory descriptor and closes it after. *)
let with_root f =
  with_temp_dir (fun dir ->
      let root = N.open_dir dir in
      Fun.protect
        ~finally:(fun () -> try Unix.close root with _ -> ())
        (fun () -> f ~dir ~root))

let write_file path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let kind_pp ppf = function
  | `Regular -> Format.pp_print_string ppf "Regular"
  | `Directory -> Format.pp_print_string ppf "Directory"
  | `Symlink -> Format.pp_print_string ppf "Symlink"
  | `Other -> Format.pp_print_string ppf "Other"

let kind_t = testable ~pp:kind_pp ~equal:( = ) ()

let expect_unix_error expected msg f =
  match f () with
  | _ -> failf "%s: expected Unix_error %s" msg (Unix.error_message expected)
  | exception Unix.Unix_error (got, _, _) when got = expected -> ()
  | exception Unix.Unix_error (got, _, _) ->
      failf "%s: expected %s, got %s" msg
        (Unix.error_message expected)
        (Unix.error_message got)

(* Refusing a symlink at a directory-opening step surfaces as ELOOP on platforms
   that report the [O_NOFOLLOW] refusal, and as ENOTDIR on those whose
   [O_DIRECTORY|O_NOFOLLOW] open notices the non-directory first — the very
   ambiguity {!Nofollow.fstatat} disambiguates. Either way the symlink is not
   traversed. *)
let expect_refused_symlink msg f =
  match f () with
  | _ -> failf "%s: expected the symlink to be refused" msg
  | exception Unix.Unix_error ((Unix.ELOOP | Unix.ENOTDIR), _, _) -> ()
  | exception Unix.Unix_error (got, _, _) ->
      failf "%s: expected ELOOP or ENOTDIR, got %s" msg (Unix.error_message got)

(* A traversal step onto a symlink is refused with ELOOP — the every-component
   O_NOFOLLOW guarantee. Both the directory-opening and read-opening steps. *)
let openat_refuses_symlink () =
  with_root @@ fun ~dir ~root ->
  Unix.mkdir (Filename.concat dir "d") 0o755;
  Unix.symlink "d" (Filename.concat dir "dlink");
  write_file (Filename.concat dir "f") "contents";
  Unix.symlink "f" (Filename.concat dir "flink");
  expect_refused_symlink "openat_dir onto a symlink" (fun () ->
      N.openat_dir root "dlink");
  expect_unix_error Unix.ELOOP "openat_read onto a symlink" (fun () ->
      N.openat_read root "flink");
  (* The real directory and file still open. *)
  Unix.close (N.openat_dir root "d");
  Unix.close (N.openat_read root "f")

let openat_create_is_exclusive () =
  with_root @@ fun ~dir:_ ~root ->
  Unix.close (N.openat_create root "new" 0o644);
  expect_unix_error Unix.EEXIST "openat_create over an existing entry"
    (fun () -> N.openat_create root "new" 0o644)

let mkdirat_is_exclusive () =
  with_root @@ fun ~dir ~root ->
  N.mkdirat root "sub" 0o755;
  expect_unix_error Unix.EEXIST "mkdirat over an existing directory" (fun () ->
      N.mkdirat root "sub" 0o755);
  Unix.symlink "sub" (Filename.concat dir "sublink");
  expect_unix_error Unix.EEXIST "mkdirat over a symlink" (fun () ->
      N.mkdirat root "sublink" 0o755)

let unlinkat_removes_files_and_dirs () =
  with_root @@ fun ~dir ~root ->
  write_file (Filename.concat dir "f") "x";
  N.unlinkat root "f" ~dir:false;
  is_true ~msg:"the file is gone"
    (not (Sys.file_exists (Filename.concat dir "f")));
  Unix.mkdir (Filename.concat dir "d") 0o755;
  N.unlinkat root "d" ~dir:true;
  is_true ~msg:"the directory is gone"
    (not (Sys.file_exists (Filename.concat dir "d")))

let linkat_does_not_replace () =
  with_root @@ fun ~dir ~root ->
  write_file (Filename.concat dir "src") "payload";
  N.linkat root "src" "dst";
  is_true ~msg:"the link is published"
    (Sys.file_exists (Filename.concat dir "dst"));
  expect_unix_error Unix.EEXIST "linkat over an existing entry" (fun () ->
      N.linkat root "src" "dst")

let renameat_moves_within_dir () =
  with_root @@ fun ~dir ~root ->
  write_file (Filename.concat dir "a") "x";
  N.renameat root "a" "b";
  is_true ~msg:"the source is gone"
    (not (Sys.file_exists (Filename.concat dir "a")));
  is_true ~msg:"the destination exists"
    (Sys.file_exists (Filename.concat dir "b"))

(* fstatat never follows a final symlink, so it reports the entry's own kind —
   the disambiguator for platforms whose no-follow open reports a symlink as
   ENOTDIR rather than ELOOP. *)
let fstatat_reports_entry_kind () =
  with_root @@ fun ~dir ~root ->
  write_file (Filename.concat dir "f") "0123456789";
  Unix.mkdir (Filename.concat dir "d") 0o755;
  Unix.symlink "f" (Filename.concat dir "flink");
  let kind name = (N.fstatat root name).N.kind in
  equal kind_t ~msg:"a regular file" `Regular (kind "f");
  equal kind_t ~msg:"a directory" `Directory (kind "d");
  equal kind_t ~msg:"a symlink is not followed" `Symlink (kind "flink");
  equal int ~msg:"size is the file's byte length" 10
    (Int64.to_int (N.fstatat root "f").N.size)

let () =
  run "nofollow"
    [
      group "traversal"
        [
          test "a symlink step is refused with ELOOP" openat_refuses_symlink;
          test "fstatat reports the entry's own kind" fstatat_reports_entry_kind;
        ];
      group "creation"
        [
          test "openat_create is exclusive" openat_create_is_exclusive;
          test "mkdirat is exclusive" mkdirat_is_exclusive;
          test "linkat does not replace" linkat_does_not_replace;
        ];
      group "mutation"
        [
          test "unlinkat removes files and directories"
            unlinkat_removes_files_and_dirs;
          test "renameat moves within a directory" renameat_moves_within_dir;
        ];
    ]
