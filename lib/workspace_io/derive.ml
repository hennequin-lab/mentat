(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let log_src =
  Logs.Src.create "mentat.workspace_io.derive"
    ~doc:"Workspace read/write scope derivation"

module Log = (val Logs.src_log log_src : Logs.LOG)

type access = Read | Read_subpaths of string list | Read_write of string list

type derived = {
  workspace_roots : (Mentat_workspace.Root.t * Lpath.Abs.t) list;
  writable : Lpath.Abs.t list;
  platform_writable : Lpath.Abs.t list;
  readable : Lpath.Abs.t list;
  protected : Lpath.Abs.t list;
  path : string;
  carried_dirs : (string * Lpath.Abs.t * access) list;
  describe : (string * Lpath.Abs.t) list;
}

let invalid ~spelling reason = Resolve_error.Invalid_root { spelling; reason }

let unix_reason = function
  | Unix.ENOENT -> Resolve_error.Does_not_exist
  | Unix.ENOTDIR -> Resolve_error.Not_a_directory
  | _ -> Resolve_error.Not_accessible

(* Canonicalize where the path exists so the policy describes what the
   backend enforces (macOS /tmp is a symlink to /private/tmp). A path that
   cannot be resolved keeps its lexical spelling. *)
let canonical path =
  match Unix.realpath (Lpath.Abs.to_string path) with
  | real -> (
      match Lpath.Abs.of_string real with Ok real -> real | Error _ -> path)
  | exception Unix.Unix_error _ -> path

let is_linux () =
  String.equal Sys.os_type "Unix" && Sys.file_exists "/proc/sys/kernel/ostype"

let is_executable_file path =
  match Unix.stat path with
  | { Unix.st_kind = Unix.S_REG; _ } -> (
      match Unix.access path [ Unix.X_OK ] with
      | () -> true
      | exception Unix.Unix_error _ -> false)
  | _ -> false
  | exception Unix.Unix_error _ -> false

(* Expand a leading [~] only against [$HOME]. User-name expansion and relative
   roots are intentionally not ambient shell behavior. *)
let abs_of_config_path ~lookup spelling =
  let expanded =
    if String.equal spelling "~" then lookup "HOME"
    else if
      String.length spelling >= 2 && String.equal (String.sub spelling 0 2) "~/"
    then
      match lookup "HOME" with
      | Some home ->
          Some (home ^ String.sub spelling 1 (String.length spelling - 1))
      | None -> None
    else Some spelling
  in
  match expanded with
  | None -> Error (invalid ~spelling Resolve_error.Not_accessible)
  | Some path ->
      Lpath.Abs.of_string path
      |> Result.map_error (fun _ ->
          invalid ~spelling Resolve_error.Not_accessible)

(* Admit a root only for an existing entry of the right kind, and describe it
   by its physical path so the policy names what the backend binds. *)
let physical_root ~spelling ~directory path =
  let path_string = Lpath.Abs.to_string path in
  match Unix.stat path_string with
  | stats
    when stats.Unix.st_kind = Unix.S_DIR
         || ((not directory) && stats.Unix.st_kind = Unix.S_REG) -> (
      match Unix.realpath path_string |> Lpath.Abs.of_string with
      | Ok path -> Ok path
      | Error _ -> Error (invalid ~spelling Resolve_error.Not_accessible))
  | _ ->
      let reason =
        if directory then Resolve_error.Not_a_directory
        else Resolve_error.Not_a_directory_or_file
      in
      Error (invalid ~spelling reason)
  | exception Unix.Unix_error (error, _, _) ->
      Error (invalid ~spelling (unix_reason error))

let account_home () =
  match (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir |> Lpath.Abs.of_string with
  | Ok home -> Some (canonical home)
  | Error _ -> None
  | exception Not_found -> None

let broad_root ~lookup ~workspace_roots path =
  let root = Lpath.Abs.of_string_exn "/" in
  Lpath.Abs.equal path root
  || List.exists (Lpath.Abs.is_strictly_within ~root:path) workspace_roots
  ||
  let broad_account_home =
    match account_home () with
    | None -> false
    | Some home -> Lpath.Abs.is_within ~root:path home
  in
  let configured_home =
    match lookup "HOME" with
    | None -> false
    | Some home -> (
        match Lpath.Abs.of_string home with
        | Error _ -> false
        | Ok home -> Lpath.Abs.equal path (canonical home))
  in
  broad_account_home || configured_home

let user_root ~field ~directory ~lookup ~workspace_roots spelling =
  let* path = abs_of_config_path ~lookup spelling in
  let* path = physical_root ~spelling ~directory path in
  if broad_root ~lookup ~workspace_roots path then
    Error (Resolve_error.Broad_root { field; spelling })
  else Ok path

let user_roots ~field ~directory ~lookup ~workspace_roots spellings =
  let rec loop roots = function
    | [] -> Ok (List.rev roots)
    | spelling :: rest ->
        let* root =
          user_root ~field ~directory ~lookup ~workspace_roots spelling
        in
        loop (root :: roots) rest
  in
  loop [] spellings

let canonical_paths paths = List.sort_uniq Lpath.Abs.compare paths

(* Collapse redundant descendants: keep only paths not strictly within
   another kept path. *)
let root_paths paths =
  let paths = canonical_paths paths in
  List.filter
    (fun path ->
      not
        (List.exists
           (fun candidate -> Lpath.Abs.is_strictly_within ~root:candidate path)
           paths))
    paths

let unique_paths paths =
  List.fold_left
    (fun unique path ->
      if List.exists (Lpath.Abs.equal path) unique then unique
      else path :: unique)
    [] paths
  |> List.rev

let existing_auto_root path =
  match Lpath.Abs.of_string path with
  | Error _ -> None
  | Ok path -> (
      match Unix.stat (Lpath.Abs.to_string path) with
      | { Unix.st_kind = Unix.S_DIR | Unix.S_REG; _ } -> Some (canonical path)
      | _ -> None
      | exception Unix.Unix_error _ -> None)

(* User-level directories that toolchain processes resolve relative to [$HOME]:
   opam's root and dune's XDG cache/config/state/data. The child environment
   redirects HOME to the private scratch, which alone would send each tool
   to an empty scratch location — opam reports an uninitialized root, and dune's
   shared build cache misses, forcing a from-scratch rebuild of the whole package
   closure (the compiler and every dependency). Each variable is recovered at its
   real value — the ambient value the launcher set, else the standard
   [$HOME]-derived default — and carried on the child environment so the tool
   resolves the same directory it would from a login shell, with HOME still
   confined to the scratch. Only an existing directory is recovered: no state is
   invented, and a scoped route admits each as a read root.

   Access is decided per variable against what the tool does with the directory
   and who else owns it, never defaulted: carrying all five read-only is what
   pointed a build at a cache it could not write, and carrying all five writable
   would hand the workspace-write posture directories no build ever touches.
   Only the cache is written by the tool the carry exists for — dune reaches
   [Xdg.cache_dir] for its shared build cache and for the revision store whose
   lock a pinned-source build must take — so only the cache is [Read_write]. Its
   own [db] and [toolchains] subdirectories are carved back out: a build cache
   entry is replayed into the user's unsandboxed builds and a downloaded
   toolchain is executed by them, so neither may be reachable through a grant
   taken for a lock file. The XDG state and data directories are not carried at
   all: no tool in the OCaml toolchain resolves them — dune reaches only
   [Xdg.cache_dir] and [Xdg.config_dir] — while they hold Mentat's own session
   store, snapshots, and logs, including the durable confinement identity a
   resume revalidates against. Left uncarried they fall back to the scratch, so
   a tool that does want them gets a private one.

   The read scope narrows the same way the write grant does. [XDG_CONFIG_HOME]
   names a base directory shared by every tool on the machine — credentials for
   version-control forges and cloud CLIs among them — while the only thing a
   build reads under it is dune's own configuration, so only that subdirectory
   is admitted. The variable still binds to the base directory, because that is
   what the tool resolves against; the generators grant ancestor metadata for
   each admitted root, so the descent still works. *)
let home_relative_dirs =
  [
    ("OPAMROOT", ".opam", Read);
    ( "XDG_CACHE_HOME",
      ".cache",
      Read_write
        [ Filename.concat "dune" "db"; Filename.concat "dune" "toolchains" ] );
    ("XDG_CONFIG_HOME", ".config", Read_subpaths [ "dune" ]);
  ]

let carried_user_dirs ~lookup ~workspace_roots =
  let home =
    match lookup "HOME" with
    | Some home when not (String.equal home "") -> Some home
    | _ -> Option.map Lpath.Abs.to_string (account_home ())
  in
  let existing_dir spelling =
    match Lpath.Abs.of_string spelling with
    | Error _ -> None
    | Ok path -> (
        match Unix.stat spelling with
        | { Unix.st_kind = Unix.S_DIR; _ } -> Some (canonical path)
        | _ -> None
        | exception Unix.Unix_error _ -> None)
  in
  (* Every sibling derivation refuses a root that resolves to [/] or [$HOME];
     a carried variable is ambient too, and an environment that sets one of them
     to the home directory would otherwise widen the posture silently. A [Read]
     entry only ever added a read root, so it is dropped with a warning like a
     stray toolchain value; a [Read_write] entry would widen the write grant to
     everything under it and fails the resolution closed. *)
  let rec loop carried = function
    | [] -> Ok (List.rev carried)
    | (var, default, access) :: rest -> (
        let spelling =
          match lookup var with
          | Some value when not (String.equal value "") -> Some value
          | _ -> Option.map (fun home -> Filename.concat home default) home
        in
        match Option.bind spelling existing_dir with
        | None -> loop carried rest
        | Some dir -> (
            if not (broad_root ~lookup ~workspace_roots dir) then
              loop ((var, dir, access) :: carried) rest
            else
              let spelling = Lpath.Abs.to_string dir in
              match access with
              | Read_write _ ->
                  Error (Resolve_error.Broad_root { field = var; spelling })
              | Read | Read_subpaths _ ->
                  Log.warn (fun m ->
                      m "ignoring carried directory %s=%S: broad root" var
                        spelling);
                  loop carried rest))
  in
  loop [] home_relative_dirs

let carried_subpath dir sub =
  existing_auto_root (Filename.concat (Lpath.Abs.to_string dir) sub)

(* The read roots a carried entry contributes, each still labelled by the
   variable that admitted it. A narrowed entry contributes its existing
   subdirectories instead of the base directory it binds. *)
let carried_read_roots carried =
  List.concat_map
    (fun (var, dir, access) ->
      match access with
      | Read | Read_write _ -> [ (var, dir) ]
      | Read_subpaths subs ->
          List.filter_map
            (fun sub ->
              Option.map (fun p -> (var, p)) (carried_subpath dir sub))
            subs)
    carried

(* Subpaths of a carried writable directory that the write grant must not
   reach. Existence-filtered like every other carveout, because a strict
   read-only bind over a missing source aborts the spawn — so a cache that has
   not yet grown one of them is not protected against its first creation. *)
let carried_carveouts carried =
  List.concat_map
    (fun (_, dir, access) ->
      match access with
      | Read | Read_subpaths _ -> []
      | Read_write excluded -> List.filter_map (carried_subpath dir) excluded)
    carried

let platform_roots ~lookup ~workspace_roots =
  let candidates =
    if is_linux () then
      [
        "/bin";
        "/sbin";
        "/usr";
        "/etc";
        "/lib";
        "/lib64";
        "/nix/store";
        "/run/current-system/sw";
      ]
    else
      [
        "/bin";
        "/sbin";
        "/usr/bin";
        "/usr/sbin";
        "/usr/lib";
        "/usr/libexec";
        "/usr/share";
        "/System/Library";
        "/System/iOSSupport/System/Library";
        "/Library/Apple/System/Library";
        "/Library/Apple/usr/lib";
        "/Library/Filesystems/NetFSPlugins";
        "/Library/Preferences";
        "/opt/homebrew";
        "/usr/local/Cellar";
        "/usr/local/opt";
        "/usr/local/lib";
        "/usr/local/share";
        "/private/etc";
        "/private/var/db";
        (* Apple's developer-tool shims (/usr/bin/git, cc, make) resolve
           through the xcode-select data link at /private/var/select into the
           active developer directory; without all three of these, every
           CLT-backed tool fails confined with "unable to read data link at
           /var/select/developer_dir". *)
        "/private/var/select";
        "/Library/Developer";
        "/Applications/Xcode.app";
      ]
  in
  let roots =
    List.concat_map
      (fun spelling ->
        match Lpath.Abs.of_string spelling with
        | Error _ -> []
        | Ok lexical -> (
            match Unix.stat spelling with
            | { Unix.st_kind = Unix.S_DIR | Unix.S_REG; _ } ->
                (* Profiles bind by pathname, so admit both spellings when the
                   lexical path is a symlink to its physical one. *)
                let physical = canonical lexical in
                if Lpath.Abs.equal lexical physical then [ lexical ]
                else [ lexical; physical ]
            | _ -> []
            | exception Unix.Unix_error _ -> []))
      candidates
  in
  match List.find_opt (broad_root ~lookup ~workspace_roots) roots with
  | None -> Ok roots
  | Some path ->
      Error
        (Resolve_error.Broad_root
           { field = "platform"; spelling = Lpath.Abs.to_string path })

let path_roots ~scoped ~lookup ~workspace_roots =
  let value = Option.value (lookup "PATH") ~default:"" in
  let segments = String.split_on_char ':' value in
  let rec loop roots = function
    | [] -> Ok (List.rev roots)
    | segment :: rest -> (
        match Lpath.Abs.of_string segment with
        | Error _ ->
            Error (invalid ~spelling:segment Resolve_error.Not_accessible)
        | Ok path -> (
            let spelling = Lpath.Abs.to_string path in
            match Unix.stat spelling with
            | { Unix.st_kind = Unix.S_DIR; _ } ->
                let path = canonical path in
                if scoped && broad_root ~lookup ~workspace_roots path then
                  Error (Resolve_error.Broad_root { field = "PATH"; spelling })
                else loop (path :: roots) rest
            | _ -> Error (invalid ~spelling Resolve_error.Not_a_directory)
            | exception Unix.Unix_error (Unix.ENOENT, _, _) -> loop roots rest
            | exception Unix.Unix_error (error, _, _) ->
                Error (invalid ~spelling (unix_reason error))))
  in
  loop [] segments

(* The executable environment resolves PATH and the toolchain variables the
   way the OCaml toolchain ladder would for a spawned build tool, so the sandbox
   admits the directories dune-family tools actually run from — and, through
   {!Mentat_ocaml_toolchain.toolchain_env}, the switch [bin] directory dune
   hands its child compiler, which a [dune] found on [PATH] would otherwise not
   carry. This same PATH becomes the child's PATH downstream, so bridging it
   here both admits the switch as a read root and puts the compiler on the
   child's search path. *)
let executable_env ~lookup ~project_root =
  let bindings =
    List.filter_map
      (fun name -> Option.map (fun value -> name ^ "=" ^ value) (lookup name))
      [ "PATH"; "OPAM_SWITCH_PREFIX"; "MENTAT_DUNE" ]
    |> Array.of_list
  in
  let toolchain =
    Mentat_ocaml_toolchain.discover ~env:bindings
      ~workspace_root:(Some (Lpath.Abs.to_string project_root))
  in
  let bindings = Mentat_ocaml_toolchain.toolchain_env toolchain in
  fun name ->
    Array.find_map
      (fun binding ->
        match String.index_opt binding '=' with
        | Some index when String.equal (String.sub binding 0 index) name ->
            Some
              (String.sub binding (index + 1)
                 (String.length binding - index - 1))
        | Some _ | None -> None)
      bindings
    |> Option.fold ~none:(lookup name) ~some:Option.some

(* An executable on [PATH] usually reads runtime data beside itself, so a
   [bin]/[sbin] entry also admits the sibling directories that hold it. Only
   those siblings: admitting the whole prefix would make the read scope a
   function of the launcher's [PATH], which is exactly the wrong thing to derive
   authority from — a per-user tool installed under [$HOME] would contribute its
   entire tree, so [~/.local/bin] on [PATH] admits [~/.local] and with it every
   application's data, Mentat's own session store included. The prefix of a
   [PATH] entry is where a tool happens to live, not a statement about what a
   build needs to read. *)
let executable_runtime_roots ~lookup ~scope_roots executable_roots =
  List.concat_map
    (fun executable_root ->
      match
        (Lpath.Abs.basename executable_root, Lpath.Abs.parent executable_root)
      with
      | Some ("bin" | "sbin"), Some prefix ->
          List.filter_map
            (fun component ->
              match Lpath.Abs.add_component prefix component with
              | Error _ -> None
              | Ok candidate -> (
                  match existing_auto_root (Lpath.Abs.to_string candidate) with
                  | Some root
                    when not
                           (broad_root ~lookup ~workspace_roots:scope_roots root)
                    ->
                      Some root
                  | Some _ | None -> None))
            [ "etc"; "lib"; "libexec"; "share" ]
      | _ -> [])
    executable_roots
  |> root_paths

let toolchain_roots ~lookup ~workspace_roots =
  let variables =
    [
      ("CAML_LD_LIBRARY_PATH", true);
      ("OCAMLPATH", true);
      ("OCAML_TOPLEVEL_PATH", false);
      ("OPAM_SWITCH_PREFIX", false);
      ("OCAMLLIB", false);
      ("DUNE_OCAML_STDLIB", false);
    ]
  in
  (* Ambient toolchain values recover the launcher's real toolchain layout with
     HOME confined to the scratch; they are best-effort, not user intent. A value
     that names no usable directory is dropped with a logged warning so the
     derived read scope only narrows and a stray artifact never bricks startup:
     [dune exec] leaks unexpanded placeholders ([OCAML_TOPLEVEL_PATH=%{toplevel}%]),
     PATH-like values carry empty segments, and a variable can point at a file or
     a since-removed directory. Two cases still fail closed: a value resolving to
     a broad root (/ or $HOME) would silently widen reads to everything, and
     configured roots ([sandbox.readable_roots]/[writable_roots], see [user_roots])
     are an explicit request whose garbage is a real config error. *)
  let rec add_values name roots = function
    | [] -> Ok roots
    | spelling :: rest -> (
        let skip reason =
          Log.warn (fun m ->
              m "ignoring toolchain root %s=%S: %s" name spelling reason);
          add_values name roots rest
        in
        match Lpath.Abs.of_string spelling with
        | Error _ -> skip "not an absolute path"
        | Ok path -> (
            match Unix.stat spelling with
            | { Unix.st_kind = Unix.S_DIR; _ } ->
                let path = canonical path in
                if broad_root ~lookup ~workspace_roots path then
                  Error
                    (Resolve_error.Broad_root
                       { field = name; spelling = Lpath.Abs.to_string path })
                else add_values name ((name, path) :: roots) rest
            | _ -> skip "not a directory"
            | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                add_values name roots rest
            | exception Unix.Unix_error (error, _, _) ->
                skip (Unix.error_message error)))
  in
  let rec loop roots = function
    | [] -> Ok roots
    | (name, list) :: rest ->
        let values =
          match lookup name with
          | None -> []
          | Some value when list -> String.split_on_char ':' value
          | Some value -> [ value ]
        in
        let* roots = add_values name roots values in
        loop roots rest
  in
  loop [] variables

let opam_bin_root ~lookup toolchain_roots =
  match
    List.find_opt
      (fun (name, _) -> String.equal name "OPAM_SWITCH_PREFIX")
      toolchain_roots
  with
  | Some (_, prefix) -> (
      match Lpath.Abs.add_component prefix "bin" with
      | Error _ -> None
      | Ok path -> existing_auto_root (Lpath.Abs.to_string path))
  | None -> (
      match lookup "OPAM_SWITCH_PREFIX" with
      | None -> None
      | Some prefix -> existing_auto_root (Filename.concat prefix "bin"))

let environment_path ~scoped ~path_roots ~toolchain_roots ~lookup =
  let admitted = Option.to_list (opam_bin_root ~lookup toolchain_roots) in
  let paths =
    if scoped then
      admitted @ path_roots |> unique_paths |> List.map Lpath.Abs.to_string
    else List.map Lpath.Abs.to_string admitted @ Option.to_list (lookup "PATH")
  in
  String.concat ":" paths

let existing_entry root name =
  match Lpath.Abs.add_component root name with
  | Error _ -> None
  | Ok path -> (
      match Unix.lstat (Lpath.Abs.to_string path) with
      | _ -> Some path
      | exception Unix.Unix_error _ -> None)

let protected_meta_paths root =
  List.filter_map (existing_entry root) Mentat_workspace.protected_meta_names

let read_metadata_path path =
  let spelling = Lpath.Abs.to_string path in
  match open_in_bin spelling with
  | input ->
      Fun.protect
        ~finally:(fun () -> close_in input)
        (fun () ->
          let length = in_channel_length input in
          if length > 4096 then
            Error (invalid ~spelling Resolve_error.Not_accessible)
          else
            let value = really_input_string input length |> String.trim in
            if
              String.equal value "" || String.contains value '\n'
              || String.contains value '\r'
            then Error (invalid ~spelling Resolve_error.Not_accessible)
            else Ok value)
  | exception Sys_error _ ->
      Error (invalid ~spelling Resolve_error.Not_accessible)

let resolve_metadata_dir ~lookup ~workspace_roots ~base spelling =
  let* path =
    Lpath.Abs.resolve_any ~base spelling
    |> Result.map_error (fun _ ->
        invalid ~spelling Resolve_error.Not_accessible)
  in
  let* path = physical_root ~spelling ~directory:true path in
  if broad_root ~lookup ~workspace_roots path then
    Error
      (Resolve_error.Broad_root
         { field = "project gitdir"; spelling = Lpath.Abs.to_string path })
  else Ok path

(* A linked git worktree keeps its metadata outside the project root; the
   [gitdir] and [commondir] targets must be readable and protected or git
   cannot run at all. *)
let linked_git_roots ~lookup ~scope_roots project_root =
  match Lpath.Abs.add_component project_root ".git" with
  | Error _ -> Ok []
  | Ok git -> (
      let spelling = Lpath.Abs.to_string git in
      match Unix.lstat spelling with
      | { Unix.st_kind = Unix.S_DIR; _ } -> Ok []
      | { Unix.st_kind = Unix.S_REG; _ } -> (
          let* line = read_metadata_path git in
          let prefix = "gitdir: " in
          if not (String.starts_with ~prefix line) then
            Error (invalid ~spelling Resolve_error.Not_accessible)
          else
            let target =
              String.sub line (String.length prefix)
                (String.length line - String.length prefix)
            in
            let* gitdir =
              resolve_metadata_dir ~lookup ~workspace_roots:scope_roots
                ~base:project_root target
            in
            match Lpath.Abs.add_component gitdir "commondir" with
            | Error _ -> Ok [ gitdir ]
            | Ok commondir -> (
                match Unix.lstat (Lpath.Abs.to_string commondir) with
                | { Unix.st_kind = Unix.S_REG; _ } ->
                    let* target = read_metadata_path commondir in
                    let* common =
                      resolve_metadata_dir ~lookup ~workspace_roots:scope_roots
                        ~base:gitdir target
                    in
                    Ok [ common; gitdir ]
                | _ -> Ok [ gitdir ]
                | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok [ gitdir ]
                | exception Unix.Unix_error (error, _, _) ->
                    Error
                      (invalid
                         ~spelling:(Lpath.Abs.to_string commondir)
                         (unix_reason error))))
      | _ -> Error (invalid ~spelling Resolve_error.Not_a_directory_or_file)
      | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok []
      | exception Unix.Unix_error (error, _, _) ->
          Error (invalid ~spelling (unix_reason error)))

let workspace_root ~scoped ~lookup root =
  let dir = Mentat_workspace.Root.dir root in
  let spelling = Lpath.Abs.to_string dir in
  match physical_root ~spelling ~directory:true dir with
  | Error _ -> Error (Resolve_error.Missing_root root)
  | Ok path ->
      if scoped && broad_root ~lookup ~workspace_roots:[] path then
        Error (Resolve_error.Broad_root { field = "workspace"; spelling })
      else Ok path

let workspace_roots ~scoped ~lookup logical =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | root :: rest ->
        let* path = workspace_root ~scoped ~lookup root in
        loop ((root, path) :: acc) rest
  in
  loop [] (Mentat_workspace.roots logical)

let roots_overlap a b =
  Lpath.Abs.is_within ~root:a b || Lpath.Abs.is_within ~root:b a

(* [/tmp] is where a command puts a scratch file when it does not consult the
   environment for one, and a literal [/tmp/...] path is common enough in build
   scripts and in model-authored commands that its absence reads as a broken
   sandbox rather than a confined one. It went unnoticed because the temp-dir
   family is redirected to the private scratch, so only a path spelled out in
   full ever reaches it. Admitted writable on both platforms, canonicalized
   because macOS resolves it to [/private/tmp]. The scratch may be minted
   beneath it, which is why the whole platform-writable set is exempt from the
   scratch-disjointness guard. *)
let shared_temp_dirs () = List.filter_map existing_auto_root [ "/tmp" ]

(* Apple's developer-tool shims (git, xcrun, clang) cache under the per-user
   confstr [DARWIN_USER_CACHE_DIR] — the [C] sibling of the [T] temp bucket —
   which no environment redirect can move; without it every CLT-backed tool
   fails confined ("couldn't create cache file"). The directory is created
   0700 by the system for this user alone, so admitting it as a writable root
   keeps the workspace-write posture: no other user's or project's state is
   reachable through it. Linux has no analogue. *)
let darwin_user_dirs ~lookup =
  if is_linux () then []
  else
    match lookup "TMPDIR" with
    | None -> []
    | Some spelling -> (
        match Lpath.Abs.of_string spelling with
        | Error _ -> []
        | Ok path ->
            let temp = canonical path in
            let temp_spelling = Lpath.Abs.to_string temp in
            if not (String.equal (Filename.basename temp_spelling) "T") then []
            else
              let existing_dir spelling =
                match Unix.stat spelling with
                | { Unix.st_kind = Unix.S_DIR; _ } ->
                    Some (canonical (Lpath.Abs.of_string_exn spelling))
                | _ -> None
                | exception Unix.Unix_error _ -> None
              in
              List.filter_map existing_dir
                [
                  temp_spelling;
                  Filename.concat (Filename.dirname temp_spelling) "C";
                ])

(* A configured writable root that overlaps an admitted read-only root would
   grant writes the admission forbids; refuse it as over-broad. *)
let reject_read_only_writes read_only configured =
  match
    List.find_opt
      (fun configured -> List.exists (roots_overlap configured) read_only)
      configured
  with
  | None -> Ok ()
  | Some path ->
      Error
        (Resolve_error.Broad_root
           {
             field = "sandbox.writable_roots";
             spelling = Lpath.Abs.to_string path;
           })

let run ~scoped ~lookup ~logical ~configured_reads ~configured_writes =
  let* roots = workspace_roots ~scoped ~lookup logical in
  let primary, read_only =
    match roots with
    | (_, primary) :: rest -> (primary, List.map snd rest)
    | [] -> assert false (* a workspace always admits a primary root *)
  in
  let scope_roots = List.map snd roots in
  let* configured_reads =
    if scoped then
      user_roots ~field:"sandbox.readable_roots" ~directory:false ~lookup
        ~workspace_roots:scope_roots configured_reads
    else Ok []
  in
  let* configured_writes =
    user_roots ~field:"sandbox.writable_roots" ~directory:true ~lookup
      ~workspace_roots:scope_roots configured_writes
  in
  let* () = reject_read_only_writes read_only configured_writes in
  (* The switch bridge runs on every route, scoped or not: a spawned [dune] needs
     the compiler on its [PATH] whether or not reads are project-scoped, and the
     child [PATH] below is built from this lookup. Only the read-root derivation
     (executable/toolchain/platform roots) is gated on [scoped]. *)
  let exec_lookup = executable_env ~lookup ~project_root:primary in
  let* executable_roots =
    if scoped then
      path_roots ~scoped:true ~lookup:exec_lookup ~workspace_roots:scope_roots
    else Ok []
  in
  let runtime_roots =
    if scoped then
      executable_runtime_roots ~lookup ~scope_roots executable_roots
    else []
  in
  let* toolchain =
    if scoped then toolchain_roots ~lookup ~workspace_roots:scope_roots
    else Ok []
  in
  let* platform =
    if scoped then platform_roots ~lookup ~workspace_roots:scope_roots
    else Ok []
  in
  let* git =
    if scoped then linked_git_roots ~lookup ~scope_roots primary else Ok []
  in
  let path =
    environment_path ~scoped ~path_roots:executable_roots
      ~toolchain_roots:toolchain ~lookup:exec_lookup
  in
  let* carried_dirs = carried_user_dirs ~lookup ~workspace_roots:scope_roots in
  let readable =
    if scoped then
      root_paths
        (scope_roots @ configured_reads @ platform @ executable_roots
       @ runtime_roots @ List.map snd toolchain @ git
        @ List.map snd (carried_read_roots carried_dirs))
    else []
  in
  let protected =
    protected_meta_paths primary
    @ read_only @ git
    @ carried_carveouts carried_dirs
    |> canonical_paths
  in
  let describe =
    if scoped then
      [ ("project", primary) ]
      @ List.map (fun p -> ("user-configured", p)) configured_reads
      @ List.map (fun p -> ("platform", p)) platform
      @ List.map (fun p -> ("executable:PATH", p)) executable_roots
      @ List.map (fun p -> ("executable:PATH runtime", p)) runtime_roots
      @ List.map (fun (name, p) -> ("toolchain:" ^ name, p)) toolchain
      @ List.map
          (fun (var, p) -> ("toolchain:" ^ var, p))
          (carried_read_roots carried_dirs)
      @ List.map (fun p -> ("git-worktree", p)) git
    else []
  in
  Ok
    {
      workspace_roots = roots;
      writable = primary :: configured_writes;
      platform_writable = shared_temp_dirs () @ darwin_user_dirs ~lookup;
      readable;
      protected;
      path;
      carried_dirs;
      describe;
    }

let carried_bindings derived =
  List.map (fun (var, dir, _) -> (var, dir)) derived.carried_dirs

let carried_writable derived =
  List.filter_map
    (fun (_, dir, access) ->
      match access with
      | Read | Read_subpaths _ -> None
      | Read_write _ -> Some dir)
    derived.carried_dirs
