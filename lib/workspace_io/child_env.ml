(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = { bindings : string array; path_dirs : string list }

let derived_names = [ "HOME"; "TEMP"; "TMP"; "TMPDIR" ]

let fixed_bindings =
  [
    ("CLICOLOR", "0");
    ("CLICOLOR_FORCE", "0");
    ("GIT_PAGER", "cat");
    ("LESS", "-FRX");
    ("NO_COLOR", "1");
    ("PAGER", "cat");
    ("TERM", "dumb");
  ]

let inherited_names =
  [
    "LANG";
    "LANGUAGE";
    "LC_ALL";
    "LC_COLLATE";
    "LC_CTYPE";
    "LC_MESSAGES";
    "LC_MONETARY";
    "LC_NUMERIC";
    "LC_TIME";
  ]

let single_toolchain_paths =
  [
    "DUNE_OCAML_STDLIB"; "OCAMLLIB"; "OCAML_TOPLEVEL_PATH"; "OPAM_SWITCH_PREFIX";
  ]

let toolchain_path_lists = [ "CAML_LD_LIBRARY_PATH"; "OCAMLPATH" ]

(* Normalization drops what cannot be represented rather than failing:
   construction is total, and a bad ambient segment costs only itself. *)
let normalize_path_list value =
  let rec loop seen normalized = function
    | [] -> List.rev normalized
    | segment :: rest -> (
        match Lpath.Abs.of_string segment with
        | Error _ -> loop seen normalized rest
        | Ok path ->
            let spelling = Lpath.Abs.to_string path in
            if List.mem spelling seen then loop seen normalized rest
            else loop (spelling :: seen) (spelling :: normalized) rest)
  in
  loop [] [] (String.split_on_char ':' value)

let normalize_single_path value =
  match Lpath.Abs.of_string value with
  | Ok path -> Some (Lpath.Abs.to_string path)
  | Error _ -> None

let add_inherited ~normalize lookup names bindings =
  List.fold_left
    (fun bindings name ->
      match lookup name with
      | None -> bindings
      | Some value when String.contains value '\000' -> bindings
      | Some value -> (
          match normalize value with
          | None -> bindings
          | Some value -> (name, value) :: bindings))
    bindings names

let make ~path ~scratch ~carried_dirs ~lookup =
  let path_dirs = normalize_path_list path in
  let scratch_value = Lpath.Abs.to_string scratch in
  (* The redirected [HOME] would send opam and dune to empty scratch locations
     for their root and shared cache; the resolver-recovered real directories
     are exported so each tool resolves the same location it would from a login
     shell, HOME still confined. *)
  let carried_bindings =
    List.map (fun (var, dir) -> (var, Lpath.Abs.to_string dir)) carried_dirs
  in
  let bindings =
    (("PATH", String.concat ":" path_dirs) :: fixed_bindings)
    @ List.map (fun name -> (name, scratch_value)) derived_names
    @ carried_bindings
  in
  let bindings =
    add_inherited ~normalize:Option.some lookup inherited_names bindings
  in
  let bindings =
    add_inherited ~normalize:normalize_single_path lookup single_toolchain_paths
      bindings
  in
  let bindings =
    add_inherited
      ~normalize:(fun value ->
        match normalize_path_list value with
        | [] -> None
        | dirs -> Some (String.concat ":" dirs))
      lookup toolchain_path_lists bindings
  in
  let bindings =
    List.sort (fun (a, _) (b, _) -> String.compare a b) bindings
    |> List.map (fun (name, value) -> name ^ "=" ^ value)
    |> Array.of_list
  in
  { bindings; path_dirs }
