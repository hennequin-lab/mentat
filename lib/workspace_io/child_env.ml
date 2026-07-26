(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = { bindings : string array; path_dirs : string list }

(* [HOME] is inherited. Rewriting it was the only mechanism in the system that
   could point a child at a directory the policy had not authorized: the child
   resolved its toolchain state under a scratch, so every real location had to
   be handed back one variable at a time, and a directory handed back without
   the matching grant is a tool sent somewhere it cannot work. The child and the
   resolver now read the same [$HOME], so they cannot disagree.

   The temp family still resolves to the private scratch. That is not the same
   decision deferred — it is the one the escalation stance is currently coupled
   to, since a seal infers [Available] from a non-empty writable set
   ([Seal.confined]), so granting a read-only route a temp root would flip its
   no-mutation promise as a side effect. Until that stance is stated rather than
   inferred, the temp redirect stays. *)
let derived_names = [ "TEMP"; "TMP"; "TMPDIR" ]

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
    "HOME";
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

let make ~path ~scratch ~lookup =
  let path_dirs = normalize_path_list path in
  let scratch_value = Lpath.Abs.to_string scratch in
  let bindings =
    (("PATH", String.concat ":" path_dirs) :: fixed_bindings)
    @ List.map (fun name -> (name, scratch_value)) derived_names
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
