(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Error = struct
  type t =
    | Empty
    | Relative
    | Absolute
    | Escapes_root
    | Malformed_component of string

  let message = function
    | Empty -> "path must not be empty"
    | Relative -> "path must be absolute"
    | Absolute -> "path must be relative"
    | Escapes_root -> "path escapes root"
    | Malformed_component c -> Printf.sprintf "malformed path component %S" c

  let equal a b =
    match (a, b) with
    | Empty, Empty
    | Relative, Relative
    | Absolute, Absolute
    | Escapes_root, Escapes_root ->
        true
    | Malformed_component a, Malformed_component b -> String.equal a b
    | (Empty | Relative | Absolute | Escapes_root | Malformed_component _), _ ->
        false

  let pp ppf error = Format.pp_print_string ppf (message error)
end

let error error = Error error

(* A leading ASCII letter immediately followed by [:] — a Windows drive prefix
   such as ["C:"]. *)
let has_windows_drive_prefix path =
  String.length path >= 2
  && Char.Ascii.is_letter path.[0]
  && Char.equal path.[1] ':'

let starts_with_slash path =
  (not (String.is_empty path)) && Char.equal path.[0] '/'

(* Absolute-looking syntax: a leading slash, a backslash root, or a drive
   prefix. None is an accepted absolute path, but each is absolute-looking and
   must not be reinterpreted as a relative path either. *)
let is_relative_absolute_syntax path =
  starts_with_slash path
  || ((not (String.is_empty path)) && Char.equal path.[0] '\\')
  || has_windows_drive_prefix path

(* Component grammar. NUL, [/], and backslash cannot occur in a component; [.],
   [..], the empty string, and an ASCII-letter drive prefix are structural forms,
   not names. Every other byte is a valid component byte. *)
let is_component_char = function '\000' | '/' | '\\' -> false | _ -> true

let malformed_component component =
  String.is_empty component || String.equal component "."
  || String.equal component ".."
  || has_windows_drive_prefix component
  || not (String.for_all is_component_char component)

let checked_component component =
  if malformed_component component then
    error (Error.Malformed_component component)
  else Ok component

let split_components = String.split_all ~sep:"/" ~drop:String.is_empty
let join_components = String.concat "/"

(* [a] and [b] joined by a single separator into a freshly allocated, fully
   initialized string. *)
let append_with_slash a b =
  let len_a = String.length a in
  let len_b = String.length b in
  let bytes = Bytes.create (len_a + 1 + len_b) in
  Bytes.blit_string a 0 bytes 0 len_a;
  Bytes.set bytes len_a '/';
  Bytes.blit_string b 0 bytes (len_a + 1) len_b;
  Bytes.unsafe_to_string bytes

(* How [..] behaves against an empty component stack: a relative path escapes its
   root and is rejected; an absolute path clamps and stays at [/]. *)
type underflow = Escape | Clamp

(* [t] as the suffix below [prefix] when [prefix] is a strict, component-aligned
   ancestor; [None] otherwise. The [t.[n] = '/'] guard makes containment a
   component-boundary test, not a string-prefix test, so ["src-lib/a"] is not
   below ["src"]. The suffix is a canonical relative-path string. *)
let below_by_component ~prefix t =
  let n = String.length prefix in
  if String.length t > n && Char.equal t.[n] '/' && String.starts_with ~prefix t
  then Some (String.drop_first (n + 1) t)
  else None

(* The two kinds share every operation whose behaviour does not depend on the
   root's rendering. A kind supplies only its genuine asymmetries: the root
   spelling, the [..]-at-root law, and how a validated component stack renders
   (the leading slash). *)
module type Kind = sig
  val root : string
  val underflow : underflow

  val build : string list -> string
  (** Renders a reversed stack of validated components to a canonical string. *)
end

module Common (K : Kind) = struct
  type t = string

  let root = K.root
  let is_root t = String.equal t root
  let to_string t = t
  let components t = if is_root t then [] else split_components t

  (* Appends one already-valid component. Below the root the component stands
     alone; [build] supplies the root's rendering. *)
  let snoc t component =
    if is_root t then K.build [ component ] else append_with_slash t component

  let add_component t component =
    match checked_component component with
    | Error _ as error -> error
    | Ok component -> Ok (snoc t component)

  (* A non-root value carries at most one leading separator ([Some 0], only for
     [Abs]) and, for a single relative component, none at all ([None], only for
     [Rel]); both drop back to the root. *)
  let parent t =
    if is_root t then None
    else
      match String.rindex_opt t '/' with
      | None | Some 0 -> Some root
      | Some index -> Some (String.sub t 0 index)

  (* Resolves [input] onto [stack], a reversed list of validated components,
     applying the lexical operators [.] and [..]. Surviving components are
     validated once and pushed, then rendered with one join. Rendering directly
     in the terminal case avoids allocating an intermediate [Ok stack]. *)
  let rec resolve_onto stack = function
    | [] -> Ok (K.build stack)
    | "." :: rest -> resolve_onto stack rest
    | ".." :: rest -> (
        match stack with
        | _ :: stack -> resolve_onto stack rest
        | [] -> (
            match K.underflow with
            | Clamp -> resolve_onto [] rest
            | Escape -> error Error.Escapes_root))
    | component :: rest -> (
        match checked_component component with
        | Error _ as error -> error
        | Ok component -> resolve_onto (component :: stack) rest)

  (* Resolving below a non-root base edits its already-canonical string
     incrementally. Splitting the base into components and joining it again is
     simpler, but makes the common non-root resolve path allocate in proportion
     to the base as well as the input. Root-based parsing still uses
     [resolve_onto] so multi-component input is joined once. *)
  let rec resolve_from t = function
    | [] -> Ok t
    | "." :: rest -> resolve_from t rest
    | ".." :: rest -> (
        match parent t with
        | Some parent -> resolve_from parent rest
        | None -> (
            match K.underflow with
            | Clamp -> resolve_from root rest
            | Escape -> error Error.Escapes_root))
    | component :: rest -> (
        match checked_component component with
        | Error _ as error -> error
        | Ok component -> resolve_from (snoc t component) rest)

  (* Parses [s] as relative syntax below [base]: absolute-looking input is
     rejected, and [resolve_onto] threads the kind's [..]-at-root law, so a [..]
     that over-pops escapes for [Rel] and clamps for [Abs]. *)
  let resolve base s =
    if String.is_empty s then error Error.Empty
    else if is_relative_absolute_syntax s then error Error.Absolute
    else
      let input = split_components s in
      if is_root base then resolve_onto [] input else resolve_from base input

  let basename t =
    if is_root t then None
    else
      match String.rindex_opt t '/' with
      | None -> Some t
      | Some index -> Some (String.drop_first (index + 1) t)

  let equal = String.equal
  let compare = String.compare
  let hash = String.hash

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = String.compare
  end)

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = String.compare
  end)

  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

module Rel = struct
  include Common (struct
    let root = "."
    let underflow = Escape

    let build stack =
      match List.rev stack with
      | [] -> "."
      | components -> join_components components
  end)

  let of_string s =
    if String.is_empty s then error Error.Empty
    else if is_relative_absolute_syntax s then error Error.Absolute
    else resolve_onto [] (split_components s)

  let of_string_exn s =
    match of_string s with
    | Ok t -> t
    | Error error ->
        invalid_arg
          (Format.asprintf "Lpath.Rel.of_string_exn %S: %a" s Error.pp error)

  let is_component component = not (malformed_component component)

  let append a b =
    if is_root a then b else if is_root b then a else append_with_slash a b

  let relativize ~root:prefix t =
    if is_root prefix then Some t
    else if String.equal t prefix then Some root
    else below_by_component ~prefix t

  let is_within ~root p = Option.is_some (relativize ~root p)
  let is_strictly_within ~root p = is_within ~root p && not (equal root p)
end

module Abs = struct
  include Common (struct
    let root = "/"
    let underflow = Clamp

    let build stack =
      match List.rev stack with [] -> "/" | cs -> "/" ^ join_components cs
  end)

  let of_string s =
    if String.is_empty s then error Error.Empty
    else if not (starts_with_slash s) then error Error.Relative
    else resolve_onto [] (split_components s)

  let of_string_exn s =
    match of_string s with
    | Ok t -> t
    | Error error ->
        invalid_arg
          (Format.asprintf "Lpath.Abs.of_string_exn %S: %a" s Error.pp error)

  let append_rel t r =
    if Rel.is_root r then t
    else if is_root t then root ^ Rel.to_string r
    else append_with_slash t (Rel.to_string r)

  let resolve_any ~base s =
    if starts_with_slash s then of_string s else resolve base s

  (* Yields a relative suffix for either kind; [/]-rooted [t] loses its leading
     separator, the relative root has no rendering to strip. *)
  let relativize ~root:prefix t =
    if String.equal t prefix then Some Rel.root
    else if is_root prefix then Some (String.drop_first 1 t)
    else below_by_component ~prefix t

  let is_within ~root p = Option.is_some (relativize ~root p)
  let is_strictly_within ~root p = is_within ~root p && not (equal root p)
end
