(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* A press is a set of modifiers and a key. A chord is one or two presses. The
   modifiers are matched exactly against a decoded event: ctrl, alt, shift, and
   super must all agree, while meta, hyper, and the lock toggles are ignored
   (terminal Alt already sets meta, so alt alone identifies the gesture). *)
type press = {
  ctrl : bool;
  alt : bool;
  shift : bool;
  super : bool;
  key : Matrix.Input.Key.t;
}

let press_equal a b =
  Bool.equal a.ctrl b.ctrl && Bool.equal a.alt b.alt
  && Bool.equal a.shift b.shift && Bool.equal a.super b.super
  && Matrix.Input.Key.equal a.key b.key

let press_matches press (event : Matrix.Input.Key.event) =
  let m = event.Matrix.Input.Key.modifier in
  Bool.equal press.ctrl m.Matrix.Input.Modifier.ctrl
  && Bool.equal press.alt m.Matrix.Input.Modifier.alt
  && Bool.equal press.shift m.Matrix.Input.Modifier.shift
  && Bool.equal press.super m.Matrix.Input.Modifier.super
  && Matrix.Input.Key.equal press.key event.Matrix.Input.Key.key

(* Token vocabulary shared by the chord parser and pretty-printer. *)

let modifier_of_token = function
  | "ctrl" | "control" -> Some `Ctrl
  | "shift" -> Some `Shift
  | "alt" | "option" | "opt" -> Some `Alt
  | "cmd" | "super" | "meta" | "win" -> Some `Super
  | _ -> None

let single_uchar s =
  if String.equal s "" then None
  else
    let decode = String.get_utf_8_uchar s 0 in
    if
      Uchar.utf_decode_is_valid decode
      && Uchar.utf_decode_length decode = String.length s
    then Some (Uchar.utf_decode_uchar decode)
    else None

let named_key = function
  | "escape" | "esc" -> Some Matrix.Input.Key.Escape
  | "tab" -> Some Matrix.Input.Key.Tab
  | "enter" | "return" -> Some Matrix.Input.Key.Enter
  | "space" -> Some (Matrix.Input.Key.Char (Uchar.of_char ' '))
  | "backspace" -> Some Matrix.Input.Key.Backspace
  | "delete" | "del" -> Some Matrix.Input.Key.Delete
  | "up" -> Some Matrix.Input.Key.Up
  | "down" -> Some Matrix.Input.Key.Down
  | "left" -> Some Matrix.Input.Key.Left
  | "right" -> Some Matrix.Input.Key.Right
  | "home" -> Some Matrix.Input.Key.Home
  | "end" -> Some Matrix.Input.Key.End
  | "pageup" | "pgup" -> Some Matrix.Input.Key.Page_up
  | "pagedown" | "pgdn" -> Some Matrix.Input.Key.Page_down
  | "insert" | "ins" -> Some Matrix.Input.Key.Insert
  | token ->
      if String.length token >= 2 && Char.equal token.[0] 'f' then
        match
          int_of_string_opt (String.sub token 1 (String.length token - 1))
        with
        | Some n when n >= 1 && n <= 35 -> Some (Matrix.Input.Key.F n)
        | _ -> None
      else None

let key_of_token token =
  match named_key token with
  | Some key -> Some key
  | None -> Option.map (fun u -> Matrix.Input.Key.Char u) (single_uchar token)

let key_to_token key =
  match key with
  | Matrix.Input.Key.Escape -> "escape"
  | Matrix.Input.Key.Tab -> "tab"
  | Matrix.Input.Key.Enter -> "enter"
  | Matrix.Input.Key.Backspace -> "backspace"
  | Matrix.Input.Key.Delete -> "delete"
  | Matrix.Input.Key.Up -> "up"
  | Matrix.Input.Key.Down -> "down"
  | Matrix.Input.Key.Left -> "left"
  | Matrix.Input.Key.Right -> "right"
  | Matrix.Input.Key.Home -> "home"
  | Matrix.Input.Key.End -> "end"
  | Matrix.Input.Key.Page_up -> "pageup"
  | Matrix.Input.Key.Page_down -> "pagedown"
  | Matrix.Input.Key.Insert -> "insert"
  | Matrix.Input.Key.F n -> "f" ^ string_of_int n
  | Matrix.Input.Key.Char u when Uchar.equal u (Uchar.of_char ' ') -> "space"
  | Matrix.Input.Key.Char u ->
      let buffer = Buffer.create 4 in
      Buffer.add_utf_8_uchar buffer u;
      Buffer.contents buffer
  | _ -> "?"

type t = press list

let press_of_token token =
  match List.rev (String.split_on_char '+' token) with
  | [] -> Error (Printf.sprintf "empty key in %S" token)
  | key_token :: modifier_tokens -> (
      if String.equal key_token "" then
        Error (Printf.sprintf "empty key in %S" token)
      else
        match key_of_token key_token with
        | None -> Error (Printf.sprintf "unknown key %S" key_token)
        | Some key ->
            let base =
              { ctrl = false; alt = false; shift = false; super = false; key }
            in
            let rec collect press = function
              | [] -> Ok press
              | modifier :: rest -> (
                  match modifier_of_token modifier with
                  | None ->
                      Error (Printf.sprintf "unknown modifier %S" modifier)
                  | Some `Ctrl -> collect { press with ctrl = true } rest
                  | Some `Shift -> collect { press with shift = true } rest
                  | Some `Alt -> collect { press with alt = true } rest
                  | Some `Super -> collect { press with super = true } rest)
            in
            collect base modifier_tokens)

let of_string s =
  let s = String.lowercase_ascii (String.trim s) in
  let tokens =
    List.filter (fun t -> not (String.equal t "")) (String.split_on_char ' ' s)
  in
  match tokens with
  | [] -> Error "empty chord"
  | _ :: _ :: _ :: _ -> Error (Printf.sprintf "%S has more than two presses" s)
  | tokens ->
      let rec build acc = function
        | [] -> Ok (List.rev acc)
        | token :: rest -> (
            match press_of_token token with
            | Ok press -> build (press :: acc) rest
            | Error _ as error -> error)
      in
      build [] tokens

let press_to_string press =
  let modifiers =
    List.filter_map Fun.id
      [
        (if press.ctrl then Some "ctrl" else None);
        (if press.alt then Some "alt" else None);
        (if press.shift then Some "shift" else None);
        (if press.super then Some "cmd" else None);
      ]
  in
  String.concat "+" (modifiers @ [ key_to_token press.key ])

let to_string chord = String.concat " " (List.map press_to_string chord)
let equal = List.equal press_equal
let pp ppf chord = Format.pp_print_string ppf (to_string chord)
let presses chord = chord

(* Two chords conflict when one is a prefix of the other: an equal pair, or a
   single-press binding that shadows a two-press chord's first press (which would
   fire before the chord could complete). [ctrl+x] vs [ctrl+x ctrl+e] conflict;
   [ctrl+x ctrl+e] vs [ctrl+x ctrl+r] do not — they diverge at the second press. *)
let rec chord_prefix a b =
  match (a, b) with
  | [], _ -> true
  | pa :: a', pb :: b' -> press_equal pa pb && chord_prefix a' b'
  | _ :: _, [] -> false

let conflict a b = chord_prefix a b || chord_prefix b a
