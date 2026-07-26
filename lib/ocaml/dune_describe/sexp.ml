(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t = Atom of string | List of t list

let parse ~source input =
  let len = String.length input in
  let error ?offset message =
    Error
      (Error.Parse_error
         { source; offset = Some (Option.value offset ~default:0); message })
  in
  let rec skip i =
    if i >= len then i
    else
      match input.[i] with ' ' | '\t' | '\r' | '\n' -> skip (i + 1) | _ -> i
  in
  let rec quoted buffer i =
    if i >= len then error ~offset:i "unterminated string"
    else
      match input.[i] with
      | '"' -> Ok (Atom (Buffer.contents buffer), i + 1)
      | '\\' when i + 1 < len ->
          let char =
            match input.[i + 1] with
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | c -> c
          in
          Buffer.add_char buffer char;
          quoted buffer (i + 2)
      | '\\' -> error ~offset:i "unterminated escape"
      | c ->
          Buffer.add_char buffer c;
          quoted buffer (i + 1)
  in
  let atom i =
    let start = i in
    let rec loop i =
      if i >= len then i
      else
        match input.[i] with
        | ' ' | '\t' | '\r' | '\n' | '(' | ')' -> i
        | _ -> loop (i + 1)
    in
    let stop = loop i in
    if stop = start then error ~offset:start "expected atom"
    else Ok (Atom (String.sub input start (stop - start)), stop)
  in
  let rec one i =
    let i = skip i in
    if i >= len then error ~offset:i "expected s-expression"
    else
      match input.[i] with
      | '(' -> list [] (i + 1)
      | ')' -> error ~offset:i "unexpected ')'"
      | '"' -> quoted (Buffer.create 16) (i + 1)
      | _ -> atom i
  and list acc i =
    let i = skip i in
    if i >= len then error ~offset:i "unterminated list"
    else
      match input.[i] with
      | ')' -> Ok (List (List.rev acc), i + 1)
      | _ -> (
          match one i with
          | Error _ as error -> error
          | Ok (sexp, i) -> list (sexp :: acc) i)
  in
  match one 0 with
  | Error _ as error -> error
  | Ok (sexp, i) ->
      let i = skip i in
      if i = len then Ok sexp else error ~offset:i "trailing input"

let atom = function Atom value -> Some value | List _ -> None
let list = function List values -> Some values | Atom _ -> None
let to_string = function Atom value -> value | List _ -> "<list>"
