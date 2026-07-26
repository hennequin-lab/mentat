(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Name = struct
  type t = string

  let max_segment_bytes = 64

  let valid_segment segment =
    String.length segment > 0
    && String.length segment <= max_segment_bytes
    && (match segment.[0] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false)
    && String.for_all
         (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
         segment

  let valid name =
    String.length name > 0
    && List.for_all valid_segment (String.split_on_char ':' name)

  let of_string name =
    if valid name then Ok name
    else
      Error
        (Printf.sprintf
           "%S is not a command name (one or more :-joined segments of \
            lowercase letters, digits, and hyphens, each at most %d bytes and \
            leading with a letter or digit)"
           name max_segment_bytes)

  let to_string name = name
  let equal = String.equal
  let compare = String.compare
  let pp = Format.pp_print_string

  let jsont =
    Jsont.map ~kind:"command name"
      ~dec:(fun s ->
        match of_string s with
        | Ok name -> name
        | Error message -> Jsont.Error.msg Jsont.Meta.none message)
      ~enc:to_string Jsont.string
end

type scope = Project | User

type t = {
  name : Name.t;
  description : string option;
  argument_hint : string option;
  scope : scope;
}

let scope_jsont =
  Jsont.enum ~kind:"user command scope" [ ("project", Project); ("user", User) ]

let jsont =
  Jsont.Object.map ~kind:"user command"
    (fun name description argument_hint scope ->
      { name; description; argument_hint; scope })
  |> Jsont.Object.mem "name" Name.jsont ~enc:(fun t -> t.name)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun t ->
      t.description)
  |> Jsont.Object.opt_mem "argument_hint" Jsont.string ~enc:(fun t ->
      t.argument_hint)
  |> Jsont.Object.mem "scope" scope_jsont ~enc:(fun t -> t.scope)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
