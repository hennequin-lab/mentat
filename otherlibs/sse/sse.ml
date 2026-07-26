(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Event = struct
  type t = { id : string option; name : string; data : string }
end

module Writer = struct
  let frame ?id ?event ~data () =
    let buffer = Buffer.create (String.length data + 16) in
    (match id with
    | Some id ->
        Buffer.add_string buffer "id: ";
        Buffer.add_string buffer id;
        Buffer.add_char buffer '\n'
    | None -> ());
    (match event with
    | Some event ->
        Buffer.add_string buffer "event: ";
        Buffer.add_string buffer event;
        Buffer.add_char buffer '\n'
    | None -> ());
    String.split_on_char '\n' data
    |> List.iter (fun line ->
        Buffer.add_string buffer "data: ";
        Buffer.add_string buffer line;
        Buffer.add_char buffer '\n');
    Buffer.add_char buffer '\n';
    Buffer.contents buffer

  let heartbeat = ": hb\n\n"
end

module Reader = struct
  let next (read_line : unit -> string option) : Event.t option =
    let data = Buffer.create 256 in
    let id = ref None in
    let name = ref "" in
    let has_data = ref false in
    let emit () =
      { Event.id = !id; name = !name; data = Buffer.contents data }
    in
    let rec loop () =
      match read_line () with
      | None -> if !has_data then Some (emit ()) else None
      | Some line ->
          if String.equal line "" then
            if !has_data then Some (emit ()) else loop ()
          else if String.length line > 0 && line.[0] = ':' then loop ()
          else begin
            (match String.index_opt line ':' with
            | None -> ()
            | Some colon ->
                let field = String.sub line 0 colon in
                let rest_start =
                  (* An optional single space after the colon is stripped. *)
                  if colon + 1 < String.length line && line.[colon + 1] = ' '
                  then colon + 2
                  else colon + 1
                in
                let value =
                  String.sub line rest_start (String.length line - rest_start)
                in
                if String.equal field "data" then begin
                  if !has_data then Buffer.add_char data '\n';
                  Buffer.add_string data value;
                  has_data := true
                end
                else if String.equal field "id" then id := Some value
                else if String.equal field "event" then name := value);
            loop ()
          end
    in
    loop ()
end
