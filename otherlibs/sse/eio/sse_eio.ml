(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Reader = struct
  type t = {
    source : Eio.Flow.source_ty Eio.Std.r;
    chunk : Cstruct.t;
    mutable head : int;
    mutable tail : int;
    mutable closed : bool;
  }

  let of_source (source : _ Eio.Flow.source) =
    {
      source :> Eio.Flow.source_ty Eio.Std.r;
      chunk = Cstruct.create 4096;
      head = 0;
      tail = 0;
      closed = false;
    }

  (* Refill the 4 KiB window with the next read; a closed source leaves the
     window empty. The body is never buffered whole — only one window at a
     time. *)
  let refill t =
    t.head <- 0;
    t.tail <- 0;
    match Eio.Flow.single_read t.source t.chunk with
    | exception End_of_file -> t.closed <- true
    | count -> t.tail <- count

  (* Pull one line (without its terminator), or [None] at end of stream. A
     trailing unterminated line is returned once, then the source reads closed. *)
  let read_line t =
    let buffer = Buffer.create 256 in
    let flush () =
      if Buffer.length buffer = 0 then None else Some (Buffer.contents buffer)
    in
    let rec loop () =
      if t.head >= t.tail then
        if t.closed then flush ()
        else begin
          refill t;
          if t.closed && t.tail = 0 then flush () else loop ()
        end
      else begin
        let char = Cstruct.get_char t.chunk t.head in
        t.head <- t.head + 1;
        match char with
        | '\n' -> Some (Buffer.contents buffer)
        | '\r' -> loop ()
        | char ->
            Buffer.add_char buffer char;
            loop ()
      end
    in
    loop ()

  let next t = Sse.Reader.next (fun () -> read_line t)
end
