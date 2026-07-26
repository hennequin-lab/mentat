(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let drain body =
  let buffer = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read body buffer with
    | exception End_of_file -> ()
    | _ -> loop ()
  in
  loop ()

let health ?(timeout_s = 2.) ~env ~base_url () =
  let clock = Eio.Stdenv.clock env in
  match
    Eio.Time.with_timeout clock timeout_s (fun () ->
        Ok
          ( Eio.Switch.run ~name:"local.health" @@ fun sw ->
            try
              let client =
                Cohttp_eio.Client.make ~https:None (Eio.Stdenv.net env)
              in
              let response, body =
                Cohttp_eio.Client.call client ~sw `GET
                  (Uri.of_string (base_url ^ "/health"))
              in
              let status =
                Cohttp.Code.code_of_status (Cohttp.Response.status response)
              in
              drain body;
              if status >= 200 && status < 300 then Ok ()
              else Error (Printf.sprintf "health check returned HTTP %d" status)
            with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | exn -> Error (Printexc.to_string exn) ))
  with
  | Ok result -> result
  | Error `Timeout -> Error "health check timed out"
