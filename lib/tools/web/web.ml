(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module Policy = Policy
module Transport = Transport
module Search_service = Search_service
module Web_fetch = Web_fetch
module Web_search = Web_search

let tools ~policy ~net ~mono_clock ?search () =
  let fetch = Transport_eio.make ~net ~mono_clock in
  let search_tools =
    match search with
    | None -> []
    | Some (backend, api_key) ->
        [ Web_search.make ~policy ~backend ?api_key ~fetch () ]
  in
  Web_fetch.make ~policy ~fetch :: search_tools

module For_testing = Transport_eio.For_testing
