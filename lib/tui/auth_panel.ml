(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Mosaic
module Declaration = Mentat_provider
module Auth = Mentat_provider.Auth
module Login = Auth.Login
module Protocol = Login.Protocol
module Progress = Login.Progress
module Account = Mentat_provider.Account
module Phase = Account.Phase
module Discovery = Account.Discovery
module Logout = Account.Logout
module Credential_error = Mentat_provider.Credential_error
module Error = Mentat_protocol.Error
module Client_login = Mentat_client.Login

module Mode = struct
  type t = Login | Logout
end

type mode = Mode.t
type attempt = int

let attempt value = value

type pick = { query : string; selected : int }
type direct_action = Saving_api_key | Logging_out

type api_key_state = {
  ak_declaration : Declaration.t;
  ak_secret : string;
  ak_flash : string option;
}

type copied = Nothing | Primary | Url

type flow_state = {
  fl_declaration : Declaration.t;
  fl_login : Login.t;
  fl_attempt : attempt option;
  fl_progress : Progress.t option;
  fl_challenge : Progress.t option;
  fl_opened : bool;
  fl_open_error : string option;
  fl_copied : copied;
}

type external_state = {
  ex_declaration : Declaration.t;
  ex_login : Login.t;
  ex_copied : bool;
}

type direct_state = {
  dr_declaration : Declaration.t;
  dr_action : direct_action;
  dr_attempt : attempt option;
}

type settlement =
  | Login_saved of Account.t
  | Login_cancelled
  | Client_error of Error.t
  | Api_key_saved of Account.t
  | Logged_out of Logout.t
  | Direct_error of direct_action * Error.t

type stage =
  | Loading
  | Load_error of Error.t
  | Provider_pick of pick
  | Method_pick of Declaration.t * pick
  | Api_key of api_key_state
  | External of external_state
  | Flow of flow_state
  | Direct of direct_state
  | Settled of Declaration.t * settlement
  | Empty
  | Closed

type t = {
  mode : mode;
  declarations : Declaration.t list;
  readiness : Discovery.t list;
  requested : Mentat_llm.Provider.t option;
  stage : stage;
}

type msg = Key of Panel.key | Select_index of int | Activate_index of int

type event =
  | Stay
  | Close
  | Reload_readiness
  | Save_api_key of { provider : Mentat_llm.Provider.t; key : string }
  | Begin_login of { provider : Mentat_llm.Provider.t; method_ : Login.Id.t }
  | Logout of { provider : Mentat_llm.Provider.t }
  | Cancel_login of attempt
  | Copy of string
  | Open_url of { attempt : attempt; url : Uri.t }
  | Flash of string

let empty_pick = { query = ""; selected = 0 }

let deduplicate_declarations declarations =
  List.fold_left
    (fun kept declaration ->
      let provider = Declaration.id declaration in
      if
        List.exists
          (fun existing ->
            Mentat_llm.Provider.equal provider (Declaration.id existing))
          kept
      then kept
      else declaration :: kept)
    [] declarations
  |> List.rev

let loading ~mode ~declarations ?provider () =
  {
    mode;
    declarations = deduplicate_declarations declarations;
    readiness = [];
    requested = provider;
    stage = Loading;
  }

(* Safe text. *)

let strip_ansi value =
  if not (String.contains value '\x1b') then value
  else
    let length = String.length value in
    let buffer = Buffer.create length in
    let rec skip_csi index =
      if index >= length then index
      else
        let byte = Char.code value.[index] in
        if byte >= 0x40 && byte <= 0x7e then index + 1 else skip_csi (index + 1)
    in
    let rec skip_osc index =
      if index >= length then index
      else if Char.equal value.[index] '\x07' then index + 1
      else if
        Char.equal value.[index] '\x1b'
        && index + 1 < length
        && Char.equal value.[index + 1] '\\'
      then index + 2
      else skip_osc (index + 1)
    in
    let rec loop index =
      if index >= length then Buffer.contents buffer
      else if
        Char.equal value.[index] '\x1b'
        && index + 1 < length
        && Char.equal value.[index + 1] '['
      then loop (skip_csi (index + 2))
      else if
        Char.equal value.[index] '\x1b'
        && index + 1 < length
        && Char.equal value.[index + 1] ']'
      then loop (skip_osc (index + 2))
      else if Char.equal value.[index] '\x1b' then loop (min length (index + 2))
      else begin
        Buffer.add_char buffer value.[index];
        loop (index + 1)
      end
    in
    loop 0

type line_breaks = Preserve | Flatten

let append_break line_breaks buffer =
  Buffer.add_char buffer
    (match line_breaks with Preserve -> '\n' | Flatten -> ' ')

let append_uchar ~line_breaks buffer uchar =
  let code = Uchar.to_int uchar in
  if code = 0x09 then Buffer.add_string buffer "  "
  else if code = 0x0a || code = 0x0d || code = 0x2028 || code = 0x2029 then
    append_break line_breaks buffer
  else if code < 0x20 || (code >= 0x7f && code <= 0x9f) then
    Buffer.add_utf_8_uchar buffer Uchar.rep
  else Buffer.add_utf_8_uchar buffer uchar

let normalize ~line_breaks value =
  let value = strip_ansi value in
  let buffer = Buffer.create (String.length value) in
  let rec loop offset =
    if offset < String.length value then
      match value.[offset] with
      | '\r' ->
          let next = offset + 1 in
          let next =
            if next < String.length value && Char.equal value.[next] '\n' then
              next + 1
            else next
          in
          append_break line_breaks buffer;
          loop next
      | '\n' ->
          append_break line_breaks buffer;
          loop (offset + 1)
      | _ ->
          let decoded = String.get_utf_8_uchar value offset in
          let length = max 1 (Uchar.utf_decode_length decoded) in
          let uchar =
            if Uchar.utf_decode_is_valid decoded then
              Uchar.utf_decode_uchar decoded
            else Uchar.rep
          in
          append_uchar ~line_breaks buffer uchar;
          loop (offset + length)
  in
  loop 0;
  Buffer.contents buffer

let one_line value = normalize ~line_breaks:Flatten value |> String.trim
let many_lines value = normalize ~line_breaks:Preserve value
let uri_text uri = Uri.to_string uri

let visible_or ~fallback value =
  let value = one_line value in
  if String.is_empty value then fallback else value

let provider_id declaration =
  Mentat_llm.Provider.id (Declaration.id declaration)

let provider_name declaration =
  match Declaration.display_name declaration with
  | Some name -> visible_or ~fallback:(provider_id declaration) name
  | None -> provider_id declaration

let login_label login =
  visible_or ~fallback:(Login.Id.to_string (Login.id login)) (Login.label login)

let drop_last_grapheme value =
  if String.is_empty value then ""
  else if not (String.is_valid_utf_8 value) then
    String.sub value 0 (String.length value - 1)
  else
    let last = ref None in
    Matrix.Text.iter_graphemes
      (fun ~offset ~len ->
        let next = offset + len in
        if next <= String.length value then last := Some offset)
      value;
    match !last with None -> "" | Some offset -> String.sub value 0 offset

let strip_cr_lf value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (fun character ->
      if not (Char.equal character '\r' || Char.equal character '\n') then
        Buffer.add_char buffer character)
    value;
  Buffer.contents buffer

(* Owner-value joins and pickers. *)

let auth declaration = Declaration.auth declaration
let logins declaration = Auth.logins (auth declaration)

let env_names declaration =
  Auth.env (auth declaration)
  |> List.map (fun env -> Auth.Env.name env |> one_line)

let readiness_for t declaration =
  let provider = Declaration.id declaration in
  List.find_map
    (fun discovery ->
      if Mentat_llm.Provider.equal provider (Discovery.provider discovery) then
        Some discovery
      else None)
    t.readiness

let has_credential = function
  | Some (Discovery.Known account) -> (
      match Account.phase account with
      | Phase.Unchecked | Phase.Ready | Phase.Degraded | Phase.Blocked -> true
      | Phase.Missing -> false)
  | Some (Discovery.Resolution_failed _) -> true
  | None -> false

let selectable t declaration =
  match t.mode with
  | Mode.Login -> not (List.is_empty (logins declaration))
  | Mode.Logout -> has_credential (readiness_for t declaration)

let rank t declaration =
  match t.mode with
  | Mode.Logout -> if selectable t declaration then 0 else 1
  | Mode.Login -> (
      if List.is_empty (logins declaration) then 4
      else
        match readiness_for t declaration with
        | Some (Discovery.Known account) -> (
            match Account.phase account with
            | Phase.Missing -> 2
            | Phase.Blocked -> 1
            | Phase.Unchecked | Phase.Ready | Phase.Degraded -> 0)
        | Some (Discovery.Resolution_failed _) -> 1
        | None -> 3)

let base_declarations t =
  List.stable_sort
    (fun first second -> Int.compare (rank t first) (rank t second))
    t.declarations

let contains ~needle haystack =
  String.includes
    ~affix:(String.lowercase_ascii needle)
    (String.lowercase_ascii haystack)

let declaration_matches ~query declaration =
  let query = one_line query in
  String.is_empty query
  || contains ~needle:query (provider_name declaration)
  || contains ~needle:query (provider_id declaration)

let method_matches ~query login =
  let query = one_line query in
  String.is_empty query
  || contains ~needle:query (login_label login)
  || contains ~needle:query (Login.Id.to_string (Login.id login))

let pick_declarations t pick =
  List.filter (declaration_matches ~query:pick.query) (base_declarations t)

let pick_methods declaration pick =
  List.filter (method_matches ~query:pick.query) (logins declaration)

let clamp count selected = max 0 (min (max 0 (count - 1)) selected)

let find_declaration t provider =
  List.find_opt
    (fun declaration ->
      Mentat_llm.Provider.equal provider (Declaration.id declaration))
    t.declarations

let flow declaration login =
  Flow
    {
      fl_declaration = declaration;
      fl_login = login;
      fl_attempt = None;
      fl_progress = None;
      fl_challenge = None;
      fl_opened = false;
      fl_open_error = None;
      fl_copied = Nothing;
    }

let direct declaration action =
  Direct { dr_declaration = declaration; dr_action = action; dr_attempt = None }

let back_to_provider t = { t with stage = Provider_pick empty_pick }

let back_from_protocol declaration t =
  if List.length (logins declaration) > 1 then
    { t with stage = Method_pick (declaration, empty_pick) }
  else back_to_provider t

let flash_for_unavailable t declaration =
  let name = provider_name declaration in
  match t.mode with
  | Mode.Login ->
      let env = env_names declaration in
      if not (List.is_empty env) then
        name ^ " is configured through " ^ String.concat ", " env
      else name ^ " declares no login methods"
  | Mode.Logout -> name ^ " has no credential to remove"

let begin_direct declaration action t event =
  ({ t with stage = direct declaration action }, event)

let confirm_method declaration login t =
  match Login.protocol login with
  | Protocol.Api_key ->
      ( {
          t with
          stage =
            Api_key
              { ak_declaration = declaration; ak_secret = ""; ak_flash = None };
        },
        Stay )
  | Protocol.External _ ->
      ( {
          t with
          stage =
            External
              {
                ex_declaration = declaration;
                ex_login = login;
                ex_copied = false;
              };
        },
        Stay )
  | Protocol.OAuth2_device_code _ | Protocol.OAuth2_authorization_code _
  | Protocol.Provider_defined _ ->
      ( { t with stage = flow declaration login },
        Begin_login
          { provider = Declaration.id declaration; method_ = Login.id login } )

let confirm_provider declaration t =
  if not (selectable t declaration) then
    (t, Flash (flash_for_unavailable t declaration))
  else
    match t.mode with
    | Mode.Login -> (
        match logins declaration with
        | [] -> (t, Flash (flash_for_unavailable t declaration))
        | [ login ] -> confirm_method declaration login t
        | _ -> ({ t with stage = Method_pick (declaration, empty_pick) }, Stay))
    | Mode.Logout ->
        begin_direct declaration Logging_out t
          (Logout { provider = Declaration.id declaration })

let successful_readiness readiness t =
  let t = { t with readiness } in
  match t.requested with
  | Some provider -> (
      match find_declaration t provider with
      | Some declaration -> confirm_provider declaration t
      | None ->
          ( { t with stage = Provider_pick empty_pick },
            Flash
              (Mentat_llm.Provider.id provider ^ " is not a declared provider")
          ))
  | None -> (
      let eligible = List.filter (selectable t) t.declarations in
      match (t.mode, eligible) with
      | Mode.Logout, [] -> ({ t with stage = Empty }, Stay)
      | Mode.Logout, [ declaration ] -> confirm_provider declaration t
      | Mode.Login, _ | Mode.Logout, _ ->
          ({ t with stage = Provider_pick empty_pick }, Stay))

let readiness_loaded result t =
  match t.stage with
  | Loading -> (
      match result with
      | Ok readiness -> successful_readiness readiness t
      | Error error -> ({ t with stage = Load_error error }, Stay))
  | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _ | External _
  | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      (t, Stay)

(* Input and transitions. *)

let key event =
  match Panel.classify event with
  | Panel.Action Panel.Other -> None
  | classified -> Some (Key classified)

let close t = ({ t with stage = Closed }, Close)

let query t =
  match t.stage with
  | Provider_pick pick | Method_pick (_, pick) -> pick.query
  | Loading | Load_error _ | Api_key _ | External _ | Flow _ | Direct _
  | Settled _ | Empty | Closed ->
      ""

let with_query query t =
  match t.stage with
  | Provider_pick _ -> { t with stage = Provider_pick { query; selected = 0 } }
  | Method_pick (declaration, _) ->
      { t with stage = Method_pick (declaration, { query; selected = 0 }) }
  | Loading | Load_error _ | Api_key _ | External _ | Flow _ | Direct _
  | Settled _ | Empty | Closed ->
      t

let move offset t =
  let wrap count selected =
    if count = 0 then 0 else Option_list.wrap ~count selected offset
  in
  match t.stage with
  | Provider_pick pick ->
      let count = List.length (pick_declarations t pick) in
      {
        t with
        stage = Provider_pick { pick with selected = wrap count pick.selected };
      }
  | Method_pick (declaration, pick) ->
      let count = List.length (pick_methods declaration pick) in
      {
        t with
        stage =
          Method_pick
            (declaration, { pick with selected = wrap count pick.selected });
      }
  | Loading | Load_error _ | Api_key _ | External _ | Flow _ | Direct _
  | Settled _ | Empty | Closed ->
      t

let select_index index t =
  let index = max 0 index in
  match t.stage with
  | Provider_pick pick ->
      let count = List.length (pick_declarations t pick) in
      {
        t with
        stage = Provider_pick { pick with selected = clamp count index };
      }
  | Method_pick (declaration, pick) ->
      let count = List.length (pick_methods declaration pick) in
      {
        t with
        stage =
          Method_pick (declaration, { pick with selected = clamp count index });
      }
  | Loading | Load_error _ | Api_key _ | External _ | Flow _ | Direct _
  | Settled _ | Empty | Closed ->
      t

let confirm t =
  match t.stage with
  | Provider_pick pick -> (
      let declarations = pick_declarations t pick in
      let selected = clamp (List.length declarations) pick.selected in
      match List.nth_opt declarations selected with
      | Some declaration -> confirm_provider declaration t
      | None -> (t, Stay))
  | Method_pick (declaration, pick) -> (
      let methods = pick_methods declaration pick in
      let selected = clamp (List.length methods) pick.selected in
      match List.nth_opt methods selected with
      | Some login -> confirm_method declaration login t
      | None -> (t, Stay))
  | Loading | Load_error _ | Api_key _ | External _ | Flow _ | Direct _
  | Settled _ | Empty | Closed ->
      (t, Stay)

let jump digit t =
  let select index =
    match t.stage with
    | Provider_pick pick ->
        { t with stage = Provider_pick { pick with selected = index } }
    | Method_pick (declaration, pick) ->
        {
          t with
          stage = Method_pick (declaration, { pick with selected = index });
        }
    | Loading | Load_error _ | Api_key _ | External _ | Flow _ | Direct _
    | Settled _ | Empty | Closed ->
        t
  in
  let count =
    match t.stage with
    | Provider_pick pick -> List.length (pick_declarations t pick)
    | Method_pick (declaration, pick) ->
        List.length (pick_methods declaration pick)
    | Loading | Load_error _ | Api_key _ | External _ | Flow _ | Direct _
    | Settled _ | Empty | Closed ->
        0
  in
  if digit >= 1 && digit <= count then confirm (select (digit - 1))
  else (t, Stay)

let submit_api_key state t =
  if String.is_empty state.ak_secret then
    ( {
        t with
        stage = Api_key { state with ak_flash = Some "Enter an API key." };
      },
      Stay )
  else
    let provider = Declaration.id state.ak_declaration in
    let key = state.ak_secret in
    ( { t with stage = direct state.ak_declaration Saving_api_key },
      Save_api_key { provider; key } )

let external_instructions login =
  match Login.protocol login with
  | Protocol.External { instructions = Some instructions; _ } ->
      let instructions = many_lines instructions |> String.trim in
      if String.is_empty instructions then None else Some instructions
  | Protocol.External { instructions = None; _ }
  | Protocol.Api_key | Protocol.OAuth2_device_code _
  | Protocol.OAuth2_authorization_code _ | Protocol.Provider_defined _ ->
      None

let structured_error_text error =
  Error.diagnostic error |> Mentat_diagnostic.to_string |> many_lines

let flow_primary flow =
  match flow.fl_challenge with
  | Some (Progress.Browser_url url) -> Some (uri_text url)
  | Some (Progress.Device_challenge { user_code; _ }) -> Some user_code
  | Some (Progress.Listening _) | None -> None

let flow_url flow =
  match flow.fl_challenge with
  | Some (Progress.Browser_url url) -> Some url
  | Some (Progress.Device_challenge { url; _ }) -> Some url
  | Some (Progress.Listening _) | None -> None

let cancel_flow flow t =
  let t = back_from_protocol flow.fl_declaration t in
  match flow.fl_attempt with
  | Some attempt -> (t, Cancel_login attempt)
  | None -> (t, Stay)

let update message t =
  match message with
  | Select_index index -> (select_index index t, Stay)
  | Activate_index index -> confirm (select_index index t)
  | Key classified -> (
      match t.stage with
      | Api_key state -> (
          match classified with
          | Panel.Action Panel.Escape ->
              (back_from_protocol state.ak_declaration t, Stay)
          | Panel.Action Panel.Enter -> submit_api_key state t
          | Panel.Action Panel.Backspace ->
              ( {
                  t with
                  stage =
                    Api_key
                      {
                        state with
                        ak_secret = drop_last_grapheme state.ak_secret;
                        ak_flash = None;
                      };
                },
                Stay )
          | Panel.Printable text ->
              ( {
                  t with
                  stage =
                    Api_key
                      {
                        state with
                        ak_secret = state.ak_secret ^ text;
                        ak_flash = None;
                      };
                },
                Stay )
          | Panel.Digit digit ->
              ( {
                  t with
                  stage =
                    Api_key
                      {
                        state with
                        ak_secret = state.ak_secret ^ string_of_int digit;
                        ak_flash = None;
                      };
                },
                Stay )
          | Panel.Action
              ( Panel.Tab | Panel.Left | Panel.Right | Panel.Up | Panel.Down
              | Panel.Ctrl_d | Panel.Other ) ->
              (t, Stay))
      | External state -> (
          match classified with
          | Panel.Action Panel.Escape ->
              (back_from_protocol state.ex_declaration t, Stay)
          | Panel.Printable "c" -> (
              match external_instructions state.ex_login with
              | Some instructions ->
                  ( { t with stage = External { state with ex_copied = true } },
                    Copy instructions )
              | None -> (t, Stay))
          | Panel.Printable _ | Panel.Digit _ | Panel.Action _ -> (t, Stay))
      | Flow flow -> (
          match classified with
          | Panel.Action Panel.Escape -> cancel_flow flow t
          | Panel.Printable "c" -> (
              match flow_primary flow with
              | Some value ->
                  ( { t with stage = Flow { flow with fl_copied = Primary } },
                    Copy value )
              | None -> (t, Stay))
          | Panel.Printable "u" -> (
              match flow.fl_challenge with
              | Some (Progress.Device_challenge { url; _ }) ->
                  let url = uri_text url in
                  ( { t with stage = Flow { flow with fl_copied = Url } },
                    Copy url )
              | Some (Progress.Browser_url _ | Progress.Listening _) | None ->
                  (t, Stay))
          | Panel.Action Panel.Enter -> (
              match (flow.fl_attempt, flow_url flow) with
              | Some attempt, Some url -> (t, Open_url { attempt; url })
              | None, _ | _, None -> (t, Stay))
          | Panel.Printable _ | Panel.Digit _ | Panel.Action _ -> (t, Stay))
      | Direct _ -> (
          match classified with
          | Panel.Action Panel.Escape -> close t
          | Panel.Printable _ | Panel.Digit _ | Panel.Action _ -> (t, Stay))
      | Settled _ -> (
          match classified with
          | Panel.Action (Panel.Escape | Panel.Enter) -> close t
          | Panel.Printable _ | Panel.Digit _ | Panel.Action _ -> (t, Stay))
      | Load_error _ -> (
          match classified with
          | Panel.Action Panel.Escape -> close t
          | Panel.Action Panel.Enter ->
              ({ t with stage = Loading }, Reload_readiness)
          | Panel.Printable _ | Panel.Digit _ | Panel.Action _ -> (t, Stay))
      | Loading | Empty -> (
          match classified with
          | Panel.Action Panel.Escape -> close t
          | Panel.Printable _ | Panel.Digit _ | Panel.Action _ -> (t, Stay))
      | Closed -> (t, Stay)
      | Provider_pick _ | Method_pick _ -> (
          match classified with
          | Panel.Action Panel.Escape -> (
              match t.stage with
              | Provider_pick _ -> close t
              | Method_pick _ -> (back_to_provider t, Stay)
              | Loading | Load_error _ | Api_key _ | External _ | Flow _
              | Direct _ | Settled _ | Empty | Closed ->
                  (t, Stay))
          | Panel.Action (Panel.Enter | Panel.Tab) -> confirm t
          | Panel.Action Panel.Up -> (move (-1) t, Stay)
          | Panel.Action Panel.Down -> (move 1 t, Stay)
          | Panel.Action Panel.Backspace ->
              (with_query (drop_last_grapheme (query t)) t, Stay)
          | Panel.Printable text -> (with_query (query t ^ text) t, Stay)
          | Panel.Digit digit ->
              if String.is_empty (query t) then jump digit t
              else (with_query (query t ^ string_of_int digit) t, Stay)
          | Panel.Action (Panel.Left | Panel.Right | Panel.Ctrl_d | Panel.Other)
            ->
              (t, Stay)))

let accepts_paste t = match t.stage with Api_key _ -> true | _ -> false

let paste value t =
  match t.stage with
  | Api_key state ->
      {
        t with
        stage =
          Api_key
            {
              state with
              ak_secret = state.ak_secret ^ strip_cr_lf value;
              ak_flash = None;
            };
      }
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | External _
  | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      t

(* Correlated async folds. *)

let started ~attempt t =
  match t.stage with
  | Flow flow when Option.is_none flow.fl_attempt ->
      { t with stage = Flow { flow with fl_attempt = Some attempt } }
  | Direct direct when Option.is_none direct.dr_attempt ->
      { t with stage = Direct { direct with dr_attempt = Some attempt } }
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _
  | External _ | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      t

let active_attempt t =
  match t.stage with
  | Flow flow -> flow.fl_attempt
  | Direct direct -> direct.dr_attempt
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _
  | External _ | Settled _ | Empty | Closed ->
      None

let same_attempt expected actual = expected = Some actual

let cancel_active t =
  match t.stage with
  | Flow flow when Option.is_some flow.fl_attempt -> Some (cancel_flow flow t)
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _
  | External _ | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      None

let login_step ~attempt step t =
  match t.stage with
  | Flow flow when same_attempt flow.fl_attempt attempt -> (
      match step with
      | Client_login.Progress (Progress.Browser_url url as progress) ->
          ( {
              t with
              stage =
                Flow
                  {
                    flow with
                    fl_progress = Some progress;
                    fl_challenge = Some progress;
                    fl_opened = false;
                    fl_open_error = None;
                    fl_copied = Nothing;
                  };
            },
            Open_url { attempt; url } )
      | Client_login.Progress (Progress.Device_challenge _ as progress) ->
          ( {
              t with
              stage =
                Flow
                  {
                    flow with
                    fl_progress = Some progress;
                    fl_challenge = Some progress;
                    fl_opened = false;
                    fl_open_error = None;
                    fl_copied = Nothing;
                  };
            },
            Stay )
      | Client_login.Progress (Progress.Listening _ as progress) ->
          ( {
              t with
              stage =
                Flow
                  { flow with fl_progress = Some progress; fl_copied = Nothing };
            },
            Stay )
      | Client_login.Saved account ->
          ( { t with stage = Settled (flow.fl_declaration, Login_saved account) },
            Stay )
      | Client_login.Cancelled ->
          ( { t with stage = Settled (flow.fl_declaration, Login_cancelled) },
            Stay ))
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _
  | External _ | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      (t, Stay)

let login_failed ~attempt error t =
  match t.stage with
  | Flow flow when same_attempt flow.fl_attempt attempt ->
      { t with stage = Settled (flow.fl_declaration, Client_error error) }
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _
  | External _ | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      t

let save_api_key_finished ~attempt result t =
  match t.stage with
  | Direct ({ dr_action = Saving_api_key; _ } as direct)
    when same_attempt direct.dr_attempt attempt ->
      let settlement =
        match result with
        | Ok account -> Api_key_saved account
        | Error error -> Direct_error (direct.dr_action, error)
      in
      { t with stage = Settled (direct.dr_declaration, settlement) }
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _
  | External _ | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      t

let logout_finished ~attempt result t =
  match t.stage with
  | Direct ({ dr_action = Logging_out; _ } as direct)
    when same_attempt direct.dr_attempt attempt ->
      let settlement =
        match result with
        | Ok logout -> Logged_out logout
        | Error error -> Direct_error (direct.dr_action, error)
      in
      { t with stage = Settled (direct.dr_declaration, settlement) }
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _
  | External _ | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      t

let url_opened ~attempt t =
  match t.stage with
  | Flow flow when same_attempt flow.fl_attempt attempt ->
      {
        t with
        stage = Flow { flow with fl_opened = true; fl_open_error = None };
      }
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _
  | External _ | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      t

let url_open_failed ~attempt ~message t =
  match t.stage with
  | Flow flow when same_attempt flow.fl_attempt attempt ->
      let message =
        visible_or ~fallback:"Could not open the browser." message
      in
      {
        t with
        stage =
          Flow { flow with fl_opened = false; fl_open_error = Some message };
      }
  | Loading | Load_error _ | Provider_pick _ | Method_pick _ | Api_key _
  | External _ | Flow _ | Direct _ | Settled _ | Empty | Closed ->
      t

(* View. *)

let minimum_size = { width = px 0; height = px 0 }
let full_width = { width = pct 100; height = auto }

let prose ?key ?(style = Ansi.Style.default) ?(wrap = `Word)
    ?(selectable = false) value =
  text ?key ~style ~wrap ~truncate:false ~selectable ~flex_shrink:0.
    ~min_size:minimum_size ~size:full_width value

let prose_group ?key children =
  (* A label and its value are one field. The surrounding content column adds
     space between fields without separating the two parts of a field. *)
  box ?key ~flex_direction:Flex_direction.Column ~flex_shrink:0.
    ~min_size:minimum_size ~size:full_width children

let content_column ?key children =
  box ?key ~flex_direction:Flex_direction.Column ~flex_grow:1. ~flex_shrink:1.
    ~min_size:minimum_size ~size:full_width
    ~gap:{ width = px 0; height = px 1 }
    ~padding:(padding_lrtb 2 2 0 0) children

let scrollable ?(autofocus = true) key children =
  scroll_box ~key ~scroll_x:false ~scroll_y:true ~focusable:true ~autofocus
    ~flex_grow:1. ~flex_shrink:1. ~min_size:minimum_size
    ~size:{ width = pct 100; height = pct 100 }
    [ content_column children ]

let discovery_text = function
  | Discovery.Known account -> Phase.to_string (Account.phase account)
  | Discovery.Resolution_failed { error; _ } ->
      "credential error: " ^ Credential_error.message error

let readiness_word = function
  | None -> "status unavailable"
  | Some discovery -> discovery_text discovery

let provider_detail t declaration =
  match t.mode with
  | Mode.Login when List.is_empty (logins declaration) -> (
      match env_names declaration with
      | [] -> "Unavailable: no declared login method"
      | names -> "Configured outside Mentat through " ^ String.concat ", " names
      )
  | Mode.Login | Mode.Logout ->
      let readiness = readiness_word (readiness_for t declaration) in
      if selectable t declaration then readiness
      else "Unavailable: " ^ readiness

let method_detail login =
  match Login.protocol login with
  | Protocol.Api_key -> "API key"
  | Protocol.OAuth2_authorization_code _ -> "Browser"
  | Protocol.OAuth2_device_code _ -> "Device code"
  | Protocol.Provider_defined _ -> "Provider flow"
  | Protocol.External _ -> "Outside Mentat"

let picker ~palette ~key ~title ~selected items =
  let options =
    List.mapi
      (fun index (label, description) ->
        {
          Select.label = string_of_int (index + 1) ^ "  " ^ label;
          description = Some (one_line description);
        })
      items
  in
  content_column ~key
    [
      prose ~style:(Theme.Palette.muted_style palette) title;
      select ~key:(key ^ ".select") ~selected_index:selected
        ~show_description:true ~show_scroll_indicator:true ~wrap_selection:true
        ~background:Ansi.Color.default ~text_color:Ansi.Color.default
        ~focused_background:Ansi.Color.default
        ~focused_text_color:(Theme.Palette.muted palette)
        ~selected_background:(Theme.Palette.selection_bg palette)
        ~selected_text_color:(Theme.Palette.selection_fg palette)
        ~description_color:(Theme.Palette.faint palette)
        ~selected_description_color:(Theme.Palette.muted palette)
        ~autofocus:true ~flex_grow:1. ~flex_shrink:1. ~min_size:minimum_size
        ~size:{ width = pct 100; height = pct 100 }
        ~on_change:(fun index -> Some (Select_index index))
        ~on_activate:(fun index -> Some (Activate_index index))
        options;
    ]

let provider_picker ~palette t pick =
  let declarations = pick_declarations t pick in
  if List.is_empty declarations then
    scrollable "auth.providers.empty"
      [
        prose
          ~style:(Theme.Palette.muted_style palette)
          "No matching providers.";
        prose
          ~style:(Theme.Palette.faint_style palette)
          "Backspace edits the filter; Escape closes this panel.";
      ]
  else
    let title =
      match t.mode with
      | Mode.Login -> "Choose a provider."
      | Mode.Logout -> "Choose credentials to remove."
    in
    let items =
      List.map
        (fun declaration ->
          (provider_name declaration, provider_detail t declaration))
        declarations
    in
    picker ~palette ~key:"auth.providers" ~title
      ~selected:(clamp (List.length declarations) pick.selected)
      items

let method_picker ~palette declaration pick =
  let methods = pick_methods declaration pick in
  if List.is_empty methods then
    scrollable "auth.methods.empty"
      [
        prose ~style:Theme.bold ("Log in to " ^ provider_name declaration);
        prose
          ~style:(Theme.Palette.muted_style palette)
          "No matching login methods.";
      ]
  else
    let items =
      List.map (fun login -> (login_label login, method_detail login)) methods
    in
    picker ~palette ~key:"auth.methods"
      ~title:("Choose how to log in to " ^ provider_name declaration ^ ".")
      ~selected:(clamp (List.length methods) pick.selected)
      items

let masked_secret secret =
  let count =
    if not (String.is_valid_utf_8 secret) then String.length secret
    else
      let count = ref 0 in
      Matrix.Text.iter_graphemes (fun ~offset:_ ~len:_ -> incr count) secret;
      !count
  in
  let buffer = Buffer.create (String.length secret) in
  let rec append = function
    | 0 -> ()
    | remaining ->
        Buffer.add_string buffer "•";
        append (remaining - 1)
  in
  append count;
  (Buffer.contents buffer, count)

let api_key_content ~palette state =
  let masked, cursor = masked_secret state.ak_secret in
  let field =
    box ~key:"auth.api-key.field" ~flex_direction:Flex_direction.Row
      ~flex_shrink:0. ~min_size:minimum_size ~size:full_width
      [
        text
          ~style:(Theme.Palette.accent_style palette)
          ~wrap:`None ~selectable:false ~flex_shrink:0. Theme.cursor;
        (* The field holds focus so the terminal caret tracks the controlled
           cursor; Mosaic surfaces a caret only on the focused node. The panel
           still owns editing: the hook classifies exactly as the shell's auth
           branch would and consumes what the panel handles, so the widget's
           own editing never runs and unclassified chords (Ctrl+C) flow on.
           Pastes reach the panel through the shell borrow either way; the
           controlled value reconciles the widget's default insert before the
           next frame renders. *)
        Mosaic.input ~value:masked ~cursor
          ~placeholder:"Paste or type an API key" ~max_length:max_int
          ~text_color:(Theme.Palette.accent palette)
          ~background_color:Ansi.Color.default ~focusable:true ~autofocus:true
          ~selectable:false
          ~on_key:(fun ev ->
            match key (Event.Key.data ev) with
            | None -> None
            | Some message ->
                Event.Key.prevent_default ev;
                Some message)
          ~flex_grow:1. ~flex_shrink:1. ~min_size:minimum_size
          ~size:{ width = pct 100; height = px 1 }
          ();
      ]
  in
  let env =
    match env_names state.ak_declaration with
    | [] -> []
    | names ->
        [
          prose
            ~style:(Theme.Palette.faint_style palette)
            ("The provider also declares " ^ String.concat ", " names
           ^ " for configuration outside this editor.");
        ]
  in
  let flash =
    match state.ak_flash with
    | None -> []
    | Some message ->
        [ prose ~style:(Theme.Palette.warning_style palette) message ]
  in
  (* The stage's only autofocus target must be the field: sibling autofocus
     nodes in one mount batch resolve first-mounted-wins, and the scroll box
     mounts before its content. *)
  scrollable ~autofocus:false "auth.api-key"
    ([
       prose ~style:Theme.bold
         ("Enter an API key for " ^ provider_name state.ak_declaration);
       field;
     ]
    @ flash
    @ [
        prose
          ~style:(Theme.Palette.muted_style palette)
          "The key is masked and leaves this panel only when saved.";
      ]
    @ env
    @ [
        prose
          ~style:(Theme.Palette.faint_style palette)
          "Enter saves. Escape discards and goes back.";
      ])

let protocol_title login =
  match Login.protocol login with
  | Protocol.OAuth2_authorization_code _ -> "browser"
  | Protocol.OAuth2_device_code _ -> "device code"
  | Protocol.Provider_defined _ -> "provider login"
  | Protocol.Api_key -> "API key"
  | Protocol.External _ -> "external"

let copied_word copied target =
  match (copied, target) with
  | Primary, `Primary | Url, `Url -> " · copied"
  | Nothing, _ | Primary, `Url | Url, `Primary -> ""

let challenge_content ~palette flow =
  match flow.fl_challenge with
  | Some (Progress.Browser_url url) ->
      [
        prose_group ~key:"auth.flow.browser-url"
          [
            prose ~style:(Theme.Palette.muted_style palette) "Sign-in URL";
            prose
              ~style:(Theme.Palette.accent_style palette)
              ~wrap:`Char ~selectable:true
              (one_line (uri_text url) ^ copied_word flow.fl_copied `Primary);
          ];
      ]
  | Some (Progress.Device_challenge { url; user_code; expires_in }) ->
      [
        prose_group ~key:"auth.flow.device-url"
          [
            prose ~style:(Theme.Palette.muted_style palette) "Verification URL";
            prose
              ~style:(Theme.Palette.accent_style palette)
              ~wrap:`Char ~selectable:true
              (one_line (uri_text url) ^ copied_word flow.fl_copied `Url);
          ];
        prose_group ~key:"auth.flow.device-code"
          [
            prose ~style:(Theme.Palette.muted_style palette) "Device code";
            prose
              ~style:(Theme.Palette.accent_style palette)
              ~wrap:`Char ~selectable:true
              (one_line user_code ^ copied_word flow.fl_copied `Primary);
          ];
        prose
          ~style:(Theme.Palette.faint_style palette)
          (Printf.sprintf "Challenge expires in %d second%s." expires_in
             (if expires_in = 1 then "" else "s"));
      ]
  | Some (Progress.Listening _) | None -> []

let flow_content ~palette flow =
  let status =
    match flow.fl_progress with
    | None ->
        [ prose ~style:(Theme.Palette.muted_style palette) "Starting login…" ]
    | Some (Progress.Browser_url _) ->
        [
          prose
            ~style:(Theme.Palette.muted_style palette)
            (if flow.fl_opened then "Browser opened. Complete sign-in there."
             else "Opening your browser…");
        ]
    | Some (Progress.Listening { redirect_uri }) ->
        [
          prose
            ~style:(Theme.Palette.muted_style palette)
            "Listening for the authorization callback at";
          prose
            ~style:(Theme.Palette.faint_style palette)
            ~wrap:`Char ~selectable:true
            (one_line (uri_text redirect_uri));
        ]
    | Some (Progress.Device_challenge _) ->
        [
          prose
            ~style:(Theme.Palette.muted_style palette)
            (if flow.fl_opened then
               "Verification page opened. Enter the device code there."
             else "Open the verification page, then enter the device code.");
          prose
            ~style:(Theme.Palette.warning_style palette)
            "Never share a device code.";
        ]
  in
  let opening =
    match flow.fl_open_error with
    | None -> []
    | Some message ->
        [ prose ~style:(Theme.Palette.warning_style palette) message ]
  in
  scrollable "auth.flow"
    (prose ~style:Theme.bold
       ("Log in to "
       ^ provider_name flow.fl_declaration
       ^ " · "
       ^ protocol_title flow.fl_login)
     :: challenge_content ~palette flow
    @ opening @ status)

let external_content ~palette state =
  let instructions =
    match external_instructions state.ex_login with
    | None -> "Complete this login outside Mentat."
    | Some instructions -> instructions
  in
  let copy_hint =
    match external_instructions state.ex_login with
    | None -> "Escape goes back."
    | Some _ ->
        if state.ex_copied then "Instructions copied. Escape goes back."
        else "Press c to copy the instructions. Escape goes back."
  in
  scrollable "auth.external"
    [
      prose ~style:Theme.bold
        ("Log in to "
        ^ provider_name state.ex_declaration
        ^ " · " ^ login_label state.ex_login);
      prose
        ~style:(Theme.Palette.muted_style palette)
        ~selectable:true instructions;
      prose
        ~style:(Theme.Palette.muted_style palette)
        "This declared method cannot be completed by the Mentat client.";
      prose ~style:(Theme.Palette.faint_style palette) copy_hint;
    ]

let direct_working = function
  | Saving_api_key -> "Saving API key…"
  | Logging_out -> "Removing credentials…"

let direct_failure = function
  | Saving_api_key -> "Saving the API key failed."
  | Logging_out -> "Removing credentials failed."

let phase_style ~palette = function
  | Phase.Ready -> Theme.Palette.success_style palette
  | Phase.Unchecked | Phase.Degraded -> Theme.Palette.warning_style palette
  | Phase.Missing -> Theme.Palette.muted_style palette
  | Phase.Blocked -> Theme.Palette.error_style palette

let discovery_content ~palette label = function
  | Discovery.Known account ->
      let phase = Account.phase account in
      [
        prose
          ~style:(phase_style ~palette phase)
          (label ^ ": " ^ Phase.to_string phase);
      ]
  | Discovery.Resolution_failed { error; _ } ->
      [
        prose
          ~style:(Theme.Palette.error_style palette)
          ~selectable:true
          (label ^ ": " ^ Credential_error.message error);
      ]

let logout_content ~palette logout =
  let settlement =
    match logout.Logout.revocation with
    | None ->
        [
          prose
            ~style:(Theme.Palette.success_style palette)
            "Local credential removal settled.";
        ]
    | Some Logout.Not_stored ->
        [
          prose
            ~style:(Theme.Palette.muted_style palette)
            "No stored credential was present.";
        ]
    | Some (Logout.Settled { remote; local }) ->
        let remote =
          match remote with
          | Logout.Revoked ->
              prose
                ~style:(Theme.Palette.success_style palette)
                "Remote credential revoked."
          | Logout.Unsupported ->
              prose
                ~style:(Theme.Palette.warning_style palette)
                "Remote revocation is unsupported."
          | Logout.Failed problem ->
              prose
                ~style:(Theme.Palette.error_style palette)
                ("Remote revocation failed: "
                ^ Format.asprintf "%a" Account.Problem.pp problem)
        in
        let local =
          match local with
          | Logout.Removed ->
              prose
                ~style:(Theme.Palette.success_style palette)
                "Stored credential removed."
          | Logout.Superseded ->
              prose
                ~style:(Theme.Palette.warning_style palette)
                "A newer stored credential replaced the one being removed."
        in
        [ remote; local ]
  in
  settlement
  @ discovery_content ~palette "Current account readiness" logout.Logout.current

let settled_content ~palette declaration settlement =
  let result =
    match settlement with
    | Login_saved account ->
        let phase = Account.phase account in
        [
          prose ~style:(Theme.Palette.success_style palette) "Credential saved.";
          prose
            ~style:(phase_style ~palette phase)
            ("Account readiness: " ^ Phase.to_string phase);
        ]
    | Login_cancelled ->
        [ prose ~style:(Theme.Palette.muted_style palette) "Login cancelled." ]
    | Client_error error ->
        [
          prose
            ~style:(Theme.Palette.error_style palette)
            ~selectable:true
            (structured_error_text error);
        ]
    | Api_key_saved account ->
        prose ~style:(Theme.Palette.success_style palette) "API key saved."
        :: discovery_content ~palette "Account readiness"
             (Discovery.Known account)
    | Logged_out logout -> logout_content ~palette logout
    | Direct_error (action, error) ->
        [
          prose
            ~style:(Theme.Palette.error_style palette)
            (direct_failure action);
          prose
            ~style:(Theme.Palette.error_style palette)
            ~selectable:true
            (structured_error_text error);
        ]
  in
  scrollable "auth.settled"
    ([ prose ~style:Theme.bold (provider_name declaration) ]
    @ result
    @ [
        prose
          ~style:(Theme.Palette.faint_style palette)
          "Enter or Escape closes.";
      ])

let empty_message = function
  | Mode.Login -> "No providers declare a login method."
  | Mode.Logout -> "No provider has credentials to remove."

let content ~palette t =
  match t.stage with
  | Loading ->
      scrollable "auth.loading"
        [
          prose
            ~style:(Theme.Palette.muted_style palette)
            "⠋ Loading account readiness…";
        ]
  | Load_error error ->
      scrollable "auth.load-error"
        [
          prose
            ~style:(Theme.Palette.error_style palette)
            ~selectable:true
            (structured_error_text error);
          prose
            ~style:(Theme.Palette.faint_style palette)
            "Enter retries. Escape closes.";
        ]
  | Provider_pick pick -> provider_picker ~palette t pick
  | Method_pick (declaration, pick) -> method_picker ~palette declaration pick
  | Api_key state -> api_key_content ~palette state
  | External state -> external_content ~palette state
  | Flow flow -> flow_content ~palette flow
  | Direct state ->
      scrollable "auth.direct"
        [
          prose
            ~style:(Theme.Palette.muted_style palette)
            (direct_working state.dr_action);
        ]
  | Settled (declaration, settlement) ->
      settled_content ~palette declaration settlement
  | Empty ->
      scrollable "auth.empty"
        [
          prose
            ~style:(Theme.Palette.muted_style palette)
            (empty_message t.mode);
        ]
  | Closed ->
      scrollable "auth.closed"
        [ prose ~style:(Theme.Palette.muted_style palette) "Closing…" ]

let chip_name = function Mode.Login -> "log in" | Mode.Logout -> "log out"

let hints t =
  match t.stage with
  | Provider_pick _ | Method_pick _ -> "↵ choose · esc back · ↑↓ select"
  | Api_key _ -> "↵ save · esc discard"
  | External _ -> "c copy · ↑↓ scroll · esc back"
  | Flow { fl_challenge = Some (Progress.Device_challenge _); _ } ->
      "↵ open · c code · u copy URL · esc cancel"
  | Flow _ -> "↵ open · c copy URL · esc cancel"
  | Load_error _ -> "↵ retry · ↑↓ scroll · esc close"
  | Settled _ -> "↑↓ scroll · ↵ close · esc close"
  | Loading | Direct _ | Empty | Closed -> "esc close"

let view ~palette ~frame t =
  Panel.view ~palette ~frame ~name:(chip_name t.mode)
    ~filter:(query t |> one_line)
    ~hint:[ hints t ]
    ~content:(content ~palette t)
