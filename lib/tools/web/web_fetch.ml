(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let name = "web_fetch"
let maximum_url_bytes = 2_000
let maximum_diagnostic_bytes = 4_096
let maximum_redirects = 10
let user_agent = "mentat/0 (+https://github.com/invariant-hq/mentat)"
let json_string value = Jsont.Json.string value
let json_int value = Jsont.Json.int value

module Input = struct
  type format = Markdown | Text | Html
  type t = { url : string; format : format; timeout_ms : int option }

  let format_to_string = function
    | Markdown -> "markdown"
    | Text -> "text"
    | Html -> "html"

  let format_of_string = function
    | None | Some "markdown" -> Markdown
    | Some "text" -> Text
    | Some "html" -> Html
    | Some format -> invalid_arg ("unknown format: " ^ format)

  let make url format timeout_ms =
    Mentat_tool.Codec.decode_invalid_arg @@ fun () ->
    if String.is_empty url then invalid_arg "url must not be empty";
    if String.length url > maximum_url_bytes then
      invalid_arg "url must be at most 2000 bytes";
    if String.contains url '\x00' then invalid_arg "url must not contain NUL";
    if not (String.is_valid_utf_8 url) then
      invalid_arg "url must be valid UTF-8";
    (match timeout_ms with
    | Some timeout_ms when timeout_ms <= 0 ->
        invalid_arg "timeout_ms must be positive"
    | Some _ | None -> ());
    { url; format = format_of_string format; timeout_ms }

  let max_input_integer =
    Float.min 9_007_199_254_740_991. (float_of_int Int.max_int)

  let exact_integer =
    let decode = function
      | Jsont.Number (value, _)
        when Float.is_integer value && Float.abs value <= max_input_integer ->
          int_of_float value
      | Jsont.Number _ | Jsont.Null _ | Jsont.Bool _ | Jsont.String _
      | Jsont.Array _ | Jsont.Object _ ->
          Codec.decode_error "expected an integer in JSON's safe integer range"
    in
    Jsont.map ~kind:"integer" ~dec:decode ~enc:json_int Jsont.json

  let object_codec =
    Jsont.Object.map ~kind:"web_fetch input" make
    |> Jsont.Object.mem "url" Jsont.string ~enc:(fun input -> input.url)
    |> Jsont.Object.opt_mem "format" Jsont.string ~enc:(fun input ->
        Some (format_to_string input.format))
    |> Jsont.Object.opt_mem "timeout_ms" exact_integer ~enc:(fun input ->
        input.timeout_ms)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let codec = Codec.strict_object ~kind:"strict web_fetch input" object_codec

  let schema =
    Codec.obj
      [
        ("type", json_string "object");
        ( "properties",
          Codec.obj
            [
              ( "url",
                Codec.obj
                  [
                    ("type", json_string "string");
                    ( "description",
                      json_string
                        "Public HTTP or HTTPS URL to fetch. HTTP is upgraded \
                         to HTTPS. Its UTF-8 encoding must not exceed 2000 \
                         bytes." );
                    ("minLength", json_int 1);
                  ] );
              ( "format",
                Codec.obj
                  [
                    ("type", json_string "string");
                    ( "enum",
                      Jsont.Json.list
                        [
                          json_string "markdown";
                          json_string "text";
                          json_string "html";
                        ] );
                    ( "description",
                      json_string
                        "Response rendering format. Defaults to markdown." );
                  ] );
              ( "timeout_ms",
                Codec.obj
                  [
                    ("type", json_string "integer");
                    ( "description",
                      json_string
                        "Whole-request timeout in milliseconds, bounded by \
                         host policy." );
                    ("minimum", json_int 1);
                    ("maximum", Jsont.Json.number max_input_integer);
                  ] );
            ] );
        ("required", Jsont.Json.list [ json_string "url" ]);
        ("additionalProperties", Jsont.Json.bool false);
      ]

  let contract = Mentat_tool.Input.make codec ~schema
end

type url_error =
  | Empty_url
  | Url_too_long
  | Invalid_url
  | Unsupported_scheme
  | Missing_host
  | Invalid_host
  | Userinfo_not_allowed
  | Fragment_not_allowed
  | Invalid_port
  | Private_host_not_allowed

let url_error_message = function
  | Empty_url -> "url must not be empty"
  | Url_too_long -> "url must be at most 2000 bytes"
  | Invalid_url -> "url is not a valid absolute HTTP or HTTPS URL"
  | Unsupported_scheme -> "url must use HTTP or HTTPS"
  | Missing_host -> "url must include a host"
  | Invalid_host -> "url host must be an ASCII DNS name or IP literal"
  | Userinfo_not_allowed -> "url must not include username or password"
  | Fragment_not_allowed -> "url must not include a fragment"
  | Invalid_port -> "url port must be between 1 and 65535"
  | Private_host_not_allowed -> "private or local URL hosts are not allowed"

let ascii_space = function
  | ' ' | '\t' | '\n' | '\r' | '\x0c' -> true
  | _ -> false

let trim_ascii text =
  let length = String.length text in
  let rec left index =
    if index < length && ascii_space text.[index] then left (index + 1)
    else index
  in
  let rec right index =
    if index >= 0 && ascii_space text.[index] then right (index - 1) else index
  in
  let first = left 0 in
  let last = right (length - 1) in
  if last < first then "" else String.sub text first (last - first + 1)

let contains_forbidden_url_byte text =
  String.exists
    (fun byte ->
      let code = Char.code byte in
      code <= 0x20 || code = 0x7f)
    text

let has_userinfo text =
  match String.index_opt text ':' with
  | None -> false
  | Some colon ->
      let start = colon + 1 in
      let length = String.length text in
      if
        start + 1 >= length
        || (not (Char.equal text.[start] '/'))
        || not (Char.equal text.[start + 1] '/')
      then false
      else
        let rec loop index =
          if index >= length then false
          else
            match text.[index] with
            | '@' -> true
            | '/' | '?' | '#' -> false
            | _ -> loop (index + 1)
        in
        loop (start + 2)

let ipv4_parts host =
  let part text =
    if
      String.is_empty text || (String.length text > 1 && Char.equal text.[0] '0')
    then None
    else
      match int_of_string_opt text with
      | Some value when value >= 0 && value <= 255 -> Some value
      | Some _ | None -> None
  in
  match List.map part (String.split_on_char '.' host) with
  | [ Some a; Some b; Some c; Some d ] -> Some (a, b, c, d)
  | _ -> None

let private_ipv4 a b c d =
  a = 0 || a = 10 || a = 127
  || (a = 100 && b >= 64 && b <= 127)
  || (a = 169 && b = 254)
  || (a = 172 && b >= 16 && b <= 31)
  || (a = 192 && b = 0 && c = 0 && d <> 9 && d <> 10)
  || (a = 192 && b = 0 && c = 2)
  || (a = 192 && b = 88 && c = 99)
  || (a = 192 && b = 168)
  || (a = 198 && (b = 18 || b = 19 || (b = 51 && c = 100)))
  || (a = 203 && b = 0 && c = 113)
  || a >= 224

let private_host host =
  let host = String.lowercase_ascii host in
  let numeric_host =
    String.for_all (function '0' .. '9' | '.' -> true | _ -> false) host
  in
  String.equal host "localhost"
  || String.ends_with ~suffix:".localhost" host
  ||
  match ipv4_parts host with
  | Some (a, b, c, d) -> private_ipv4 a b c d
  | None when String.contains host ':' ->
      String.equal host "::" || String.equal host "::1"
      || String.starts_with ~prefix:"fc" host
      || String.starts_with ~prefix:"fd" host
      || String.starts_with ~prefix:"fe8" host
      || String.starts_with ~prefix:"fe9" host
      || String.starts_with ~prefix:"fea" host
      || String.starts_with ~prefix:"feb" host
      || String.starts_with ~prefix:"ff" host
      || String.starts_with ~prefix:"::ffff:" host
  | None -> numeric_host || not (String.contains host '.')

let valid_hostname_label label =
  let length = String.length label in
  length > 0 && length <= 63
  && label.[0] <> '-'
  && label.[length - 1] <> '-'
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false)
       label

let valid_ipv6 host =
  let valid_hex group =
    let length = String.length group in
    length >= 1 && length <= 4
    && String.for_all
         (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
         group
  in
  let rec double_colon index =
    if index + 1 >= String.length host then None
    else if Char.equal host.[index] ':' && Char.equal host.[index + 1] ':' then
      Some index
    else double_colon (index + 1)
  in
  let side_groups side =
    if String.is_empty side then Some []
    else
      let groups = String.split_on_char ':' side in
      if List.exists String.is_empty groups then None else Some groups
  in
  let valid_groups ~compressed groups =
    let last = List.length groups - 1 in
    let rec loop index units = function
      | [] -> if compressed then units < 8 else units = 8
      | group :: rest ->
          if String.contains group '.' then
            if index <> last || Option.is_none (ipv4_parts group) then false
            else loop (index + 1) (units + 2) rest
          else if valid_hex group then loop (index + 1) (units + 1) rest
          else false
    in
    loop 0 0 groups
  in
  match double_colon 0 with
  | None -> valid_groups ~compressed:false (String.split_on_char ':' host)
  | Some separator -> (
      let after = separator + 2 in
      let left = String.sub host 0 separator in
      let right = String.sub host after (String.length host - after) in
      if Option.is_some (double_colon after) then false
      else
        match (side_groups left, side_groups right) with
        | Some left, Some right -> valid_groups ~compressed:true (left @ right)
        | _ -> false)

let valid_host host =
  String.length host <= 253
  && String.for_all (fun byte -> Char.code byte < 128) host
  &&
  if String.contains host ':' then valid_ipv6 host
  else List.for_all valid_hostname_label (String.split_on_char '.' host)

type normalized_url = {
  uri : Uri.t;
  rendered : string;
  host : string;
  effective_port : int;
  scheme : [ `Http | `Https ];
}

let normalize_url_inner ~allow_private_network ~upgrade_http raw =
  let raw = trim_ascii raw in
  if String.is_empty raw then Error Empty_url
  else if String.length raw > maximum_url_bytes then Error Url_too_long
  else if contains_forbidden_url_byte raw || not (String.is_valid_utf_8 raw)
  then Error Invalid_url
  else if has_userinfo raw then Error Userinfo_not_allowed
  else
    match Uri.of_string raw with
    | exception _ -> Error Invalid_url
    | uri ->
        let scheme = Option.map String.lowercase_ascii (Uri.scheme uri) in
        begin match scheme with
        | Some (("http" | "https") as scheme) ->
            begin match Uri.host uri with
            | None | Some "" -> Error Missing_host
            | Some host ->
                let host = String.lowercase_ascii host in
                let host =
                  if String.ends_with ~suffix:"." host then
                    String.drop_last 1 host
                  else host
                in
                if not (valid_host host) then Error Invalid_host
                else if Option.is_some (Uri.fragment uri) then
                  Error Fragment_not_allowed
                else
                  begin match Uri.port uri with
                  | Some port when port < 1 || port > 65_535 ->
                      Error Invalid_port
                  | port ->
                      if (not allow_private_network) && private_host host then
                        Error Private_host_not_allowed
                      else
                        let scheme =
                          if upgrade_http && String.equal scheme "http" then
                            "https"
                          else scheme
                        in
                        let uri = Uri.with_scheme uri (Some scheme) in
                        let uri = Uri.with_host uri (Some host) in
                        let uri = Uri.with_fragment uri None in
                        let text = Uri.to_string uri in
                        if String.length text > maximum_url_bytes then
                          Error Url_too_long
                        else
                          let scheme_kind =
                            if String.equal scheme "https" then `Https
                            else `Http
                          in
                          Ok
                            {
                              uri;
                              rendered = text;
                              host;
                              effective_port =
                                Option.value port
                                  ~default:
                                    (match scheme_kind with
                                    | `Http -> 80
                                    | `Https -> 443);
                              scheme = scheme_kind;
                            }
                  end
            end
        | Some _ | None -> Error Unsupported_scheme
        end

let normalize_url ~allow_private_network ~upgrade_http raw =
  match normalize_url_inner ~allow_private_network ~upgrade_http raw with
  | result -> result
  | exception (Invalid_argument _ | Failure _) -> Error Invalid_url

let same_authority left right =
  left.scheme = right.scheme
  && left.effective_port = right.effective_port
  && String.equal left.host right.host

let accept_header = function
  | Input.Markdown ->
      "text/markdown;q=1.0, text/x-markdown;q=0.9, text/plain;q=0.8, \
       text/html;q=0.7, */*;q=0.1"
  | Input.Text ->
      "text/plain;q=1.0, text/markdown;q=0.9, text/html;q=0.8, */*;q=0.1"
  | Input.Html ->
      "text/html;q=1.0, application/xhtml+xml;q=0.9, text/plain;q=0.8, \
       text/markdown;q=0.7, */*;q=0.1"

let headers format =
  [
    ("user-agent", user_agent);
    ("accept", accept_header format);
    ("accept-language", "en-US,en;q=0.9");
  ]

let permissions policy input =
  match
    ( normalize_url
        ~allow_private_network:(Policy.allow_private_network policy)
        ~upgrade_http:true input.Input.url,
      Policy.resolve_timeout_ms policy input.Input.timeout_ms )
  with
  | Ok url, Ok _ ->
      [
        Mentat_permission.Request.of_accesses ~source:name
          [
            Mentat_permission.Access.network ~protocol:`Https
              ~port:url.effective_port ~host:url.host ();
          ];
      ]
  | _ -> []

let diagnostic text =
  let text =
    text |> Text_helpers.utf8_lossy |> Text_helpers.strip_ansi
    |> Text_helpers.scrub_controls |> String.trim
  in
  let text = if String.is_empty text then "web fetch failed" else text in
  Text_helpers.valid_utf8_prefix text maximum_diagnostic_bytes

let interrupted () = Mentat_tool.Result.cancelled ()
let failed kind message = Mentat_tool.Result.failed kind (diagnostic message)

let tag text start stop =
  let rec skip index =
    if index < stop && ascii_space text.[index] then skip (index + 1) else index
  in
  let start = skip start in
  let closing, start =
    if start < stop && Char.equal text.[start] '/' then (true, skip (start + 1))
    else (false, start)
  in
  let rec finish index =
    if index >= stop then index
    else
      match text.[index] with
      | 'a' .. 'z' | '0' .. '9' -> finish (index + 1)
      | _ -> index
  in
  let finish = finish start in
  (closing, String.sub text start (finish - start))

let container_tag = function
  | "script" | "style" | "noscript" | "iframe" | "object" -> true
  | _ -> false

let metadata_tag = function "meta" | "link" | "embed" -> true | _ -> false

let sanitize_html html =
  let lower = String.lowercase_ascii html in
  let length = String.length html in
  let buffer = Buffer.create length in
  let rec loop index =
    if index >= length then Buffer.contents buffer
    else if not (Char.equal html.[index] '<') then begin
      Buffer.add_char buffer html.[index];
      loop (index + 1)
    end
    else
      match String.index_from_opt html index '>' with
      | None -> Buffer.contents buffer
      | Some tag_end ->
          let closing, name = tag lower (index + 1) tag_end in
          if metadata_tag name then loop (tag_end + 1)
          else if container_tag name && closing then loop (tag_end + 1)
          else if container_tag name then
            let close = "</" ^ name in
            begin match
              String.find_first ~sub:close ~start:(tag_end + 1) lower
            with
            | None -> Buffer.contents buffer
            | Some close_start ->
                begin match String.index_from_opt html close_start '>' with
                | None -> Buffer.contents buffer
                | Some close_end -> loop (close_end + 1)
                end
            end
          else begin
            Buffer.add_substring buffer html index (tag_end - index + 1);
            loop (tag_end + 1)
          end
  in
  loop 0

let decode_entity entity =
  let named =
    match entity with
    | "amp" -> Some "&"
    | "lt" -> Some "<"
    | "gt" -> Some ">"
    | "quot" -> Some "\""
    | "apos" -> Some "'"
    | "nbsp" -> Some " "
    | _ -> None
  in
  match named with
  | Some decoded -> decoded
  | None ->
      let scalar =
        if
          String.starts_with ~prefix:"#x" entity
          || String.starts_with ~prefix:"#X" entity
        then int_of_string_opt ("0x" ^ String.drop_first 2 entity)
        else if String.starts_with ~prefix:"#" entity then
          int_of_string_opt (String.drop_first 1 entity)
        else None
      in
      begin match scalar with
      | Some value when Uchar.is_valid value ->
          let buffer = Buffer.create 4 in
          Buffer.add_utf_8_uchar buffer (Uchar.of_int value);
          Buffer.contents buffer
      | Some _ | None -> "&" ^ entity ^ ";"
      end

let decode_entities text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let rec loop index =
    if index >= length then Buffer.contents buffer
    else if Char.equal text.[index] '&' then (
      match String.index_from_opt text index ';' with
      | Some stop when stop - index <= 16 ->
          Buffer.add_string buffer
            (decode_entity (String.sub text (index + 1) (stop - index - 1)));
          loop (stop + 1)
      | Some _ | None ->
          Buffer.add_char buffer '&';
          loop (index + 1))
    else begin
      Buffer.add_char buffer text.[index];
      loop (index + 1)
    end
  in
  loop 0

let project_html ~markdown html =
  let html = sanitize_html html in
  let lower = String.lowercase_ascii html in
  let length = String.length html in
  let buffer = Buffer.create length in
  let rec loop index =
    if index >= length then Buffer.contents buffer
    else if Char.equal html.[index] '<' then (
      match String.index_from_opt html index '>' with
      | None -> Buffer.contents buffer
      | Some tag_end ->
          let closing, name = tag lower (index + 1) tag_end in
          (if markdown then
             begin match (closing, name) with
             | false, "br" -> Buffer.add_char buffer '\n'
             | false, ("p" | "div") -> Buffer.add_char buffer '\n'
             | false, "li" -> Buffer.add_string buffer "\n- "
             | false, "h1" -> Buffer.add_string buffer "\n# "
             | false, "h2" -> Buffer.add_string buffer "\n## "
             | false, "h3" -> Buffer.add_string buffer "\n### "
             | true, ("p" | "div" | "li" | "h1" | "h2" | "h3") ->
                 Buffer.add_char buffer '\n'
             | _ -> ()
             end
           else
             match (closing, name) with
             | false, "br" -> Buffer.add_char buffer '\n'
             | (false | true), ("p" | "div" | "li" | "h1" | "h2" | "h3") ->
                 Buffer.add_char buffer '\n'
             | _ -> ());
          loop (tag_end + 1))
    else begin
      Buffer.add_char buffer html.[index];
      loop (index + 1)
    end
  in
  loop 0 |> decode_entities |> String.trim

let mime_and_charset = function
  | None -> ("", None)
  | Some content_type ->
      let parts = String.split_on_char ';' content_type in
      let mime =
        match parts with
        | [] -> ""
        | mime :: _ -> String.lowercase_ascii (String.trim mime)
      in
      let charset =
        List.find_map
          (fun parameter ->
            match String.index_opt parameter '=' with
            | None -> None
            | Some separator ->
                let name =
                  String.sub parameter 0 separator
                  |> String.trim |> String.lowercase_ascii
                in
                if not (String.equal name "charset") then None
                else
                  let value =
                    String.sub parameter (separator + 1)
                      (String.length parameter - separator - 1)
                    |> String.trim |> String.lowercase_ascii
                  in
                  let length = String.length value in
                  let value =
                    if
                      length >= 2
                      && Char.equal value.[0] '"'
                      && Char.equal value.[length - 1] '"'
                    then String.sub value 1 (length - 2)
                    else value
                  in
                  Some value)
          parts
      in
      (mime, charset)

let textual_mime mime =
  String.is_empty mime
  || String.starts_with ~prefix:"text/" mime
  || String.equal mime "application/json"
  || String.ends_with ~suffix:"+json" mime
  || String.equal mime "application/xml"
  || String.ends_with ~suffix:"+xml" mime
  || String.equal mime "application/javascript"
  || String.equal mime "application/x-javascript"

let html_mime mime =
  String.equal mime "text/html" || String.equal mime "application/xhtml+xml"

let supported_charset = function
  | None | Some ("" | "utf-8" | "utf8" | "us-ascii" | "ascii") -> true
  | Some _ -> false

let project_body format ~mime body =
  let projected =
    if html_mime mime then
      match format with
      | Input.Markdown -> project_html ~markdown:true body
      | Input.Text -> project_html ~markdown:false body
      | Input.Html -> sanitize_html body
    else body
  in
  Text_helpers.strip_ansi projected

let utf8_character_count text =
  let rec loop index count =
    if index >= String.length text then count
    else
      let decoded = String.get_utf_8_uchar text index in
      loop (index + Uchar.utf_decode_length decoded) (count + 1)
  in
  loop 0 0

let byte_offset_after_characters text count =
  let rec loop index remaining =
    if remaining = 0 || index >= String.length text then index
    else
      let decoded = String.get_utf_8_uchar text index in
      loop (index + Uchar.utf_decode_length decoded) (remaining - 1)
  in
  loop 0 count

let truncate_middle ~max_chars text =
  let count = utf8_character_count text in
  if count <= max_chars then (text, false, 0)
  else
    let marker = "\n[... omitted ...]\n" in
    let marker_chars = String.length marker in
    if max_chars <= marker_chars then
      let stop = byte_offset_after_characters text max_chars in
      (String.sub text 0 stop, true, count - max_chars)
    else
      let retained = max_chars - marker_chars in
      let head_chars = retained / 2 in
      let tail_chars = retained - head_chars in
      let head_stop = byte_offset_after_characters text head_chars in
      let tail_start = byte_offset_after_characters text (count - tail_chars) in
      let head = String.sub text 0 head_stop in
      let tail = String.sub text tail_start (String.length text - tail_start) in
      (head ^ marker ^ tail, true, count - retained)

type output = {
  semantic : Mentat_tools_output.Web.Fetch.t;
  text : string;
  truncated : bool;
}

let encode_output { semantic; text; truncated } =
  Mentat_tools_output.Codec.encode Mentat_tools_output.Web.Fetch.jsont ~text
    ~truncated semantic

let status_output ~disposition ~head ~bytes ~text ~truncated =
  let semantic =
    Mentat_tools_output.Web.Fetch.make ~disposition
      ~status:head.Transport.status ~bytes
  in
  { semantic; text; truncated }

let response_header kind head bytes =
  let content_type =
    Option.value head.Transport.content_type ~default:"unspecified"
    |> diagnostic
  in
  Printf.sprintf
    "%s %s\nStatus: %d\nContent-Type: %s\nBytes: %d\nDuration: %dms\n\n" kind
    (Uri.to_string head.Transport.effective_url)
    head.Transport.status content_type bytes head.Transport.duration_ms

let body_result policy format head body =
  let bytes = String.length body in
  if bytes > Policy.max_fetch_bytes policy then
    failed `Failed
      (Printf.sprintf
         "web transport returned %d bytes, exceeding the %d-byte response limit"
         bytes
         (Policy.max_fetch_bytes policy))
  else
    let mime, charset = mime_and_charset head.Transport.content_type in
    if not (textual_mime mime) then
      failed `Failed
        ("web response has unsupported non-text MIME type: " ^ diagnostic mime)
    else if not (supported_charset charset) then
      failed `Failed
        ("web response has unsupported charset: "
        ^ diagnostic (Option.value charset ~default:"unknown"))
    else if not (String.is_valid_utf_8 body) then
      failed `Failed "web response body is not valid UTF-8"
    else
      let body = project_body format ~mime body in
      let body, truncated, omitted =
        truncate_middle ~max_chars:(Policy.max_output_chars policy) body
      in
      let truncation_note =
        if truncated then
          Printf.sprintf "\n\n[Output omitted %d Unicode characters.]" omitted
        else ""
      in
      let kind =
        if head.Transport.status >= 200 && head.Transport.status <= 299 then
          "Fetched"
        else "HTTP response from"
      in
      let text = response_header kind head bytes ^ body ^ truncation_note in
      if head.Transport.status >= 200 && head.Transport.status <= 299 then
        Mentat_tool.Result.completed
          ~output:
            (status_output ~disposition:Mentat_tools_output.Web.Fetch.Fetched
               ~head ~bytes ~text ~truncated)
          ()
      else
        Mentat_tool.Result.failed
          ~output:
            (status_output ~disposition:Mentat_tools_output.Web.Fetch.Http_error
               ~head ~bytes ~text ~truncated)
          `Failed
          (Printf.sprintf "web server returned HTTP status %d"
             head.Transport.status)

let redirect_result head target =
  let text =
    Printf.sprintf
      "Redirect %d from %s to %s\n\
       Fetch the target URL in a new web_fetch call so its network authority \
       can be reviewed."
      head.Transport.status
      (Uri.to_string head.Transport.effective_url)
      target.rendered
  in
  Mentat_tool.Result.completed
    ~output:
      (status_output ~disposition:Mentat_tools_output.Web.Fetch.Redirected ~head
         ~bytes:0 ~text ~truncated:false)
    ()

let validate_head policy request_url head =
  match
    normalize_url
      ~allow_private_network:(Policy.allow_private_network policy)
      ~upgrade_http:false
      (Uri.to_string head.Transport.effective_url)
  with
  | Error _ -> Error "transport returned an invalid effective URL"
  | Ok effective_url when not (same_authority request_url effective_url) ->
      Error "transport crossed the permitted network authority"
  | Ok effective_url -> Ok effective_url

let response_result policy format request_url = function
  | Transport.Body { head; body } ->
      begin match validate_head policy request_url head with
      | Error message -> failed `Failed message
      | Ok _ -> body_result policy format head body
      end
  | Transport.Redirect { head; target } ->
      begin match validate_head policy request_url head with
      | Error message -> failed `Failed message
      | Ok _ when head.Transport.status < 300 || head.Transport.status > 399 ->
          failed `Failed "transport returned a redirect with a non-3xx status"
      | Ok _ ->
          begin match
            normalize_url ~allow_private_network:true ~upgrade_http:false
              (Uri.to_string target)
          with
          | Error _ ->
              failed `Failed "transport returned an invalid redirect URL"
          | Ok target when same_authority request_url target ->
              failed `Failed
                "transport returned a same-authority redirect instead of \
                 following it"
          | Ok target -> redirect_result head target
          end
      end

let partial_http_error policy request_url head bytes message =
  match validate_head policy request_url head with
  | Error protocol -> failed `Failed protocol
  | Ok _ ->
      if head.Transport.status >= 200 && head.Transport.status <= 299 then
        failed `Failed message
      else
        let text =
          response_header "Incomplete HTTP response from" head bytes ^ message
        in
        Mentat_tool.Result.failed
          ~output:
            (status_output ~disposition:Mentat_tools_output.Web.Fetch.Http_error
               ~head ~bytes ~text ~truncated:true)
          `Failed (diagnostic message)

let transport_url_on_authority policy request_url uri =
  match
    normalize_url
      ~allow_private_network:(Policy.allow_private_network policy)
      ~upgrade_http:false (Uri.to_string uri)
  with
  | Ok url when same_authority request_url url -> true
  | Ok _ | Error _ -> false

let transport_error policy request_url = function
  | Transport.Cancelled -> interrupted ()
  | Transport.Timed_out -> failed `Timed_out "web fetch timed out"
  | Transport.Private_address { url; address } ->
      if not (transport_url_on_authority policy request_url url) then
        failed `Failed
          "transport reported an address outside the permitted authority"
      else
        failed `Permission_denied
          ("web fetch resolved to a disallowed address: " ^ diagnostic address)
  | Transport.Response_too_large { head; limit; received } ->
      if limit <> Policy.max_fetch_bytes policy || received < 0 then
        failed `Failed "transport returned an invalid response-size error"
      else
        partial_http_error policy request_url head received
          (Printf.sprintf "Response exceeded the %d-byte limit after %d bytes."
             limit received)
  | Transport.Too_many_redirects { head; limit } as error ->
      begin match validate_head policy request_url head with
      | Error message -> failed `Failed message
      | Ok _
        when limit <> maximum_redirects
             || head.Transport.status < 300
             || head.Transport.status > 399 ->
          failed `Failed "transport returned an invalid redirect-limit error"
      | Ok _ -> failed `Failed (Transport.error_message error)
      end
  | Transport.Protocol { diagnostic = detail; _ } ->
      failed `Failed ("web protocol failure: " ^ diagnostic detail)
  | Transport.Transport detail ->
      failed `Unavailable ("web transport failure: " ^ diagnostic detail)

let run policy fetch input ~cancelled =
  if cancelled () then interrupted ()
  else
    match
      normalize_url
        ~allow_private_network:(Policy.allow_private_network policy)
        ~upgrade_http:true input.Input.url
    with
    | Error error -> failed `Invalid_input (url_error_message error)
    | Ok url ->
        begin match Policy.resolve_timeout_ms policy input.Input.timeout_ms with
        | Error error ->
            failed `Invalid_input (Policy.Timeout_error.message error)
        | Ok timeout_ms ->
            let request =
              Transport.Request.make ~url:url.uri
                ~headers:(headers input.Input.format)
                ~timeout_ms
                ~max_body_bytes:(Policy.max_fetch_bytes policy)
                ~allow_private_network:(Policy.allow_private_network policy)
                ~max_redirects:maximum_redirects ()
            in
            let result =
              match Eio.Switch.run (fun sw -> fetch ~sw ~cancelled request) with
              | result -> Ok result
              | exception (Eio.Cancel.Cancelled _ as cancelled) ->
                  raise cancelled
              | exception exn -> Error (Printexc.to_string exn)
            in
            begin match result with
            | Error _ when cancelled () -> interrupted ()
            | Error message ->
                failed `Unavailable ("web transport raised: " ^ message)
            | Ok _ when cancelled () -> interrupted ()
            | Ok (Error error) -> transport_error policy url error
            | Ok (Ok response) ->
                response_result policy input.Input.format url response
            end
        end

let make ~policy ~fetch =
  Mentat_tool.make ~name ~description:Mentat_prompts.Tools.web_fetch
    ~input:Input.contract ~output:encode_output
    ~permissions:(permissions policy)
    ~run:(fun ~cancelled input -> run policy fetch input ~cancelled)
    ()
