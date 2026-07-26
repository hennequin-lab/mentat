(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let invalid fn message =
  invalid_arg ("Mentat_provider.Model." ^ fn ^ ": " ^ message)

let decode_error message = Jsont.Error.msg Jsont.Meta.none message

let decode_invalid_arg f =
  match f () with
  | value -> value
  | exception Invalid_argument message -> decode_error message

let check_optional_non_empty fn field = function
  | None -> ()
  | Some value ->
      if String.is_empty value then invalid fn (field ^ " must not be empty")

let check_positive_option fn field = function
  | None -> ()
  | Some value -> if value <= 0 then invalid fn (field ^ " must be positive")

let check_sorted_no_duplicates fn field compare values =
  let rec loop = function
    | first :: second :: rest ->
        if compare first second = 0 then
          invalid fn (field ^ " contain duplicates");
        loop (second :: rest)
    | [] | [ _ ] -> ()
  in
  loop values

let sort_unique fn field compare values =
  let sorted = List.sort compare values in
  check_sorted_no_duplicates fn field compare sorted;
  sorted

let check_no_duplicates fn field compare values =
  check_sorted_no_duplicates fn field compare (List.sort compare values)

(* Modality and capability tags share one grammar: a lowercase ASCII letter
   followed by lowercase letters, digits, '-', or '_'. *)
module Tag = struct
  let is_first c = Char.Ascii.is_lower c

  let is_rest c =
    Char.Ascii.is_lower c || Char.Ascii.is_digit c || Char.equal c '-'
    || Char.equal c '_'

  let valid tag =
    let len = String.length tag in
    let rec loop index =
      index = len || (is_rest (String.unsafe_get tag index) && loop (index + 1))
    in
    len > 0 && is_first (String.unsafe_get tag 0) && loop 1

  let check fn kind tag =
    if not (valid tag) then invalid fn (kind ^ " tag is invalid")

  let reserved names tag = List.exists (String.equal tag) names
end

module Date = struct
  type t = { year : int; month : int; day : int }

  let is_leap_year year =
    year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0)

  let days_in_month year = function
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    | 4 | 6 | 9 | 11 -> 30
    | 2 -> if is_leap_year year then 29 else 28
    | _ -> 0

  let make ~year ~month ~day =
    if year < 1 || year > 9999 then
      invalid "Date.make" "year must be between 1 and 9999";
    if month < 1 || month > 12 then
      invalid "Date.make" "month must be between 1 and 12";
    let max_day = days_in_month year month in
    if day < 1 || day > max_day then
      invalid "Date.make" "day is invalid for month";
    { year; month; day }

  let digit s index = Char.Ascii.digit_to_int (String.unsafe_get s index)

  let all_digits s indexes =
    List.for_all
      (fun index -> Char.Ascii.is_digit (String.unsafe_get s index))
      indexes

  let of_string s =
    if
      String.length s = 10
      && Char.equal (String.unsafe_get s 4) '-'
      && Char.equal (String.unsafe_get s 7) '-'
      && all_digits s [ 0; 1; 2; 3; 5; 6; 8; 9 ]
    then
      let year =
        (digit s 0 * 1000) + (digit s 1 * 100) + (digit s 2 * 10) + digit s 3
      in
      let month = (digit s 5 * 10) + digit s 6 in
      let day = (digit s 8 * 10) + digit s 9 in
      match make ~year ~month ~day with
      | date -> Some date
      | exception Invalid_argument _ -> None
    else None

  let to_string t = Printf.sprintf "%04d-%02d-%02d" t.year t.month t.day
  let equal a b = a.year = b.year && a.month = b.month && a.day = b.day

  let compare a b =
    match Int.compare a.year b.year with
    | 0 -> (
        match Int.compare a.month b.month with
        | 0 -> Int.compare a.day b.day
        | order -> order)
    | order -> order

  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.map ~kind:"date"
      ~dec:(fun s ->
        match of_string s with
        | Some date -> date
        | None -> decode_error ("invalid date: " ^ s))
      ~enc:to_string Jsont.string
end

module Month = struct
  type t = { year : int; month : int }

  let make ~year ~month =
    if year < 1 || year > 9999 then
      invalid "Month.make" "year must be between 1 and 9999";
    if month < 1 || month > 12 then
      invalid "Month.make" "month must be between 1 and 12";
    { year; month }

  let digit s index = Char.Ascii.digit_to_int (String.unsafe_get s index)

  let all_digits s indexes =
    List.for_all
      (fun index -> Char.Ascii.is_digit (String.unsafe_get s index))
      indexes

  let of_string s =
    if
      String.length s = 7
      && Char.equal (String.unsafe_get s 4) '-'
      && all_digits s [ 0; 1; 2; 3; 5; 6 ]
    then
      let year =
        (digit s 0 * 1000) + (digit s 1 * 100) + (digit s 2 * 10) + digit s 3
      in
      let month = (digit s 5 * 10) + digit s 6 in
      match make ~year ~month with
      | value -> Some value
      | exception Invalid_argument _ -> None
    else None

  let to_string t = Printf.sprintf "%04d-%02d" t.year t.month
  let equal a b = a.year = b.year && a.month = b.month

  let compare a b =
    match Int.compare a.year b.year with
    | 0 -> Int.compare a.month b.month
    | order -> order

  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.map ~kind:"month"
      ~dec:(fun s ->
        match of_string s with
        | Some month -> month
        | None -> decode_error ("invalid month: " ^ s))
      ~enc:to_string Jsont.string
end

module Modality = struct
  type t = string

  let text = "text"
  let image = "image"
  let audio = "audio"
  let video = "video"
  let pdf = "pdf"
  let reserved = [ audio; image; pdf; text; video ]

  let extension tag =
    Tag.check "Modality.extension" "modality" tag;
    if Tag.reserved reserved tag then
      invalid "Modality.extension" "tag is reserved";
    tag

  let to_string t = t

  let of_string = function
    | "audio" -> Some audio
    | "image" -> Some image
    | "pdf" -> Some pdf
    | "text" -> Some text
    | "video" -> Some video
    | tag -> if Tag.valid tag then Some tag else None

  let equal = String.equal
  let compare = String.compare
  let pp = Format.pp_print_string

  let jsont =
    Jsont.map ~kind:"modality"
      ~dec:(fun s ->
        match of_string s with
        | Some modality -> modality
        | None -> decode_error ("invalid modality: " ^ s))
      ~enc:to_string Jsont.string
end

module Capability = struct
  type t = string

  let tools = "tools"
  let reasoning = "reasoning"
  let json_schema = "json_schema"
  let reserved = [ json_schema; reasoning; tools ]

  let extension tag =
    Tag.check "Capability.extension" "capability" tag;
    if Tag.reserved reserved tag then
      invalid "Capability.extension" "tag is reserved";
    tag

  let to_string t = t

  let of_string = function
    | "json_schema" -> Some json_schema
    | "reasoning" -> Some reasoning
    | "tools" -> Some tools
    | tag -> if Tag.valid tag then Some tag else None

  let equal = String.equal
  let compare = String.compare
  let pp = Format.pp_print_string

  let jsont =
    Jsont.map ~kind:"capability"
      ~dec:(fun s ->
        match of_string s with
        | Some capability -> capability
        | None -> decode_error ("invalid capability: " ^ s))
      ~enc:to_string Jsont.string
end

module Pricing = struct
  module Rate = struct
    type t = {
      input_per_million : float option;
      cached_input_per_million : float option;
      output_per_million : float option;
      cache_write_5m_per_million : float option;
      cache_write_1h_per_million : float option;
    }

    let check field = function
      | None -> ()
      | Some value ->
          if (not (Float.is_finite value)) || value < 0. then
            invalid "Pricing.Rate.make"
              (field ^ " must be finite and non-negative")

    let make ?input_per_million ?cached_input_per_million ?output_per_million
        ?cache_write_5m_per_million ?cache_write_1h_per_million () =
      check "input_per_million" input_per_million;
      check "cached_input_per_million" cached_input_per_million;
      check "output_per_million" output_per_million;
      check "cache_write_5m_per_million" cache_write_5m_per_million;
      check "cache_write_1h_per_million" cache_write_1h_per_million;
      {
        input_per_million;
        cached_input_per_million;
        output_per_million;
        cache_write_5m_per_million;
        cache_write_1h_per_million;
      }

    let input_per_million t = t.input_per_million
    let cached_input_per_million t = t.cached_input_per_million
    let output_per_million t = t.output_per_million
    let cache_write_5m_per_million t = t.cache_write_5m_per_million
    let cache_write_1h_per_million t = t.cache_write_1h_per_million

    let equal a b =
      let eq = Option.equal Float.equal in
      eq a.input_per_million b.input_per_million
      && eq a.cached_input_per_million b.cached_input_per_million
      && eq a.output_per_million b.output_per_million
      && eq a.cache_write_5m_per_million b.cache_write_5m_per_million
      && eq a.cache_write_1h_per_million b.cache_write_1h_per_million

    let pp ppf t =
      let pp_rate ppf = function
        | None -> Format.pp_print_string ppf "-"
        | Some rate -> Format.fprintf ppf "%g" rate
      in
      Format.fprintf ppf "@[<2>{in=%a; cached=%a; out=%a; cw5m=%a; cw1h=%a}@]"
        pp_rate t.input_per_million pp_rate t.cached_input_per_million pp_rate
        t.output_per_million pp_rate t.cache_write_5m_per_million pp_rate
        t.cache_write_1h_per_million

    let jsont =
      let dec input cached output cw5 cw1 =
        decode_invalid_arg (fun () ->
            make ?input_per_million:input ?cached_input_per_million:cached
              ?output_per_million:output ?cache_write_5m_per_million:cw5
              ?cache_write_1h_per_million:cw1 ())
      in
      Jsont.Object.map ~kind:"pricing rate" dec
      |> Jsont.Object.opt_mem "input_per_million" Jsont.number
           ~enc:input_per_million
      |> Jsont.Object.opt_mem "cached_input_per_million" Jsont.number
           ~enc:cached_input_per_million
      |> Jsont.Object.opt_mem "output_per_million" Jsont.number
           ~enc:output_per_million
      |> Jsont.Object.opt_mem "cache_write_5m_per_million" Jsont.number
           ~enc:cache_write_5m_per_million
      |> Jsont.Object.opt_mem "cache_write_1h_per_million" Jsont.number
           ~enc:cache_write_1h_per_million
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
  end

  type t = { default : Rate.t; context_over : (int * Rate.t) list }

  let make ?(context_over = []) default =
    List.iter
      (fun (threshold, _) ->
        if threshold < 0 then
          invalid "Pricing.make" "context_over thresholds must be non-negative")
      context_over;
    let context_over =
      List.sort
        (fun (left, _) (right, _) -> Int.compare left right)
        context_over
    in
    check_no_duplicates "Pricing.make" "context_over thresholds" Int.compare
      (List.map fst context_over);
    { default; context_over }

  let default t = t.default
  let context_over t = t.context_over

  let rate_for ?context_tokens t =
    match context_tokens with
    | Some tokens when tokens < 0 ->
        invalid "Pricing.rate_for" "context_tokens must be non-negative"
    | None -> t.default
    | Some tokens -> (
        let selected =
          List.fold_left
            (fun selected (threshold, rate) ->
              if tokens > threshold then
                match selected with
                | Some (best, _) when best >= threshold -> selected
                | Some _ | None -> Some (threshold, rate)
              else selected)
            None t.context_over
        in
        match selected with None -> t.default | Some (_, rate) -> rate)

  let equal a b =
    Rate.equal a.default b.default
    && List.equal
         (fun (ta, ra) (tb, rb) -> Int.equal ta tb && Rate.equal ra rb)
         a.context_over b.context_over

  let pp ppf t =
    let pp_tier ppf (threshold, rate) =
      Format.fprintf ppf "@[>%d:%a@]" threshold Rate.pp rate
    in
    Format.fprintf ppf "@[<2>{default=%a; tiers=[%a]}@]" Rate.pp t.default
      Format.(
        pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf "; ") pp_tier)
      t.context_over

  let tier_jsont =
    let dec threshold rate = (threshold, rate) in
    Jsont.Object.map ~kind:"pricing tier" dec
    |> Jsont.Object.mem "threshold" Jsont.int ~enc:fst
    |> Jsont.Object.mem "rate" Rate.jsont ~enc:snd
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let jsont =
    let dec default context_over =
      decode_invalid_arg (fun () -> make ~context_over default)
    in
    Jsont.Object.map ~kind:"pricing" dec
    |> Jsont.Object.mem "default" Rate.jsont ~enc:default
    |> Jsont.Object.mem "context_over" (Jsont.list tier_jsont) ~dec_absent:[]
         ~enc:context_over ~enc_omit:(function
         | [] -> true
         | _ :: _ -> false)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

type status = Stable | Preview | Deprecated | Unavailable of string

type t = {
  llm : Mentat_llm.Model.t;
  display_name : string option;
  family : string option;
  released_on : Date.t option;
  knowledge_cutoff : Month.t option;
  context_window : int option;
  max_output_tokens : int option;
  default_reasoning : Mentat_llm.Request.Options.Reasoning_effort.t option;
  supported_reasoning : Mentat_llm.Request.Options.Reasoning_effort.t list;
  input_modalities : Modality.t list;
  output_modalities : Modality.t list;
  capabilities : Capability.t list;
  pricing : Pricing.t option;
  status : status;
}

let reasoning_rank effort =
  let open Mentat_llm.Request.Options.Reasoning_effort in
  match effort with
  | Disabled -> 0
  | Minimal -> 1
  | Low -> 2
  | Medium -> 3
  | High -> 4
  | Extra_high -> 5
  | Max -> 6

let compare_reasoning a b = Int.compare (reasoning_rank a) (reasoning_rank b)

let check_default_reasoning supported_reasoning = function
  | None -> ()
  | Some effort ->
      if
        (not (List.is_empty supported_reasoning))
        && not
             (List.exists
                (fun supported -> compare_reasoning effort supported = 0)
                supported_reasoning)
      then invalid "make" "default_reasoning is not supported"

let make llm ?display_name ?family ?released_on ?knowledge_cutoff
    ?context_window ?max_output_tokens ?default_reasoning ?supported_reasoning
    ?input_modalities ?output_modalities ?capabilities ?pricing
    ?(status = Stable) () =
  check_optional_non_empty "make" "display_name" display_name;
  check_optional_non_empty "make" "family" family;
  check_positive_option "make" "context_window" context_window;
  check_positive_option "make" "max_output_tokens" max_output_tokens;
  let supported_reasoning =
    supported_reasoning |> Option.value ~default:[]
    |> sort_unique "make" "supported_reasoning" compare_reasoning
  in
  check_default_reasoning supported_reasoning default_reasoning;
  let input_modalities =
    input_modalities
    |> Option.value ~default:[ Modality.text ]
    |> sort_unique "make" "input_modalities" Modality.compare
  in
  let output_modalities =
    output_modalities
    |> Option.value ~default:[ Modality.text ]
    |> sort_unique "make" "output_modalities" Modality.compare
  in
  let capabilities =
    capabilities |> Option.value ~default:[]
    |> sort_unique "make" "capabilities" Capability.compare
  in
  {
    llm;
    display_name;
    family;
    released_on;
    knowledge_cutoff;
    context_window;
    max_output_tokens;
    default_reasoning;
    supported_reasoning;
    input_modalities;
    output_modalities;
    capabilities;
    pricing;
    status;
  }

let llm t = t.llm
let provider t = Mentat_llm.Model.provider t.llm
let api t = Mentat_llm.Model.api t.llm
let id t = Mentat_llm.Model.id t.llm
let selector t = Selector.of_model t.llm
let display_name t = t.display_name
let family t = t.family
let released_on t = t.released_on
let knowledge_cutoff t = t.knowledge_cutoff
let context_window t = t.context_window
let max_output_tokens t = t.max_output_tokens
let default_reasoning t = t.default_reasoning
let supported_reasoning t = t.supported_reasoning
let input_modalities t = t.input_modalities
let output_modalities t = t.output_modalities

let has_input_modality modality t =
  List.exists (Modality.equal modality) t.input_modalities

let capabilities t = t.capabilities

let has_capability capability t =
  List.exists (Capability.equal capability) t.capabilities

let pricing t = t.pricing

let cost_context_tokens usage =
  match Mentat_llm.Usage.input_total usage with
  | tokens -> tokens
  | exception Invalid_argument _ -> max_int

let cost t usage =
  match t.pricing with
  | None -> None
  | Some pricing ->
      let rate =
        Pricing.rate_for ~context_tokens:(cost_context_tokens usage) pricing
      in
      (* Each disjoint usage lane bills against its rate. A lane that spent
         nothing costs nothing whatever its rate; a spent lane with an unknown
         rate makes the total unknown. *)
      let lanes =
        [
          (usage.Mentat_llm.Usage.input, Pricing.Rate.input_per_million rate);
          ( usage.Mentat_llm.Usage.cache_read,
            Pricing.Rate.cached_input_per_million rate );
          ( usage.Mentat_llm.Usage.cache_write,
            Pricing.Rate.cache_write_5m_per_million rate );
          (usage.Mentat_llm.Usage.output, Pricing.Rate.output_per_million rate);
          ( usage.Mentat_llm.Usage.reasoning,
            Pricing.Rate.output_per_million rate );
        ]
      in
      List.fold_left
        (fun acc (tokens, rate) ->
          match acc with
          | None -> None
          | Some total -> (
              if tokens = 0 then Some total
              else
                match rate with
                | None -> None
                | Some per_million ->
                    Some
                      (total
                      +. (float_of_int tokens /. 1_000_000. *. per_million))))
        (Some 0.) lanes

(* The base-tier input and output rates, the fresh-input and output lanes of
   {!Pricing.default}. This is the tier {!cost} bills a small request against,
   so a single-lane cost recovers the rate reported here. *)
let rate_per_million t =
  match t.pricing with
  | None -> (None, None)
  | Some pricing ->
      let rate = Pricing.default pricing in
      (Pricing.Rate.input_per_million rate, Pricing.Rate.output_per_million rate)

let status t = t.status

let visible t =
  match t.status with
  | Stable | Preview | Deprecated -> true
  | Unavailable _ -> false

let selectable t =
  match t.status with
  | Stable | Preview -> true
  | Deprecated | Unavailable _ -> false

let equal a b =
  Mentat_llm.Model.equal a.llm b.llm
  && Option.equal String.equal a.display_name b.display_name
  && Option.equal String.equal a.family b.family
  && Option.equal Date.equal a.released_on b.released_on
  && Option.equal Month.equal a.knowledge_cutoff b.knowledge_cutoff
  && Option.equal Int.equal a.context_window b.context_window
  && Option.equal Int.equal a.max_output_tokens b.max_output_tokens
  && Option.equal
       (fun x y -> compare_reasoning x y = 0)
       a.default_reasoning b.default_reasoning
  && List.equal
       (fun x y -> compare_reasoning x y = 0)
       a.supported_reasoning b.supported_reasoning
  && List.equal Modality.equal a.input_modalities b.input_modalities
  && List.equal Modality.equal a.output_modalities b.output_modalities
  && List.equal Capability.equal a.capabilities b.capabilities
  && Option.equal Pricing.equal a.pricing b.pricing
  &&
  match (a.status, b.status) with
  | Stable, Stable | Preview, Preview | Deprecated, Deprecated -> true
  | Unavailable a, Unavailable b -> String.equal a b
  | (Stable | Preview | Deprecated | Unavailable _), _ -> false

let pp_status ppf = function
  | Stable -> Format.pp_print_string ppf "stable"
  | Preview -> Format.pp_print_string ppf "preview"
  | Deprecated -> Format.pp_print_string ppf "deprecated"
  | Unavailable reason -> Format.fprintf ppf "unavailable(%s)" reason

let pp ppf t =
  Format.fprintf ppf "%a[%a]" Mentat_llm.Model.pp t.llm pp_status t.status

let status_tag = function
  | Stable -> "stable"
  | Preview -> "preview"
  | Deprecated -> "deprecated"
  | Unavailable _ -> "unavailable"

let status_reason = function
  | Unavailable reason -> Some reason
  | Stable | Preview | Deprecated -> None

let status_of tag reason =
  match (tag, reason) with
  | "stable", None -> Stable
  | "preview", None -> Preview
  | "deprecated", None -> Deprecated
  | "unavailable", Some reason -> Unavailable reason
  | "unavailable", None ->
      decode_error "unavailable model status requires a reason"
  | ("stable" | "preview" | "deprecated"), Some _ ->
      decode_error "model status reason is only valid for unavailable"
  | tag, _ -> decode_error ("unknown model status: " ^ tag)

let jsont =
  let dec llm display_name family released_on knowledge_cutoff context_window
      max_output_tokens default_reasoning supported_reasoning input_modalities
      output_modalities capabilities pricing tag reason =
    let status = status_of tag reason in
    decode_invalid_arg (fun () ->
        make llm ?display_name ?family ?released_on ?knowledge_cutoff
          ?context_window ?max_output_tokens ?default_reasoning
          ~supported_reasoning ~input_modalities ~output_modalities
          ~capabilities ?pricing ~status ())
  in
  let effort = Mentat_llm.Request.Options.Reasoning_effort.jsont in
  Jsont.Object.map ~kind:"model" dec
  |> Jsont.Object.mem "model" Mentat_llm.Model.jsont ~enc:llm
  |> Jsont.Object.opt_mem "display_name" Jsont.string ~enc:display_name
  |> Jsont.Object.opt_mem "family" Jsont.string ~enc:family
  |> Jsont.Object.opt_mem "released_on" Date.jsont ~enc:released_on
  |> Jsont.Object.opt_mem "knowledge_cutoff" Month.jsont ~enc:knowledge_cutoff
  |> Jsont.Object.opt_mem "context_window" Jsont.int ~enc:context_window
  |> Jsont.Object.opt_mem "max_output_tokens" Jsont.int ~enc:max_output_tokens
  |> Jsont.Object.opt_mem "default_reasoning" effort ~enc:default_reasoning
  |> Jsont.Object.mem "supported_reasoning" (Jsont.list effort) ~dec_absent:[]
       ~enc:supported_reasoning ~enc_omit:(function
       | [] -> true
       | _ :: _ -> false)
  |> Jsont.Object.mem "input_modalities"
       (Jsont.list Modality.jsont)
       ~dec_absent:[ Modality.text ] ~enc:input_modalities
  |> Jsont.Object.mem "output_modalities"
       (Jsont.list Modality.jsont)
       ~dec_absent:[ Modality.text ] ~enc:output_modalities
  |> Jsont.Object.mem "capabilities" (Jsont.list Capability.jsont)
       ~dec_absent:[] ~enc:capabilities ~enc_omit:(function
       | [] -> true
       | _ :: _ -> false)
  |> Jsont.Object.opt_mem "pricing" Pricing.jsont ~enc:pricing
  |> Jsont.Object.mem "status" Jsont.string ~dec_absent:"stable" ~enc:(fun t ->
      status_tag t.status)
  |> Jsont.Object.opt_mem "status_reason" Jsont.string ~enc:(fun t ->
      status_reason t.status)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish
