(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Import

module Id =
  String_id.Make
    (struct
      let module_path = "Mentat_mutation.Revert.Id"
      let kind = "revert id"
    end)
    ()

let invalid fn message = invalid_arg' "Mentat_mutation.Revert" fn message

let pp_comma pp_item ppf items =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ',')
    pp_item ppf items

module Selection = struct
  type t =
    | All
    | Turns of Mentat_session.Turn.Id.t list
    | Changes of Change.Id.t list
    | Paths of Mentat_workspace.Path.t list

  let all = All

  let non_empty what wrap = function
    | [] -> invalid ("Selection." ^ what) "selection must not be empty"
    | _ :: _ as items -> wrap items

  let turns ids =
    non_empty "turns"
      (fun ids -> Turns ids)
      (List.sort_uniq Mentat_session.Turn.Id.compare ids)

  let changes ids =
    non_empty "changes"
      (fun ids -> Changes ids)
      (List.sort_uniq Change.Id.compare ids)

  let paths ps =
    non_empty "paths"
      (fun ps -> Paths ps)
      (List.sort_uniq Mentat_workspace.Path.compare ps)

  let equal a b =
    match (a, b) with
    | All, All -> true
    | Turns a, Turns b -> List.equal Mentat_session.Turn.Id.equal a b
    | Changes a, Changes b -> List.equal Change.Id.equal a b
    | Paths a, Paths b -> List.equal Mentat_workspace.Path.equal a b
    | (All | Turns _ | Changes _ | Paths _), _ -> false

  let pp ppf = function
    | All -> Format.pp_print_string ppf "all"
    | Turns ids ->
        Format.fprintf ppf "turns:%a" (pp_comma Mentat_session.Turn.Id.pp) ids
    | Changes ids -> Format.fprintf ppf "changes:%a" (pp_comma Change.Id.pp) ids
    | Paths ps ->
        Format.fprintf ppf "paths:%a" (pp_comma Mentat_workspace.Path.pp) ps

  let canonical ~what compare items =
    match items with
    | [] -> decode_error (what ^ " selection must not be empty")
    | _ :: _ ->
        if Canon.strictly_sorted compare items then items
        else decode_error (what ^ " selection must be deduplicated and sorted")

  let jsont =
    let all_case =
      Jsont.Object.map ~kind:"all selection" All
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "all" ~dec:Fun.id
    in
    let turns_case =
      Jsont.Object.map ~kind:"turns selection" (fun ids ->
          Turns (canonical ~what:"turns" Mentat_session.Turn.Id.compare ids))
      |> Jsont.Object.mem "turns" (Jsont.list Mentat_session.Turn.Id.jsont)
           ~enc:(function
           | Turns ids -> ids
           | All | Changes _ | Paths _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "turns" ~dec:Fun.id
    in
    let changes_case =
      Jsont.Object.map ~kind:"changes selection" (fun ids ->
          Changes (canonical ~what:"changes" Change.Id.compare ids))
      |> Jsont.Object.mem "changes" (Jsont.list Change.Id.jsont) ~enc:(function
        | Changes ids -> ids
        | All | Turns _ | Paths _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "changes" ~dec:Fun.id
    in
    let paths_case =
      Jsont.Object.map ~kind:"paths selection" (fun ps ->
          Paths (canonical ~what:"paths" Mentat_workspace.Path.compare ps))
      |> Jsont.Object.mem "paths" (Jsont.list Mentat_workspace.Path.jsont)
           ~enc:(function
           | Paths ps -> ps
           | All | Turns _ | Changes _ -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "paths" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make
        [ all_case; turns_case; changes_case; paths_case ]
    in
    let enc_case = function
      | All -> Jsont.Object.Case.value all_case All
      | Turns _ as selection -> Jsont.Object.Case.value turns_case selection
      | Changes _ as selection -> Jsont.Object.Case.value changes_case selection
      | Paths _ as selection -> Jsont.Object.Case.value paths_case selection
    in
    Jsont.Object.map ~kind:"selection" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Blob_map = Map.Make (Mentat_digest.Content_ref)

module Evidence = struct
  type t = {
    current : Mentat_edit.Observed.t Mentat_workspace.Path.Map.t;
    blobs : string Blob_map.t;
  }

  let make ~current ~blobs =
    let current =
      List.fold_left
        (fun map (path, observed) ->
          if Mentat_workspace.Path.Map.mem path map then
            invalid "Evidence.make" "duplicate current read for a path"
          else Mentat_workspace.Path.Map.add path observed map)
        Mentat_workspace.Path.Map.empty current
    in
    let blobs =
      List.fold_left
        (fun map (content, bytes) ->
          if not (Mentat_digest.Content_ref.matches content bytes) then
            invalid "Evidence.make" "blob bytes do not match their reference"
          else Blob_map.add content bytes map)
        Blob_map.empty blobs
    in
    { current; blobs }

  let read t path = Mentat_workspace.Path.Map.find_opt path t.current
  let find_blob t content = Blob_map.find_opt content t.blobs
end

module Override = struct
  type t = Mentat_workspace.Path.t list

  let accept_unrecorded_loss paths =
    match List.sort_uniq Mentat_workspace.Path.compare paths with
    | [] -> invalid "Override.accept_unrecorded_loss" "paths must not be empty"
    | _ :: _ as paths -> paths

  let paths t = t
  let equal = List.equal Mentat_workspace.Path.equal

  let jsont =
    Jsont.Object.map ~kind:"override" (fun paths ->
        match paths with
        | [] -> decode_error "override paths must not be empty"
        | _ :: _ ->
            if Canon.strictly_sorted Mentat_workspace.Path.compare paths then
              paths
            else decode_error "override paths must be deduplicated and sorted")
    |> Jsont.Object.mem "paths"
         (Jsont.list Mentat_workspace.Path.jsont)
         ~enc:paths
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Problem = struct
  type t =
    | Nothing_to_revert
    | Stale of {
        path : Mentat_workspace.Path.t;
        expected : Image.t;
        actual : Image.t;
      }
    | Unreadable of { path : Mentat_workspace.Path.t }
    | Missing_read of Mentat_workspace.Path.t
    | Missing_blob of {
        path : Mentat_workspace.Path.t;
        content : Mentat_digest.Content_ref.t;
      }
    | Needs_override of { path : Mentat_workspace.Path.t }
    | Unmatched_override of { path : Mentat_workspace.Path.t }
    | Superseded of { path : Mentat_workspace.Path.t; by : Change.Id.t }
    | Conflict of { path : Mentat_workspace.Path.t; merge : Textdiff.Merge.t }
    | Unresolved_revert of Id.t

  let message = function
    | Nothing_to_revert -> "nothing to revert"
    | Stale { path; expected; actual } ->
        Format.asprintf "%a: current state %a does not match recorded %a"
          Mentat_workspace.Path.pp path Image.pp actual Image.pp expected
    | Unreadable { path } ->
        Format.asprintf "%a: current target is not an editable text file"
          Mentat_workspace.Path.pp path
    | Missing_read path ->
        Format.asprintf "%a: evidence carries no current read"
          Mentat_workspace.Path.pp path
    | Missing_blob { path; content } ->
        Format.asprintf "%a: restore image bytes are missing (%a)"
          Mentat_workspace.Path.pp path Mentat_digest.Content_ref.pp content
    | Needs_override { path } ->
        Format.asprintf
          "%a: history is non-contiguous; reverting erases unrecorded edits \
           and needs an explicit override"
          Mentat_workspace.Path.pp path
    | Unmatched_override { path } ->
        Format.asprintf "%a: override names a path that is not non-contiguous"
          Mentat_workspace.Path.pp path
    | Superseded { path; by } ->
        Format.asprintf "%a: later change %a would be silently undone"
          Mentat_workspace.Path.pp path Change.Id.pp by
    | Conflict { path; merge } ->
        Format.asprintf
          "%a: reverting conflicts with a later change; resolve the markers \
           and apply the result as an ordinary edit:\n\
           %s"
          Mentat_workspace.Path.pp path
          (Textdiff.Merge.render_markers merge)
    | Unresolved_revert id ->
        Format.asprintf "revert %a started but never settled" Id.pp id

  let pp ppf t = Format.pp_print_string ppf (message t)
end

module Target = struct
  type t = {
    path : Mentat_workspace.Path.t;
    expected : Image.t;
    restore : Image.t;
    sources : Change.Id.t list;
  }

  let v ~path ~expected ~restore ~sources =
    if Image.equal expected restore then
      invalid "Target" "expected and restore images must differ";
    (match sources with
    | [] -> invalid "Target" "sources must not be empty"
    | _ :: _ -> ());
    { path; expected; restore; sources }

  let equal a b =
    Mentat_workspace.Path.equal a.path b.path
    && Image.equal a.expected b.expected
    && Image.equal a.restore b.restore
    && List.equal Change.Id.equal a.sources b.sources

  let jsont =
    Jsont.Object.map ~kind:"revert target" (fun path expected restore sources ->
        decode_invalid_arg (fun () -> v ~path ~expected ~restore ~sources))
    |> Jsont.Object.mem "path" Mentat_workspace.Path.jsont ~enc:(fun t ->
        t.path)
    |> Jsont.Object.mem "expected" Image.jsont ~enc:(fun t -> t.expected)
    |> Jsont.Object.mem "restore" Image.jsont ~enc:(fun t -> t.restore)
    |> Jsont.Object.mem "sources" (Jsont.list Change.Id.jsont) ~enc:(fun t ->
        t.sources)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Started = struct
  type t = {
    id : Id.t;
    selection : Selection.t;
    targets : Target.t list;
    override : Override.t option;
  }

  let v ~id ~selection ~targets ~override =
    (match targets with
    | [] -> invalid "Started" "targets must not be empty"
    | _ :: _ -> ());
    let target_paths =
      List.map (fun (target : Target.t) -> target.Target.path) targets
    in
    if not (Canon.unique_paths target_paths) then
      invalid "Started" "duplicate target paths";
    (match override with
    | None -> ()
    | Some paths ->
        List.iter
          (fun path ->
            if not (List.exists (Mentat_workspace.Path.equal path) target_paths)
            then invalid "Started" "override names a path outside the targets")
          (Override.paths paths));
    { id; selection; targets; override }

  let sources t =
    List.concat_map (fun (target : Target.t) -> target.Target.sources) t.targets

  let equal a b =
    Id.equal a.id b.id
    && Selection.equal a.selection b.selection
    && List.equal Target.equal a.targets b.targets
    && Option.equal Override.equal a.override b.override

  let jsont =
    Jsont.Object.map ~kind:"revert started"
      (fun id selection targets override ->
        decode_invalid_arg (fun () -> v ~id ~selection ~targets ~override))
    |> Jsont.Object.mem "id" Id.jsont ~enc:(fun t -> t.id)
    |> Jsont.Object.mem "selection" Selection.jsont ~enc:(fun t -> t.selection)
    |> Jsont.Object.mem "targets" (Jsont.list Target.jsont) ~enc:(fun t ->
        t.targets)
    |> Jsont.Object.opt_mem "override" Override.jsont ~enc:(fun t -> t.override)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Plan = struct
  type t = {
    started : Started.t;
    edit : Mentat_edit.t;
    merge_blobs : (Mentat_digest.Content_ref.t * string) list;
  }

  let v ~started ~edit ~merge_blobs = { started; edit; merge_blobs }
  let started t = t.started
  let edit t = t.edit
  let merge_blobs t = t.merge_blobs
end

module Edit_failure = struct
  type kind =
    | Invalid_text
    | Duplicate_path
    | State_mismatch
    | Conflict
    | Too_large
    | Invalid_target
    | Workspace
    | Out_of_workspace
    | Read_only_path
    | Protected_path
    | Io

  type t = {
    kind : kind;
    path : Mentat_workspace.Path.t option;
    message : string;
  }

  let v ~kind ~path ~message =
    if String.is_empty message then
      invalid "Edit_failure" "message must not be empty";
    { kind; path; message }

  let of_error (error : Mentat_edit.Error.t) =
    let kind =
      match error with
      | Mentat_edit.Error.Invalid_text _ -> Invalid_text
      | Mentat_edit.Error.Duplicate_path _ -> Duplicate_path
      | Mentat_edit.Error.State_mismatch _ -> State_mismatch
      | Mentat_edit.Error.Conflict _ -> Conflict
      | Mentat_edit.Error.Too_large _ -> Too_large
      | Mentat_edit.Error.Invalid_target _ -> Invalid_target
      | Mentat_edit.Error.Workspace _ -> Workspace
      | Mentat_edit.Error.Out_of_workspace _ -> Out_of_workspace
      | Mentat_edit.Error.Read_only_path _ -> Read_only_path
      | Mentat_edit.Error.Protected_path _ -> Protected_path
      | Mentat_edit.Error.Io _ -> Io
    in
    v ~kind
      ~path:(Mentat_edit.Error.path error)
      ~message:(Mentat_edit.Error.message error)

  let kind t = t.kind
  let path t = t.path
  let message t = t.message

  let kind_string = function
    | Invalid_text -> "invalid_text"
    | Duplicate_path -> "duplicate_path"
    | State_mismatch -> "state_mismatch"
    | Conflict -> "conflict"
    | Too_large -> "too_large"
    | Invalid_target -> "invalid_target"
    | Workspace -> "workspace"
    | Out_of_workspace -> "out_of_workspace"
    | Read_only_path -> "read_only_path"
    | Protected_path -> "protected_path"
    | Io -> "io"

  let kind_jsont =
    Jsont.map ~kind:"edit failure kind"
      ~dec:(function
        | "invalid_text" -> Invalid_text
        | "duplicate_path" -> Duplicate_path
        | "state_mismatch" -> State_mismatch
        | "conflict" -> Conflict
        | "too_large" -> Too_large
        | "invalid_target" -> Invalid_target
        | "workspace" -> Workspace
        | "out_of_workspace" -> Out_of_workspace
        | "read_only_path" -> Read_only_path
        | "protected_path" -> Protected_path
        | "io" -> Io
        | other -> decode_error ("unknown edit failure kind: " ^ other))
      ~enc:kind_string Jsont.string

  let equal a b =
    String.equal (kind_string a.kind) (kind_string b.kind)
    && Option.equal Mentat_workspace.Path.equal a.path b.path
    && String.equal a.message b.message

  let pp ppf t =
    match t.path with
    | Some path ->
        Format.fprintf ppf "%s(%a)" (kind_string t.kind)
          Mentat_workspace.Path.pp path
    | None -> Format.pp_print_string ppf (kind_string t.kind)

  let jsont =
    Jsont.Object.map ~kind:"edit failure" (fun kind path message ->
        decode_invalid_arg (fun () -> v ~kind ~path ~message))
    |> Jsont.Object.mem "kind" kind_jsont ~enc:(fun t -> t.kind)
    |> Jsont.Object.opt_mem "path" Mentat_workspace.Path.jsont ~enc:(fun t ->
        t.path)
    |> Jsont.Object.mem "message" Jsont.string ~enc:(fun t -> t.message)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

module Settled = struct
  type outcome = Confirmed of Change.Id.t | Ambiguous | Not_attempted

  module Failure = struct
    type phase = Preflight | Commit of { target : Mentat_workspace.Path.t }
    type t = { phase : phase; error : Edit_failure.t }

    let v ~phase ~error = { phase; error }
  end

  type disposition = Applied of { failure : Failure.t option } | Recovered

  type t = {
    revert : Id.t;
    outcomes : (Mentat_workspace.Path.t * outcome) list;
    changes : Change.t list;
    disposition : disposition;
  }

  let v ~revert ~outcomes ~changes ~disposition =
    (match outcomes with
    | [] -> invalid "Settled" "outcomes must not be empty"
    | _ :: _ -> ());
    if not (Canon.unique_paths (List.map fst outcomes)) then
      invalid "Settled" "duplicate outcome paths";
    if not (Canon.unique_paths (List.map Change.path changes)) then
      invalid "Settled" "duplicate restoration row paths";
    let derived path =
      Change.Id.of_string
        (Change_id.for_revert ~revert:(Id.to_string revert) path)
    in
    List.iter
      (fun change ->
        if
          not
            (Change.Id.equal (derived (Change.path change)) (Change.id change))
        then
          invalid "Settled"
            "restoration row id does not derive from the revert and path")
      changes;
    List.iter
      (fun (path, outcome) ->
        match outcome with
        | Confirmed id ->
            if not (Change.Id.equal id (derived path)) then
              invalid "Settled"
                "confirmed outcome id does not derive from the revert and path"
        | Ambiguous | Not_attempted -> ())
      outcomes;
    let confirmed =
      List.filter_map
        (fun (_, outcome) ->
          match outcome with
          | Confirmed id -> Some id
          | Ambiguous | Not_attempted -> None)
        outcomes
    in
    let rows = List.map Change.id changes in
    if
      not
        (List.equal Change.Id.equal
           (List.sort Change.Id.compare confirmed)
           (List.sort Change.Id.compare rows))
    then
      invalid "Settled"
        "confirmed outcomes and restoration rows do not correspond";
    (* Disposition/outcome consistency — [Revert.settle] and
       [settle_ambiguous] mint exactly these shapes, and decode admits no
       other. Row emptiness for the recovered and preflight cases is already
       forced: neither admits a confirmed outcome, and rows correspond to
       confirmed outcomes. *)
    (match disposition with
    | Recovered ->
        List.iter
          (fun (_, outcome) ->
            match outcome with
            | Ambiguous -> ()
            | Confirmed _ | Not_attempted ->
                invalid "Settled"
                  "a recovered settlement admits only ambiguous outcomes")
          outcomes
    | Applied { failure = None } ->
        List.iter
          (fun (_, outcome) ->
            match outcome with
            | Confirmed _ -> ()
            | Ambiguous | Not_attempted ->
                invalid "Settled" "a clean apply admits only confirmed outcomes")
          outcomes
    | Applied { failure = Some { Failure.phase = Failure.Preflight; _ } } ->
        List.iter
          (fun (_, outcome) ->
            match outcome with
            | Not_attempted -> ()
            | Confirmed _ | Ambiguous ->
                invalid "Settled"
                  "a preflight failure admits only not-attempted outcomes")
          outcomes
    | Applied
        { failure = Some { Failure.phase = Failure.Commit { target }; _ } } ->
        (* The Phase contract's shape: a confirmed prefix, exactly one
           ambiguous outcome at the stopping target, a not-attempted
           suffix. *)
        let rec scan = function
          | (_, Confirmed _) :: rest -> scan rest
          | (path, Ambiguous) :: rest ->
              if not (Mentat_workspace.Path.equal path target) then
                invalid "Settled"
                  "the ambiguous outcome must sit at the commit stopping target";
              List.iter
                (fun (_, outcome) ->
                  match outcome with
                  | Not_attempted -> ()
                  | Confirmed _ | Ambiguous ->
                      invalid "Settled"
                        "a commit failure admits only not-attempted outcomes \
                         after the stopping target")
                rest
          | (_, Not_attempted) :: _ | [] ->
              invalid "Settled"
                "a commit failure requires the ambiguous outcome at the \
                 stopping target after the confirmed prefix"
        in
        scan outcomes);
    { revert; outcomes; changes; disposition }

  let outcome_equal a b =
    match (a, b) with
    | Confirmed a, Confirmed b -> Change.Id.equal a b
    | Ambiguous, Ambiguous | Not_attempted, Not_attempted -> true
    | (Confirmed _ | Ambiguous | Not_attempted), _ -> false

  let phase_equal a b =
    match (a, b) with
    | Failure.Preflight, Failure.Preflight -> true
    | Failure.Commit { target = a }, Failure.Commit { target = b } ->
        Mentat_workspace.Path.equal a b
    | (Failure.Preflight | Failure.Commit _), _ -> false

  let failure_equal (a : Failure.t) (b : Failure.t) =
    phase_equal a.Failure.phase b.Failure.phase
    && Edit_failure.equal a.Failure.error b.Failure.error

  let disposition_equal a b =
    match (a, b) with
    | Applied { failure = a }, Applied { failure = b } ->
        Option.equal failure_equal a b
    | Recovered, Recovered -> true
    | (Applied _ | Recovered), _ -> false

  let revert t = t.revert
  let changes t = t.changes

  let equal a b =
    Id.equal a.revert b.revert
    && List.equal
         (fun (pa, oa) (pb, ob) ->
           Mentat_workspace.Path.equal pa pb && outcome_equal oa ob)
         a.outcomes b.outcomes
    && List.equal Change.equal a.changes b.changes
    && disposition_equal a.disposition b.disposition

  let outcome_jsont =
    let confirmed_case =
      Jsont.Object.map ~kind:"confirmed outcome" (fun id -> Confirmed id)
      |> Jsont.Object.mem "change" Change.Id.jsont ~enc:(function
        | Confirmed id -> id
        | Ambiguous | Not_attempted -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "confirmed" ~dec:Fun.id
    in
    let ambiguous_case =
      Jsont.Object.map ~kind:"ambiguous outcome" Ambiguous
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "ambiguous" ~dec:Fun.id
    in
    let not_attempted_case =
      Jsont.Object.map ~kind:"not attempted outcome" Not_attempted
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "not_attempted" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make
        [ confirmed_case; ambiguous_case; not_attempted_case ]
    in
    let enc_case = function
      | Confirmed _ as outcome -> Jsont.Object.Case.value confirmed_case outcome
      | Ambiguous -> Jsont.Object.Case.value ambiguous_case Ambiguous
      | Not_attempted ->
          Jsont.Object.Case.value not_attempted_case Not_attempted
    in
    Jsont.Object.map ~kind:"outcome" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let outcome_pair_jsont =
    Jsont.Object.map ~kind:"path outcome" (fun path outcome -> (path, outcome))
    |> Jsont.Object.mem "path" Mentat_workspace.Path.jsont ~enc:fst
    |> Jsont.Object.mem "outcome" outcome_jsont ~enc:snd
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let phase_jsont =
    let preflight_case =
      Jsont.Object.map ~kind:"preflight phase" Failure.Preflight
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "preflight" ~dec:Fun.id
    in
    let commit_case =
      Jsont.Object.map ~kind:"commit phase" (fun target ->
          Failure.Commit { target })
      |> Jsont.Object.mem "target" Mentat_workspace.Path.jsont ~enc:(function
        | Failure.Commit { target } -> target
        | Failure.Preflight -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "commit" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make [ preflight_case; commit_case ]
    in
    let enc_case = function
      | Failure.Preflight ->
          Jsont.Object.Case.value preflight_case Failure.Preflight
      | Failure.Commit _ as phase -> Jsont.Object.Case.value commit_case phase
    in
    Jsont.Object.map ~kind:"stopping phase" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let failure_jsont =
    Jsont.Object.map ~kind:"stopping failure" (fun phase error ->
        { Failure.phase; error })
    |> Jsont.Object.mem "phase" phase_jsont ~enc:(fun (f : Failure.t) ->
        f.Failure.phase)
    |> Jsont.Object.mem "error" Edit_failure.jsont ~enc:(fun (f : Failure.t) ->
        f.Failure.error)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let disposition_jsont =
    let applied_case =
      Jsont.Object.map ~kind:"applied disposition" (fun failure ->
          Applied { failure })
      |> Jsont.Object.opt_mem "failure" failure_jsont ~enc:(function
        | Applied { failure } -> failure
        | Recovered -> assert false)
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "applied" ~dec:Fun.id
    in
    let recovered_case =
      Jsont.Object.map ~kind:"recovered disposition" Recovered
      |> Jsont.Object.error_unknown |> Jsont.Object.finish
      |> Jsont.Object.Case.map "recovered" ~dec:Fun.id
    in
    let cases =
      List.map Jsont.Object.Case.make [ applied_case; recovered_case ]
    in
    let enc_case = function
      | Applied _ as disposition ->
          Jsont.Object.Case.value applied_case disposition
      | Recovered -> Jsont.Object.Case.value recovered_case Recovered
    in
    Jsont.Object.map ~kind:"disposition" Fun.id
    |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
    |> Jsont.Object.error_unknown |> Jsont.Object.finish

  let jsont =
    Jsont.Object.map ~kind:"revert settled"
      (fun revert outcomes changes disposition ->
        decode_invalid_arg (fun () -> v ~revert ~outcomes ~changes ~disposition))
    |> Jsont.Object.mem "revert" Id.jsont ~enc:(fun t -> t.revert)
    |> Jsont.Object.mem "outcomes" (Jsont.list outcome_pair_jsont)
         ~enc:(fun t -> t.outcomes)
    |> Jsont.Object.mem "changes" (Jsont.list Change.jsont) ~enc:(fun t ->
        t.changes)
    |> Jsont.Object.mem "disposition" disposition_jsont ~enc:(fun t ->
        t.disposition)
    |> Jsont.Object.error_unknown |> Jsont.Object.finish
end

(* Top-level seams for [Revert]'s operations. They live outside the submodules
   so [Revert]'s module aliases do not re-export them: the public surface
   keeps only the smart constructors, while the operations reach the checked
   internals. *)
let target = Target.v
let started = Started.v
let settled = Settled.v
let stopping_failure = Settled.Failure.v
let plan = Plan.v
let evidence_read = Evidence.read
let evidence_find_blob = Evidence.find_blob
let override_paths = Override.paths
