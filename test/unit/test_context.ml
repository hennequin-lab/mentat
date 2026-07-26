(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Windtrap
open Test_support
module C = Mentat_context.Context

(* Context discovery is a filesystem effect, so these exercise the pure cores —
   budget truncation, shadowing precedence, projection identity, trust gating —
   through [load] over a real temporary workspace, the same way the workspace-io
   suite tests its observations. *)
module Context_tests = struct
  module Abs = Lpath.Abs
  module Source = C.Source

  let user_config_file = Abs.of_string_exn "/mentat-no-such-home/config.json"
  let user_path = user_config_file

  let set field text doc =
    match Mentat_config.set_text field text doc with
    | Ok doc -> doc
    | Error e -> failf "config set: %s" (Mentat_config.Error.message e)

  let resolved settings =
    let user_doc =
      List.fold_left (fun doc apply -> apply doc) Mentat_config.empty settings
    in
    match
      Mentat_config.resolve
        ~env:(fun _ -> None)
        ~user:(user_path, user_doc) ~extra:None
        ~workspace_config:
          (Mentat_config.Disabled
             { status = "test"; project = None; project_local = None })
        ~overrides:[]
    with
    | Ok r -> r
    | Error e -> failf "resolve: %s" (Mentat_config.Error.message e)

  let rec mkdir_p dir =
    if not (Sys.file_exists dir) then begin
      mkdir_p (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end

  let write_file base rel contents =
    let path = Filename.concat base rel in
    mkdir_p (Filename.dirname path);
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc contents)

  let rec rm_rf path =
    match Sys.is_directory path with
    | true -> (
        Array.iter
          (fun name -> rm_rf (Filename.concat path name))
          (Sys.readdir path);
        try Unix.rmdir path with Unix.Unix_error _ -> ())
    | false -> ( try Sys.remove path with Sys_error _ -> ())
    | exception Sys_error _ -> ()

  let with_workspace name fn =
    Eio_main.run @@ fun stdenv ->
    let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
    let raw = Filename.temp_dir ("mentat-ctx-" ^ name) "" in
    let base = Unix.realpath raw in
    let root = Abs.of_string_exn base in
    Fun.protect
      ~finally:(fun () -> rm_rf base)
      (fun () -> fn ~stdenv ~base ~root)

  let load ?(nested_scan = false) ?(settings = []) ?(trusted = true)
      ?(environment = C.Environment.none) ~stdenv ~root ~cwd () =
    match
      C.load ~environment ~stdenv ~nested_scan ~config:(resolved settings)
        ~trusted ~root ~cwd ~user_config_file
    with
    | Ok t -> t
    | Error d -> failf "load: %s" (Mentat_diagnostic.to_string d)

  (* The environment block is the developer fragment: its text is the only place
     the workspace facts render. [fragments_json] projects each fragment as an
     object with [role] and [text] members. *)
  let developer_fragment_text t =
    let string_member name members =
      match Option.map snd (Jsont.Json.find_mem name members) with
      | Some (Jsont.String (value, _)) -> Some value
      | Some _ | None -> None
    in
    List.find_map
      (function
        | Jsont.Object (members, _) -> (
            match string_member "role" members with
            | Some "developer" -> string_member "text" members
            | Some _ | None -> None)
        | _ -> None)
      (C.fragments_json t)
    |> function
    | Some text -> text
    | None -> failf "no developer fragment in the projection"

  let contains ~affix text = Option.is_some (String.find_first ~sub:affix text)

  let source_by_kind t kind =
    List.find_opt (fun s -> Source.kind s = kind) (C.sources t)

  let state_of t kind =
    Option.map
      (fun s -> Source.state_string (Source.status s))
      (source_by_kind t kind)

  let active_project () =
    with_workspace "active" (fun ~stdenv ~base ~root ->
        write_file base "AGENTS.md" "be helpful";
        let t = load ~stdenv ~root ~cwd:root () in
        equal (option string) ~msg:"the project AGENTS.md is active"
          (Some "active")
          (state_of t Source.Project);
        equal int ~msg:"system, workspace, and instruction fragments" 3
          (List.length (C.fragments t));
        equal string ~msg:"the git marker is absent in a bare directory" ""
          (Option.value ~default:"" (C.root_marker t));
        is_true ~msg:"the budget records the file bytes"
          (C.budget_used t = String.length "be helpful"))

  let shadowing () =
    with_workspace "shadow" (fun ~stdenv ~base ~root ->
        write_file base "AGENTS.override.md" "override";
        write_file base "AGENTS.md" "agents";
        write_file base "CLAUDE.md" "claude";
        let t = load ~stdenv ~root ~cwd:root () in
        equal (option string) ~msg:"the override wins the directory"
          (Some "active")
          (state_of t Source.Local_override);
        equal (option string) ~msg:"AGENTS.md is shadowed" (Some "shadowed")
          (state_of t Source.Project);
        equal (option string) ~msg:"CLAUDE.md is shadowed" (Some "shadowed")
          (state_of t Source.Compatibility);
        equal (option string) ~msg:"the shadowing reason names the override"
          (Some "shadowed_by_override")
          (Option.bind (source_by_kind t Source.Project) (fun s ->
               Source.reason_string (Source.status s))))

  let budget_truncation_utf8 () =
    with_workspace "budget" (fun ~stdenv ~base ~root ->
        (* Ten ASCII bytes then a three-byte U+20AC; a budget of 11 cuts one
           byte into the euro sign. *)
        let content = String.make 10 'a' ^ "\xe2\x82\xac" in
        write_file base "AGENTS.md" content;
        let t =
          load
            ~settings:
              [ set Mentat_config.Field.instructions_project_max_bytes "11" ]
            ~stdenv ~root ~cwd:root ()
        in
        match Option.map Source.status (source_by_kind t Source.Project) with
        | Some (Source.Active c) ->
            equal int
              ~msg:"truncation stops at the UTF-8 boundary, not mid-char" 10
              c.Source.included_bytes;
            is_true ~msg:"the budget omitted the trailing bytes"
              (c.Source.omitted_bytes > 0);
            equal int ~msg:"the budget consumed its original-byte allowance" 11
              (C.budget_used t)
        | _ -> failf "expected an active project source")

  let projection_identity () =
    with_workspace "digest" (fun ~stdenv ~base ~root ->
        write_file base "AGENTS.md" "alpha";
        let d1 = C.rendered_digest (load ~stdenv ~root ~cwd:root ()) in
        let d2 = C.rendered_digest (load ~stdenv ~root ~cwd:root ()) in
        equal string ~msg:"identical snapshots share a rendered digest" d1 d2;
        write_file base "AGENTS.md" "beta";
        let d3 = C.rendered_digest (load ~stdenv ~root ~cwd:root ()) in
        is_false ~msg:"changed instructions change the digest"
          (String.equal d1 d3))

  let trust_gating () =
    with_workspace "trust" (fun ~stdenv ~base ~root ->
        write_file base "AGENTS.md" "secret instructions";
        let t = load ~trusted:false ~stdenv ~root ~cwd:root () in
        is_true ~msg:"an untrusted workspace discovers no project source"
          (Option.is_none (source_by_kind t Source.Project));
        equal int ~msg:"no budget is consumed when untrusted" 0
          (C.budget_used t);
        equal int ~msg:"only the base fragments remain" 2
          (List.length (C.fragments t)))

  let disabled_compat_warns () =
    with_workspace "compat" (fun ~stdenv ~base ~root ->
        write_file base "CLAUDE.md" "compatibility only";
        let t =
          load
            ~settings:[ set Mentat_config.Field.instructions_claude_md "false" ]
            ~stdenv ~root ~cwd:root ()
        in
        equal (option string) ~msg:"the compatibility file is disabled"
          (Some "disabled")
          (state_of t Source.Compatibility);
        is_true ~msg:"a disabled-only compatibility file warns"
          (List.exists
             (fun w ->
               Option.is_some
                 (String.find_first ~sub:"CLAUDE.md compatibility is disabled" w))
             (C.warnings t)))

  let nested_audit () =
    with_workspace "nested" (fun ~stdenv ~base ~root ->
        write_file base "AGENTS.md" "root instructions";
        write_file base "sub/AGENTS.md" "nested instructions";
        let t = load ~nested_scan:true ~stdenv ~root ~cwd:root () in
        let not_activated =
          List.filter
            (fun s ->
              String.equal "not_activated"
                (Source.state_string (Source.status s)))
            (C.sources t)
        in
        equal int ~msg:"the nested AGENTS.md is an audit source" 1
          (List.length not_activated);
        is_true ~msg:"the scan completed" (C.nested_scan t = `Complete))

  let source_jsont_roundtrips () =
    with_workspace "jsont" (fun ~stdenv ~base ~root ->
        (* Active (override), Shadowed (agents), Disabled (claude, gated off),
           and Not_activated (nested) all appear, so the codec is exercised
           across the status families. *)
        write_file base "AGENTS.override.md" "override";
        write_file base "AGENTS.md" "agents";
        write_file base "CLAUDE.md" "claude";
        write_file base "sub/AGENTS.md" "nested";
        let t =
          load ~nested_scan:true
            ~settings:[ set Mentat_config.Field.instructions_claude_md "false" ]
            ~stdenv ~root ~cwd:root ()
        in
        let states =
          List.sort_uniq String.compare
            (List.map
               (fun s -> Source.state_string (Source.status s))
               (C.sources t))
        in
        is_true ~msg:"the fixture spans several status families"
          (List.length states >= 4);
        List.iter
          (fun s ->
            let j = encode Source.jsont s in
            let round = encode Source.jsont (decode Source.jsont j) in
            check "a source projection round-trips through jsont"
              (Json.equal j round))
          (C.sources t))

  let environment_block () =
    with_workspace "environment" (fun ~stdenv ~base:_ ~root ->
        let render environment =
          developer_fragment_text (load ~environment ~stdenv ~root ~cwd:root ())
        in
        let with_cutoff =
          render
            (C.Environment.make ~model:"GPT-5.5 (gpt-5.5)"
               ~model_cutoff:"2025-12" ())
        in
        is_true ~msg:"the model line carries the knowledge cutoff clause"
          (contains ~affix:"Model: GPT-5.5 (gpt-5.5); knowledge cutoff 2025-12"
             with_cutoff);
        let without_cutoff =
          render (C.Environment.make ~model:"GPT-5.5 (gpt-5.5)" ())
        in
        is_true ~msg:"the model line renders without a cutoff clause"
          (contains ~affix:"Model: GPT-5.5 (gpt-5.5)" without_cutoff);
        is_false ~msg:"no cutoff clause when the cutoff is absent"
          (contains ~affix:"knowledge cutoff" without_cutoff);
        is_false ~msg:"a cutoff without a model is dropped"
          (contains ~affix:"knowledge cutoff"
             (render (C.Environment.make ~model_cutoff:"2025-12" ()))))

  let suite =
    [
      test "an active project AGENTS.md projects three fragments" active_project;
      test "the environment block renders the model knowledge cutoff"
        environment_block;
      test "higher-precedence candidates shadow lower ones" shadowing;
      test "the byte budget truncates at a UTF-8 boundary"
        budget_truncation_utf8;
      test "the rendered digest is a per-projection identity"
        projection_identity;
      test "an untrusted workspace reads no project instructions" trust_gating;
      test "a disabled-only compatibility file is reported"
        disabled_compat_warns;
      test "the nested scan records AGENTS.md below the cwd" nested_audit;
      test "source projections round-trip through jsont" source_jsont_roundtrips;
    ]
end

(* Skill discovery, catalog rendering, and injections. Each case runs a real
   [Skills.load] over a fresh temp root, exercising the loader end to end:
   catalog budget math, cross-root shadowing, config disabling, invalid
   frontmatter classification, and the CLI injections. *)
module Skills_tests = struct
  module Skills = Mentat_context.Skills
  module Skill = Skills.Skill

  (* Filesystem + Eio scaffolding. Each test runs in a fresh temp root that is
     removed on the way out; paths are [realpath]'d so they compare equal to the
     loader's canonical spellings. *)

  let rec rm_rf path =
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | { Unix.st_kind = Unix.S_DIR; _ } ->
        Array.iter (fun e -> rm_rf (Filename.concat path e)) (Sys.readdir path);
        Unix.rmdir path
    | _ | (exception _) -> ( try Unix.unlink path with _ -> ())

  let rec mkdir_p dir =
    if Sys.file_exists dir then ()
    else begin
      mkdir_p (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end

  let write_file path content =
    let oc = open_out path in
    output_string oc content;
    close_out oc

  (* Writes [content] to [base]/[sub]/[name]/SKILL.md, creating parents. *)
  let write_skill ~base ~sub ~name ~content =
    let dir = Filename.concat (Filename.concat base sub) name in
    mkdir_p dir;
    write_file (Filename.concat dir "SKILL.md") content

  let skill_doc ?description ?(extra = "") ?(body = "") () =
    let fm =
      (match description with
        | None -> ""
        | Some d -> Printf.sprintf "description: %s\n" d)
      ^ extra
    in
    Printf.sprintf "---\n%s---\n%s" fm body

  let contains ~needle hay =
    let nl = String.length needle and hl = String.length hay in
    let rec go i =
      i + nl <= hl && (String.equal (String.sub hay i nl) needle || go (i + 1))
    in
    nl = 0 || go 0

  let with_base name fn =
    Eio_main.run @@ fun stdenv ->
    let stdenv = (stdenv :> Eio_unix.Stdenv.base) in
    let raw = Filename.temp_dir ("mentat-skills-" ^ name) "" in
    Fun.protect
      ~finally:(fun () -> rm_rf raw)
      (fun () -> fn ~stdenv ~base:(Unix.realpath raw))

  let set field value doc =
    match Mentat_config.set field value doc with
    | Ok doc -> doc
    | Error e ->
        failf "set %s: %s"
          (Mentat_config.Field.name field)
          (Mentat_config.Error.message e)

  let resolved configure =
    let user = Lpath.Abs.of_string_exn "/mentat/config" in
    match
      Mentat_config.resolve
        ~env:(fun _ -> None)
        ~user:(user, configure Mentat_config.empty)
        ~extra:None
        ~workspace_config:
          (Mentat_config.Disabled
             { status = "untrusted"; project = None; project_local = None })
        ~overrides:[]
    with
    | Ok r -> r
    | Error e -> failf "resolve: %s" (Mentat_config.Error.message e)

  let load ~stdenv ~base ?(builtins = []) ?(configure = Fun.id)
      ?(trusted = true) () =
    let abs s = Lpath.Abs.of_string_exn s in
    let root = abs base in
    Skills.load ~stdenv ~builtins ~config:(resolved configure) ~trusted ~root
      ~cwd:root
      ~user_config_file:(abs (Filename.concat base "userhome/config"))

  (* Queries over a snapshot. *)

  let name_of s = Skill.Name.to_string (Skill.name s)

  let find t n =
    List.find_opt (fun s -> String.equal (name_of s) n) (Skills.skills t)

  let find_kind t n kind =
    List.find_opt
      (fun s -> String.equal (name_of s) n && Skill.kind s = kind)
      (Skills.skills t)

  let state t n =
    match find t n with
    | Some s -> Skill.state_string (Skill.status s)
    | None -> "absent"

  let reason t n =
    match find t n with
    | Some s -> Skill.reason_string (Skill.status s)
    | None -> None

  (* --- Catalog budget math --- *)

  let ellipsis = "\xe2\x80\xa6"

  let catalog_budget =
    group "Catalog budget"
      [
        test "full descriptions fit under a generous budget" (fun () ->
            with_base "budget-full" @@ fun ~stdenv ~base ->
            write_skill ~base ~sub:".mentat/skills" ~name:"alpha"
              ~content:(skill_doc ~description:"Alpha" ());
            write_skill ~base ~sub:".mentat/skills" ~name:"beta"
              ~content:(skill_doc ~description:"Beta" ());
            let t =
              load ~stdenv ~base
                ~configure:
                  (set Mentat_config.Field.skills_catalog_max_bytes 200)
                ()
            in
            let cat = Skills.catalog t in
            equal string "- alpha: Alpha\n- beta: Beta"
              (Skills.Catalog.text cat);
            equal bool false (Skills.Catalog.names_only cat);
            equal (list string) []
              (List.map Skill.Name.to_string (Skills.Catalog.trimmed cat)));
        test "a tight budget trims non-builtin descriptions at a UTF-8 boundary"
          (fun () ->
            with_base "budget-trim" @@ fun ~stdenv ~base ->
            write_skill ~base ~sub:".mentat/skills" ~name:"alpha"
              ~content:(skill_doc ~description:(String.make 40 'a') ());
            write_skill ~base ~sub:".mentat/skills" ~name:"beta"
              ~content:(skill_doc ~description:(String.make 40 'b') ());
            let t =
              load ~stdenv ~base
                ~configure:(set Mentat_config.Field.skills_catalog_max_bytes 60)
                ()
            in
            let cat = Skills.catalog t in
            (* budget 60, name_overhead 19, available 41, per_entry 20, cut 17. *)
            let expect =
              Printf.sprintf "- alpha: %s%s\n- beta: %s%s" (String.make 17 'a')
                ellipsis (String.make 17 'b') ellipsis
            in
            equal string expect (Skills.Catalog.text cat);
            equal bool false (Skills.Catalog.names_only cat);
            equal (list string) [ "alpha"; "beta" ]
              (List.map Skill.Name.to_string (Skills.Catalog.trimmed cat)));
        test "a budget below the description floor drops descriptions"
          (fun () ->
            with_base "budget-names-only" @@ fun ~stdenv ~base ->
            write_skill ~base ~sub:".mentat/skills" ~name:"alpha"
              ~content:(skill_doc ~description:(String.make 40 'a') ());
            write_skill ~base ~sub:".mentat/skills" ~name:"beta"
              ~content:(skill_doc ~description:(String.make 40 'b') ());
            let t =
              load ~stdenv ~base
                ~configure:(set Mentat_config.Field.skills_catalog_max_bytes 25)
                ()
            in
            let cat = Skills.catalog t in
            equal string "- alpha\n- beta" (Skills.Catalog.text cat);
            equal bool true (Skills.Catalog.names_only cat);
            is_true
              (List.exists
                 (contains ~needle:"descriptions dropped")
                 (Skills.warnings t)));
        test "builtin descriptions are never dropped, even names-only"
          (fun () ->
            with_base "budget-builtin" @@ fun ~stdenv ~base ->
            write_skill ~base ~sub:".mentat/skills" ~name:"alpha"
              ~content:(skill_doc ~description:(String.make 40 'a') ());
            let builtins =
              [
                ( "zbuiltin",
                  skill_doc ~description:"Builtin description kept intact" () );
              ]
            in
            let t =
              load ~stdenv ~base ~builtins
                ~configure:(set Mentat_config.Field.skills_catalog_max_bytes 60)
                ()
            in
            let cat = Skills.catalog t in
            equal string "- alpha\n- zbuiltin: Builtin description kept intact"
              (Skills.Catalog.text cat);
            equal bool true (Skills.Catalog.names_only cat));
      ]

  (* --- Discovery shadowing order --- *)

  let shadowing =
    group "Shadowing"
      [
        test "the first active candidate for a name wins across roots"
          (fun () ->
            with_base "shadow" @@ fun ~stdenv ~base ->
            write_skill ~base ~sub:".mentat/skills" ~name:"foo"
              ~content:(skill_doc ~description:"project foo" ());
            write_skill ~base ~sub:".agents/skills" ~name:"foo"
              ~content:(skill_doc ~description:"agents foo" ());
            let t = load ~stdenv ~base () in
            let project = Option.get (find_kind t "foo" Skill.Project) in
            let agents = Option.get (find_kind t "foo" Skill.Compat_agents) in
            equal string "active" (Skill.state_string (Skill.status project));
            equal string "shadowed" (Skill.state_string (Skill.status agents));
            equal (option string)
              (Some (Skill.origin project))
              (Skill.detail_string (Skill.status agents));
            (* the winner is served, and the catalog lists foo once. *)
            let active =
              Skills.find_active t (Result.get_ok (Skill.Name.of_string "foo"))
            in
            equal bool true (Option.is_some active);
            equal string "- foo: project foo"
              (Skills.Catalog.text (Skills.catalog t)));
      ]

  (* --- Disabled filtering --- *)

  let disabled =
    group "Disabled"
      [
        test "skills.disabled marks the candidate before shadowing" (fun () ->
            with_base "disabled-config" @@ fun ~stdenv ~base ->
            write_skill ~base ~sub:".mentat/skills" ~name:"foo"
              ~content:(skill_doc ~description:"foo" ());
            let t =
              load ~stdenv ~base
                ~configure:(set Mentat_config.Field.skills_disabled [ "foo" ])
                ()
            in
            equal string "disabled" (state t "foo");
            equal (option string) (Some "config_disabled") (reason t "foo");
            equal bool false
              (Option.is_some
                 (Skills.find_active t
                    (Result.get_ok (Skill.Name.of_string "foo"))));
            equal string "" (Skills.Catalog.text (Skills.catalog t)));
        test "skills.project = false disables project skills" (fun () ->
            with_base "disabled-project" @@ fun ~stdenv ~base ->
            write_skill ~base ~sub:".mentat/skills" ~name:"foo"
              ~content:(skill_doc ~description:"foo" ());
            let t =
              load ~stdenv ~base
                ~configure:(set Mentat_config.Field.skills_project false)
                ()
            in
            equal string "disabled" (state t "foo");
            equal (option string) (Some "project_skills_disabled")
              (reason t "foo"));
        test "skills.builtin = false disables built-ins" (fun () ->
            with_base "disabled-builtin" @@ fun ~stdenv ~base ->
            let builtins = [ ("bar", skill_doc ~description:"bar" ()) ] in
            let t =
              load ~stdenv ~base ~builtins
                ~configure:(set Mentat_config.Field.skills_builtin false)
                ()
            in
            equal string "disabled" (state t "bar");
            equal (option string) (Some "builtin_disabled") (reason t "bar"));
        test "an untrusted workspace does not discover project skills"
          (fun () ->
            with_base "untrusted" @@ fun ~stdenv ~base ->
            write_skill ~base ~sub:".mentat/skills" ~name:"foo"
              ~content:(skill_doc ~description:"foo" ());
            let t = load ~stdenv ~base ~trusted:false () in
            equal string "absent" (state t "foo");
            is_true
              (List.exists (contains ~needle:"not trusted") (Skills.warnings t)));
      ]

  (* --- Frontmatter-invalid statuses --- *)

  let invalid_frontmatter =
    let load_one ~name ~content fn =
      with_base ("invalid-" ^ name) @@ fun ~stdenv ~base ->
      write_skill ~base ~sub:".mentat/skills" ~name ~content;
      fn (load ~stdenv ~base ())
    in
    group "Invalid frontmatter"
      [
        test "a missing description is invalid" (fun () ->
            load_one ~name:"nodesc" ~content:(skill_doc ~body:"x" ())
            @@ fun t ->
            equal string "invalid" (state t "nodesc");
            equal (option string) (Some "description_missing")
              (reason t "nodesc");
            is_true
              (List.exists
                 (contains ~needle:"no description")
                 (Skills.warnings t)));
        test "an over-long description is invalid" (fun () ->
            load_one ~name:"toolong"
              ~content:(skill_doc ~description:(String.make 1025 'x') ())
            @@ fun t ->
            equal string "invalid" (state t "toolong");
            equal (option string) (Some "description_too_long")
              (reason t "toolong"));
        test "a non-string description is invalid, not missing" (fun () ->
            load_one ~name:"listdesc"
              ~content:(skill_doc ~description:"[one, two]" ())
            @@ fun t ->
            equal string "invalid" (state t "listdesc");
            equal (option string) (Some "invalid_frontmatter")
              (reason t "listdesc");
            is_true
              (List.exists
                 (contains ~needle:"must be a string")
                 (Skills.warnings t)));
        test "unterminated frontmatter is invalid" (fun () ->
            load_one ~name:"unterminated" ~content:"---\ndescription: x\nbody"
            @@ fun t ->
            equal string "invalid" (state t "unterminated");
            equal (option string) (Some "invalid_frontmatter")
              (reason t "unterminated"));
        test "a duplicated consumed key is invalid" (fun () ->
            load_one ~name:"dup"
              ~content:"---\ndescription: a\ndescription: b\n---\nbody"
            @@ fun t ->
            equal string "invalid" (state t "dup");
            equal (option string) (Some "invalid_frontmatter") (reason t "dup"));
      ]

  (* --- Injections --- *)

  let injections =
    let with_foo fn =
      with_base "injections" @@ fun ~stdenv ~base ->
      write_skill ~base ~sub:".mentat/skills" ~name:"foo"
        ~content:(skill_doc ~description:"Foo desc" ~body:"FOO-BODY" ());
      fn (load ~stdenv ~base ())
    in
    group "Injections"
      [
        test "an unknown name errors and lists the active names" (fun () ->
            with_foo @@ fun t ->
            match Skills.injections t ~names:[ "nope" ] with
            | Ok _ -> fail "expected an error for an unknown skill"
            | Error message ->
                is_true (contains ~needle:"unknown skill" message);
                is_true (contains ~needle:"foo" message));
        test "a known name yields framed guidance" (fun () ->
            with_foo @@ fun t ->
            match Skills.injections t ~names:[ "foo" ] with
            | Error message -> failf "unexpected error: %s" message
            | Ok texts ->
                equal int 1 (List.length texts);
                let text = List.hd texts in
                is_true
                  (contains ~needle:"The user invoked the \"foo\" skill" text);
                is_true (contains ~needle:"FOO-BODY" text));
        test "an empty name list yields an empty result" (fun () ->
            with_foo @@ fun t ->
            equal
              (result (list string) string)
              (Ok [])
              (Skills.injections t ~names:[]));
        test "a malformed name errors" (fun () ->
            with_foo @@ fun t ->
            match Skills.injections t ~names:[ "BadName" ] with
            | Ok _ -> fail "expected an error for a malformed name"
            | Error _ -> ());
      ]

  (* --- Body cap --- *)

  (* The [skill] tool and [--skill] injections serve guidance bounded at 256 KiB
     (see the mli); an over-cap body is cut at a UTF-8 boundary and flagged so
     the model knows it was truncated. Injections route through the same payload
     helper as the tool, so they observe the same bound. *)
  let body_cap =
    let cap = 256 * 1024 in
    let marker = "skill guidance truncated by mentat" in
    let count_x =
      String.fold_left (fun n c -> if c = 'x' then n + 1 else n) 0
    in
    let served ~body fn =
      with_base "body-cap" @@ fun ~stdenv ~base ->
      write_skill ~base ~sub:".mentat/skills" ~name:"big"
        ~content:(skill_doc ~description:"Big" ~body ());
      match Skills.injections (load ~stdenv ~base ()) ~names:[ "big" ] with
      | Error message -> failf "unexpected error: %s" message
      | Ok [ text ] -> fn text
      | Ok texts -> failf "expected one injection, got %d" (List.length texts)
    in
    group "Body cap"
      [
        test "a body at the cap is served whole and unflagged" (fun () ->
            served ~body:(String.make cap 'x') @@ fun text ->
            equal int cap (count_x text);
            equal bool false (contains ~needle:marker text));
        test "an over-cap body is truncated at the cap and flagged" (fun () ->
            served ~body:(String.make (cap + 4096) 'x') @@ fun text ->
            equal int cap (count_x text);
            is_true (contains ~needle:marker text));
      ]

  let suite =
    [
      catalog_budget;
      shadowing;
      disabled;
      invalid_frontmatter;
      injections;
      body_cap;
    ]
end

(* Commands: [Commands.load] over a fresh temp root plus the pure [substitute]
   eliminator. The substitution group pins the grammar edge-table byte for
   byte; the discovery group covers namespacing, cross-root precedence, the
   config gates, and frontmatter classification. *)
module Commands_tests = struct
  module Commands = Mentat_context.Commands
  module Command = Commands.Command

  (* Filesystem scaffolding shared with {!Skills_tests}; the commands roots are
     [<root>/.mentat/commands] and friends, with user commands under the config
     home. *)

  let write_command ~base ~sub ~path ~content =
    let full = Filename.concat (Filename.concat base sub) path in
    Skills_tests.mkdir_p (Filename.dirname full);
    Skills_tests.write_file full content

  let command_doc ?description ?argument_hint ?(extra = "") ~body () =
    let fm =
      (match description with
        | None -> ""
        | Some d -> Printf.sprintf "description: %s\n" d)
      ^ (match argument_hint with
        | None -> ""
        | Some h -> Printf.sprintf "argument-hint: %s\n" h)
      ^ extra
    in
    if String.equal fm "" then body else Printf.sprintf "---\n%s---\n%s" fm body

  let load ~stdenv ~base ?(configure = Fun.id) ?(trusted = true) () =
    let abs s = Lpath.Abs.of_string_exn s in
    let root = abs base in
    Commands.load ~stdenv
      ~config:(Skills_tests.resolved configure)
      ~trusted ~root
      ~user_config_file:(abs (Filename.concat base "userhome/config"))

  let nm s = Result.get_ok (Commands.Name.of_string s)

  let find t n =
    List.find_opt
      (fun c -> String.equal (Command.name c) n)
      (Commands.commands t)

  let find_kind t n kind =
    List.find_opt
      (fun c -> String.equal (Command.name c) n && Command.kind c = kind)
      (Commands.commands t)

  let state t n =
    match find t n with
    | Some c -> Command.state_string (Command.status c)
    | None -> "absent"

  let reason t n =
    match find t n with
    | Some c -> Command.reason_string (Command.status c)
    | None -> None

  (* --- Substitution: the grammar edge-table --- *)

  let subst body args expected =
    test (Printf.sprintf "subst %S / %S" body args) (fun () ->
        equal string expected (Commands.substitute ~body ~arguments:args))

  let substitution =
    group "substitution"
      [
        subst "Fix $1 in $2" "bug login" "Fix bug in login";
        subst "Fix $1 in $2" "bug" "Fix bug in ";
        subst "Note: $ARGUMENTS" "" "Note: ";
        subst "Cost is $5" "a b" "Cost is ";
        subst "Home is $HOME" "anything here" "Home is $HOME";
        subst "Price $12" "a b" "Price a2";
        subst "$ARGUMENTS" "x $1 y" "x $1 y";
        subst "$ARGUMENTSX" "a b" "a bX";
        subst "$arguments" "a b" "$arguments";
        subst "$$1" "a b" "$a";
        subst "$0" "a b" "$0";
        subst "$" "x" "$";
        subst "a$" "x" "a$";
        subst "$_" "a b" "$_";
        subst "$ARGUMENTS" "a b c" "a b c";
        subst "$10" "a b" "a0";
        subst "$$ARGUMENTS" "a b" "$a b";
        subst "$1|$2|$3" "a   b" "a|b|";
        subst "[$1]" "a\tb" "[a]";
        subst "$1" "a\nb c" "a\nb";
        subst "$1" "$2" "$2";
        prop "a dollar-free body is returned unchanged" string (fun s ->
            let body = String.map (fun c -> if c = '$' then '#' else c) s in
            String.equal body (Commands.substitute ~body ~arguments:"x y"));
        prop "$ARGUMENTS expands to the whole argument string" string
          (fun args ->
            String.equal args
              (Commands.substitute ~body:"$ARGUMENTS" ~arguments:args));
      ]

  (* --- Discovery --- *)

  let namespacing =
    group "namespacing"
      [
        test "a subdirectory namespaces the command with a colon" (fun () ->
            Skills_tests.with_base "ns" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"git/commit.md"
              ~content:(command_doc ~body:"Commit the staged changes." ());
            let t = load ~stdenv ~base () in
            equal string "active" (state t "git:commit");
            equal
              (option (option string))
              (Some None)
              (Option.map
                 (fun c -> c.Command.description)
                 (Commands.find_active t (nm "git:commit"))));
        test "an ill-formed path segment is Invalid, not renamed" (fun () ->
            Skills_tests.with_base "badname" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"Bad Name.md"
              ~content:(command_doc ~body:"x" ());
            let t = load ~stdenv ~base () in
            equal string "invalid" (state t "Bad Name");
            equal (option string) (Some "invalid_name") (reason t "Bad Name"));
        test "an ill-formed directory segment taints the whole name" (fun () ->
            Skills_tests.with_base "baddir" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"My Dir/foo.md"
              ~content:(command_doc ~body:"x" ());
            let t = load ~stdenv ~base () in
            equal string "invalid" (state t "My Dir:foo");
            equal (option string) (Some "invalid_name") (reason t "My Dir:foo"));
      ]

  let precedence =
    group "precedence"
      [
        test "project shadows compat shadows user" (fun () ->
            Skills_tests.with_base "prec" @@ fun ~stdenv ~base ->
            let doc origin = command_doc ~body:("from " ^ origin) () in
            write_command ~base ~sub:".mentat/commands" ~path:"review.md"
              ~content:(doc "project");
            write_command ~base ~sub:".agents/commands" ~path:"review.md"
              ~content:(doc "agents");
            write_command ~base ~sub:".claude/commands" ~path:"review.md"
              ~content:(doc "claude");
            write_command ~base ~sub:"userhome/commands" ~path:"review.md"
              ~content:(doc "user");
            let t = load ~stdenv ~base () in
            let st kind =
              match find_kind t "review" kind with
              | Some c -> Command.state_string (Command.status c)
              | None -> "absent"
            in
            equal string "active" (st Command.Project);
            equal string "shadowed" (st Command.Compat_agents);
            equal string "shadowed" (st Command.Compat_claude);
            equal string "shadowed" (st Command.User);
            match Commands.find_active t (nm "review") with
            | Some c -> equal string "from project" c.Command.body
            | None -> failf "expected an active review command");
        test "disabled is applied before shadowing" (fun () ->
            Skills_tests.with_base "disabled" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"foo.md"
              ~content:(command_doc ~body:"project foo" ());
            write_command ~base ~sub:".agents/commands" ~path:"foo.md"
              ~content:(command_doc ~body:"agents foo" ());
            let t =
              load ~stdenv ~base
                ~configure:(fun d ->
                  Skills_tests.set Mentat_config.Field.commands_disabled
                    [ "foo" ] d)
                ()
            in
            equal string "disabled"
              (match find_kind t "foo" Command.Project with
              | Some c -> Command.state_string (Command.status c)
              | None -> "absent");
            equal string "disabled"
              (match find_kind t "foo" Command.Compat_agents with
              | Some c -> Command.state_string (Command.status c)
              | None -> "absent");
            equal (option string) None
              (Option.map
                 (fun c -> c.Command.body)
                 (Commands.find_active t (nm "foo"))));
      ]

  let gates =
    group "config gates"
      [
        test "an untrusted workspace discovers no project commands" (fun () ->
            Skills_tests.with_base "untrusted" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"proj.md"
              ~content:(command_doc ~body:"x" ());
            write_command ~base ~sub:"userhome/commands" ~path:"usr.md"
              ~content:(command_doc ~body:"y" ());
            let t = load ~stdenv ~base ~trusted:false () in
            equal string "absent" (state t "proj");
            equal string "active" (state t "usr");
            is_true
              (List.exists
                 (Skills_tests.contains ~needle:"not trusted")
                 (Commands.warnings t)));
        test "commands.project=false disables all project roots" (fun () ->
            Skills_tests.with_base "no-project" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"a.md"
              ~content:(command_doc ~body:"x" ());
            write_command ~base ~sub:".agents/commands" ~path:"b.md"
              ~content:(command_doc ~body:"y" ());
            let t =
              load ~stdenv ~base
                ~configure:(fun d ->
                  Skills_tests.set Mentat_config.Field.commands_project false d)
                ()
            in
            equal (option string) (Some "project_disabled") (reason t "a");
            equal (option string) (Some "project_disabled") (reason t "b"));
        test "commands.compat=false disables only the compat roots" (fun () ->
            Skills_tests.with_base "no-compat" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"a.md"
              ~content:(command_doc ~body:"x" ());
            write_command ~base ~sub:".agents/commands" ~path:"b.md"
              ~content:(command_doc ~body:"y" ());
            let t =
              load ~stdenv ~base
                ~configure:(fun d ->
                  Skills_tests.set Mentat_config.Field.commands_compat false d)
                ()
            in
            equal string "active" (state t "a");
            equal (option string) (Some "compat_disabled") (reason t "b"));
        test "commands.enabled=false yields an empty, disabled snapshot"
          (fun () ->
            Skills_tests.with_base "off" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"a.md"
              ~content:(command_doc ~body:"x" ());
            let t =
              load ~stdenv ~base
                ~configure:(fun d ->
                  Skills_tests.set Mentat_config.Field.commands_enabled false d)
                ()
            in
            equal bool false (Commands.enabled t);
            equal int 0 (List.length (Commands.commands t)));
      ]

  let frontmatter =
    group "frontmatter"
      [
        test "description and argument-hint are carried on the content"
          (fun () ->
            Skills_tests.with_base "fm" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"review.md"
              ~content:
                (command_doc ~description:"Review a PR" ~argument_hint:"<pr>"
                   ~body:"Review $1." ());
            let t = load ~stdenv ~base () in
            match Commands.find_active t (nm "review") with
            | None -> failf "expected an active review command"
            | Some content ->
                equal (option string) (Some "Review a PR")
                  content.Command.description;
                equal (option string) (Some "<pr>")
                  content.Command.argument_hint);
        test "a body with no frontmatter is active with no metadata" (fun () ->
            Skills_tests.with_base "nofm" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"plain.md"
              ~content:"Just a plain body, no frontmatter.\n";
            let t = load ~stdenv ~base () in
            equal string "active" (state t "plain");
            equal
              (option (option string))
              (Some None)
              (Option.map
                 (fun c -> c.Command.description)
                 (Commands.find_active t (nm "plain"))));
        test "model and unknown keys warn distinctly" (fun () ->
            Skills_tests.with_base "warn" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"cmd.md"
              ~content:
                (command_doc ~extra:"model: gpt-4\nfoo: bar\n" ~body:"x" ());
            let t = load ~stdenv ~base () in
            let warnings = Commands.warnings t in
            is_true
              (List.exists
                 (Skills_tests.contains ~needle:"not supported in this version")
                 warnings);
            is_true
              (List.exists
                 (Skills_tests.contains ~needle:"ignored frontmatter keys")
                 warnings));
        test "an empty or blank body is Invalid Empty_body" (fun () ->
            Skills_tests.with_base "empty" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"blank.md"
              ~content:"---\ndescription: x\n---\n   \n";
            write_command ~base ~sub:".mentat/commands" ~path:"nothing.md"
              ~content:"";
            let t = load ~stdenv ~base () in
            equal (option string) (Some "empty_body") (reason t "blank");
            equal (option string) (Some "empty_body") (reason t "nothing"));
        test "malformed frontmatter is Invalid_frontmatter" (fun () ->
            Skills_tests.with_base "badfm" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"bad.md"
              ~content:"---\ndescription: x\nunterminated body\n";
            let t = load ~stdenv ~base () in
            equal (option string) (Some "invalid_frontmatter") (reason t "bad"));
        test "a non-string description is Invalid_frontmatter, not dropped"
          (fun () ->
            Skills_tests.with_base "listdesc" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"listdesc.md"
              ~content:(command_doc ~description:"[one, two]" ~body:"x" ());
            let t = load ~stdenv ~base () in
            equal (option string) (Some "invalid_frontmatter")
              (reason t "listdesc"));
        test "a non-string argument-hint is Invalid_frontmatter, not dropped"
          (fun () ->
            Skills_tests.with_base "listhint" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"listhint.md"
              ~content:
                (command_doc ~description:"ok" ~argument_hint:"[a, b]" ~body:"x"
                   ());
            let t = load ~stdenv ~base () in
            equal (option string) (Some "invalid_frontmatter")
              (reason t "listhint"));
      ]

  let expansion =
    group "expansion"
      [
        test "expand substitutes arguments into the active body" (fun () ->
            Skills_tests.with_base "expand" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"greet.md"
              ~content:(command_doc ~body:"Hello $1 from $ARGUMENTS" ());
            let t = load ~stdenv ~base () in
            match
              Commands.expand t ~name:(nm "greet") ~arguments:"World and all"
            with
            | Ok [ Mentat_llm.Content.Text s ] ->
                equal string "Hello World from World and all" s
            | Ok _ -> failf "expected one text block"
            | Error (`Unknown n) -> failf "unexpected unknown %s" n
            | Error (`File_unresolved e) ->
                failf "unexpected file error %s" e.Commands.path);
        test "an unknown name is Error Unknown" (fun () ->
            Skills_tests.with_base "expand-unknown" @@ fun ~stdenv ~base ->
            let t = load ~stdenv ~base () in
            let outcome =
              match Commands.expand t ~name:(nm "nope") ~arguments:"x" with
              | Error (`Unknown n) -> "unknown:" ^ n
              | Error (`File_unresolved _) -> "file"
              | Ok _ -> "ok"
            in
            equal string "unknown:nope" outcome);
        test "a blank expansion is the empty content list" (fun () ->
            Skills_tests.with_base "expand-blank" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"pos.md"
              ~content:(command_doc ~body:"$1" ());
            let t = load ~stdenv ~base () in
            let outcome =
              match Commands.expand t ~name:(nm "pos") ~arguments:"" with
              | Ok [] -> "empty"
              | Ok _ -> "some"
              | Error _ -> "error"
            in
            equal string "empty" outcome);
      ]

  (* --- @file embedding (the fast-follow) --- *)

  (* Writes a plain workspace file at [base]/[rel], creating parents. *)
  let write_at ~base ~rel ~content =
    let full = Filename.concat base rel in
    Skills_tests.mkdir_p (Filename.dirname full);
    Skills_tests.write_file full content

  let text_of = function
    | Ok [ Mentat_llm.Content.Text s ] -> "text:" ^ s
    | Ok [] -> "blank"
    | Ok _ -> "other"
    | Error (`Unknown n) -> "unknown:" ^ n
    | Error (`File_unresolved e) ->
        Printf.sprintf "unresolved:%s:%s" e.Commands.path e.Commands.reason

  let at_file =
    group "@file"
      [
        test "a body @file embeds the workspace file's bytes" (fun () ->
            Skills_tests.with_base "at-happy" @@ fun ~stdenv ~base ->
            write_at ~base ~rel:"notes.txt" ~content:"the meeting notes";
            write_command ~base ~sub:".mentat/commands" ~path:"review.md"
              ~content:(command_doc ~body:"Notes: @notes.txt" ());
            let t = load ~stdenv ~base () in
            equal string "text:Notes: the meeting notes"
              (text_of (Commands.expand t ~name:(nm "review") ~arguments:"")));
        test "a namespaced-path @file resolves from the workspace root"
          (fun () ->
            Skills_tests.with_base "at-nested" @@ fun ~stdenv ~base ->
            write_at ~base ~rel:"src/foo.ml" ~content:"let x = 1";
            write_command ~base ~sub:".mentat/commands" ~path:"explain.md"
              ~content:(command_doc ~body:"@src/foo.ml" ());
            let t = load ~stdenv ~base () in
            equal string "text:let x = 1"
              (text_of (Commands.expand t ~name:(nm "explain") ~arguments:"")));
        test "a missing @file is a hard File_unresolved failure" (fun () ->
            Skills_tests.with_base "at-missing" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"c.md"
              ~content:(command_doc ~body:"See @gone.txt here" ());
            let t = load ~stdenv ~base () in
            equal string "unresolved:gone.txt:no such file"
              (text_of (Commands.expand t ~name:(nm "c") ~arguments:"")));
        test "an @file token in argument text is inert (no re-scan)" (fun () ->
            Skills_tests.with_base "at-inert" @@ fun ~stdenv ~base ->
            (* [secret] exists, but $1's value must not be resolved as a
               reference: the argument splices literally. *)
            write_at ~base ~rel:"secret" ~content:"leaked";
            write_command ~base ~sub:".mentat/commands" ~path:"echo.md"
              ~content:(command_doc ~body:"$1" ());
            let t = load ~stdenv ~base () in
            equal string "text:@secret"
              (text_of
                 (Commands.expand t ~name:(nm "echo") ~arguments:"@secret")));
        test "an @-reference inside embedded bytes is inert (no re-scan)"
          (fun () ->
            Skills_tests.with_base "at-file-inert-at" @@ fun ~stdenv ~base ->
            (* [inner.txt] exists with different bytes; embedding [outer.txt] must
               splice its literal [@inner.txt], not resolve inner.txt. *)
            write_at ~base ~rel:"inner.txt" ~content:"SECRET";
            write_at ~base ~rel:"outer.txt" ~content:"see @inner.txt here";
            write_command ~base ~sub:".mentat/commands" ~path:"c.md"
              ~content:(command_doc ~body:"@outer.txt" ());
            let t = load ~stdenv ~base () in
            equal string "text:see @inner.txt here"
              (text_of (Commands.expand t ~name:(nm "c") ~arguments:"")));
        test "a $-token inside embedded bytes is inert (no re-scan)" (fun () ->
            Skills_tests.with_base "at-file-inert-dollar"
            @@ fun ~stdenv ~base ->
            write_at ~base ~rel:"tpl.txt" ~content:"$1 and $ARGUMENTS";
            write_command ~base ~sub:".mentat/commands" ~path:"c.md"
              ~content:(command_doc ~body:"@tpl.txt" ());
            let t = load ~stdenv ~base () in
            equal string "text:$1 and $ARGUMENTS"
              (text_of (Commands.expand t ~name:(nm "c") ~arguments:"ARG")));
        test "adjacent @a@b is one greedy token, not two references" (fun () ->
            Skills_tests.with_base "at-adjacent-at" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"c.md"
              ~content:(command_doc ~body:"@a.txt@b.txt" ());
            let t = load ~stdenv ~base () in
            equal string "unresolved:a.txt@b.txt:no such file"
              (text_of (Commands.expand t ~name:(nm "c") ~arguments:"")));
        test "a $-token then an @-token compose ($1@a.txt)" (fun () ->
            Skills_tests.with_base "at-adjacent-dollar" @@ fun ~stdenv ~base ->
            write_at ~base ~rel:"a.txt" ~content:"FILE";
            write_command ~base ~sub:".mentat/commands" ~path:"c.md"
              ~content:(command_doc ~body:"$1@a.txt" ());
            let t = load ~stdenv ~base () in
            equal string "text:XFILE"
              (text_of (Commands.expand t ~name:(nm "c") ~arguments:"X")));
        test "@@ is not an escape: the second @ joins the path" (fun () ->
            Skills_tests.with_base "at-atat" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"c.md"
              ~content:(command_doc ~body:"@@x" ());
            let t = load ~stdenv ~base () in
            equal string "unresolved:@x:no such file"
              (text_of (Commands.expand t ~name:(nm "c") ~arguments:"")));
        test "a bare @ and an @ before whitespace stay literal" (fun () ->
            Skills_tests.with_base "at-literal" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"c.md"
              ~content:(command_doc ~body:"ask @ now then ping@" ());
            let t = load ~stdenv ~base () in
            equal string "text:ask @ now then ping@"
              (text_of (Commands.expand t ~name:(nm "c") ~arguments:"")));
        test "an @file that escapes the workspace via symlink is rejected"
          (fun () ->
            Skills_tests.with_base "at-escape" @@ fun ~stdenv ~base ->
            let outside = Filename.temp_file "mentat-outside" ".txt" in
            Fun.protect
              ~finally:(fun () -> try Unix.unlink outside with _ -> ())
              (fun () ->
                Skills_tests.write_file outside "outside the workspace";
                Unix.symlink outside (Filename.concat base "escape.txt");
                write_command ~base ~sub:".mentat/commands" ~path:"c.md"
                  ~content:(command_doc ~body:"@escape.txt" ());
                let t = load ~stdenv ~base () in
                equal string
                  "unresolved:escape.txt:resolves outside the workspace"
                  (text_of (Commands.expand t ~name:(nm "c") ~arguments:""))));
        test "an @file in an untrusted workspace does not resolve" (fun () ->
            Skills_tests.with_base "at-untrusted" @@ fun ~stdenv ~base ->
            (* The file exists, but an untrusted checkout's bytes stay out of the
               turn: only the always-discovered user command is active. *)
            write_at ~base ~rel:"notes.txt" ~content:"secret";
            write_command ~base ~sub:"userhome/commands" ~path:"u.md"
              ~content:(command_doc ~body:"@notes.txt" ());
            let t = load ~stdenv ~base ~trusted:false () in
            equal string "unresolved:notes.txt:workspace is not trusted"
              (text_of (Commands.expand t ~name:(nm "u") ~arguments:"")));
        test "an over-cap @file is embedded truncated with a marker" (fun () ->
            Skills_tests.with_base "at-truncate" @@ fun ~stdenv ~base ->
            let big = String.make 300_000 'x' in
            write_at ~base ~rel:"big.txt" ~content:big;
            write_command ~base ~sub:".mentat/commands" ~path:"c.md"
              ~content:(command_doc ~body:"@big.txt" ());
            let t = load ~stdenv ~base () in
            match Commands.expand t ~name:(nm "c") ~arguments:"" with
            | Ok [ Mentat_llm.Content.Text s ] ->
                is_true ~msg:"embedded is shorter than the source"
                  (String.length s < String.length big);
                is_true ~msg:"truncation marker present"
                  (Skills_tests.contains ~needle:"truncated by mentat" s)
            | other -> failf "expected truncated text, got %s" (text_of other));
      ]

  let projection =
    group "projection"
      [
        test "jsont round-trips a command's diagnostic projection" (fun () ->
            Skills_tests.with_base "jsont" @@ fun ~stdenv ~base ->
            write_command ~base ~sub:".mentat/commands" ~path:"review.md"
              ~content:
                (command_doc ~description:"Review" ~argument_hint:"<pr>"
                   ~extra:"model: x\n" ~body:"do it" ());
            let t = load ~stdenv ~base () in
            let c = Option.get (find t "review") in
            let c' =
              match Jsont.Json.encode Command.jsont c with
              | Error m -> failf "encode: %s" m
              | Ok json -> (
                  match Jsont.Json.decode Command.jsont json with
                  | Ok v -> v
                  | Error m -> failf "decode: %s" m)
            in
            equal string (Command.name c) (Command.name c');
            equal string
              (Command.kind_string (Command.kind c))
              (Command.kind_string (Command.kind c'));
            equal string
              (Command.state_string (Command.status c))
              (Command.state_string (Command.status c'));
            match (Command.status c, Command.status c') with
            | Command.Active a, Command.Active a' ->
                equal (option string) a.Command.description
                  a'.Command.description;
                equal (option string) a.Command.argument_hint
                  a'.Command.argument_hint;
                equal (list string) a.Command.ignored_keys
                  a'.Command.ignored_keys;
                equal int a.Command.bytes a'.Command.bytes;
                equal string a.Command.digest a'.Command.digest
            | _ -> failf "expected both projections active");
      ]

  let suite =
    [
      substitution;
      namespacing;
      precedence;
      gates;
      frontmatter;
      expansion;
      at_file;
      projection;
    ]
end

let () =
  run "mentat.context"
    (Context_tests.suite @ Skills_tests.suite @ Commands_tests.suite)
