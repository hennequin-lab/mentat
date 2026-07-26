type marker = Fixed of string | Unique of string
type rule = { re : Re.t; marker : marker }

let pcre pat marker = { re = Re.Pcre.re pat; marker }

(* A crash/boot-failure report path stamps its filename [<run_id>.log], where
   [run_id] is [<monotonic>-<pid>]: the pid keeps concurrent reports distinct but
   is non-reproducible, so fold the whole filename. The shape — 12+ hex, a dash,
   digits, then [.log] — occurs only in these report paths, so the rule cannot
   touch other output. It precedes the bare-digest rule so it claims the digest
   before that rule rewrites it in isolation. *)
let default =
  [
    pcre {|[0-9a-f]{12,}-[0-9]+\.log|} (Fixed "$DIGEST-$PID.log");
    pcre "[0-9a-f]{12,}" (Unique "$DIGEST");
    pcre {|127\.0\.0\.1:[0-9]{2,5}|} (Fixed "127.0.0.1:$PORT");
    pcre {|\bpid [0-9]+|} (Unique "pid $PID");
  ]

let times =
  [
    pcre {|\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:\d{2})|}
      (Fixed "$TS");
    pcre {|\b\d{13}\b|} (Fixed "$NOW");
  ]

(* Number distinct matches in first-seen order, then rewrite: a lone match keeps
   the bare marker, two or more get a 1-based numeric suffix, so cram goldens and
   TUI frames stay byte-identical. *)
let apply_unique compiled base s =
  let seen = Hashtbl.create 16 in
  let next = ref 0 in
  let note m =
    if not (Hashtbl.mem seen m) then (
      incr next;
      Hashtbl.add seen m !next)
  in
  ignore
    (Re.replace ~all:true compiled s ~f:(fun g ->
         note (Re.Group.get g 0);
         ""));
  let count = Hashtbl.length seen in
  Re.replace ~all:true compiled s ~f:(fun g ->
      let n = Hashtbl.find seen (Re.Group.get g 0) in
      if count <= 1 then base else base ^ string_of_int n)

let apply_rule s { re; marker } =
  let compiled = Re.compile re in
  match marker with
  | Fixed m -> Re.replace ~all:true compiled s ~f:(fun _ -> m)
  | Unique base -> apply_unique compiled base s

(* Every known spelling of the working directory folds to $TESTCASE_ROOT. Sort
   longest-first so a symlinked parent never shadows the canonical child, and
   quote each so path separators and metacharacters are matched literally. *)
let cwd_rules cwd =
  let canonical = try Sys.getcwd () with Sys_error _ -> cwd in
  let logical =
    match Sys.getenv_opt "PWD" with Some p when p <> "" -> [ p ] | _ -> []
  in
  cwd :: canonical :: logical
  |> List.filter (fun p -> p <> "")
  |> List.sort_uniq (fun a b -> compare (String.length b) (String.length a))
  |> List.map (fun p -> { re = Re.str p; marker = Fixed "$TESTCASE_ROOT" })

let apply ?(extra = []) ?cwd s =
  let cwd = match cwd with Some c -> c | None -> Sys.getcwd () in
  let pipeline = cwd_rules cwd @ default @ extra in
  List.fold_left apply_rule s pipeline
