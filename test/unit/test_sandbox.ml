(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Unit suite for [mentat_sandbox], the pure half of the command-confinement
   subsystem. The suite pins this library's security-boundary
   contracts: the pure golden generators (Seatbelt SBPL and Bubblewrap argv,
   asserted through [lower_argv] — the generators are library-internal), the
   fail-closed route constructors, the scratch-invariant confinement identity,
   the startup gate, and the obligation vocabulary the effect twin discharges.

   Everything here is a pure function of its inputs — no filesystem, no probe,
   no platform dependency — so a macOS profile golden verifies on Linux CI and
   back. Enforcement is injected as a required [(Backend.t, Error.t) result]
   {e selector}, never a caller-built profile, so a profile lowered from a
   different policy — and a forgotten probe outcome — are
   unrepresentable; the type system makes those tests unnecessary and we
   document them as such rather than forcing vacuous cases. *)

open Windtrap
module Sandbox = Mentat_sandbox
module Policy = Mentat_sandbox.Policy
module Evidence = Mentat_sandbox.Evidence
module Error = Mentat_sandbox.Error
module Backend = Mentat_sandbox.Backend
module Requirement = Mentat_sandbox.Requirement
module Identity = Mentat_sandbox.Identity
module Abs = Lpath.Abs
module Digest = Mentat_digest
module Json = Jsont.Json

(* Helpers *)

let abs path = Abs.of_string_exn path
let abs_value = testable ~pp:Abs.pp ~equal:Abs.equal ()
let policy_value = testable ~pp:Policy.pp ~equal:Policy.equal ()
let evidence_value = testable ~pp:Evidence.pp ~equal:Evidence.equal ()
let error_value = testable ~pp:Error.pp ~equal:Error.equal ()
let json_string s = Json.string s

let confined ?(scratch = abs "/tmp") ?(reads = Policy.All)
    ?(writable_roots = []) ?(protected_paths = [])
    ?(network = Policy.Network.Restricted) () =
  Policy.make ~scratch ~reads ~writable_roots ~protected_paths ~network

let sealed ?(backend = Backend.Seatbelt) policy =
  Sandbox.confined ~backend:(Ok backend) policy

let refused_sealed ?(error = Error.Unavailable "no backend") policy =
  Sandbox.confined ~backend:(Error error) policy

let identity_of ?backend policy = Sandbox.identity (sealed ?backend policy)

let lowered ?cwd sandbox ~program args =
  let cwd = match cwd with Some cwd -> cwd | None -> abs "/tmp" in
  match Sandbox.lower_argv sandbox ~cwd (program :: args) with
  | Ok tokens -> tokens
  | Error error -> fail (Error.message error)

(* The generators are library-internal; the golden surface is the lowered
   argv. For Seatbelt the SBPL document is the argv token after
   ["-p"] and each following ["-DKEY=VALUE"] token carries one (key, path)
   parameter; for Bubblewrap the enforcing prefix is the argv verbatim.
   [seatbelt_sbpl] lowers a probe command with the policy's scratch as cwd
   (always inside the read scope) and projects the (sbpl, params) pair back
   out of the argv. *)
let seatbelt_sbpl policy =
  let tokens =
    lowered
      (sealed ~backend:Backend.Seatbelt policy)
      ~cwd:(Policy.scratch policy) ~program:"cmd" []
  in
  match tokens with
  | "/usr/bin/sandbox-exec" :: "-p" :: sbpl :: rest ->
      let param token =
        match String.index_opt token '=' with
        | Some i when String.starts_with ~prefix:"-D" token ->
            ( String.sub token 2 (i - 2),
              String.sub token (i + 1) (String.length token - i - 1) )
        | _ -> failf "malformed -D parameter: %s" token
      in
      let rec params acc = function
        | [ "--"; "cmd" ] -> List.rev acc
        | token :: rest -> params (param token :: acc) rest
        | [] -> fail "seatbelt lowering lost the -- command tail"
      in
      (sbpl, params [] rest)
  | _ -> fail "seatbelt lowering must start with the executable and -p profile"

let bubblewrap_lowered ?cwd policy ~program args =
  lowered (sealed ~backend:Backend.Bubblewrap policy) ?cwd ~program args

(* Error. *)

let error_constructors_are_distinct () =
  let cases =
    [
      Error.Unavailable "x";
      Error.Cwd_outside_scope (abs "/x");
      Error.Escalation_denied;
      Error.Escalation_irrelevant;
      Error.Empty_program;
      Error.Nul_in_argv { index = 1 };
      Error.Stale_policy { path = abs "/x" };
    ]
  in
  List.iteri
    (fun i a ->
      List.iteri
        (fun j b ->
          if i = j then is_true ~msg:"an error equals itself" (Error.equal a b)
          else
            is_false ~msg:"distinct constructors are unequal" (Error.equal a b))
        cases)
    cases;
  is_false ~msg:"fields participate in equality"
    (Error.equal (Error.Unavailable "a") (Error.Unavailable "b"));
  is_false ~msg:"paths participate in equality"
    (Error.equal
       (Error.Stale_policy { path = abs "/a" })
       (Error.Stale_policy { path = abs "/b" }))

let error_messages_carry_fields () =
  equal string ~msg:"unavailable message is the probe diagnostic" "gone"
    (Error.message (Error.Unavailable "gone"));
  equal string ~msg:"cwd_outside_scope names the cwd"
    "working directory /elsewhere is outside the confined readable roots"
    (Error.message (Error.Cwd_outside_scope (abs "/elsewhere")));
  equal string ~msg:"stale_policy names the exact path"
    "resolved sandbox path /work/.git disappeared or changed kind after sealing"
    (Error.message (Error.Stale_policy { path = abs "/work/.git" }));
  equal string ~msg:"nul_in_argv names the token index"
    "argv token 1 contains a NUL byte"
    (Error.message (Error.Nul_in_argv { index = 1 }));
  equal string ~msg:"pp renders the message"
    (Error.message Error.Empty_program)
    (Format.asprintf "%a" Error.pp Error.Empty_program)

let error_json_shape () =
  let obj fields =
    Json.object'
      (List.map (fun (name, value) -> Json.mem (Json.name name) value) fields)
  in
  is_true ~msg:"unavailable json is kind and message"
    (Json.equal
       (obj
          [
            ("kind", json_string "unavailable"); ("message", json_string "gone");
          ])
       (Error.to_json (Error.Unavailable "gone")));
  is_true ~msg:"cwd_outside_scope json carries the structured path"
    (Json.equal
       (obj
          [
            ("kind", json_string "cwd_outside_scope");
            ( "message",
              json_string
                "working directory /elsewhere is outside the confined readable \
                 roots" );
            ("path", json_string "/elsewhere");
          ])
       (Error.to_json (Error.Cwd_outside_scope (abs "/elsewhere"))));
  is_true ~msg:"stale_policy json carries the structured path"
    (Json.equal
       (obj
          [
            ("kind", json_string "stale_policy");
            ( "message",
              json_string
                "resolved sandbox path /work/.git disappeared or changed kind \
                 after sealing" );
            ("path", json_string "/work/.git");
          ])
       (Error.to_json (Error.Stale_policy { path = abs "/work/.git" })));
  is_true ~msg:"nul_in_argv json carries the structured index"
    (Json.equal
       (obj
          [
            ("kind", json_string "nul_in_argv");
            ("message", json_string "argv token 1 contains a NUL byte");
            ("index", Json.int 1);
          ])
       (Error.to_json (Error.Nul_in_argv { index = 1 })));
  is_true ~msg:"escalation_denied json is kind and message"
    (Json.equal
       (obj
          [
            ("kind", json_string "escalation_denied");
            ("message", json_string (Error.message Error.Escalation_denied));
          ])
       (Error.to_json Error.Escalation_denied))

(* Policy. *)

let policy_normalizes_writable_roots () =
  let a =
    confined ~writable_roots:[ abs "/work"; abs "/tmp"; abs "/work" ] ()
  in
  let b = confined ~writable_roots:[ abs "/tmp"; abs "/work" ] () in
  equal policy_value ~msg:"duplicate roots dedup and order canonically" a b;
  equal (list abs_value) ~msg:"accessor reports canonical order"
    [ abs "/tmp"; abs "/work" ]
    (Policy.writable_roots a)

let policy_collapses_descendant_roots () =
  (* A root strictly within another is redundant and dropped (RFC L9). *)
  let policy =
    confined ~writable_roots:[ abs "/work"; abs "/work/sub"; abs "/other" ] ()
  in
  equal (list abs_value) ~msg:"descendant roots collapse into their ancestor"
    [ abs "/other"; abs "/work" ]
    (Policy.writable_roots policy)

let policy_distinguishes_network () =
  let restricted = confined () in
  let enabled = confined ~network:Policy.Network.Enabled () in
  is_false ~msg:"network state participates in equality"
    (Policy.equal restricted enabled);
  is_true ~msg:"default is restricted"
    (match Policy.network restricted with
    | Policy.Network.Restricted -> true
    | Policy.Network.Enabled -> false)

let policy_protected_paths_are_scoped () =
  let policy =
    confined
      ~writable_roots:[ abs "/private/tmp"; abs "/private/tmp/ws" ]
      ~protected_paths:[ abs "/outside/.mentat"; abs "/private/tmp/ws/.mentat" ]
      ()
  in
  equal (list abs_value)
    ~msg:"protected paths outside the writable roots are discarded"
    [ abs "/private/tmp/ws/.mentat" ]
    (Policy.protected_paths policy)

let policy_folds_writable_and_scratch_into_reads () =
  let policy =
    confined ~scratch:(abs "/scratch")
      ~reads:(Policy.Only [ abs "/data" ])
      ~writable_roots:[ abs "/work" ]
      ()
  in
  equal (list abs_value)
    ~msg:"Only reads include the read roots, writable roots, and the scratch"
    [ abs "/data"; abs "/scratch"; abs "/work" ]
    (match Policy.reads policy with
    | Policy.Only roots -> roots
    | Policy.All -> []);
  equal abs_value ~msg:"the scratch is an explicit policy field"
    (abs "/scratch") (Policy.scratch policy);
  is_false ~msg:"the scratch stays out of the writable-roots observer"
    (List.exists (Abs.equal (abs "/scratch")) (Policy.writable_roots policy))

let policy_scratch_participates_in_equality () =
  is_false ~msg:"policies differing only by scratch are unequal"
    (Policy.equal
       (confined ~scratch:(abs "/a") ())
       (confined ~scratch:(abs "/b") ()))

(* Generated antichain/coverage law for [Policy.make]'s root normalization
   (RFC L9). The result is strictly sorted, contains no root strictly within
   another (an antichain), and every input root is covered by some result
   root. *)

let path_component = Gen.oneofl [ "a"; "b"; "c"; "d" ]

let abs_path_gen =
  Gen.map
    (fun components -> abs ("/" ^ String.concat "/" components))
    (Gen.list_size (Gen.int_range 1 4) path_component)

let abs_path = testable ~pp:Abs.pp ~equal:Abs.equal ~gen:abs_path_gen ()
let root_list = list abs_path

let policy_root_normalization_is_an_antichain () =
  prop' "normalized writable roots form a sorted, covering antichain" root_list
    (fun roots ->
      let normalized =
        Policy.writable_roots (confined ~writable_roots:roots ())
      in
      let rec sorted_strict = function
        | a :: (b :: _ as rest) ->
            is_true ~msg:"strictly increasing" (Abs.compare a b < 0);
            sorted_strict rest
        | _ -> ()
      in
      sorted_strict normalized;
      List.iter
        (fun a ->
          List.iter
            (fun b ->
              if not (Abs.equal a b) then
                is_false ~msg:"no root is strictly within another"
                  (Abs.is_strictly_within ~root:a b))
            normalized)
        normalized;
      List.iter
        (fun input ->
          is_true ~msg:"every input root is covered by a result root"
            (List.exists (fun root -> Abs.is_within ~root input) normalized))
        roots)

(* Backend / Requirement / Network vocabulary. *)

let backend_identity () =
  equal string ~msg:"seatbelt id" "macos-seatbelt" (Backend.id Backend.Seatbelt);
  equal string ~msg:"bubblewrap id" "linux-bubblewrap"
    (Backend.id Backend.Bubblewrap);
  equal (list string) ~msg:"all backends in stable order"
    [ "macos-seatbelt"; "linux-bubblewrap" ]
    (List.map Backend.id Backend.all)

let backend_of_id_round_trips () =
  List.iter
    (fun backend ->
      is_true ~msg:"of_id inverts id"
        (match Backend.of_id (Backend.id backend) with
        | Some decoded -> Backend.equal backend decoded
        | None -> false))
    Backend.all;
  is_true ~msg:"unknown id is None" (Backend.of_id "windows-appcontainer" = None)

let backend_executables_are_fixed_and_absolute () =
  (* Hardening #3: fixed absolute enforcing executables; PATH cannot inject. *)
  equal string ~msg:"absolute sandbox-exec path" "/usr/bin/sandbox-exec"
    (Backend.executable Backend.Seatbelt);
  equal string ~msg:"absolute bwrap path" "/usr/bin/bwrap"
    (Backend.executable Backend.Bubblewrap)

let requirement_round_trips () =
  List.iter
    (fun r ->
      is_true ~msg:"of_string inverts to_string"
        (Requirement.of_string (Requirement.to_string r) = Some r))
    Requirement.all;
  equal (list string) ~msg:"configuration spellings"
    [ "off"; "enforced-or-external"; "enforced" ]
    (List.map Requirement.to_string Requirement.all);
  is_true ~msg:"unknown spelling is None" (Requirement.of_string "loose" = None)

let network_round_trips () =
  List.iter
    (fun n ->
      is_true ~msg:"of_string inverts to_string"
        (Policy.Network.of_string (Policy.Network.to_string n) = Some n))
    Policy.Network.all;
  is_true ~msg:"unknown spelling is None" (Policy.Network.of_string "vpn" = None)

(* Seatbelt SBPL generation (golden surface, through lower_argv). *)

(* Exact variable sections. The fixed base policy is a platform-independent
   constant; the read/write/network sections are what a policy generates, and
   each appears verbatim in the joined SBPL text. Pinning the full section
   string (not a loose fragment) is the exact-text golden. *)

let all_read_section = {|(allow file-read*)
(allow file-map-executable)|}

let read_only_write_section =
  {|(allow file-write*
(subpath (param "WRITABLE_ROOT_0"))
)|}

let scoped_read_section =
  {|(allow file-read* file-test-existence file-map-executable
(literal (param "READABLE_ROOT_0")) (subpath (param "READABLE_ROOT_0")) (literal (param "READABLE_ROOT_1")) (subpath (param "READABLE_ROOT_1")) (literal (param "READABLE_ROOT_2")) (subpath (param "READABLE_ROOT_2"))
)|}

let scoped_write_section =
  {|(allow file-write*
(subpath (param "WRITABLE_ROOT_0")) (subpath (param "WRITABLE_ROOT_1"))
)|}

(* The Unix-domain socket allow scopes [network-bind]/[network-outbound] to the
   writable roots by pathname, so build tools (dune's [_build/.rpc/dune] RPC
   socket) work while an INET bind or connect — which carries no pathname — stays
   subject to the network posture. It reuses the [WRITABLE_ROOT_%d] parameters,
   introducing no new [-D] parameter. *)
let unix_socket_read_only_section =
  {|(allow network-bind network-outbound
(subpath (param "WRITABLE_ROOT_0"))
)|}

let unix_socket_scoped_section =
  {|(allow network-bind network-outbound
(subpath (param "WRITABLE_ROOT_0")) (subpath (param "WRITABLE_ROOT_1"))
)|}

let seatbelt_read_only_golden () =
  let text, params = seatbelt_sbpl (confined ()) in
  is_true ~msg:"profile starts closed-by-default"
    (String.includes ~affix:"(deny default)" text);
  is_true ~msg:"host-wide reads section is exact"
    (String.includes ~affix:all_read_section text);
  is_true ~msg:"read-only write section permits only the private scratch"
    (String.includes ~affix:read_only_write_section text);
  is_true
    ~msg:
      "local Unix-socket IPC is admitted under the scratch, restricted or not"
    (String.includes ~affix:unix_socket_read_only_section text);
  equal
    (list (pair string string))
    ~msg:"read-only binds only the scratch root"
    [ ("WRITABLE_ROOT_0", "/tmp") ]
    params;
  is_false
    ~msg:"restricted network opens no INET boundary (no blanket outbound rule)"
    (String.includes ~affix:"(allow network-outbound)" text)

let seatbelt_scoped_reads_golden () =
  let policy =
    confined
      ~reads:(Policy.Only [ abs "/work"; abs "/opt/ocaml" ])
      ~writable_roots:[ abs "/work" ]
      ()
  in
  let text, params = seatbelt_sbpl policy in
  is_false ~msg:"scoped reads drop the host-wide read rule"
    (String.includes ~affix:"(allow file-read*)" text);
  is_true ~msg:"scoped read section is exact"
    (String.includes ~affix:scoped_read_section text);
  is_true ~msg:"scoped write section is exact"
    (String.includes ~affix:scoped_write_section text);
  is_true ~msg:"the Unix-socket allow spans the scratch and the writable root"
    (String.includes ~affix:unix_socket_scoped_section text);
  equal
    (list (pair string string))
    ~msg:"every normalized read and write root is a parameter"
    [
      ("READABLE_ROOT_0", "/opt/ocaml");
      ("READABLE_ROOT_1", "/tmp");
      ("READABLE_ROOT_2", "/work");
      ("WRITABLE_ROOT_0", "/tmp");
      ("WRITABLE_ROOT_1", "/work");
    ]
    params

let seatbelt_carveout_golden () =
  let policy =
    confined
      ~writable_roots:[ abs "/usr"; abs "/tmp" ]
      ~protected_paths:[ abs "/usr/bin"; abs "/usr/lib"; abs "/usr/share" ]
      ()
  in
  let text, params = seatbelt_sbpl policy in
  equal
    (list (pair string string))
    ~msg:"params bind each root and every carveout beneath it, in order"
    [
      ("WRITABLE_ROOT_0", "/tmp");
      ("WRITABLE_ROOT_1", "/tmp");
      ("WRITABLE_ROOT_2", "/usr");
      ("WRITABLE_ROOT_2_EXCLUDED_0", "/usr/bin");
      ("WRITABLE_ROOT_2_EXCLUDED_1", "/usr/lib");
      ("WRITABLE_ROOT_2_EXCLUDED_2", "/usr/share");
    ]
    params;
  is_true ~msg:"a carved-out root uses a require-all with require-not clauses"
    (String.includes
       ~affix:
         "(require-all (subpath (param \"WRITABLE_ROOT_2\")) (require-not \
          (literal (param \"WRITABLE_ROOT_2_EXCLUDED_0\"))) (require-not \
          (subpath (param \"WRITABLE_ROOT_2_EXCLUDED_0\")))"
       text)

let seatbelt_nested_roots_share_carveouts () =
  (* A workspace nested under another writable root must not have its protected
     metadata reachable through the enclosing root (hardening #10). *)
  let policy =
    confined
      ~writable_roots:[ abs "/private/tmp"; abs "/private/tmp/ws" ]
      ~protected_paths:[ abs "/private/tmp/ws/.git" ]
      ()
  in
  let _text, params = seatbelt_sbpl policy in
  is_true ~msg:"the enclosing root carves out the nested root's .git"
    (List.exists
       (fun (key, value) ->
         String.equal value "/private/tmp/ws/.git"
         && String.starts_with ~prefix:"WRITABLE_ROOT_1" key)
       params)

let seatbelt_network_enabled_golden () =
  let text, _ = seatbelt_sbpl (confined ~network:Policy.Network.Enabled ()) in
  is_true ~msg:"enabled network allows outbound"
    (String.includes ~affix:"(allow network-outbound)" text);
  is_true ~msg:"enabled network admits inbound"
    (String.includes ~affix:"(allow network-inbound)" text);
  is_true ~msg:"enabled network admits the TLS platform services"
    (String.includes ~affix:"com.apple.SecurityServer" text)

let seatbelt_no_path_enters_the_text () =
  (* Hardening #1: no path byte enters the SBPL text; every resolved path is a
     [-D] parameter. Distinctive paths never present in the fixed base policy
     make the property observable. *)
  let paths =
    [ "/data/secret"; "/scratch/xyz"; "/work/project"; "/work/project/.git" ]
  in
  let policy =
    confined ~scratch:(abs "/scratch/xyz")
      ~reads:(Policy.Only [ abs "/data/secret" ])
      ~writable_roots:[ abs "/work/project" ]
      ~protected_paths:[ abs "/work/project/.git" ]
      ()
  in
  let text, params = seatbelt_sbpl policy in
  let values = List.map snd params in
  List.iter
    (fun path ->
      is_false
        ~msg:(Printf.sprintf "no path %s appears in the SBPL text" path)
        (String.includes ~affix:path text);
      is_true
        ~msg:(Printf.sprintf "path %s travels as a -D parameter" path)
        (List.mem path values))
    paths

let seatbelt_generation_is_deterministic () =
  let policy =
    confined
      ~writable_roots:[ abs "/usr"; abs "/tmp" ]
      ~protected_paths:[ abs "/usr/bin" ]
      ()
  in
  is_true ~msg:"equal policies produce byte-equal text and params"
    (seatbelt_sbpl policy = seatbelt_sbpl policy)

(* Bubblewrap argv generation (golden surface, through lower_argv). *)

let namespace_prefix =
  [
    "/usr/bin/bwrap";
    "--new-session";
    "--die-with-parent";
    "--unshare-user";
    "--unshare-pid";
  ]

let bubblewrap_read_only_golden () =
  equal (list string) ~msg:"read-only bubblewrap lowering is exact"
    (namespace_prefix
    @ [
        "--ro-bind";
        "/";
        "/";
        "--dev";
        "/dev";
        "--bind";
        "/tmp";
        "/tmp";
        "--unshare-net";
        "--proc";
        "/proc";
        "--chdir";
        "/tmp";
        "--";
        "true";
      ])
    (bubblewrap_lowered (confined ()) ~cwd:(abs "/tmp") ~program:"true" [])

let bubblewrap_scoped_reads_golden () =
  let policy =
    confined
      ~reads:(Policy.Only [ abs "/usr" ])
      ~writable_roots:[ abs "/usr" ]
      ~protected_paths:[ abs "/usr/bin" ]
      ()
  in
  let args = bubblewrap_lowered policy ~cwd:(abs "/usr") ~program:"true" [] in
  equal (list string) ~msg:"scoped-read bubblewrap lowering is exact"
    (namespace_prefix
    @ [
        "--tmpfs";
        "/";
        "--dev";
        "/dev";
        "--ro-bind";
        "/tmp";
        "/tmp";
        "--ro-bind";
        "/usr";
        "/usr";
        "--bind";
        "/tmp";
        "/tmp";
        "--bind";
        "/usr";
        "/usr";
        "--ro-bind";
        "/usr/bin";
        "/usr/bin";
        "--unshare-net";
        "--proc";
        "/proc";
        "--remount-ro";
        "/";
        "--chdir";
        "/usr";
        "--";
        "true";
      ])
    args;
  is_false ~msg:"scoped reads never expose the host root via --ro-bind / /"
    (List.exists (String.equal "--tmpfs") args
    &&
    let rec seq = function
      | "--ro-bind" :: "/" :: "/" :: _ -> true
      | _ :: rest -> seq rest
      | [] -> false
    in
    seq args)

let bubblewrap_network_enabled_golden () =
  let args =
    bubblewrap_lowered
      (confined ~network:Policy.Network.Enabled ())
      ~cwd:(abs "/tmp") ~program:"true" []
  in
  is_false ~msg:"enabled network does not unshare the net namespace"
    (List.exists (String.equal "--unshare-net") args)

(* The [Only] root is a writable tmpfs, so an unbound write would land in it and
   report success. The seal closes that, and it must trail [--proc]/[--dev],
   which bwrap cannot create under a sealed root. [All] binds its root read-only
   already and takes no seal. *)
let bubblewrap_seals_a_scoped_root () =
  let scoped =
    bubblewrap_lowered
      (confined ~reads:(Policy.Only [ abs "/usr" ]) ())
      ~cwd:(abs "/usr") ~program:"true" []
  in
  let rec index_of needle index = function
    | [] -> None
    | x :: _ when String.equal x needle -> Some index
    | _ :: rest -> index_of needle (index + 1) rest
  in
  (match (index_of "--remount-ro" 0 scoped, index_of "--proc" 0 scoped) with
  | Some seal, Some proc ->
      is_true ~msg:"the scoped root is sealed read-only" (seal > proc)
  | _ -> fail "a scoped bubblewrap lowering must seal its root after --proc");
  is_false ~msg:"an all-reads root is bound read-only and takes no seal"
    (List.exists
       (String.equal "--remount-ro")
       (bubblewrap_lowered (confined ()) ~cwd:(abs "/tmp") ~program:"true" []))

let bubblewrap_uses_strict_ro_bind () =
  (* Hardening #2: carveouts use strict [--ro-bind], never [--ro-bind-try],
     which would fail open on a mid-flight deletion. *)
  let policy =
    confined
      ~writable_roots:[ abs "/work" ]
      ~protected_paths:[ abs "/work/.git" ]
      ()
  in
  let args = bubblewrap_lowered policy ~cwd:(abs "/tmp") ~program:"true" [] in
  is_false ~msg:"no --ro-bind-try token is ever emitted"
    (List.exists (String.equal "--ro-bind-try") args);
  is_false ~msg:"no token carries a -try suffix"
    (List.exists (fun a -> String.includes ~affix:"-try" a) args);
  is_true ~msg:"the protected carveout is a strict --ro-bind"
    (let rec seq = function
       | "--ro-bind" :: "/work/.git" :: "/work/.git" :: _ -> true
       | _ :: rest -> seq rest
       | [] -> false
     in
     seq args)

(* Route constructors — evidence table, fail-closed refusal, coupling. *)

let workspace_policy = confined ~writable_roots:[ abs "/work" ] ()

let route_evidence_table () =
  (* RFC L2: the backend-outcome -> evidence class table, fixed at seal time.
     A forgotten probe outcome is a type error ([~backend] is required), so the
     old fail-closed-default case is unrepresentable rather than tested. *)
  (match Sandbox.evidence (sealed workspace_policy) with
  | Evidence.Enforced { backend; _ } ->
      equal string ~msg:"confined+Ok -> Enforced with the backend id"
        "macos-seatbelt" (Backend.id backend)
  | _ -> fail "confined+Ok should be Enforced evidence");
  (match Sandbox.evidence (refused_sealed workspace_policy) with
  | Evidence.Refused error ->
      equal error_value ~msg:"confined+Error -> Refused with the error"
        (Error.Unavailable "no backend") error
  | _ -> fail "confined+Error should be Refused evidence");
  is_true ~msg:"direct -> Not_requested"
    (match Sandbox.evidence Sandbox.direct with
    | Evidence.Not_requested -> true
    | _ -> false);
  is_true ~msg:"external -> Declared_external"
    (match Sandbox.evidence Sandbox.external_ with
    | Evidence.Declared_external -> true
    | _ -> false)

let route_policy_projection () =
  (match Sandbox.policy (sealed workspace_policy) with
  | Some policy ->
      equal policy_value ~msg:"an enforced route carries its policy"
        workspace_policy policy
  | None -> fail "an enforced route must carry a policy");
  (match Sandbox.policy (refused_sealed workspace_policy) with
  | Some policy ->
      equal policy_value ~msg:"a refused route still carries its policy"
        workspace_policy policy
  | None -> fail "a refused route must carry a policy");
  is_true ~msg:"direct has no policy" (Sandbox.policy Sandbox.direct = None);
  is_true ~msg:"external has no policy" (Sandbox.policy Sandbox.external_ = None)

let evidence_is_fixed_across_lowerings () =
  let sandbox = sealed workspace_policy in
  let first = lowered sandbox ~cwd:(abs "/work") ~program:"true" [] in
  let second = lowered sandbox ~cwd:(abs "/work/sub") ~program:"true" [] in
  is_true ~msg:"lowering returns argv, not evidence"
    (first <> [] && second <> []);
  equal evidence_value ~msg:"the sealed evidence is identical across lowerings"
    (Sandbox.evidence sandbox) (Sandbox.evidence sandbox)

let refused_route_refuses_every_command () =
  (* RFC L3: an unavailable backend refuses every command — fail-closed. *)
  let sandbox = refused_sealed workspace_policy in
  match Sandbox.lower_argv sandbox ~cwd:(abs "/work") [ "true" ] with
  | Error error ->
      equal error_value ~msg:"the lowering error is the sealed refusal"
        (Error.Unavailable "no backend") error
  | Ok _ -> fail "a refused route must lower nothing"

let route_couples_profile_to_policy () =
  (* RFC L5: the constructor receives a Backend selector, never a profile, so
     the sealed policy, evidence, obligations, and identity always describe the
     same policy. Injecting a foreign profile is unrepresentable at the type
     level; the observable coupling is that the sealed policy is exactly the
     input and the evidence backend matches the injected selector. *)
  List.iter
    (fun backend ->
      let sandbox = sealed ~backend workspace_policy in
      (match Sandbox.policy sandbox with
      | Some policy ->
          equal policy_value ~msg:"the sealed policy is exactly the input"
            workspace_policy policy
      | None -> fail "a confined route must carry a policy");
      match Sandbox.evidence sandbox with
      | Evidence.Enforced { backend = recorded; _ } ->
          is_true ~msg:"evidence backend matches the injected selector"
            (Backend.equal backend recorded)
      | _ -> fail "an enforced seal should carry Enforced evidence")
    Backend.all

let digest_stability_across_construction () =
  (* Hardening #17: equal policy => equal profile => equal Evidence and
     Identity digest; a different policy differs. Two policies built from
     differently ordered, duplicated inputs normalize to the same value. *)
  let a =
    confined
      ~writable_roots:[ abs "/usr"; abs "/tmp" ]
      ~protected_paths:[ abs "/usr/bin"; abs "/usr/lib" ]
      ()
  in
  let b =
    confined
      ~writable_roots:[ abs "/tmp"; abs "/usr"; abs "/usr" ]
      ~protected_paths:[ abs "/usr/lib"; abs "/usr/bin" ]
      ()
  in
  is_true ~msg:"differently-ordered inputs normalize to an equal policy"
    (Policy.equal a b);
  is_true ~msg:"equal policies produce an equal enforced-profile digest"
    (Evidence.equal (Sandbox.evidence (sealed a)) (Sandbox.evidence (sealed b)));
  is_true ~msg:"equal policies produce an equal identity"
    (Identity.equal (identity_of a) (identity_of b));
  is_false
    ~msg:"a different policy produces a different enforced-profile digest"
    (Evidence.equal
       (Sandbox.evidence (sealed a))
       (Sandbox.evidence (sealed (confined ()))))

let sealing_is_pure_and_total () =
  (* RFC L1: equal (policy, backend outcome) give equal seals in every
     projection. *)
  let a = sealed ~backend:Backend.Bubblewrap workspace_policy in
  let b = sealed ~backend:Backend.Bubblewrap workspace_policy in
  equal evidence_value ~msg:"equal evidence" (Sandbox.evidence a)
    (Sandbox.evidence b);
  is_true ~msg:"equal identity"
    (Identity.equal (Sandbox.identity a) (Sandbox.identity b));
  is_true ~msg:"equal escalation stance"
    (match (Sandbox.escalation a, Sandbox.escalation b) with
    | Sandbox.Available, Sandbox.Available -> true
    | _ -> false)

(* Escalation stance. *)

let escalation_stances () =
  is_true ~msg:"workspace-write is escalation-available"
    (match Sandbox.escalation (sealed workspace_policy) with
    | Sandbox.Available -> true
    | _ -> false);
  is_true ~msg:"read-only denies escalation"
    (match Sandbox.escalation (sealed (confined ())) with
    | Sandbox.Denied Error.Escalation_denied -> true
    | _ -> false);
  is_true ~msg:"direct ignores escalation"
    (match Sandbox.escalation Sandbox.direct with
    | Sandbox.Ignored -> true
    | _ -> false);
  is_true ~msg:"external ignores escalation"
    (match Sandbox.escalation Sandbox.external_ with
    | Sandbox.Ignored -> true
    | _ -> false)

let escalation_is_shape_based_not_availability_based () =
  (* Escalation is a permission-gated unconfined escape determined by the policy
     shape (workspace-write), orthogonal to whether the backend enforced. A
     refused workspace-write seal still reports Available, because reaching an
     escalated launch requires a separate approval and drops the sandbox
     entirely. *)
  is_true ~msg:"a refused workspace-write seal is still escalation-available"
    (match Sandbox.escalation (refused_sealed workspace_policy) with
    | Sandbox.Available -> true
    | _ -> false)

(* lower_argv / lower_escalated_argv. *)

let lower_argv_appends_command_verbatim () =
  (* The enforcing prefix cannot rewrite, reorder, or drop the wrapped command:
     the lowered argv always ends with program :: args verbatim, even when the
     tokens are empty strings or shell metacharacters. *)
  let command = [ "sh"; "-c"; "rm -rf / ; echo $HOME `id`"; "" ] in
  let seatbelt =
    lowered (sealed workspace_policy) ~cwd:(abs "/work") ~program:"sh"
      [ "-c"; "rm -rf / ; echo $HOME `id`"; "" ]
  in
  let tail tokens =
    let n = List.length tokens in
    List.filteri (fun i _ -> i >= n - 4) tokens
  in
  equal (list string) ~msg:"seatbelt lowering ends with the command verbatim"
    command (tail seatbelt);
  is_true ~msg:"seatbelt lowering separates the command with --"
    (List.exists (String.equal "--") seatbelt)

let lower_argv_enforces_cwd_containment () =
  (* RFC L9 / hardening #8: a confined cwd must be within the readable roots.
     Purely lexical: no filesystem is touched. *)
  let sandbox =
    sealed
      (confined
         ~reads:(Policy.Only [ abs "/work" ])
         ~writable_roots:[ abs "/work" ]
         ())
  in
  is_ok ~msg:"a cwd inside the read roots is accepted"
    (Sandbox.lower_argv sandbox ~cwd:(abs "/work/sub") [ "true" ]);
  match Sandbox.lower_argv sandbox ~cwd:(abs "/elsewhere") [ "true" ] with
  | Error error ->
      equal error_value
        ~msg:"a cwd outside the read roots is Cwd_outside_scope with the cwd"
        (Error.Cwd_outside_scope (abs "/elsewhere"))
        error
  | Ok _ -> fail "an out-of-scope cwd must be refused"

let lower_argv_passes_unconfined_routes_verbatim () =
  equal (list string) ~msg:"direct lowering is the command verbatim"
    [ "sh"; "-c"; "true" ]
    (lowered Sandbox.direct ~cwd:(abs "/anywhere") ~program:"sh"
       [ "-c"; "true" ]);
  equal (list string) ~msg:"external lowering is the command verbatim"
    [ "true" ]
    (lowered Sandbox.external_ ~cwd:(abs "/x") ~program:"true" [])

let lower_argv_validates_argv () =
  (* OS argv cannot represent an empty program or NUL bytes. Validation mints
     structured errors on every route; its enforcement home is the single
     launch boundary, which lowers every command through here. *)
  let check ~msg lower =
    (match lower [] with
    | Error Error.Empty_program -> ()
    | _ -> fail (msg ^ ": empty argv must be Empty_program"));
    (match lower [ ""; "a" ] with
    | Error Error.Empty_program -> ()
    | _ -> fail (msg ^ ": empty program must be Empty_program"));
    (match lower [ "pro\000gram" ] with
    | Error (Error.Nul_in_argv { index = 0 }) -> ()
    | _ -> fail (msg ^ ": NUL in the program must be Nul_in_argv index 0"));
    match lower [ "prog"; "ok"; "a\000b" ] with
    | Error (Error.Nul_in_argv { index = 2 }) -> ()
    | _ -> fail (msg ^ ": NUL in an argument must be Nul_in_argv at its index")
  in
  let ordinary sandbox argv =
    Sandbox.lower_argv sandbox ~cwd:(abs "/work") argv
  in
  check ~msg:"confined" (ordinary (sealed workspace_policy));
  check ~msg:"refused" (ordinary (refused_sealed workspace_policy));
  check ~msg:"direct" (ordinary Sandbox.direct);
  check ~msg:"external" (ordinary Sandbox.external_);
  check ~msg:"escalated" (fun argv ->
      Sandbox.lower_escalated_argv (sealed workspace_policy) ~cwd:(abs "/x")
        argv)

let lower_escalated_drops_containment_never_prefixes () =
  (* RFC L6: escalation escapes the profile, never the environment.
     The sandbox-side fact is that an escalated lowering never emits an
     enforcing prefix; the exact-environment half is enforced at the launch
     boundary, which passes an escalated command the same stripped
     environment as a confined one. It also applies no read-root cwd
     containment: the same out-of-scope cwd lower_argv rejects is accepted. *)
  let policy =
    confined
      ~reads:(Policy.Only [ abs "/work" ])
      ~writable_roots:[ abs "/work" ]
      ()
  in
  let sandbox = sealed policy in
  let out_of_scope = abs "/outside" in
  is_true ~msg:"lower_argv rejects the out-of-scope cwd"
    (Result.is_error (Sandbox.lower_argv sandbox ~cwd:out_of_scope [ "true" ]));
  match Sandbox.lower_escalated_argv sandbox ~cwd:out_of_scope [ "true" ] with
  | Ok tokens ->
      equal (list string) ~msg:"escalated lowering is the command verbatim"
        [ "true" ] tokens;
      is_true ~msg:"escalated evidence is Not_requested (the sandbox-side law)"
        (match Sandbox.escalated_evidence sandbox with
        | Evidence.Not_requested -> true
        | _ -> false)
  | Error error -> fail (Error.message error)

let lower_escalated_refuses_denied_and_ignored () =
  (match
     Sandbox.lower_escalated_argv
       (sealed (confined ()))
       ~cwd:(abs "/tmp") [ "true" ]
   with
  | Error Error.Escalation_denied ->
      (* Semantic pin: the read-only refusal message is product-neutral — no
         --sandbox flag hint leaks from the library. *)
      equal string ~msg:"read-only escalation refusal message"
        "the sealed policy has no writable roots: a read-only sandbox admits \
         no escalation"
        (Error.message Error.Escalation_denied)
  | _ -> fail "read-only escalation must be Escalation_denied");
  match
    Sandbox.lower_escalated_argv Sandbox.direct ~cwd:(abs "/tmp") [ "true" ]
  with
  | Error Error.Escalation_irrelevant -> ()
  | _ -> fail "direct escalation must be Escalation_irrelevant"

(* obligations. *)

let obligation_repr o =
  ( (match Sandbox.Obligation.kind o with
    | Sandbox.Obligation.Exists -> "exists"
    | Sandbox.Obligation.Directory -> "directory"),
    Sandbox.Obligation.path o )

let obligations_enumerate_paths_and_kinds () =
  (* RFC L10: readable and protected paths are Exists; writable roots and the
     scratch are Directory; deduplicated by path keeping the strongest kind, in
     canonical path order. The scratch appears as both a readable root (folded)
     and a directory, so its single obligation must be Directory. *)
  let policy =
    confined ~scratch:(abs "/private/scratch")
      ~reads:(Policy.Only [ abs "/data" ])
      ~writable_roots:[ abs "/work" ]
      ~protected_paths:[ abs "/work/.git" ]
      ()
  in
  let obligations =
    List.map obligation_repr (Sandbox.obligations (sealed policy))
  in
  equal
    (list (pair string abs_value))
    ~msg:
      "obligations enumerate the right paths and kinds, scratch deduped strong"
    [
      ("exists", abs "/data");
      ("directory", abs "/private/scratch");
      ("directory", abs "/work");
      ("exists", abs "/work/.git");
    ]
    obligations

let obligations_are_empty_for_unconfined_and_refused () =
  is_true ~msg:"direct has no obligations"
    (Sandbox.obligations Sandbox.direct = []);
  is_true ~msg:"external has no obligations"
    (Sandbox.obligations Sandbox.external_ = []);
  is_true ~msg:"a refused confined seal has no obligations"
    (Sandbox.obligations (refused_sealed workspace_policy) = [])

(* admits — the startup gate (3x4 requirement x evidence table). *)

let admit_ok req sandbox msg =
  is_true ~msg (Result.is_ok (Sandbox.admits req sandbox))

let admit_unenforceable req sandbox msg =
  is_true ~msg
    (match Sandbox.admits req sandbox with
    | Error (Requirement.Rejection.Unenforceable _) -> true
    | _ -> false)

let admit_external req sandbox msg =
  is_true ~msg
    (match Sandbox.admits req sandbox with
    | Error Requirement.Rejection.External_not_enforced -> true
    | _ -> false)

let admits_reproduces_the_host_gate () =
  (* RFC L7 / hardening #4: the sealed-evidence gate, over all 3x4 cells. *)
  let enforced = sealed workspace_policy in
  let refused = refused_sealed workspace_policy in
  let not_requested = Sandbox.direct in
  let external_ = Sandbox.external_ in
  (* Off admits every posture. *)
  admit_ok Requirement.Off enforced "off admits enforced";
  admit_ok Requirement.Off refused "off admits refused";
  admit_ok Requirement.Off not_requested "off admits not_requested";
  admit_ok Requirement.Off external_ "off admits external";
  (* Enforced_or_external rejects only a refused confinement. *)
  admit_ok Requirement.Enforced_or_external enforced "e-or-x admits enforced";
  admit_ok Requirement.Enforced_or_external external_ "e-or-x admits external";
  admit_ok Requirement.Enforced_or_external not_requested
    "e-or-x admits not_requested";
  admit_unenforceable Requirement.Enforced_or_external refused
    "e-or-x rejects refused as unenforceable";
  (* Enforced rejects refused and external. *)
  admit_ok Requirement.Enforced enforced "enforced admits enforced";
  admit_ok Requirement.Enforced not_requested
    "enforced admits not_requested (explicit unconfined mode)";
  admit_external Requirement.Enforced external_
    "enforced rejects external as external_not_enforced";
  admit_unenforceable Requirement.Enforced refused
    "enforced rejects refused as unenforceable"

(* Evidence codec. *)

let enforced_profile_digest sandbox =
  match Sandbox.evidence sandbox with
  | Evidence.Enforced { profile; _ } -> profile
  | _ -> fail "expected enforced evidence"

let evidence_json_shapes () =
  let obj fields =
    Json.object'
      (List.map (fun (name, value) -> Json.mem (Json.name name) value) fields)
  in
  is_true ~msg:"not_requested json"
    (Json.equal
       (obj [ ("kind", json_string "not_requested") ])
       (Evidence.to_json (Sandbox.evidence Sandbox.direct)));
  is_true ~msg:"declared_external json"
    (Json.equal
       (obj [ ("kind", json_string "declared_external") ])
       (Evidence.to_json (Sandbox.evidence Sandbox.external_)));
  (let sandbox = sealed workspace_policy in
   let profile = enforced_profile_digest sandbox in
   is_true ~msg:"enforced json carries backend and profile_hash"
     (Json.equal
        (obj
           [
             ("kind", json_string "enforced");
             ("backend", json_string "macos-seatbelt");
             ("profile_hash", json_string (Digest.to_hex profile));
           ])
        (Evidence.to_json (Sandbox.evidence sandbox))));
  (* Refused carries both a display "reason" and the structured "error" a
     consumer branches on by constructor. Stale_policy is the twin-minted case:
     a failed obligation discharge speaks this vocabulary. *)
  let error = Error.Stale_policy { path = abs "/work" } in
  match Evidence.to_json (Evidence.refused error) with
  | Jsont.Object (members, _) ->
      is_true ~msg:"refused kind"
        (match Json.find_mem "kind" members with
        | Some (_, Jsont.String (v, _)) -> String.equal v "refused"
        | _ -> false);
      is_true ~msg:"refused carries a display reason"
        (match Json.find_mem "reason" members with
        | Some (_, Jsont.String (v, _)) -> String.equal v (Error.message error)
        | _ -> false);
      is_true ~msg:"refused carries the structured error"
        (match Json.find_mem "error" members with
        | Some (_, structured) -> Json.equal structured (Error.to_json error)
        | None -> false)
  | _ -> fail "refused evidence must encode to a JSON object"

let evidence_cases_are_distinct () =
  let cases =
    [
      Sandbox.evidence Sandbox.direct;
      Sandbox.evidence (sealed workspace_policy);
      Evidence.refused (Error.Unavailable "x");
      Sandbox.evidence Sandbox.external_;
    ]
  in
  List.iteri
    (fun i a ->
      List.iteri
        (fun j b ->
          if i = j then
            is_true ~msg:"evidence equals itself" (Evidence.equal a b)
          else
            is_false ~msg:"distinct postures are unequal" (Evidence.equal a b))
        cases)
    cases;
  is_false ~msg:"enforced differs by backend"
    (Evidence.equal
       (Sandbox.evidence (sealed workspace_policy))
       (Sandbox.evidence (sealed ~backend:Backend.Bubblewrap workspace_policy)));
  is_false ~msg:"refused differs by error"
    (Evidence.equal
       (Evidence.refused (Error.Unavailable "x"))
       (Evidence.refused (Error.Stale_policy { path = abs "/x" })))

(* Identity — scratch-invariant confinement fingerprint (RFC L8). *)

let identity_domain = "mentat.sandbox.identity.v1"

(* Independently spell the documented wire framing (length-framed parts under
   the versioned domain — [Mentat_digest.derive]'s framing law) to pin the
   domain string and framing format, without reaching into the library's
   internals. *)
let framed_digest parts =
  let buffer = Buffer.create 64 in
  List.iter
    (fun part ->
      Buffer.add_string buffer (string_of_int (String.length part));
      Buffer.add_char buffer ':';
      Buffer.add_string buffer part)
    (identity_domain :: parts);
  Digest.string (Buffer.contents buffer)

let identity_domain_and_framing () =
  is_true ~msg:"not_requested digest uses the documented domain and framing"
    (Digest.equal
       (framed_digest [ "not_requested" ])
       (Identity.digest (Sandbox.identity Sandbox.direct)));
  is_true ~msg:"declared_external digest uses the documented domain and framing"
    (Digest.equal
       (framed_digest [ "declared_external" ])
       (Identity.digest (Sandbox.identity Sandbox.external_)));
  is_true ~msg:"refused digest uses the documented domain and framing"
    (Digest.equal
       (framed_digest [ "refused" ])
       (Identity.digest (Sandbox.identity (refused_sealed workspace_policy))))

(* Byte-stability pins across the [Mentat_digest.derive] refactor. The
   class-only digests below were computed from the pre-derive implementation
   (the hand-rolled length framing) and must never change: a session contract
   persists identities, and a digest shift would refuse every pending
   invocation. The enforced pins were minted when the profile canonicalization
   moved onto [derive ~domain:"mentat.sandbox.profile.v1"] and pin the current
   enforced derivation end-to-end (policy: scratch /tmp, reads Only [/data],
   writable [/work], protected [/work/.git], network restricted). The Seatbelt
   pin was re-minted when the profile gained the Unix-socket [network-bind]/
   [network-outbound] allow scoped to the writable roots (so build tools can
   bind their RPC socket); the Bubblewrap pin is unchanged, its namespace
   network isolation never blocking in-workspace Unix sockets. *)
let identity_digest_pins () =
  equal string ~msg:"not_requested identity digest is byte-stable"
    "5eb60082fd887b2d9988623b099f340426ef07e65f11c7b6ecf81cf3d01dea47"
    (Digest.to_hex (Identity.digest (Sandbox.identity Sandbox.direct)));
  equal string ~msg:"declared_external identity digest is byte-stable"
    "b79f1aebf5801657bd51568ae49cca0188222ca3fc21adaa3274b2f6196593f8"
    (Digest.to_hex (Identity.digest (Sandbox.identity Sandbox.external_)));
  equal string ~msg:"refused identity digest is byte-stable"
    "50ec9e852f77a0604c09ca5459e1cef3d560c5168b89a0459156ca718bbb2364"
    (Digest.to_hex
       (Identity.digest (Sandbox.identity (refused_sealed workspace_policy))));
  let pinned_policy =
    confined ~scratch:(abs "/tmp")
      ~reads:(Policy.Only [ abs "/data" ])
      ~writable_roots:[ abs "/work" ]
      ~protected_paths:[ abs "/work/.git" ]
      ()
  in
  equal string ~msg:"seatbelt enforced identity digest golden"
    "297fd640e39e2af77bcd6d7b5f174a07fb6a0dc71e66a1f421bfdfd928fa7496"
    (Digest.to_hex (Identity.digest (identity_of pinned_policy)));
  equal string ~msg:"bubblewrap enforced identity digest golden"
    "cd4ebac3753b0e1ea3b7906a7e745a5e0cf851db9a088ebf42aef7fa54923d5e"
    (Digest.to_hex
       (Identity.digest (identity_of ~backend:Backend.Bubblewrap pinned_policy)))

let identity_is_scratch_invariant () =
  let policy_with scratch =
    confined ~scratch:(abs scratch)
      ~reads:(Policy.Only [ abs "/data" ])
      ~writable_roots:[ abs "/work" ]
      ()
  in
  let a = policy_with "/scratch/a" in
  let b = policy_with "/scratch/b" in
  is_false ~msg:"the two policies genuinely differ by scratch"
    (Policy.equal a b);
  is_true ~msg:"identity is invariant under scratch regeneration"
    (Identity.equal (identity_of a) (identity_of b));
  (* The audit/identity distinction: Evidence keeps the real profile digest, so
     it *does* differ across scratch, while the identity does not. *)
  is_false ~msg:"the real profile digest (evidence) differs across scratch"
    (Evidence.equal (Sandbox.evidence (sealed a)) (Sandbox.evidence (sealed b)))

(* Roots under [/w]; the scratch paths this property seals with live under
   [/s], so the scratch is always disjoint from every generated root — no
   lexical ancestor/descendant relation. That is the precondition under which
   L8 holds (and the invariant the real resolver upholds: scratch is a fresh
   empty temp_dir, never an ancestor of a workspace root). The aliasing
   violation is pinned separately below. *)
let disjoint_root_gen =
  Gen.map
    (fun components -> abs ("/w/" ^ String.concat "/" components))
    (Gen.list_size (Gen.int_range 1 3) path_component)

let disjoint_root =
  testable ~pp:Abs.pp ~equal:Abs.equal ~gen:disjoint_root_gen ()

let identity_is_scratch_invariant_property () =
  prop'
    "identity is scratch-invariant when the scratch is disjoint from the roots"
    (list disjoint_root) (fun roots ->
      let policy scratch =
        confined ~scratch:(abs scratch) ~reads:(Policy.Only roots)
          ~writable_roots:roots ()
      in
      is_true ~msg:"disjoint scratch regeneration preserves the identity"
        (Identity.equal
           (identity_of (policy "/s/one"))
           (identity_of (policy "/s/two"))))

let identity_scratch_alias_is_a_safe_direction () =
  (* Known limitation, safe direction. When the per-run scratch is a lexical
     ancestor of a confined read root that is not itself a writable root,
     [Policy.make]'s descendant-normalization collapses that read root, so the
     scratch-normalized profile drops it and two seals differing only in
     scratch get *different* identities. This is unreachable via the real
     resolver (scratch is a fresh empty temp_dir, never a root ancestor), and
     it fails safe: resume refuses and re-approves rather than silently
     widening. *)
  let policy scratch =
    confined ~scratch:(abs scratch)
      ~reads:(Policy.Only [ abs "/data/sub" ])
      ~writable_roots:[ abs "/other" ]
      ()
  in
  is_false
    ~msg:"an ancestor-aliasing scratch differs (refuses) rather than widening"
    (Identity.equal
       (identity_of (policy "/data"))
       (identity_of (policy "/elsewhere")))

let identity_changes_on_confinement_change () =
  let base =
    confined
      ~reads:(Policy.Only [ abs "/data" ])
      ~writable_roots:[ abs "/work" ]
      ~protected_paths:[ abs "/work/.git" ]
      ~network:Policy.Network.Restricted ()
  in
  let base_id = identity_of base in
  let differs ~msg policy =
    is_false ~msg (Identity.equal base_id (identity_of policy))
  in
  differs ~msg:"widening a writable root flips the identity"
    (confined
       ~reads:(Policy.Only [ abs "/data" ])
       ~writable_roots:[ abs "/work"; abs "/extra" ]
       ~protected_paths:[ abs "/work/.git" ]
       ());
  differs ~msg:"flipping the network flips the identity"
    (confined
       ~reads:(Policy.Only [ abs "/data" ])
       ~writable_roots:[ abs "/work" ]
       ~protected_paths:[ abs "/work/.git" ]
       ~network:Policy.Network.Enabled ());
  differs ~msg:"adding a read root flips the identity"
    (confined
       ~reads:(Policy.Only [ abs "/data"; abs "/more" ])
       ~writable_roots:[ abs "/work" ]
       ~protected_paths:[ abs "/work/.git" ]
       ());
  differs ~msg:"changing a protected path flips the identity"
    (confined
       ~reads:(Policy.Only [ abs "/data" ])
       ~writable_roots:[ abs "/work" ]
       ~protected_paths:[ abs "/work/.mentat" ]
       ());
  is_false ~msg:"changing the backend flips the identity"
    (Identity.equal base_id (identity_of ~backend:Backend.Bubblewrap base))

let identity_class_separation () =
  (* The child environment is excluded from the identity structurally: this
     library never sees an environment, so there is no env-value case left to
     test — RFC L8's env-invariance clause is now a type-level fact. *)
  let enforced = identity_of workspace_policy in
  let not_requested = Sandbox.identity Sandbox.direct in
  let external_ = Sandbox.identity Sandbox.external_ in
  let refused = Sandbox.identity (refused_sealed workspace_policy) in
  let all = [ enforced; not_requested; external_; refused ] in
  List.iteri
    (fun i a ->
      List.iteri
        (fun j b ->
          if i <> j then
            is_false ~msg:"distinct evidence classes have distinct identities"
              (Identity.equal a b))
        all)
    all

let identity_jsont_round_trips () =
  let round_trip label sandbox =
    let identity = Sandbox.identity sandbox in
    match Json.encode Identity.jsont identity with
    | Error message -> failf "%s: encode failed: %s" label message
    | Ok json -> (
        match Json.decode Identity.jsont json with
        | Error message -> failf "%s: decode failed: %s" label message
        | Ok reloaded ->
            is_true
              ~msg:(label ^ ": round-trip preserves the digest")
              (Digest.equal (Identity.digest identity)
                 (Identity.digest reloaded)))
  in
  round_trip "enforced" (sealed ~backend:Backend.Bubblewrap workspace_policy);
  round_trip "not_requested" Sandbox.direct;
  round_trip "declared_external" Sandbox.external_;
  round_trip "refused" (refused_sealed workspace_policy)

let identity_decode_rejects_non_canonical () =
  let obj fields =
    Json.object'
      (List.map (fun (name, value) -> Json.mem (Json.name name) value) fields)
  in
  let valid_hex = Digest.to_hex (Digest.string "profile") in
  let rejects label json =
    is_true ~msg:label (Result.is_error (Json.decode Identity.jsont json))
  in
  rejects "enforced without a backend or profile"
    (obj [ ("class", json_string "enforced") ]);
  rejects "enforced without a profile"
    (obj
       [
         ("class", json_string "enforced");
         ("backend", json_string "macos-seatbelt");
       ]);
  rejects "a non-enforced class carrying a backend"
    (obj
       [
         ("class", json_string "not_requested");
         ("backend", json_string "macos-seatbelt");
       ]);
  rejects "a non-enforced class carrying a profile"
    (obj
       [ ("class", json_string "refused"); ("profile", json_string valid_hex) ]);
  rejects "an unknown class" (obj [ ("class", json_string "loose") ]);
  rejects "an unknown backend under enforced"
    (obj
       [
         ("class", json_string "enforced");
         ("backend", json_string "windows-appcontainer");
         ("profile", json_string valid_hex);
       ])

(* Suite. *)

let () =
  run "mentat.sandbox"
    [
      (* Error *)
      test "error constructors are structured and distinct"
        error_constructors_are_distinct;
      test "error messages carry the structured fields"
        error_messages_carry_fields;
      test "error json is built from the constructors" error_json_shape;
      (* Policy *)
      test "policy normalizes writable roots" policy_normalizes_writable_roots;
      test "policy collapses descendant roots" policy_collapses_descendant_roots;
      test "policy distinguishes network state" policy_distinguishes_network;
      test "policy scopes concrete protected paths"
        policy_protected_paths_are_scoped;
      test "policy folds writable roots and scratch into reads"
        policy_folds_writable_and_scratch_into_reads;
      test "policy scratch participates in equality"
        policy_scratch_participates_in_equality;
      policy_root_normalization_is_an_antichain ();
      (* Backend / Requirement / Network *)
      test "backend identity and order" backend_identity;
      test "backend of_id round-trips" backend_of_id_round_trips;
      test "enforcing executables are fixed and absolute"
        backend_executables_are_fixed_and_absolute;
      test "requirement spellings round-trip" requirement_round_trips;
      test "network spellings round-trip" network_round_trips;
      (* Seatbelt golden *)
      test "seatbelt read-only golden" seatbelt_read_only_golden;
      test "seatbelt scoped-reads golden" seatbelt_scoped_reads_golden;
      test "seatbelt carveout golden" seatbelt_carveout_golden;
      test "seatbelt nested roots share carveouts"
        seatbelt_nested_roots_share_carveouts;
      test "seatbelt network-enabled golden" seatbelt_network_enabled_golden;
      test "seatbelt puts no path byte in the profile text"
        seatbelt_no_path_enters_the_text;
      test "seatbelt generation is deterministic"
        seatbelt_generation_is_deterministic;
      (* Bubblewrap golden *)
      test "bubblewrap read-only golden" bubblewrap_read_only_golden;
      test "bubblewrap scoped-reads golden" bubblewrap_scoped_reads_golden;
      test "bubblewrap network-enabled golden" bubblewrap_network_enabled_golden;
      test "bubblewrap uses strict --ro-bind" bubblewrap_uses_strict_ro_bind;
      test "bubblewrap seals a scoped root" bubblewrap_seals_a_scoped_root;
      (* Route constructors *)
      test "route evidence table" route_evidence_table;
      test "route policy projection" route_policy_projection;
      test "evidence is fixed across lowerings"
        evidence_is_fixed_across_lowerings;
      test "a refused route refuses every command"
        refused_route_refuses_every_command;
      test "the route couples the profile to the sealed policy"
        route_couples_profile_to_policy;
      test "digest stability across construction"
        digest_stability_across_construction;
      test "sealing is pure and total" sealing_is_pure_and_total;
      (* Escalation *)
      test "escalation stances" escalation_stances;
      test "escalation is shape-based, not availability-based"
        escalation_is_shape_based_not_availability_based;
      (* lower_argv / lower_escalated_argv *)
      test "lower_argv appends the command verbatim"
        lower_argv_appends_command_verbatim;
      test "lower_argv enforces lexical cwd containment"
        lower_argv_enforces_cwd_containment;
      test "lower_argv passes unconfined routes verbatim"
        lower_argv_passes_unconfined_routes_verbatim;
      test "lower_argv validates argv on every route" lower_argv_validates_argv;
      test "lower_escalated_argv drops containment and never prefixes"
        lower_escalated_drops_containment_never_prefixes;
      test "lower_escalated_argv refuses denied and ignored routes"
        lower_escalated_refuses_denied_and_ignored;
      (* obligations *)
      test "obligations enumerate paths and kinds"
        obligations_enumerate_paths_and_kinds;
      test "obligations are empty for unconfined and refused seals"
        obligations_are_empty_for_unconfined_and_refused;
      (* admits *)
      test "admits reproduces the host gate" admits_reproduces_the_host_gate;
      (* Evidence codec *)
      test "evidence json shapes" evidence_json_shapes;
      test "evidence cases are pairwise distinct" evidence_cases_are_distinct;
      (* Identity *)
      test "identity domain and framing" identity_domain_and_framing;
      test "identity digests are byte-stable pins" identity_digest_pins;
      test "identity is scratch-invariant" identity_is_scratch_invariant;
      identity_is_scratch_invariant_property ();
      test "identity scratch aliasing is a safe direction"
        identity_scratch_alias_is_a_safe_direction;
      test "identity changes on a confinement change"
        identity_changes_on_confinement_change;
      test "identity separates evidence classes" identity_class_separation;
      test "identity jsont round-trips preserving the digest"
        identity_jsont_round_trips;
      test "identity decode rejects non-canonical states"
        identity_decode_rejects_non_canonical;
    ]
