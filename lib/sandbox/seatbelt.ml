(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* Ported from the Codex reference agent's seatbelt_base_policy.sbpl, itself
   derived from Chrome's macOS sandbox policy. Production-proven base
   allowances: process exec/fork, /dev/null, sysctls, PTYs, and read-only
   user preferences. *)
let base_policy =
  {|(version 1)

; start with closed-by-default
(deny default)

; child processes inherit the policy of their parent
(allow process-exec)
(allow process-fork)
(allow signal (target same-sandbox))

; process-info
(allow process-info* (target same-sandbox))

(allow file-write-data
  (require-all
    (path "/dev/null")
    (vnode-type CHARACTER-DEVICE)))

; inherited stdio is the host-owned process channel, not host-file authority
(allow file-read-data file-test-existence file-write-data
  (subpath "/dev/fd"))
(allow file-read* (regex #"^/dev/fd/(0|1|2)$"))
(allow file-write* (regex #"^/dev/fd/(1|2)$"))
(allow file-read-metadata
  (literal "/dev")
  (literal "/dev/stdin")
  (literal "/dev/stdout")
  (literal "/dev/stderr"))

; guarded vnodes and the sandbox-container query used by macOS runtimes
(allow system-mac-syscall (mac-policy-name "vnguard"))
(allow system-mac-syscall
  (require-all
    (mac-policy-name "Sandbox")
    (mac-syscall-number 67)))

; resolve standard symlinks and firmlink ancestors without granting their data
(allow file-read-metadata file-test-existence
  (literal "/etc")
  (literal "/tmp")
  (literal "/var")
  (literal "/private/etc/localtime"))
(allow file-read-metadata file-test-existence
  (path-ancestors "/System/Volumes/Data/private"))
(allow file-read* file-test-existence (literal "/"))
(allow system-fsctl (fsctl-command FSIOC_CAS_BSDFLAGS))

; standard non-secret device handles required by libc and language runtimes
(allow file-read* file-test-existence
  (literal "/dev/autofs_nowait")
  (literal "/dev/random")
  (literal "/dev/urandom"))
(allow file-read* file-test-existence file-write-data
  (literal "/dev/null")
  (literal "/dev/zero"))

; sysctls permitted.
(allow sysctl-read
  (sysctl-name "hw.activecpu")
  (sysctl-name "hw.busfrequency_compat")
  (sysctl-name "hw.byteorder")
  (sysctl-name "hw.cacheconfig")
  (sysctl-name "hw.cachelinesize_compat")
  (sysctl-name "hw.cpufamily")
  (sysctl-name "hw.cpufrequency_compat")
  (sysctl-name "hw.cputype")
  (sysctl-name "hw.l1dcachesize_compat")
  (sysctl-name "hw.l1icachesize_compat")
  (sysctl-name "hw.l2cachesize_compat")
  (sysctl-name "hw.l3cachesize_compat")
  (sysctl-name "hw.logicalcpu_max")
  (sysctl-name "hw.machine")
  (sysctl-name "hw.model")
  (sysctl-name "hw.memsize")
  (sysctl-name "hw.ncpu")
  (sysctl-name "hw.nperflevels")
  (sysctl-name-prefix "hw.optional.arm.")
  (sysctl-name-prefix "hw.optional.armv8_")
  (sysctl-name "hw.packages")
  (sysctl-name "hw.pagesize_compat")
  (sysctl-name "hw.pagesize")
  (sysctl-name "hw.physicalcpu")
  (sysctl-name "hw.physicalcpu_max")
  (sysctl-name "hw.logicalcpu")
  (sysctl-name "hw.cpufrequency")
  (sysctl-name "hw.tbfrequency_compat")
  (sysctl-name "hw.vectorunit")
  (sysctl-name "machdep.cpu.brand_string")
  (sysctl-name "kern.argmax")
  (sysctl-name "kern.hostname")
  (sysctl-name "kern.maxfilesperproc")
  (sysctl-name "kern.maxproc")
  (sysctl-name "kern.osproductversion")
  (sysctl-name "kern.osrelease")
  (sysctl-name "kern.ostype")
  (sysctl-name "kern.osvariant_status")
  (sysctl-name "kern.osversion")
  (sysctl-name "kern.secure_kernel")
  (sysctl-name "kern.usrstack64")
  (sysctl-name "kern.version")
  (sysctl-name "sysctl.proc_cputype")
  (sysctl-name "vm.loadavg")
  (sysctl-name-prefix "hw.perflevel")
  (sysctl-name-prefix "kern.proc.pgrp.")
  (sysctl-name-prefix "kern.proc.pid.")
  (sysctl-name-prefix "net.routetable.")
)

; Allow Java to read some CPU info. This is misclassified as a "write" because
; userspace passes a memory buffer to the sysctl, but conceptually it is a read.
(allow sysctl-write
  (sysctl-name "kern.grade_cputype"))

; IOKit
(allow iokit-open
  (iokit-registry-entry-class "RootDomainUserClient")
)

; needed to look up user info
(allow mach-lookup
  (global-name "com.apple.system.opendirectoryd.libinfo")
)

; Needed for python multiprocessing on MacOS for the SemLock
(allow ipc-posix-sem)

; Needed for PyTorch/libomp on macOS to register OpenMP runtimes.
(allow ipc-posix-shm-read-data
  ipc-posix-shm-write-create
  ipc-posix-shm-write-unlink
  (ipc-posix-name-regex #"^/__KMP_REGISTERED_LIB_[0-9]+$"))

(allow mach-lookup
  (global-name "com.apple.analyticsd")
  (global-name "com.apple.analyticsd.messagetracer")
  (global-name "com.apple.appsleep")
  (global-name "com.apple.bsd.dirhelper")
  (global-name "com.apple.diagnosticd")
  (global-name "com.apple.logd")
  (global-name "com.apple.logd.events")
  (global-name "com.apple.runningboard")
  (global-name "com.apple.secinitd")
  (global-name "com.apple.system.DirectoryService.libinfo_v1")
  (global-name "com.apple.system.logger")
  (global-name "com.apple.system.notification_center")
  (global-name "com.apple.system.opendirectoryd.membership")
  (global-name "com.apple.trustd")
  (global-name "com.apple.trustd.agent")
  (global-name "com.apple.xpc.activity.unmanaged")
  (global-name "com.apple.PowerManagement.control")
)

(allow network-outbound (literal "/private/var/run/syslog"))
(allow ipc-posix-shm-read*
  (ipc-posix-name "apple.shm.notification_center"))

; allow openpty()
(allow pseudo-tty)
(allow file-read* file-write* file-ioctl (literal "/dev/ptmx"))
(allow file-read* file-write*
  (require-all
    (regex #"^/dev/ttys[0-9]+")
    (extension "com.apple.sandbox.pty")))
; PTYs created before entering seatbelt may lack the extension; allow ioctl
; on those slave ttys so interactive shells detect a TTY and remain functional.
(allow file-ioctl (regex #"^/dev/ttys[0-9]+"))

; allow readonly user preferences
(allow ipc-posix-shm-read* (ipc-posix-name-prefix "apple.cfprefs."))
(allow mach-lookup
  (global-name "com.apple.cfprefsd.daemon")
  (global-name "com.apple.cfprefsd.agent")
  (local-name "com.apple.cfprefsd.agent"))
(allow user-preference-read)|}

(* Ported from the Codex reference agent's seatbelt_network_policy.sbpl:
   platform services TLS, DNS, and network configuration need beyond raw
   socket access when the policy enables network. *)
let network_policy =
  {|; allow only safe AF_SYSTEM sockets used for local platform services.
(allow system-socket
  (require-all
    (socket-domain AF_SYSTEM)
    (socket-protocol 2)
  )
)

(allow mach-lookup
    ; Used by platform helpers that resolve user directory locations.
    (global-name "com.apple.bsd.dirhelper")
    (global-name "com.apple.system.opendirectoryd.membership")

    ; Communicate with the security server for TLS certificate information.
    (global-name "com.apple.SecurityServer")
    (global-name "com.apple.networkd")
    (global-name "com.apple.ocspd")
    (global-name "com.apple.trustd.agent")

    ; Read network configuration.
    (global-name "com.apple.SystemConfiguration.DNSConfiguration")
    (global-name "com.apple.SystemConfiguration.configd")
)

(allow sysctl-read
  (sysctl-name-regex #"^net.routetable")
)|}

let writable_param index = Printf.sprintf "WRITABLE_ROOT_%d" index
let readable_param index = Printf.sprintf "READABLE_ROOT_%d" index

let file_read_policy policy =
  match Policy.reads policy with
  | Policy.All -> ("(allow file-read*)\n(allow file-map-executable)", [])
  | Policy.Only roots ->
      let params =
        List.mapi
          (fun index root -> (readable_param index, Lpath.Abs.to_string root))
          roots
      in
      let predicates =
        List.concat_map
          (fun (param, _) ->
            [
              Printf.sprintf "(literal (param \"%s\"))" param;
              Printf.sprintf "(subpath (param \"%s\"))" param;
            ])
          params
      in
      (* [realpath] and path resolution walk every ancestor of a root
         (e.g. /Applications above an admitted /Applications/Xcode.app), so
         each root also grants metadata — never data — on its ancestor
         chain. *)
      let ancestors =
        List.map
          (fun (param, _) ->
            Printf.sprintf "(path-ancestors (param \"%s\"))" param)
          params
      in
      ( Printf.sprintf
          "(allow file-read* file-test-existence file-map-executable\n\
           %s\n\
           )\n\
           (allow file-read-metadata file-test-existence\n\
           %s\n\
           )"
          (String.concat " " predicates)
          (String.concat " " ancestors),
        params )

let excluded_param index excluded_index =
  Printf.sprintf "WRITABLE_ROOT_%d_EXCLUDED_%d" index excluded_index

let file_write_policy policy =
  let roots = Policy.scratch policy :: Policy.writable_roots policy in
  match roots with
  | [] -> ("", [])
  | roots ->
      let carveouts = Policy.protected_paths policy in
      let components, params =
        List.fold_left
          (fun (components, params) root ->
            let index = List.length components in
            let root_param = writable_param index in
            let params = (root_param, Lpath.Abs.to_string root) :: params in
            let excluded =
              List.filter (fun path -> Lpath.Abs.is_within ~root path) carveouts
            in
            let excluded_parts, params =
              List.fold_left
                (fun (parts, params) path ->
                  let param = excluded_param index (List.length parts / 2) in
                  let params = (param, Lpath.Abs.to_string path) :: params in
                  ( parts
                    @ [
                        Printf.sprintf "(require-not (literal (param \"%s\")))"
                          param;
                        Printf.sprintf "(require-not (subpath (param \"%s\")))"
                          param;
                      ],
                    params ))
                ([], params) excluded
            in
            let parts =
              Printf.sprintf "(subpath (param \"%s\"))" root_param
              :: excluded_parts
            in
            let component =
              match parts with
              | [ only ] -> only
              | parts ->
                  Printf.sprintf "(require-all %s )" (String.concat " " parts)
            in
            (components @ [ component ], params))
          ([], []) roots
      in
      ( Printf.sprintf "(allow file-write*\n%s\n)" (String.concat " " components),
        List.rev params )

(* dune (and other build tools) serve their RPC over a Unix-domain socket
   created under the build directory ([_build/.rpc/dune]). Seatbelt governs the
   [bind()] and [connect()] on such a socket with [network-bind] and
   [network-outbound] filtered by the socket's pathname; an INET bind or connect
   carries no pathname and never matches a path filter, so it stays subject to
   {!network_section}. Admitting these two operations under the writable roots —
   the same roots [file-write*] already grants, by the same [WRITABLE_ROOT_%d]
   parameter names and order, so no new [-D] parameter is introduced — lets
   local build IPC work under a restricted network without opening any INET
   boundary. *)
let unix_socket_policy policy =
  let roots = Policy.scratch policy :: Policy.writable_roots policy in
  let predicates =
    List.mapi
      (fun index _root ->
        Printf.sprintf "(subpath (param \"%s\"))" (writable_param index))
      roots
  in
  Printf.sprintf "(allow network-bind network-outbound\n%s\n)"
    (String.concat " " predicates)

let denied_param index = Printf.sprintf "DENIED_PATH_%d" index

(* SBPL is last-match-wins, so a denial emitted after every allow overrides all
   of them — including the base policy's root-literal read and the blanket
   read an [All] scope emits. Parameterized like every other root so no path
   text enters the profile body, and omitted entirely when there is nothing to
   deny, which keeps a deny-free profile byte-identical to the one this release
   generated. *)
let deny_policy policy =
  match Policy.denied_paths policy with
  | [] -> ("", [])
  | roots ->
      let params =
        List.mapi
          (fun index root -> (denied_param index, Lpath.Abs.to_string root))
          roots
      in
      let predicates =
        List.concat_map
          (fun (param, _) ->
            [
              Printf.sprintf "(literal (param \"%s\"))" param;
              Printf.sprintf "(subpath (param \"%s\"))" param;
            ])
          params
      in
      ( Printf.sprintf "(deny file-read* file-write* file-test-existence\n%s\n)"
          (String.concat " " predicates),
        params )

let network_section policy =
  match Policy.network policy with
  | Policy.Network.Restricted -> ""
  | Policy.Network.Enabled ->
      Printf.sprintf "(allow network-outbound)\n(allow network-inbound)\n%s"
        network_policy

let sbpl policy =
  let read_policy, read_params = file_read_policy policy in
  let write_policy, write_params = file_write_policy policy in
  let deny, deny_params = deny_policy policy in
  let sections =
    List.filter
      (fun section -> not (String.equal section ""))
      [
        base_policy;
        read_policy;
        write_policy;
        unix_socket_policy policy;
        network_section policy;
        deny;
      ]
  in
  (String.concat "\n" sections, read_params @ write_params @ deny_params)
