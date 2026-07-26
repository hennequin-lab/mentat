(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let path_parts path =
  [
    Mentat_workspace.Root.Key.to_string (Mentat_workspace.Path.root_key path);
    Lpath.Rel.to_string (Mentat_workspace.Path.rel path);
  ]

let derive source path =
  String_id.derive_id ~domain:"mentat.mutation.change.v2"
    (source @ path_parts path)

let for_claim ~claim ~ordinal path =
  derive [ "claim"; claim; string_of_int ordinal ] path

let for_revert ~revert path = derive [ "revert"; revert ] path
