(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let kind_name = function
  | `Regular_file -> "regular file"
  | `Directory -> "directory"
  | `Symbolic_link -> "symbolic link"
  | `Fifo -> "FIFO"
  | `Character_special -> "character device"
  | `Block_device -> "block device"
  | `Socket -> "socket"
  | `Unknown -> "special file"
