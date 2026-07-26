(*---------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type t =
  | Tool : {
      declaration : Mentat_llm.Tool.t;
      input : 'input Input.t;
      output : 'output Output.encoder;
      permissions : 'input -> Mentat_permission.Request.t list;
      run : cancelled:(unit -> bool) -> 'input -> 'output Result.t;
    }
      -> t
  | Staged : {
      declaration : Mentat_llm.Tool.t;
      input : 'input Input.t;
      prepared : 'prepared Jsont.t;
      describe : 'prepared -> string;
      output : 'output Output.encoder;
      prepare_permissions : 'input -> Mentat_permission.Request.t list;
      prepare :
        cancelled:(unit -> bool) ->
        'input ->
        [ `Finished of 'output Result.t | `Prepared of 'prepared ];
      permissions : 'prepared -> Mentat_permission.Request.t list;
      run : cancelled:(unit -> bool) -> 'prepared -> 'output Result.t;
    }
      -> t

let no_permissions _ = []

(* [Mentat_llm.Tool.make] owns the model tool-name grammar, the non-empty
   description rule, and the object-schema-root check, and raises
   [Invalid_argument] on violation. Building the declaration here delegates every
   identity invariant to that owner. *)
let make ~name ~description ~input ~output ?(permissions = no_permissions) ~run
    () =
  let declaration =
    Mentat_llm.Tool.make ~name ~description ~input_schema:(Input.schema input)
      ()
  in
  Tool { declaration; input; output; permissions; run }

let make_staged ~name ~description ~input ~prepared ~describe ~output
    ?(prepare_permissions = no_permissions) ~prepare
    ?(permissions = no_permissions) ~run () =
  let declaration =
    Mentat_llm.Tool.make ~name ~description ~input_schema:(Input.schema input)
      ()
  in
  Staged
    {
      declaration;
      input;
      prepared;
      describe;
      output;
      prepare_permissions;
      prepare;
      permissions;
      run;
    }

let declaration = function Tool t -> t.declaration | Staged t -> t.declaration
