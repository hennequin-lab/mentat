From Corelib Require Import Init.Prelude.
From Corelib Require Extraction.
From Corelib Require Import ExtrOcamlBasic.
From MentatSession Require Import Replay.

Extraction Language OCaml.

(* The verified fold speaks the host's [('s, 'e) result]. *)
Extract Inductive result => "result" [ "Ok" "Error" ].

Extraction "replay.ml" full_fold.
