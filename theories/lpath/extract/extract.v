From Corelib Require Import Init.Prelude.
From Corelib Require Extraction.
From Corelib Require Import ExtrOcamlBasic.
From MentatLpath Require Import Lpath.

Extraction Language OCaml.
Extraction "lpath_model.ml"
  abs_of_string abs_components abs_within abs_strictly_within.
