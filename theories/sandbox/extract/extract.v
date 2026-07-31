From Corelib Require Import Init.Prelude.
From Corelib Require Extraction.
From Corelib Require Import ExtrOcamlBasic.
From MentatSandbox Require Import Confinement.

Extraction Language OCaml.
Extraction "confinement.ml"
  normalize resolve emitted grant floor withinb denied_roots.
