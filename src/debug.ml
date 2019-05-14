(** Debugging utilities *)

open Printf
open Utest

(** Debug the input term *)
let debug_input = true

(** Debug the labelling *)
let debug_label = false

(** Debug the static analysis *)
let debug_sanalysis = false

(** Debug the resample transformation *)
let debug_resample_transform = false

(** Debug the CPS transformation *)
let debug_cps = false

(** Debug the resample transformation *)
let debug_smc = true

(** Debug the evaluation *)
let debug_eval = false

(** Debug applications when evaluating *)
let debug_eval_app = false

(** Debug the evaluation environment *)
let debug_eval_env = false

(** Debug the inference procedure
    TODO This is a result, not a debug printout *)
let debug_infer = true

(** Printout the normalization constant
    TODO This is a result, not a debug printout *)
let debug_norm = true

(** Debug printout *)
let debug cond heading info =
  if cond && not !utest then begin
    printf "--- %s ---\n" (String.uppercase_ascii heading);
    printf "%s\n\n" (info ());
  end

