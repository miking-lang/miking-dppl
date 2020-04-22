(** Debugging utilities. *)

open Printf
open Utest

(** Debug the input term *)
let debug_input = ref false

(** Debug the labelling *)
let debug_label = ref false

(** Debug the static analysis *)
let debug_sanalysis = ref false

(** Debug the resample transformation *)
let debug_resample_transform = ref false

(** Debug the CPS transformation *)
let debug_cps = ref false

(** Debug the CPS builtin transformation *)
let debug_cps_builtin = ref false

(** Debug SMC inference *)
let debug_smc = ref false

(** Debug dynamic SMC inference *)
let debug_smc_dyn = ref false

(** Debug the evaluation *)
let debug_eval = ref false

(** Debug applications when evaluating *)
let debug_eval_app = ref false

(** Debug the evaluation environment *)
let debug_eval_env = ref false

(** Debug the variance computations *)
let debug_var = ref false
let debug_var_queue = ref false

(** Debug printout *)
let debug cond heading info =
  if cond && not !utest then begin
    printf "--- %s ---\n" (String.uppercase_ascii heading);
    printf "%s\n\n%!" (info ());
  end
