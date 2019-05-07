(** Debugging utilities *)

open Printf

(** Debug the input term *)
let debug_input       = true

(** Debug the static analysis *)
let debug_sanalysis   = false

(** Debug the labelling *)
let debug_labeling    = true

(** Debug the resample transformation *)
let debug_resample_transform = true

(** Debug the lifting transformation *)
let debug_lift_apps   = true

(** Debug the CPS transformation of the initial environment (builtin) *)
let debug_cps_builtin = false

(** Debug the CPS transformation *)
let debug_cps         = false

(** Debug the evaluation *)
let debug_eval        = false

(** Debug the evaluation environment *)
let debug_eval_env    = false

(** Debug the inference procedure *)
let debug_infer       = true

(** Printout the normalization constant *)
let debug_norm        = false

(** Set to true if unit testing is enabled *)
let utest             = ref false

(** Counts the number of successful unit tests *)
let utest_ok          = ref 0

(** Counts the number of failed unit tests *)
let utest_fail        = ref 0

(** Counts local failed tests for one file *)
let utest_fail_local  = ref 0

(** Debugging printout *)
let debug cond heading info =
  if cond && not !utest then begin
    printf "--- %s ---\n" (String.uppercase_ascii heading);
    printf "%s\n\n" (info ());
  end

(* Print out unit test results, if applicable *)
let utest_print () =
  if !utest then
    if !utest_fail = 0 then
      printf "\nUnit testing SUCCESSFUL after executing %d tests.\n"
        (!utest_ok)
    else
      printf "\nERROR! %d successful tests and %d failed tests.\n"
        (!utest_ok) (!utest_fail)
