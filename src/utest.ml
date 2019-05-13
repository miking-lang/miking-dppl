(** Functions and refs related to unit testing *)

open Utils
open Sprint
open Printf

(** Enable unit testing *)
let utest = ref false

(** Counts the number of successful unit tests *)
let utest_ok = ref 0

(** Counts the number of failed unit tests *)
let utest_fail = ref 0

(** Counts local failed tests for one file *)
let utest_fail_local = ref 0

(* Print out unit test results, if applicable *)
let utest_print () =
  if !utest then
    if !utest_fail = 0 then
      printf "Unit testing SUCCESSFUL after executing %d tests.\n"
        (!utest_ok)
    else
      printf "ERROR! %d successful tests and %d failed tests.\n"
        (!utest_ok) (!utest_fail)

(* Ending unit test printout for a single file *)
let utest_local_print () =
  if !utest then
    if !utest_fail_local = 0 then printf " OK\n" else printf "\n"

(** Print out error message when a unit test fails *)
let unittest_failed pos v1 v2 =
  Printf.printf "\n** Unit test FAILED at %s **\n\
                \    LHS: %s\n\
                \    RHS: %s\n"
                (string_of_position pos)
                (string_of_val v1)
                (string_of_val v2)

