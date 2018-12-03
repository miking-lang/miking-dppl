(*
   Miking is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE.txt

   boot.ml is the main entry point for first stage of the
   bootstrapped Miking compiler. The bootstapper is interpreted and
   implemented in OCaml. Note that the Miking bootstrapper
   only implements a subset of the Ragnar language.
*)

open Pprint
open Eval
open Ast
open Printf

(* TODO Move? *)
let align = ref false

(* Mapping between named builtin functions (intrinsics) and the
   correspond constants *)
let builtin =
  [("not",CNot);
   ("and",CAnd(None));
   ("or",COr(None));

   ("modi",CModi(None));
   ("slli",CSlli(None));
   ("srli",CSrli(None));
   ("srai",CSrai(None));

   ("log",CLog);

   ("add",CAdd(None));
   ("sub",CSub(None));
   ("mul",CMul(None));
   ("div",CDiv(None));
   ("neg",CNeg);
   ("lt",CLt(None));
   ("leq",CLeq(None));
   ("gt",CGt(None));
   ("geq",CGeq(None));

   ("eq",CEq(None));
   ("neq",CNeq(None));

   ("infer",CInfer);
   ("logpdf",CLogPdf(None));

   ("sample",CSample([]));
   ("weight",CWeight([]));
   ("dweight",CDWeight([]));

   ("normal",CNormal([]));
   ("uniform",CUniform([]));
   ("gamma",CGamma([]));
   ("exponential",CExp(None));
   ("bernoulli",CBern(None));
  ]


(* Add a slash at the end "/" if not already available *)
let add_slash s =
  if String.length s = 0 || (String.sub s (String.length s - 1) 1) <> "/"
  then s ^ "/" else s

(* Expand a list of files and folders into a list of file names *)
let files_of_folders lst = List.fold_left (fun a v ->
  if Sys.is_directory v then
    (Sys.readdir v
        |> Array.to_list
        |> List.filter (fun x ->
            not (String.length x >= 1 && String.get x 0 = '.'))
        |> List.map (fun x -> (add_slash v) ^ x)
        |> List.filter (fun x -> not (Sys.is_directory x))
    ) @ a
  else v::a
) [] lst

(* Main function for evaluation a function. Performs lexing and parsing. Does
   not perform any type checking *)
let parse par filename =
  begin try
      let file = open_in filename in
      let lexbuf = Lexing.from_channel file in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let tm = par lexbuf in
      close_in file; tm
    with
    | e ->
      (* TODO Nice error messages using the lexbuf position *)
      if !utest then (
        printf "\n ** %s" "TODO";
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1);
      raise e (* TODO Handle error instead of crashing *)
  end

let exec filename =
  if !utest then printf "%s: " filename;
  utest_fail_local := 0;
  let tm = match Filename.extension filename with
    | ".tppl" -> parse (Tpplparser.main Tppllexer.main) filename
    | s -> failwith ("Unsupported file type: " ^ s) in

  if Debug.cps then
    (print_endline "-- pre cps --";
     print_endline (pprint tm);
     print_newline ());

  (* Function for converting consts in builtin to tms *)
  let tm_of_builtin b = List.map (fun (x, y) -> x, tm_of_const y) b in

  (* If chosen inference is aligned SMC, perform static analysis *)
  let tm = if !align
    then begin
      (* Label program and builtins in preparation for static analysis *)
      let tm,bmap,nl = Analysis.label (builtin |> List.split |> fst) tm in

      if Debug.sanalysis then
        (print_endline "-- after labeling --";
         print_endline (pprintl tm);
         print_newline ());

      (* Perform static analysis, returning all dynamic labels *)
      let dyn = Analysis.analyze bmap tm nl in

      (* By using the above analysis results, transform all dynamic
         checkpoints. This information will be handled by the inference
         algorithm. *)
      Analysis.align bmap dyn tm
    end else tm in

  if Debug.sanalysis then
    (print_endline "-- after SMC alignment --";
     print_endline (pprintl tm);
     print_newline ());

  let builtin = builtin
                (* Convert builtins from consts to tms *)
                |> tm_of_builtin

                (* Transform builtins to CPS. Required since we need to
                       wrap constant functions in CPS forms *)
                |> List.map (fun (x, y) -> (x, (cps_atomic y)))

                (* Debruijn transform builtins (since they have now been
                   CPS transformed) *)
                |> List.map (fun (x, y) -> (x, debruijn [] y)) in

  if Debug.cps_builtin then
    (print_endline "-- cps builtin --";
     (List.iter print_endline
        (List.map
           (fun (x, y) -> x ^ " = " ^ pprint y) builtin));
     print_newline ());

  (* Perform CPS transformation of main program *)
  let cps = cps idfun tm in

  if Debug.cps then
    (print_endline "-- post cps --";
     print_endline (pprint cps);
     print_newline ());

  (* Evaluate CPS form of main program *)
  let res =
    cps |> debruijn (builtin |> List.split |> fst)
    |> eval (builtin |> List.split |> snd) in

  if Debug.cps then
    (print_endline "-- post cps eval --";
     print_endline (pprint res));

  if !utest && !utest_fail_local = 0 then printf " OK\n" else printf "\n"

(* Main function. Checks arguments and reads file names *)
let main =
  let speclist = [

    "--test",
    Arg.Unit(fun _ -> utest := true),
    " Enable unit tests.";

    "--align",
    Arg.Unit(fun _ -> align := true),
    " Enable program alignment using static analysis.";

    "--inference",
    Arg.String(fun s -> match s with
        | "is" -> inference := Importance(10)
        | "smc" -> inference := SMC(10)
        | _ -> failwith "Incorrect inference algorithm"
      ),
    " Specifies inference method. Options are: is, smcu, smca.";

    "--samples",
    Arg.Int(fun i -> match !inference, i with
        | _,i when i < 1 -> failwith "Number of samples must be positive"
        | Importance _,i -> inference := Importance(i)
        | SMC _,i -> inference := SMC(i)),
    " Determines the number of samples (positive).";

  ] in
  let speclist = Arg.align speclist in
  let lst = ref [] in
  let anon_fun arg = lst := arg :: !lst in
  let usage_msg = "" in

  Arg.parse speclist anon_fun usage_msg;

  List.iter exec (files_of_folders !lst);

  (* Print out unit test results, if applicable *)
  if !utest then
    if !utest_fail = 0 then
      printf "\nUnit testing SUCCESSFUL after executing %d tests.\n"
        (!utest_ok)
    else
      printf "\nERROR! %d successful tests and %d failed tests.\n"
        (!utest_ok) (!utest_fail)
