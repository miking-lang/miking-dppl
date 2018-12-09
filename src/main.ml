(** The entrypoint for the pplcore executable *)

open Eval
open Cps
open Ast
open Const
open Printf
open Analysis

(** Mapping between predefined variable names and constants *)
let builtin_const = [
  "not",CNot;
  "and",CAnd(None);
  "or",COr(None);

  "mod",CMod(None);
  "sll",CSll(None);
  "srl",CSrl(None);
  "sra",CSra(None);

  "log",CLog;

  "add",CAdd(None);
  "sub",CSub(None);
  "mul",CMul(None);
  "div",CDiv(None);
  "neg",CNeg;
  "lt",CLt(None);
  "leq",CLeq(None);
  "gt",CGt(None);
  "geq",CGeq(None);

  "eq",CEq(None);
  "neq",CNeq(None);

  "normal",CNormal(None,None);
  "uniform",CUniform(None,None);
  "gamma",CGamma(None,None);
  "exponential",CExp(None);
  "bernoulli",CBern(None);
]

(** Mapping between predefined variable names and terms *)
let builtin_tm = [
  "infer",TmInfer(na);
  "logpdf",TmLogPdf(na,None);

  "sample",TmSample(na,None,None);
  "weight",TmWeight(na,None,None);
  "dweight",TmDWeight(na,None,None);
]

(** Combined mapping of builtin_const and builtin_tm *)
let builtin =
  List.map (fun (x, y) -> x, tm_of_const y) builtin_const
  @ builtin_tm

(** Add a slash at the end "/" if not already available *)
let add_slash s =
  if String.length s = 0 || (String.sub s (String.length s - 1) 1) <> "/"
  then s ^ "/" else s

(** Expand a list of files and folders into a list of file names *)
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

(** Function for lexing and parsing a file. *)
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

(** Function for executing a file. *)
let exec filename =
  if !utest then printf "%s: " filename;
  utest_fail_local := 0;
  let tm = match Filename.extension filename with
    | ".tppl" -> parse (Tpplparser.main Tppllexer.main) filename
    | s -> failwith ("Unsupported file type: " ^ s) in

  if debug_cps || debug_lift_apps then
    (print_endline "-- input term --";
     print_endline (string_of_tm tm);
     print_newline ());

  (* If chosen inference is aligned SMC, perform static analysis *)
  let tm = if !Analysis.align
    then begin
      (* Label program and builtins in preparation for static analysis *)
      let tm,bmap,nl = Analysis.label (builtin |> List.split |> fst) tm in

      if debug_sanalysis then
        (print_endline "-- after labeling --";
         print_endline (lstring_of_tm tm);
         print_newline ());

      (* Perform static analysis, returning all dynamic labels *)
      let dyn = Analysis.analyze bmap tm nl in

      (* By using the above analysis results, transform all dynamic
         checkpoints. This information will be handled by the inference
         algorithm. *)
      Analysis.align_weight bmap dyn tm
    end else tm in

  if debug_sanalysis then
    (print_endline "-- after SMC alignment --";
     print_endline (lstring_of_tm tm);
     print_newline ());

  let builtin = builtin
                (* Transform builtins to CPS. Required since we need to
                       wrap constant functions in CPS forms *)
                |> List.map (fun (x, y) -> (x, (cps_value y)))

                (* Debruijn transform builtins (since they have now been
                   CPS transformed) *)
                |> List.map (fun (x, y) -> (x, debruijn [] y)) in

  if debug_cps_builtin then
    (print_endline "-- cps builtin --";
     (List.iter print_endline
        (List.map
           (fun (x, y) -> x ^ " = " ^ string_of_tm y) builtin));
     print_newline ());

  (* Perform CPS transformation of main program *)
  let cps = cps tm in

  if debug_cps then
    (print_endline "-- post cps --";
     print_endline (string_of_tm cps);
     print_newline ());

  (* Evaluate CPS form of main program *)
  let res =
    cps |> debruijn (builtin |> List.split |> fst)
    |> eval (builtin |> List.split |> snd) in

  if debug_cps then
    (print_endline "-- post cps eval --";
     print_endline (string_of_tm res));

  if !utest && !utest_fail_local = 0 then printf " OK\n" else printf "\n"

(** Main function. Checks arguments and reads file names *)
let main =
  let speclist = [

    "--test",
    Arg.Unit(fun _ -> utest := true),
    " Enable unit tests.";

    "--align-weight",
    Arg.Unit(fun _ -> Analysis.align := true),
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
