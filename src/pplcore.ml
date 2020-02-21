(** The entrypoint for the pplcore executable. Handles command line arguments,
    lexing/parsing, and unit testing *)

open Ast
open Printf
open Sprint
open Utils
open Debug
open Infer
open Utest
open Lexing
open Rand

(** Output formats *)
type output =
  | None
  | Debug
  | Samples
  | Norm
  | Mean

(** Default is debug output format *)
let output = ref Debug

(** Add a slash at the end of a path if not already available *)
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

(** Function for lexing and parsing a file, parameterized by a parser as
    generated by ocamlyacc and ocamllex *)
let parse par filename =

  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  begin try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let tm = par lexbuf in
      close_in file; tm
    with | Parsing.Parse_error ->
      if !utest then (
        printf "\n** Parse error at %s **"
          (string_of_position (lexbuf.lex_curr_p));
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1;
        nop)
      else
        failwith (sprintf "Parse error at %s"
                    (string_of_position (lexbuf.lex_curr_p)))
  end

(** Function for running inference on a program. *)
let exec filename =

  (* If running unit testing, enforce usage of the Eval inference method *)
  if !utest then begin
    printf "%s: " filename;
    inference := Eval;
    output    := None;
  end;
  utest_fail_local := 0;

  (* Parse the program *)
  let tm = match Filename.extension filename with
    | ".ppl"  -> parse (Parser.main Lexer.main) filename
    | ".tppl" -> parse (Tparser.main Tlexer.main) filename
    | s       -> failwith ("Unsupported file type: " ^ s) in

  debug !debug_input "Input term" (fun () -> string_of_tm tm);

  (* Run inference *)
  begin match infer tm with

    | Empirical({ norm_const; samples; _} as emp) ->

      (* Configure string conversion function for output
         (one line, limited printing depth) *)
      let string_of_val' = string_of_val ~max_boxes:5 ~margin:max_int in

      (* Print output of inference *)
      begin match !output with

        | None -> ()

        | Debug ->
          debug true "Inference result"
            (fun () -> string_of_empirical
                ~normalize:true ~compare:Compare.compare
                string_of_val' emp);

        | Samples -> print_endline (samples_of_empirical string_of_val' emp)

        | Norm -> printf "%f\n" norm_const

        | Mean ->
          (* TODO Refactor *)
          (* Normalize (TODO should use library function) *)
          let logsum = logsumexp (Utils.map fst samples) in
          let samples = Utils.map (fun (w,t) -> w -. logsum,t) samples in

          (* Compute mean, crash if not all floats *)
          let mean = List.fold_left (fun acc (w, V{v;_}) ->
              match v with
              | VUnit when w = neg_infinity -> acc
              | VFloat{f} -> acc +. exp w *. f
              | _ -> failwith "Can't take mean of non-float"
            ) 0.0 samples in
          printf "Mean = %f\n" mean
      end

    | Variance var -> debug true "Inference result"
                        (fun () -> sprintf "Variance: %f" var)

  end;

  (* Print unit testing information for this file *)
  utest_local_print()

(** Main function. Parses command line arguments *)
let main =
  let speclist = [

    "--inference",
    Arg.String(fun s -> match s with
        | "is"          -> inference := IS
        | "smc-direct"  -> inference := SMCDirect
        | "smc-manual"  -> inference := SMCManual
        | "smc-dynamic" -> inference := SMCDynamic
        | "smc-static"  -> inference := SMCStatic
        | "eval"        -> inference := Eval
        | "var-smc"     -> inference := VarSMC
        | "var-is"      -> inference := VarIS
        | _             -> failwith "Incorrect inference algorithm"
      ),
    " Specifies inference method. Options are: eval, is, \
     smc-direct, smc-manual, smc-dynamic, and smc-static.";

    "--output",
    Arg.String(fun s -> match s with
        | "none"     -> output := None
        | "debug"    -> output := Debug
        | "samples"  -> output := Samples
        | "norm"     -> output := Norm
        | "mean"     -> output := Mean
        | _          -> failwith "Incorrect output format"
      ),
    " Specifies output format. Options are: none, debug, samples, norm, mean.";

    "--samples",
    Arg.Int(fun i -> match i with
        | i when i < 1 -> failwith "Number of samples must be positive"
        | i            -> samples := i),
    " Specifies the number of samples in affected inference algorithms.";

    "--test", Arg.Set(utest), "";

    "--debug-input",              Arg.Set(debug_input), " ";
    "--debug-label",              Arg.Set(debug_label), " ";
    "--debug-sanalysis",          Arg.Set(debug_sanalysis), " ";
    "--debug-resample-transform", Arg.Set(debug_resample_transform), " ";
    "--debug-cps",                Arg.Set(debug_cps), " ";
    "--debug-cps-builtin",        Arg.Set(debug_cps_builtin), " ";
    "--debug-smc",                Arg.Set(debug_smc), " ";
    "--debug-smc-dyn",            Arg.Set(debug_smc_dyn), " ";
    "--debug-eval",               Arg.Set(debug_eval), " ";
    "--debug-eval-app",           Arg.Set(debug_eval_app), " ";
    "--debug-eval-env",           Arg.Set(debug_eval_env), " ";
    "--debug-var",                Arg.Set(debug_var), " ";
    "--debug-var-queue",          Arg.Set(debug_var_queue), " ";

    "--pretty",                   Arg.Set(pretty),
    " Enables pretty printing of terms";

  ] in

  let speclist = Arg.align speclist in
  let lst = ref [] in
  let anon_fun arg = lst := arg :: !lst in
  let usage_msg =
    "Usage: pplcore [<options>] <files|folders> \n\
    Options:" in

  Arg.parse speclist anon_fun usage_msg;
  if List.length !lst = 0 then
    Arg.usage speclist usage_msg
  else begin
    List.iter exec (files_of_folders !lst);
    utest_print ();
  end

