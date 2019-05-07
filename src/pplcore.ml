(** The entrypoint for the pplcore executable. Handles command line argument
    parsing, unit testing, and lexing/parsing. *)

open Ast
open Print
open Printf
open Utils
open Debug
open Infer

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

(** Function for lexing and parsing a file. *)
let parse par filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  begin try
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      let tm = par lexbuf in
      close_in file; tm
    with | Parsing.Parse_error ->
      if !utest then (
        printf "\n ** Parse error at %s"
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
  if !utest then printf "%s: " filename;
  utest_fail_local := 0;

  let tm = match Filename.extension filename with
    | ".ppl"  -> parse (Parser.main Lexer.main) filename
    | ".tppl" -> parse (Tpplparser.main Tppllexer.main) filename
    | s       -> failwith ("Unsupported file type: " ^ s) in

  debug debug_input "Input term"
    (fun () -> string_of_tm tm);

  let res = infer tm in

  (* TODO Cleanup, this should not be a debug printout *)
  debug debug_infer "Infer result"
    (fun () -> String.concat "\n"
        (List.map
           (fun (w, t) ->
              sprintf "Sample: %s, Log weight: %f" (string_of_tm t) w)
           res));

  (*debug debug_norm "Normalizing constant"*)
  (*(fun () -> sprintf "%f" normconst);*)

  if !utest && !utest_fail_local = 0 then printf " OK\n"

(** Main function. Parses command line arguments *)
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
        | "is"   -> inference := Importance
        | "smc"  -> inference := SMC
        | "eval" -> inference := Eval
        | _      -> failwith "Incorrect inference algorithm"
      ),
    " Specifies inference method. Options are: is, smc.";

    "--samples",
    Arg.Int(fun i -> match i with
        | i when i < 1 -> failwith "Number of samples must be positive"
        | i            -> particles := i),
    " Specifies the number of samples in affected inference algorithms.";

  ] in

  let speclist = Arg.align speclist in
  let lst = ref [] in
  let anon_fun arg = lst := arg :: !lst in
  let usage_msg = "" in

  Arg.parse speclist anon_fun usage_msg;

  List.iter exec (files_of_folders !lst);

  Debug.utest_print ();

