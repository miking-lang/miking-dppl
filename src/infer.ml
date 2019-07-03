(** Handles inference *)

open Ast
open Sprint
open Debug
open Builtin
open Debruijn
open Rand

(** Inference types *)
type inference =
  | Eval
  | IS
  | SMCDirect
  | SMCManual
  | SMCDynamic
  | SMCStatic
  | VarSMC
  | VarIS

(** Inference result types *)
type result =
  | Empirical of value empirical
  | Variance  of float

(** Default inference is simply weighted evaluation *)
let inference = ref Eval

(** Number of samples for inference algorithms *)
let samples = ref 10

(** Transform term and builtins to CPS *)
let cps tm builtin =

  (* Transform builtins to CPS. *)
  let builtin = List.map (fun (x, y) -> (x, (Cps.cps y))) builtin in

  debug debug_cps "Post CPS builtin"
    (fun () -> string_of_builtin builtin);

  (* Perform CPS transformation of main program *)
  let tm = Cps.cps tm in

  debug debug_cps "Post CPS"
    (fun () -> string_of_tm ~pretty:false tm);

  tm,builtin

(** Preprocess a program in preparation of running inference. *)
let preprocess tm builtin = match !inference with

    (* No preprocessing required if running evaluation or IS inference *)
    | Eval | IS -> tm,builtin

    (* For direct SMC, add resamples after each weight *)
    | SMCDirect -> Smc.preprocess ~dyn:false builtin tm

    (* For dynamic SMC, add dynamic resamples after each weight *)
    | SMCDynamic -> Smc.preprocess ~dyn:true builtin tm

    (* For manual SMC, do nothing. It is the users
       responsibility to add resample points in the program *)
    | SMCManual -> tm,builtin

    (* TODO *)
    | SMCStatic -> failwith "Statically aligned SMC not yet implemented"

    (* For variance computations, we don't do any preprocessing *)
    | VarSMC
    | VarIS -> tm,builtin

(** Preprocess term and redirect to inference algorithm provided on command
    line. *)
let infer tm =

  (* Perform inference-specific preprocessing *)
  let tm,builtin = preprocess tm builtin in

  (* CPS transform the program *)
  let tm,builtin = cps tm builtin in

  (* Calculate debruijn indices *)
  let tm = debruijn (builtin |> List.split |> fst) tm in
  let builtin = List.map (fun (x, y) -> (x, debruijn [] y)) builtin in

  (* Variable names no longer required due to debruijn indices *)
  let env = (builtin |> List.split |> snd) in

  match !inference with
  | Eval                   -> Empirical(Is.infer 1 env tm)
  | IS                     -> Empirical(Is.infer !samples env tm)
  | SMCDirect  | SMCManual
  | SMCDynamic | SMCStatic -> Empirical(Smc.infer !samples env tm)
  | VarSMC                 -> Variance(VarZSmc.var !samples env tm)
  | VarIS                  -> Variance(VarZIs.var !samples env tm)

