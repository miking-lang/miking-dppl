(** SMC inference TODO Cleanup *)

open Ast
open Sprint
open Rand
open Debug
open Printf
open Utils

(** Inference hook *)
let infer n env program =

  (* Create an initial set of weighted samples for this inference run *)
  let samples = replicate n (0.0,program) in

  (* Run a particle until the next resampling point *)
  let rec eval env (w,t) =
    match Eval.eval false false env t with

    (* If the resample is dynamic and control is stochastic, just continue
       evaluation. *)
    | V{at;v=VResamp{dyn=true;
                      cont=Some cont;
                      stoch_ctrl=Some stoch_ctrl;_}} as v ->

      if stoch_ctrl then begin

        debug debug_smc_dyn "Skipping resampling"
          (fun () -> sprintf "Weight %f" w);

        eval [] (w, mkapp ~t1:(tm_of_val cont)
                   ~t2:(tm_of_val (V{at;v=VUnit})))

      end else w,v

    (* Static resample. Always perform resampling *)
    | V{v=VResamp{dyn=false; cont=Some(V{v=VCont _;_});_};_} as v -> w,v

    (* Incorrect resample *)
    | V{v=VResamp _;_} -> failwith "Erroneous VResamp in infer"

    (* Update weights *)
    | V{at;v=VWeight{cont=Some cont;w=Some w'};_} ->

      let w = w +. w' in

      (* If weight is degenerate, don't bother to continue with execution *)
      if w = neg_infinity then
        w,V{at=va;v=VUnit}
      else
        eval [] (w, mkapp ~t1:(tm_of_val cont) ~t2:(tm_of_val (V{at;v=VUnit})))

    (* Sample probability distributions. *)
    | V{at;v=VSample{cont=Some cont; d=Some d};_} ->
      let sample = Dist.sample d in
      eval [] (w, mkapp ~t1:(tm_of_val cont) ~t2:(tm_of_val (V{at;v=sample})))

    (* For everything else, simply return *)
    | v -> w,v

  in

  (* Run until first resample, attaching the initial environment *)
  let samples = Utils.map (eval env) samples in

  (* Keep track of number of resamples for debugging purposes *)
  let num_resample = ref 0 in

  (* Initialize empirical distribution *)
  let emp = { samples; n; norm_const = 0.0 } in

  (* Function for running SMC *)
  let rec recurse ({samples;_} as emp) =

    (* Check if we need to resample *)
    if List.exists
        (fun (_,v) -> match v with V{v=VResamp _;_} -> true | _ -> false)
        samples
    then begin

      (* Do the resampling *)
      num_resample := !num_resample + 1;

      debug debug_smc "Before resampling"
        (fun () -> string_of_empirical
            ~aggregate:false ~normalize:false
            (string_of_val ~max_boxes:5 ~margin:max_int) emp);

      let {samples;_} as emp = resample emp in

      debug debug_smc "After resampling"
        (fun () -> string_of_empirical
            ~aggregate:false ~normalize:false
            (string_of_val ~max_boxes:5 ~margin:max_int) emp);

      (* Run particles until next resampling point *)
      let continue (w, v) = match v with
        | V{at;v=VResamp{cont=Some cont;_}} ->
          eval [] (w, mkapp ~t1:(tm_of_val cont)
                     ~t2:(tm_of_val (V{at;v=VUnit})))
        | _ -> w,v in

      let emp =
        { emp with
          samples = Utils.map (fun (w,v) -> continue (w, v)) samples } in

      recurse emp

    end else

      normalize emp in

  let res = recurse emp in

  debug debug_smc "Total number of resamples"
    (fun () -> string_of_int !num_resample);

  res

(** Convert all weightings in the program to
    weightings followed by calls to resample *)
let add_resamples ~dyn builtin t =

  (* Convenience function for creating a sequence of two tms *)
  let seq t1 t2 = mkapp ~t1:(mklam ~x:"_" ~t1:t2) ~t2:t1 in

  let rec recurse (T{t=t';at} as t) = match t' with
    | TVar _ -> t
    | TApp{t1;t2} -> T{at;t=TApp{t1=recurse t1;t2=recurse t2}}
    | TLam({t1;_} as t) -> T{at;t=TLam{t with t1=recurse t1}}
    | TCont _ -> failwith "Continuation in add_resamples"
    | TIf{t1;t2} -> T{at;t=TIf{t1=recurse t1;t2=recurse t2}}
    | TMatch{cls} -> T{at;t=TMatch{cls=List.map (fun (p,t) -> p,recurse t) cls}}
    | TVal{v=V{v=VWeight _;_}} ->
      let makevar str i = (str, mkvar ~x:str ~i) in
      let var, var' = makevar "w" noidx in
      let weight_app = mkapp ~t1:t ~t2:var' in
      let resamp =
        mkapp ~t1:(tm_of_val' (VResamp{dyn=dyn; cont=None;stoch_ctrl=None}))
          ~t2:nop in
      mklam ~x:var ~t1:(seq weight_app resamp)
    | TVal _ -> t
in recurse t, List.map (fun (x,t) -> x,recurse t) builtin

(** Preprocessing function calling add_resamples *)
let preprocess ~dyn builtin tm =
  let tm,builtin = add_resamples ~dyn builtin tm in

  debug debug_resample_transform "After attaching resamples"
    (fun () -> string_of_tm tm);

  tm,builtin



