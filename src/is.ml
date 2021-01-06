(** Importance sampling
    (or more specifically, likelihood weighting) inference *)

open Ast
open Rand

(** Inference hook *)
let infer n env program =

  (* Create an initial set of weighted samples for this inference run *)
  let samples = Utils.replicate n (0.0, program) in

  (* Wrap evaluation function, handling calls to probabilistic constructs *)
  let rec eval env (w,t) =
    match Eval.eval false false env t with

    (* Ignore resampling, since this is specific for SMC *)
    | V{v=VResamp{cont=Some cont;_};_} ->
      eval [] (w, mkapp ~t1:(tm_of_val cont) ~t2:nop)

    (* Update weights *)
    | V{v=VWeight{cont=Some cont;w=Some w'};_} ->
      let w = w +. w' in

      (* If weight is degenerate, don't bother to continue with execution *)
      if w = neg_infinity && !Options.opt0 then
        w,V{at=va;v=VUnit}
      else
        eval [] (w, mkapp ~t1:(tm_of_val cont) ~t2:nop)

    (* Sample probability distributions. *)
    | V{v=VSample{cont=Some cont; d=Some d};_} ->
      let sample = Dist.sample d in
      eval [] (w, mkapp ~t1:(tm_of_val cont) ~t2:(tm_of_val' sample))

    (* For everything else, simply return *)
    | v -> w,v
  in

  (* Evaluate everything, producing a set of weighted samples *)
  let samples = Utils.map (eval env) samples in

  (* Calculate normalizing constant and return *)
  normalize { samples; n; norm_const=0.0 }
