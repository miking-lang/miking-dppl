(** Inference algorithms *)

open Ast
open Sprint
open Debug
open Utils
open Eval
open Cps
open Printf
open Label

(** Mapping between predefined variable names and builtin constants *)
let builtin = [

  "not",          VNot(na);
  "and",          VAnd(na,None);
  "or",           VOr(na,None);

  "mod",          VMod(na,None);
  "sll",          VSll(na,None);
  "srl",          VSrl(na,None);
  "sra",          VSra(na,None);

  "inf",          VFloat(na,infinity);
  "log",          VLog na;

  "add",          VAdd(na,None);
  "sub",          VSub(na,None);
  "mul",          VMul(na,None);
  "div",          VDiv(na,None);
  "neg",          VNeg na;
  "lt",           VLt(na,None);
  "leq",          VLeq(na,None);
  "gt",           VGt(na,None);
  "geq",          VGeq(na,None);

  "eq",           VEq(na,None);
  "neq",          VNeq(na,None);

  "normal",       VNormal(na,None, None);
  "uniform",      VUniform(na,None, None);
  "gamma",        VGamma(na,None, None);
  "exponential",  VExp(na,None);
  "bernoulli",    VBern(na,None);

  "logpdf",       VLogPdf(na,None);
  "sample",       VSample na;

  "weight",       VWeight na;
  "fix",          VFix na;

] |> List.map (fun (x, y) -> x, tm_of_val y)

(** Create a string representation of builtins *)
let string_of_builtin ?(labels = false) builtin =
  String.concat "\n"
    (List.map (fun (x, y) -> sprintf "%s = %s" x
                  (string_of_tm ~labels:labels ~pretty:false y)) builtin)

(** Inference types *)
type inference =
  | Eval
  | Importance
  | SMC

(** Default inference is simply weighted evaluation *)
let inference = ref Eval

(** Number of samples for inference algorithms *)
let samples = ref 10

(** SMC Alignment types *)
type align =
  | Static
  | Dynamic
  | Disable

(** Whether or not to align SMC. Disabled by default. *)
let align = ref Disable

(** Compute the logarithm of the average of a list of n weights using
    logsumexp-trick *)
let logavg n weights =
  let max = List.fold_left max (-. infinity) weights in
  log (List.fold_left (fun s w -> s +. exp (w -. max)) 0.0 weights)
  +. max -. log (float n)


(** Importance sampling
    (or more specifically, likelihood weighting) inference *)
let infer_is env n program =

  (* Replicate program for #samples times *)
  let s = replicate n program in

  (* Evaluate everything to the end, producing a set of weighted samples *)
  let res = List.map (eval false env 0.0) s in

  (* Calculate normalizing constant and return *)
  logavg n (List.map fst res),res


(* Systematic resampling of n samples *)
let resample n s =

  (*printf "Resampling!\n%!";*)
  (*List.iter (fun (w,_) -> printf "%f " w) s;*)
  (*print_newline();*)
  (* Compute part of the normalizing constant *)
  let logavg = logavg n (List.map fst s) in

  (* Compute normalized weights from log-weights *)
  let snorm = List.map (fun (w,t) -> exp (w -. logavg),t) s in

  (* Draw offset for resampling *)
  let offset = Random.float 1.0 in

  (* Perform resampling *)
  let rec rec1 curr next snorm acc = match snorm with
    | (w,_)::_ -> let curr = curr +. w in rec2 curr next snorm acc
    | [] -> acc
  and rec2 curr next snorm acc = match snorm with
    | (_,t)::tail ->
      if curr > next then rec2 curr (next +. 1.0) snorm ((0.0,t)::acc)
      else rec1 curr next tail acc
    | [] -> failwith "Error in resampling" in

  (* Also return the log average for computing the normalization constant *)
  logavg, rec1 0.0 offset snorm []


(** SMC inference TODO Cleanup *)
let infer_smc env n program =

  (* Replicate program for #samples times with an initial log weight of 0.0 *)
  let s = replicate n program in

  (* Run until first resample, attaching the initial environment *)
  let s = List.map (eval false env 0.0) s in

  (* Convert values to terms *)
  let s = List.map (fun (w,v) -> w,tm_of_val v) s in

  (* Run SMC *)
  let rec recurse s normconst =

    (* Evaluate a program until encountering a resample *)
    let rec sim (weight,tm) =
      let weight,v = eval false [] weight tm in
      match v with

      (* Resample *)
      | VResamp(_,Some(cont),Some b) ->

        (* If dynamic alignment is enabled and we are in stochastic control,
           skip this resampling point. Otherwise, resample here. *)
        if b && !align = Dynamic then
          sim (weight,TApp(na,tm_of_val cont,nop))
        else
          false,weight,cont

      (* Final result *)
      | _ -> true,weight,v in

    let res = List.map sim s in
    let b = List.for_all (fun (b,_,_) -> b) res in
    let logavg, res = res |> List.map (fun (_,w,t) -> (w,t)) |> resample n in
    let normconst = normconst +. logavg in
    if b then begin
      normconst,res
    end else
      let s = List.map (fun (w,v) -> w,TApp(na,tm_of_val v,nop)) res in
      recurse s normconst
  in recurse s 0.0


(** Convert all weighting to
    weighting followed by a call to resample *)
let add_resample builtin t =

  let rec recurse t = match t with
    | TVar _ -> t

    | TApp(a,t1,t2) -> TApp(a,recurse t1,recurse t2)

    | TLam(a,x,t1) -> TLam(a,x,recurse t1)

    | TIf(a,t1,t2) -> TIf(a,recurse t1,recurse t2)

    | TMatch(a,cls) -> TMatch(a,List.map (fun (p,t) -> p,recurse t) cls)

    | TVal(VWeight _) ->
      let var, var'  = makevar "w" noidx in
      let weight_app = TApp(na,t,var') in
      let resamp     = TApp(na,TVal(VResamp(na,None,None)),nop) in
      TLam(na,var,seq weight_app resamp)

    | TVal _ -> t

  in
  recurse t, List.map (fun (x,t) -> x,recurse t) builtin

(* Function for producing a nicely formatted string representation of the
   empirical distributions returned by infer below. Aggregates samples with the
   same value to produce a more compact output.
   TODO Cleanup
   TODO Comparison of vals should ignore attributes *)
let string_of_empirical ls =

  (* Start by sorting the list *)
  let sorted = List.sort (fun (_,v1) (_,v2) -> compare v1 v2) ls in

  (* Compute the logarithm of the average of the weights *)
  let logavg = logavg !samples (List.map fst sorted) in

  (* Compute normalized samples from log-weights *)
  let normalized = List.map
      (fun (w,t) -> exp (w -. logavg -. (log (float_of_int !samples))),t)
      sorted in

  let rec aggregate acc ls = match acc,ls with
    | (w1,v1)::acc,(w2,v2)::ls when v1 = v2 -> aggregate ((w1+.w2,v1)::acc) ls
    | acc, (w,v)::ls                        -> aggregate ((w,v)::acc) ls
    | acc, [] -> acc in

  let aggr = aggregate [] normalized in

  (* Sort based on weight to show the most important sample at the top *)
  let last = List.sort
      (fun (w1,_) (w2,_) -> - Pervasives.compare w1 w2) aggr in

  let line (w,v) = sprintf "  %-15s%-15f" (string_of_val v) w in

  sprintf "%-15s%-15s\n" "SAMPLE" "WEIGHT"
  ^ (String.concat "\n" (List.map line last))

(** Preprocess term and redirect to inference algorithm provided by user.
    TODO Cleanup *)
let infer tm =

  let tm,builtin = match !inference with

    (* Nothing more required if not running SMC *)
    | Eval | Importance -> tm,builtin

    (* Perform SMC specific transformations *)
    | SMC ->

      (* Label program and builtins *)
      let tm,builtin,_nl = label builtin tm in

      debug debug_label "After labeling builtins"
        (fun () -> string_of_builtin ~labels:true builtin);

      debug debug_label "After labeling program"
        (fun () -> string_of_tm ~labels:true tm);

      (* If alignment is turned on, perform the corresponding static analysis.
         Otherwise, simply add resamples after each call to weight.
         TODO Note that dynamic is enabled by default if not static. *)
      let tm,builtin = if !align = Static
        then tm,builtin (*TODO Analysis.align_weight builtin_map tm nl*)
        else add_resample builtin tm
      in

      debug debug_resample_transform "After attaching resamples to builtins"
        (fun () -> string_of_builtin builtin);

      debug debug_resample_transform "After attaching resamples"
        (fun () -> string_of_tm tm);

      let builtin =
        builtin

        (* Transform builtins to CPS. Required since we need to wrap constant
           functions in CPS forms *)
        |> List.map (fun (x, y) -> (x, (cps_atomic y)))

        (* Debruijn transform builtins (since they have now been
           CPS transformed) *)
        |> List.map (fun (x, y) -> (x, debruijn [] y)) in

      debug debug_cps "Post CPS builtin"
        (fun () -> string_of_builtin builtin);

      (* Perform CPS transformation of main program *)
      let tm = cps tm in

      debug debug_cps "Post CPS"
        (fun () -> string_of_tm ~pretty:false tm);

      tm,builtin

  in

  (* Calculate debruijn indices *)
  let tm = debruijn (builtin |> List.split |> fst) tm in

  (* Variable names no longer required due to debruijn indices *)
  let env = (builtin |> List.split |> snd) in

  match !inference,!samples with
  | Eval,_ -> infer_is env 1 tm
  | Importance,n -> infer_is env n tm
  | SMC,n -> infer_smc env n tm

