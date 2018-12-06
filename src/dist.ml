(** Operations on probability distributions *)

open Ast
open Const

(** Whether to use a random seed or not *)
let random_seed = true

(** Gsl default seed **)
let seed =
  let rng = Gsl.Rng.make (Gsl.Rng.default ()) in
  if random_seed then
    (Random.self_init();
     Gsl.Rng.set rng (Random.nativeint Nativeint.max_int));
  rng

(** Probability functions for built in distributions.
    TODO Split between const distributions and tm distributions **)
let logpdf value dist = match value,dist with
  (* Normal distribution *)
  | TmConst(_,CFloat(v)),
    TmConst(_,CNormal(Some sigma, Some mu)) ->
    TmConst(na,CFloat(log (Gsl.Randist.gaussian_pdf (v -. mu) ~sigma:sigma)))

  (* Exponential distribution *)
  | TmConst(_,CFloat(v)),
    TmConst(_,CExp Some lambda) ->
    let mu = 1.0 /. lambda in
    TmConst(na,CFloat(log (Gsl.Randist.exponential_pdf v ~mu:mu)))

  (* Bernoulli distribution *)
  | TmConst(_,CBool(v)),
    TmConst(_,CBern Some p) ->
    let i = if v then 1 else 0 in
    TmConst(na,CFloat(log (Gsl.Randist.bernoulli_pdf i ~p:p)))

  (* Gamma distribution *)
  | TmConst(_,CFloat(v)),
    TmConst(_,CGamma(Some b, Some a)) ->
    TmConst(na,CFloat(log (Gsl.Randist.gamma_pdf v ~a:a ~b:b)))

  | _ ->
    failwith "Incorrect distribution or value applied as argument to logpdf"

(** Sample functions for built in distributions.
    TODO Split between const distributions and tm distributions **)
let sample dist = match dist with

  (* Normal distribution *)
  | TmConst(_,CNormal(Some sigma,Some mu)) ->
    let sample = CFloat(mu +. Gsl.Randist.gaussian seed ~sigma:sigma) in
    TmConst(na,sample)

  (* Exponential distribution *)
  | TmConst(_,CExp Some lambda) ->
    let mu = 1.0 /. lambda in
    TmConst(na,CFloat(Gsl.Randist.exponential seed ~mu:mu))

  (* Bernoulli distribution *)
  | TmConst(_,CBern Some p) ->
    let b = Gsl.Randist.bernoulli seed ~p:p == 1 in
    TmConst(na,CBool(b))

  (* Gamma distribution *)
  | TmConst(_,CGamma(Some a,Some b)) ->
    TmConst(na,CFloat(Gsl.Randist.gamma seed ~a:a ~b:b))

  | _ ->
    failwith "Incorrect distribution applied as argument to sample."
