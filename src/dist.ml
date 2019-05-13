(** Functions related to probability distributions *)

open Ast

(** Whether to use a random seed or not for probability distributions *)
let random_seed = false

(** Gsl default seed **)
let seed =
  let rng = Gsl.Rng.make (Gsl.Rng.default ()) in
  if random_seed then
    (Random.self_init();
     Gsl.Rng.set rng (Random.nativeint Nativeint.max_int));
  rng

(** Probability density/mass functions for built in distributions. *)
let logpdf value dist = match value,dist with

  (* Normal distribution *)
  | VFloat(_,v),
    VNormal(_,Some mu,Some sigma) ->
    VFloat(na,log (Gsl.Randist.gaussian_pdf (v -. mu) ~sigma:sigma))

  (* Exponential distribution *)
  | VFloat(_,v),
    VExp(_,Some lambda) ->
    let mu = 1.0 /. lambda in
    VFloat(na,log (Gsl.Randist.exponential_pdf v ~mu:mu))

  (* Bernoulli distribution *)
  | VBool(_,v),
    VBern(_,Some p) ->
    let i = if v then 1 else 0 in
    VFloat(na,log (Gsl.Randist.bernoulli_pdf i ~p:p))

  (* Gamma distribution *)
  | VFloat(_,v),
    VGamma(_,Some b, Some a) ->
    VFloat(na,log (Gsl.Randist.gamma_pdf v ~a:a ~b:b))

  | _ -> failwith "Incorrect distribution\
                   or value applied as argument to logpdf"

(** Sample functions for built in distributions. **)
let sample dist = match dist with

  (* Normal distribution *)
  | VNormal(_,Some mu,Some sigma) ->
    let sample = VFloat(na,mu +. Gsl.Randist.gaussian seed ~sigma:sigma) in
    sample

  (* Exponential distribution *)
  | VExp(_,Some lambda) ->
    let mu = 1.0 /. lambda in
    VFloat(na,Gsl.Randist.exponential seed ~mu:mu)

  (* Bernoulli distribution *)
  | VBern(_,Some p) ->
    let b = Gsl.Randist.bernoulli seed ~p:p == 1 in
    VBool(na,b)

  (* Gamma distribution *)
  | VGamma(_,Some a,Some b) ->
    VFloat(na,Gsl.Randist.gamma seed ~a:a ~b:b)

  | _ -> failwith "Incorrect distribution applied as argument to sample."

