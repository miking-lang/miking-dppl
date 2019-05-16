(** Functions related to probability distributions *)

open Ast
open Attribute

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
  | VFloat{f;_},
    VNormal{mu=Some mu;sigma=Some sigma;_} ->
    VFloat{at=va;f=log (Gsl.Randist.gaussian_pdf (f -. mu) ~sigma:sigma)}

  (* Exponential distribution *)
  | VFloat{f;_},
    VExp{lam=Some lam;_} ->
    let mu = 1.0 /. lam in
    VFloat{at=va;f=log (Gsl.Randist.exponential_pdf f ~mu:mu)}

  (* Bernoulli distribution *)
  | VBool{b;_},
    VBern{p=Some p;_} ->
    let i = if b then 1 else 0 in
    VFloat{at=va;f=log (Gsl.Randist.bernoulli_pdf i ~p:p)}

  (* Gamma distribution *)
  | VFloat{f;_},
    VGamma{a=Some a;b=Some b;_} ->
    VFloat{at=va;f=log (Gsl.Randist.gamma_pdf f ~a:a ~b:b)}

  | _ -> failwith "Incorrect distribution\
                   or value applied as argument to logpdf"

(** Sample functions for built in distributions. **)
let sample dist = match dist with

  (* Normal distribution *)
  | VNormal{mu=Some mu;sigma=Some sigma;_} ->
    VFloat{at=va; f=mu +. Gsl.Randist.gaussian seed ~sigma:sigma}

  (* Exponential distribution *)
  | VExp{lam=Some lam;_} ->
    let mu = 1.0 /. lam in
    VFloat{at=va;f=Gsl.Randist.exponential seed ~mu:mu}

  (* Bernoulli distribution *)
  | VBern{p=Some p;_} ->
    let b = Gsl.Randist.bernoulli seed ~p:p == 1 in
    VBool{at=va;b=b}

  (* Gamma distribution *)
  | VGamma{a=Some a;b=Some b;_} ->
    VFloat{at=va;f=Gsl.Randist.gamma seed ~a:a ~b:b}

  | _ -> failwith "Incorrect distribution applied as argument to sample."

