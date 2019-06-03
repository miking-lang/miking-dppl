(** Functions related to probability distributions *)

open Ast
open Sprint

(** Error message for incorrect distributions *)
let fail_dist dist =
  (Printf.printf "\n**  Incorrect distribution: %s\n" (string_of_val dist));
  failwith "fail_dist"

(** Whether to use a random seed or not for probability distributions *)
let random_seed = true

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
  | value, VNormal{mu=Some mu;sigma=Some sigma;_} ->
    let f = match value with
      | VFloat{f;_} -> f
      | VInt{i;_} -> float_of_int i
      | _ -> failwith "TODO" in
    VFloat{at=va;f=log (Gsl.Randist.gaussian_pdf (f -. mu) ~sigma:sigma)}

  (* Exponential distribution *)
  | value, VExp{lam=Some lam;_} ->
    let f = match value with
      | VFloat{f;_} -> f
      | VInt{i;_} -> float_of_int i
      | _ -> failwith "TODO" in
    let mu = 1.0 /. lam in
    VFloat{at=va;f=log (Gsl.Randist.exponential_pdf f ~mu:mu)}

  (* Bernoulli distribution *)
  | VBool{b;_},
    VBern{p=Some p;_} ->
    let i = if b then 1 else 0 in
    VFloat{at=va;f=log (Gsl.Randist.bernoulli_pdf i ~p:p)}

  (* Beta distribution *)
  | value,VBeta{a=Some a;b=Some b;_} ->
    let f = match value with
      | VFloat{f;_} -> f
      | VInt{i;_} -> float_of_int i
      | _ -> failwith "TODO" in
    VFloat{at=va;f=log (Gsl.Randist.beta_pdf f ~a:a ~b:b)}

  (* Gamma distribution *)
  | value,VGamma{a=Some a;b=Some b;_} ->
    let f = match value with
      | VFloat{f;_} -> f
      | VInt{i;_} -> float_of_int i
      | _ -> failwith "TODO" in
    VFloat{at=va;f=log (Gsl.Randist.gamma_pdf f ~a:a ~b:b)}

  | _ -> fail_dist dist (* TODO Make this static instead *)

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

  (* Beta distribution *)
  | VBeta{a=Some a;b=Some b;_} ->
    VFloat{at=va; f=Gsl.Randist.beta seed ~a:a ~b:b}

  (* Gamma distribution *)
  | VGamma{a=Some a;b=Some b;_} ->
    VFloat{at=va;f=Gsl.Randist.gamma seed ~a:a ~b:b}

  | _ -> fail_dist dist (* TODO Make this static instead *)

