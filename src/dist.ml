(** Functions related to probability distributions *)

open Ast
open Rand

(** Compute log-pdfs of probability distributions *)
let logpdf x dist = match x,dist with

  (* Logpdf of normal distribution *)
  | VFloat{f;_},DNormal{mu=Some mu;sigma=Some sigma;_} ->
    VFloat{f=normal_logpdf f mu sigma}
  | VInt{i;_},DNormal{mu=Some mu;sigma=Some sigma;_} ->
    VFloat{f=normal_logpdf (float_of_int i) mu sigma}
  | _,DNormal _ -> failwith "TODO"

  (* Logpdf of uniform distribution *)
  | VFloat{f;_},DUniform{a=Some a;b=Some b;_} ->
    VFloat{f=uniform_logpdf f a b}
  | VInt{i;_},DUniform{a=Some a;b=Some b;_} ->
    VFloat{f=uniform_logpdf (float_of_int i) a b}
  | _,DUniform _ -> failwith "TODO"

  (* Logpdf of exponential distribution *)
  | VFloat{f;_},DExp{lam=Some lam;_} ->
    VFloat{f=exp_logpdf f lam}
  | VInt{i;_},DExp{lam=Some lam;_} ->
    VFloat{f=exp_logpdf (float_of_int i) lam}
  | _,DExp _ -> failwith "TODO"

  (* Logpdf of bernoulli distribution *)
  | VBool{b;_},DBern{p=Some p;_} ->
    VFloat{f=bern_logpdf b p}
  | _,DBern _ -> failwith "TODO"

  (* Logpdf of beta distribution *)
  | VFloat{f;_},DBeta{a=Some a;b=Some b;_} ->
    VFloat{f=beta_logpdf f a b}
  | VInt{i;_},DBeta{a=Some a;b=Some b;_} ->
    VFloat{f=beta_logpdf (float_of_int i) a b}
  | _,DBeta _ -> failwith "TODO"

  (* Logpdf of gamma distribution *)
  | VFloat{f;_},DGamma{a=Some a;b=Some b;_} ->
    VFloat{f=gamma_logpdf f a b}
  | VInt{i;_},DGamma{a=Some a;b=Some b;_} ->
    VFloat{f=gamma_logpdf (float_of_int i) a b}
  | _,DGamma _ -> failwith "TODO"

(** Sample from probability distribution *)
let sample dist = match dist with

  (* Sample normal distribution *)
  | DNormal{mu=Some mu;sigma=Some sigma;_} ->
    VFloat{f=normal_sample mu sigma}
  | DNormal _ -> failwith "TODO"

  (* Sample uniform distribution *)
  | DUniform{a=Some a;b=Some b;_} ->
    VFloat{f=uniform_sample a b}
  | DUniform _ -> failwith "TODO"

  (* Sample exponential distribution *)
  | DExp{lam=Some lam;_} ->
    VFloat{f=exp_sample lam}
  | DExp _ -> failwith "TODO"

  (* Sample bernoulli distribution *)
  | DBern{p=Some p;_} ->
    VBool{b=bern_sample p}
  | DBern _ -> failwith "TODO"

  (* Sample beta distribution *)
  | DBeta{a=Some a;b=Some b;_} ->
    VFloat{f=beta_sample a b}
  | DBeta _ -> failwith "TODO"

  (* Sample gamma distribution *)
  | DGamma{a=Some a;b=Some b;_} ->
    VFloat{f=gamma_sample a b}
  | DGamma _ -> failwith "TODO"

(** Type alias for the support of a distribution *)
type support = (float * value') list

(** Returns the support of a given distribution *)
let support dist = match dist with

  | DNormal _ | DUniform _
  | DExp _    | DBeta _    | DGamma _ ->
    failwith "Support for continuous distribution requested"

  (* Support for Bernoulli distribution *)
  | DBern{p=Some p;_} ->
    [ log p,VBool{b=true};
      log (1.0 -. p),VBool{b=false} ]
  | DBern _ -> failwith "TODO"

