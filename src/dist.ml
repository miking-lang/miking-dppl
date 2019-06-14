(** Functions related to probability distributions *)

(** Whether to use a random seed or not for probability distributions *)
let random_seed = true

(** Gsl default seed **)
let seed =
  let rng = Gsl.Rng.make (Gsl.Rng.default ()) in
  if random_seed then
    (Random.self_init();
     Gsl.Rng.set rng (Random.nativeint Nativeint.max_int));
  rng

(** Normal distribution log pdf *)
let normal_logpdf f mu sigma =
  log (Gsl.Randist.gaussian_pdf (f -. mu) ~sigma:sigma)

(* Uniform distribution log pdf *)
let uniform_logpdf f a b =
  if f >= a && f <= b then -. (log (b -. a)) else neg_infinity

(* Exponential distribution log pdf *)
let exp_logpdf f lam =
  let mu = 1.0 /. lam in
  log (Gsl.Randist.exponential_pdf f ~mu:mu)

(* Bernoulli distribution log pdf *)
let bern_logpdf b p =
  let i = if b then 1 else 0 in
  log (Gsl.Randist.bernoulli_pdf i ~p:p)

(* Beta distribution log pdf *)
let beta_logpdf f a b =
  log (Gsl.Randist.beta_pdf f ~a:a ~b:b)

(* Gamma distribution log pdf *)
let gamma_logpdf f a b =
  log (Gsl.Randist.gamma_pdf f ~a:a ~b:b)

(* Normal distribution sample *)
let normal_sample mu sigma =
  mu +. Gsl.Randist.gaussian seed ~sigma:sigma

(* Uniform distribution sample *)
let uniform_sample a b =
  Gsl.Randist.flat seed ~a:a ~b:b

(* Exponential distribution sample *)
let exp_sample lam =
  let mu = 1.0 /. lam in
  Gsl.Randist.exponential seed ~mu:mu

(* Bernoulli distribution sample *)
let bern_sample p =
  Gsl.Randist.bernoulli seed ~p:p == 1

(* Beta distribution sample *)
let beta_sample a b =
  Gsl.Randist.beta seed ~a:a ~b:b

(* Gamma distribution sample *)
let gamma_sample a b =
  Gsl.Randist.gamma seed ~a:a ~b:b

