(** Utilities for random numbers and statistics *)

open Printf

(** Whether to use a random seed or not when sampling *)
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

(** Uniform distribution log pdf *)
let uniform_logpdf f a b =
  if f >= a && f <= b then -. (log (b -. a)) else neg_infinity

(** Exponential distribution log pdf *)
let exp_logpdf f lam =
  let mu = 1.0 /. lam in
  log (Gsl.Randist.exponential_pdf f ~mu:mu)

(** Bernoulli distribution log pdf *)
let bern_logpdf b p =
  let i = if b then 1 else 0 in
  log (Gsl.Randist.bernoulli_pdf i ~p:p)

(** Beta distribution log pdf *)
let beta_logpdf f a b =
  log (Gsl.Randist.beta_pdf f ~a:a ~b:b)

(** Gamma distribution log pdf *)
let gamma_logpdf f a b =
  log (Gsl.Randist.gamma_pdf f ~a:a ~b:b)

(** Normal distribution sample *)
let normal_sample mu sigma =
  mu +. Gsl.Randist.gaussian seed ~sigma:sigma

(** Uniform distribution sample *)
let uniform_sample a b =
  Gsl.Randist.flat seed ~a:a ~b:b

(** Exponential distribution sample *)
let exp_sample lam =
  let mu = 1.0 /. lam in
  Gsl.Randist.exponential seed ~mu:mu

(** Bernoulli distribution sample *)
let bern_sample p =
  Gsl.Randist.bernoulli seed ~p:p == 1

(** Beta distribution sample *)
let beta_sample a b =
  Gsl.Randist.beta seed ~a:a ~b:b

(** Gamma distribution sample *)
let gamma_sample a b =
  Gsl.Randist.gamma seed ~a:a ~b:b

(** Type constructor for empirical distribution *)
type 'a empirical = {

  (* The set of weighted samples *)
  samples:(float * 'a) list;

  (* The number of samples, for convenience *)
  n:int;

  (* The normalizing constant of this distribution *)
  norm_const:float;

}

(** Compute log(sum_{i=1}^n e^(x_i)) for a list of floats x_i using
    logsumexp-trick *)
let logsumexp ls =
  let max = List.fold_left max neg_infinity ls in
  max +. log (List.fold_left (fun s w -> s +. exp (w -. max)) 0.0 ls)

(** Normalize an empirical distribution so that the weights sum to n, updating
    the normalizing constant. *)
let normalize ({ samples; norm_const; n } as emp) =

  (* Compute the logarithm of the sum of the weights of the samples *)
  let logavg = logsumexp (Utils.map fst samples) -. log (float_of_int n) in

  (* Compute normalized weights *)
  { emp with
    samples = Utils.map (fun (w,t) -> w -. logavg,t) samples;
    norm_const = norm_const +. logavg }

(* Systematic resampling of an empirical distribution. Includes a normalization
   of the distribution. *)
let resample emp =

  (* Normalize distribution *)
  let { samples; norm_const; _ } as emp = normalize emp in

  (* Compute exp-weights from log-weights *)
  let samples_exp = Utils.map (fun (w,t) -> exp w,t) samples in

  (* Draw offset for resampling *)
  let offset = Random.float 1.0 in

  (* Function for performing resampling *)
  let rec rec1 curr next samples_exp acc = match samples_exp with
    | (w,_)::_ -> let curr = curr +. w in rec2 curr next samples_exp acc
    | [] -> acc
  and rec2 curr next samples_exp acc = match samples_exp with
    | (_,t)::tail ->
      if curr > next then rec2 curr (next +. 1.0) samples_exp ((0.0,t)::acc)
      else rec1 curr next tail acc
    | [] -> failwith "Error in resampling" in

  (* Perform the actual resampling *)
  { emp with
    samples = rec1 0.0 offset samples_exp [];
    norm_const }

(** Function for producing a nicely formatted string representation of an
    empirical distribution. Aggregates samples with the
    same value to produce a more compact output.
    TODO Debug, cleanup *)
let string_of_empirical
    ?(aggregate = true)
    ?(diag_ess = true)
    ?(log_weights = false)
    ?(normalize = false)
    ?(compare = compare)
    ?(show_norm_const = true)
    string_of_sample {samples;norm_const;_} =

  (* Aggregate and log_weights do not go together TODO Why not? *)
  let aggregate = aggregate && not log_weights in

  (* Compute ESS Diagnostic TODO Update formula to unnormalized emp as well *)
  let ess =
    (List.fold_left (fun acc (w,_) -> acc +. exp w) 0.0 samples) ** 2.0
    /. List.fold_left (fun acc (w,_) -> acc +. (exp w) ** 2.0) 0.0 samples in

  (* Normalize distribution so that the weights sum to 1, depending on arg *)
  (* TODO Why are we not using the above normalize function here? *)
  let samples =
    if normalize then
      let logsum = logsumexp (Utils.map fst samples) in
      Utils.map (fun (w,t) -> w -. logsum,t) samples
    else samples in

  (* Convert from log weights to ordinary weights, depending on arg *)
  let samples = if log_weights then samples else
      Utils.map (fun (w,t) -> (exp w,t)) samples in

  (* Sort and aggregate samples depending on argument (only available if using
     ordinary weights) *)
  let samples =
    if aggregate then
      let rec aggregate acc emp = match acc,emp with
        | (w1,v1,n)::acc,(w2,v2)::emp when v1 = v2 ->
          aggregate ((w1+.w2,v1,n+1)::acc) emp
        | acc, (w,v)::emp ->
          aggregate ((w,v,1)::acc) emp
        | acc, [] -> acc
      in
      samples
      |> List.sort (fun (_,v1) (_,v2) -> compare v1 v2)
      |> aggregate []
      |> List.sort (fun (w1,_,_) (w2,_,_) -> - Stdlib.compare w1 w2)
    else
      Utils.map (fun (w,v) -> (w,v,1)) samples
  in

  (* Convert values to strings *)
  let samples = List.map
      (fun (w,v,n) -> w,string_of_sample v,n) samples in

  (* Column width for printout *)
  let cw = 15 in

  (* Function for printing one sample *)
  let line (w,v,n) = if aggregate then
      sprintf "  %-*f%-*d%s" cw w cw n v
    else
      sprintf "  %-*f%s" cw w v
  in

  (* The weight header changes depending on if the weights are logarithmic or
     not. *)
  let weight_header = if log_weights then "LOG WEIGHT" else "WEIGHT" in

  (* Header *)
  let header = if aggregate then
      sprintf "%-*s%-*s%s\n" cw weight_header cw "#SAMPLES" "SAMPLE"
    else
      sprintf "%-*s%s\n" cw weight_header "SAMPLE"
  in

  (* Return final string *)
  header
  ^ (String.concat "\n" (Utils.map line samples))
  ^ (if diag_ess then sprintf "\n\n  ESS=%f" ess else "")
  ^
  if show_norm_const then
    sprintf "\n\n  Log normalizing constant: %f\
             \n  Normalizing constant: %f" norm_const (exp norm_const)
  else ""


(** Create a string of unweighted samples from an empirical distribution,
    separated by line breaks *)
let samples_of_empirical string_of_sample emp =
  let {samples;_} = resample emp in
  let x = samples
          |> List.map snd
          |> List.map string_of_sample
          |> String.concat "\n" in
  sprintf "%s" x

