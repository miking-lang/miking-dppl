(** Analytical variance computations for SMC when estimating
    the normalizing constant for discrete programs.
    TODO We could handle symmetries during enumeration better
    TODO We should also sort out code duplication between the different
    inference methods (is, smc, varZSmc, varZIs)
    TODO General cleanup *)

open Ast
open Dist
open Rand
open Sprint
open Debug

(** Outcomes are the central objects in this analysis *)
type outcome = {

  (* The log probability of this outcome *)
  p:float;

  (* The empirical distribution for this outcome *)
  emp:value empirical

}

(** Create a string representation of an outcome *)
let string_of_outcome { p; emp} =
  Printf.sprintf "%s\n\nLog-probability of this outcome: %f\
                 \nProbability of this outcome: %f"
    (string_of_empirical
       ~aggregate:false ~normalize:false ~diag_ess:false
       (string_of_val ~max_boxes:5 ~margin:max_int) emp) p (exp p)

(** TODO *)
let normalize ({ emp ; _ } as o) = { o with emp = Rand.normalize emp }

(** Evaluate a (weight, term) tuple until encountering a sample or resample
    TODO Code duplication *)
let rec eval env (w,t) =
  match Eval.eval false false env t with

  (* Update weights *)
  | V{v=VWeight{cont=Some cont;w=Some w'};_} ->
    let w = w +. w' in

    (* If weight is degenerate, don't bother to continue with execution *)
    if w = neg_infinity then
      w,V{at=va;v=VUnit}
    else
      eval [] (w, mkapp ~t1:(tm_of_val cont) ~t2:nop)

  (* For everything else, simply return *)
  | v -> w,v

(** Generate a list containing all integers between 0 and n *)
let gen_list n =
  let rec recurse res n =
    if n < 0 then res
    else recurse (n :: res) (n-1) in
  recurse [] n

(** Generate a list of all possible multinomial resamplings *)
let gen_resamplings n =
  let rec recurse n m =
    if m = 1 then [[n]]
    else
      let this = gen_list n in
      let rest x = recurse (n - x) (m - 1) in
      this |> List.map (fun x -> List.map (List.cons x) (rest x)) |> List.concat
  in
  recurse n n

(** Factorial function (does not bound check arg) *)
let fact n =
  let rec recurse n acc =
    if n < 2 then acc else recurse (n - 1) (n * acc) in
  recurse n 1

(** Calculates the multinomial coefficient (does not check args) *)
let mult_coefficient n ks =
  fact n / List.fold_left (fun acc k -> acc * fact k) 1 ks

(** TODO *)
let explore { p; emp={samples;_} as emp; _ } =
  let rec recurse prev samples =
    match samples with
    | (w, V{v=VSample{cont=Some cont; d=Some d};_}) :: xs ->
      let sup = support d in
      let outcomes =
        List.map
          (fun (p',sample) ->
             p',
             eval [] (w, mkapp ~t1:(tm_of_val cont) ~t2:(tm_of_val' sample)))
          sup in
      List.map
        (fun (p',v) -> { p = p +. p';
                         emp={emp with
                              samples=List.rev_append prev (v :: xs)}})
        outcomes
    | (_, V{v=VSample _;_}) :: _ -> failwith "Invalid sample"
    | x::xs -> recurse (x::prev) xs
    | [] -> [{p; emp}]
  in

  match recurse [] samples with
  | [{p;emp={samples;_}}] as outcomes ->
    if List.exists
        (fun (_,v) -> match v with V{v=VResamp _;_} -> true | _ -> false)
        samples
    then
      (* If we get here, there is at least one resample, and no samplings,
         amongst our samples. We therefore need to resample. *)

      (* Start by normalizing the empirical distribution (part of the
         normalizing constant computation) *)
      let {samples;n;_} as emp = Rand.normalize emp in

      (* Extract the resampling (log) weights, normalize them to probabilities
         (sum to 1 as proper weights) *)
      let weights =
        List.map (fun (w,_) -> w -. (log (float_of_int n))) samples in

      (* Evaluate everything one step (with weights reset to 0) *)
      let samples =
        List.map
          (fun (_,v) -> match v with
             | V{v=VResamp{cont=Some cont;_};_} ->
               eval [] (0.0, mkapp ~t1:(tm_of_val cont) ~t2:nop)
             | v -> 0.0, v) samples in

      (* Calculate the probability of a given resampling *)
      let prob ws rs =
        let rec recurse res ws rs = match ws,rs with
          | w::ws,r::rs -> recurse (res +. w *. (float_of_int r)) ws rs
          | [],[] -> res
          | _ -> failwith "Weights and resampling does not match" in
        log (float_of_int (mult_coefficient n rs)) +. recurse 0.0 ws rs in

      (* Create a corresponding set of samples for a given resampling *)
      let samples' ss rs =
        let rec recurse res ss rs = match ss,rs with
          | s::ss,r::rs -> recurse (Utils.append r s res) ss rs
          | [],[] -> List.rev res
          | _ -> failwith "Samples and resampling does not match"
        in
        recurse [] ss rs
      in

      (* Create an outcome for each possible resampling *)
      List.map
        (fun rs ->
           let prob = p +. prob weights rs in
           let samples' = samples' samples rs in
           {p=prob;emp={emp with samples=samples'}})
        (gen_resamplings n)

    else
      (* Nothing left to explore *)
      outcomes

  | outcomes -> outcomes

(** Compute variance in the estimate of the normalizing
    constant produced by SMC *)
let var n env program =

  (* Start by constructing the initial outcome *)
  let init =
    { p = 0.0;
      emp =
        { samples = Utils.replicate n (0.0, program) |> Utils.map (eval env);
          n;
          norm_const = 0.0 } } in

  (* Enumerate all possible outcomes *)
  let rec enumerate queue res =
  debug debug_var "Queue"
    (fun () -> String.concat "\n\n" (List.map string_of_outcome queue));

    match queue with
    | [] -> res
    | x :: xs -> match explore x with
      | [] -> failwith "Cannot happen"
      | [x] -> enumerate xs (x :: res)
      | queue' -> enumerate (queue' @ xs) res in

  (* Do the enumeration *)
  let outcomes = enumerate [init] [] in

  (* Compute normalizing constant estimate for each outcome *)
  let outcomes = List.map normalize outcomes in

  debug debug_var "Final set of outcomes"
    (fun () -> String.concat "\n\n" (List.map string_of_outcome outcomes));

  (* Calculate expected value *)
  let exp_val =
    List.fold_left
      (fun a { p; emp = { norm_const; _ } } -> a +. exp (norm_const +. p) )
      0.0 outcomes in

  (* Finally calculate and return variance *)
  List.fold_left
    (fun a { p; emp = { norm_const; _ } } -> a +. exp (2.0 *. norm_const +. p))
    0.0 outcomes
  -. exp_val ** 2.0

