(** Analytical variance computations for IS inference when estimating
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
  Printf.sprintf "%s\n\nLog-probability of this outcome: %f"
    (string_of_empirical
       ~aggregate:false ~normalize:false ~diag_ess:false
       (string_of_val ~max_boxes:5 ~margin:max_int) emp) p

(** TODO *)
let normalize ({ emp ; _ } as o) = { o with emp = Rand.normalize emp }

(** Evaluate a (weight, term) tuple until encountering a sample
    TODO Lots of code duplication here (see smc.ml and is.ml) *)
let rec eval env (w,t) =
  match Eval.eval false false env t with

  (* Ignore resampling, since this is specific for SMC
     TODO update for var_smc *)
  | V{v=VResamp{cont=Some cont;_};_} ->
    eval [] (w, mkapp ~t1:(tm_of_val cont) ~t2:nop)

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
    | x::xs -> recurse (x::prev) xs
    | [] -> [] in
  recurse [] samples

(** Compute variance in the estimate of the normalizing
    constant produced by IS *)
let var n env program =

  (* Start by constructing the initial outcome *)
  let init =
    { p = 0.0;
      emp =
        { samples = Utils.replicate n (0.0, program) |> Utils.map (eval env);
          n;
          norm_const = 0.0 } } in

  (* Enumerate all possible outcomes *)
  let rec enumerate queue res = match queue with
    | [] -> res
    | x :: xs -> match explore x with
      | [] -> enumerate xs (x :: res)
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
