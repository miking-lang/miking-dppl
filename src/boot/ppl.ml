
(*
   Miking is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE.txt

   The main experiment platform for probabilitic programming.
   Main contributer: Daniel Lundén
*)

open Utils
open Ustring.Op
open Printf
open Ast
open Msg
open Pprint

let enable_debug_cps = false
let enable_debug_cps_builtin = false
let enable_debug_infer = false
let enable_debug_sanalysis = false

let random_seed = true

type inference =
  | Importance     of int
  | SMCAligned     of int
  | SMCUnaligned   of int

let inference = ref (Importance(10))

(** eval ref since I can't import Boot (circular) TODO Refactor? **)
let empty_eval _ t = t
let eval = ref empty_eval

(** Identifiers for atoms **)
let anormal = usid "normal"
let auniform = usid "uniform"
let agamma = usid "gamma"
let aexp = usid "exp"
let abern = usid "bern"
let ainfer = usid "infer"
let aprob = usid "prob"
let asample = usid "sample"
let aweight = usid "weight"
let adweight = usid "dweight"

(** All CPS transformed atoms **)
let pre_cps_builtin = [
  (* Continuous distributions *)
  ("normal", CAtom(anormal, []));
  ("uniform", CAtom(auniform, []));
  ("gamma", CAtom(agamma, []));
  ("exponential", CAtom(aexp, []));

  (* Discrete distributions *)
  ("bernoulli", CAtom(abern, []));

  (* Other atoms *)
  ("infer", CAtom(ainfer, []));
  ("prob", CAtom(aprob, []));
]

(** All non-CPS-transformed atoms **)
let post_cps = [
  ("sample", asample);
  ("weight", aweight);
  ("dweight", adweight);
]
let post_cps_builtin = List.map (fun (str,id) -> (str,CAtom(id,[]))) post_cps

(** Used for constants with unspecified arities **)
let fail_arity c =
  failwith ("Arity of " ^
            Ustring.to_utf8 (pprint_const c) ^ " not specified.")

(** Arity specification for atoms **)
let atom_arity c =
  let id,len = match c with
    | CAtom(id,ls) -> id, List.length ls
    | _ -> fail_arity c in
  let max_arity =
    (*** CPS transformed ***)
    if      id = anormal  then 2
    else if id = auniform then 2
    else if id = agamma   then 2
    else if id = aexp     then 1
    else if id = abern    then 1
    else if id = ainfer   then 1
    else if id = aprob    then 2

    (*** Not CPS transformed (continuations included in arity) ***)
    else if id = asample  then 2
    else if id = aweight  then 2
    else if id = adweight then 2
    else fail_arity c
  in max_arity - len

(** Generate fresh variable names for CPS transformation.  Avoids clashes by
    using $ as first char (not allowed in lexer for vars).  Takes a debruijn
    index as argument (for idfun). **)
let nextvar = ref 0
let genvar i =
  let res = !nextvar in
  let ustr = us "$" ^. ustring_of_int res in
  nextvar := res + 1;
  (ustr, TmVar(def_attr,NoInfo, ustr, i, false))

(** The identity function (with debruijn index) as a tm. **)
let idfun =
  let var, var' = genvar 0 in
  TmLam(def_attr,NoInfo, var, var')

(** Gsl default seed **)
let seed =
  let rng = Gsl.Rng.make (Gsl.Rng.default ()) in
  if random_seed then
    (Random.self_init();
     Gsl.Rng.set rng (Random.nativeint Nativeint.max_int));
  rng

(** Probability functions for built in distributions. **)
let prob value dist = match dist with
  | TmConst(_,fi, CAtom(dist, args)) ->
    (match value, args with

     (* Normal distribution *)
     | TmConst(_,_, CFloat(v)),
       [TmConst(_,_, CFloat(sigma));
        TmConst(_,_, CFloat(mu))] when dist = anormal ->
       TmConst(def_attr,fi,
               CFloat(Gsl.Randist.gaussian_pdf (v -. mu) ~sigma:sigma))

     (* Exponential distribution *)
     | TmConst(_,_, CFloat(v)),
       [TmConst(_,_, CFloat(lambda))] when dist = aexp ->
       let mu = 1.0 /. lambda in
       TmConst(def_attr,fi, CFloat(Gsl.Randist.exponential_pdf v ~mu:mu))

     (* Bernoulli distribution *)
     | TmConst(_,_, CBool(v)),
       [TmConst(_,_, CFloat(p))] when dist = abern ->
       let i = if v then 1 else 0 in
       TmConst(def_attr,fi, CFloat(Gsl.Randist.bernoulli_pdf i ~p:p))

     (* Gamma distribution *)
     | TmConst(_,_, CFloat(v)),
       [TmConst(_,_, CFloat(a));
        TmConst(_,_, CFloat(b))] when dist = agamma ->
       TmConst(def_attr,fi, CFloat(Gsl.Randist.gamma_pdf v ~a:a ~b:b))

     | _ -> failwith "Unknown distribution applied as argument to prob")
  | _ -> failwith "Incorrect distribution applied as argument to prob"

(** Sample functions for built in distributions. **)
let sample dist = match dist with
  | TmConst(_,fi, CAtom(dist, args)) ->
    (match args with

     (* Normal distribution *)
     | [TmConst(_,_, CFloat(sigma));
        TmConst(_,_, CFloat(mu))] when dist = anormal ->
       TmConst(def_attr,fi,
               CFloat(mu +. Gsl.Randist.gaussian seed ~sigma:sigma))

     (* Exponential distribution *)
     | [TmConst(_,_, CFloat(lambda))] when dist = aexp ->
       let mu = 1.0 /. lambda in
       TmConst(def_attr,fi, CFloat(Gsl.Randist.exponential seed ~mu:mu))

     (* Bernoulli distribution *)
     | [TmConst(_,_, CFloat(p))] when dist = abern ->
       let b = Gsl.Randist.bernoulli seed ~p:p == 1 in
       TmConst(def_attr,fi, CBool(b))

     (* Gamma distribution *)
     | [TmConst(_,_, CFloat(a));
        TmConst(_,_, CFloat(b))] when dist = agamma ->
       TmConst(def_attr,fi, CFloat(Gsl.Randist.gamma seed ~a:a ~b:b))

     | _ -> failwith "Unknown distribution applied as argument to sample")
  | _ -> failwith (sprintf
                     "Incorrect distribution %s applied as argument to sample"
                     (Ustring.to_utf8 (pprint false dist)))

(** Importance sampling (Likelihood weighting) inference **)
let infer_is model n =

  (* Remove continuation by applying idfun *)
  let model = !eval [] (TmApp(def_attr,NoInfo, model, idfun)) in

  (* Replicate model for #samples times with an initial log weight of 0.0 *)
  let s = replicate n (TmApp(def_attr,NoInfo, model, TmNop(def_attr)), 0.0) in

  (* Evaluate one sample to the end *)
  let rec sim (t, w) =
    let t = !eval [] t in
    match t with

    (* Sample *)
    | TmConst(_,_, CAtom(id, args))
      when id = asample -> (match args with
        | [dist; cont] -> sim (TmApp(def_attr,NoInfo, cont, sample dist), w)
        | _ -> failwith (sprintf "Incorrect sample application in infer_is: %s"
                           (Ustring.to_utf8 (pprint false t))))

    (* Weight *)
    | TmConst(_,_fi, CAtom(id, args))
      when id = aweight -> (match args with
        | [TmConst(_,_, CFloat(wadj)); cont] ->
          if wadj = -. infinity then (TmNop(def_attr),wadj) else
            sim (TmApp(def_attr,NoInfo, cont, TmNop(def_attr)), w +. wadj)
        | _ -> failwith (sprintf "Incorrect weight application in infer_is: %s"
                           (Ustring.to_utf8 (pprint false t))))

    (* Result *)
    | _ -> t, w in

  let res = List.map sim s in

  if enable_debug_infer then
    (print_endline "-- infer result --";
     List.iter
       (fun (t, w) ->
          print_string "Sample: ";
          uprint_string (pprint false t);
          print_string ", Log weight: ";
          print_endline (string_of_float w))
       res;
     print_newline ());

  TmNop(def_attr) (* Here we should return an empirical distribution *)

(** SMC inference **)
let infer_smc model n =

  (* Remove continuation by applying idfun *)
  let model = !eval [] (TmApp(def_attr,NoInfo, model, idfun)) in

  (* Replicate model for #samples times with an initial log weight of 0.0 *)
  let s = replicate n (TmApp(def_attr,NoInfo, model, TmNop(def_attr)), 0.0) in

  (* Evaluate a sample until encountering a weight *)
  let rec sim (t, w) =
    let t = !eval [] t in

    match t with

    (* Sample *)
    | TmConst(_,_, CAtom(id, args))
      when id = asample ->
      (match args with
        | [dist; cont] -> sim (TmApp(def_attr,NoInfo,cont,sample dist), w)
        | _ -> failwith (sprintf "Incorrect sample application in infer: %s"
                           (Ustring.to_utf8 (pprint false t))))

    (* Dweight *)
    | TmConst(_,_fi,CAtom(id,args))
      when id = adweight ->
      (match args with
        | [TmConst(_,_,CFloat(wadj)); cont] ->
          if wadj = -. infinity then (true,TmNop(def_attr),wadj) else
            sim (TmApp(def_attr,NoInfo,cont,TmNop(def_attr)), w +. wadj)
        | _ -> failwith (sprintf "Incorrect dweight application in infer: %s"
                           (Ustring.to_utf8 (pprint false t))))

    (* Weight *)
    | TmConst(_,_fi,CAtom(id,args))
      when id = aweight ->
      (match args with
        | [TmConst(_,_, CFloat(wadj)); cont] ->
          if wadj = -. infinity then (true,TmNop(def_attr),wadj) else
            (false,TmApp(def_attr,NoInfo,cont,TmNop(def_attr)), w +. wadj)
        | _ -> failwith (sprintf "Incorrect weight application in infer: %s"
                           (Ustring.to_utf8 (pprint false t))))

    (* Result *)
    | _ -> true,t,w in

  (* Systematic resampling *)
  let resample s =
    (* Compute the logarithm of the average of the weights using
       logsumexp-trick *)
    let weights = List.map snd s in
    let max = List.fold_left max (-. infinity) weights in
    let logavg =
      log (List.fold_left (fun s w -> s +. exp (w -. max)) 0.0 weights)
        +. max -. log (float n) in

    (* Compute normalized weights from log-weights *)
    let snorm = List.map (fun (t,w) -> t, exp (w -. logavg)) s in

    (* Draw offset for resampling *)
    let offset = Random.float 1.0 in

    (* Perform resampling *)
    let rec rec1 curr next snorm acc = match snorm with
      | (_,w)::_ -> let curr = curr +. w in rec2 curr next snorm acc
      | [] -> acc
    and rec2 curr next snorm acc = match snorm with
      | (t,_)::tail ->
        if curr > next then rec2 curr (next +. 1.0) snorm ((t,0.0)::acc)
        else rec1 curr next tail acc
      | [] -> failwith "Error in resampling" in

    (* Also return the log average for computing the normalization constant *)
    logavg, rec1 0.0 offset snorm [] in

  (* Run SMC *)
  let rec smc s normconst =
    let res = List.map sim s in
    let b = List.for_all (fun (b,_,_) -> b) res in
    let logavg, res = res |> List.map (fun (_,t,w) -> (t,w)) |> resample in
    let normconst = normconst +. logavg in
    if b then begin
      print_endline (string_of_float normconst);

      TmNop(def_attr) (* Here we should return an empirical distribution *)
    end else
      smc res normconst

  in smc s 0.0

(** Select correct inference algorithm **)
let infer model = match !inference with
  | Importance(i) -> infer_is model i
  | SMCUnaligned(i) | SMCAligned(i) -> infer_smc model i

(** This is the main hook for new constructs in the mcore.**)
let eval_atom fi id tms v =
  let args = v :: tms in
  let c = CAtom(id, args) in
  if arity c = 0 then
    match args with
    | [model]      when id = ainfer -> infer model
    | [dist; v]    when id = aprob  -> prob v dist
    | _ -> TmConst(def_attr,fi,c)
  else
    TmConst(def_attr,fi,c)

(** Used for unsupported CPS transformations **)
let fail_cps tm =
  failwith ("CPS-transformation of " ^
            Ustring.to_utf8 (pprint false tm) ^ " not supported")

(** Wrap constant functions in CPS forms **)
let cps_const t = match t with
  | TmConst(_,_,c) ->
    let vars = List.map genvar (replicate (arity c) noidx) in
    let inner = List.fold_left
        (fun acc (_, v') ->
           TmApp(def_attr,NoInfo, acc, v'))
        t vars in
    List.fold_right
      (fun (v, _) acc ->
         let k, k' = genvar noidx in
         TmLam(def_attr,
               NoInfo, k, TmLam(def_attr,NoInfo,
                                v, TmApp(def_attr,NoInfo, k', acc))))
      vars inner
  | _ -> failwith "cps_const of non-constant"

(** CPS transformations
    Requirements:
    - All functions must take one extra parameter: a continuation function with
    exactly one parameter
    - A function never "returns" (i.e., it never returns something that is not a
    TmApp). Instead, it applies its continuation to its "return value". **)

(** Atomic cps transformation (basically everything except function
    application).  Atomic means that we can CPS transform the expression without
    supplying a continuation to the transformation.  **)
let rec cps_atomic t = match t with
  | TmVar _ -> t

  | TmLam(_,fi,x,t1) ->
    let k, k' = genvar noidx in
    TmLam(def_attr,NoInfo, k, TmLam(def_attr,fi, x, cps k' t1))

  (* Should not exist before eval *)
  | TmClos _-> fail_cps t

  (* Function application is not atomic. *)
  | TmApp _ -> failwith "TmApp is not atomic."

  (* Records are treated as atomic for now, but can actually be complex. TODO
     Fix? *)
  | TmRec _ -> t

  (* Tuple projection can also be complex, but is treated as atomic for now.
     TODO Fix? *)
  | TmProj _ -> t

  (* Constant transformation  *)
  | TmConst(_,_,CAtom(id,[])) ->
    if List.mem id (post_cps |> List.map snd) then t
    else cps_const t

  | TmConst(_,_,CAtom(_,_)) ->
    failwith "Should not exist any arguments for atoms"

  | TmConst _ -> cps_const t

  (* Not supported *)
  | TmPEval _ -> fail_cps t

  (* Transforms similarly to constant functions. The difference is that the
     last continuation must be supplied to the branches, and not applied to
     the result. *)
  | TmIfexp _ ->
    let a, a' = genvar noidx in
    let b, b' = genvar noidx in
    let c, c' = genvar noidx in
    let c1, c1' = genvar noidx in
    let c2, c2' = genvar noidx in
    let c3, c3' = genvar noidx in
    let bapp = TmApp(def_attr,NoInfo, b', c3') in
    let capp = TmApp(def_attr,NoInfo, c', c3') in
    let inner =
      TmApp(def_attr,NoInfo,
            TmApp(def_attr,NoInfo,
                  TmApp(def_attr,NoInfo, t, a'), bapp), capp) in
    let clam =
      TmLam(def_attr,NoInfo,
            c3, TmLam(def_attr,NoInfo, c, inner)) in
    let blam =
      TmLam(def_attr,NoInfo, c2,
            TmLam(def_attr,NoInfo, b, TmApp(def_attr,NoInfo, c2', clam))) in
    let alam =
      TmLam(def_attr,NoInfo, c1,
            TmLam(def_attr,NoInfo, a, TmApp(def_attr,NoInfo, c1', blam))) in
    alam

  (* Treat similar as constant function with a single argument. We need to
     apply the id function to the argument before applying fix, since the
     argument expects a continuation as first argument. TODO Correct? *)
  | TmFix _ ->
    let v, v' = genvar noidx in
    let k, k' = genvar noidx in
    let inner = TmApp(def_attr,NoInfo,
                      t, TmApp(def_attr,NoInfo, v', idfun)) in
    TmLam(def_attr,NoInfo,
          k, TmLam(def_attr, NoInfo, v,
                   TmApp(def_attr,NoInfo, k', inner)))

  (* Treat as constant *)
  | TmChar _ -> t

  (* Not supported *)
  | TmExprSeq _ -> fail_cps t

  (* Treat as constant *)
  | TmUC _ -> t

  (* CPS transform both lhs and rhs and apply identity function on result.
     Also transform tnext. TODO Move to complex? *)
  | TmUtest(_,fi, t1, t2, tnext) ->
    TmUtest(def_attr,fi, cps idfun t1, cps idfun t2, cps idfun tnext)

  (* Not supported *)
  | TmMatch _ -> fail_cps t

  (* Treat as constant *)
  | TmNop _ -> t

(** Complex cps transformation. Complex means that the term is a computation
    (i.e., not a value). A continuation must also be supplied as argument to the
    transformation. **)
and cps cont t =
  match t with
  (* Function application is a complex expression (since it is a computation).
     Optimize the case when either the function or argument is atomic. *)
  | TmApp(_,fi,t1,t2) ->
    let wrapopt (a, a') = Some a, a' in
    let f, f' = match t1 with
      | TmApp _ -> wrapopt (genvar noidx)
      | _ -> None, cps_atomic t1 in
    let e, e' = match t2 with
      | TmApp _ -> wrapopt (genvar noidx)
      | _ -> None, cps_atomic t2 in
    let app = TmApp(def_attr,fi, TmApp(def_attr,NoInfo, f', cont), e') in
    let inner = match e with
      | None -> app
      | Some(e) -> cps (TmLam(def_attr,NoInfo, e, app)) t2 in
    let outer = match f with
      | None -> inner
      | Some(f) -> cps (TmLam(def_attr,NoInfo, f, inner)) t1 in
    outer

  (* Everything else is atomic *)
  | _ -> TmApp(def_attr,NoInfo, cont, cps_atomic t)

(** Function for uniquely labeling all subterms and variables in a term **)
let label builtin tm =
  let open StrMap in
  let label = ref 0 in
  let next () = let res = !label in label := !label + 1; res in
  let rec label_vars map tm = match tm with
    | TmVar(a,fi,x,i1,pe) ->
      (match find_opt x map with
       | Some i2 -> TmVar({a with var_label = i2},fi,x,i1,pe)
       | _ -> failwith ("Unbound var: " ^ (Ustring.to_utf8 x)))
    | TmLam(a,fi,x,t1) ->
      let i = next() in TmLam({a with var_label = i},fi,x,
                              label_vars (add x i map) t1)
    | TmApp(a,fi,t1,t2) -> TmApp(a,fi,label_vars map t1, label_vars map t2)
    | TmClos _ -> failwith "Closure before eval"
    | TmConst _ | TmIfexp _ | TmFix _ | TmRec _ | TmProj _ | TmNop _ -> tm

    | _ -> failwith "Not supported" in
  let rec label_terms tm = match tm with
    | TmVar(a,fi,x,i1,pe) -> TmVar({a with label=next()},fi,x,i1,pe)
    | TmLam(a,fi,x,t1)    -> TmLam({a with label=next()},fi,x,
                                   label_terms t1)
    | TmApp(a,fi,t1,t2)   -> TmApp({a with label = next()},fi,
                                   label_terms t1,label_terms t2)
    | TmConst(a,fi,c)     -> TmConst({a with label=next()},fi,c)
    | TmIfexp(a,fi,c,t1)  -> TmIfexp({a with label=next()},fi,c,t1)
    | TmFix(a,fi)         -> TmFix({a with label=next()},fi)
    | TmRec(a,fi,sm)      -> TmRec({a with label=next()},fi,sm)
    | TmProj(a,fi,t1,x)   -> TmProj({a with label=next()},fi,t1,x)
    | TmNop(a)            -> TmNop({a with label=next()})

    | TmClos _ -> failwith "Closure before eval"
    | _ -> failwith "Not supported" in
  let sm = List.fold_left
      (fun sm x -> add x (next ()) sm)
      empty builtin in
  let tm = tm |> label_vars sm |> label_terms in
  tm, sm, !label

(** Abstract values used in the 0-CFA analysis **)
type absval =
  | Stoch
  | Fun of { louter:int; linner:int; lvar:int }
  | Fix

(** Returns string representation of abstract values **)
let string_of_absval = function
  | Stoch -> "Stoch"
  | Fun{louter;linner;lvar} ->
    "Fun(" ^
    (String.concat "," (List.map string_of_int [louter;linner;lvar])) ^ ")"
  | Fix -> "Fix"

(** Constraints used in the 0-CFA analysis **)
type cstr =
  | Dir of absval * int
  | Sub of int * int
  | Impl of absval * int * int * int

(** Returns string representation of constraints **)
let string_of_cstr = function
  | Dir(av,n) -> string_of_absval av ^ " in " ^ string_of_int n
  | Sub(n1,n2) -> string_of_int n1 ^ " in " ^ string_of_int n2
  | Impl(av,n1,n2,n3) ->
    string_of_absval av ^ " in " ^ string_of_int n1
    ^ " => " ^ string_of_int n2 ^ " in " ^ string_of_int n3

(** Returns abstract value representations of all functions in a program **)
let functions tm =
  let rec recurse tm funs = match tm with
    | TmVar _ -> funs
    | TmLam({label;var_label},_,_,t1) ->
      Fun{louter=label;linner=tm_label t1;lvar=var_label} :: recurse t1 funs
    | TmApp(_,_,t1,t2) -> funs |> recurse t1 |> recurse t2
    | TmConst _ | TmIfexp _ | TmFix _
    | TmRec _ | TmProj _ | TmNop _ -> funs
    | TmClos _ -> failwith "Closure before eval"
    | _ -> failwith "Not supported" in
  recurse tm []

(** Generate a set of 0-CFA constraints for a program. For now, built in
    functions must be applied immediately  where occuring (no currying). **)
let gen_cstrs bmap tm =
  let idmatch str id =
    match StrMap.find_opt (us str) bmap with
    | Some i -> i = id
    | _ -> false in
  let funs = functions tm in
  let rec recurse tm cstrs = match tm with
    (* Binary operators *)
    | TmApp({label=l;_},_,TmApp(_,_,TmConst(_,_,const),t1),t2)
      when arity const = 2 ->
      let l1 = tm_label t1 in
      let l2 = tm_label t2 in
      let cstrs = cstrs |> recurse t1 |> recurse t2 in
      Sub(l1,l) :: Sub(l2,l) :: cstrs

    (* Unary operators *)
    | TmApp({label=l;_},_,TmConst(_,_,const),t1)
      when arity const = 1 ->
      let l1 = tm_label t1 in
      let cstrs = cstrs |> recurse t1 in
      Sub(l1,l) :: cstrs

    (* If expressions *)
    | TmApp({label=l;_},_, TmApp(_,_, TmApp(_,_,TmIfexp(_,_,_,_),t1),
                                 TmLam(_,_,_,t2)), TmLam(_,_,_,t3)) ->
      let l2 = tm_label t2 in
      let l3 = tm_label t3 in
      let cstrs = cstrs |> recurse t1 |> recurse t2 |> recurse t3 in
      Sub(l2,l) :: Sub(l3,l) :: cstrs

    (* Sample *)
    | TmApp({label=l;_},_,TmVar({var_label;_},_,_,_,_),t1)
      when idmatch "sample" var_label ->
      let cstrs = cstrs |> recurse t1 in
      Dir(Stoch, l) :: cstrs

    (* Fixpoint *)
    | TmApp({label;_},_,TmFix(_,_), t1) ->
      let l = label in
      let l1 = tm_label t1 in
      let cstrs = cstrs |> recurse t1 in
      List.fold_left
        (fun cstrs av -> match av with
           | Fun{linner=l2;lvar=x;_} ->
             Impl(av,l1,l2,x) :: Impl(av,l1,l2,l) :: cstrs
           | _ -> failwith "Non-fun absval in funs")
        cstrs funs

    (* Variables *)
    | TmVar({label=l;var_label=x},_,_,_,_) -> Sub(x, l) :: cstrs

    (* Lambdas *)
    | TmLam({label;var_label},_,_,t1) ->
      Dir(Fun{louter=label;linner=tm_label t1;lvar=var_label},label)
      :: recurse t1 cstrs

    (* General applications (not caught by operators/keywords above) *)
    | TmApp({label=l;_},_,t1,t2) ->
      let l1 = tm_label t1 in
      let l2 = tm_label t2 in
      let cstrs = cstrs |> recurse t1 |> recurse t2 in
      List.fold_left
        (fun cstrs av -> match av with
           | Fun{linner=l3;lvar=x;_} ->
             Impl(av,l1,l2,x) :: Impl(av,l1,l3,l) :: cstrs
           | _ -> failwith "Non-fun absval in funs")
        cstrs funs

    | TmConst _ | TmIfexp _ | TmRec _ | TmProj _ | TmNop _ -> cstrs

    | TmClos _ -> failwith "Closure before eval"
    | _ -> failwith "Not supported"
  in recurse tm []

(** Sets of abstract values **)
module AbsValSet = Set.Make(struct let compare = compare type t = absval end)

(** Analyze the program using 0-CFA to discover dynamic parts **)
let analyze bmap tm nl =
  let open AbsValSet in
  let worklist = ref [] in
  let data = Array.make nl empty in
  let edges = Array.make nl [] in
  let cstrs = gen_cstrs bmap tm in
  let add q d =
    if not (subset d data.(q)) then
      (data.(q) <- union data.(q) d;
       worklist := q :: !worklist) in

  if enable_debug_sanalysis then
    (print_endline "-- constraints --";
     List.iter (fun cstr -> print_endline (string_of_cstr cstr)) cstrs;
     uprint_newline ());

  (* Building the graph *)
  let f cstr = match cstr with
    | Dir(t,p) -> add p (singleton t)
    | Sub(p1,_) -> edges.(p1) <- cstr :: edges.(p1)
    | Impl(_,p,p1,_) ->
      edges.(p1) <- cstr :: edges.(p1);
      edges.(p)  <- cstr :: edges.(p) in
  List.iter f cstrs;

  (* Iteration *)
  while match !worklist with [] -> false | _ -> true do
    let q = List.hd !worklist in
    worklist := List.tl !worklist;
    let f cstr = match cstr with
      | Sub(p1,p2) -> add p2 data.(p1)
      | Impl(t,p,p1,p2) -> if mem t data.(p) then add p2 data.(p1)
      | Dir _ -> failwith "Direct constraint in iteration" in
    List.iter f edges.(q)
  done;

  (* Mark dynamic parts *)
  let mark = Array.make nl false in
  let modified = ref true in

  let rec recurse flag tm =
    let l = tm_label tm in
    if flag || mark.(l) then
      (if not mark.(l) then (mark.(l) <- true; modified := true);
       iter (fun av -> match av with
           | Fun{louter=l;_} ->
             if not mark.(l) then (mark.(l) <- true; modified := true)
           | _ -> ())
         data.(l));
    match tm with
    | TmApp(_,_,TmApp(_,_,TmApp(_,_,TmIfexp(_,_,_,_),t1),t2),t3)
      when not flag ->
      recurse flag t1;
      let flag = mem Stoch data.(tm_label t1) in
      recurse flag t2; recurse flag t3

    | TmVar _ -> ()
    | TmLam({label=l;_},_,_,t1) -> recurse (mark.(l) || flag) t1

    | TmApp(_,_,t1,t2) -> recurse flag t1; recurse flag t2;


    | TmConst _ | TmIfexp _ | TmFix _ | TmRec _
    | TmProj _ | TmNop _ -> ()

    | TmClos _ -> failwith "Closure before eval"
    | _ -> failwith "Not supported" in

  while !modified do
    modified := false;
    recurse false tm;
  done;

  if enable_debug_sanalysis then
    (print_endline "-- data --";
     Array.iteri (fun i set ->
         print_string ("Label " ^ string_of_int i ^ ": { ");
         print_string (String.concat ", "
                         (List.map string_of_absval (elements set)));
         print_endline (" }")) data;
     uprint_newline ();

     print_endline "-- dynamic --";
     Array.iteri
       (fun i b ->
          print_endline ("Label " ^ string_of_int i ^ " = " ^ string_of_bool b);
       ) mark;
     uprint_newline ());

  mark

(** Transform all dynamic weights to dweights. We ignore other synchronization
    checkpoints for now since we are only dealing with SMC. **)
let align bmap dyn tm =
  let idmatch str id =
    match StrMap.find_opt (us str) bmap with
    | Some i -> i = id
    | _ -> false in
  let rec recurse tm = match tm with
    | TmVar({label;var_label} as a,fi,_,_,_)
      when idmatch "weight" var_label ->
      if dyn.(label) then TmConst(a,fi,CAtom(adweight,[]))
      else tm

    | TmLam(a,fi,x,t1) -> TmLam(a,fi,x,recurse t1)

    | TmApp(a,fi,t1,t2) -> TmApp(a,fi,recurse t1,recurse t2)

    | TmFix _ | TmVar _ | TmConst _
    | TmIfexp _ | TmRec _ | TmProj _ | TmNop _ -> tm

    | TmClos _ -> failwith "Closure before eval"
    | _ -> failwith "Not supported"
  in recurse tm

(** Main function for the evaluation of a probabilistic program **)
let evalprog debruijn eval' builtin argv filename =
  eval := eval';
  if !utest then printf "%s: " filename;
  utest_fail_local := 0;
  let fs1 = open_in filename in
  let tablength = 8 in

  (* Check for flags if not running tests *)
  if not (!utest) then begin
    let argv = Array.of_list ("cmd" :: argv) in
    let speclist = Arg.align [

        "--inference",
        String(fun s -> match s with
            | "is" -> inference := Importance(10)
            | "smcu" -> inference := SMCUnaligned(10)
            | "smca" -> inference := SMCAligned(10)
            | _ -> failwith "Incorrect inference algorithm"
          ),
        " Specifies inference method. Options are: is, smcu, smca.";

        "--samples",
        Int(fun i -> match !inference, i with
            | _,i when i < 1 -> failwith "Number of samples must be positive"
            | Importance _,i -> inference := Importance(i)
            | SMCUnaligned _,i -> inference := SMCUnaligned(i)
            | SMCAligned _,i -> inference := SMCAligned(i)),
        " Determines the number of samples (positive).";

      ] in
    let anon_fun _str = () in
    let usage_msg = "" in
    Arg.parse_argv argv speclist anon_fun usage_msg;
  end;

  begin try

      (* Get main program *)
      let tm =
        Ppllexer.init (us filename) tablength;
        fs1 |> Ustring.lexing_from_channel
        |> Pplparser.main Ppllexer.main in

      if enable_debug_cps then
        (print_endline "-- pre cps --";
         uprint_endline (pprint false tm);
         uprint_newline ());

      (* Function for converting consts in builtin to tms *)
      let tm_of_builtin b = List.map (fun (x, y) -> x, tm_of_const y) b in

      (* If chosen inference is aligned SMC, perform static analysis *)
      let tm = if match !inference with SMCAligned _ -> true | _ -> false
        then begin
          (* Label program and builtins in preparation for static analysis *)
          let tm,bmap,nl = label
              ((builtin @ pre_cps_builtin @ post_cps_builtin)
               |> List.split |> fst |> List.map us) tm in

          if enable_debug_sanalysis then
            (print_endline "-- after labeling --";
             uprint_endline (pprintl tm);
             uprint_newline ());

          (* Perform static analysis, returning all dynamic labels *)
          let dyn = analyze bmap tm nl in

          (* By using the above analysis results, transform all dynamic
             checkpoints. This information will be forwarded to the inference
             algorithm. *)
          align bmap dyn tm
        end else tm in


      if enable_debug_sanalysis then
        (print_endline "-- after SMC alignment --";
         uprint_endline (pprintl tm);
         uprint_newline ());

      let builtin = builtin
                    (* Convert builtins from consts to tms *)
                    |> tm_of_builtin

                    (* Add PPL builtins that should be CPS transformed *)
                    |> (@) (tm_of_builtin pre_cps_builtin)

                    (* Transform builtins to CPS. Required since we need to
                       wrap constant functions in CPS forms *)
                    |> List.map (fun (x, y) -> (x, (cps_const y)))

                    (* Add PPL builtins that should not be CPS transformed *)
                    |> (@) (tm_of_builtin post_cps_builtin)

                    (* Debruijn transform builtins (since they have now been
                       CPS transformed) *)
                    |> List.map (fun (x, y) -> (x, debruijn [] y)) in

      if enable_debug_cps_builtin then
        (print_endline "-- cps builtin --";
         (List.iter uprint_endline
            (List.map
               (fun (x, y) -> us x ^. us" = " ^. pprint false y) builtin));
         print_newline ());

      (* Perform CPS transformation of main program *)
      let cps = cps idfun tm in

      if enable_debug_cps then
        (print_endline "-- post cps --";
         uprint_endline (pprint false cps);
         print_newline ());

      (* Evaluate CPS form of main program *)
      let res =
        cps |> debruijn (builtin |> List.split |> fst |> List.map us)
        |> !eval (builtin |> List.split |> snd) in

      if enable_debug_cps then
        (print_endline "-- post cps eval --";
         uprint_endline (pprint false res));

    with
    | Ppllexer.Lex_error m ->
      if !utest then (
        printf "\n ** %s" (Ustring.to_utf8 (Msg.message2str m));
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1)
      else
        fprintf stderr "%s\n" (Ustring.to_utf8 (Msg.message2str m))
    | Error m ->
      if !utest then (
        printf "\n ** %s" (Ustring.to_utf8 (Msg.message2str m));
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1)
      else
        fprintf stderr "%s\n" (Ustring.to_utf8 (Msg.message2str m))
    | Parsing.Parse_error ->
      if !utest then (
        printf "\n ** %s"
          (Ustring.to_utf8 (Msg.message2str (Ppllexer.parse_error_message())));
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1)
      else
        fprintf stderr "%s\n"
          (Ustring.to_utf8 (Msg.message2str (Ppllexer.parse_error_message())))
  end; close_in fs1;
  if !utest && !utest_fail_local = 0 then printf " OK\n"
