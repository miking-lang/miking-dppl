
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

(* eval ref since I can't import Boot (circular) TODO Refactor? *)
let empty_eval _ t = t
let eval = ref empty_eval

(* Identifiers for atoms *)
let anormal = usid "normal"
let auniform = usid "uniform"
let agamma = usid "gamma"
let aexp = usid "exp"
let abern = usid "bern"
let ainfer = usid "infer"
let aprob = usid "prob"
let asample = usid "sample"
let aweight = usid "weight"

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

(* All non-CPS-transformed atoms *)
let post_cps_builtin = [
  ("sample", CAtom(asample, []));
  ("weight", CAtom(aweight, []));
]

(* Used for constants with unspecified arities *)
let fail_arity c =
  failwith ("Arity of " ^
            Ustring.to_utf8 (pprint_const c) ^ " not specified.")

(* Arity specification for atoms *)
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
    else fail_arity c
  in max_arity - len

(* Generate fresh variable names for CPS transformation.  Avoids clashes by
   using $ as first char (not allowed in lexer for vars).  Takes a debruijn
   index as argument (for idfun). *)
let nextvar = ref 0
let genvar i =
  let res = !nextvar in
  let ustr = us "$" ^. ustring_of_int res in
  nextvar := res + 1;
  (ustr, TmVar(def_attr,NoInfo, ustr, i, false))

(* The identity function (with debruijn index) as a tm. *)
let idfun =
  let var, var' = genvar 0 in
  TmLam(def_attr,NoInfo, var, var')

(* Gsl default seed *)
let seed = Gsl.Rng.make (Gsl.Rng.default ())
(* let sample = (fun () -> Gsl.Randist.gaussian seed 200.0) *)

(* Probability functions for built in distributions. *)
let prob value dist = match dist with
  | TmConst(_,fi, CAtom(dist, args)) ->
    (match value, args with
     | TmConst(_,_, CFloat(v)),
       [TmConst(_,_, CFloat(sigma));
        TmConst(_,_, CFloat(mu))] when dist = anormal ->
       TmConst(def_attr,fi,
               CFloat(Gsl.Randist.gaussian_pdf (v -. mu) ~sigma:sigma))
     | _ -> failwith "Unknown distribution applied as argument to prob")
  | _ -> failwith "Incorrect distribution applied as argument to prob"

(* Sample functions for built in distributions. *)
let sample dist = match dist with
  | TmConst(_,fi, CAtom(dist, args)) ->
    (match args with
     | [TmConst(_,_, CFloat(sigma));
        TmConst(_,_, CFloat(mu))] when dist = anormal ->
       TmConst(def_attr,fi,
               CFloat(mu +. Gsl.Randist.gaussian seed ~sigma:sigma))
     | _ -> failwith "Unknown distribution applied as argument to sample")
  | _ -> failwith "Incorrect distribution applied as argument to sample"

(* Temporary demonstration implementation of importance sampling with fixed
   sample size of 20 *)
let infer model =

  (* Remove continuation by applying idfun *)
  let model = !eval [] (TmApp(def_attr,NoInfo, model, idfun)) in

  (* Replicate model for #samples times with an initial log weight of 0.0 *)
  let s = replicate 20 (TmApp(def_attr,NoInfo, model, TmNop(def_attr)), 0.0) in

  (* Evaluate one sample to the end *)
  let rec sim (t, w) =
    let t = !eval [] t in
    match t with
    (* Sample *)
    | TmConst(_,_, CAtom(id, [dist; cont]))
      when id = asample ->
      sim (TmApp(def_attr,NoInfo, cont, sample dist), w)

    (* Weight *)
    | TmConst(_,_fi, CAtom(id, [TmConst(_,_, CFloat(wadj)); cont]))
      when id = aweight ->
      sim (TmApp(def_attr,NoInfo, cont, TmNop(def_attr)), w +. wadj)

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

(* This is the main hook for new constructs in the mcore.*)
let eval_atom fi id tms v =
  let args = v :: tms in
  let c = CAtom(id, args) in
  if arity c = 0 then
    match args with
    | [model]      when id = ainfer -> infer model
    | [dist; v]    when id = aprob  -> prob v dist
    | _ -> TmConst(def_attr,fi, c)
  else
    TmConst(def_attr,fi, c)

(* Used for unsupported CPS transformations *)
let fail_cps tm =
  failwith ("CPS-transformation of " ^
            Ustring.to_utf8 (pprint false tm) ^ " not supported")

(* Wrap constant functions in CPS forms *)
let cps_const t = match t with
  | TmConst(_,_, c) ->
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

(* CPS transformations
   Requirements:
   - All functions must take one extra parameter: a continuation function with
   exactly one parameter
   - A function never "returns" (i.e., it never returns something that is not a
   TmApp). Instead, it applies its continuation to its "return value". *)

(* Atomic cps transformation (basically everything except function
   application).  Atomic means that we can CPS transform the expression without
   supplying a continuation to the transformation.  *)
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


(* Complex cps transformation. Complex means that the term is a computation
   (i.e., not a value). A continuation must also be supplied as argument to the
   transformation. *)
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
let label tm =
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
    | TmLam(a,fi,x,t1) -> TmLam({a with label=next()},fi,x,
                                label_terms t1)
    | TmApp(a,fi,t1,t2) -> TmApp({a with label = next()},fi,
                                 label_terms t1,label_terms t2)
    | TmConst(a,fi,c) -> TmConst({a with label=next()},fi,c)
    | TmIfexp(a,fi,c,t1) -> TmIfexp({a with label=next()},fi,c,t1)
    | TmFix(a,fi) -> TmFix({a with label=next()},fi)
    | TmRec(a,fi,sm) -> TmRec({a with label=next()},fi,sm)
    | TmProj(a,fi,t1,x) -> TmProj({a with label=next()},fi,t1,x)
    | TmNop(a) -> TmNop({a with label=next()})

    | TmClos _ -> failwith "Closure before eval"
    | _ -> failwith "Not supported" in
  let tm = tm |> label_vars empty |> label_terms in
  tm, !label

let analyze tm _ = tm

let align tm _ = tm

(* Main function for the evaluation of a probabilistic program *)
let evalprog debruijn eval' builtin filename =
  eval := eval';
  if !utest then printf "%s: " filename;
  utest_fail_local := 0;
  let fs1 = open_in filename in
  let tablength = 8 in
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

      (* Label program in preparation for static analysis *)
      let tm,_nl = label tm in

      (print_endline "-- after labeling --";
       uprint_endline (pprintl tm);
       uprint_newline ());

      (* Perform static analysis, returning all dynamic labels *)
      let dyn = analyze
          (tm_of_builtin (builtin @ pre_cps_builtin @ post_cps_builtin))
          tm in

      (* By using the above analysis, transform all dynamic checkpoints. This
         information will be forwarded to the inference algorithm. *)
      let tm = align tm dyn in

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
         uprint_endline (pprint false res))

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
  if !utest && !utest_fail_local = 0 then printf " OK\n" else printf "\n"
