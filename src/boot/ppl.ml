
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

let enable_debug_cps = true
let enable_debug_cps_builtin = false
let enable_debug_infer = true

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
  (** Distributions **)
  (* Continuous *)
  ("normal", CAtom(anormal, []));
  ("uniform", CAtom(auniform, []));
  ("gamma", CAtom(agamma, []));
  ("exponential", CAtom(aexp, []));

  (* Discrete *)
  ("bernoulli", CAtom(abern, []));

  (** Other atoms **)
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
  (ustr, TmVar(NoInfo, ustr, i, false))

(* The identity function (with debruijn index) as a tm. *)
let idfun =
  let var, var' = genvar 0 in
  TmLam(NoInfo, var, var')

(* Gsl default seed *)
let seed = Gsl.Rng.make (Gsl.Rng.default ())
(* let sample = (fun () -> Gsl.Randist.gaussian seed 200.0) *)

(* Probability functions for built in distributions. *)
let prob value dist = match dist with
  | TmConst(fi, CAtom(dist, args)) ->
    (match value, args with
     | TmConst(_, CFloat(v)),
       [TmConst(_, CFloat(sigma));
        TmConst(_, CFloat(mu))] when dist = anormal ->
       TmConst(fi, CFloat(Gsl.Randist.gaussian_pdf (v -. mu) sigma))
     | _ -> failwith "Unknown distribution applied as argument to prob")
  | _ -> failwith "Incorrect distribution applied as argument to prob"

(* Sample functions for built in distributions. *)
let sample dist = match dist with
  | TmConst(fi, CAtom(dist, args)) ->
    (match args with
     | [TmConst(_, CFloat(sigma));
        TmConst(_, CFloat(mu))] when dist = anormal ->
       TmConst(fi, CFloat(mu +. Gsl.Randist.gaussian seed sigma))
     | _ -> failwith "Unknown distribution applied as argument to sample")
  | _ -> failwith "Incorrect distribution applied as argument to sample"

(* Temporary demonstration implementation of importance sampling with fixed
   sample size of 20 *)
let infer model =

  (* Remove continuation by applying idfun *)
  let model = !eval [] (TmApp(NoInfo, model, idfun)) in

  (* Replicate model for #samples times with an initial log weight of 0.0 *)
  let s = replicate 20 (TmApp(NoInfo, model, TmNop), 0.0) in

  (* Evaluate one sample to the end *)
  let rec sim (t, w) =
    let t = !eval [] t in
    match t with
    (* Sample *)
    | TmConst(fi, CAtom(id, [dist; cont]))
      when id = asample ->
      sim (TmApp(NoInfo, cont, sample dist), w)

    (* Weight *)
    | TmConst(fi, CAtom(id, [TmConst(_, CFloat(wadj)); cont]))
      when id = aweight ->
      sim (TmApp(NoInfo, cont, TmNop), w +. wadj)

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

  TmNop (* Here we should return an empirical distribution *)

(* This is the main hook for new constructs in the mcore.*)
let eval_atom fi id tms v =
  let args = v :: tms in
  let c = CAtom(id, args) in
  if arity c = 0 then
    match args with
    | [model]      when id = ainfer -> infer model
    | [dist; v]    when id = aprob  -> prob v dist
    | _ -> TmConst(fi, c)
  else
    TmConst(fi, c)

(* Used for unsupported CPS transformations *)
let fail_cps tm =
  failwith ("CPS-transformation of " ^
            Ustring.to_utf8 (pprint false tm) ^ " not supported")

(* Wrap constant functions in CPS forms *)
let cps_const t = match t with
  | TmConst(_, c) ->
    let vars = List.map genvar (replicate (arity c) noidx) in
    let inner = List.fold_left
        (fun acc (_, v') ->
           TmApp(NoInfo, acc, v'))
        t vars in
    List.fold_right
      (fun (v, _) acc ->
         let k, k' = genvar noidx in
         TmLam(NoInfo, k, TmLam(NoInfo, v, TmApp(NoInfo, k', acc))))
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

  | TmLam(fi,x,t1) ->
    let k, k' = genvar noidx in
    TmLam(NoInfo, k, TmLam(fi, x, cps k' t1))

  (* Should not exist before eval *)
  | TmClos _-> fail_cps t

  (* Function application is not atomic. *)
  | TmApp _ -> failwith "TmApp is not atomic."

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
    let bapp = TmApp(NoInfo, b', c3') in
    let capp = TmApp(NoInfo, c', c3') in
    let inner =
      TmApp(NoInfo, TmApp(NoInfo, TmApp(NoInfo, t, a'), bapp), capp) in
    let clam =
      TmLam(NoInfo, c3, TmLam(NoInfo, c, inner)) in
    let blam =
      TmLam(NoInfo, c2, TmLam(NoInfo, b, TmApp(NoInfo, c2', clam))) in
    let alam =
      TmLam(NoInfo, c1, TmLam(NoInfo, a, TmApp(NoInfo, c1', blam))) in
    alam

  (* Treat similar as constant function with a single argument. We need to
     apply the id function to the argument before applying fix, since the
     argument expects a continuation as first argument. TODO Correct? *)
  | TmFix _ ->
    let v, v' = genvar noidx in
    let k, k' = genvar noidx in
    let inner = TmApp(NoInfo, t, TmApp(NoInfo, v', idfun)) in
    TmLam(NoInfo, k, TmLam(NoInfo, v, TmApp(NoInfo, k', inner)))

  (* Treat as constant *)
  | TmChar _ -> t

  (* Not supported *)
  | TmExprSeq _ -> fail_cps t

  (* Treat as constant *)
  | TmUC _ -> t

  (* CPS transform both lhs and rhs and apply identity function on result.
     Also transform tnext. *)
  | TmUtest(fi, t1, t2, tnext) ->
    TmUtest(fi, cps idfun t1, cps idfun t2, cps idfun tnext)

  (* Not supported *)
  | TmMatch _ -> fail_cps t

  (* Treat as constant *)
  | TmNop -> t


(* Complex cps transformation. Complex means that in order to do the
   transformation, a continuation must also be supplied as argument to the
   transformation. *)
and cps cont t = match t with

  (* Function application is the only complex expression.
     Optimize the case when either the function or argument is atomic. *)
  | TmApp(fi,t1,t2) ->
    let wrapopt (a, a') = Some a, a' in
    let f, f' = match t1 with
      | TmApp _ -> wrapopt (genvar noidx)
      | _ -> None, cps_atomic t1 in
    let e, e' = match t2 with
      | TmApp _ -> wrapopt (genvar noidx)
      | _ -> None, cps_atomic t2 in
    let app = TmApp(fi, TmApp(NoInfo, f', cont), e') in
    let inner = match e with
      | None -> app
      | Some(e) -> cps (TmLam(NoInfo, e, app)) t2 in
    let outer = match f with
      | None -> inner
      | Some(f) -> cps (TmLam(NoInfo, f, inner)) t1 in
    outer

  (* Everything else is atomic *)
  | _ -> TmApp(NoInfo, cont, cps_atomic t)


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
