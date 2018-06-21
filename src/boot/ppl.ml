
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

(* eval ref *)
let empty_eval _ t = t
let eval = ref empty_eval

(* List of all CPS transformed atom functions. *)
let auniform = usid "uniform"
let ainfer = usid "infer"
let aprob = usid "prob"

let pre_cps_builtin = [
  ("uniform", CAtom(auniform, []));
  ("infer", CAtom(ainfer, []));
  ("prob", CAtom(aprob, []));
]

(* List of all non-CPS-transformed atom functions *)
let asample = usid "sample"
let aweight = usid "weight"

let post_cps_builtin = [
  ("sample", CAtom(asample, []));
  ("weight", CAtom(aweight, []));
]

(* Used for constants with unspecified arities *)
let fail_arity c =
  failwith ("Arity of " ^
            Ustring.to_utf8 (pprint_const c) ^ " not specified.")

(* Arity specification for atoms *)
let atom_arity c = match c with
  | CAtom(id,_) when id = auniform -> 1
  | CAtom(id,_) when id = ainfer -> 1
  | CAtom(id,_) when id = aprob -> 2
  | _ -> fail_arity c

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

(* Probability functions for built in distributions. *)
let prob value dist = match dist with
  | TmConst(_, CAtom(dist, [TmConst(_, CBool(v))])) when dist = auniform ->
    (match v, value with
     | true, TmConst(_, CFloat(v)) when v >= 0.0 || v <= 1.0 ->
       TmConst(NoInfo, CFloat(0.0))
     | false, TmConst(_, CBool(v)) when v == true || v == false ->
       TmConst(NoInfo, CFloat(-0.693147))
     | _ -> TmConst(NoInfo, CFloat(neg_infinity)))
  | _ -> failwith "Unknown distribution applied as argument to prob."

(* Sample functions for built in distributions. *)
let sample dist = match dist with
  | TmConst(_, CAtom(dist, [TmConst(_, CBool(v))])) when dist = auniform ->
    if v then TmConst(NoInfo, CFloat(Random.float 1.0))
    else TmConst(NoInfo, CBool(Random.float 1.0 > 0.5))
  | _ -> failwith "Unkown distribution applied as argument to sample."

(* Temporary demonstration implementation of importance sampling with fixed
   sample size of 10 *)
let infer model = match model with
  (* Deconstruct continuation and thunk *)
  | TmClos(_, _, TmLam(_,_,tm), env, _) ->

    (* TODO I think static structure analysis should be done globally before
       infer is actually called.
       (Alternative: Static structure analysis here? Maybe CPS transforms as
       well?) *)

    (* The environment which tm will be evaluated in. Note the idfun which
       cancels the continuation and the TmNop which unwraps the thunk. *)
    let env = TmNop::idfun::env in

    (* Replicate the model for #samples times with an initial log weight of 0.0
    *)
    let s = replicate 20 (tm, env, 0.0) in

    (* Evaluate one sample to the end *)
    let rec recur (tm, env, w) =

      (* Evaluate until sample, weight, or result. Continue if sample or
         weight, stop at result. *)
      let tm = !eval env tm in
      match tm with

      | TmConst(fi, CAtom(id, [dist; TmClos(_,_, tm, env, _)]))
        when id = asample ->
        recur (tm, (sample dist)::env, w)

      | TmConst(fi, CAtom(id, [TmConst(_, CFloat(wadj));
                               TmClos(_,_, tm, env, _)]))
        when id = aweight ->
        recur (tm, TmNop::env, w +. wadj)

      | _ -> tm, [], w in

    let res = List.map recur s in

    if enable_debug_infer then
      (printf "\n-- infer result -- \n";
       List.iter
         (fun (tm, _, w) ->
            print_string "Sample: ";
            uprint_string (pprint false tm);
            print_string ", Log weight: ";
            print_endline (string_of_float w))
         res);

    TmNop (* Here we should return an empirical distribution *)

  | _ -> failwith "Incorrect infer application."

let fail_eval_atom id tms v =
  let sid = ustring_of_sid id in
  let stms =
    List.map (fun t -> pprint false t) tms |> Ustring.concat (us"\n") in
  let sv = pprint false v in
  let msg = Ustring.to_utf8
      (us"Unsupported atom application:\n" ^.
       us"id = " ^. sid ^. us"\n" ^.
       us"tms =\n" ^. stms ^. us"\n" ^.
       us"v = " ^. sv ^. us"\n") in
  failwith msg

(* This is the main hook for new constructs in the mcore. TODO Check for
   correct structure of args. *)
let eval_atom fi id tms v =
  match id,tms,v with

  (* AUTOMATICALLY CPS TRANSFORMED *)
  (* Uniform distribution *)
  | id, [], v when id = auniform -> TmConst(fi, CAtom(auniform, [v]))

  (* Infer *)
  | id, [], model when id = ainfer -> infer model

  (* This function (prob) returns the probability or probability density of a
     value in a given distribution *)
  | id, [], v when id = aprob -> TmConst(fi, CAtom(id,[v]))
  | id, [v], dist when id = aprob -> prob v dist

  (* NOT AUTOMATICALLY CPS TRANSFORMED (CONTINUATION MUST BE HANDLED
     EXPLICITLY)  *)
  (* Sample *)
  | id, [], cont when id = asample -> TmConst(fi, CAtom(id,[cont]))
  | id, [cont], dist when id = asample -> TmConst(fi, CAtom(id, [dist; cont]))

  (* Weight *)
  | id, [], cont when id = aweight -> TmConst(fi, CAtom(id,[cont]))
  | id, [cont], w when id = aweight -> TmConst(fi, CAtom(id, [w; cont]))

  (* No match *)
  | _ -> fail_eval_atom id tms v


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
        (printf "\n-- pre cps -- \n";
         uprint_endline (pprint false tm));

      (* Function for converting consts in builtin to tms *)
      let tm_of_builtin b = List.map (fun (x, y) -> x, tm_of_const y) b in

      let builtin = builtin
                    (* Convert builtins from consts to tms *)
                    |> tm_of_builtin

                    (* Add PPL builtins that should be CPS transformed *)
                    |> (@) (tm_of_builtin pre_cps_builtin)

                    (* Transform builtins to CPS. Required since we need to wrap constant
                       functions in CPS forms *)
                    |> List.map (fun (x, y) -> (x, (cps_const y)))

                    (* Add PPL builtins that should not be CPS transformed *)
                    |> (@) (tm_of_builtin post_cps_builtin)

                    (* Debruijn transform builtins (since they have now been CPS
                       transformed) *)
                    |> List.map (fun (x, y) -> (x, debruijn [] y)) in

      if enable_debug_cps_builtin then
        (printf "\n-- cps builtin -- \n";
         (List.iter uprint_endline
            (List.map
               (fun (x, y) -> us x ^. us" = " ^. pprint false y) builtin)));

      (* Perform CPS transformation of main program *)
      let cps = cps idfun tm in

      if enable_debug_cps then
        (printf "\n-- post cps -- \n";
         uprint_endline (pprint false cps));

      (* Evaluate CPS form of main program *)
      let res =
        cps |> debruijn (builtin |> List.split |> fst |> List.map us)
        |> !eval (builtin |> List.split |> snd) in

      if enable_debug_cps then
        (printf "\n-- post cps eval -- \n";
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
