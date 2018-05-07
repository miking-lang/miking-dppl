
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
let enable_debug_infer = true

(* eval ref *)
let empty_eval _ t = t
let eval = ref empty_eval

(* List of all atom constructs. See ppllexer.mll *)
let asample = usid Ppllexer.atom_sample
let aweight = usid Ppllexer.atom_weight
let ainfer = usid Ppllexer.atom_infer
let auniform = usid Ppllexer.atom_uniform

(* Rudimentary importance sampling with fixed sample size of 100 *)
let infer model cont =
  let var = TmVar(NoInfo, us"", 0, false) in
  let idfun = TmLam(NoInfo, us"", var) in
  match model with

  | TmClos(_, _, TmLam(_,_,tm), env, _) ->

    let s = replicate 10 (tm, TmNop::idfun::env, 0.0) in

    let run s = List.map
        (fun (tm, env, w) -> !eval env tm, w) s in

    let rec recur s =
      let t = run s in
      let d = ref false in
      let s' = List.map
          (fun (tm, w) ->
             match tm with
             | TmConst(fi, CAtom(id, [TmConst(_, CAtom(dist,_));
                                      TmClos(_,_, tm, env, _)]))
               when id = asample && dist = auniform ->
               (tm, (TmConst(NoInfo, CFloat(Random.float 1.0))::env), w)

             | TmConst(fi, CAtom(id, [TmConst(_, CFloat(wadj));
                                      TmClos(_,_, tm, env, _)]))
               when id = aweight ->
               (tm, TmNop::env, w +. wadj)

             | _ -> d := true; (tm, [], w))
          t in
      if !d then
        s'
      else
        recur s' in

    let res = recur s in

    if enable_debug_infer then
      (printf "\n-- infer result -- \n";
       List.iter (fun (tm, _, w) ->
           print_string "Sample: ";
           uprint_string (pprint false tm);
           print_string ", Log weight: ";
           print_endline (string_of_float w))
         res);

    !eval [] (TmApp(NoInfo, cont, TmNop))

  | _ -> failwith "Incorrect infer application."

(* This is the main hook for new constructs in the mcore *)
let eval_atom fi id tms v =
  match id,tms,v with

  (* Sample *)
  | id, [], (TmClos(fi,_,_,_,_) as cont) when id = asample ->
    TmConst(fi, CAtom(id,[cont]))
  | id, [cont], (TmConst(fi, CAtom(vid,[])) as dist)
    when id = asample && vid = auniform ->
    TmConst(fi, CAtom(id, [dist; cont]))

  (* Weight *)
  | id, [], (TmClos(fi,_,_,_,_) as cont) when id = aweight ->
    TmConst(fi, CAtom(id,[cont]))
  | id, [cont], (TmConst(fi, CFloat _) as w) when id = aweight ->
    TmConst(fi, CAtom(id, [w; cont]))

  (* Infer *)
  | id, [], (TmClos(fi,_,_,_,_) as cont) when id = ainfer ->
    TmConst(fi, CAtom(id,[cont]))
  | id, [cont], (TmClos(fi,_,_,_,_) as model) when id = ainfer ->
    infer model cont

  (* No match *)
  | _,_,_ ->
    uprint_endline (ustring_of_sid id);
    uprint_endline (pprint false v);
    raise_error fi "Incorrect atom application."

(* Generate fresh variable names for CPS transformation.
   Avoids clashes by using $ as first char (not allowed in lexer).
   TODO Way around having to do this?  *)
let nextvar = ref 0
let genvar () =
  let res = !nextvar in
  let ustr = us "$" ^. ustring_of_int res in
  nextvar := res + 1;
  (ustr, TmVar(NoInfo, ustr, noidx, false))

let idfun =
  let var, var' = genvar () in
  TmLam(NoInfo, var, var')

(* Used for unsupported CPS transformations *)
let failcps tm =
  failwith ("CPS-transformation of " ^
            Ustring.to_utf8 (pprint false tm) ^ " not supported")

(* Wrap constant functions in CPS forms *)
let cps_const c arity =
  let vars = List.map genvar (replicate arity ()) in
  let inner = List.fold_left
      (fun acc (_, v') ->
         TmApp(NoInfo, acc, v'))
      c vars in
  List.fold_right
    (fun (v, _) acc ->
       let k, k' = genvar () in
       TmLam(NoInfo, k, TmLam(NoInfo, v, TmApp(NoInfo, k', acc))))
    vars inner

(* CPS transformation
   Requirements:
   - All functions must take one extra parameter: a continuation function with
   exactly one parameter
   - A function never "returns" (i.e., it never returns something that is not a
   TmApp). Instead, it applies its continuation to its "return value". *)
let cps cont t =
  let rec recur cont t = match t with

    (* For vars, simply apply continuation. All vars are either builtin and
       handled by cps_const, or bound by outer lambdas and handled by some
       other rules below. *)
    | TmVar _ -> TmApp(NoInfo, cont, t)

    (* Transform lambda to CPS, then apply continuation to the new lambda. *)
    | TmLam(fi,x,t1) ->
      let k, k' = genvar () in
      let res = TmLam(NoInfo, k, TmLam(fi, x, recur k' t1)) in
      TmApp(NoInfo, cont, res)

    (* Should not exist before eval *)
    | TmClos _-> failcps t

    (* Function application.
       TODO Optimization possible when t1 and t2 are not TmApps *)
    | TmApp(fi,t1,t2) ->
      let f, f' = genvar () in
      let e, e' = genvar () in
      let app = TmApp(fi, TmApp(NoInfo, f', cont), e') in
      let inner = TmLam(NoInfo, e, app) in
      let outer = TmLam(NoInfo, f, recur inner t2) in
      recur outer t1

    (* Only true constants (not constant functions) and CAtoms should exist
       before eval.  *)
    | TmConst(_,c) -> assert (arity c = 0); TmApp(NoInfo, cont, t)

    (* Not supported TODO? *)
    | TmPEval _ -> failcps t

    (* Treat as a constant function with 3 arguments. Since branches are
       wrapped in thunks and therefore also CPS transformed as other lambda
       expressions, apply the branches to the identity function to get rid of
       the outermost continuation and recover the thunk (similar to below for
       the fixpoint operator). *)
    | TmIfexp _ ->
      let vars = List.map genvar (replicate 3 ()) in
      let _, hd' = List.hd vars in
      let c = TmApp(NoInfo, t, hd') in
      let inner = List.fold_left
          (fun acc (_, v') ->
             TmApp(NoInfo, acc, TmApp(NoInfo, v', idfun)))
          c (List.tl vars) in
      let res =
        List.fold_right
          (fun (v, _) acc ->
             let k, k' = genvar () in
             TmLam(NoInfo, k, TmLam(NoInfo, v, TmApp(NoInfo, k', acc))))
          vars inner
      in TmApp(NoInfo, cont, res)

    (* Treat similar as constant function with a single argument. We need to
       apply the id function to the argument before applying fix (similar to
       above for ifexp), since the argument expects a continuation as first
       argument. *)
    | TmFix _ ->
      let v, v' = genvar () in
      let k, k' = genvar () in
      let inner =
        TmApp(NoInfo, t, TmApp(NoInfo, v', idfun)) in
      let res =
        TmLam(NoInfo, k, TmLam(NoInfo, v, TmApp(NoInfo, k', inner))) in
      TmApp(NoInfo, cont, res)

    (* Treat as constant *)
    | TmChar _ -> TmApp(NoInfo, cont, t)

    (* Not supported TODO? *)
    | TmExprSeq _ -> failcps t

    (* Treat as constant *)
    | TmUC _ -> TmApp(NoInfo, cont, t)

    (* Handle specially since only top level *)
    | TmUtest(fi, t1, t2, tnext) ->
      TmUtest(fi, recur idfun t1, recur idfun t2, recur idfun tnext)

    (* Not supported TODO? *)
    | TmMatch _ -> failcps t

    (* Treat as constant *)
    | TmNop -> TmApp(NoInfo, cont, t)

  in recur cont t

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

      (* Transform builtins to CPS. Required since we need to wrap constant
         functions in CPS forms *)
      let builtin = List.map
          (fun (x, y) ->
             let c = TmConst(NoInfo, y) in
             (x, debruijn [] (cps_const c (arity y))))
          builtin in

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
