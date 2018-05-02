
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

(* List of all atom constructs. See ppllexer.mll *)
let asample = usid Ppllexer.atom_sample
let aweight = usid Ppllexer.atom_weight
let ainfer = usid Ppllexer.atom_infer

(* This is the main hook for new constructs in the mcore *)
let eval_atom fi id tms v =
  match id,tms,v with
(* TODO *)

  (* Sample *)
  | id, [], v when id = asample -> v

  (* Weight *)
  | id, [], v when id = aweight -> v

  (* Infer *)
  | id, [], v when id = ainfer -> v

  (* No match *)
  | _,_,_ -> raise_error fi "Incorrect atom application."

(* Generate fresh variable names for CPS transformation.
 * Avoids clashes by using $ as first char (not allowed in lexer).
 * TODO Not needed if possible to directly CPS transform with de Bruijn
 * indices?  *)
let nextvar = ref 0
let genvar () =
  let res = !nextvar in
  nextvar := res + 1;
  us "$" ^. ustring_of_int res

let idfun =
  let var = genvar () in
  let var' = TmVar(NoInfo, var, noidx, false) in
  TmLam(NoInfo, var, var')

(* Used for unsupported CPS transformations *)
let failcps tm =
  failwith ("CPS-transformation of " ^
  Ustring.to_utf8 (pprint false tm) ^ " not supported")

(* Transform constant functions to cps *)
let cps_const c arity =
  let vars = List.map genvar (replicate arity ()) in
  let inner = List.fold_left
      (fun acc v ->
         let v' = TmVar(NoInfo, v, noidx, false) in
         TmApp(NoInfo, acc, v'))
      c vars in
  List.fold_right
    (fun v acc ->
       let k = genvar () in
       let k' = TmVar(NoInfo, k, noidx, false) in
       TmLam(NoInfo, v, TmLam(NoInfo, k, TmApp(NoInfo, k', acc))))
    vars inner

(* CPS transformation
 * Requirements:
 * - All functions must take one extra parameter: a continuation function with
 * exactly one parameter
 * - A function never "returns" (i.e., it never returns something that is not a
 * TmApp). Instead, it applies its continuation to its "return value". *)
let cps cont t =
  let rec recur cont t = match t with
    (* For vars, simply apply continuation. All vars are either builtin and
       handled by cps_const, or bound by outer lambdas and handled by other
       rules. *)
    | TmVar _ -> TmApp(NoInfo, cont, t)

    (* Transform lambda to CPS, then apply continuation to the new lambda. *)
    | TmLam(fi,x,t1) ->
      let k = genvar () in
      let k' = TmVar(NoInfo, k, noidx, false) in
      TmApp(NoInfo, cont, TmLam(fi, x, TmLam(NoInfo, k, recur k' t1)))

    (* Should not exist before eval *)
    | TmClos _-> failcps t

    (* Function application.
       TODO Optimization possible when t1 and t2 are not TmApps *)
    | TmApp(fi,t1,t2) ->
      let f = genvar () in
      let e = genvar () in
      let f' = TmVar(NoInfo, f, noidx, false) in
      let e' = TmVar(NoInfo, e, noidx, false) in
      let inner = TmLam(NoInfo, e, TmApp(fi, TmApp(NoInfo, f', e'), cont)) in
      let outer = TmLam(NoInfo, f, recur inner t2) in
      recur outer t1

    (* Only true constants (not constant functions) should exist before eval.
    *)
    | TmConst(_,c) -> assert (arity c = 0); TmApp(NoInfo, cont, t)

    (* Not supported *)
    | TmPEval _ -> failcps t

    (* Treat as a constant function with 3 arguments. Since branches are
       wrapped in thunks and therefore also CPS transformed as other lambda
       expressions, apply the branches to the identity function to get rid of
       the outermost continuation and recover the thunk.
       TODO Cleanup *)
    | TmIfexp _ ->
      let vars = List.map genvar (replicate 3 ()) in
      let c = TmApp(NoInfo, t, TmVar(NoInfo, List.hd vars, noidx, false)) in
      let inner = List.fold_left
          (fun acc v ->
             let v' = TmVar(NoInfo, v, noidx, false) in
             (* Very messy because of CPS in combination with thunks ... *)
             let b =
               TmLam(NoInfo, us"_",
                     TmApp(NoInfo, TmApp(NoInfo, v', idfun), idfun)) in
             TmApp(NoInfo, acc, b))
          c (List.tl vars) in
      let res =
        List.fold_right
          (fun v acc ->
             let k = genvar () in
             let k' = TmVar(NoInfo, k, noidx, false) in
             TmLam(NoInfo, v, TmLam(NoInfo, k, TmApp(NoInfo, k', acc))))
          vars inner
      in TmApp(NoInfo, cont, res)

    (* Not supported TODO *)
    | TmFix _ -> TmApp(NoInfo, cont, cps_const t 1)


    (* Treat as constant *)
    | TmChar _ -> TmApp(NoInfo, cont, t)

    (* Not supported *)
    | TmExprSeq _ -> failcps t

    (* Treat as constant *)
    | TmUC _ -> TmApp(NoInfo, cont, t)

    (* Handle specially since only top level *)
    | TmUtest(fi, t1, t2, tnext) ->
      TmUtest(fi, recur idfun t1, recur idfun t2, recur idfun tnext)

    (* Not supported *)
    | TmMatch _ -> failcps t

    (* Treat as constant *)
    | TmNop -> TmApp(NoInfo, cont, t)

  in recur cont t

(* Main function for the evaluation of a probabilitic program *)
let evalprog debruijn eval builtin filename =
  if !utest then printf "%s: " filename;
  utest_fail_local := 0;
  let fs1 = open_in filename in
  let tablength = 8 in
  begin try
      let tm =
        Ppllexer.init (us filename) tablength;
        fs1 |> Ustring.lexing_from_channel
        |> Pplparser.main Ppllexer.main in

      (*uprint_endline (pprint false tm);
      print_endline "";*)

      let builtin = List.map
          (fun (x, y) ->
             let c = TmConst(NoInfo, y) in
             (x, debruijn [] (cps_const c (arity y))))
          builtin in

      (*List.iter uprint_endline
        (List.map (fun (x, y) -> us x ^. us" = " ^. pprint false y) builtin);
      print_endline "";*)

      let cps = cps idfun tm in

      (*uprint_endline (pprint false cps);
      print_endline "";*)

      (*let res =*)
      cps |> debruijn (builtin |> List.split |> fst |> List.map us)
      |> eval (builtin |> List.split |> snd); ()
      (*in uprint_endline (pprint false res)*)

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
