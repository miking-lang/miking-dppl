
(*
   Miking is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE.txt

   boot.ml is the main entry point for first stage of the
   bootstrapped Miking compiler. The bootstapper is interpreted and
   implemented in OCaml. Note that the Miking bootstrapper
   only implements a subset of the Ragnar language.
*)


open Ustring.Op
open Printf
open Ast
open Msg
open Pprint


let prog_argv = ref []          (* Argv for the program that is executed *)

(* Debug options *)
let enable_debug_normalize = false
let enable_debug_normalize_env = false
let enable_debug_readback = false
let enable_debug_readback_env = false
let enable_debug_eval = false
let enable_debug_eval_env = false
let enable_debug_after_peval = false

(* Evaluation of atoms. This is changed depending on the DSL *)
let empty_eval_atom _fi _id _tms _v = failwith "Atom evaluation must be defined"
let eval_atom = ref empty_eval_atom


(* Traditional map function on unified collection (UC) types *)
let rec ucmap f uc = match uc with
  | UCLeaf(tms) -> UCLeaf(List.map f tms)
  | UCNode(uc1,uc2) -> UCNode(ucmap f uc1, ucmap f uc2)


(* Print out error message when a unit test fails *)
let unittest_failed fi t1 t2=
  uprint_endline
    (match fi with
    | Info(_filename,l1,_,_,_) -> us"\n ** Unit test FAILED on line " ^.
        us(string_of_int l1) ^. us" **\n    LHS: " ^. (pprint false t1) ^.
        us"\n    RHS: " ^. (pprint false t2)
    | NoInfo -> us"Unit test FAILED ")

(* Add pattern variables to environment. Used in the debruijn function *)
let rec patvars env pat =
  match pat with
  | PatIdent(_,x) -> x::env
  | PatChar(_,_) -> env
  | PatUC(fi,p::ps,o,u) -> patvars (patvars env p) (PatUC(fi,ps,o,u))
  | PatUC(_fi,[],_o,_u) -> env
  | PatBool(_,_) -> env
  | PatInt(_,_) -> env
  | PatConcat(_,p1,p2) -> patvars (patvars env p1) p2


(* Convert a term into de Bruijn indices *)
let rec debruijn env t =
  match t with
  | TmVar(_,fi,x,_,_) ->
    let rec find env n = match env with
      | y::ee -> if y =. x then n else find ee (n+1)
      | [] -> raise_error fi ("Unknown variable '" ^ Ustring.to_utf8 x ^ "'")
    in TmVar(def_attr,fi,x,find env 0,false)
  | TmLam(_,fi,x,t1) -> TmLam(def_attr,fi,x,debruijn (x::env) t1)
  | TmClos(_,_fi,_x,_t1,_env1,_) -> failwith "Closures should not be available."
  | TmApp(_,fi,t1,t2) -> TmApp(def_attr,fi,debruijn env t1,debruijn env t2)
  | TmConst(_,_,_) -> t
  | TmFix(_,_) -> t
  | TmPEval(_,_) -> t
  | TmIfexp(_,_,_,_) -> t
  | TmChar(_,_,_) -> t
  | TmExprSeq(_,fi,t1,t2) -> TmExprSeq(def_attr,fi,debruijn env t1,debruijn env t2)
  | TmUC(_,fi,uct,o,u) -> TmUC(def_attr,fi, UCLeaf(List.map (debruijn env) (uct2list uct)),o,u)
  | TmUtest(_,fi,t1,t2,tnext)
      -> TmUtest(def_attr,fi,debruijn env t1,debruijn env t2,debruijn env tnext)
  | TmMatch(_,fi,t1,cases) ->
      TmMatch(def_attr,fi,debruijn env t1,
               List.map (fun (Case(fi,pat,tm)) ->
                 Case(fi,pat,debruijn (patvars env pat) tm)) cases)
  | TmNop _ -> t
  | TmRec(_,fi,sm) -> TmRec(def_attr,fi,StrMap.map (debruijn env) sm)
  | TmProj(_,fi,t1,x) -> TmProj(def_attr,fi,debruijn env t1, x)


(* Check if two value terms are equal *)
let rec val_equal v1 v2 =
  match v1,v2 with
  | TmChar(_,_,n1),TmChar(_,_,n2) -> n1 = n2
  | TmConst(_,_,c1),TmConst(_,_,c2) -> c1 = c2
  | TmUC(_,_,t1,o1,u1),TmUC(_,_,t2,o2,u2) ->
      let rec eql lst1 lst2 = match lst1,lst2 with
        | l1::ls1,l2::ls2 when val_equal l1 l2 -> eql ls1 ls2
        | [],[] -> true
        | _ -> false
      in o1 = o2 && u1 = u2 && eql (uct2revlist t1) (uct2revlist t2)
  | TmNop _,TmNop _ -> true
  | _ -> false

let ustring2uctstring s =
  let ls = List.map (fun i -> TmChar(def_attr,NoInfo,i)) (ustring2list s) in
  TmUC(def_attr,NoInfo,UCLeaf(ls),UCOrdered,UCMultivalued)


(* Update all UC to have the form of lists *)
let rec make_tm_for_match tm =
  let rec mklist uc acc =
    match uc with
    | UCNode(uc1,uc2) -> (mklist uc2 (mklist uc1 acc))
    | UCLeaf(lst) -> (List.map make_tm_for_match lst)::acc
  in
  let rec mkuclist lst acc =
    match lst with
    | x::xs -> mkuclist xs (UCNode(UCLeaf(x),acc))
    | [] -> acc
  in
  match tm with
  | TmUC(_,fi,uc,o,u) ->
    TmUC(def_attr,fi,mkuclist (mklist uc []) (UCLeaf([])),o,u)
  | _ -> tm

(* Check if a UC struct has zero length *)
let rec uctzero uct =
  match uct with
  | UCNode(n1,n2) -> (uctzero n1) && (uctzero n2)
  | UCLeaf([]) -> true
  | UCLeaf(_) -> false


(* Matches a pattern against a value and returns a new environment
   Notes:
    - final is used to detect if a sequence be checked to be complete or not *)
let rec eval_match env pat t final =
    match pat,t with
  | PatIdent(_,_x1),v -> Some(v::env,TmNop(def_attr))
  | PatChar(_,c1),TmChar(_,_,c2) -> if c1 = c2 then Some(env,TmNop(def_attr)) else None
  | PatChar(_,_),_ -> None
  | PatUC(fi1,p::ps,o1,u1),TmUC(_,fi2,UCLeaf(t::ts),o2,u2) ->
    (match eval_match env p t true with
    | Some(env,_) ->
      eval_match env (PatUC(fi1,ps,o1,u1)) (TmUC(def_attr,fi2,UCLeaf(ts),o2,u2)) final
    | None -> None)
  | PatUC(_fi1,_p::_ps,_o1,_u1),TmUC(_,_fi2,UCLeaf([]),_o2,_u2) -> None
  | PatUC(fi1,p::ps,o1,u1),TmUC(_,fi2,UCNode(UCLeaf(t::ts),t2),o2,u2) ->
    (match eval_match env p t true with
    | Some(env,_) ->
      eval_match env (PatUC(fi1,ps,o1,u1))
        (TmUC(def_attr,fi2,UCNode(UCLeaf(ts),t2),o2,u2)) final
    | None -> None)
  | PatUC(_fi1,_p::_ps,_o1,_u1),TmUC(_,fi2,UCNode(UCLeaf([]),t2),o2,u2) ->
      eval_match env pat (TmUC(def_attr,fi2,t2,o2,u2)) final
  | PatUC(_fi1,[],_o1,_u1),TmUC(_,_fi2,uct,_,_) when uctzero uct && final -> Some(env,TmNop(def_attr))
  | PatUC(_fi1,[],_o1,_u1),t when not final-> Some(env,t)
  | PatUC(_fi1,_lst,_o1,_u2),_t -> None
  | PatBool(_,b1),TmConst(_,_,CBool(b2)) -> if b1 = b2 then Some(env,TmNop(def_attr)) else None
  | PatBool(_,_),_ -> None
  | PatInt(_fi,i1),TmConst(_,_,CInt(i2)) -> if i1 = i2 then Some(env,TmNop(def_attr)) else None
  | PatInt(_,_),_ -> None
  | PatConcat(_,PatIdent(_,_x),_p2),_ ->
      failwith "Pattern variable first is not part of Ragnar--"
  | PatConcat(_,p1,p2),t1 ->
    (match eval_match env p1 t1 false with
    | Some(env,t2) -> eval_match env p2 t2 (final && true)
    | None -> None)

let fail_constapp fi = raise_error fi "Incorrect application "

(* Debug function used in the PE readback function *)
let debug_readback env n t =
  if enable_debug_readback then
    (printf "\n-- readback --   n=%d  \n" n;
     uprint_endline (pprint true t);
     if enable_debug_readback_env then
        uprint_endline (pprint_env env))
  else ()

(* Debug function used in the PE normalize function *)
let debug_normalize env n t =
  if enable_debug_normalize then
    (printf "\n-- normalize --   n=%d" n;
     uprint_endline (pprint true t);
     if enable_debug_normalize_env then
        uprint_endline (pprint_env env))
  else ()

(* Debug function used in the eval function *)
let debug_eval env t =
  if enable_debug_eval then
    (printf "\n-- eval -- \n";
     uprint_endline (pprint true t);
     if enable_debug_eval_env then
        uprint_endline (pprint_env env))
  else ()

(* Debug function used after partial evaluation *)
let debug_after_peval t =
  if enable_debug_after_peval then
    (printf "\n-- after peval --  \n";
     uprint_endline (pprint true t);
     t)
  else t


(* Mapping between named builtin functions (intrinsics) and the
   correspond constants *)
let builtin =
  [("not",Cnot);("and",Cand(None));("or",Cor(None));
   ("addi",Caddi(None));("subi",Csubi(None));("muli",Cmuli(None));
   ("divi",Cdivi(None));("modi",Cmodi(None));("negi",Cnegi);
   ("lti",Clti(None));("leqi",Cleqi(None));("gti",Cgti(None));("geqi",Cgeqi(None));
   ("eqi",Ceqi(None));("neqi",Cneqi(None));
   ("slli",Cslli(None));("srli",Csrli(None));("srai",Csrai(None));
   ("addf",Caddf(None));("subf",Csubf(None));("mulf",Cmulf(None));
   ("divf",Cdivf(None));("negf",Cnegf);
   ("add",Cadd(TNone));("sub",Csub(TNone));("mul",Cmul(TNone));
   ("div",Cdiv(TNone));("neg",Cneg);
   ("lt",Clt(TNone)); ("leq",Cleq(TNone)); ("gt",Cgt(TNone)); ("geq",Cgeq(TNone));
   ("eq",Ceq(TNone)); ("neq",Cneq(TNone));
   ("dstr",CDStr);("dprint",CDPrint);("print",CPrint);("argv",CArgv);
   ("concat",CConcat(None))]



(* Evaluates a constant application. This is the standard delta function
   delta(c,v) with the exception that it returns an expression and not
   a value. This is why the returned value is evaluated in the eval() function.
   The reason for this is that if-expressions return expressions
   and not values. *)
let delta c v  =
    match c,v with
    (* MCore boolean intrinsics *)
    | CBool(_),t -> fail_constapp (tm_info t)

    | Cnot,TmConst(_,fi,CBool(v)) -> TmConst(def_attr,fi,CBool(not v))
    | Cnot,t -> fail_constapp (tm_info t)

    | Cand(None),TmConst(_,fi,CBool(v)) -> TmConst(def_attr,fi,Cand(Some(v)))
    | Cand(Some(v1)),TmConst(_,fi,CBool(v2)) -> TmConst(def_attr,fi,CBool(v1 && v2))
    | Cand(None),t | Cand(Some(_)),t  -> fail_constapp (tm_info t)

    | Cor(None),TmConst(_,fi,CBool(v)) -> TmConst(def_attr,fi,Cor(Some(v)))
    | Cor(Some(v1)),TmConst(_,fi,CBool(v2)) -> TmConst(def_attr,fi,CBool(v1 || v2))
    | Cor(None),t | Cor(Some(_)),t  -> fail_constapp (tm_info t)

    (* MCore integer intrinsics *)
    | CInt(_),t -> fail_constapp (tm_info t)

    | Caddi(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Caddi(Some(v)))
    | Caddi(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 + v2))
    | Caddi(None),t | Caddi(Some(_)),t  -> fail_constapp (tm_info t)

    | Csubi(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Csubi(Some(v)))
    | Csubi(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 - v2))
    | Csubi(None),t | Csubi(Some(_)),t  -> fail_constapp (tm_info t)

    | Cmuli(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cmuli(Some(v)))
    | Cmuli(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 * v2))
    | Cmuli(None),t | Cmuli(Some(_)),t  -> fail_constapp (tm_info t)

    | Cdivi(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cdivi(Some(v)))
    | Cdivi(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 / v2))
    | Cdivi(None),t | Cdivi(Some(_)),t  -> fail_constapp (tm_info t)

    | Cmodi(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cmodi(Some(v)))
    | Cmodi(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 mod v2))
    | Cmodi(None),t | Cmodi(Some(_)),t  -> fail_constapp (tm_info t)

    | Cnegi,TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,CInt((-1)*v))
    | Cnegi,t -> fail_constapp (tm_info t)

    | Clti(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Clti(Some(v)))
    | Clti(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 < v2))
    | Clti(None),t | Clti(Some(_)),t  -> fail_constapp (tm_info t)

    | Cleqi(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cleqi(Some(v)))
    | Cleqi(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 <= v2))
    | Cleqi(None),t | Cleqi(Some(_)),t  -> fail_constapp (tm_info t)

    | Cgti(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cgti(Some(v)))
    | Cgti(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 > v2))
    | Cgti(None),t | Cgti(Some(_)),t  -> fail_constapp (tm_info t)

    | Cgeqi(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cgeqi(Some(v)))
    | Cgeqi(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 >= v2))
    | Cgeqi(None),t | Cgeqi(Some(_)),t  -> fail_constapp (tm_info t)

    | Ceqi(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Ceqi(Some(v)))
    | Ceqi(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 = v2))
    | Ceqi(None),t | Ceqi(Some(_)),t  -> fail_constapp (tm_info t)

    | Cneqi(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cneqi(Some(v)))
    | Cneqi(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 <> v2))
    | Cneqi(None),t | Cneqi(Some(_)),t  -> fail_constapp (tm_info t)

    | Cslli(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cslli(Some(v)))
    | Cslli(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 lsl v2))
    | Cslli(None),t | Cslli(Some(_)),t  -> fail_constapp (tm_info t)

    | Csrli(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Csrli(Some(v)))
    | Csrli(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 lsr v2))
    | Csrli(None),t | Csrli(Some(_)),t  -> fail_constapp (tm_info t)

    | Csrai(None),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Csrai(Some(v)))
    | Csrai(Some(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 asr v2))
    | Csrai(None),t | Csrai(Some(_)),t  -> fail_constapp (tm_info t)

    (* MCore intrinsic: Floating-point number constant and operations *)
    | CFloat(_),t -> fail_constapp (tm_info t)

    | Caddf(None),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Caddf(Some(v)))
    | Caddf(Some(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat(v1 +. v2))
    | Caddf(None),t | Caddf(Some(_)),t  -> fail_constapp (tm_info t)

    | Csubf(None),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Csubf(Some(v)))
    | Csubf(Some(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat(v1 -. v2))
    | Csubf(None),t | Csubf(Some(_)),t  -> fail_constapp (tm_info t)

    | Cmulf(None),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Cmulf(Some(v)))
    | Cmulf(Some(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat(v1 *. v2))
    | Cmulf(None),t | Cmulf(Some(_)),t  -> fail_constapp (tm_info t)

    | Cdivf(None),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Cdivf(Some(v)))
    | Cdivf(Some(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat(v1 /. v2))
    | Cdivf(None),t | Cdivf(Some(_)),t  -> fail_constapp (tm_info t)

    | Cnegf,TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,CFloat((-1.0)*.v))
    | Cnegf,t -> fail_constapp (tm_info t)

    (* Mcore intrinsic: Polymorphic integer and floating-point numbers *)

    | Cadd(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cadd(TInt(v)))
    | Cadd(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Cadd(TFloat(v)))
    | Cadd(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 + v2))
    | Cadd(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat(v1 +. v2))
    | Cadd(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CFloat(v1 +. (float_of_int v2)))
    | Cadd(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat((float_of_int v1) +. v2))
    | Cadd(_),t -> fail_constapp (tm_info t)

    | Csub(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Csub(TInt(v)))
    | Csub(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Csub(TFloat(v)))
    | Csub(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 - v2))
    | Csub(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat(v1 -. v2))
    | Csub(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CFloat(v1 -. (float_of_int v2)))
    | Csub(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat((float_of_int v1) -. v2))
    | Csub(_),t -> fail_constapp (tm_info t)

    | Cmul(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cmul(TInt(v)))
    | Cmul(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Cmul(TFloat(v)))
    | Cmul(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 * v2))
    | Cmul(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat(v1 *. v2))
    | Cmul(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CFloat(v1 *. (float_of_int v2)))
    | Cmul(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat((float_of_int v1) *. v2))
    | Cmul(_),t -> fail_constapp (tm_info t)

    | Cdiv(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cdiv(TInt(v)))
    | Cdiv(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Cdiv(TFloat(v)))
    | Cdiv(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CInt(v1 / v2))
    | Cdiv(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat(v1 /. v2))
    | Cdiv(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CFloat(v1 /. (float_of_int v2)))
    | Cdiv(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CFloat((float_of_int v1) /. v2))
    | Cdiv(_),t -> fail_constapp (tm_info t)

    | Cneg,TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,CFloat((-1.0)*.v))
    | Cneg,TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,CInt((-1)*v))
    | Cneg,t -> fail_constapp (tm_info t)

    | Clt(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Clt(TInt(v)))
    | Clt(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Clt(TFloat(v)))
    | Clt(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 < v2))
    | Clt(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool(v1 < v2))
    | Clt(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 < (float_of_int v2)))
    | Clt(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool((float_of_int v1) < v2))
    | Clt _,t -> fail_constapp (tm_info t)

    | Cleq(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cleq(TInt(v)))
    | Cleq(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Cleq(TFloat(v)))
    | Cleq(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 <= v2))
    | Cleq(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool(v1 <= v2))
    | Cleq(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 <= (float_of_int v2)))
    | Cleq(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool((float_of_int v1) <= v2))
    | Cleq _,t -> fail_constapp (tm_info t)

    | Cgt(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cgt(TInt(v)))
    | Cgt(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Cgt(TFloat(v)))
    | Cgt(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 > v2))
    | Cgt(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool(v1 > v2))
    | Cgt(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 > (float_of_int v2)))
    | Cgt(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool((float_of_int v1) > v2))
    | Cgt _,t -> fail_constapp (tm_info t)

    | Cgeq(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cgeq(TInt(v)))
    | Cgeq(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Cgeq(TFloat(v)))
    | Cgeq(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 >= v2))
    | Cgeq(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool(v1 >= v2))
    | Cgeq(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 >= (float_of_int v2)))
    | Cgeq(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool((float_of_int v1) >= v2))
    | Cgeq _,t -> fail_constapp (tm_info t)

    | Ceq(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Ceq(TInt(v)))
    | Ceq(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Ceq(TFloat(v)))
    | Ceq(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 = v2))
    | Ceq(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool(v1 = v2))
    | Ceq(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 = (float_of_int v2)))
    | Ceq(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool((float_of_int v1) = v2))
    | Ceq _,t -> fail_constapp (tm_info t)

    | Cneq(TNone),TmConst(_,fi,CInt(v)) -> TmConst(def_attr,fi,Cneq(TInt(v)))
    | Cneq(TNone),TmConst(_,fi,CFloat(v)) -> TmConst(def_attr,fi,Cneq(TFloat(v)))
    | Cneq(TInt(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 <> v2))
    | Cneq(TFloat(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool(v1 <> v2))
    | Cneq(TFloat(v1)),TmConst(_,fi,CInt(v2)) -> TmConst(def_attr,fi,CBool(v1 <> (float_of_int v2)))
    | Cneq(TInt(v1)),TmConst(_,fi,CFloat(v2)) -> TmConst(def_attr,fi,CBool((float_of_int v1) <> v2))
    | Cneq _, t -> fail_constapp (tm_info t)

    (* MCore debug and stdio intrinsics *)
    | CDStr, t -> ustring2uctstring (pprint true t)
    | CDPrint, t -> uprint_endline (pprint true t);TmNop(def_attr)
    | CPrint, t ->
      (match t with
      | TmUC(_,_,uct,_,_) ->
        uct2list uct |> uc2ustring |> list2ustring |> Ustring.to_utf8
      |> printf "%s"; TmNop(def_attr)
      | _ -> raise_error (tm_info t) "Cannot print value with this type")
    | CArgv,_ ->
      let lst = List.map (fun x -> ustring2uctm NoInfo (us x)) (!prog_argv)
      in TmUC(def_attr,NoInfo,UCLeaf(lst),UCOrdered,UCMultivalued)
    | CConcat(None),t -> TmConst(def_attr,NoInfo,CConcat((Some t)))
    | CConcat(Some(TmUC(_,l,t1,o1,u1))),TmUC(_,_,t2,o2,u2)
      when o1 = o2 && u1 = u2 -> TmUC(def_attr,l,UCNode(t1,t2),o1,u1)
    | CConcat(Some(tm1)),TmUC(_,l,t2,o2,u2) -> TmUC(def_attr,l,UCNode(UCLeaf([tm1]),t2),o2,u2)
    | CConcat(Some(TmUC(_,l,t1,o1,u1))),tm2 -> TmUC(def_attr,l,UCNode(t1,UCLeaf([tm2])),o1,u1)
    | CConcat(Some(_)),t -> fail_constapp (tm_info t)

    (* Ragnar polymorphic functions, special case for Ragnar in the boot interpreter.
       These functions should be defined using well-defined ad-hoc polymorphism
       in the real Ragnar compiler. *)
    | CPolyEq(None),t -> TmConst(def_attr,NoInfo,CPolyEq((Some(t))))
    | CPolyEq(Some(TmConst(_,_,c1))),TmConst(_,_,c2) -> TmConst(def_attr,NoInfo,CBool(c1 = c2))
    | CPolyEq(Some(TmChar(_,_,v1))),TmChar(_,_,v2) -> TmConst(def_attr,NoInfo,CBool(v1 = v2))
    | CPolyEq(Some(TmUC(_,_,_,_,_) as v1)),(TmUC(_,_,_,_,_) as v2) -> TmConst(def_attr,NoInfo,CBool(val_equal v1 v2))
    | CPolyEq(Some(_)),t  -> fail_constapp (tm_info t)

    | CPolyNeq(None),t -> TmConst(def_attr,NoInfo,CPolyNeq(Some(t)))
    | CPolyNeq(Some(TmConst(_,_,c1))),TmConst(_,_,c2) -> TmConst(def_attr,NoInfo,CBool(c1 <> c2))
    | CPolyNeq(Some(TmChar(_,_,v1))),TmChar(_,_,v2) -> TmConst(def_attr,NoInfo,CBool(v1 <> v2))
    | CPolyNeq(Some(TmUC(_,_,_,_,_) as v1)),(TmUC(_,_,_,_,_) as v2) -> TmConst(def_attr,NoInfo,CBool(not (val_equal v1 v2)))
    | CPolyNeq(Some(_)),t  -> fail_constapp (tm_info t)

    (* Atom - an untyped label that can be used to implement
       domain specific constructs *)
    | CAtom(id,tms),t -> !eval_atom (tm_info t) id tms t



(* Optimize away constant applications (mul with 0 or 1, add with 0 etc.) *)
let optimize_const_app fi v1 v2 =
  match v1,v2 with
  (*|   0 * x  ==>  0   |*)
  | TmConst(_,_,Cmuli(Some(0))),_v2 -> TmConst(def_attr,fi,CInt(0))
  (*|   1 * x  ==>  x   |*)
  | TmConst(_,_,Cmuli(Some(1))),v2 -> v2
  (*|   0 + x  ==>  x   |*)
  | TmConst(_,_,Caddi(Some(0))),v2 -> v2
  (*|   0 * x  ==>  0   |*)
  | TmApp(_,_,TmConst(_,_,Cmuli(None)),TmConst(_,_,CInt(0))),_vv1 -> TmConst(def_attr,fi,CInt(0))
  (*|   1 * x  ==>  x   |*)
  | TmApp(_,_,TmConst(_,_,Cmuli(None)),TmConst(_,_,CInt(1))),vv1 -> vv1
  (*|   0 + x  ==>  x   |*)
  | TmApp(_,_,TmConst(_,_,Caddi(None)),TmConst(_,_,CInt(0))),vv1 -> vv1
  (*|   x * 0  ==>  0   |*)
  | TmApp(_,_,TmConst(_,_,Cmuli(None)),_vv1),TmConst(_,_,CInt(0)) -> TmConst(def_attr,fi,CInt(0))
  (*|   x * 1  ==>  x   |*)
  | TmApp(_,_,TmConst(_,_,Cmuli(None)),vv1),TmConst(_,_,CInt(1)) -> vv1
  (*|   x + 0  ==>  x   |*)
  | TmApp(_,_,TmConst(_,_,Caddi(None)),vv1),TmConst(_,_,CInt(0)) -> vv1
  (*|   x - 0  ==>  x   |*)
  | TmApp(_,_,TmConst(_,_,Csubi(None)),vv1),TmConst(_,_,CInt(0)) -> vv1
  (*|   x op y  ==>  res(x op y)   |*)
  | TmConst(_,_fi1,c1),(TmConst(_,_fi2,_c2) as tt)-> delta c1 tt
  (* No optimization *)
  | vv1,vv2 -> TmApp(def_attr,fi,vv1,vv2)


(* The readback function is the second pass of the partial evaluation.
   It removes symbols for the term. If this is the complete version,
   this is the final pass before JIT *)
let rec readback env n t =
  debug_readback env n t;
  match t with
  (* Variables using debruijn indices. Need to evaluate because fix point. *)
  | TmVar(_,_fi,_x,k,false) -> readback env n (List.nth env k)
  (* Variables as PE symbol. Convert symbol to de bruijn index. *)
  | TmVar(_,fi,x,k,true) -> TmVar(def_attr,fi,x,n-k,false)
  (* Lambda *)
  | TmLam(_,fi,x,t1) -> TmLam(def_attr,fi,x,readback (TmVar(def_attr,fi,x,n+1,true)::env) (n+1) t1)
  (* Normal closure *)
  | TmClos(_,_fi,_x,_t1,_env2,false) -> t
  (* PE closure *)
  | TmClos(_,fi,x,t1,env2,true) ->
      TmLam(def_attr,fi,x,readback (TmVar(def_attr,fi,x,n+1,true)::env2) (n+1) t1)
  (* Application *)
  | TmApp(_,fi,t1,t2) -> optimize_const_app fi (readback env n t1) (readback env n t2)
  (* Constant, fix, and PEval  *)
  | TmConst(_,_,_) | TmFix(_,_) | TmPEval(_,_) -> t
  (* If expression *)
  | TmIfexp(_,fi,x,Some(t3)) -> TmIfexp(def_attr,fi,x,Some(readback env n t3))
  | TmIfexp(_,fi,x,None) -> TmIfexp(def_attr,fi,x,None)
  (* Other old, to remove *)
  | TmChar(_,_,_) -> t
  | TmExprSeq(_,fi,t1,t2) ->
      TmExprSeq(def_attr,fi,readback env n t1, readback env n t2)
  | TmUC(_,_fi,_uct,_o,_u) -> t
  | TmUtest(_,fi,t1,t2,tnext) ->
      TmUtest(def_attr,fi,readback env n t1, readback env n t2,tnext)
  | TmMatch(_,fi,t1,cases) ->
      TmMatch(def_attr,fi,readback env n t1,cases)
  | TmNop _ -> t
  | TmRec _ -> failwith "Not supported"
  | TmProj _ -> failwith "Not supported"




(* The function normalization function that leaves symbols in the
   term. These symbols are then removed using the readback function.
   'env' is the environment, 'n' the lambda depth number, 'm'
   the number of lambdas that we can go under, and
   't' the term. *)
let rec normalize env n t =
  debug_normalize env n t;
  match t with
  (* Variables using debruijn indices. *)
  | TmVar(_,_fi,_x,n,false) -> normalize env n (List.nth env n)
  (* PEMode variable (symbol) *)
  | TmVar(_,_fi,_x,_n,true) -> t
  (* Lambda and closure conversions to PE closure *)
  | TmLam(_,fi,x,t1) -> TmClos(def_attr,fi,x,t1,env,true)
  (* Closures, both PE and non PE *)
  | TmClos(_,_fi,_x,_t2,_env2,_pemode) -> t
  (* Application: closures and delta  *)
  | TmApp(_,fi,t1,t2) ->
    (match normalize env n t1 with
    (* Closure application (PE on non PE) TODO: use affine lamba check *)
    | TmClos(_,_fi,_x,t3,env2,_) ->
         normalize ((normalize env n t2)::env2) n t3
    (* Constant application using the delta function *)
    | TmConst(_,fi1,c1) ->
        (match normalize env n t2 with
        | TmConst(_,_fi2,_c2) as tt-> delta c1 tt
        | nf -> TmApp(def_attr,fi,TmConst(def_attr,fi1,c1),nf))
    (* Partial evaluation *)
    | TmPEval(_,fi) ->
      (match normalize env n t2 with
      | TmClos(_,fi2,x,t2,env2,_pemode) ->
          let pesym = TmVar(def_attr,NoInfo,us"",n+1,true) in
          let t2' = (TmApp(def_attr,fi,TmPEval(def_attr,fi),t2)) in
          TmClos(def_attr,fi2,x,normalize (pesym::env2) (n+1) t2',env2,true)
      | v2 -> v2)
    (* If-expression *)
    | TmIfexp(_,fi2,x1,x2) ->
      (match x1,x2,normalize env n t2 with
      | None,None,TmConst(_,_fi3,CBool(b)) -> TmIfexp(def_attr,fi2,Some(b),None)
      | Some(b),Some(TmClos(_,_,_,t3,env3,_)),TmClos(_,_,_,t4,env4,_) ->
        if b then normalize (TmNop(def_attr)::env3) n t3 else normalize (TmNop(def_attr)::env4) n t4
      | Some(b),_,(TmClos(_,_,_,_t3,_,_) as v3) -> TmIfexp(def_attr,fi2,Some(b),Some(v3))
      | _,_,v2 -> TmApp(def_attr,fi,TmIfexp(def_attr,fi2,x1,x2),v2))
    (* Fix *)
    | TmFix(_,fi2) ->
       (match normalize env n t2 with
       | TmClos(_,fi,_x,t3,env2,_) as tt ->
           normalize ((TmApp(def_attr,fi,TmFix(def_attr,fi2),tt))::env2) n t3
       | v2 -> TmApp(def_attr,fi,TmFix(def_attr,fi2),v2))
    (* Stay in normalized form *)
    | v1 -> TmApp(def_attr,fi,v1,normalize env n t2))
  (* Constant, fix, and Peval  *)
  | TmConst(_,_,_) | TmFix(_,_) | TmPEval(_,_) -> t
  (* If expression *)
  | TmIfexp(_,_,_,_) -> t  (* TODO!!!!!! *)
  (* Other old, to remove *)
  | TmChar(_,_,_) -> t
  | TmExprSeq(_,fi,t1,t2) ->
      TmExprSeq(def_attr,fi,normalize env n t1, normalize env n t2)
  | TmUC(_,_fi,_uct,_o,_u) -> t
  | TmUtest(_,fi,t1,t2,tnext) ->
      TmUtest(def_attr,fi,normalize env n t1,normalize env n t2,tnext)
  | TmMatch(_,fi,t1,cases) ->
      TmMatch(def_attr,fi,normalize env n t1,cases)
  | TmNop _ -> t
  | TmRec _ -> failwith "Not supported"
  | TmProj _ -> failwith "Not supported"



(* Main evaluation loop of a term. Evaluates using big-step semantics *)
let rec eval env t =
  debug_eval env t;
  match t with
  (* Variables using debruijn indices. Need to evaluate because fix point. *)
  | TmVar(_,_fi,_x,n,_) -> eval env  (List.nth env n)
  (* Lambda and closure conversions *)
  | TmLam(_,fi,x,t1) -> TmClos(def_attr,fi,x,t1,env,false)
  | TmClos(_,_fi,_x,_t1,_env2,_) -> t
  (* Application *)
  | TmApp(_,fi,t1,t2) ->
      (match eval env t1 with
       (* Closure application *)
       | TmClos(_,_fi,_x,t3,env2,_) -> eval ((eval env t2)::env2) t3
       (* Constant application using the delta function *)
       | TmConst(_,_fi,c) -> delta c (eval env t2)
       (* Partial evaluation *)
       | TmPEval(_,fi2) -> normalize env 0 (TmApp(def_attr,fi,TmPEval(def_attr,fi2),t2))
           |> readback env 0 |> debug_after_peval |> eval env
       (* Fix *)
       | TmFix(_,_fi) ->
         (match eval env t2 with
         | TmClos(_,fi,_x,t3,env2,_) as tt -> eval ((TmApp(def_attr,fi,TmFix(def_attr,fi),tt))::env2) t3
         | _ -> failwith "Incorrect CFix")
       (* If-expression *)
       | TmIfexp(_,fi,x1,x2) ->
         (match x1,x2,eval env t2 with
         | None,None,TmConst(_,fi,CBool(b)) -> TmIfexp(def_attr,fi,Some(b),None)
         | Some(b),Some(TmClos(_,_,_,t3,env3,_)),TmClos(_,_,_,t4,env4,_) ->
              if b then eval (TmNop(def_attr)::env3) t3 else eval (TmNop(def_attr)::env4) t4
         | Some(b),_,(TmClos(_,_,_,_t3,_,_) as v3) -> TmIfexp(def_attr,fi,Some(b),Some(v3))
         | _ -> raise_error fi "Incorrect if-expression in the eval function.")
       | _ -> raise_error fi "Application to a non closure value.")
  (* Constant *)
  | TmConst(_,_,_) | TmFix(_,_) | TmPEval(_,_) -> t
  (* If expression *)
  | TmIfexp(_,_fi,_,_) -> t
  (* The rest *)
  | TmChar(_,_,_) -> t
  | TmExprSeq(_,_,t1,t2) -> let _ = eval env t1 in eval env t2
  | TmUC(_,fi,uct,o,u) -> TmUC(def_attr,fi,ucmap (eval env) uct,o,u)
  | TmUtest(_,fi,t1,t2,tnext) ->
    if !utest then begin
      let (v1,v2) = ((eval env t1),(eval env t2)) in
        if val_equal v1 v2 then
         (printf "."; utest_ok := !utest_ok + 1)
       else (
        unittest_failed fi v1 v2;
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1)
     end;
    eval env tnext
  | TmMatch(_,fi,t1,cases) -> (
     let v1 = make_tm_for_match (eval env t1) in
     let rec appcases cases =
       match cases with
       | Case(_,p,t)::cs ->
          (match eval_match env p v1 true with
         | Some(env,_) -> eval env t
         | None -> appcases cs)
       | [] -> raise_error fi  "Match error"
     in
      appcases cases)
  | TmNop _ -> t
  | TmRec _ -> t (* We don't actually evaluated inside records for now *)
  | TmProj(_,_,t1,x) ->
    match eval env t1 with
    | TmRec(_,_,sm) ->
      (match StrMap.find_opt x sm with
       | Some t1 -> t1
       | _ -> failwith "Key doesn't exist in record")
    | _ -> failwith "Record projection on non-record tm"


(* Main function for evaluation a function. Performs lexing, parsing
   and evaluation. Does not perform any type checking *)
let evalprog filename  =
  if !utest then printf "%s: " filename;
  utest_fail_local := 0;
  let fs1 = open_in filename in
  let tablength = 8 in
  begin try
    Lexer.init (us filename) tablength;
    fs1 |> Ustring.lexing_from_channel
        |> Parser.main Lexer.main
        |> debruijn (builtin |> List.split |> fst |> List.map us)
        |> eval (builtin |> List.split |> snd |> List.map (fun x -> TmConst(def_attr,NoInfo,x)))
        |> fun _ -> ()

    with
    | Lexer.Lex_error m ->
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
        printf "\n ** %s" (Ustring.to_utf8 (Msg.message2str (Lexer.parse_error_message())));
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1)
      else
        fprintf stderr "%s\n"
	(Ustring.to_utf8 (Msg.message2str (Lexer.parse_error_message())))
  end; close_in fs1;
  if !utest && !utest_fail_local = 0 then printf " OK\n" else printf "\n"


(* Define the file slash, to make it platform independent *)
let sl = if Sys.win32 then "\\" else "/"

(* Add a slash at the end "\\" or "/" if not already available *)
let add_slash s =
  if String.length s = 0 || (String.sub s (String.length s - 1) 1) <> sl
  then s ^ sl else s

(* Expand a list of files and folders into a list of file names *)
let files_of_folders lst = List.fold_left (fun a v ->
  if Sys.is_directory v then
    (Sys.readdir v
        |> Array.to_list
        |> List.filter (fun x -> not (String.length x >= 1 && String.get x 0 = '.'))
        |> List.map (fun x -> (add_slash v) ^ x)
        |> List.filter (fun x -> not (Sys.is_directory x))
    ) @ a
  else v::a
) [] lst




(* Print out main menu *)
let menu() =
  printf "Usage: boot [run|test] <files>\n";
  printf "\n"


(* Main function. Checks arguments and reads file names *)
let main =
  (* Check command  *)
  (match Array.to_list Sys.argv |> List.tl with

  (* Run tests on one or more files *)
  | "test"::lst | "t"::lst -> (
    utest := true;
    (* Select the lexer and parser, depending on the DSL*)
    let eprog name =
      if Ustring.ends_with (us".ppl") (us name) then
        (eval_atom := Ppl.eval_atom;
         atom_arity := Ppl.atom_arity;
         (Ppl.evalprog debruijn eval builtin) name)
      else evalprog name
    in
    (* Evaluate each of the programs in turn *)
    List.iter eprog (files_of_folders lst);

    (* Print out unit test results, if applicable *)
    if !utest_fail = 0 then
      printf "\nUnit testing SUCCESSFUL after executing %d tests.\n"
        (!utest_ok)
            else
      printf "\nERROR! %d successful tests and %d failed tests.\n"
        (!utest_ok) (!utest_fail))

  (* Run one program with program arguments *)
  | "run"::name::lst | name::lst -> (
    prog_argv := lst;
      if Ustring.ends_with (us".ppl") (us name) then
        (eval_atom := Ppl.eval_atom;
         atom_arity := Ppl.atom_arity;
         (Ppl.evalprog debruijn eval builtin) name)
      else evalprog name)


  (* Show the menu *)
  | _ -> menu())
