(** The semantics of pplcore *)

open Printf
open Ast
open Const
open Pattern
open Utils

(** Debug the evaluation *)
let debug_eval        = false

(** Debug the evaluation environment *)
let debug_eval_env    = false

(** Debug the CPS transformation *)
let debug_cps         = false

(** Debug the CPS transformation of the initial environment (builtin) *)
let debug_cps_builtin = false

(** Debug the inference procedure *)
let debug_infer       = false

(** Printout the normalization constant when using SMC inference *)
let debug_norm        = true

(** Set to true if unit testing is enabled *)
let utest = ref false

(** Counts the number of successful unit tests *)
let utest_ok = ref 0

(** Counts the number of failed unit tests *)
let utest_fail = ref 0

(** Counts local failed tests for one file *)
let utest_fail_local = ref 0

(** Inference types *)
type inference =
  | Importance of int
  | SMC        of int

(** Default inference is importance sampling with 10 particles *)
let inference = ref (Importance(10))

(** Reference used for genvar *)
let nextvar = ref 0

(** Generate fresh variable names for CPS transformation.  Avoids clashes by
    using $ as first char (not allowed in lexer for vars).  Takes a debruijn
    index as argument (for idfun). *)
let genvar i =
  let res = !nextvar in
  let str = "$" ^ string_of_int res in
  nextvar := res + 1;
  (str, TmVar(na,str,i))

(** The identity function (with proper debruijn index) as a tm. *)
let idfun =
  let var, var' = genvar 0 in
  TmLam(na,var,var')

(** Print out error message when a unit test fails *)
let unittest_failed pos t1 t2 =
  print_string ("\n ** Unit test FAILED at " ^
                string_of_position pos ^ " **\n    LHS: " ^
                (string_of_tm t1) ^
                "\n    RHS: " ^ (string_of_tm t2))

(** Extends an environment used in debruijn conversion with the identifiers
    found in the given pattern *)
let rec patenv env pat = match pat with
  | PatVar(s) -> s :: env

  | PatRec((_,p)::ps) -> patenv (patenv env p) (PatRec(ps))
  | PatRec([]) -> env

  | PatList(p::ps) -> patenv (patenv env p) (PatList(ps))
  | PatList([]) -> env

  | PatTup(p::ps) -> patenv (patenv env p) (PatTup(ps))
  | PatTup([]) -> env

  | PatCons(p1,p2) -> patenv (patenv env p1) p2

  | PatUnit | PatChar _ | PatString _ | PatInt _ | PatFloat _ -> env

(** Add debruijn indices to a term *)
let rec debruijn env t = match t with
  | TmVar(_,x,_) ->
    let rec find env n = match env with
      | y::ee -> if y = x then n else find ee (n+1)
      | [] -> failwith ("Unknown variable: " ^ x)
    in TmVar(na,x,find env 0)
  | TmLam(a,x,t1) -> TmLam(a,x,debruijn (x::env) t1)
  | TmClos _ -> failwith "Closures should not be available."
  | TmApp(a,t1,t2) -> TmApp(a,debruijn env t1,debruijn env t2)
  | TmConst _ -> t
  | TmIf _ -> t
  | TmFix _ -> t

  | TmUtest(a,t1,t2) -> TmUtest(a,debruijn env t1,debruijn env t2)

  | TmMatch(a,tm,cases) ->
    TmMatch(a,debruijn env tm,
            List.map (fun (p,tm) -> (p, debruijn (patenv env p) tm)) cases)

  | TmTup(a,arr) -> TmTup(a, Array.map (debruijn env) arr)
  | TmTupProj(a,t1,i) -> TmTupProj(a,debruijn env t1,i)

  | TmRec(a,sm) -> TmRec(a,List.map (fun (k,v) -> k,debruijn env v) sm)
  | TmRecProj(a,t1,x) -> TmRecProj(a,debruijn env t1,x)

  | TmList(a,ls) -> TmList(a,List.map (debruijn env) ls)

  | TmConcat(_,None) -> t
  | TmConcat _ -> failwith "Should not exist before eval"

  | TmInfer _  -> t
  | TmLogPdf(_,None) -> t
  | TmLogPdf _ -> failwith "Should not exist before eval"
  | TmSample(_,None,None) -> t
  | TmSample _ -> failwith "Should not exist before eval"
  | TmWeight(_,None,None) -> t
  | TmWeight _ -> failwith "Should not exist before eval"
  | TmDWeight(_,None,None) -> t
  | TmDWeight _ -> failwith "Should not exist before eval"

(** Check if two value terms are equal *)
let val_equal v1 v2 = match v1,v2 with
  | TmConst(_,c1),TmConst(_,c2) -> c1 = c2
  | _ -> false

(** TODO Add more information about what failed *)
let fail_constapp () = failwith "Incorrect application "

(** Debug function used in the eval function *)
let debug_eval env t =
  if debug_eval then
    (printf "\n-- eval -- \n";
     print_endline (string_of_tm t);
     if debug_eval_env then
       print_endline (string_of_env env))
  else ()

(* Evaluate constant applications *)
let eval_const c v  = match c,v with

  (* Unit constant *)
  | CUnit,_ -> fail_constapp()

  (* Boolean constant and operations *)
  | CBool _,_ -> fail_constapp()

  | CNot,CBool(v) -> CBool(not v)
  | CNot,_ -> fail_constapp()

  | CAnd(None),CBool(v) -> CAnd(Some(v))
  | CAnd(Some(v1)),CBool(v2) -> CBool(v1 && v2)
  | CAnd _,_ -> fail_constapp()

  | COr(None),CBool(v) -> COr(Some(v))
  | COr(Some(v1)),CBool(v2) -> CBool(v1 || v2)
  | COr _,_  -> fail_constapp()

  (* Character constant and operations *)
  | CChar _,_ -> fail_constapp()

  (* String constant and operations *)
  | CString _,_ -> fail_constapp()

  (* Integer constant and operations *)
  | CInt _,_ -> fail_constapp()

  | CMod(None),CInt(v) -> CMod(Some(v))
  | CMod(Some(v1)),CInt(v2) -> CInt(v1 mod v2)
  | CMod _,_ -> fail_constapp()

  | CSll(None),CInt(v) -> CSll(Some(v))
  | CSll(Some(v1)),CInt(v2) -> CInt(v1 lsl v2)
  | CSll _,_  -> fail_constapp()

  | CSrl(None),CInt(v) -> CSrl(Some(v))
  | CSrl(Some(v1)),CInt(v2) -> CInt(v1 lsr v2)
  | CSrl _,_  -> fail_constapp()

  | CSra(None),CInt(v) -> CSra(Some(v))
  | CSra(Some(v1)),CInt(v2) -> CInt(v1 asr v2)
  | CSra _,_  -> fail_constapp()

  (* Floating-point number constant and operations *)
  | CFloat(_),_ -> fail_constapp()

  | CLog,CFloat(v) -> CFloat(log v)
  | CLog,_ -> fail_constapp()

  (* Polymorphic integer/floating-point functions *)
  | CAdd(None),CInt(v) -> CAdd(Some(CInt(v)))
  | CAdd(None),CFloat(v) -> CAdd(Some(CFloat(v)))
  | CAdd(Some(CInt(v1))),CInt(v2) -> CInt(v1 + v2)
  | CAdd(Some(CFloat(v1))),CFloat(v2) -> CFloat(v1 +. v2)
  | CAdd(Some(CFloat(v1))),CInt(v2) -> CFloat(v1 +. (float_of_int v2))
  | CAdd(Some(CInt(v1))),CFloat(v2) -> CFloat((float_of_int v1) +. v2)
  | CAdd(_),_ -> fail_constapp()

  | CSub(None),CInt(v) -> CSub(Some(CInt(v)))
  | CSub(None),CFloat(v) -> CSub(Some(CFloat(v)))
  | CSub(Some(CInt(v1))),CInt(v2) -> CInt(v1 - v2)
  | CSub(Some(CFloat(v1))),CFloat(v2) -> CFloat(v1 -. v2)
  | CSub(Some(CFloat(v1))),CInt(v2) -> CFloat(v1 -. (float_of_int v2))
  | CSub(Some(CInt(v1))),CFloat(v2) -> CFloat((float_of_int v1) -. v2)
  | CSub(_),_ -> fail_constapp()

  | CMul(None),CInt(v) -> CMul(Some(CInt(v)))
  | CMul(None),CFloat(v) -> CMul(Some(CFloat(v)))
  | CMul(Some(CInt(v1))),CInt(v2) -> CInt(v1 * v2)
  | CMul(Some(CFloat(v1))),CFloat(v2) -> CFloat(v1 *. v2)
  | CMul(Some(CFloat(v1))),CInt(v2) -> CFloat(v1 *. (float_of_int v2))
  | CMul(Some(CInt(v1))),CFloat(v2) -> CFloat((float_of_int v1) *. v2)
  | CMul(_),_ -> fail_constapp()

  | CDiv(None),CInt(v) -> CDiv(Some(CInt(v)))
  | CDiv(None),CFloat(v) -> CDiv(Some(CFloat(v)))
  | CDiv(Some(CInt(v1))),CInt(v2) -> CInt(v1 / v2)
  | CDiv(Some(CFloat(v1))),CFloat(v2) -> CFloat(v1 /. v2)
  | CDiv(Some(CFloat(v1))),CInt(v2) -> CFloat(v1 /. (float_of_int v2))
  | CDiv(Some(CInt(v1))),CFloat(v2) -> CFloat((float_of_int v1) /. v2)
  | CDiv(_),_ -> fail_constapp()

  | CNeg,CFloat(v) -> CFloat((-1.0)*.v)
  | CNeg,CInt(v) -> CInt((-1)*v)
  | CNeg,_ -> fail_constapp()

  | CLt(None),CInt(v) -> CLt(Some(CInt(v)))
  | CLt(None),CFloat(v) -> CLt(Some(CFloat(v)))
  | CLt(Some(CInt(v1))),CInt(v2) -> CBool(v1 < v2)
  | CLt(Some(CFloat(v1))),CFloat(v2) -> CBool(v1 < v2)
  | CLt(Some(CFloat(v1))),CInt(v2) -> CBool(v1 < (float_of_int v2))
  | CLt(Some(CInt(v1))),CFloat(v2) -> CBool((float_of_int v1) < v2)
  | CLt _,_ -> fail_constapp()

  | CLeq(None),CInt(v) -> CLeq(Some(CInt(v)))
  | CLeq(None),CFloat(v) -> CLeq(Some(CFloat(v)))
  | CLeq(Some(CInt(v1))),CInt(v2) -> CBool(v1 <= v2)
  | CLeq(Some(CFloat(v1))),CFloat(v2) -> CBool(v1 <= v2)
  | CLeq(Some(CFloat(v1))),CInt(v2) -> CBool(v1 <= (float_of_int v2))
  | CLeq(Some(CInt(v1))),CFloat(v2) -> CBool((float_of_int v1) <= v2)
  | CLeq _,_ -> fail_constapp()

  | CGt(None),CInt(v) -> CGt(Some(CInt(v)))
  | CGt(None),CFloat(v) -> CGt(Some(CFloat(v)))
  | CGt(Some(CInt(v1))),CInt(v2) -> CBool(v1 > v2)
  | CGt(Some(CFloat(v1))),CFloat(v2) -> CBool(v1 > v2)
  | CGt(Some(CFloat(v1))),CInt(v2) -> CBool(v1 > (float_of_int v2))
  | CGt(Some(CInt(v1))),CFloat(v2) ->
    CBool((float_of_int v1) > v2)
  | CGt _,_ -> fail_constapp()

  | CGeq(None),CInt(v) -> CGeq(Some(CInt(v)))
  | CGeq(None),CFloat(v) -> CGeq(Some(CFloat(v)))
  | CGeq(Some(CInt(v1))),CInt(v2) -> CBool(v1 >= v2)
  | CGeq(Some(CFloat(v1))),CFloat(v2) ->
    CBool(v1 >= v2)
  | CGeq(Some(CFloat(v1))),CInt(v2) ->
    CBool(v1 >= (float_of_int v2))
  | CGeq(Some(CInt(v1))),CFloat(v2) ->
    CBool((float_of_int v1) >= v2)
  | CGeq _,_ -> fail_constapp()

  (* Polymorphic functions *)
  | CEq(None),c -> CEq(Some(c))
  | CEq(Some c1), c2 -> CBool(c1 = c2)

  | CNeq(None),c -> CNeq(Some(c))
  | CNeq(Some(c1)),c2 -> CBool(c1 <> c2)

  (* Probability distributions *)
  | CNormal(None,None),CFloat(f) -> CNormal(Some f,None)
  | CNormal(None,None),CInt(i) -> CNormal(Some (float_of_int i),None)
  | CNormal(Some f1,None),CFloat(f2) -> CNormal(Some f1, Some f2)
  | CNormal(Some f1,None),CInt(i2) -> CNormal(Some f1, Some (float_of_int i2))
  | CNormal _,_  -> fail_constapp()

  | CUniform(None,None),CFloat(f) -> CUniform(Some f,None)
  | CUniform(None,None),CInt(i) -> CUniform(Some (float_of_int i),None)
  | CUniform(Some f1,None),CFloat(f2) -> CUniform(Some f1, Some f2)
  | CUniform(Some f1,None),CInt(i2) -> CUniform(Some f1, Some (float_of_int i2))
  | CUniform _,_  -> fail_constapp()

  | CGamma(None,None),CFloat(f) -> CGamma(Some f,None)
  | CGamma(None,None),CInt(i) -> CGamma(Some (float_of_int i),None)
  | CGamma(Some f1,None),CFloat(f2) -> CGamma(Some f1, Some f2)
  | CGamma(Some f1,None),CInt(i2) -> CGamma(Some f1, Some (float_of_int i2))
  | CGamma _,_  -> fail_constapp()

  | CExp(None),CFloat(f) -> CExp(Some(f))
  | CExp(None),CInt(i) -> CExp(Some (float_of_int i))
  | CExp _,_  -> fail_constapp()

  | CBern(None),CFloat(f) -> CBern(Some(f))
  | CBern(None),CInt(i) -> CBern(Some (float_of_int i))
  | CBern _,_  -> fail_constapp()

(** If the pattern matches the given value, return the extended environment
    where the variables in the pattern are bound to the corresponding terms in
    the value. IMPORTANT: Follows the same traversal order of the pattern as in
    the patenv function to get the correct debruijn indices. *)
let rec match_case env pattern value = match pattern,value with
  | PatVar _,v ->  Some(v :: env)

  | PatRec((k,p)::ps),(TmRec(_,es) as v) ->
    (match List.assoc_opt k es with
     | Some v1 ->
       (match match_case env p v1 with
        | Some env -> match_case env (PatRec(ps)) v
        | None -> None)
     | None -> None)
  | PatRec([]),TmRec _ -> Some env
  | PatRec _,_ -> None

  | PatList(p::ps),TmList(a,v::vs) ->
    (match match_case env p v with
     | Some env -> match_case env (PatList(ps)) (TmList(a,vs))
     | None -> None)
  | PatList([]),TmList(_,[]) -> Some env
  | PatList _,_ -> None

  | PatTup(ps),TmTup(_,varr) ->
    let rec fold env ps i = match ps with
      | p::ps when i < Array.length varr ->
        (match match_case env p varr.(i) with
         | Some env -> fold env ps (i + 1)
         | None -> None)
      | [] when i = Array.length varr -> Some env
      | _ -> None
    in fold env ps 0
  | PatTup _,_ -> None

  | PatCons(p1,p2),TmList(a,v::vs) ->
    (match match_case env p1 v with
     | Some env -> match_case env p2 (TmList(a,vs))
     | None -> None)
  | PatCons _,_ -> None

  | PatUnit, TmConst(_,CUnit) -> Some env
  | PatUnit, _ -> None

  | PatChar(c1), TmConst(_,CChar(c2)) when c1 = c2 -> Some env
  | PatChar _,_ -> None

  | PatString(s1), TmConst(_,CString(s2)) when s1 = s2 -> Some env
  | PatString _,_ -> None

  | PatInt(i1), TmConst(_,CInt(i2)) when i1 = i2 -> Some env
  | PatInt _,_ -> None

  | PatFloat(f1), TmConst(_,CFloat(f2)) when f1 = f2 -> Some env
  | PatFloat _,_ -> None


(** Big-step evaluation of terms *)
let rec eval env t =
  debug_eval env t;
  match t with

  (* Variables using debruijn indices. Need to evaluate because fix point. *)
  | TmVar(_,_,n) -> eval env (List.nth env n)

  (* Lambda and closure conversions *)
  | TmLam(a,x,t1) -> TmClos(a,x,t1,env)
  | TmClos _ -> t

  (* Application *)
  | TmApp(_,t1,t2) -> (match eval env t1,eval env t2 with

     (* Closure application *)
     | TmClos(_,_,t3,env2),v2 -> eval (v2::env2) t3

     (* Constant application using the delta function *)
     | TmConst(_,c),TmConst(_,v) -> TmConst(na,eval_const c v)
     | TmConst _,_ -> failwith "Non constant applied to constant"

     (* Fix *)
     | TmFix _,(TmClos(_,_,t3,env2) as tt) ->
       eval ((TmApp(na,TmFix na,tt))::env2) t3
     | TmFix _,_ -> failwith "Incorrect fix application."

     (* If-expression *)
     | TmIf(_,x1,x2),v2 ->
       (match x1,x2,v2 with
        | None,None,TmConst(_,CBool(b)) -> TmIf(na,Some(b),None)
        | Some(b),None,(TmClos(_,_,_t3,_) as v3) -> TmIf(na,Some(b),Some(v3))
        | Some(b),Some(TmClos(_,_,t3,env3)),TmClos(_,_,t4,env4) ->
          if b then eval (nop::env3) t3 else eval (nop::env4) t4
        | _ -> failwith "Incorrect if-expression in the eval function.")

     | TmConcat(a,None),(TmConst(_,CString _) as v2)
     | TmConcat(a,None),(TmList _ as v2) -> TmConcat(a,Some v2)
     | TmConcat(_,Some TmConst(_,CString s1)),TmConst(_,CString s2) ->
       TmConst(na,CString (s1 ^ s2))
     | TmConcat(_,Some TmList(_,ls1)),TmList(_,ls2) -> TmList(na,ls1 @ ls2)
     | TmConcat _,_ -> failwith "Incorrect concatenation application"

     | TmInfer _,(TmClos _ as model) -> infer model
     | TmInfer _,_ -> failwith "Incorrect infer application"

     | TmLogPdf(a,None),v2 -> TmLogPdf(a,Some v2)
     | TmLogPdf(_,Some v1),v2 -> Dist.logpdf v1 v2

     | TmSample(a,None,None),(TmClos _ as cont) -> TmSample(a,Some cont,None)
     | TmSample(a,Some cont,None),v2 -> TmSample(a,Some cont,Some v2)
     | TmSample _,_ -> failwith "Incorrect sample application"

     | TmWeight(a,None,None),(TmClos _ as cont) -> TmWeight(a,Some cont,None)
     | TmWeight(a,Some cont,None),TmConst(_,(CFloat _ as w)) ->
       TmWeight(a,Some cont,Some w)
     | TmWeight _,_ -> failwith "Incorrect weight application"

     | TmDWeight(a,None,None),(TmClos _ as cont) -> TmDWeight(a,Some cont,None)
     | TmDWeight(a,Some cont,None),TmConst(_,(CFloat _ as w)) ->
       TmDWeight(a,Some cont,Some w)
     | TmDWeight _,_ -> failwith "Incorrect dweight application"

     | _ -> failwith "Application to a non closure value.")

  (* Constant *)
  | TmConst _ | TmFix _ -> t

  (* If expression *)
  | TmIf _ -> t

  (* Utest *)
  | TmUtest({pos;_},t1,t2) ->
    if !utest then begin
      let (v1,v2) = ((eval env t1),(eval env t2)) in
      if val_equal v1 v2 then
        (printf "."; utest_ok := !utest_ok + 1)
      else (
        unittest_failed pos v1 v2;
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1)
    end;
    nop

  | TmMatch(_,t1,cases) ->
    let v1 = eval env t1 in
    let rec match_cases cases = match cases with
      | (p,t) :: cases ->
        (match match_case env p v1 with
         | Some env -> eval env t
         | None -> match_cases cases)
      | [] -> failwith "Pattern matching failed TODO"

    in match_cases cases

  | TmTup(a,tarr) -> TmTup(a,Array.map (eval env) tarr)
  | TmTupProj(_,t1,i) ->
    (match eval env t1 with
     | TmTup(_,varr) -> varr.(i)
     | _ -> failwith "Tuple projection on non-tuple TODO")


  (* Records *)
  | TmRec(a,tls) -> TmRec(a,List.map (fun (k,tm) -> k,eval env tm) tls)

  (* Record projection *)
  | TmRecProj(_,t1,x) ->
    (match eval env t1 with
     | TmRec(_,vls) ->
       (match List.assoc_opt x vls with
        | Some v1 -> v1
        | _ -> failwith "Record projection where Key TODO not found in record")
     | t -> failwith (sprintf "Record projection on non-record %s"
                        (string_of_tm t)))

  | TmList(a,tls) -> TmList(a,List.map (eval env) tls)

  | TmConcat _ -> t

  | TmInfer _ -> t
  | TmLogPdf _ -> t
  | TmSample _ -> t
  | TmWeight _ -> t
  | TmDWeight _ -> t

(** Importance sampling (Likelihood weighting) inference *)
and infer_is model n =

  (* Remove continuation by applying idfun *)
  let model = eval [] (TmApp(na, model, idfun)) in

  (* Replicate model for #samples times with an initial log weight of 0.0 *)
  let s = replicate n (TmApp(na, model, nop), 0.0) in

  (* Evaluate one sample to the end *)
  let rec sim (t, w) =
    let t = eval [] t in
    match t with

    (* Sample *)
    | TmSample(_,Some(cont),Some(dist)) ->
      sim (TmApp(na, cont, Dist.sample dist), w)

    (* Weight (and DWeight)*)
    | TmWeight(_,Some(cont),Some(CFloat(wadj)))
    | TmDWeight(_,Some(cont),Some(CFloat(wadj))) ->
      if wadj = -. infinity then
        (nop,wadj)
      else
        sim (TmApp(na, cont, nop), w +. wadj)

    (* Result *)
    | _ -> t, w in

  let res = List.map sim s in

  if debug_infer then
    (print_endline "-- infer result --";
     List.iter
       (fun (t, w) ->
          print_string "Sample: ";
          print_endline (string_of_tm t);
          print_string ", Log weight: ";
          print_endline (string_of_float w))
       res;
     print_newline ());

  nop (* TODO Here we should return a proper distribution *)

(** SMC inference *)
and infer_smc model n =

  (* Remove continuation by applying idfun *)
  let model = eval [] (TmApp(na, model, idfun)) in

  (* Replicate model for #samples times with an initial log weight of 0.0 *)
  let s = replicate n (TmApp(na, model, nop), 0.0) in

  (* Evaluate a sample until encountering a weight *)
  let rec sim (t, w) =
    let t = eval [] t in
    match t with

    (* Sample *)
    | TmSample(_,Some(cont),Some(dist)) ->
      sim (TmApp(na,cont,Dist.sample dist), w)

    (* Dweight *)
    | TmDWeight(_,Some(cont),Some(CFloat(wadj))) ->
      if wadj = -. infinity then (true,nop,wadj) else
        sim (TmApp(na,cont,nop), w +. wadj)

    (* Weight *)
    | TmWeight(_,Some(cont),Some(CFloat(wadj))) ->
      if wadj = -. infinity then (true,nop,wadj) else
        (false,TmApp(na,cont,nop), w +. wadj)

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

    (*(List.iter (fun (_, w) -> let w = 50.0 -. ((w /. (float n)) *. 50.0) in*)
    (*print_endline ("w:" ^ (string_of_float w))) snorm);*)

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
      if debug_norm then
        print_endline (string_of_float normconst);

      if debug_infer then
        (List.iter (fun (t, _) -> print_endline (string_of_tm t)) res);

      nop (* Here we should return a proper distribution *)
    end else
      smc res normconst

  in smc s 0.0

(** Select correct inference algorithm *)
and infer model = match !inference with
  | Importance(i) -> infer_is model i
  | SMC(i) -> infer_smc model i


(* CPS transformations
    Requirements:
    - All functions must take one extra parameter: a continuation function with
    exactly one parameter
    - A function never "returns" (i.e., it never returns something that is not a
    TmApp). Instead, it applies its continuation to its "return value". *)

(** Used for unsupported CPS transformations *)
let fail_cps tm =
  failwith ("CPS-transformation of " ^ (string_of_tm tm) ^ " not supported")

(** Wrap opaque builtin functions in CPS forms *)
let cps_builtin t arity =
  let vars = List.map genvar (replicate arity noidx) in
  let inner = List.fold_left
      (fun acc (_, v') ->
         TmApp(na, acc, v'))
      t vars in
  List.fold_right
    (fun (v, _) acc ->
       let k, k' = genvar noidx in
       TmLam(na, k, TmLam(na, v, TmApp(na, k', acc))))
    vars inner

(** Wrap constant functions in CPS forms *)
let cps_const t = match t with
  | TmConst(_,c) -> cps_builtin t (arity c)
  | _ -> failwith "cps_const of non-constant"

(** Lift applications in a term *)
let rec lift_apps t =
  let lift t apps = match t with
    | TmApp _ | TmMatch _ when not (is_value t) ->
      let var,var' = genvar noidx in
      var',(var,t)::apps
    | _ -> t,apps in

  let wrap_app t apps =
    let lam = List.fold_left (fun t (var,_) -> TmLam(na,var,t)) t apps in
    List.fold_right (fun (_,app) t -> TmApp(na,t,app)) apps lam in

  match t with
  | TmVar _ -> t
  | TmLam(a,s,t) -> TmLam(a,s,lift_apps t)

  | TmClos _ -> failwith "Should not exist before eval"

  | TmApp(a,t1,t2) -> TmApp(a,lift_apps t1, lift_apps t2)

  | TmConst _ -> t

  | TmIf(_,None,None) -> t
  | TmIf _ -> failwith "Should not exist before eval"

  | TmFix _ -> t

  | TmUtest(a,t1,t2) ->
    let t1,apps = lift (lift_apps t1) [] in
    let t2,apps = lift (lift_apps t2) apps in
    wrap_app (TmUtest(a,t1,t2)) apps

  | TmMatch(a,t1,cases) ->
    let cases = List.map (fun (p,t) -> p,lift_apps t) cases in
    let t1,apps = lift (lift_apps t1) [] in
    wrap_app (TmMatch(a,t1,cases)) apps

  | TmRec(a,rels) ->
    let f (rels,apps) (p,t) =
      let t,apps = lift t apps in (p,t)::rels,apps in
    let rels,apps = List.fold_left f ([],[]) rels in
    wrap_app (TmRec(a,rels)) apps

  | TmRecProj(a,t1,s) ->
    let t1,apps = lift (lift_apps t1) [] in
    wrap_app (TmRecProj(a,t1,s)) apps

  | TmTup(a,tarr) ->
    let f (tls,apps) t =
      let t,apps = lift t apps in t::tls,apps in
    let tarr,apps = Array.fold_left f ([],[]) tarr in
    wrap_app (TmTup(a,Array.of_list tarr)) apps

  | TmTupProj(a,t1,i) ->
    let t1,apps = lift (lift_apps t1) [] in
    wrap_app (TmTupProj(a,t1,i)) apps

  | TmList(a,tls) ->
    let f (tls,apps) t =
      let t,apps = lift t apps in t::tls,apps in
    let tls,apps = List.fold_left f ([],[]) tls in
    wrap_app (TmList(a,tls)) apps

  | TmConcat(_,None) -> t
  | TmConcat _ -> failwith "Should not happen before eval"

  | TmInfer _ -> t
  | TmLogPdf(_,None) -> t
  | TmLogPdf _ -> failwith "Should not happen before eval"
  | TmSample(_,None,None) -> t
  | TmSample _ -> failwith "Should not happen before eval"
  | TmWeight(_,None,None) -> t
  | TmWeight _ -> failwith "Should not happen before eval"
  | TmDWeight(_,None,None) -> t
  | TmDWeight _ -> failwith "Should not happen before eval"

(** CPS transformation of values. Transforming values means that we can do the
    CPS transformation without supplying a continuation *)
let rec cps_value t = match t with

  (* Variables *)
  | TmVar _ -> t

  (* Lambdas *)
  | TmLam(a,x,t1) ->
    let k, k' = genvar noidx in
    TmLam(a, k, TmLam(na, x, cps_app k' t1))

  (* Should not exist before eval *)
  | TmClos _-> fail_cps t

  (* Function application is not a value. *)
  | TmApp _ -> failwith "TmApp is not a value"

  (* Pattern matching might not be a value *)
  | TmMatch(a,t1,pls) ->
    let pls = List.map (fun (p,te) -> p,cps_value te) pls in
    TmMatch(a,cps_value t1, pls)

  (* Tuples might not be values *)
  | TmTup(a,tarr) -> TmTup(a,Array.map cps_value tarr)

  (* Tuple projections might not be values *)
  | TmTupProj(a,t1,s) -> TmTupProj(a,cps_value t1,s)

  (* Records might not be values *)
  | TmRec(a,rels) ->
    let rels = List.map (fun (s,te) -> s,cps_value te) rels in
    TmRec(a,rels)

  (* Tuple projections might not be values *)
  | TmRecProj(a,t1,i) -> TmRecProj(a,cps_value t1,i)

  (* Constant transformation *)
  | TmConst _ -> cps_const t

  (* Transforms similarly to constant functions. The difference is that the
     last continuation must be supplied to the branches, and not applied to
     the result. *)
  | TmIf _ ->
    let a, a'   = genvar noidx in
    let b, b'   = genvar noidx in
    let c, c'   = genvar noidx in
    let c1, c1' = genvar noidx in
    let c2, c2' = genvar noidx in
    let c3, c3' = genvar noidx in
    let bapp    = TmApp(na, b', c3') in
    let capp    = TmApp(na, c', c3') in
    let inner   = TmApp(na, TmApp(na, TmApp(na, t, a'), bapp), capp) in
    let clam    = TmLam(na, c3, TmLam(na, c, inner)) in
    let blam    = TmLam(na, c2, TmLam(na, b, TmApp(na, c2', clam))) in
    let alam    = TmLam(na, c1, TmLam(na, a, TmApp(na, c1', blam)))
    in alam

  (* Treat similar as constant function with a single argument. We need to
     apply the id function to the argument before applying fix, since the
     argument expects a continuation as first argument. TODO Correct? *)
  | TmFix _ ->
    let v, v' = genvar noidx in
    let k, k' = genvar noidx in
    let inner = TmApp(na,
                      t, TmApp(na, v', idfun)) in
    TmLam(na,
          k, TmLam(na, v,
                   TmApp(na, k', inner)))

  (* Unit tests might not be values *)
  | TmUtest(a,t1,t2) -> TmUtest(a,cps_value t1,cps_value t2)

  (* Lists might not be values *)
  | TmList(a,tls) -> TmList(a, List.map cps_value tls)

  (* Concatenations *)
  | TmConcat(_,None)  -> cps_builtin t 2
  | TmConcat _  -> failwith "Should not exist before eval"

  (* Transform some builtin probabilistic constructs in the same way as
     constants.  It is required that the original arity of the function is
     passed to cps_builtin *)
  | TmInfer _ -> cps_builtin t 1
  | TmLogPdf(_,None) -> cps_builtin t 2
  | TmLogPdf _ -> failwith "Should not exist before eval"

  (* Already in CPS form (the whole reason why we are performing the CPS
     transformation in the first place...) *)
  | TmSample(_,None,None) -> t
  | TmSample _ -> failwith "Should not exist before eval"
  | TmWeight(_,None,None) -> t
  | TmWeight _ -> failwith "Should not exist before eval"
  | TmDWeight(_,None,None) -> t
  | TmDWeight _ -> failwith "Should not exist before eval"

(** Complex cps transformation. Complex means that the term is a computation
    (i.e., not a value). A continuation must also be supplied as argument to
    the transformation, indicating where control is transferred to when the
    computation has finished. *)
and cps_app cont t =
  match t with

  (* Function application is a complex expression (since it is a computation).
     Optimize the case when either the function or argument is a value. *)
  | TmApp(a,t1,t2) ->
    let wrapopt (a, a') = Some a,a' in
    let f, f' =
      if is_value t1
      then None, cps_value t1
      else wrapopt (genvar noidx) in
    let e, e' =
      if is_value t2
      then None, cps_value t2
      else wrapopt (genvar noidx) in
    let app = TmApp(a,TmApp(a,f',cont),e') in
    let inner = match e with
      | None -> app
      | Some(e) -> cps_app (TmLam(na,e,app)) t2 in
    let outer = match f with
      | None -> inner
      | Some(f) -> cps_app (TmLam(na,f,inner)) t1 in
    outer

  (* All applications in a match construct can not be lifted, since all but one
     of them will be discarded. Hence, they need to be handled separately.
  *)
  | TmMatch(a,v1,cases) ->
    TmMatch(a,v1,List.map (fun (p,t) -> p, cps_app cont t) cases)

  (* If we have lifted applications, everything else is values. *)
  | TmTup _ | TmTupProj _ | TmUtest _
  | TmRec _ | TmRecProj _ | TmList _ | TmVar _
  | TmLam _ | TmClos _ | TmConst _ | TmIf _
  | TmFix _ | TmConcat _ | TmInfer _ | TmLogPdf _
  | TmSample _ | TmWeight _ | TmDWeight _ -> TmApp(na, cont, cps_value t)

let cps tm =
  let tm = lift_apps tm in
  cps_app idfun tm
