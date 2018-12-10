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

(** Debug the inference procedure *)
let debug_infer       = true

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

(** Debug function used in the eval function *)
let debug_eval env t =
  if debug_eval then
    (printf "\n-- eval -- \n";
     print_endline (string_of_tm t);
     if debug_eval_env then
       print_endline (string_of_env env))
  else ()

(** If the pattern matches the given value, return the extended environment
    where the variables in the pattern are bound to the corresponding terms in
    the value. IMPORTANT: Follows the same traversal order of the pattern as in
    the patenv function to get the correct debruijn indices. *)
let rec match_case env pattern value = match pattern,value with
  | PatVar _,v -> Some(v :: env)

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
     | (TmConcat _ as t1),t2 ->
       failwith (sprintf "Incorrect concatenation application: %s %s"
                        (string_of_tm t1) (string_of_tm t2))

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
          print_string (string_of_tm t);
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

