open Printf
open Ast
open Pprint
open Utils

let debug_eval        = false
let debug_eval_env    = false
let debug_cps         = false
let debug_cps_builtin = false
let debug_infer       = false
let debug_norm        = true

let utest = ref false         (* Set to true if unit testing is enabled *)
let utest_ok = ref 0          (* Counts the number of successful unit tests *)
let utest_fail = ref 0        (* Counts the number of failed unit tests *)
let utest_fail_local = ref 0  (* Counts local failed tests for one file *)

(* Inference types *)
type inference =
  | Importance of int
  | SMC        of int

(* Default inference is importance sampling with 10 particles *)
let inference = ref (Importance(10))

(** Generate fresh variable names for CPS transformation.  Avoids clashes by
    using $ as first char (not allowed in lexer for vars).  Takes a debruijn
    index as argument (for idfun). **)
let nextvar = ref 0
let genvar i =
  let res = !nextvar in
  let str = "$" ^ string_of_int res in
  nextvar := res + 1;
  (str, TmVar(na,str,i))

(** The identity function (with debruijn index) as a tm. **)
let idfun =
  let var, var' = genvar 0 in
  TmLam(na,var,var')

(* Print out error message when a unit test fails *)
let unittest_failed pos t1 t2 =
  print_string ("\n ** Unit test FAILED at " ^
                string_of_position pos ^ " **\n    LHS: " ^ (pprint t1) ^
                "\n    RHS: " ^ (pprint t2))

(* Convert a term into de Bruijn indices *)
let rec debruijn env t =
  match t with
  | TmVar(_,x,_) ->
    let rec find env n = match env with
      | y::ee -> if y = x then n else find ee (n+1)
      | [] -> failwith ("Unknown variable: " ^ x)
    in TmVar(na,x,find env 0)
  | TmLam(a,x,t1) -> TmLam(a,x,debruijn (x::env) t1)
  | TmClos _ -> failwith "Closures should not be available."
  | TmApp(a,t1,t2) -> TmApp(a,debruijn env t1,debruijn env t2)
  | TmConst _ -> t
  | TmIfexp _ -> t
  | TmFix _ -> t

  | TmRec(a,sm) -> TmRec(a,StrMap.map (debruijn env) sm)
  | TmProj(a,t1,x) -> TmProj(a,debruijn env t1,x)

  | TmUtest(a,t1,t2) -> TmUtest(a,debruijn env t1,debruijn env t2)
  | TmNop _ -> t


(* Check if two value terms are equal *)
let val_equal v1 v2 =
  match v1,v2 with
  | TmConst(_,c1),TmConst(_,c2) -> c1 = c2
  | _ -> false

let fail_constapp () = failwith "Incorrect application "

(* Debug function used in the eval function *)
let debug_eval env t =
  if debug_eval then
    (printf "\n-- eval -- \n";
     print_endline (pprint t);
     if debug_eval_env then
        print_endline (pprint_env env))
  else ()

(* Main evaluation loop of a term. Evaluates using big-step semantics *)
let rec eval env t =
  debug_eval env t;
  match t with
  (* Variables using debruijn indices. Need to evaluate because fix point. *)
  | TmVar(_,_,n) -> eval env (List.nth env n)
  (* Lambda and closure conversions *)
  | TmLam(a,x,t1) -> TmClos(a,x,t1,env)
  | TmClos _ -> t
  (* Application *)
  | TmApp(_,t1,t2) ->
      (match eval env t1 with
       (* Closure application *)
       | TmClos(_,_,t3,env2) -> eval ((eval env t2)::env2) t3
       (* Constant application using the delta function *)
       | TmConst(_,c) -> delta c (eval env t2)
       (* Fix *)
       | TmFix _ ->
         (match eval env t2 with
         | TmClos(_,_,t3,env2) as tt -> eval ((TmApp(na,TmFix na,tt))::env2) t3
         | _ -> failwith "Incorrect fix application.")
       (* If-expression *)
       | TmIfexp(_,x1,x2) ->
         (match x1,x2,eval env t2 with
         | None,None,TmConst(_,CBool(b)) -> TmIfexp(na,Some(b),None)
         | Some(b),Some(TmClos(_,_,t3,env3)),TmClos(_,_,t4,env4) ->
              if b then eval (TmNop(na)::env3) t3 else eval (TmNop(na)::env4) t4
         | Some(b),_,(TmClos(_,_,_t3,_) as v3) -> TmIfexp(na,Some(b),Some(v3))
         | _ -> failwith "Incorrect if-expression in the eval function.")
       | _ -> failwith "Application to a non closure value.")
  (* Constant *)
  | TmConst _ | TmFix _ -> t
  (* If expression *)
  | TmIfexp _ -> t
  (* The rest *)
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
    TmNop na
  | TmNop _ -> t
  | TmRec _ -> t (* We don't actually evaluate inside records for now *)
  | TmProj(_,t1,x) ->
    match eval env t1 with
    | TmRec(_,sm) ->
      (match StrMap.find_opt x sm with
       | Some t1 -> t1
       | _ -> TmNop(na))
    | t -> failwith (sprintf "Record projection on non-record tm: %s"
                       (pprint t))


(* Evaluates a constant application. This is the standard delta function
   delta(c,v) with the exception that it returns an expression and not
   a value. This is why the returned value is evaluated in the eval() function.
   The reason for this is that if-expressions return expressions
   and not values. *)
and delta c v  =
    match c,v with
    (* MCore boolean intrinsics *)
    | CBool _,_ -> fail_constapp()

    | CNot,TmConst(a,CBool(v)) -> TmConst(a,CBool(not v))
    | CNot,_ -> fail_constapp()

    | CAnd(None),TmConst(a,CBool(v)) -> TmConst(a,CAnd(Some(v)))
    | CAnd(Some(v1)),TmConst(a,CBool(v2)) -> TmConst(a,CBool(v1 && v2))
    | CAnd _,_ -> fail_constapp()

    | COr(None),TmConst(a,CBool(v)) -> TmConst(a,COr(Some(v)))
    | COr(Some(v1)),TmConst(a,CBool(v2)) -> TmConst(a,CBool(v1 || v2))
    | COr _,_  -> fail_constapp()

    | CChar _,_ -> fail_constapp()

    | CString _,_ -> fail_constapp()

    (* MCore integer intrinsics *)
    | CInt _,_ -> fail_constapp()

    | CModi(None),TmConst(a,CInt(v)) -> TmConst(a,CModi(Some(v)))
    | CModi(Some(v1)),TmConst(a,CInt(v2)) -> TmConst(a,CInt(v1 mod v2))
    | CModi _,_ -> fail_constapp()

    | CSlli(None),TmConst(a,CInt(v)) -> TmConst(a,CSlli(Some(v)))
    | CSlli(Some(v1)),TmConst(a,CInt(v2)) -> TmConst(a,CInt(v1 lsl v2))
    | CSlli(None),_ | CSlli(Some(_)),_  -> fail_constapp()

    | CSrli(None),TmConst(a,CInt(v)) -> TmConst(a,CSrli(Some(v)))
    | CSrli(Some(v1)),TmConst(a,CInt(v2)) -> TmConst(a,CInt(v1 lsr v2))
    | CSrli(None),_ | CSrli(Some(_)),_  -> fail_constapp()

    | CSrai(None),TmConst(a,CInt(v)) -> TmConst(a,CSrai(Some(v)))
    | CSrai(Some(v1)),TmConst(a,CInt(v2)) -> TmConst(a,CInt(v1 asr v2))
    | CSrai(None),_ | CSrai(Some(_)),_  -> fail_constapp()

    (* MCore intrinsic: Floating-point number constant and operations *)
    | CFloat(_),_ -> fail_constapp()

    | CLog,TmConst(a,CFloat(v)) -> TmConst(a,CFloat(log v))
    | CLog,_ -> fail_constapp()

    (* Mcore intrinsic: Polymorphic integer and floating-point numbers *)
    | CAdd(None),TmConst(a,CInt(v)) -> TmConst(a,CAdd(Some(CInt(v))))
    | CAdd(None),TmConst(a,CFloat(v)) -> TmConst(a,CAdd(Some(CFloat(v))))
    | CAdd(Some(CInt(v1))),TmConst(a,CInt(v2)) -> TmConst(a,CInt(v1 + v2))
    | CAdd(Some(CFloat(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CFloat(v1 +. v2))
    | CAdd(Some(CFloat(v1))),TmConst(a,CInt(v2)) ->
      TmConst(a,CFloat(v1 +. (float_of_int v2)))
    | CAdd(Some(CInt(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CFloat((float_of_int v1) +. v2))
    | CAdd(_),_ -> fail_constapp()

    | CSub(None),TmConst(a,CInt(v)) -> TmConst(a,CSub(Some(CInt(v))))
    | CSub(None),TmConst(a,CFloat(v)) -> TmConst(a,CSub(Some(CFloat(v))))
    | CSub(Some(CInt(v1))),TmConst(a,CInt(v2)) -> TmConst(a,CInt(v1 - v2))
    | CSub(Some(CFloat(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CFloat(v1 -. v2))
    | CSub(Some(CFloat(v1))),TmConst(a,CInt(v2)) ->
      TmConst(a,CFloat(v1 -. (float_of_int v2)))
    | CSub(Some(CInt(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CFloat((float_of_int v1) -. v2))
    | CSub(_),_ -> fail_constapp()

    | CMul(None),TmConst(a,CInt(v)) -> TmConst(a,CMul(Some(CInt(v))))
    | CMul(None),TmConst(a,CFloat(v)) -> TmConst(a,CMul(Some(CFloat(v))))
    | CMul(Some(CInt(v1))),TmConst(a,CInt(v2)) -> TmConst(a,CInt(v1 * v2))
    | CMul(Some(CFloat(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CFloat(v1 *. v2))
    | CMul(Some(CFloat(v1))),TmConst(a,CInt(v2)) ->
      TmConst(a,CFloat(v1 *. (float_of_int v2)))
    | CMul(Some(CInt(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CFloat((float_of_int v1) *. v2))
    | CMul(_),_ -> fail_constapp()

    | CDiv(None),TmConst(a,CInt(v)) -> TmConst(a,CDiv(Some(CInt(v))))
    | CDiv(None),TmConst(a,CFloat(v)) -> TmConst(a,CDiv(Some(CFloat(v))))
    | CDiv(Some(CInt(v1))),TmConst(a,CInt(v2)) -> TmConst(a,CInt(v1 / v2))
    | CDiv(Some(CFloat(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CFloat(v1 /. v2))
    | CDiv(Some(CFloat(v1))),TmConst(a,CInt(v2)) ->
      TmConst(a,CFloat(v1 /. (float_of_int v2)))
    | CDiv(Some(CInt(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CFloat((float_of_int v1) /. v2))
    | CDiv(_),_ -> fail_constapp()

    | CNeg,TmConst(a,CFloat(v)) -> TmConst(a,CFloat((-1.0)*.v))
    | CNeg,TmConst(a,CInt(v)) -> TmConst(a,CInt((-1)*v))
    | CNeg,_ -> fail_constapp()

    | CLt(None),TmConst(a,CInt(v)) -> TmConst(a,CLt(Some(CInt(v))))
    | CLt(None),TmConst(a,CFloat(v)) -> TmConst(a,CLt(Some(CFloat(v))))
    | CLt(Some(CInt(v1))),TmConst(a,CInt(v2)) -> TmConst(a,CBool(v1 < v2))
    | CLt(Some(CFloat(v1))),TmConst(a,CFloat(v2)) -> TmConst(a,CBool(v1 < v2))
    | CLt(Some(CFloat(v1))),TmConst(a,CInt(v2)) ->
      TmConst(a,CBool(v1 < (float_of_int v2)))
    | CLt(Some(CInt(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CBool((float_of_int v1) < v2))
    | CLt _,_ -> fail_constapp()

    | CLeq(None),TmConst(a,CInt(v)) -> TmConst(a,CLeq(Some(CInt(v))))
    | CLeq(None),TmConst(a,CFloat(v)) -> TmConst(a,CLeq(Some(CFloat(v))))
    | CLeq(Some(CInt(v1))),TmConst(a,CInt(v2)) -> TmConst(a,CBool(v1 <= v2))
    | CLeq(Some(CFloat(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CBool(v1 <= v2))
    | CLeq(Some(CFloat(v1))),TmConst(a,CInt(v2)) ->
      TmConst(a,CBool(v1 <= (float_of_int v2)))
    | CLeq(Some(CInt(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CBool((float_of_int v1) <= v2))
    | CLeq _,_ -> fail_constapp()

    | CGt(None),TmConst(a,CInt(v)) -> TmConst(a,CGt(Some(CInt(v))))
    | CGt(None),TmConst(a,CFloat(v)) -> TmConst(a,CGt(Some(CFloat(v))))
    | CGt(Some(CInt(v1))),TmConst(a,CInt(v2)) -> TmConst(a,CBool(v1 > v2))
    | CGt(Some(CFloat(v1))),TmConst(a,CFloat(v2)) -> TmConst(a,CBool(v1 > v2))
    | CGt(Some(CFloat(v1))),TmConst(a,CInt(v2)) ->
      TmConst(a,CBool(v1 > (float_of_int v2)))
    | CGt(Some(CInt(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CBool((float_of_int v1) > v2))
    | CGt _,_ -> fail_constapp()

    | CGeq(None),TmConst(a,CInt(v)) -> TmConst(a,CGeq(Some(CInt(v))))
    | CGeq(None),TmConst(a,CFloat(v)) -> TmConst(a,CGeq(Some(CFloat(v))))
    | CGeq(Some(CInt(v1))),TmConst(a,CInt(v2)) -> TmConst(a,CBool(v1 >= v2))
    | CGeq(Some(CFloat(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CBool(v1 >= v2))
    | CGeq(Some(CFloat(v1))),TmConst(a,CInt(v2)) ->
      TmConst(a,CBool(v1 >= (float_of_int v2)))
    | CGeq(Some(CInt(v1))),TmConst(a,CFloat(v2)) ->
      TmConst(a,CBool((float_of_int v1) >= v2))
    | CGeq _,_ -> fail_constapp()

    (* Ragnar polymorphic functions, special case for Ragnar in the boot interpreter.
       These functions should be defined using well-defined ad-hoc polymorphism
       in the real Ragnar compiler. *)
    | CEq(None),t -> TmConst(na,CEq(Some(t)))
    | CEq(Some(TmConst(_,c1))),TmConst(_,c2) -> TmConst(na,CBool(c1 = c2))
    | CEq(Some(TmNop _)), TmNop _ -> TmConst(na,CBool(true))
    | CEq(_),             TmNop _ -> TmConst(na,CBool(false))
    | CEq(Some(TmNop _)), _ -> TmConst(na,CBool(false))
    | CEq _,_  -> fail_constapp()

    | CNeq(None),t -> TmConst(na,CNeq(Some(t)))
    | CNeq(Some(TmConst(_,c1))),TmConst(_,c2) -> TmConst(na,CBool(c1 <> c2))
    | CNeq(Some(TmNop _)), TmNop _ -> TmConst(na,CBool(false))
    | CNeq(_),             TmNop _ -> TmConst(na,CBool(true))
    | CNeq(Some(TmNop _)), _ -> TmConst(na,CBool(true))
    | CNeq _,_  -> fail_constapp()

    (* Probabilistic constructs and probability distributions *)
    | CInfer,(TmClos _ as model) -> infer model
    | CInfer,_ -> fail_constapp()

    | CLogPdf(None),t -> TmConst(na,CLogPdf(Some(t)))
    | CLogPdf(Some(t1)),t2 -> Dist.logpdf t1 t2

    | CSample([]),(TmClos _ as cont) -> TmConst(na,CSample([cont]))
    | CSample([cont]),dist -> TmConst(na,CSample([dist;cont]))
    | CSample _,_ -> fail_constapp()

    | CWeight([]),(TmClos _ as cont) -> TmConst(na,CWeight([cont]))
    | CWeight([cont]),(TmConst(_,CFloat _) as t) ->
      TmConst(na,CWeight([t;cont]))
    | CWeight([cont]),TmConst(_,CInt(i)) ->
      let t = TmConst(na,CFloat(float_of_int i)) in
      TmConst(na,CWeight([t;cont]))
    | CWeight _,_ -> fail_constapp()

    | CDWeight([]),(TmClos _ as cont) -> TmConst(na,CDWeight([cont]))
    | CDWeight([cont]),(TmConst(_,CFloat _) as t) ->
      TmConst(na,CDWeight([t;cont]))
    | CDWeight([cont]),TmConst(_,CInt(i)) ->
      let t = TmConst(na,CFloat(float_of_int i)) in
      TmConst(na,CDWeight([t;cont]))
    | CDWeight _,_ -> fail_constapp()

    | CNormal([]),TmConst(_,(CFloat _ as mu))  -> TmConst(na,CNormal([mu]))
    | CNormal([]),TmConst(_,CInt(i)) ->
      let mu = CFloat(float_of_int i) in TmConst(na,CNormal([mu]))
    | CNormal([mu]),TmConst(_,(CFloat _ as sigma)) ->
      TmConst(na,CNormal([sigma;mu]))
    | CNormal([mu]),TmConst(_,CInt(i)) ->
      let sigma = CFloat(float_of_int i) in TmConst(na,CNormal([sigma;mu]))
    | CNormal _,_  -> fail_constapp()

    | CUniform([]),TmConst(_,(CFloat _ as a)) -> TmConst(na,CUniform([a]))
    | CUniform([]),TmConst(_,CInt(i)) ->
      let a = CFloat(float_of_int i) in TmConst(na,CUniform([a]))
    | CUniform([a]),TmConst(_,(CFloat _ as b)) -> TmConst(na,CUniform([b;a]))
    | CUniform([a]),TmConst(_,CInt(i)) ->
      let b = CFloat(float_of_int i) in TmConst(na,CUniform([b;a]))
    | CUniform _,_  -> fail_constapp()

    | CGamma([]),TmConst(_,(CFloat _ as a)) -> TmConst(na,CGamma([a]))
    | CGamma([]),TmConst(_,CInt(i)) ->
      let a = CFloat(float_of_int i) in TmConst(na,CGamma([a]))
    | CGamma([a]),TmConst(_,(CFloat _ as b)) -> TmConst(na,CGamma([b;a]))
    | CGamma([a]),TmConst(_,CInt(i)) ->
      let b = CFloat(float_of_int i) in TmConst(na,CGamma([b;a]))
    | CGamma _,_  -> fail_constapp()

    | CExp(None),TmConst(_,(CFloat _ as lambda)) ->
      TmConst(na,CExp(Some(lambda)))
    | CExp(None),TmConst(_,CInt(i)) ->
      let lambda = CFloat(float_of_int i) in TmConst(na,CExp(Some(lambda)))
    | CExp _,_  -> fail_constapp()

    | CBern(None),TmConst(_,(CFloat _ as p)) -> TmConst(na,CBern(Some(p)))
    | CBern(None),TmConst(_,CInt(i)) ->
      let p = CFloat(float_of_int i) in TmConst(na,CBern(Some(p)))
    | CBern _,_  -> fail_constapp()

(** Importance sampling (Likelihood weighting) inference **)
and infer_is model n =

  (* Remove continuation by applying idfun *)
  let model = eval [] (TmApp(na, model, idfun)) in

  (* Replicate model for #samples times with an initial log weight of 0.0 *)
  let s = replicate n (TmApp(na, model, TmNop(na)), 0.0) in

  (* Evaluate one sample to the end *)
  let rec sim (t, w) =
    let t = eval [] t in
    match t with

    (* Sample *)
    | TmConst(_,CSample([dist;cont])) ->
      sim (TmApp(na, cont, Dist.sample dist), w)

    (* Weight (and DWeight)*)
    | TmConst(_,CWeight([TmConst(_,CFloat(wadj));cont]))
    | TmConst(_,CDWeight([TmConst(_,CFloat(wadj));cont])) ->
      if wadj = -. infinity then
        (TmNop(na),wadj)
      else
        sim (TmApp(na, cont, TmNop(na)), w +. wadj)

    (* Result *)
    | _ -> t, w in

  let res = List.map sim s in

  if debug_infer then
    (print_endline "-- infer result --";
     List.iter
       (fun (t, w) ->
          print_string "Sample: ";
          print_endline (pprint t);
          print_string ", Log weight: ";
          print_endline (string_of_float w))
       res;
     print_newline ());

  TmNop(na) (* TODO Here we should return an empirical distribution *)

(** SMC inference **)
and infer_smc model n =

  (* Remove continuation by applying idfun *)
  let model = eval [] (TmApp(na, model, idfun)) in

  (* Replicate model for #samples times with an initial log weight of 0.0 *)
  let s = replicate n (TmApp(na, model, TmNop(na)), 0.0) in

  (* Evaluate a sample until encountering a weight *)
  let rec sim (t, w) =
    let t = eval [] t in
    match t with

    (* Sample *)
    | TmConst(_,CSample([dist; cont])) ->
      sim (TmApp(na,cont,Dist.sample dist), w)

    (* Dweight *)
    | TmConst(_,CDWeight[TmConst(_,CFloat(wadj)); cont]) ->
      if wadj = -. infinity then (true,TmNop(na),wadj) else
        sim (TmApp(na,cont,TmNop(na)), w +. wadj)

    (* Weight *)
    | TmConst(_,CWeight([TmConst(_,CFloat(wadj)); cont])) ->
      if wadj = -. infinity then (true,TmNop(na),wadj) else
        (false,TmApp(na,cont,TmNop(na)), w +. wadj)

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
        (List.iter (fun (t, _) -> print_endline (pprint t)) res);

      TmNop(na) (* Here we should return an empirical distribution *)
    end else
      smc res normconst

  in smc s 0.0

(** Select correct inference algorithm **)
and infer model = match !inference with
  | Importance(i) -> infer_is model i
  | SMC(i) -> infer_smc model i

(** Used for unsupported CPS transformations **)
let fail_cps tm =
  failwith ("CPS-transformation of " ^ (pprint tm) ^ " not supported")

(** Wrap constant functions in CPS forms **)
let cps_const t = match t with
  | TmConst(_,c) ->
    let vars = List.map genvar (replicate (arity c) noidx) in
    let inner = List.fold_left
        (fun acc (_, v') ->
           TmApp(na, acc, v'))
        t vars in
    List.fold_right
      (fun (v, _) acc ->
         let k, k' = genvar noidx in
         TmLam(na, k, TmLam(na, v, TmApp(na, k', acc))))
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

  | TmLam(a,x,t1) ->
    let k, k' = genvar noidx in
    TmLam(a, k, TmLam(na, x, cps k' t1))

  (* Should not exist before eval *)
  | TmClos _-> fail_cps t

  (* Function application is not atomic. *)
  | TmApp _ -> failwith "TmApp is not atomic."

  (* Records are treated as atomic for now, but can actually be complex.
     TODO Fix *)
  | TmRec _ -> t

  (* Tuple projection can also be complex, but is treated as atomic for now.
     TODO Fix *)
  | TmProj _ -> t

  (* Constant transformation  *)
  | TmConst(_,c) ->
    (match c with
     | CSample _ | CWeight _ | CDWeight _-> t
     | _ -> cps_const t)

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
    let bapp = TmApp(na, b', c3') in
    let capp = TmApp(na, c', c3') in
    let inner =
      TmApp(na,
            TmApp(na,
                  TmApp(na, t, a'), bapp), capp) in
    let clam =
      TmLam(na,
            c3, TmLam(na, c, inner)) in
    let blam =
      TmLam(na, c2,
            TmLam(na, b, TmApp(na, c2', clam))) in
    let alam =
      TmLam(na, c1,
            TmLam(na, a, TmApp(na, c1', blam))) in
    alam

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

  (* CPS transform both lhs and rhs and apply identity function on result.
     Also transform tnext. TODO Move to complex? *)
  | TmUtest(a,t1,t2) ->
    TmUtest(a,cps idfun t1,cps idfun t2)

  (* Treat as constant *)
  | TmNop _ -> t

(** Complex cps transformation. Complex means that the term is a computation
    (i.e., not a value). A continuation must also be supplied as argument to the
    transformation. **)
and cps cont t =
  match t with
  (* Function application is a complex expression (since it is a computation).
     Optimize the case when either the function or argument is atomic. *)
  | TmApp(a,t1,t2) ->
    let wrapopt (a, a') = Some a, a' in
    let f, f' = match t1 with
      | TmApp _ -> wrapopt (genvar noidx)
      | _ -> None, cps_atomic t1 in
    let e, e' = match t2 with
      | TmApp _ -> wrapopt (genvar noidx)
      | _ -> None, cps_atomic t2 in
    let app = TmApp(a,TmApp(a, f', cont), e') in
    let inner = match e with
      | None -> app
      | Some(e) -> cps (TmLam(na, e, app)) t2 in
    let outer = match f with
      | None -> inner
      | Some(f) -> cps (TmLam(na, f, inner)) t1 in
    outer

  (* Everything else is atomic *)
  | _ -> TmApp(na, cont, cps_atomic t)


