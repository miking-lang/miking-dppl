(** The semantics of pplcore *)

open Printf
open Ast
open Pattern
open Sprint
open Utils
open Debug

(** Print out error message when a unit test fails *)
let unittest_failed pos v1 v2 =
  print_string ("\n ** Unit test FAILED at " ^
                string_of_position pos ^
                " **\n    LHS: " ^ (string_of_tm (tm_of_val v1)) ^
                "\n    RHS: " ^ (string_of_tm (tm_of_val v2)))

(** Error message for incorrect constant applications *)
let fail_app left right =
  (Printf.printf "\n  Incorrect application:\n\
                  \    LHS: %s\n\
                  \    RHS: %s\n"
     (string_of_tm (tm_of_val left))
     (string_of_tm (tm_of_val right)));
  failwith "fail_app"

(** Extends an environment used in debruijn conversion with the identifiers
    found in the given pattern *)
let rec patenv env pat = match pat with
  | PVar(s)         -> s :: env
  | PRec((_,p)::ps) -> patenv (patenv env p) (PRec(ps))
  | PRec([])        -> env
  | PList(p::ps)    -> patenv (patenv env p) (PList(ps))
  | PList([])       -> env
  | PTup(p::ps)     -> patenv (patenv env p) (PTup(ps))
  | PTup([])        -> env
  | PCons(p1,p2)    -> patenv (patenv env p1) p2

  | PUnit     | PChar _
  | PString _ | PInt _  | PFloat _ -> env

(** Add debruijn indices to a term *)
let rec debruijn env t = match t with
  | TVar(a,x,_) ->
    let rec find env n = match env with
      | y::ee -> if y = x then n else find ee (n+1)
      | [] -> failwith ("Unknown variable in debruijn conversion: " ^ x)
    in TVar(a,x,find env 0)

  | TApp(a,t1,t2) -> TApp(a,debruijn env t1,debruijn env t2)

  | TLam(a,x,t1) -> TLam(a,x,debruijn (x::env) t1)

  | TIf(a,t1,t2) -> TIf(a,debruijn env t1,debruijn env t2)

  | TMatch(a,cls) ->
    TMatch(a, List.map (fun (p,t) -> p,debruijn (patenv env p) t) cls)

  | TVal _ -> t

(** If the pattern matches the given value, return the extended environment
    where the variables in the pattern are bound to the corresponding terms in
    the value. IMPORTANT: Follows the same traversal order of the pattern as in
    the patenv function to get the correct debruijn indices. *)
let rec match_case env pattern value = match pattern,value with
  | PVar _,v -> Some(tm_of_val v :: env)

  | PRec((k,p)::ps),(VRec([],es) as v) ->
    (match List.assoc_opt k es with
     | Some v1 ->
       (match match_case env p v1 with
        | Some env -> match_case env (PRec(ps)) v
        | None     -> None)
     | None -> None)
  | PRec([]),VRec _ -> Some env
  | PRec _,_        -> None

  | PList(p::ps),VList(v::vs) ->
    (match match_case env p v with
     | Some env -> match_case env (PList(ps)) (VList(vs))
     | None     -> None)
  | PList([]),VList [] -> Some env
  | PList _,_              -> None

  | PTup(ps),VTup(0,varr) ->
    let rec fold env ps i = match ps with
      | p::ps when i < Array.length varr ->
        (match match_case env p varr.(i) with
         | Some env -> fold env ps (i + 1)
         | None     -> None)
      | [] when i = Array.length varr -> Some env
      | _                             -> None
    in fold env ps 0
  | PTup _,_ -> None

  | PCons(p1,p2),VList(v::vs) ->
    (match match_case env p1 v with
     | Some env -> match_case env p2 (VList(vs))
     | None     -> None)
  | PCons _,_ -> None

  | PUnit,        VUnit                    -> Some env
  | PUnit,        _                        -> None
  | PChar(c1),    VChar(c2)   when c1 = c2 -> Some env
  | PChar _,      _                        -> None
  | PString(s1),  VString(s2) when s1 = s2 -> Some env
  | PString _,    _                        -> None
  | PInt(i1),     VInt(i2)    when i1 = i2 -> Some env
  | PInt _,       _                        -> None
  | PFloat(f1),   VFloat(f2)  when f1 = f2 -> Some env
  | PFloat _,     _                        -> None

(** Big-step evaluation of terms
    TODO Optimize for degenerate weights *)
let rec eval stoch_ctrl env weight t =

  debug debug_eval "Eval" (fun () -> string_of_tm ~pretty:false t);

  match t with

  (* Variables using debruijn indices.
     Need to evaluate because fix point might exist in env. *)
  | TVar(_,_,n) -> eval stoch_ctrl env weight (List.nth env n)

  (* Lambdas, ifs, and matches *)
  | TLam(a,x,t1)  -> weight,TVal(a,VClos(x,t1,env))
  | TIf(a,t1,t2)  -> weight,TVal(a,VClosIf(t1,t2,env))
  | TMatch(a,cls) -> weight,TVal(a,VClosMatch(cls,env))

  (* Values *)
  | TVal _ -> weight,t

  (* Applications *)
  | TApp(_,t1,t2) ->
    let weight,v1 = eval stoch_ctrl env weight t1 in
    let weight,v2 = eval stoch_ctrl env weight t2 in
    eval_app stoch_ctrl weight v1 v2

(* Evaluate constant applications
   TODO Cleanup*)
and eval_app stoch_ctrl weight t1 t2 =

  (* Extract values *)
  let v1,v2 = match t1,t2 with
    | TVal(_,v1),TVal(_,v2) -> v1,v2
    | _ -> failwith "Non-value in eval_app" in

  (* Extract stochasticness *)
  let ({stoch=s1;_} as a1),({stoch=s2;_} as _a2) = tm_attr t1,tm_attr t2 in

  (* Default attribute *)
  let da = {a1 with stoch = s1 || s2} in

  match v1,v2 with

  (* Closure application. If LHS is stochastic, the result is stochastic *)
  | VClos(_,t11,env),_ ->
    let weight,t = eval stoch_ctrl (t2::env) weight t11 in
    weight,set_stoch s1 t

  (* If-application. Evaluate the chosen branch with stoch_ctrl set if the
     condition is stochastic. Furthermore, the result itself is stochastic if
     the condition (or the if expression itself) is stochastic. *)
  | VClosIf(t1,t2,env),VBool(b) ->
    let stoch_ctrl = stoch_ctrl || s2 in
    let weight,t = match b with
      | true -> eval stoch_ctrl env weight t1
      | false -> eval stoch_ctrl env weight t2 in
    weight,set_stoch (s1 || s2) t

  (* Match-application. Evaluate the chosen term with stoch_ctrl set if the
     condition is stochastic. Furthermore, the result itself is stochastic if
     the condition (or the if expression itself) is stochastic. *)
  | VClosMatch(cls,env),v2 ->
    let stoch_ctrl = stoch_ctrl || s2 in
    let rec match_cases cls = match cls with
      | (p,t) :: cls ->
        (match match_case env p v2 with
         | Some env -> eval stoch_ctrl env weight t
         | None -> match_cases cls)
      | [] -> failwith "Pattern matching failed" in
    let weight,t = match_cases cls in
    weight,set_stoch (s1 || s2) t

  (* Fixpoint application *)
  | VFix,VClos(_,t21,env2) ->
    let weight,t =
      eval stoch_ctrl ((TApp(na,t1,t2))::env2) weight t21 in
    weight,set_stoch s1 t

  (* Result of sample is always stochastic. *)
  | VSample,dist-> weight,set_stoch true (TVal(a1,Dist.sample dist))

  (* Concatenation application.  *)
  | VConcat(None), VList _
  | VConcat(None), VString _ -> weight,TVal(da,VConcat (Some v2))
  | VConcat(Some VString str1), VString str2 ->
    weight,TVal(da,VString (str1 ^ str2))
  | VConcat(Some VList ls1),    VList ls2 ->
    weight,TVal(da,VList(ls1 @ ls2))

  (* Unit testing application. *)
  | VUtest(None),v2 -> weight,TVal(a1,VUtest(Some v2))
  | VUtest(Some v1),v2 ->
    let {pos;_} = a1 in
    if !utest then
      if compare v1 v2 = 0 then begin
        printf "."; utest_ok := !utest_ok + 1;
      end
      else begin
        unittest_failed pos v1 v2;
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1;
      end;
    weight,TVal(a1,VUnit)

  (* Weighting application. *)
  | VWeight,VFloat w ->
    let weight = w +. weight in
    weight,TVal(a1,VUnit)
  | VWeight,VInt w ->
    let weight = (float_of_int w) +. weight in
    weight,TVal(a1,VUnit)

  (* Resampling application, in CPS form natively. Note the usage of stoch_ctrl
     here.
     TODO Stochastic if the resample is itself stochastic.  *)
  | VResamp(None,None),(VClos _ as cont) ->
    weight,TVal(a1,VResamp(Some cont,None))
  | VResamp(Some cont,None),VUnit ->
    weight,TVal(a1,VResamp(Some cont,Some stoch_ctrl))

  (* Tuple projection. TODO Values also need attributes *)
  | VTupProj(i),VTup(0,varr) -> weight,TVal(da,varr.(i))

  (* Record projection. Stochastic if itself or its subterm is stochastic
     TODO Values also need attributes *)
  | VRecProj(x),VRec([],vls) ->
    let v = match List.assoc_opt x vls with
      | Some v -> v
      | _ -> failwith "Key not found in record" in
    weight,TVal(da,v)

  | VNot,VBool(v) -> weight,TVal(da,VBool(not v))

  | VAnd(None),    VBool(v)  -> weight,TVal(da,VAnd(Some(v)))
  | VAnd(Some(v1)),VBool(v2) -> weight,TVal(da,VBool(v1 && v2))

  | VOr(None),    VBool(v)  -> weight,TVal(da,VOr(Some(v)))
  | VOr(Some(v1)),VBool(v2) -> weight,TVal(da,VBool(v1 || v2))

  (* Character operations *)

  (* String operations *)

  (* Integer operations *)
  | VMod(None),    VInt(v)  -> weight,TVal(da, VMod(Some(v)))
  | VMod(Some(v1)),VInt(v2) -> weight,TVal(da, VInt(v1 mod v2))

  | VSll(None),    VInt(v)  -> weight,TVal(da, VSll(Some(v)))
  | VSll(Some(v1)),VInt(v2) -> weight,TVal(da, VInt(v1 lsl v2))

  | VSrl(None),    VInt(v)  -> weight,TVal(da, VSrl(Some(v)))
  | VSrl(Some(v1)),VInt(v2) -> weight,TVal(da, VInt(v1 lsr v2))

  | VSra(None),    VInt(v)  -> weight,TVal(da, VSra(Some(v)))
  | VSra(Some(v1)),VInt(v2) -> weight,TVal(da, VInt(v1 asr v2))

  (* Floating-point number operations *)
  | VLog,VFloat(v) -> weight,TVal(da, VFloat(log v))

  (* Polymorphic integer/floating-point functions *)
  | VAdd(None),VInt _   -> weight, TVal(da,VAdd(Some v2))
  | VAdd(None),VFloat _ -> weight, TVal(da,VAdd(Some v2))

  | VAdd(Some VInt v1), VInt v2 ->
    weight,TVal(da,VInt(v1 + v2))
  | VAdd(Some VFloat v1),VFloat v2 ->
    weight,TVal(da,VFloat(v1 +. v2))
  | VAdd(Some VFloat v1),VInt v2 ->
    weight, TVal(da,VFloat(v1 +. (float_of_int v2)))
  | VAdd(Some VInt v1),VFloat v2 ->
    weight, TVal(da,VFloat((float_of_int v1) +. v2))

  | VSub(None),VInt _          -> weight,TVal(da,VSub(Some v2))
  | VSub(None),VFloat _        -> weight,TVal(da,VSub(Some v2))
  | VSub(Some(VInt(v1))),VInt(v2) ->
    weight,TVal(da,VInt(v1 - v2))
  | VSub(Some(VFloat(v1))),VFloat(v2) ->
    weight,TVal(da,VFloat(v1 -. v2))
  | VSub(Some(VFloat(v1))),VInt(v2) ->
    weight,TVal(da,VFloat(v1 -. (float_of_int v2)))
  | VSub(Some(VInt(v1))),VFloat(v2) ->
    weight,TVal(da,VFloat((float_of_int v1) -. v2))

  | VMul(None),VInt _   -> weight,TVal(da,VMul(Some v2))
  | VMul(None),VFloat _ -> weight,TVal(da,VMul(Some v2))
  | VMul(Some(VInt(v1))),VInt(v2)
    -> weight,TVal(da,VInt(v1 * v2))
  | VMul(Some(VFloat(v1))),VFloat(v2)
    -> weight,TVal(da,VFloat(v1 *. v2))
  | VMul(Some(VFloat(v1))),VInt(v2)
    -> weight,TVal(da,VFloat(v1 *. (float_of_int v2)))
  | VMul(Some(VInt(v1))),VFloat(v2)
    -> weight,TVal(da,VFloat((float_of_int v1) *. v2))

  | VDiv(None),VInt _                -> weight,TVal(da,VDiv(Some v2))
  | VDiv(None),VFloat _              -> weight,TVal(da,VDiv(Some v2))
  | VDiv(Some(VInt(v1))),VInt(v2)
    -> weight,TVal(da,VInt(v1 / v2))
  | VDiv(Some(VFloat(v1))),VFloat(v2)
    -> weight,TVal(da,VFloat(v1 /. v2))
  | VDiv(Some(VFloat(v1))),VInt(v2)
    -> weight,TVal(da,VFloat(v1 /. (float_of_int v2)))
  | VDiv(Some(VInt(v1))),VFloat(v2)
    -> weight,TVal(da,VFloat((float_of_int v1) /. v2))

  | VNeg,VFloat(v) -> weight,TVal(da,VFloat((-1.0)*.v))
  | VNeg,VInt(v)   -> weight,TVal(da,VInt((-1)*v))

  | VLt(None),VInt _                -> weight,TVal(da,VLt(Some v2))
  | VLt(None),VFloat _              -> weight,TVal(da,VLt(Some v2))
  | VLt(Some(VInt(v1))),VInt(v2)     -> weight,TVal(da,VBool(v1 < v2))
  | VLt(Some(VFloat(v1))),VFloat(v2) -> weight,TVal(da,VBool(v1 < v2))
  | VLt(Some(VFloat(v1))),VInt(v2)   -> weight,TVal(da,VBool(v1 < (float_of_int v2)))
  | VLt(Some(VInt(v1))),VFloat(v2)   -> weight,TVal(da,VBool((float_of_int v1) < v2))

  | VLeq(None),VInt _                -> weight,TVal(da,VLeq(Some v2))
  | VLeq(None),VFloat _              -> weight,TVal(da,VLeq(Some v2))
  | VLeq(Some(VInt(v1))),VInt(v2)     -> weight,TVal(da,VBool(v1 <= v2))
  | VLeq(Some(VFloat(v1))),VFloat(v2) -> weight,TVal(da,VBool(v1 <= v2))
  | VLeq(Some(VFloat(v1))),VInt(v2)   -> weight,TVal(da,VBool(v1 <= (float_of_int v2)))
  | VLeq(Some(VInt(v1))),VFloat(v2)   -> weight,TVal(da,VBool((float_of_int v1) <= v2))

  | VGt(None),VInt _                -> weight,TVal(da,VGt(Some v2))
  | VGt(None),VFloat _              -> weight,TVal(da,VGt(Some v2))
  | VGt(Some(VInt(v1))),VInt(v2)     -> weight,TVal(da,VBool(v1 > v2))
  | VGt(Some(VFloat(v1))),VFloat(v2) -> weight,TVal(da,VBool(v1 > v2))
  | VGt(Some(VFloat(v1))),VInt(v2)   -> weight,TVal(da,VBool(v1 > (float_of_int v2)))
  | VGt(Some(VInt(v1))),VFloat(v2)   -> weight,TVal(da,VBool((float_of_int v1) > v2))

  | VGeq(None),VInt _                -> weight,TVal(da,VGeq(Some v2))
  | VGeq(None),VFloat _              -> weight,TVal(da,VGeq(Some v2))
  | VGeq(Some(VInt(v1))),VInt(v2)     -> weight,TVal(da,VBool(v1 >= v2))
  | VGeq(Some(VFloat(v1))),VFloat(v2) -> weight,TVal(da,VBool(v1 >= v2))
  | VGeq(Some(VFloat(v1))),VInt(v2)   -> weight,TVal(da,VBool(v1 >= (float_of_int v2)))
  | VGeq(Some(VInt(v1))),VFloat(v2)   -> weight,TVal(da,VBool((float_of_int v1) >= v2))

  (* Polymorphic functions *)
  | VEq(None),v      -> weight,TVal(da,VEq(Some(v)))
  | VEq(Some v1), v2 -> weight,TVal(da,VBool(v1 = v2))

  | VNeq(None),v      -> weight,TVal(da,VNeq(Some(v)))
  | VNeq(Some(v1)),v2 -> weight,TVal(da,VBool(v1 <> v2))

  (* Probability distributions *)
  | VNormal(None,None),VFloat(f) -> weight,TVal(da,VNormal(Some f,None))
  | VNormal(None,None),VInt(i)   -> weight,TVal(da,VNormal(Some (float_of_int i),None))
  | VNormal(Some f1,None),VFloat(f2)
    -> weight,TVal(da,VNormal(Some f1,Some f2))
  | VNormal(Some f1,None),VInt(i2)
    -> weight,TVal(da,VNormal(Some f1,Some (float_of_int i2)))

  | VUniform(None,None),VFloat(f) -> weight,TVal(da,VUniform(Some f,None))
  | VUniform(None,None),VInt(i)   -> weight,TVal(da,VUniform(Some (float_of_int i),None))
  | VUniform(Some f1,None),VFloat(f2)
    -> weight,TVal(da,VUniform(Some f1, Some f2))
  | VUniform(Some f1,None),VInt(i2)
    -> weight,TVal(da,VUniform(Some f1,Some(float_of_int i2)))

  | VGamma(None,None),VFloat(f) -> weight,TVal(da,VGamma(Some f,None))
  | VGamma(None,None),VInt(i)   -> weight,TVal(da,VGamma(Some (float_of_int i),None))
  | VGamma(Some f1,None),VFloat(f2)
    -> weight,TVal(da,VGamma(Some f1, Some f2))
  | VGamma(Some f1,None),VInt(i2)
    -> weight,TVal(da,VGamma(Some f1, Some (float_of_int i2)))

  | VExp(None),VFloat(f) -> weight,TVal(da,VExp(Some(f)))
  | VExp(None),VInt(i)   -> weight,TVal(da,VExp(Some (float_of_int i)))

  | VBern(None),VFloat(f) -> weight,TVal(da,VBern(Some(f)))
  | VBern(None),VInt(i)   -> weight,TVal(da,VBern(Some (float_of_int i)))

  | VLogPdf(None),v1      -> weight,TVal(da,VLogPdf(Some v1))
  | VLogPdf(Some v1),v2   -> weight,TVal(da,Dist.logpdf v1 v2)

  | v1,v2 -> fail_app v1 v2



