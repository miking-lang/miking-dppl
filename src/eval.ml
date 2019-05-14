(** The semantics of pplcore *)

open Printf
open Ast
open Pattern
open Sprint
open Debug
open Utest

(** Error message for incorrect constant applications *)
let fail_app left right =
  (Printf.printf "\n**  Incorrect application: **\n\
                  \    LHS: %s\n\
                  \    RHS: %s\n"
     (string_of_val left)
     (string_of_val right));
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

  | PRec((k,p)::ps),(VRec(_,[],es) as v) ->
    (match List.assoc_opt k es with
     | Some v1 ->
       (match match_case env p v1 with
        | Some env -> match_case env (PRec(ps)) v
        | None     -> None)
     | None -> None)
  | PRec([]),VRec _ -> Some env
  | PRec _,_        -> None

  | PList(p::ps),VList(a,v::vs) ->
    (match match_case env p v with
     | Some env -> match_case env (PList(ps)) (VList(a,vs))
     | None     -> None)
  | PList([]),VList(_,[]) -> Some env
  | PList _,_              -> None

  | PTup(ps),VTup(_,0,varr) ->
    let rec fold env ps i = match ps with
      | p::ps when i < Array.length varr ->
        (match match_case env p varr.(i) with
         | Some env -> fold env ps (i + 1)
         | None     -> None)
      | [] when i = Array.length varr -> Some env
      | _                             -> None
    in fold env ps 0
  | PTup _,_ -> None

  | PCons(p1,p2),VList(a,v::vs) ->
    (match match_case env p1 v with
     | Some env -> match_case env p2 (VList(a,vs))
     | None     -> None)
  | PCons _,_ -> None

  | PUnit,        VUnit _                    -> Some env
  | PUnit,        _                          -> None
  | PChar(c1),    VChar(_,c2)   when c1 = c2 -> Some env
  | PChar _,      _                          -> None
  | PString(s1),  VString(_,s2) when s1 = s2 -> Some env
  | PString _,    _                          -> None
  | PInt(i1),     VInt(_,i2)    when i1 = i2 -> Some env
  | PInt _,       _                          -> None
  | PFloat(f1),   VFloat(_,f2)  when f1 = f2 -> Some env
  | PFloat _,     _                          -> None

(** Big-step evaluation of terms *)
let rec eval stoch_ctrl env weight t =

  debug debug_eval "Eval"
    (fun () ->
       let str = (string_of_tm ~closure_env:debug_eval_env t) in
       if debug_eval_env then
         sprintf "%s\n%s" (string_of_env ~prefix:"env: " env) str
       else
         str);

  let weight,v = match t with

    (* Variables using debruijn indices.
       Need to evaluate because fix point might exist in env. *)
    | TVar(_,_,n) -> eval stoch_ctrl env weight (List.nth env n)

    (* Lambdas, ifs, and matches *)
    | TLam(a,x,t1)  -> weight,VClos(a,x,t1,env)
    | TIf(a,t1,t2)  -> weight,VClosIf(a,t1,t2,env)
    | TMatch(a,cls) -> weight,VClosMatch(a,cls,env)

    (* Values *)
    | TVal v -> weight,v

    (* Applications.
       When weight is degenerate, cut off evaluation already here. *)
    | TApp(_,t1,t2) ->
      let weight,v1 = eval stoch_ctrl env weight t1 in
      if weight = neg_infinity then weight,VUnit na
      else let weight,v2 = eval stoch_ctrl env weight t2 in
        if weight = neg_infinity then weight,VUnit na
        else eval_app stoch_ctrl weight v1 v2

  in

  debug debug_eval "Eval complete"
    (fun () ->
       string_of_val
         ~closure_env:debug_eval_env v);

  weight,v

(* Evaluate applications *)
and eval_app stoch_ctrl weight v1 v2 =

  debug debug_eval_app "Eval application"
    (fun () ->
       (Printf.sprintf "%s\n%s"
          (string_of_val ~prefix:"LHS: " ~closure_env:debug_eval_env v1)
          (string_of_val ~prefix:"RHS: " ~closure_env:debug_eval_env v2)));

  (* Extract stochasticness *)
  let ({stoch=s1;_} as a1),({stoch=s2;_} as _a2) = val_attr v1,val_attr v2 in

  (* Default attribute with stochasticness set if either the LHS or the RHS is
     stochastic. *)
  let da = {a1 with stoch = s1 || s2} in

  match v1,v2 with

  (* Closure application. If LHS is stochastic, the result is stochastic *)
  | VClos(_,_,t11,env),_ ->
    let weight,v = eval stoch_ctrl (tm_of_val v2::env) weight t11 in
    weight,set_stoch s1 v

  (* If-application. Evaluate the chosen branch with stoch_ctrl set if the
     condition is stochastic. Furthermore, the result itself is stochastic if
     the condition (or the if expression itself) is stochastic. *)
  | VClosIf(_,t1,t2,env),VBool(_,b) ->
    let stoch_ctrl = stoch_ctrl || s2 in
    let weight,v = match b with
      | true -> eval stoch_ctrl env weight t1
      | false -> eval stoch_ctrl env weight t2 in
    weight,set_stoch (s1 || s2) v
  | VClosIf _,_ -> fail_app v1 v2

  (* Match-application. Evaluate the chosen term with stoch_ctrl set if the
     condition is stochastic. Furthermore, the result itself is stochastic if
     the condition (or the if expression itself) is stochastic. *)
  | VClosMatch(_,cls,env),v2 ->
    let stoch_ctrl = stoch_ctrl || s2 in
    let rec match_cases cls = match cls with
      | (p,t) :: cls ->
        (match match_case env p v2 with
         | Some env -> eval stoch_ctrl env weight t
         | None -> match_cases cls)
      | [] -> failwith "Pattern matching failed" in
    let weight,v = match_cases cls in
    weight,set_stoch (s1 || s2) v

  (* Fixpoint application *)
  | VFix _,VClos(_,_,t21,env2) ->
    let weight,v =
      eval stoch_ctrl
        ((TApp(na,tm_of_val v1,tm_of_val v2))::env2) weight t21 in
    weight,set_stoch (s1 || s2) v
  | VFix _,_ -> fail_app v1 v2

  (* Record construction *)
  | VRec(_,arg::args,ls),v2 -> weight,VRec(da,args,(arg,v2)::ls)
  | VRec _,_ -> fail_app v1 v2

  (* Record projection. *)
  | VRecProj(_,x),VRec(_,[],vls) ->
    let v = match List.assoc_opt x vls with
      | Some v -> v
      | _ -> failwith "Key not found in record" in
    weight,set_stoch (s1 || s2) v
  | VRecProj _,_ -> fail_app v1 v2

  | VTup(_,0,_),v2   -> fail_app v1 v2
  | VTup(_,i,arr),v2 -> weight,VTup(da,i-1,Array.append [|v2|] arr)

  (* Tuple projection. *)
  | VTupProj(_,i),VTup(_,0,varr) -> weight,set_stoch (s1 || s2) varr.(i)
  | VTupProj _,_                 -> fail_app v1 v2

  (* Lists *)
  | VList _,_ -> fail_app v1 v2

  (* List construction *)
  | VCons(_,None),v2              -> weight,VCons(da,Some v2)
  | VCons(_,Some v), VList(_,vls) -> weight,VList(da,v::vls)
  | VCons _,_                     -> fail_app v1 v2

  (* Unit testing application. Only stochastic if LHS is stochastic *)
  | VUtest(_,None),v2    -> weight,VUtest(a1,Some v2)
  | VUtest(_,Some v1),v2 ->
    let {pos;_} = a1 in
    if !utest then
      if Ast.compare v1 v2 = 0 then begin
        printf "."; utest_ok := !utest_ok + 1;
      end
      else begin
        unittest_failed pos v1 v2;
        utest_fail := !utest_fail + 1;
        utest_fail_local := !utest_fail_local + 1;
      end;
    weight,VUnit a1

  (* Probability distributions *)
  | VNormal(_,None,None),
    VFloat(_,f)  -> weight,VNormal(da,Some f,None)
  | VNormal(_,None,None),
    VInt(_,i)    -> weight,VNormal(da,Some (float_of_int i),None)
  | VNormal(_,Some f1,None),
    VFloat(_,f2) -> weight,VNormal(da,Some f1,Some f2)
  | VNormal(_,Some f1,None),
    VInt(_,i2)   -> weight,VNormal(da,Some f1,Some (float_of_int i2))
  | VNormal _,_  -> fail_app v1 v2

  | VUniform(_,None,None),
    VFloat(_,f)  -> weight,VUniform(da,Some f,None)
  | VUniform(_,None,None),
    VInt(_,i)    -> weight,VUniform(da,Some (float_of_int i),None)
  | VUniform(_,Some f1,None),
    VFloat(_,f2) -> weight,VUniform(da,Some f1, Some f2)
  | VUniform(_,Some f1,None),
    VInt(_,i2)   -> weight,VUniform(da,Some f1,Some(float_of_int i2))
  | VUniform _,_ -> fail_app v1 v2

  | VGamma(_,None,None),
    VFloat(_,f)  -> weight,VGamma(da,Some f,None)
  | VGamma(_,None,None),
    VInt(_,i)    -> weight,VGamma(da,Some (float_of_int i),None)
  | VGamma(_,Some f1,None),
    VFloat(_,f2) -> weight,VGamma(da,Some f1, Some f2)
  | VGamma(_,Some f1,None),
    VInt(_,i2)   -> weight,VGamma(da,Some f1, Some (float_of_int i2))
  | VGamma _,_   -> fail_app v1 v2

  | VExp(_,None),VFloat(_,f) -> weight,VExp(da,Some(f))
  | VExp(_,None),VInt(_,i)   -> weight,VExp(da,Some (float_of_int i))
  | VExp _,_                 -> fail_app v1 v2

  | VBern(_,None),VFloat(_,f) -> weight,VBern(da,Some(f))
  | VBern(_,None),VInt(_,i)   -> weight,VBern(da,Some (float_of_int i))
  | VBern _,_                 -> fail_app v1 v2

  (* Result of sample is always stochastic. *)
  | VSample _,dist -> weight,set_stoch true (Dist.sample dist)

  | VLogPdf(a,None),v1      -> weight,VLogPdf(a,Some v1)
  | VLogPdf(_,Some v1),v2   -> weight,Dist.logpdf v1 v2

  (* Weighting application. *)
  | VWeight _,VFloat(_,w) ->
    let weight = w +. weight in
    weight,VUnit a1
  | VWeight _,VInt(_,w)   ->
    let weight = (float_of_int w) +. weight in
    weight,VUnit a1
  | VWeight _,_           -> fail_app v1 v2

  (* Resampling application, in CPS form natively. Note the usage of stoch_ctrl
     here. *)
  | VResamp(_,None,None),
    (VClos _ as cont) -> weight,VResamp(a1,Some cont,None)
  | VResamp(_,Some cont,None),
    VUnit _           -> weight,VResamp(a1,Some cont,Some stoch_ctrl)
  | VResamp _,_       -> fail_app v1 v2

  (* Unit constant *)
  | VUnit _,_ -> fail_app v1 v2

  (* Boolean constant and operations *)
  | VBool _ ,_ -> fail_app v1 v2

  | VNot _,VBool(_,v) -> weight,VBool(da,not v)
  | VNot _,_          -> fail_app v1 v2

  | VAnd(_,None),    VBool(_,v)  -> weight,VAnd(da,Some(v))
  | VAnd(_,Some(v1)),VBool(_,v2) -> weight,VBool(da,v1 && v2)
  | VAnd _,_                     -> fail_app v1 v2

  | VOr(_,None),    VBool(_,v)  -> weight,VOr(da,Some(v))
  | VOr(_,Some(v1)),VBool(_,v2) -> weight,VBool(da,v1 || v2)
  | VOr _,_                     -> fail_app v1 v2

  (* Character constants and operations *)
  | VChar _,_ -> fail_app v1 v2

  (* String constants and operations *)
  | VString _,_ -> fail_app v1 v2

  (* Integer constants and operations *)
  | VInt _,_ -> fail_app v1 v2

  | VMod(_,None),    VInt(_,v)  -> weight, VMod(da,Some(v))
  | VMod(_,Some(v1)),VInt(_,v2) -> weight, VInt(da,v1 mod v2)
  | VMod _,_                    -> fail_app v1 v2

  | VSll(_,None),    VInt(_,v)  -> weight, VSll(da,Some(v))
  | VSll(_,Some(v1)),VInt(_,v2) -> weight, VInt(da,v1 lsl v2)
  | VSll _,_                    -> fail_app v1 v2

  | VSrl(_,None),    VInt(_,v)  -> weight, VSrl(da,Some(v))
  | VSrl(_,Some(v1)),VInt(_,v2) -> weight, VInt(da,v1 lsr v2)
  | VSrl _,_                    -> fail_app v1 v2

  | VSra(_,None),    VInt(_,v)  -> weight, VSra(da,Some(v))
  | VSra(_,Some(v1)),VInt(_,v2) -> weight, VInt(da,v1 asr v2)
  | VSra _,_                    -> fail_app v1 v2

  (* Floating-point constants and number operations *)
  | VFloat _,_ -> fail_app v1 v2

  | VLog _,VFloat(_,v) -> weight, VFloat(da,log v)
  | VLog _,_           -> fail_app v1 v2

  (* Polymorphic integer/floating-point functions *)
  | VAdd(_,None),
    VInt _       -> weight, VAdd(da,Some v2)
  | VAdd(_,None),
    VFloat _     -> weight, VAdd(da,Some v2)
  | VAdd(_,Some VInt(_,v1)),
    VInt(_,v2)   -> weight,VInt(da,v1 + v2)
  | VAdd(_,Some VFloat(_,v1)),
    VFloat(_,v2) -> weight,VFloat(da,v1 +. v2)
  | VAdd(_,Some VFloat(_,v1)),
    VInt(_,v2)   -> weight, VFloat(da,v1 +. (float_of_int v2))
  | VAdd(_,Some VInt(_,v1)),
    VFloat(_,v2) -> weight, VFloat(da,(float_of_int v1) +. v2)
  | VAdd _,_     -> fail_app v1 v2

  | VSub(_,None),
    VInt _       -> weight,VSub(da,Some v2)
  | VSub(_,None),
    VFloat _     -> weight,VSub(da,Some v2)
  | VSub(_,Some(VInt(_,v1))),
    VInt(_,v2)   -> weight,VInt(da,v1 - v2)
  | VSub(_,Some(VFloat(_,v1))),
    VFloat(_,v2) -> weight,VFloat(da,v1 -. v2)
  | VSub(_,Some(VFloat(_,v1))),
    VInt(_,v2)   -> weight,VFloat(da,v1 -. (float_of_int v2))
  | VSub(_,Some(VInt(_,v1))),
    VFloat(_,v2) -> weight,VFloat(da,(float_of_int v1) -. v2)
  | VSub _,_     -> fail_app v1 v2

  | VMul(_,None),
    VInt _       -> weight,VMul(da,Some v2)
  | VMul(_,None),
    VFloat _     -> weight,VMul(da,Some v2)
  | VMul(_,Some(VInt(_,v1))),
    VInt(_,v2)   -> weight,VInt(da,v1 * v2)
  | VMul(_,Some(VFloat(_,v1))),
    VFloat(_,v2) -> weight,VFloat(da,v1 *. v2)
  | VMul(_,Some(VFloat(_,v1))),
    VInt(_,v2)   -> weight,VFloat(da,v1 *. (float_of_int v2))
  | VMul(_,Some(VInt(_,v1))),
    VFloat(_,v2) -> weight,VFloat(da,(float_of_int v1) *. v2)
  | VMul _,_     -> fail_app v1 v2

  | VDiv(_,None),
    VInt _       -> weight,VDiv(da,Some v2)
  | VDiv(_,None),
    VFloat _     -> weight,VDiv(da,Some v2)
  | VDiv(_,Some(VInt(_,v1))),
    VInt(_,v2)   -> weight,VInt(da,v1 / v2)
  | VDiv(_,Some(VFloat(_,v1))),
    VFloat(_,v2) -> weight,VFloat(da,v1 /. v2)
  | VDiv(_,Some(VFloat(_,v1))),
    VInt(_,v2)   -> weight,VFloat(da,v1 /. (float_of_int v2))
  | VDiv(_,Some(VInt(_,v1))),
    VFloat(_,v2) -> weight,VFloat(da,(float_of_int v1) /. v2)
  | VDiv _,_     -> fail_app v1 v2

  | VNeg _,VFloat(_,v) -> weight,VFloat(da,(-1.0)*.v)
  | VNeg _,VInt(_,v)   -> weight,VInt(da,(-1)*v)
  | VNeg _, _          -> fail_app v1 v2

  | VLt(_,None),
    VInt _       -> weight,VLt(da,Some v2)
  | VLt(_,None),
    VFloat _     -> weight,VLt(da,Some v2)
  | VLt(_,Some(VInt(_,v1))),
    VInt(_,v2)   -> weight,VBool(da,v1 < v2)
  | VLt(_,Some(VFloat(_,v1))),
    VFloat(_,v2) -> weight,VBool(da,v1 < v2)
  | VLt(_,Some(VFloat(_,v1))),
    VInt(_,v2)   -> weight,VBool(da,v1 < (float_of_int v2))
  | VLt(_,Some(VInt(_,v1))),
    VFloat(_,v2) -> weight,VBool(da,(float_of_int v1) < v2)
  | VLt _,_      -> fail_app v1 v2

  | VLeq(_,None),
    VInt _       -> weight,VLeq(da,Some v2)
  | VLeq(_,None),
    VFloat _     -> weight,VLeq(da,Some v2)
  | VLeq(_,Some(VInt(_,v1))),
    VInt(_,v2)   -> weight,VBool(da,v1 <= v2)
  | VLeq(_,Some(VFloat(_,v1))),
    VFloat(_,v2) -> weight,VBool(da,v1 <= v2)
  | VLeq(_,Some(VFloat(_,v1))),
    VInt(_,v2)   -> weight,VBool(da,v1 <= (float_of_int v2))
  | VLeq(_,Some(VInt(_,v1))),
    VFloat(_,v2) -> weight,VBool(da,(float_of_int v1) <= v2)
  | VLeq _,_     -> fail_app v1 v2

  | VGt(_,None),
    VInt _       -> weight,VGt(da,Some v2)
  | VGt(_,None),
    VFloat _     -> weight,VGt(da,Some v2)
  | VGt(_,Some(VInt(_,v1))),
    VInt(_,v2)   -> weight,VBool(da,v1 > v2)
  | VGt(_,Some(VFloat(_,v1))),
    VFloat(_,v2) -> weight,VBool(da,v1 > v2)
  | VGt(_,Some(VFloat(_,v1))),
    VInt(_,v2)   -> weight,VBool(da,v1 > (float_of_int v2))
  | VGt(_,Some(VInt(_,v1))),
    VFloat(_,v2) -> weight,VBool(da,(float_of_int v1) > v2)
  | VGt _,_ -> fail_app v1 v2

  | VGeq(_,None),
    VInt _       -> weight,VGeq(da,Some v2)
  | VGeq(_,None),
    VFloat _     -> weight,VGeq(da,Some v2)
  | VGeq(_,Some(VInt(_,v1))),
    VInt(_,v2)   -> weight,VBool(da,v1 >= v2)
  | VGeq(_,Some(VFloat(_,v1))),
    VFloat(_,v2) -> weight,VBool(da,v1 >= v2)
  | VGeq(_,Some(VFloat(_,v1))),
    VInt(_,v2)   -> weight,VBool(da,v1 >= (float_of_int v2))
  | VGeq(_,Some(VInt(_,v1))),
    VFloat(_,v2) -> weight,VBool(da,(float_of_int v1) >= v2)
  | VGeq _,_     -> fail_app v1 v2

  (* Polymorphic functions *)
  | VEq(_,None),v      -> weight,VEq(da,Some(v))
  | VEq(_,Some v1), v2 -> weight,VBool(da,Ast.compare v1 v2 = 0)

  | VNeq(_,None),v      -> weight,VNeq(da,Some(v))
  | VNeq(_,Some(v1)),v2 -> weight,VBool(da,Ast.compare v1 v2 <> 0)

  (* Concatenation application.  *)
  | VConcat(_,None),
    VList _
  | VConcat(_,None),
    VString _       -> weight,VConcat(da,Some v2)
  | VConcat(_,Some VString(_,str1)),
    VString(_,str2) -> weight,VString(da,str1 ^ str2)
  | VConcat(_,Some VList(_,ls1)),
    VList(_,ls2)    -> weight,VList(da,ls1 @ ls2)
  | VConcat _,_     -> fail_app v1 v2





