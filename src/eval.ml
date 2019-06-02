(** The semantics of pplcore *)

open Printf
open Ast
open Pattern
open Debug
open Utest
open Sprint

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
  | TVar({x;_} as t) ->
    let rec find env n = match env with
      | y::ee -> if y = x then n else find ee (n+1)
      | [] -> failwith ("Unknown variable in debruijn conversion: " ^ x)
    in TVar{t with i=find env 0}

  | TApp{at;t1;t2;_} -> TApp{at;t1=debruijn env t1; t2=debruijn env t2}

  | TLam({x;t1;_} as t) -> TLam{t with t1=debruijn (x::env) t1}

  | TCont({x;t1;_} as t) -> TCont{t with t1=debruijn (x::env) t1}

  | TIf{at;t1;t2} -> TIf{at;t1=debruijn env t1;t2=debruijn env t2}

  | TMatch{at;cls} ->
    TMatch{at;cls=Utils.map (fun (p,t) -> p,debruijn (patenv env p) t) cls}

  | TVal _ -> t

(** If the pattern matches the given value, return the extended environment
    where the variables in the pattern are bound to the corresponding terms in
    the value. IMPORTANT: Follows the same traversal order of the pattern as in
    the patenv function to get the correct debruijn indices. *)
let rec match_case env pattern value = match pattern,value with
  | PVar _,v -> Some(tm_of_val v :: env)

  | PRec((k,p)::ps),(VRec{pls=[];rls;_} as v) ->
    (match List.assoc_opt k rls with
     | Some v1 ->
       (match match_case env p v1 with
        | Some env -> match_case env (PRec(ps)) v
        | None     -> None)
     | None -> None)
  | PRec([]),VRec _ -> Some env
  | PRec _,_        -> None

  | PList(p::ps),VList{at;vls=v::vs;_} ->
    (match match_case env p v with
     | Some env -> match_case env (PList(ps)) (VList{at;vls=vs})
     | None     -> None)
  | PList([]),VList{vls=[];_} -> Some env
  | PList _,_              -> None

  | PTup(ps),VTup{np=0;varr;_} ->
    let rec fold env ps i = match ps with
      | p::ps when i < Array.length varr ->
        (match match_case env p varr.(i) with
         | Some env -> fold env ps (i + 1)
         | None     -> None)
      | [] when i = Array.length varr -> Some env
      | _                             -> None
    in fold env ps 0
  | PTup _,_ -> None

  | PCons(p1,p2),VList{at;vls=v::vs;_} ->
    (match match_case env p1 v with
     | Some env -> match_case env p2 (VList{at;vls=vs})
     | None     -> None)
  | PCons _,_ -> None

  | PUnit,        VUnit _                      -> Some env
  | PUnit,        _                            -> None
  | PChar(c1),    VChar{c=c2;_}   when c1 = c2 -> Some env
  | PChar _,      _                            -> None
  | PString(s1),  VString{s=s2;_} when s1 = s2 -> Some env
  | PString _,    _                            -> None
  | PInt(i1),     VInt{i=i2;_}    when i1 = i2 -> Some env
  | PInt _,       _                            -> None
  | PFloat(f1),   VFloat{f=f2;_}  when f1 = f2 -> Some env
  | PFloat _,     _                            -> None

(** Big-step evaluation of terms *)
let rec eval stoch stoch_ctrl env weight t =

  debug debug_eval "Eval"
    (fun () ->
       let str = (string_of_tm ~closure_env:debug_eval_env t) in
       if debug_eval_env then
         sprintf "%s\n%s" (string_of_env ~prefix:"env: " env) str
       else
         str);

  match t with

    (* Variables using debruijn indices.
       Need to evaluate because fix point might exist in env. *)
    | TVar{i;_} -> eval stoch stoch_ctrl env weight (List.nth env i)

    (* Lambdas, ifs, and matches *)
    | TLam{x;t1;_}  -> weight,set_stoch stoch (VLam{at=va;x;t1;env})
    | TIf{t1;t2;_}  -> weight,set_stoch stoch (VIf{at=va;t1;t2;env})
    | TMatch{cls;_} -> weight,set_stoch stoch (VMatch{at=va;cls;env})

    (* Continuations. For later use, save the state of stoch_ctrl at the
       syntactic origin in the continuation. TODO Correct? *)
    | TCont{x;t1;_} -> weight,set_stoch stoch (VCont{at=va;x;t1;stoch_ctrl;env})

    (* Values *)
    | TVal{v;_} -> weight,set_stoch stoch v

    (* Applications.
       When weight is degenerate, cut off evaluation already here. *)
    | TApp{t1;t2;_} ->
      let weight,v1 = eval false stoch_ctrl env weight t1 in
      if weight = neg_infinity then weight,VUnit{at=va}
      else let weight,v2 = eval false stoch_ctrl env weight t2 in
        if weight = neg_infinity then weight,VUnit{at=va}
        else eval_app stoch stoch_ctrl weight v1 v2

(* Evaluate applications *)
and eval_app stoch stoch_ctrl weight v1 v2 =

  debug debug_eval_app "Eval application"
    (fun () ->
       (Printf.sprintf "%s\n%s\nstoch_ctrl=%B"
          (string_of_val ~prefix:"LHS: "
             ~pretty:false ~closure_env:debug_eval_env v1)
          (string_of_val ~prefix:"RHS: "
             ~pretty:false ~closure_env:debug_eval_env v2)
          stoch_ctrl));

  (* Extract stochasticness *)
  let ({stoch=s1;_} as a1),({stoch=s2;_} as _a2) = val_attr v1,val_attr v2 in

  (* Default attribute with stochasticness set if either the LHS or the RHS is
     stochastic, or if the result should be stochastic *)
  let stoch = stoch || s1 || s2 in
  let va = {stoch = stoch} in

  match v1,v2 with

    (* Closure application. If LHS is stochastic, the result is stochastic. *)
    | VLam{t1=t11;env;_},_ ->
      eval (stoch || s1) stoch_ctrl (tm_of_val v2::env) weight t11

    (* Continuation application.
       NOTE: If we are applying a continuation, use the value of stoch_ctrl
       bound in that continuation, and not stoch_ctrl in the current evaluation
       context! *)
    | VCont{t1=t11;env;stoch_ctrl;_},_ ->
      eval (stoch || s1) stoch_ctrl (tm_of_val v2::env) weight t11

    (* If-application. Evaluate the chosen branch with stoch_ctrl set if the
       condition is stochastic. Furthermore, the result is stochastic if
       the condition (or the if expression itself) is stochastic. *)
    | VIf{t1;t2;env;_},VBool{b;_} ->
      let stoch_ctrl = stoch_ctrl || s2 in
      (match b with
        | true -> eval stoch stoch_ctrl env weight t1
        | false -> eval stoch stoch_ctrl env weight t2)
    | VIf _,_ -> fail_app v1 v2

    (* Match-application. Evaluate the chosen term with stoch_ctrl set if the
       condition is stochastic. Furthermore, the result itself is stochastic if
       the condition (or the if expression itself) is stochastic. *)
    | VMatch{cls;env;_},v2 ->
      let stoch_ctrl = stoch_ctrl || s2 in
      let rec match_cases cls = match cls with
        | (p,t) :: cls ->
          (match match_case env p v2 with
           | Some env -> eval stoch stoch_ctrl env weight t
           | None -> match_cases cls)
        | [] -> failwith "Pattern matching failed" in
      match_cases cls

    (* Fixpoint application *)
    | VFix _,VLam{t1=t21;env;_} ->
        eval stoch stoch_ctrl
          ((TApp{at=ta;t1=tm_of_val v1;t2=tm_of_val v2})::env) weight t21
    | VFix _,_ -> fail_app v1 v2

    (* Record construction *)
    | VRec{pls=arg::args;rls;_},v2 ->
      weight,VRec{at=va;pls=args;rls=(arg,v2)::rls}
    | VRec _,_ -> fail_app v1 v2

    (* Record projection. *)
    | VRecProj{k;_},VRec{pls=[];rls;_} ->
      let v = match List.assoc_opt k rls with
        | Some v -> v
        | _ -> failwith "Key not found in record" in
      weight,set_stoch (s1 || s2) v
    | VRecProj _,_ -> fail_app v1 v2

    | VTup{np=0;_},v2   -> fail_app v1 v2
    | VTup{np;varr;_},v2 ->
      weight,VTup{at=va;np=np-1;varr=Array.append [|v2|] varr}

    (* Tuple projection. *)
    | VTupProj{i;_},VTup{np=0;varr;_} -> weight,set_stoch (s1 || s2) varr.(i)
    | VTupProj _,_                    -> fail_app v1 v2

    (* Lists *)
    | VList _,_ -> fail_app v1 v2

    (* List construction *)
    | VCons{v1=None;_},v2              -> weight,VCons{at=va;v1=Some v2}
    | VCons{v1=Some v;_}, VList{vls;_} -> weight,VList{at=va;vls=v::vls}
    | VCons _,_                     -> fail_app v1 v2

    (* Unit testing application. Only stochastic if LHS is stochastic *)
    | VUtest{pos;v1=None;_},v1    -> weight,VUtest{at=a1;v1=Some v1;pos}
    | VUtest{pos;v1=Some v1;_},v2 ->
      if !utest then
        if Compare.compare v1 v2 = 0 then begin
          printf "."; utest_ok := !utest_ok + 1;
        end
        else begin
          unittest_failed pos v1 v2;
          utest_fail := !utest_fail + 1;
          utest_fail_local := !utest_fail_local + 1;
        end;
      weight,VUnit{at=a1}

    (* Probability distributions *)
    | VNormal{mu=None;sigma=None;_},
      VFloat{f;_}  -> weight,VNormal{at=va;mu=Some f;sigma=None}
    | VNormal{mu=None;sigma=None;_},
      VInt{i;_}    -> weight,VNormal{at=va;mu=Some (float_of_int i);sigma=None}
    | VNormal{mu=Some f1;sigma=None;_},
      VFloat{f=f2;_} -> weight,VNormal{at=va;mu=Some f1;sigma=Some f2 }
    | VNormal{mu=Some f1;sigma=None;_},
      VInt{i=i2;_}   ->
      weight,VNormal{at=va;mu=Some f1;sigma=Some (float_of_int i2)}
    | VNormal _,_  -> fail_app v1 v2

    | VUniform{a=None;b=None;_},
      VFloat{f;_}  -> weight,VUniform{at=va;a=Some f;b=None}
    | VUniform{a=None;b=None;_},
      VInt{i;_}    -> weight,VUniform{at=va;a=Some (float_of_int i);b=None}
    | VUniform{a=Some f1;b=None;_},
      VFloat{f=f2;_} -> weight,VUniform{at=va;a=Some f1;b=Some f2}
    | VUniform{a=Some f1;b=None;_},
      VInt{i=i2;_}   -> weight,VUniform{at=va;a=Some f1;b=Some(float_of_int i2)}
    | VUniform _,_ -> fail_app v1 v2

    | VGamma{a=None;b=None;_},
      VFloat{f;_}  -> weight,VGamma{at=va;a=Some f;b=None}
    | VGamma{a=None;b=None;_},
      VInt{i;_}    -> weight,VGamma{at=va;a=Some (float_of_int i);b=None}
    | VGamma{a=Some f1;b=None;_},
      VFloat{f=f2;_} -> weight,VGamma{at=va;a=Some f1;b=Some f2}
    | VGamma{a=Some f1;b=None;_},
      VInt{i=i2;_}   -> weight,VGamma{at=va;a=Some f1;b=Some (float_of_int i2)}
    | VGamma _,_   -> fail_app v1 v2

    | VExp{lam=None;_},VFloat{f;_} -> weight,VExp{at=va;lam=Some(f)}
    | VExp{lam=None;_},VInt{i;_}   -> weight,VExp{at=va;lam=Some(float_of_int i)}
    | VExp _,_                 -> fail_app v1 v2

    | VBern{p=None;_},VFloat{f;_} -> weight,VBern{at=va;p=Some(f)}
    | VBern{p=None;_},VInt{i;_}   -> weight,VBern{at=va;p=Some(float_of_int i)}
    | VBern _,_                   -> fail_app v1 v2

    (* Result of sample is always stochastic. *)
    | VSample _,dist -> weight,set_stoch true (Dist.sample dist)

    | VLogPdf{v1=None;_},v1    -> weight,VLogPdf{at=va;v1=Some v1}
    | VLogPdf{v1=Some v1;_},v2 -> weight,set_stoch (s1 || s2) (Dist.logpdf v1 v2)

    (* Weighting application. *)
    | VWeight _,VFloat{f=w;_} ->
      let weight = w +. weight in
      weight,VUnit{at=a1}
    | VWeight _,VInt{i=w;_}   ->
      let weight = (float_of_int w) +. weight in
      weight,VUnit{at=a1}
    | VWeight _,_           -> fail_app v1 v2

    (* Resampling application, in CPS form natively. *)
    | VResamp({cont=None;_} as v),
      (VCont _ as cont) -> weight,VResamp{v with cont=Some cont}
    | VResamp({cont=Some _;_} as v),
      VUnit _           -> weight,VResamp{v with stoch_ctrl=Some stoch_ctrl}
    | VResamp _,_       -> fail_app v1 v2

    (* Unit constant *)
    | VUnit _,_ -> fail_app v1 v2

    (* Boolean constant and operations *)
    | VBool _ ,_ -> fail_app v1 v2

    | VNot _,VBool{b;_} -> weight,VBool{at=va;b=not b}
    | VNot _,_          -> fail_app v1 v2

    | VAnd{b1=None;_},    VBool{b;_}    -> weight,VAnd{at=va;b1=Some(b)}
    | VAnd{b1=Some b1;_}, VBool{b=b2;_} -> weight,VBool{at=va;b=b1 && b2}
    | VAnd _,_                          -> fail_app v1 v2

    | VOr{b1=None;_},    VBool{b;_}    -> weight,VOr{at=va;b1=Some(b)}
    | VOr{b1=Some(b1);_},VBool{b=b2;_} -> weight,VBool{at=va;b=b1 || b2}
    | VOr _,_                          -> fail_app v1 v2

    (* Character constants and operations *)
    | VChar _,_ -> fail_app v1 v2

    (* String constants and operations *)
    | VString _,_ -> fail_app v1 v2

    (* Integer constants and operations *)
    | VInt _,_ -> fail_app v1 v2

    | VMod{i1=None;_},    VInt{i;_}   -> weight, VMod{at=va;i1=Some i}
    | VMod{i1=Some i1;_},VInt{i=i2;_} -> weight, VInt{at=va;i=i1 mod i2}
    | VMod _,_                        -> fail_app v1 v2

    | VSll{i1=None;_},    VInt{i;_}   -> weight, VSll{at=va;i1=Some i}
    | VSll{i1=Some i1;_},VInt{i=i2;_} -> weight, VInt{at=va;i=i1 lsl i2}
    | VSll _,_                        -> fail_app v1 v2

    | VSrl{i1=None;_},    VInt{i;_}   -> weight, VSrl{at=va;i1=Some i}
    | VSrl{i1=Some i1;_},VInt{i=i2;_} -> weight, VInt{at=va;i=i1 lsr i2}
    | VSrl _,_                        -> fail_app v1 v2

    | VSra{i1=None;_},    VInt{i;_}   -> weight, VSra{at=va;i1=Some i}
    | VSra{i1=Some i1;_},VInt{i=i2;_} -> weight, VInt{at=va;i=i1 asr i2}
    | VSra _,_                        -> fail_app v1 v2

    (* Floating-point constants and number operations *)
    | VFloat _,_ -> fail_app v1 v2

    | VLog _,VFloat{f;_} -> weight, VFloat{at=va;f=log f}
    | VLog _,VInt{i;_}   -> weight, VFloat{at=va;f=log (float_of_int i)}
    | VLog _,_           -> fail_app v1 v2

    (* Polymorphic integer/floating-point functions *)
    | VAdd{v1=None;_},
      VInt _       -> weight,VAdd{at=va;v1=Some v2}
    | VAdd{v1=None;_},
      VFloat _     -> weight,VAdd{at=va;v1=Some v2}
    | VAdd{v1=Some VInt{i=i1;_};_},
      VInt{i=i2;_}   -> weight,VInt{at=va;i=i1 + i2}
    | VAdd{v1=Some VFloat{f=f1;_};_},
      VFloat{f=f2;_} -> weight,VFloat{at=va;f=f1 +. f2}
    | VAdd{v1=Some VFloat{f=f1;_};_},
      VInt{i=i2;_}   -> weight, VFloat{at=va;f=f1 +. (float_of_int i2)}
    | VAdd{v1=Some VInt{i=i1;_};_},
      VFloat{f=f2;_} -> weight, VFloat{at=va;f=(float_of_int i1) +. f2}
    | VAdd _,_     -> fail_app v1 v2

    | VSub{v1=None;_},
      VInt _       -> weight,VSub{at=va;v1=Some v2}
    | VSub{v1=None;_},
      VFloat _     -> weight,VSub{at=va;v1=Some v2}
    | VSub{v1=Some(VInt{i=i1;_});_},
      VInt{i=i2;_}   -> weight,VInt{at=va;i=i1 - i2}
    | VSub{v1=Some(VFloat{f=f1;_});_},
      VFloat{f=f2;_} -> weight,VFloat{at=va;f=f1 -. f2}
    | VSub{v1=Some(VFloat{f=f1;_});_},
      VInt{i=i2;_}   -> weight,VFloat{at=va;f=f1 -. (float_of_int i2)}
    | VSub{v1=Some(VInt{i=i1;_});_},
      VFloat{f=f2;_} -> weight,VFloat{at=va;f=(float_of_int i1) -. f2}
    | VSub _,_     -> fail_app v1 v2

    | VMul{v1=None;_},
      VInt _       -> weight,VMul{at=va;v1=Some v2}
    | VMul{v1=None;_},
      VFloat _     -> weight,VMul{at=va;v1=Some v2}
    | VMul{v1=Some(VInt{i=i1;_});_},
      VInt{i=i2;_}   -> weight,VInt{at=va;i=i1 * i2}
    | VMul{v1=Some(VFloat{f=f1;_});_},
      VFloat{f=f2;_} -> weight,VFloat{at=va;f=f1 *. f2}
    | VMul{v1=Some(VFloat{f=f1;_});_},
      VInt{i=i2;_}   -> weight,VFloat{at=va;f=f1 *. (float_of_int i2)}
    | VMul{v1=Some(VInt{i=i1;_});_},
      VFloat{f=f2;_} -> weight,VFloat{at=va;f=(float_of_int i1) *. f2}
    | VMul _,_     -> fail_app v1 v2

    | VDiv{v1=None;_},
      VInt _       -> weight,VDiv{at=va;v1=Some v2}
    | VDiv{v1=None;_},
      VFloat _     -> weight,VDiv{at=va;v1=Some v2}
    | VDiv{v1=Some(VInt{i=i1;_});_},
      VInt{i=i2;_}   -> weight,VInt{at=va;i=i1 / i2}
    | VDiv{v1=Some(VFloat{f=f1;_});_},
      VFloat{f=f2;_} -> weight,VFloat{at=va;f=f1 /. f2}
    | VDiv{v1=Some(VFloat{f=f1;_});_},
      VInt{i=i2;_}   -> weight,VFloat{at=va;f=f1 /. (float_of_int i2)}
    | VDiv{v1=Some(VInt{i=i1;_});_},
      VFloat{f=f2;_} -> weight,VFloat{at=va;f=(float_of_int i1) /. f2}
    | VDiv _,_     -> fail_app v1 v2

    | VNeg _,VFloat{f;_} -> weight,VFloat{at=va;f=(-1.0)*.f}
    | VNeg _,VInt{i;_}   -> weight,VInt{at=va;i=(-1)*i}
    | VNeg _, _          -> fail_app v1 v2

    | VLt{v1=None;_},
      VInt _       -> weight,VLt{at=va;v1=Some v2}
    | VLt{v1=None;_},
      VFloat _     -> weight,VLt{at=va;v1=Some v2}
    | VLt{v1=Some(VInt{i=i1;_});_},
      VInt{i=i2;_}   -> weight,VBool{at=va;b=i1 < i2}
    | VLt{v1=Some(VFloat{f=f1;_});_},
      VFloat{f=f2;_} -> weight,VBool{at=va;b=f1 < f2}
    | VLt{v1=Some(VFloat{f=f1;_});_},
      VInt{i=i2;_}   -> weight,VBool{at=va;b=f1 < (float_of_int i2)}
    | VLt{v1=Some(VInt{i=i1;_});_},
      VFloat{f=f2;_} -> weight,VBool{at=va;b=(float_of_int i1) < f2}
    | VLt _,_      -> fail_app v1 v2

    | VLeq{v1=None;_},
      VInt _       -> weight,VLeq{at=va;v1=Some v2}
    | VLeq{v1=None;_},
      VFloat _     -> weight,VLeq{at=va;v1=Some v2}
    | VLeq{v1=Some(VInt{i=i1;_});_},
      VInt{i=i2;_}   -> weight,VBool{at=va;b=i1 <= i2}
    | VLeq{v1=Some(VFloat{f=f1;_});_},
      VFloat{f=f2;_} -> weight,VBool{at=va;b=f1 <= f2}
    | VLeq{v1=Some(VFloat{f=f1;_});_},
      VInt{i=i2;_}   -> weight,VBool{at=va;b=f1 <= (float_of_int i2)}
    | VLeq{v1=Some(VInt{i=i1;_});_},
      VFloat{f=f2;_} -> weight,VBool{at=va;b=(float_of_int i1) <= f2}
    | VLeq _,_     -> fail_app v1 v2

    | VGt{v1=None;_},
      VInt _       -> weight,VGt{at=va;v1=Some v2}
    | VGt{v1=None;_},
      VFloat _     -> weight,VGt{at=va;v1=Some v2}
    | VGt{v1=Some(VInt{i=i1;_});_},
      VInt{i=i2;_}   -> weight,VBool{at=va;b=i1 > i2}
    | VGt{v1=Some(VFloat{f=f1;_});_},
      VFloat{f=f2;_} -> weight,VBool{at=va;b=f1 > f2}
    | VGt{v1=Some(VFloat{f=f1;_});_},
      VInt{i=i2;_}   -> weight,VBool{at=va;b=f1 > (float_of_int i2)}
    | VGt{v1=Some(VInt{i=i1;_});_},
      VFloat{f=f2;_} -> weight,VBool{at=va;b=(float_of_int i1) > f2}
    | VGt _,_ -> fail_app v1 v2

    | VGeq{v1=None;_},
      VInt _       -> weight,VGeq{at=va;v1=Some v2}
    | VGeq{v1=None;_},
      VFloat _     -> weight,VGeq{at=va;v1=Some v2}
    | VGeq{v1=Some(VInt{i=i1;_});_},
      VInt{i=i2;_}   -> weight,VBool{at=va;b=i1 >= i2}
    | VGeq{v1=Some(VFloat{f=f1;_});_},
      VFloat{f=f2;_} -> weight,VBool{at=va;b=f1 >= f2}
    | VGeq{v1=Some(VFloat{f=f1;_});_},
      VInt{i=i2;_}   -> weight,VBool{at=va;b=f1 >= (float_of_int i2)}
    | VGeq{v1=Some(VInt{i=i1;_});_},
      VFloat{f=f2;_} -> weight,VBool{at=va;b=(float_of_int i1) >= f2}
    | VGeq _,_     -> fail_app v1 v2

    (* Polymorphic functions *)
    | VEq{v1=None;_},v      -> weight,VEq{at=va;v1=Some(v)}
    | VEq{v1=Some v1;_}, v2 -> weight,VBool{at=va;b=Compare.compare v1 v2 = 0}

    | VNeq{v1=None;_},v      -> weight,VNeq{at=va;v1=Some(v)}
    | VNeq{v1=Some(v1);_},v2 -> weight,VBool{at=va;b=Compare.compare v1 v2 <> 0}

    (* Concatenation application.  *)
    | VConcat{v1=None;_}, VList _
    | VConcat{v1=None;_}, VString _ ->
      weight,VConcat{at=va;v1=Some v2}
    | VConcat{v1=Some VString{s=s1;_};_}, VString{s=s2;_} ->
      weight,VString{at=va;s=s1 ^ s2}
    | VConcat{v1=Some VList{vls=vls1;_};_}, VList{vls=vls2;_} ->
      weight,VList{at=va;vls=vls1 @ vls2}
    | VConcat _,_ -> fail_app v1 v2
