(** Interpreter for pplcore *)

open Printf
open Ast
open Debug
open Utest
open Sprint
open Match

(** Error message for incorrect applications *)
let fail_app left right =
  (Printf.printf "\n**  Incorrect application: **\n\
                 \    LHS: %s\n\
                 \    RHS: %s\n"
     (string_of_val left)
     (string_of_val right));
  failwith "fail_app"

(** Make a value stochastic if cond is true. If the value is already
    stochastic, do nothing *)
let set_stoch cond v =
  let {stoch;_} = val_attr v in
  update_val_attr {stoch=stoch || cond} v

(** Big-step evaluation of terms *)
let rec eval stoch stoch_ctrl env (T{t=t';_} as t) =

  debug !debug_eval "Eval"
    (fun () ->
       let str = (string_of_tm ~closure_env:!debug_eval_env t) in
       if !debug_eval_env then
         sprintf "%s\n%s" (string_of_env ~prefix:"env: " env) str
       else
         str);

  match t' with

    (* Variables using debruijn indices.
       Need to evaluate because fix point might exist in env. *)
    | TVar{i;_} -> eval stoch stoch_ctrl env (List.nth env i)

    (* Lambdas, ifs, and matches *)
    | TLam{x;t1;_}  -> set_stoch stoch (val_of_val' (VLam{x;t1;env}))
    | TIf{t1;t2;_}  -> set_stoch stoch (val_of_val' (VIf{t1;t2;env}))
    | TMatch{cls;_} -> set_stoch stoch (val_of_val' (VMatch{cls;env}))

    (* Continuations. For later use, save the state of stoch_ctrl at the
       syntactic origin in the continuation. *)
    | TCont{x;t1;_} -> set_stoch stoch (val_of_val' (VCont{x;t1;stoch_ctrl;env}))

    (* Values *)
    | TVal{v;_} -> set_stoch stoch v

    (* Applications.
       When weight is degenerate, cut off evaluation. *)
    | TApp{t1;t2;_} ->
      let v1 = eval false stoch_ctrl env t1 in
      let v2 = eval false stoch_ctrl env t2 in
      eval_app stoch stoch_ctrl v1 v2

(* Evaluate applications *)
and eval_app stoch stoch_ctrl
    (V{v=v1';at={stoch=s1} as a1} as v1)
    (V{v=v2';at={stoch=s2} as _a2} as v2) =

  debug !debug_eval_app "Eval application"
    (fun () ->
       (Printf.sprintf "%s\n%s\nstoch_ctrl=%B"
          (string_of_val ~prefix:"LHS: "
             ~pretty:false ~closure_env:!debug_eval_env v1)
          (string_of_val ~prefix:"RHS: "
             ~pretty:false ~closure_env:!debug_eval_env v2)
          stoch_ctrl));

  (* Default attribute with stochasticness set if either the LHS or the RHS is
     stochastic, or if the result should be stochastic *)
  let stoch = stoch || s1 || s2 in
  let va = {stoch} in

  match v1',v2' with

    (* Closure application. If LHS is stochastic, the result is stochastic. *)
    | VLam{t1=t11;env;_},_ ->
      eval (stoch || s1) stoch_ctrl (tm_of_val v2::env) t11

    (* Continuation application.
       NOTE: If we are applying a continuation, use the value of stoch_ctrl
       bound in that continuation, and not stoch_ctrl in the current evaluation
       context! *)
    | VCont{t1=t11;env;stoch_ctrl;_},_ ->
      eval (stoch || s1) stoch_ctrl (tm_of_val v2::env) t11

    (* If-application. Evaluate the chosen branch with stoch_ctrl set if the
       condition is stochastic. Furthermore, the result is stochastic if
       the condition (or the if expression itself) is stochastic. *)
    | VIf{t1;t2;env;_},VBool{b;_} ->
      let stoch_ctrl = stoch_ctrl || s2 in
      (match b with
        | true -> eval stoch stoch_ctrl env t1
        | false -> eval stoch stoch_ctrl env t2)
    | VIf _,_ -> fail_app v1 v2

    (* Match-application. Evaluate the chosen term with stoch_ctrl set if the
       condition is stochastic. Furthermore, the result itself is stochastic if
       the condition (or the if expression itself) is stochastic. *)
    | VMatch{cls;env;_},_ ->
      let stoch_ctrl = stoch_ctrl || s2 in
      let rec match_cases cls = match cls with
        | (p,t) :: cls ->
          (match match_case env p v2 with
           | Some env -> eval stoch stoch_ctrl env t
           | None -> match_cases cls)
        | [] -> failwith "Pattern matching failed" in
      match_cases cls

    (* Fixpoint application *)
    | VFix,VLam{t1=t21;env;_} ->
        eval stoch stoch_ctrl
          ((mkapp ~t1:(tm_of_val v1) ~t2:(tm_of_val v2))::env) t21
    | VFix,_ -> fail_app v1 v2

    (* Record construction *)
    | VRec{pls=arg::args;rls;_},_ ->
      V{at=va;v=VRec{pls=args;rls=(arg,v2)::rls}}
    | VRec _,_ -> fail_app v1 v2

    (* Record projection. *)
    | VRecProj{k;_},VRec{pls=[];rls;_} ->
      let v = match List.assoc_opt k rls with
        | Some v -> v
        | _ -> failwith "Key not found in record" in
      set_stoch (s1 || s2) v
    | VRecProj _,_ -> fail_app v1 v2

    (* Tuple construction *)
    | VTup{np=0;_},_   -> fail_app v1 v2
    | VTup{np;varr;_},_ ->
      V{at=va;v=VTup{np=np-1;varr=Array.append [|v2|] varr}}

    (* Tuple projection. *)
    | VTupProj{i;_},VTup{np=0;varr;_} -> set_stoch (s1 || s2) varr.(i)
    | VTupProj _,_                    -> fail_app v1 v2

    (* Lists *)
    | VList _,_ -> fail_app v1 v2

    (* List construction *)
    | VCons{v1=None;_},_               -> V{at=va;v=VCons{v1=Some v2}}
    | VCons{v1=Some v;_}, VList{vls;_} -> V{at=va;v=VList{vls=v::vls}}
    | VCons _,_                        -> fail_app v1 v2

    (* Unit testing application. Only stochastic if LHS is stochastic *)
    | VUtest{pos;v1=None;_},_    -> V{at=a1;v=VUtest{v1=Some v2;pos}}
    | VUtest{pos;v1=Some v1;_},_ ->
      if !utest then
        if Compare.compare v1 v2 = 0 then begin
          printf "."; utest_ok := !utest_ok + 1;
        end
        else begin
          unittest_failed pos v1 v2;
          utest_fail := !utest_fail + 1;
          utest_fail_local := !utest_fail_local + 1;
        end;
      V{at=a1;v=VUnit}

    (* Construct probability distributions *)
    | VDist{d},_ ->
      let d = match d,v2' with
        | DNormal{mu=None;sigma=None;_},
          VFloat{f;_}  -> DNormal{mu=Some f;sigma=None}
        | DNormal{mu=None;sigma=None;_},
          VInt{i;_}    -> DNormal{mu=Some (float_of_int i);sigma=None}
        | DNormal{mu=Some f1;sigma=None;_},
          VFloat{f=f2;_} -> DNormal{mu=Some f1;sigma=Some f2 }
        | DNormal{mu=Some f1;sigma=None;_},
          VInt{i=i2;_}   ->
          DNormal{mu=Some f1;sigma=Some (float_of_int i2)}
        | DNormal _,_  -> fail_app v1 v2

        | DUniform{a=None;b=None;_},
          VFloat{f;_}  -> DUniform{a=Some f;b=None}
        | DUniform{a=None;b=None;_},
          VInt{i;_}    -> DUniform{a=Some (float_of_int i);b=None}
        | DUniform{a=Some f1;b=None;_},
          VFloat{f=f2;_} -> DUniform{a=Some f1;b=Some f2}
        | DUniform{a=Some f1;b=None;_},
          VInt{i=i2;_}   -> DUniform{a=Some f1;b=Some(float_of_int i2)}
        | DUniform _,_ -> fail_app v1 v2

        | DGamma{a=None;b=None;_},
          VFloat{f;_}  -> DGamma{a=Some f;b=None}
        | DGamma{a=None;b=None;_},
          VInt{i;_}    -> DGamma{a=Some (float_of_int i);b=None}
        | DGamma{a=Some f1;b=None;_},
          VFloat{f=f2;_} -> DGamma{a=Some f1;b=Some f2}
        | DGamma{a=Some f1;b=None;_},
          VInt{i=i2;_}   -> DGamma{a=Some f1;b=Some (float_of_int i2)}
        | DGamma _,_   -> fail_app v1 v2

        | DExp{lam=None;_},VFloat{f;_} -> DExp{lam=Some(f)}
        | DExp{lam=None;_},VInt{i;_}   -> DExp{lam=Some(float_of_int i)}
        | DExp _,_                 -> fail_app v1 v2

        | DBern{p=None;_},VFloat{f;_} -> DBern{p=Some(f)}
        | DBern{p=None;_},VInt{i;_}   -> DBern{p=Some(float_of_int i)}
        | DBern _,_                   -> fail_app v1 v2

        | DBeta{a=None;b=None;_},
          VFloat{f;_}  -> DBeta{a=Some f;b=None}
        | DBeta{a=None;b=None;_},
          VInt{i;_}    -> DBeta{a=Some (float_of_int i);b=None}
        | DBeta{a=Some f1;b=None;_},
          VFloat{f=f2;_} -> DBeta{a=Some f1;b=Some f2 }
        | DBeta{a=Some f1;b=None;_},
          VInt{i=i2;_}   ->
          DBeta{a=Some f1;b=Some (float_of_int i2)}
        | DBeta _,_  -> fail_app v1 v2
      in V{at=va;v=VDist{d}}

    (* Sampling application, in CPS form natively.  Semantics handled by
       inference algorithm. *)
    | VSample({cont=None;_} as v),VCont _ ->
      V{at=va; v=VSample{v with cont=Some v2}}
    | VSample({cont=Some _;_} as v),VDist{d} ->
      set_stoch true (V{at=va; v=VSample{v with d=Some d}})
    | VSample _,_       -> fail_app v1 v2

    (* Weighting application, in CPS form natively. Semantics handled by
       inference algorithm. *)
    | VWeight({cont=None;_} as v),VCont _ ->
      V{at=va;v=VWeight{v with cont=Some v2}}
    | VWeight({cont=Some _;_} as v),VFloat{f} ->
      V{at=va;v=VWeight{v with w=Some f}}
    | VWeight({cont=Some _;_} as v),VInt{i} ->
      V{at=va;v=VWeight{v with w=Some (float_of_int i)}}
    | VWeight _,_       -> fail_app v1 v2

    (* Resampling application, in CPS form natively. Semantics handled by
       inference algorithm*)
    | VResamp({cont=None;_} as v),VCont _ ->
      V{at=va;v=VResamp{v with cont=Some v2}}
    | VResamp({cont=Some _;_} as v), VUnit ->
      V{at=va;v=VResamp{v with stoch_ctrl=Some stoch_ctrl}}
    | VResamp _,_       -> fail_app v1 v2

    (* Compute log pdf *)
    | VLogPdf{v1=None;_},_ -> V{at=va;v=VLogPdf{v1=Some v2}}
    | VLogPdf{v1=Some V{v=v1';_}},VDist{d} -> V{at=va;v=Dist.logpdf v1' d}
    | VLogPdf{v1=Some _},_ -> fail_app v1 v2

    (* Unit constant *)
    | VUnit ,_ -> fail_app v1 v2

    (* Boolean constant and operations *)
    | VBool _,_ -> fail_app v1 v2

    | VNot,VBool{b;_} -> V{at=va;v=VBool{b=not b}}
    | VNot,_          -> fail_app v1 v2

    | VAnd{b1=None;_},    VBool{b;_}    -> V{at=va;v=VAnd{b1=Some(b)}}
    | VAnd{b1=Some b1;_}, VBool{b=b2;_} -> V{at=va;v=VBool{b=b1 && b2}}
    | VAnd _,_                          -> fail_app v1 v2

    | VOr{b1=None;_},    VBool{b;_}    -> V{at=va;v=VOr{b1=Some(b)}}
    | VOr{b1=Some(b1);_},VBool{b=b2;_} -> V{at=va;v=VBool{b=b1 || b2}}
    | VOr _,_                          -> fail_app v1 v2

    (* Character constants and operations *)
    | VChar _,_ -> fail_app v1 v2

    (* String constants and operations *)
    | VString _,_ -> fail_app v1 v2

    (* Integer constants and operations *)
    | VInt _,_ -> fail_app v1 v2

    | VMod{i1=None;_},    VInt{i;_}   -> V{at=va;v=VMod{i1=Some i}}
    | VMod{i1=Some i1;_},VInt{i=i2;_} -> V{at=va;v=VInt{i=i1 mod i2}}
    | VMod _,_                        -> fail_app v1 v2

    | VSll{i1=None;_},    VInt{i;_}   -> V{at=va;v=VSll{i1=Some i}}
    | VSll{i1=Some i1;_},VInt{i=i2;_} -> V{at=va;v=VInt{i=i1 lsl i2}}
    | VSll _,_                        -> fail_app v1 v2

    | VSrl{i1=None;_},   VInt{i;_}   -> V{at=va;v=VSrl{i1=Some i}}
    | VSrl{i1=Some i1;_},VInt{i=i2;_} -> V{at=va;v=VInt{i=i1 lsr i2}}
    | VSrl _,_                        -> fail_app v1 v2

    | VSra{i1=None;_},    VInt{i;_}   -> V{at=va;v=VSra{i1=Some i}}
    | VSra{i1=Some i1;_},VInt{i=i2;_} -> V{at=va;v=VInt{i=i1 asr i2}}
    | VSra _,_                        -> fail_app v1 v2

    (* Floating-point constants and number operations *)
    | VFloat _,_ -> fail_app v1 v2

    | VLog,VFloat{f;_} -> V{at=va;v=VFloat{f=log f}}
    | VLog,VInt{i;_}   -> V{at=va;v=VFloat{f=log (float_of_int i)}}
    | VLog,_           -> fail_app v1 v2

    (* Polymorphic integer/floating-point functions *)
    | VAdd{v1=None;_},
      VInt _       -> V{at=va;v=VAdd{v1=Some v2}}
    | VAdd{v1=None;_},
      VFloat _     -> V{at=va;v=VAdd{v1=Some v2}}
    | VAdd{v1=Some V{v=VInt{i=i1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VInt{i=i1 + i2}}
    | VAdd{v1=Some V{v=VFloat{f=f1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VFloat{f=f1 +. f2}}
    | VAdd{v1=Some V{v=VFloat{f=f1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VFloat{f=f1 +. (float_of_int i2)}}
    | VAdd{v1=Some V{v=VInt{i=i1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VFloat{f=(float_of_int i1) +. f2}}
    | VAdd _,_     -> fail_app v1 v2

    | VSub{v1=None;_},
      VInt _       -> V{at=va;v=VSub{v1=Some v2}}
    | VSub{v1=None;_},
      VFloat _     -> V{at=va;v=VSub{v1=Some v2}}
    | VSub{v1=Some V{v=VInt{i=i1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VInt{i=i1 - i2}}
    | VSub{v1=Some V{v=VFloat{f=f1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VFloat{f=f1 -. f2}}
    | VSub{v1=Some V{v=VFloat{f=f1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VFloat{f=f1 -. (float_of_int i2)}}
    | VSub{v1=Some V{v=VInt{i=i1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VFloat{f=(float_of_int i1) -. f2}}
    | VSub _,_     -> fail_app v1 v2

    | VMul{v1=None;_},
      VInt _       -> V{at=va;v=VMul{v1=Some v2}}
    | VMul{v1=None;_},
      VFloat _     -> V{at=va;v=VMul{v1=Some v2}}
    | VMul{v1=Some V{v=VInt{i=i1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VInt{i=i1 * i2}}
    | VMul{v1=Some V{v=VFloat{f=f1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VFloat{f=f1 *. f2}}
    | VMul{v1=Some V{v=VFloat{f=f1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VFloat{f=f1 *. (float_of_int i2)}}
    | VMul{v1=Some V{v=VInt{i=i1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VFloat{f=(float_of_int i1) *. f2}}
    | VMul _,_     -> fail_app v1 v2

    | VDiv{v1=None;_},
      VInt _       -> V{at=va;v=VDiv{v1=Some v2}}
    | VDiv{v1=None;_},
      VFloat _     -> V{at=va;v=VDiv{v1=Some v2}}
    | VDiv{v1=Some V{v=VInt{i=i1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VInt{i=i1 / i2}}
    | VDiv{v1=Some V{v=VFloat{f=f1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VFloat{f=f1 /. f2}}
    | VDiv{v1=Some V{v=VFloat{f=f1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VFloat{f=f1 /. (float_of_int i2)}}
    | VDiv{v1=Some V{v=VInt{i=i1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VFloat{f=(float_of_int i1) /. f2}}
    | VDiv _,_     -> fail_app v1 v2

    | VNeg,VFloat{f;_} -> V{at=va;v=VFloat{f=(-1.0)*.f}}
    | VNeg,VInt{i;_}   -> V{at=va;v=VInt{i=(-1)*i}}
    | VNeg, _          -> fail_app v1 v2

    | VLt{v1=None;_},
      VInt _       -> V{at=va;v=VLt{v1=Some v2}}
    | VLt{v1=None;_},
      VFloat _     -> V{at=va;v=VLt{v1=Some v2}}
    | VLt{v1=Some V{v=VInt{i=i1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VBool{b=i1 < i2}}
    | VLt{v1=Some V{v=VFloat{f=f1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VBool{b=f1 < f2}}
    | VLt{v1=Some V{v=VFloat{f=f1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VBool{b=f1 < (float_of_int i2)}}
    | VLt{v1=Some V{v=VInt{i=i1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VBool{b=(float_of_int i1) < f2}}
    | VLt _,_      -> fail_app v1 v2

    | VLeq{v1=None;_},
      VInt _       -> V{at=va;v=VLeq{v1=Some v2}}
    | VLeq{v1=None;_},
      VFloat _     -> V{at=va;v=VLeq{v1=Some v2}}
    | VLeq{v1=Some V{v=VInt{i=i1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VBool{b=i1 <= i2}}
    | VLeq{v1=Some V{v=VFloat{f=f1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VBool{b=f1 <= f2}}
    | VLeq{v1=Some V{v=VFloat{f=f1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VBool{b=f1 <= (float_of_int i2)}}
    | VLeq{v1=Some V{v=VInt{i=i1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VBool{b=(float_of_int i1) <= f2}}
    | VLeq _,_     -> fail_app v1 v2

    | VGt{v1=None;_},
      VInt _       -> V{at=va;v=VGt{v1=Some v2}}
    | VGt{v1=None;_},
      VFloat _     -> V{at=va;v=VGt{v1=Some v2}}
    | VGt{v1=Some V{v=VInt{i=i1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VBool{b=i1 > i2}}
    | VGt{v1=Some V{v=VFloat{f=f1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VBool{b=f1 > f2}}
    | VGt{v1=Some V{v=VFloat{f=f1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VBool{b=f1 > (float_of_int i2)}}
    | VGt{v1=Some V{v=VInt{i=i1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VBool{b=(float_of_int i1) > f2}}
    | VGt _,_ -> fail_app v1 v2

    | VGeq{v1=None;_},
      VInt _       -> V{at=va;v=VGeq{v1=Some v2}}
    | VGeq{v1=None;_},
      VFloat _     -> V{at=va;v=VGeq{v1=Some v2}}
    | VGeq{v1=Some V{v=VInt{i=i1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VBool{b=i1 >= i2}}
    | VGeq{v1=Some V{v=VFloat{f=f1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VBool{b=f1 >= f2}}
    | VGeq{v1=Some V{v=VFloat{f=f1;_};_}},
      VInt{i=i2;_}   -> V{at=va;v=VBool{b=f1 >= (float_of_int i2)}}
    | VGeq{v1=Some V{v=VInt{i=i1;_};_}},
      VFloat{f=f2;_} -> V{at=va;v=VBool{b=(float_of_int i1) >= f2}}
    | VGeq _,_     -> fail_app v1 v2

    (* Polymorphic functions *)
    | VEq{v1=None;_},_      -> V{at=va;v=VEq{v1=Some v2}}
    | VEq{v1=Some v1;_},_ -> V{at=va;v=VBool{b=Compare.compare v1 v2 = 0}}

    | VNeq{v1=None;_},_      -> V{at=va;v=VNeq{v1=Some v2}}
    | VNeq{v1=Some v1;_},_ -> V{at=va;v=VBool{b=Compare.compare v1 v2 <> 0}}

    (* Concatenation application.  *)
    | VConcat{v1=None;_}, VList _
    | VConcat{v1=None;_}, VString _ ->
      V{at=va;v=VConcat{v1=Some v2}}
    | VConcat{v1=Some V{v=VString{s=s1;_};_}}, VString{s=s2} ->
      V{at=va;v=VString{s=s1 ^ s2}}
    | VConcat{v1=Some V{v=VList{vls=vls1;_};_}}, VList{vls=vls2} ->
      V{at=va;v=VList{vls=vls1 @ vls2}}
    | VConcat _,_ -> fail_app v1 v2

