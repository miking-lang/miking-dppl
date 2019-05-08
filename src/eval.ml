(** The semantics of pplcore *)

open Printf
open Ast
open Const
open Pattern
open Utils
open Debug

(** Print out error message when a unit test fails *)
let unittest_failed pos t1 t2 =
  print_string ("\n ** Unit test FAILED at " ^
                string_of_position pos ^
                " **\n    LHS: " ^ (string_of_tm t1) ^
                "\n    RHS: " ^ (string_of_tm t2))

(** Extends an environment used in debruijn conversion with the identifiers
    found in the given pattern *)
let rec patenv env pat = match pat with
  | PatVar(s)         -> s :: env
  | PatRec((_,p)::ps) -> patenv (patenv env p) (PatRec(ps))
  | PatRec([])        -> env
  | PatList(p::ps)    -> patenv (patenv env p) (PatList(ps))
  | PatList([])       -> env
  | PatTup(p::ps)     -> patenv (patenv env p) (PatTup(ps))
  | PatTup([])        -> env
  | PatCons(p1,p2)    -> patenv (patenv env p1) p2
  | PatUnit     | PatChar _
  | PatString _ | PatInt _  | PatFloat _ -> env

(** Add debruijn indices to a term *)
let rec debruijn env t = match t with
  | TmVar(_,x,_) ->
    let rec find env n = match env with
      | y::ee -> if y = x then n else find ee (n+1)
      | [] -> failwith ("Unknown variable in debruijn conversion: " ^ x)
    in TmVar(na,x,find env 0)

  | TmMatch(a,tm,cases) ->
    TmMatch(a,debruijn env tm,
            List.map (fun (p,tm) -> (p, debruijn (patenv env p) tm)) cases)

  | TmLam(a,x,t1)     -> TmLam(a,x,debruijn (x::env) t1)
  | TmClos _          -> failwith "Closures should not be available."

  | _ -> tm_traverse (debruijn env) t

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
        | None     -> None)
     | None -> None)
  | PatRec([]),TmRec _ -> Some env
  | PatRec _,_        -> None

  | PatList(p::ps),TmList(a,v::vs) ->
    (match match_case env p v with
     | Some env -> match_case env (PatList(ps)) (TmList(a,vs))
     | None     -> None)
  | PatList([]),TmList(_,[]) -> Some env
  | PatList _,_              -> None

  | PatTup(ps),TmTup(_,varr) ->
    let rec fold env ps i = match ps with
      | p::ps when i < Array.length varr ->
        (match match_case env p varr.(i) with
         | Some env -> fold env ps (i + 1)
         | None     -> None)
      | [] when i = Array.length varr -> Some env
      | _                             -> None
    in fold env ps 0
  | PatTup _,_ -> None

  | PatCons(p1,p2),TmList(a,v::vs) ->
    (match match_case env p1 v with
     | Some env -> match_case env p2 (TmList(a,vs))
     | None     -> None)
  | PatCons _,_ -> None

  | PatUnit, TmConst(_,CUnit) -> Some env
  | PatUnit, _                -> None

  | PatChar(c1), TmConst(_,CChar(c2)) when c1 = c2 -> Some env
  | PatChar _,_                                    -> None

  | PatString(s1), TmConst(_,CString(s2)) when s1 = s2 -> Some env
  | PatString _,_                                      -> None

  | PatInt(i1), TmConst(_,CInt(i2)) when i1 = i2 -> Some env
  | PatInt _,_                                   -> None

  | PatFloat(f1), TmConst(_,CFloat(f2)) when f1 = f2 -> Some env
  | PatFloat _,_                                     -> None

(** Big-step evaluation of terms
    TODO Optimize for degenerate weights
    TODO Cleanup *)
let rec eval stoch_ctrl env weight t =

  debug debug_eval "Eval" (fun () -> string_of_tm ~pretty:false t);

  match t with

  (* Variables using debruijn indices.
     Need to evaluate because of fix point. *)
  | TmVar(_,_,n) -> eval stoch_ctrl env weight (List.nth env n)

  (* Constants and builtins *)
  | TmConst _  | TmFix _    | TmUtest _
  | TmConcat _ | TmWeight _ | TmResamp _ -> weight,t

  (* Lambda and closure conversions *)
  | TmLam(a,x,t1) -> weight,TmClos(a,x,t1,env)
  | TmClos _      -> weight,t

  (* Application *)
  | TmApp(_,t1,t2) ->
    let weight,v1 = eval stoch_ctrl env weight t1 in
    let weight,v2 = eval stoch_ctrl env weight t2 in

    (match v1,v2 with

     (* Closure application. If LHS is stochastic, the result is stochastic *)
     | TmClos({stoch;_},_,t3,env2),v2 ->
       let weight,tm = eval stoch_ctrl (v2::env2) weight t3 in
       if stoch then weight,make_stoch tm
       else          weight,tm

     (* Constant application with sample as LHS. Always stochastic. *)
     | TmConst(_,(CSample as c)),TmConst(_,v) ->
       weight,make_stoch (TmConst(na,eval_const c v))

     (* Other constant applications. Stochastic if LHS or RHS is stochastic *)
     | TmConst({stoch = s1;_},c),
       TmConst({stoch = s2;_},v) ->
       weight,TmConst({na with stoch = s1 || s2},eval_const c v)

     (* Fixpoint application *)
     | TmFix{stoch;_},(TmClos(_,_,t3,env2) as tt) ->
       let weight,tm =
         eval stoch_ctrl ((TmApp(na,TmFix na,tt))::env2) weight t3 in
       if stoch then weight,make_stoch tm
       else          weight,tm

     (* Concatenation application. Stochastic if the concatenation itself or
        any argument is stochastic *)
     | TmConcat(a,None),(TmList _ as v2)
     | TmConcat(a,None),(TmConst(_,CString _) as v2) ->
       weight,TmConcat(a,Some v2)
     | TmConcat({stoch=s;_},Some TmConst({stoch=s1;_},CString str1)),
       TmConst({stoch=s2;_},CString str2) ->
       weight,TmConst({na with stoch = s || s1 || s2},CString (str1 ^ str2))
     | TmConcat({stoch=s;_},Some TmList({stoch=s1;_},ls1)),
       TmList({stoch=s2;_},ls2) ->
       weight,TmList({na with stoch = s || s1 || s2},ls1 @ ls2)

     (* Unit testing application. Since the arguments to utest are discarded,
        it is only stochastic if the utest itself is stochastic. *)
     | TmUtest(a,None),v1          -> weight,TmUtest(a,Some v1)
     | TmUtest({stoch;pos;_},Some v1),v2 ->
       if !utest then begin
         if tm_compare v1 v2 = 0 then
           (printf "."; utest_ok := !utest_ok + 1)
         else (
           unittest_failed pos v1 v2;
           utest_fail := !utest_fail + 1;
           utest_fail_local := !utest_fail_local + 1)
       end;
       if stoch then weight,make_stoch nop
       else          weight,nop

     (* Weighting application. Stochastic if the weight itself is
        stochastic *)
     | TmWeight({stoch;_}),TmConst(_,CFloat w) ->
       let w = w +. weight in
       if stoch then w, make_stoch nop
       else          w, nop
     | TmWeight({stoch;_}),TmConst(_,CInt w) ->
       let w = (float_of_int w) +. weight in
       if stoch then w, make_stoch nop
       else          w, nop

     (* Resampling application, in CPS form natively. Stochastic if the
        resample is itself stochastic. Note the usage of stoch_ctrl here. *)
     | TmResamp(a,None,None),(TmClos _ as cont) ->
       weight,TmResamp(a,Some cont,None)
     | TmResamp({stoch;_} as a,Some cont,None),TmConst(_,CUnit) ->
       let tm = TmResamp(a,Some cont,Some stoch_ctrl) in
       if stoch then weight,make_stoch tm
       else          weight,tm

     | _ -> failwith (sprintf "Incorrect application:\
                               LHS: %s\n\
                               RHS: %s\n"
                        (string_of_tm v1) (string_of_tm v2)))

  (* If-expression. Evaluate the chosen branch with stoch_ctrl set if the
     condition is stochastic. Furthermore, the result itself is stochastic if
     the condition (or the if expression itself) is stochastic. *)
  | TmIf({stoch;_},t,t1,t2) ->
    let weight,v = eval stoch_ctrl env weight t in
    (match v with
     | TmConst({stoch=stoch_cond;_},CBool(b)) ->
       let stoch_ctrl = stoch_cond || stoch_ctrl in
       let weight,tm = (match b with
        | true -> eval stoch_ctrl env weight t1
        | false -> eval stoch_ctrl env weight t2) in
       if stoch || stoch_cond then weight,make_stoch tm
       else                        weight,tm
     | _ -> failwith "Incorrect condition in if-expression.")

  (* Match expression. Evaluate the chosen term with stoch_ctrl set if the
     condition is stochastic. Furthermore, the result itself is stochastic if
     the condition (or the if expression itself) is stochastic. *)
  | TmMatch({stoch;_},t,cases) ->
    let weight,v = eval stoch_ctrl env weight t in
    let {stoch=stoch_cond;_} = tm_attr v in
    let stoch_ctrl = stoch_cond || stoch_ctrl in
    let rec match_cases cases = match cases with
      | (p,t) :: cases ->
        (match match_case env p v with
         | Some env -> eval stoch_ctrl env weight t
         | None -> match_cases cases)
      | [] -> failwith "Pattern matching failed" in
    let weight,tm = match_cases cases in
    if stoch || stoch_cond then weight,make_stoch tm
    else                        weight,tm

  (* Tuples. If the tuple itself or any of its components are stochastic, the
     result is stochastic. *)
  | TmTup({stoch;_} as a,tarr) ->
    let f = (fun weight e -> eval stoch_ctrl env weight e) in
    let weight,tarr = arr_map_accum f weight tarr in
    let stoch_tarr = Array.exists is_stoch tarr in
    let tm = TmTup(a,tarr) in
    if stoch || stoch_tarr then weight,make_stoch tm
    else                        weight,tm

  (* Tuple projection. Stochastic if itself or its subterm is stochastic. *)
  | TmTupProj({stoch;_},t1,i) ->
    let weight,v = eval stoch_ctrl env weight t1 in
    let {stoch=stoch_inner;_} = tm_attr v in
    let tm =
      (match v with
       | TmTup(_,varr) -> varr.(i)
       | _ -> failwith "Tuple projection on non-tuple") in
    if stoch || stoch_inner then weight,make_stoch tm
    else                         weight,tm

  (* Records. If the record itself or any of its components are stochastic, the
     result is stochastic. *)
  | TmRec({stoch;_} as a,tls) ->
    let f = (fun weight (k,tm) ->
        let weight,tm = eval stoch_ctrl env weight tm in weight,(k,tm)) in
    let weight,tls = map_accum f weight tls in
    let stoch_tls = List.exists (fun (_,tm) -> is_stoch tm) tls in
    let tm = TmRec(a,tls) in
    if stoch || stoch_tls then weight,make_stoch tm
    else                       weight,tm

  (* Record projection. Stochastic if itself or its subterm is stochastic *)
  | TmRecProj({stoch;_},t1,x) ->
    let weight,v = eval stoch_ctrl env weight t1 in
    let {stoch=stoch_inner;_} = tm_attr v in
    let tm =
      (match v with
       | TmRec(_,vls) ->
         (match List.assoc_opt x vls with
          | Some v1 -> v1
          | _ -> failwith "Key not found in record")
       | t -> failwith (sprintf "Record projection on non-record %s"
                          (string_of_tm t))) in
    if stoch || stoch_inner then weight,make_stoch tm
    else                         weight,tm

  (* List eval. Stochastic if itself or any of its elements are stochastic. *)
  | TmList({stoch;_} as a,tls) ->
    let f = (fun weight e -> eval stoch_ctrl env weight e) in
    let weight,tls = map_accum f weight tls in
    let stoch_tls = List.exists is_stoch tls in
    let tm = TmList(a,tls) in
    if stoch || stoch_tls then weight,make_stoch tm
    else                       weight,tm

