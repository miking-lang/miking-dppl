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

(** Big-step evaluation of terms TODO Optimize for degenerate weights *)
let rec eval env weight t =

  debug debug_eval "Eval" (fun () -> string_of_tm ~pretty:false t);

  match t with

  (* Variables using debruijn indices.
     Need to evaluate because of fix point. *)
  | TmVar(_,_,n) -> eval env weight (List.nth env n)

  (* Constants and builtins *)
  | TmConst _  | TmFix _    | TmUtest _
  | TmConcat _ | TmWeight _ | TmResamp _ -> weight,t

  (* Lambda and closure conversions *)
  | TmLam(a,x,t1) -> weight,TmClos(a,x,t1,env)
  | TmClos _      -> weight,t

  (* Application *)
  | TmApp(_,t1,t2) ->
    let weight,v1 = eval env weight t1 in
    let weight,v2 = eval env weight t2 in
    (match v1,v2 with

     (* Closure application *)
     | TmClos(_,_,t3,env2),v2 -> eval (v2::env2) weight t3

     (* Constant application *)
     | TmConst(_,c),TmConst(_,v) -> weight,TmConst(na,eval_const c v)

     (* Fix *)
     | TmFix _,(TmClos(_,_,t3,env2) as tt) ->
       eval ((TmApp(na,TmFix na,tt))::env2) weight t3

     | TmConcat(a,None),(TmList _ as v2)
     | TmConcat(a,None),(TmConst(_,CString _) as v2)
       -> weight,TmConcat(a,Some v2)
     | TmConcat(_,Some TmConst(_,CString s1)),TmConst(_,CString s2)
       -> weight,TmConst(na,CString (s1 ^ s2))
     | TmConcat(_,Some TmList(_,ls1)),TmList(_,ls2)
       -> weight,TmList(na,ls1 @ ls2)

     | TmUtest(a,None),v1          -> weight,TmUtest(a,Some v1)
     | TmUtest({pos;_},Some v1),v2 ->
       if !utest then begin
         if val_equal v1 v2 then
           (printf "."; utest_ok := !utest_ok + 1)
         else (
           unittest_failed pos v1 v2;
           utest_fail := !utest_fail + 1;
           utest_fail_local := !utest_fail_local + 1)
       end;
       weight,nop

     | TmWeight _,TmConst(_,CFloat w) -> w +. weight, nop
     | TmWeight _,TmConst(_,CInt w) -> (float_of_int w) +. weight, nop

     | TmResamp(a,None,None),(TmClos _ as cont)
       -> weight,TmResamp(a,Some cont,None)
     | TmResamp(a,cont,None),TmConst(_,CUnit)
       -> weight,TmResamp(a,cont,Some CUnit)

     | _ -> failwith (sprintf "Incorrect application:\
                               LHS: %s\n\
                               RHS: %s\n"
                        (string_of_tm v1) (string_of_tm v2)))

  (* If-expression *)
  | TmIf(_,t,t1,t2) ->
    let weight,v = eval env weight t in
    (match v with
    | TmConst(_,CBool(true)) -> eval env weight t1
    | TmConst(_,CBool(false)) -> eval env weight t2
    | _ -> failwith "Incorrect condition in if-expression.")

  (* Match expression *)
  | TmMatch(_,t,cases) ->
    let weight,v = eval env weight t in
    let rec match_cases cases = match cases with
      | (p,t) :: cases ->
        (match match_case env p v with
         | Some env -> eval env weight t
         | None -> match_cases cases)
      | [] -> failwith "Pattern matching failed"
    in match_cases cases

  (* Tuples *)
  | TmTup(a,tarr) ->
    let f = (fun weight e -> eval env weight e) in
    let weight,tarr = arr_map_accum f weight tarr in
    weight,TmTup(a,tarr)
  | TmTupProj(_,t1,i) ->
    let weight,v = eval env weight t1 in
    (match v with
     | TmTup(_,varr) -> weight,varr.(i)
     | _ -> failwith "Tuple projection on non-tuple")

  (* Records *)
  | TmRec(a,tls) ->
    let f = (fun weight (k,tm) ->
        let weight,tm = eval env weight tm in weight,(k,tm)) in
    let weight,tls = map_accum f weight tls
    in weight,TmRec(a,tls)
  | TmRecProj(_,t1,x) ->
    let weight,v = eval env weight t1 in
    (match v with
     | TmRec(_,vls) ->
       (match List.assoc_opt x vls with
        | Some v1 -> weight,v1
        | _ -> failwith "Key not found in record")
     | t -> failwith (sprintf "Record projection on non-record %s"
                        (string_of_tm t)))

  (* List eval *)
  | TmList(a,tls) ->
    let f = (fun weight e -> eval env weight e) in
    let weight,tls = map_accum f weight tls in
    weight,TmList(a,tls)

