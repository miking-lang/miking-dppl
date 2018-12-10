(** CPS transformations
    Requirements:
    - All functions must take one extra parameter: a continuation function with
    exactly one parameter
    - A function never "returns" (i.e., it never returns something that is not a
    TmApp). Instead, it applies its continuation to its "return value". *)

open Ast
open Const
open Utils

(** Debug the CPS transformation *)
let debug_cps         = false

(** Debug the CPS transformation of the initial environment (builtin) *)
let debug_cps_builtin = false

(** Debug the lifting transformation *)
let debug_lift_apps   = false

(** Check if a term is atomic (contains no computation). TmApp
    is a special case that is never atomic, even if both its function and
    argument are atomic (otherwise, everything would be atomic!). *)
let rec is_atomic = function
  | TmApp _   -> false

  | TmVar _ -> true
  | TmLam _ -> true
  | TmClos _ -> true

  | TmConst _ -> true

  | TmIf _ -> true

  | TmFix _ -> true

  | TmUtest(_,t1,t2) -> is_atomic t1 && is_atomic t2

  | TmMatch(_,t1,pls) ->
    is_atomic t1 && List.for_all (fun (_,te) -> is_atomic te) pls

  | TmRec(_,rels) -> List.for_all (fun (_,te) -> is_atomic te) rels
  | TmRecProj(_,t1,_) -> is_atomic t1

  | TmTup(_,tarr) -> Array.for_all is_atomic tarr
  | TmTupProj(_,t1,_) -> is_atomic t1

  | TmList(_,tls) -> List.for_all is_atomic tls

  | TmConcat _ -> true

  | TmInfer _
  | TmLogPdf _
  | TmSample _
  | TmWeight _
  | TmDWeight _ -> true


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

(** Lift applications as far up as possible in a term. This has the consequence
    of making everything except TmApp and TmMatch (TODO and TmIf?) terms
    atomic. As an effect, CPS transformation is simplified. *)
let rec lift_apps t =

  (* Extract complex terms replacing them with variables, using the argument
     apps as an accumulator. *)
  let extract_complex t apps =
    let res t = let var,var' = genvar noidx in var',(var,t)::apps in
    match lift_apps t with
    | TmApp _ as t -> res t
    | TmMatch _ as t when not (is_atomic t) -> res t
    | _ -> t,apps in

  (* Wrap a term t in applications, binding variables in t to applications as
     specified by apps *)
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
    let t1,apps = extract_complex t1 [] in
    let t2,apps = extract_complex t2 apps in
    wrap_app (TmUtest(a,t1,t2)) apps

  | TmMatch(a,t1,cases) ->
    let cases = List.map (fun (p,t) -> p,lift_apps t) cases in
    let t1,apps = extract_complex t1 [] in
    wrap_app (TmMatch(a,t1,cases)) apps

  | TmRec(a,rels) ->
    let f (rels,apps) (p,t) =
      let t,apps = extract_complex t apps in (p,t)::rels,apps in
    let rels,apps = List.fold_left f ([],[]) rels in
    wrap_app (TmRec(a,List.rev rels)) apps

  | TmRecProj(a,t1,s) ->
    let t1,apps = extract_complex t1 [] in
    wrap_app (TmRecProj(a,t1,s)) apps

  | TmTup(a,tarr) ->
    let f (tls,apps) t =
      let t,apps = extract_complex t apps in t::tls,apps in
    let tarr,apps = Array.fold_left f ([],[]) tarr in
    wrap_app (TmTup(a,Array.of_list (List.rev tarr))) apps

  | TmTupProj(a,t1,i) ->
    let t1,apps = extract_complex t1 [] in
    wrap_app (TmTupProj(a,t1,i)) apps

  | TmList(a,tls) ->
    let f (tls,apps) t =
      let t,apps = extract_complex t apps in t::tls,apps in
    let tls,apps = List.fold_left f ([],[]) tls in
    wrap_app (TmList(a,List.rev tls)) apps

  | TmConcat(_,None) -> t
  | TmConcat _ -> failwith "Should not exist before eval"

  | TmInfer _ -> t
  | TmLogPdf(_,None) -> t
  | TmLogPdf _ -> failwith "Should not exist before eval"
  | TmSample(_,None,None) -> t
  | TmSample _ -> failwith "Should not exist before eval"
  | TmWeight(_,None,None) -> t
  | TmWeight _ -> failwith "Should not exist before eval"
  | TmDWeight(_,None,None) -> t
  | TmDWeight _ -> failwith "Should not exist before eval"

(** CPS transformation of atomic terms (terms containing no computation).
    Transforming atomic terms means that we can perform the CPS transformation
    without supplying a continuation *)
let rec cps_atomic t = match t with

  (* Variables *)
  | TmVar _ -> t

  (* Lambdas *)
  | TmLam(a,x,t1) ->
    let k, k' = genvar noidx in
    TmLam(a, k, TmLam(na, x, cps_complex k' t1))

  (* Should not exist before eval *)
  | TmClos _-> failwith "Closure in cps_atomic"

  (* Function application is never atomic. *)
  | TmApp _ -> failwith "Complex term in cps_atomic"

  (* Pattern matching *)
  | TmMatch(a,t1,pls) ->
    let pls = List.map (fun (p,te) -> p,cps_atomic te) pls in
    TmMatch(a,cps_atomic t1, pls)

  (* Tuples *)
  | TmTup(a,tarr) -> TmTup(a,Array.map cps_atomic tarr)

  (* Tuple projections *)
  | TmTupProj(a,t1,s) -> TmTupProj(a,cps_atomic t1,s)

  (* Records *)
  | TmRec(a,rels) ->
    let rels = List.map (fun (s,te) -> s,cps_atomic te) rels in
    TmRec(a,rels)

  (* Tuple projections *)
  | TmRecProj(a,t1,i) -> TmRecProj(a,cps_atomic t1,i)

  (* Constants *)
  | TmConst _ -> cps_const t

  (* Transforms similarly to constant functions. The difference is that the
     last continuation must be supplied to the branches, and not applied to
     the result. TODO Make if non-curried and simplify this. *)
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
     argument expects a continuation as first argument. TODO Correct? Seems to
     work fine *)
  | TmFix _ ->
    let v, v' = genvar noidx in
    let k, k' = genvar noidx in
    let inner = TmApp(na,
                      t, TmApp(na, v', idfun)) in
    TmLam(na,
          k, TmLam(na, v,
                   TmApp(na, k', inner)))

  (* Unit tests *)
  | TmUtest(a,t1,t2) -> TmUtest(a,cps_atomic t1,cps_atomic t2)

  (* Lists *)
  | TmList(a,tls) -> TmList(a, List.map cps_atomic tls)

  (* Transform some builtin constructs in the same way as
     constants. It is required that the original arity of the function is
     passed to cps_builtin *)
  | TmConcat(_,None)  -> cps_builtin t 2
  | TmConcat _  -> failwith "Should not exist before eval"
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

(** Complex cps transformation. Complex means that the term contains
    computations (i.e., not atomic). A continuation must also be supplied as
    argument to the transformation, indicating where control is transferred to
    when the computation has finished. *)
and cps_complex cont t =
  match t with

  (* Function application is a complex expression.
     Optimize the case when either the function or argument is atomic. *)
  | TmApp(a,t1,t2) ->
    let wrapopt (a, a') = Some a,a' in
    let f, f' =
      if is_atomic t1
      then None, cps_atomic t1
      else wrapopt (genvar noidx) in
    let e, e' =
      if is_atomic t2
      then None, cps_atomic t2
      else wrapopt (genvar noidx) in
    let app = TmApp(a,TmApp(a,f',cont),e') in
    let inner = match e with
      | None -> app
      | Some(e) -> cps_complex (TmLam(na,e,app)) t2 in
    let outer = match f with
      | None -> inner
      | Some(f) -> cps_complex (TmLam(na,f,inner)) t1 in
    outer

  (* All possible applications in a match construct can not be lifted, since
     some of them might be discarded due to the patterns. Hence, TmMatch might
     not be atomic and needs to be handled separately here. We assume that any
     complex terms in v1 has been lifted. Optimize the case when TmMatch is in
     fact atomic (does not replicate the continuation!). TODO Should probably
     make TmIf non-curried and move it here as well for consistency *)
  | TmMatch _ when is_atomic t -> TmApp(na, cont, cps_atomic t)
  | TmMatch(a,v1,cases) ->
    TmMatch(a,v1,List.map (fun (p,t) -> p, cps_complex cont t) cases)

  (* If we have lifted applications, everything else is atomic! *)
  | TmTup _ | TmTupProj _ | TmUtest _
  | TmRec _ | TmRecProj _ | TmList _ | TmVar _
  | TmLam _ | TmClos _ | TmConst _ | TmIf _
  | TmFix _ | TmConcat _ | TmInfer _ | TmLogPdf _
  | TmSample _ | TmWeight _ | TmDWeight _ -> TmApp(na, cont, cps_atomic t)

(** CPS transforms a term, with the identity function as continuation *)
let cps tm =
  let tm = lift_apps tm in

  if debug_lift_apps then
    (print_endline "-- after lift_apps --";
     print_endline (string_of_tm tm);
     print_newline ());

  cps_complex idfun tm
