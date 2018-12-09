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

(** TODO *)
let debug_lift_apps   = false

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
  let lift t apps = match lift_apps t with
    | TmApp _ | TmMatch _ as t when not (is_value t) ->
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
    let t1,apps = lift t1 [] in
    let t2,apps = lift t2 apps in
    wrap_app (TmUtest(a,t1,t2)) apps

  | TmMatch(a,t1,cases) ->
    let cases = List.map (fun (p,t) -> p,lift_apps t) cases in
    let t1,apps = lift t1 [] in
    wrap_app (TmMatch(a,t1,cases)) apps

  | TmRec(a,rels) ->
    let f (rels,apps) (p,t) =
      let t,apps = lift t apps in (p,t)::rels,apps in
    let rels,apps = List.fold_left f ([],[]) rels in
    wrap_app (TmRec(a,List.rev rels)) apps

  | TmRecProj(a,t1,s) ->
    let t1,apps = lift t1 [] in
    wrap_app (TmRecProj(a,t1,s)) apps

  | TmTup(a,tarr) ->
    let f (tls,apps) t =
      let t,apps = lift t apps in t::tls,apps in
    let tarr,apps = Array.fold_left f ([],[]) tarr in
    wrap_app (TmTup(a,Array.of_list (List.rev tarr))) apps

  | TmTupProj(a,t1,i) ->
    let t1,apps = lift t1 [] in
    wrap_app (TmTupProj(a,t1,i)) apps

  | TmList(a,tls) ->
    let f (tls,apps) t =
      let t,apps = lift t apps in t::tls,apps in
    let tls,apps = List.fold_left f ([],[]) tls in
    wrap_app (TmList(a,List.rev tls)) apps

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

  (* All possible applications in a match construct can not be lifted, since
     some of them might be discarded due to the patterns. Hence, TmMatch needs
     to be handled separately here. *)
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

  if debug_lift_apps then
    (print_endline "-- after lift_apps --";
     print_endline (string_of_tm tm);
     print_newline ());

  cps_app idfun tm
