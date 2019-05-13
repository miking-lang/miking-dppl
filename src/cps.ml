(** CPS transformations
    Requirements:
    - All functions must take one extra parameter: a continuation function with
      exactly one parameter
    - A function never "returns" (i.e., it never returns something that is not
      a TApp). Instead, it applies its continuation to its "return value". *)

open Ast
open Utils

(** Wrap direct-style functions in CPS *)
let cps_fun t arity =
  let vars = List.map genvar (replicate arity noidx) in
  let inner = List.fold_left
      (fun acc (_, v') ->
         TApp(na, acc, v'))
      t vars in
  List.fold_right
    (fun (v, _) acc ->
       let k, k' = genvar noidx in
       TLam(na, k, TLam(na, v, TApp(na, k', acc))))
    vars inner

(** Default CPS transformation of values, based on their arities. *)
let cps_val t = match t with
  | TVal(v) -> cps_fun t (arity v)
  | _ -> failwith "cps_val of non-constant"

(** CPS transformation of atomic terms (terms without computation).
    Transforming atomic terms means that we can perform the CPS transformation
    without supplying a continuation. *)
let rec cps_atomic t = match t with

  (* Variables do not require any action *)
  | TVar _ -> t

  (* Function application is never atomic. *)
  | TApp _ -> failwith "Application term in cps_atomic"

  (* Lambdas.
     Wrap in continuation and transform inner using the continuation. *)
  | TLam(a,x,t1) ->
    let k, k' = genvar noidx in
    TLam(a,k,TLam(na,x,cps_complex k' t1))

  (* If expressions
     Wrap in continuation and transform inner using the continuation. *)
  | TIf(a,t1,t2) ->
    let k, k' = genvar noidx in
    TLam(a,k,TIf(a,cps_complex k' t1,cps_complex k' t2))

  (* Pattern matching
     Wrap in continuation and transform inner using the continuation. *)
  | TMatch(a,pls) ->
    let k, k' = genvar noidx in
    let pls = List.map (fun (p,te) -> p,cps_complex k' te) pls in
    TLam(na,k,TMatch(a,pls))

  (* Values *)
  | TVal(v) -> match v with

    (* Should not exist before eval *)
    | VClos _      -> failwith "Closure in cps_atomic"
    | VClosIf _    -> failwith "Closure in cps_atomic"
    | VClosMatch _ -> failwith "Closure in cps_atomic"

    (* Fixpoint. Treat similar as constant function with a single argument. We
       need to apply the id function to the argument before applying fix, since
       the argument expects a continuation as first argument.
       TODO Correct? Seems to work fine *)
    | VFix _ ->
      let v, v' = genvar noidx in
      let k, k' = genvar noidx in
      let inner = TApp(na, t, TApp(na, v', idfun)) in
      TLam(na, k, TLam(na, v, TApp(na, k', inner)))

    (* Resampling is natively in CPS by design (also the reason why we are
       performing the CPS transformation in the first place) *)
    | VResamp _ -> t

    (* Other values *)
    | VCons _
    | VRec _    | VRecProj _ | VTup _    | VTupProj _
    | VList _   | VUtest _   | VNormal _ | VUniform _
    | VGamma _  | VExp _     | VBern _   | VSample _
    | VLogPdf _ | VWeight _  | VUnit _   | VConcat _
    | VBool _   | VNot _     | VAnd _    | VOr _
    | VChar _   | VString _  | VInt _    | VMod _
    | VSll _    | VSrl _     | VSra _    | VFloat _
    | VLog _    | VAdd _     | VSub _    | VMul _
    | VDiv _    | VNeg _     | VLt _     | VLeq _
    | VGt _     | VGeq _     | VEq _     | VNeq _ -> cps_val t

(** Complex cps transformation. Complex means that the term contains
    computations. A continuation must also be supplied as
    argument to the transformation, indicating where control is transferred to
    when the computation has finished. *)
and cps_complex cont t =
  match t with

  (* Function application is the only complex expression.
     Optimize the case when either the function or argument is atomic. *)
  | TApp(a,t1,t2) ->
    let wrapopt (a, a') = Some a,a' in
    let f, f' = match t1 with
      | TApp _ -> wrapopt (genvar noidx)
      | _      -> None,cps_atomic t1 in
    let e, e' = match t2 with
      | TApp _ -> wrapopt (genvar noidx)
      | _      -> None,cps_atomic t2 in
    let app = TApp(a,TApp(a,f',cont),e') in
    let inner = match e with
      | None -> app
      | Some(e) -> cps_complex (TLam(na,e,app)) t2 in
    let outer = match f with
      | None -> inner
      | Some(f) -> cps_complex (TLam(na,f,inner)) t1 in
    outer

  (* Everything else is atomic. *)
  | TVar _ | TLam _ | TIf _ | TMatch _ | TVal _ -> TApp(na, cont, cps_atomic t)

(** CPS transforms a term, with the identity function as continuation if it is
    complex*)
let cps tm = match tm with
  | TApp _ -> cps_complex idfun tm
  | _      -> cps_atomic tm
