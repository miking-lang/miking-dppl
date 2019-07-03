(** CPS transformations
    Requirements:
    - All functions must take one extra parameter: a continuation function with
      exactly one parameter
    - A function never "returns" (i.e., it never returns something that is not
      a TApp). Instead, it applies its continuation to its "return value". *)

open Ast
open Utils

(** Reference used for genvar *)
let nextvar = ref 0

(** Generate fresh variable names. Avoids
    clashes by using $ as first char (not allowed in lexer for vars).  Takes a
    debruijn index as argument (for idfun). *)
let genvar i =
  let res = !nextvar in
  let str = "$" ^ string_of_int res in
  nextvar := res + 1;
  (str,mkvar ~x:str ~i:i)

(** The identity function (with proper debruijn index) as a continuation. *)
let idfun = let var,var' = genvar 0 in mkcont ~x:var ~t1:var'

(** Wrap direct-style functions in CPS *)
let cps_fun t arity =
  let vars = List.map genvar (replicate arity noidx) in
  let inner = List.fold_left
      (fun acc (_, v') ->
         mkapp ~t1:acc ~t2:v')
      t vars in
  List.fold_right
    (fun (v, _) acc ->
       let k, k' = genvar noidx in
       mklam ~x:k
         ~t1:(mklam ~x:v
                ~t1:(mkapp ~t1:k' ~t2:acc)))
    vars inner

(** Default CPS transformation of values, based on their arities. *)
let cps_val t = match t with
  | T{t=TVal{v};_} -> cps_fun t (arity v)
  | _ -> failwith "cps_val of non-constant"

(** CPS transformation of atomic terms (terms without computation).
    Transforming atomic terms means that we can perform the CPS transformation
    without supplying a continuation. *)
let rec cps_atomic (T{t=t';_} as t) = match t' with

  (* Variables do not require any action *)
  | TVar _ -> t

  (* Function application is never atomic. *)
  | TApp _ -> failwith "Application term in cps_atomic"

  (* Lambdas.
     Wrap in continuation and transform inner using the continuation. *)
  | TLam{x;t1;_} ->
    let k, k' = genvar noidx in
    mklam ~x:k
      ~t1:(mklam ~x:x ~t1:(cps_complex k' t1))

  (* If expressions
     Wrap in continuation and transform inner using the continuation. *)
  | TIf{t1;t2} ->
    let k, k' = genvar noidx in
    mklam ~x:k
      ~t1:(mkif ~t1:(cps_complex k' t1) ~t2:(cps_complex k' t2))

  (* Pattern matching
     Wrap in continuation and transform inner using the continuation. *)
  | TMatch{cls;_} ->
    let k, k' = genvar noidx in
    let cls = List.map (fun (p,te) -> p,cps_complex k' te) cls in
    mklam ~x:k
      ~t1:(mkmatch ~cls)

  (* Continuations should never occur here *)
  | TCont _ -> failwith "Continuation in cps_atomic"

  (* Values *)
  | TVal{v=V{v;_}} -> match v with

    (* Should not exist before eval *)
    | VLam  _  -> failwith "Closure in cps_atomic"
    | VIf _    -> failwith "Closure in cps_atomic"
    | VMatch _ -> failwith "Closure in cps_atomic"
    | VCont  _ -> failwith "Closure in cps_atomic"

    (* Fixpoint. Treat similar as constant function with a single argument. We
       need to apply the id function to the argument before applying fix, since
       the argument expects a continuation as first argument.
       TODO Correct? Seems to work fine *)
    | VFix ->
      let v, v' = genvar noidx in
      let k, k' = genvar noidx in
      let inner = mkapp ~t1:t ~t2:(mkapp ~t1:v' ~t2:idfun) in
      mklam ~x:k
        ~t1:(mklam ~x:v
               ~t1:(mkapp ~t1:k' ~t2:inner))

    (* Probabilistic constructs natively in CPS by design (also the reason why
       we are performing the CPS transformation in the first place) *)
    | VSample _ -> t
    | VWeight _ -> t
    | VResamp _ -> t

    (* Other values *)
    | VDist _
    | VRec _  | VRecProj _ | VTup _    | VTupProj _
    | VList _ | VUtest _   | VLogPdf _
    | VUnit   | VConcat _  | VCons _
    | VBool _ | VNot       | VAnd _    | VOr _
    | VChar _ | VString _  | VInt _    | VMod _
    | VSll _  | VSrl _     | VSra _    | VFloat _
    | VLog    | VAdd _     | VSub _    | VMul _
    | VDiv _  | VNeg       | VLt _     | VLeq _
    | VGt _   | VGeq _     | VEq _     | VNeq _ -> cps_val t

(** Complex cps transformation. Complex means that the term contains
    computations. A continuation must also be supplied as
    argument to the transformation, indicating where control is transferred to
    when the computation has finished. *)
and cps_complex cont (T{t=t';_} as t) =
  match t' with

  (* Function application is the only complex expression.
     Optimize the case when either the function or argument is atomic. *)
  | TApp{t1;t2} ->
    let wrapopt (a,a') = Some a,a' in
    let f, f' = match t1 with
      | T{t=TApp _;_} -> wrapopt (genvar noidx)
      | _      -> None,cps_atomic t1 in
    let e, e' = match t2 with
      | T{t=TApp _;_}-> wrapopt (genvar noidx)
      | _      -> None,cps_atomic t2 in
    let app = mkapp ~t1:(mkapp ~t1:f' ~t2:cont) ~t2:e' in
    let inner = match e with
      | None -> app
      | Some(e) -> cps_complex (mkcont ~x:e ~t1:app) t2 in
    let outer = match f with
      | None -> inner
      | Some(f) -> cps_complex (mkcont ~x:f ~t1:inner) t1 in
    outer

  (* Continuations should never occur here *)
  | TCont _ -> failwith "Continuation in cps_complex"

  (* Everything else is atomic. *)
  | TVar _ | TLam _ | TIf _ | TMatch _ | TVal _ ->
    mkapp ~t1:cont ~t2:(cps_atomic t)

(** CPS transforms a term, with the identity function as continuation if it is
    complex*)
let cps (T{t=t';_} as t) = match t' with
  | TApp _ -> cps_complex idfun t
  | _      -> cps_atomic t

