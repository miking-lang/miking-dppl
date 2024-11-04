-- Nested dual number Interface base on:
-- Siskind, Jeffrey Mark, and Barak A. Pearlmutter. “Nesting Forward-Mode AD in
-- a Functional Framework.” Higher-Order and Symbolic Computation 21, no. 4
-- (December 1, 2008): 361–76. https://doi.org/10.1007/s10990-008-9037-1.

include "utest.mc"

include "mexpr/ast.mc"
include "mexpr/const-arity.mc"
include "mexpr/const-types.mc"
include "mexpr/eq.mc"
include "mexpr/type.mc"
include "mexpr/unify.mc"

include "coreppl.mc"
include "dist.mc"


lang DualNumAst =
  ConstArity + ConstSideEffectBase + ConstCFA + ConstPrettyPrint + Unify

  -- ┌───────────────────────────────────────┐
  -- │ Lifted constants and Dual number API  │
  -- └───────────────────────────────────────┘

  syn Const =
  -- Represents a version of a constant lifted to dual numbers.
  | CLifted Const

  -- Generates a unique tag ε.
  | CGenEpsilon {}

  -- Returns ε₁ < ε₂ for two tags.
  | CLtEpsilon {}

  -- Boxes a float as a dual number, i.e.,
  -- `createPrimal (x : Float) : Dual Float`.
  | CCreatePrimal {}

  -- `createDual ε (x : Dual Float) (x' : Dual Float)` creates a dual
  -- number x+εx' with tag `ε`, primal `x`, and pertubation `x'`.
  | CCreateDual {}

  -- `isDualNum n` returns `false` if `n` is a boxed float and `true` if `n` is
  -- a dual number with a primal and pertubation.
  | CIsDualNum {}

  -- `epsilon (n : Dual Float)` returns the tag ε from n = x+εx'. Runtime error
  -- of if `n` is a boxed float.
  | CEpsilon {}

  -- `primal ε₁ (n : Dual Float)` returns x if n = x+ε₂x' and ε₁≤ε₂, otherwise
  -- it returns `n`
  | CPrimal {}

  -- `primalRec (n : Dual Float) : Float` returns x in n = x+ε₁(x+ε₂(x+₃(...)))
  -- and unboxes it to a float. I.e., it returns the innermost primal of a
  -- nested dual number.
  | CPrimalRec {}

  -- `unboxPrimalExn (n : Dual Float)` unboxes a dual number to a float and
  -- gives a runtime error of the dual number has a pertubation.
  | CUnboxPrimalExn {}

  -- `pertubation ε₁ (n : Dual Float)` returns x' if n = x+ε₂x' and ε₁≤ε₂,
  -- otherwise it returns `0` boxed as a dual number.
  | CPertubation {}


  -- ┌──────────────┐
  -- │ AST builders │
  -- └──────────────┘

  sem dualnumGenEpsilon : Info -> Expr
  sem dualnumGenEpsilon =| info ->
    let i = withInfo info in
    i (appf1_ (i (uconst_ (CGenEpsilon ()))) (i unit_))

  sem dualnumLtEpsilon : Info -> Expr -> Expr -> Expr
  sem dualnumLtEpsilon info e1 =| e2 ->
    let i = withInfo info in
    i (appf2_ (i (uconst_ (CLtEpsilon ()))) e1 e2)

  sem dualnumCreatePrimal : Info -> Expr -> Expr
  sem dualnumCreatePrimal info =| x ->
    let i = withInfo info in
    i (appf1_ (i (uconst_ (CCreatePrimal ()))) x)

  sem dualnumCreateDual : Info -> Expr -> Expr -> Expr -> Expr
  sem dualnumCreateDual info e x =| xp ->
    let i = withInfo info in
    i (appf3_ (i (uconst_ (CCreateDual ()))) e x xp)

  sem dualnumIsDualNum : Info -> Expr -> Expr
  sem dualnumIsDualNum =| info ->
    let i = withInfo info in
    appf1_ (i (uconst_ (CIsDualNum ())))

  sem dualnumEpsilon : Info -> Expr -> Expr
  sem dualnumEpsilon info =| x ->
    let i = withInfo info in
    i (appf1_ (i (uconst_ (CEpsilon ()))) x)

  sem dualnumPrimal : Info -> Expr -> Expr -> Expr
  sem dualnumPrimal info e =| x ->
    let i = withInfo info in
    i (appf2_ (i (uconst_ (CPrimal ()))) e x)

  sem dualnumPrimalRec : Info -> Expr -> Expr
  sem dualnumPrimalRec info =| x ->
    let i = withInfo info in
    i (appf1_ (i (uconst_ (CPrimalRec ()))) x)

  sem dualnumUnboxPrimalExn : Info -> Expr -> Expr
  sem dualnumUnboxPrimalExn info =| x ->
    let i = withInfo info in
    i (appf1_ (i (uconst_ (CUnboxPrimalExn ()))) x)

  sem dualnumPertubationExn : Info -> Expr -> Expr -> Expr
  sem dualnumPertubationExn info e =| x ->
    let i = withInfo info in
    i (appf2_ (i (uconst_ (CPertubation ()))) e x)


  -- ┌───────────────────────────────────────────┐
  -- │ Type directed dual number transformations │
  -- └───────────────────────────────────────────┘

  -- Returns a function that boxes an expression into a dual number based on its
  -- type, if it needs unboxing. Otherwise it returns `None ()`. The difference
  -- compared to lifting is that pertubations are not allowed to propagate
  -- through (un)boxed expressions. In particular applying a boxed function to a
  -- dual number with a pertubation will result in a runtime error.
  sem dualnumBoxTypeDirected : Type -> Option (Expr -> Expr)
  sem dualnumBoxTypeDirected =
  | ty -> _convTypeDirected
           (lam tm. dualnumCreatePrimal (infoTm tm) tm)
           (lam tm. dualnumUnboxPrimalExn (infoTm tm) tm)
           ty

  -- Similar to `dualnumBoxTypeDirected` but instead unboxes
  sem dualnumUnboxTypeDirected : Type -> Option (Expr -> Expr)
  sem dualnumUnboxTypeDirected =
  | ty -> _convTypeDirected
           (lam tm. dualnumUnboxPrimalExn (infoTm tm) tm)
           (lam tm. dualnumCreatePrimal (infoTm tm) tm)
           ty

  -- `dualnumPertubationTypeDirectedExn ty`, based on the type `ty`, returns a
  -- function f whose first argument should be a dual number epsilon tag `e` and
  -- the second argument should be a term `tm` of type `ty` before it is lifted
  -- to dual numbers. The resulting expression recursivly retrieves the
  -- pertubation of all dual numbers in the term `tm`, given `e`.
  --
  -- Examples:
  -- `dualnumPertubationTypeDirectedExn Float e x` = `pertubation e x`
  -- `dualnumPertubationTypeDirectedExn (Float, Float) e x` =
  --   `(pertubation e x.0, pertubation e x.1)`
  -- `dualnumPertubationTypeDirectedExn (Float, [Float]) e x` =
  --   `(pertubation e x.0, map (pertubation e) x.1)`
  --
  -- If the given type `ty` is not first order and does not contain `Float` in
  -- its leafs, `None ()` is returned.
  sem dualnumPertubationTypeDirectedExn : Type -> Option (Expr -> Expr -> Expr)
  sem dualnumPertubationTypeDirectedExn =
  | TyFloat _ -> Some (lam e. lam tm. dualnumPertubationExn (infoTm tm) e tm)
  | TySeq r ->
    optionMap
      (lam f. lam e. _mapSeqTm (f e))
      (dualnumPertubationTypeDirectedExn r.ty)
  | TyRecord r ->
    optionMap
      (lam ts. lam e. _mapRecordTm (map (lam t. (t.0, t.1 e)) ts))
      (optionMapM
         (lam t. match t with (key, ty) in
               optionMap
                 (lam f. (key, f))
                 (dualnumPertubationTypeDirectedExn ty))
         (map (lam t. (sidToString t.0, t.1)) (mapBindings r.fields)))
  | TyAlias r -> dualnumPertubationTypeDirectedExn r.content
  | _ -> None ()

  -- Similar to `dualnumPertubationTypeDirectedExn` but instead creates
  -- aggregated dual numbers based on a type.
  sem dualnumCreateDualTypeDirected : Type -> Option (Expr -> Expr -> Expr -> Expr)
  sem dualnumCreateDualTypeDirected =
  | TyFloat _ -> Some (lam e. lam x. lam xp. dualnumCreateDual (infoTm xp) e x xp)
  | TySeq r ->
    optionMap
      (lam f. lam e. _map2SeqTm (f e))
      (dualnumCreateDualTypeDirected r.ty)
  | TyRecord r ->
    optionMap
      (lam ts. lam e. _map2RecordTm (map (lam t. (t.0, t.1 e)) ts))
      (optionMapM
         (lam t. match t with (key, ty) in
               optionMap
                 (lam f. (key, f))
                 (dualnumCreateDualTypeDirected ty))
         (map (lam t. (sidToString t.0, t.1)) (mapBindings r.fields)))
  | TyAlias r -> dualnumCreateDualTypeDirected r.content
  | _ -> None ()


  -- ┌─────────┐
  -- │ Helpers │
  -- └─────────┘

  sem _mapSeqTm f =| tm ->
    let i = withInfo (infoTm tm) in
    let _x = nameSym "x" in
    i (map_ (i (nulam_ _x (f (i (nvar_ _x))))) tm)

  sem _map2SeqTm f tm1 =| tm2 ->
    let i = withInfo (infoTm tm1) in
    let _x = nameSym "x" in
    let _i = nameSym "i" in
    i (mapi_
         (i (nulams_ [_i, _x] (f (i (nvar_ _x)) (get_ tm2 (nvar_ _i)))))
         tm1)

  sem _mapRecordTm fs =| tm ->
    let info = infoTm tm in
    let i = withInfo info in
    tmRecord info (TyUnknown { info = info })
      (map (lam t. match t with (key, f) in (key, f (i (recordproj_ key tm)))) fs)

  sem _map2RecordTm fs tm1 =| tm2 ->
    let info = infoTm tm1 in
    let i = withInfo info in
    tmRecord info (TyUnknown { info = info })
      (map (lam t. match t with (key, f) in
                 (key, f (i (recordproj_ key tm1)) (i (recordproj_ key tm2))))
         fs)

  sem _hasFloatLeaf =| ty -> _hasFloatLeafH false ty

  sem _hasFloatLeafH acc =
  | TyFloat _ -> true
  | ty -> sfold_Type_Type _hasFloatLeafH acc ty

  sem _convTypeDirected from to =
  | TyFloat _ -> Some from
  | TySeq r -> optionMap _mapSeqTm (_convTypeDirected from to r.ty)
  | ty & TyRecord r ->
    let bs =
      (map
         (lam t. (sidToString t.0,
                optionGetOr (lam x. x) (_convTypeDirected from to t.1)))
         (mapBindings r.fields))
    in
    if not (_hasFloatLeaf ty) then None ()
    else Some (_mapRecordTm bs)
  | TyArrow r ->
    let f = lam from. lam to.
      let _x = nameSym "x" in Some (
        lam tm.
          let i = withInfo (infoTm tm) in
          i (nulam_ _x (to (i (appf1_ tm (from (i (nvar_ _x))))))))
    in
    switch (_convTypeDirected to from r.from, _convTypeDirected from to r.to)
    case (Some from, Some to) then f from to
    case (Some from, None _) then f from (lam x. x)
    case (None _, Some to) then f (lam x. x) to
    case (None _, None _) then None ()
    end
  | TyAll r -> _convTypeDirected from to r.ty
  | TyAlias r -> _convTypeDirected from to r.content
  | _ -> None ()


  -- ┌───────────────────────────────────────────────┐
  -- │ Extension to const related semantic functions │
  -- └───────────────────────────────────────────────┘

  sem constArity =
  | CLifted const -> constArity const
  | CGenEpsilon _
  | CCreatePrimal _
  | CIsDualNum _
  | CEpsilon _
  | CPrimalRec _
  | CUnboxPrimalExn _ -> 1
  | CLtEpsilon _ | CPrimal _ | CPertubation _ -> 2
  | CCreateDual _ -> 3

  sem constHasSideEffect =
  | CLifted const -> constHasSideEffect const
  | CGenEpsilon _ -> true
  | CLtEpsilon _
  | CCreatePrimal _
  | CCreateDual _
  | CIsDualNum _
  | CEpsilon _
  | CPrimal _
  | CPrimalRec _
  | CUnboxPrimalExn _
  | CPertubation _ -> false

  sem generateConstraintsConst graph info ident =
  | CLifted const -> generateConstraintsConst graph info ident const
  | CGenEpsilon _
  | CLtEpsilon _
  | CCreatePrimal _
  | CCreateDual _
  | CIsDualNum _
  | CEpsilon _
  | CPrimal _
  | CPrimalRec _
  | CUnboxPrimalExn _
  | CPertubation _ -> graph

  sem getConstStringCode (indent : Int) =
  | CLifted const -> join ["L(", getConstStringCode indent const, ")"]
  | CGenEpsilon _ -> "genEpsilon"
  | CLtEpsilon _ -> "ltEpsilon"
  | CCreatePrimal _ -> "createPrimal"
  | CCreateDual _ -> "createDual"
  | CIsDualNum _ -> "isDualNum"
  | CEpsilon _ -> "epsilon"
  | CPrimal _ -> "primal"
  | CPrimalRec _ -> "primalRec"
  | CUnboxPrimalExn _ -> "unboxPrimalExn"
  | CPertubation _ -> "pertubation"


  -- ┌───────────────────────────────────────────┐
  -- │ Lifted types and types of dual number API │
  -- └───────────────────────────────────────────┘

  syn Type =
  | TyDualNum { info : Info }

  sem itydualnum_ =| info -> TyDualNum { info = NoInfo () }
  sem tydualnum_ =| () -> itydualnum_ ( NoInfo ())

  sem infoTy =
  | TyDualNum r -> r.info

  sem tyWithInfo (info : Info) =
  | TyDualNum r -> TyDualNum { r with info = info }

  sem getTypeStringCode (indent : Int) (env : PprintEnv) =
  | TyDualNum _ -> (env, "DualNum")

  sem unifyBase u env =
  | (TyDualNum _, TyDualNum _) -> u.empty

  -- ┌───────────────────┐
  -- │ Lifting interface │
  -- └───────────────────┘

  -- `dualnumLiftType ty` lifts a type to dual numbers.
  sem dualnumLiftType : Type -> Type

  -- `dualnumLiftExpr tm` lifts the term `tm` to dual numbers. Any term that is
  -- lifted using the dual number API above is independent of the runtime.
  sem dualnumLiftExpr : Expr -> Expr
  sem dualnumLiftExpr =| tm ->
    -- First lift terms
    let tm = dualnumLiftExprH tm in
    -- Then lift type labels
    recursive let inner = lam tm.
      match tm with TmExt r then TmExt { r with inexpr = inner r.inexpr }
      else smap_Expr_Expr inner (smap_Expr_Type dualnumLiftType tm)
    in
    inner tm

  -- `dualnumLiftExpr tm` lifts the term `tm` to dual numbers. Any term that is
  -- lifted using the dual number API above is independent of the runtime.
  sem dualnumLiftExprH : Expr -> Expr
end

lang DualNumDist = Dist + DualNumAst
  syn Dist =
  | DDual Dist

  sem smapAccumL_Dist_Expr f acc =
  | DDual d ->
    match smapAccumL_Dist_Expr f acc d with (acc, d) in
    (acc, DDual d)

  sem distTy info =
  | DDual d ->
    match distTy info d with (vars, paramTys, ty) in
    -- NOTE(oerikss, 2024-11-04): We lift the support and instead make sure that
    -- TmDist unboxes its parameters.
    (vars, paramTys, dualnumLiftType ty)

  sem distName =
  | DDual d -> join ["Dual<", distName d, ">"]
end


lang DualNumRuntimeBase = DualNumAst

  -- ┌──────────────────────────────────────────────────────┐
  -- │ Interface for runtime implementation of dual numbers │
  -- └──────────────────────────────────────────────────────┘

  -- The environment is decied by the runtime implementation.
  syn DualNumRuntimeEnv =

  -- `dualnumTransformAPIConst env tm const` provides runtime implementations
  -- for the dual number API and constant functions lifted to dual numbers.
  sem dualnumTransformAPIConst : DualNumRuntimeEnv -> Expr -> Const -> Expr

  -- `dualnumTransformAPIConst env tm const` provides a runtime implementation
  -- for the dual number type.
  sem dualnumTransformTypeAPI : DualNumRuntimeEnv -> Type -> Type
  sem dualnumTransformTypeAPI env =| ty ->
    smap_Type_Type (dualnumTransformTypeAPI env) ty


  -- `dualnumTransformAPIExpr env tm` replaces the dual number API and constants
  -- lifted to dual numbers in `tm` with their runtime implementations.
  sem dualnumTransformAPIExpr : DualNumRuntimeEnv -> Expr -> Expr
  sem dualnumTransformAPIExpr env = --
  | tm & TmConst r -> dualnumTransformAPIConst env tm r.val
  | tm ->
    let tm = smap_Expr_Expr (dualnumTransformAPIExpr env) tm in
    (smap_Expr_Type (dualnumTransformTypeAPI env) tm)
end


lang DualNumLift =
  MExprPPL + DualNumAst + ElementaryFunctions + Diff + DualNumDist + TyConst

  sem tyConstBase d =
  -- NOTE(oerikss, 2024-04-25): The type of a lifted constant is its type lifted
  -- to dual numbers. Lifting of the expectation intrinsic is handled by the
  -- runtime implementation when it is applied to a distribution so we type it
  -- as lifted.
  | CDistExpectation _ | CLifted const -> dualnumLiftType (tyConstBase d const)
  | CGenEpsilon _ -> tyarrows_ [tyunit_, tyint_]
  | CLtEpsilon _ -> tyarrows_ [tyint_, tyint_]
  | CCreatePrimal _ -> tyarrows_ [tyfloat_, tydualnum_ ()]
  | CCreateDual _ -> let ty = tydualnum_ () in tyarrows_ [tyint_, ty, ty, ty]
  | CIsDualNum _ -> tyarrows_ [tydualnum_ (), tybool_]
  | CEpsilon _ -> tyarrows_ [tydualnum_ (), tyint_]
  | CPrimal _ | CPertubation _ ->
    let ty = tydualnum_ () in tyarrows_ [tyint_, ty, ty]
  | CPrimalRec _ | CUnboxPrimalExn _ -> tyarrows_ [tydualnum_ (), tyfloat_]

  -- ┌──────────────────────────────────────┐
  -- │ Lift types and terms to dual numbers │
  -- └──────────────────────────────────────┘

  sem dualnumLiftType =
  | TyFloat r -> TyDualNum r
  | ty -> smap_Type_Type dualnumLiftType ty

  -- `dualnumLiftExprH tm` lifts the term `tm` to dual numbers. Any term that is
  -- lifted using the dual number API above is independent of the runtime.
  -- However, most elementary functions are mutually recursive so it is more
  -- convenient to lift them in the runtime and then simply refer to its lifted
  -- implementation in this semantic function.
  sem dualnumLiftExprH =
  | TmDiff r ->
    match r.ty with TyArrow tyr then
      optionMapOrElse
        (lam.
          errorSingle [r.info]
            (join [
              "Cannot differentiate functions with this return type: ",
              type2str tyr.to
            ]))
        (lam pertubation.
          optionMapOrElse
            (lam.
              errorSingle [r.info]
                (join [
                  "Cannot differentiate functions with this parameter type: ",
                  type2str tyr.from
                ]))
            (lam createDual.
              let i = withInfo r.info in
              let e_ = nameSym "e" in
              let x_ = nameSym "x" in
              let v_ = nameSym "v" in
              let d_ = nameSym "d" in
              let fn = dualnumLiftExprH r.fn in
              let arg = dualnumLiftExprH r.arg in
              bind_
                (i (nulet_ e_ (dualnumGenEpsilon r.info)))
                (nulam_ v_
                   (bindall_ [
                     i (nulet_ x_ arg),
                     i (nulet_ d_
                          (i (appf1_ fn
                                (createDual (i (nvar_ e_))
                                   (i (nvar_ x_)) (i (nvar_ v_)))))),
                     pertubation (i (nvar_ e_)) (i (nvar_ d_))
                   ])))
            (dualnumCreateDualTypeDirected tyr.from))
        (dualnumPertubationTypeDirectedExn tyr.to)
    else error "impossible"
  | TmExt r ->
    optionMapOrElse
      (lam. TmExt { r with inexpr = dualnumLiftExprH r.inexpr })
      (lam box.
        let id1 = nameSetNewSym r.ident in
        -- NOTE(oerikss, 2024-04-16): We use an intermediate eta expanded alias
        -- because externals needs to be fully applied.
        let id2 = nameSetNewSym r.ident in
        let i = withInfo r.info in
        TmExt {
          r with
          ident = id2,
          inexpr =
            bindall_ [
              i (nulet_ id1
                   (let ps = create (arityFunType r.tyIdent) (lam. nameSym "p") in
                    foldl
                      (lam body. lam p. i (nulam_ p body))
                      (foldr
                         (lam p. lam fn. i (appf1_ fn p))
                         (nvar_ id2)
                         (map (lam p. i (nvar_ p)) ps))
                      ps)),
              i (nulet_ r.ident (box (i (nvar_ id1)))),
              dualnumLiftExprH r.inexpr
            ]
        })
      (dualnumBoxTypeDirected r.tyIdent)
  | TmDist (r & { dist = ! DEmpirical _ }) ->
    smap_Expr_Expr
      (lam tm.
        optionMapOrElse
          (lam. dualnumLiftExprH tm)
          (lam unbox. unbox (dualnumLiftExprH tm))
          (dualnumUnboxTypeDirected (tyTm tm)))
      (TmDist { r with dist = DDual r.dist })
  | TmWeight r -> TmWeight {
    r with weight = dualnumPrimalRec r.info (dualnumLiftExprH r.weight)
  }
  | TmInfer r ->
    let method =
      inferSmap_Expr_Expr
        (lam tm.
          optionMapOrElse
            (lam. dualnumLiftExprH tm)
            (lam unbox. unbox (dualnumLiftExprH tm))
            (dualnumUnboxTypeDirected (tyTm tm)))
        r.method
    in
    TmInfer { r with model = dualnumLiftExprH r.model, method = method }
  | tm & TmConst r -> dualnumLiftConst tm r.val
  | tm -> smap_Expr_Expr dualnumLiftExprH tm

  -- We handle constants in this semantic function. `dualnumLiftConst env tm c`
  -- lifts the constant `c`, where `tm` is the term holding the constant.
  sem dualnumLiftConst : Expr -> Const -> Expr
  sem dualnumLiftConst tm =
  -- Lift elementary functions
  | const & (
    CFloat _
  | CAddf _
  | CMulf _
  | CNegf _
  | CSubf _
  | CDivf _
  | CEqf _
  | CLtf _
  | CLeqf _
  | CGtf _
  | CGeqf _
  | CSin _
  | CCos _
  | CSqrt _
  | CExp _
  | CLog _
  | CPow _)
    -> withInfo (infoTm tm) (uconst_ (CLifted const))
  -- Lift remaining constants based on their type signature
  | const ->
    optionMapOr tm
      (lam box. box tm)
      (dualnumBoxTypeDirected (tyConst const))
end


lang TestLang = DualNumLift + MExprAst + MExprEq + MExprPrettyPrint
  -- We implement som additional constants to make testing easier and more
  -- accurate.

  syn Const =
  | CTmPlaceholder { str : String } -- Represents an arbritrary term

  sem getConstStringCode (indent : Int) =
  | CTmPlaceholder r -> join ["<", r.str, ">"]
end


mexpr

use TestLang in

let createPrimal_ = appf1_ (uconst_ (CCreatePrimal ())) in
let unboxPrimal_ = appf1_ (uconst_ (CUnboxPrimalExn ())) in
let primalRec_ = appf1_ (uconst_ (CPrimalRec ())) in
let pertubation_ = appf2_ (uconst_ (CPertubation ())) in
let createDual_ = appf3_ (uconst_ (CCreateDual ())) in
let tm_ = uconst_ (CTmPlaceholder { str = "term" } ) in
let e_ = uconst_ (CTmPlaceholder { str = "e" } ) in

-- ┌─────────────────────────────┐
-- │ Test dualnumBoxTypeDirected │
-- └─────────────────────────────┘

let _boxPrimal = lam ty.
  optionMapOr tm_ (lam primal. primal tm_)
    (dualnumBoxTypeDirected ty)
in
let failToString = utestDefaultToString expr2str expr2str in

-- Base types

utest _boxPrimal tyfloat_ with createPrimal_ tm_
  using eqExpr else failToString
in

utest _boxPrimal tyint_ with tm_
  using eqExpr else failToString
in

utest _boxPrimal tybool_ with tm_
  using eqExpr else failToString
in

utest _boxPrimal tychar_ with tm_
  using eqExpr else failToString
in

-- Sequences

utest _boxPrimal (tyseq_ tyfloat_) with
  map_ (ulam_ "x" (createPrimal_ (var_ "x"))) tm_
  using eqExpr else failToString
in

utest _boxPrimal (tyseq_ (tyseq_ tyfloat_)) with
  map_ (ulam_ "x" (map_ (ulam_ "x" (createPrimal_ (var_ "x"))) (var_ "x"))) tm_
  using eqExpr else failToString
in

utest _boxPrimal (tyseq_ tyint_) with tm_
  using eqExpr else failToString
in

-- Records

-- NOTE(oerikss, 2024-04-16): We define the record type outside the unit-test so
-- that the label ordering is stable.
let ty = tyrecord_ [
  ("a", tyfloat_),
  ("b", tyint_),
  ("c", tyrecord_ [("d", tyfloat_), ("e", tyint_)])
] in
utest
  _boxPrimal ty
  with
  urecord_ [
    ("a", createPrimal_ (recordproj_ "a" tm_)),
    ("b", recordproj_ "b" tm_),
    ("c", urecord_ [
      ("d", createPrimal_ (recordproj_ "d" (recordproj_ "c" tm_))),
      ("e", recordproj_ "e" (recordproj_ "c" tm_))
    ])
  ]
  using eqExpr else failToString
in

-- Functions

utest _boxPrimal (tyarrow_ tyfloat_ tyfloat_) with
  ulam_ "x" (createPrimal_ (appf1_ tm_ (unboxPrimal_ (var_ "x"))))
  using eqExpr else failToString
in

utest _boxPrimal (tyarrows_ [tyfloat_, tyfloat_, tyfloat_]) with
  ulam_ "x"
    (ulam_ "y"
       (createPrimal_
          (appf2_ tm_ (unboxPrimal_ (var_ "x")) (unboxPrimal_ (var_ "y")))))
  using eqExpr else failToString
in

utest _boxPrimal (tyarrows_ [tyfloat_, tyint_, tyfloat_]) with
  ulam_ "x"
    (ulam_ "y"
       (createPrimal_ (appf2_ tm_ (unboxPrimal_ (var_ "x")) (var_ "y"))))
  using eqExpr else failToString
in

-- ┌────────────────────────────────────────┐
-- │ Test dualnumPertubationTypeDirectedExn │
-- └────────────────────────────────────────┘

let _pertubation = lam ty.
  optionMapOr tm_ (lam pertubation.  pertubation e_ tm_)
    (dualnumPertubationTypeDirectedExn ty)
in

-- Base types

utest _pertubation tyfloat_ with pertubation_ e_ tm_
  using eqExpr else failToString
in

utest _pertubation tyint_ with tm_
  using eqExpr else failToString
in

utest _pertubation tybool_ with tm_
  using eqExpr else failToString
in

utest _pertubation tychar_ with tm_
  using eqExpr else failToString
in

-- Sequences

utest _pertubation (tyseq_ tyfloat_) with
  map_ (ulam_ "x" (pertubation_ e_ (var_ "x"))) tm_
  using eqExpr else failToString
in

utest _pertubation (tyseq_ (tyseq_ tyfloat_)) with
  map_ (ulam_ "x" (map_ (ulam_ "x" (pertubation_ e_ (var_ "x"))) (var_ "x"))) tm_
  using eqExpr else failToString
in

utest _pertubation (tyseq_ tyint_) with tm_
  using eqExpr else failToString
in

-- Records

-- NOTE(oerikss, 2024-04-16): We define the record type outside the unit-test so
-- that the label ordering is stable.
let ty = tyrecord_ [
  ("a", tyfloat_),
  ("b", tyrecord_ [("c", tyfloat_)])
] in
utest
  _pertubation ty
  with
  urecord_ [
    ("a", pertubation_ e_ (recordproj_ "a" tm_)),
    ("b", urecord_ [
      ("c", pertubation_ e_ (recordproj_ "c" (recordproj_ "b" tm_)))
    ])
  ]
  using eqExpr else failToString
in

let ty = tyrecord_ [
  ("a", tyfloat_),
  ("b", tyrecord_ [("c", tyint_)])
] in
utest _pertubation ty with tm_
  using eqExpr else failToString
in


-- ┌────────────────────────────────────┐
-- │ Test dualnumCreateDualTypeDirected │
-- └────────────────────────────────────┘

let tm1_ = uconst_ (CTmPlaceholder { str = "term1" } ) in
let tm2_ = uconst_ (CTmPlaceholder { str = "term2" } ) in

let _createDual = lam ty.
  optionMapOr tm1_ (lam createDual.  createDual e_ tm1_ tm2_)
    (dualnumCreateDualTypeDirected ty)
in

-- Base types

utest _createDual tyfloat_ with createDual_ e_ tm1_ tm2_
  using eqExpr else failToString
in

utest _createDual tyint_ with tm_
  using eqExpr else failToString
in

utest _createDual tybool_ with tm_
  using eqExpr else failToString
in

utest _createDual tychar_ with tm_
  using eqExpr else failToString
in

-- Sequences

-- utest _createDual (tyseq_ tyfloat_) with
--   map_ (ulam_ "x" (pertubation_ e_ (var_ "x"))) tm_
--   using eqExpr else failToString
-- in

-- utest _createDual (tyseq_ (tyseq_ tyfloat_)) with
--   map_ (ulam_ "x" (map_ (ulam_ "x" (pertubation_ e_ (var_ "x"))) (var_ "x"))) tm_
--   using eqExpr else failToString
-- in

-- utest _createDual (tyseq_ tyint_) with tm_
--   using eqExpr else failToString
-- in

()
