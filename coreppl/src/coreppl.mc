-- CorePPL
-- Note that we should NOT implement eval or compile functions for
-- CorePPL. Instead, we implement a function which translates
-- the core terms into an MExpr term. By doing so, we can use the
-- standard eval and compile functions when running the inference.

include "mexpr/ast.mc"
include "mexpr/pprint.mc"
include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "mexpr/mexpr.mc"
include "mexpr/info.mc"
include "mexpr/eq.mc"
include "mexpr/anf.mc"
include "mexpr/type-check.mc"
include "mexpr/type-lift.mc"
include "mexpr/const-arity.mc"
include "mexpr/cfa.mc"

include "peval/peval.mc"

include "string.mc"
include "math.mc"
include "utest.mc"

include "dist.mc"
include "infer-method.mc"
include "ode-solver-method.mc"

-------------
-- HELPERS --
-------------

let _isUnitTy = use RecordTypeAst in lam ty.
  match ty with TyRecord { fields = fields } then mapIsEmpty fields
  else false


--------------------------
-- Elementary Functions --
--------------------------

lang ElementaryFunctions =
  FloatAst +
  TyConst +
  ConstArity +
  ConstPrettyPrint +
  ConstDelta +
  ConstSideEffectBase +
  ConstCFA

  -- NOTE(oerikss, 2024-04-12): We extend constants with additional elementary
  -- functions that are typically implemented as externals in the runtime.
  -- However, we need to treat these as builtin constants in the AD
  -- transformation. In addition they will allow us to better partially
  -- evaluate.
  syn Const =
  | CSin {}
  | CCos {}
  | CSqrt {}
  | CExp {}
  | CLog {}
  | CPow {}

  sem tyConst =
  | CSin _ | CCos _ | CSqrt _ | CExp _ | CLog _ -> tyarrows_ [tyfloat_, tyfloat_]
  | CPow _ -> tyarrows_ [tyfloat_, tyfloat_, tyfloat_]

  sem tyConstBase d =
  | CSin _ | CCos _ | CSqrt _ | CExp _ | CLog _ -> tyarrows_ [tyfloat_, tyfloat_]
  | CPow _ -> tyarrows_ [tyfloat_, tyfloat_, tyfloat_]

  sem constArity =
  | CSin _ | CCos _ | CSqrt _ | CExp _ | CLog _ -> 1
  | CPow _ -> 2

  sem getConstStringCode (indent : Int) =
  | CSin _ -> "sin"
  | CCos _ -> "cos"
  | CSqrt _ -> "sqrt"
  | CExp _ -> "exp"
  | CLog _ -> "log"
  | CPow _ -> "pow"

  sem delta info =
  | (CSin _, [TmConst (cr & {val = CFloat fr})]) ->
    TmConst { cr with val = CFloat { fr with val = sin fr.val }, info = info }
  | (CCos _, [TmConst (cr & {val = CFloat fr})]) ->
    TmConst { cr with val = CFloat { fr with val = cos fr.val }, info = info }
  | (CSqrt _, [TmConst (cr & {val = CFloat fr})]) ->
    TmConst { cr with val = CFloat { fr with val = sqrt fr.val }, info = info }
  | (CExp _, [TmConst (cr & {val = CFloat fr})]) ->
    TmConst { cr with val = CFloat { fr with val = exp fr.val }, info = info }
  | (CLog _, [TmConst (cr & {val = CFloat fr})]) ->
    TmConst { cr with val = CFloat { fr with val = log fr.val }, info = info }
  | (CPow _, [TmConst (cr & {val = CFloat fr1}), TmConst {val = CFloat fr2}]) ->
    TmConst {
      cr with val = CFloat { fr2 with val = pow fr1.val fr2.val }, info = info
    }

  sem constHasSideEffect =
  | CSin _ | CCos _ | CSqrt _ | CExp _ | CLog _ | CPow _ -> false

  sem generateConstraintsConst graph info ident =
  | CSin _ | CCos _ | CSqrt _ | CExp _ | CLog _ | CPow _ -> graph
end


-----------
-- TERMS --
-----------

lang Infer =
  Ast + PrettyPrint + Eq + Sym + Dist + FunTypeAst + TypeCheck + ANF +
  TypeLift + InferMethodBase + PEval

  -- Evaluation of TmInfer returns a TmDist
  syn Expr =
  | TmInfer { model: Expr,
              method: InferMethod,
              ty: Type,
              info: Info }

  sem infoTm =
  | TmInfer t -> t.info

  sem tyTm =
  | TmInfer t -> t.ty

  sem withInfo (info: Info) =
  | TmInfer t -> TmInfer { t with info = info }

  sem withType (ty: Type) =
  | TmInfer t -> TmInfer { t with ty = ty }

  sem smapAccumL_Expr_Expr f acc =
  | TmInfer t ->
    match inferSmapAccumL_Expr_Expr f acc t.method with (acc,method) in
    match f acc t.model with (acc,model) in
    (acc, TmInfer { t with method = method, model = model })

  -- Pretty printing
  sem isAtomic =
  | TmInfer _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmInfer t ->
    let i = pprintIncr indent in
    match pprintInferMethod i env t.method with (env, method) in
    match printParen i env t.model with (env,model) in
    (env, join ["infer", pprintNewline i, method, pprintNewline i, model])

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmInfer r ->
    match lhs with TmInfer l then
      if eqi (constructorTag l.method) (constructorTag r.method) then
        eqExprH env free l.model r.model
      else None ()
    else None ()

  -- Type check
  sem typeCheckExpr (env : TCEnv) =
  | TmInfer t ->
    let model = typeCheckExpr env t.model in
    let method = typeCheckInferMethod env t.info t.method in
    let tyRes = newvar env.currentLvl t.info in
    unify env [infoTm model]
      (ityarrow_ t.info (tyWithInfo t.info tyunit_) tyRes) (tyTm model);
    TmInfer { t with model = model, method = method,
                     ty = TyDist {info = t.info, ty = tyRes} }

  -- ANF
  sem normalize (k : Expr -> Expr) =
  | TmInfer ({ model = model } & t) ->
    normalizeName (lam model. k (TmInfer { t with model = model })) model

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmInfer t ->
    match typeLiftExpr env t.model with (env, model) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmInfer {{t with model = model}
                          with ty = ty})
      else never
    else never

  -- Partial evaluation
  sem pevalBindThis =
  | TmInfer _ -> true

  sem pevalEval ctx k =
  | TmInfer r ->
    pevalBind ctx
      (lam model. k (TmInfer {r with model = model}))
      r.model
end


-- Assume defines a new random variable
lang Assume =
  Ast + Dist + PrettyPrint + Eq + Sym + TypeCheck + ANF + TypeLift + PEval

  syn Expr =
  | TmAssume { dist: Expr,
               ty: Type,
               info: Info }

  sem infoTm =
  | TmAssume t -> t.info

  sem tyTm =
  | TmAssume t -> t.ty

  sem withInfo (info: Info) =
  | TmAssume t -> TmAssume { t with info = info }

  sem withType (ty: Type) =
  | TmAssume t -> TmAssume { t with ty = ty }

  sem smapAccumL_Expr_Expr f acc =
  | TmAssume t ->
    match f acc t.dist with (acc,dist) in
    (acc, TmAssume { t with dist = dist })

  -- Pretty printing
  sem isAtomic =
  | TmAssume _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmAssume t ->
    let i = pprintIncr indent in
    match printParen i env t.dist with (env,dist) then
      (env, join ["assume", pprintNewline i, dist])
    else never

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmAssume r ->
    match lhs with TmAssume l then
      eqExprH env free l.dist r.dist
    else None ()

  -- Type check
  sem typeCheckExpr (env : TCEnv) =
  | TmAssume t ->
    let dist = typeCheckExpr env t.dist in
    let tyRes = newvar env.currentLvl t.info in
    unify env [infoTm dist] (TyDist { info = t.info, ty = tyRes }) (tyTm dist);
    TmAssume {{ t with dist = dist }
                  with ty = tyRes }

  -- ANF
  sem normalize (k : Expr -> Expr) =
  | TmAssume ({ dist = dist } & t) ->
    normalizeName (lam dist. k (TmAssume { t with dist = dist })) dist

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmAssume t ->
    match typeLiftExpr env t.dist with (env, dist) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmAssume {{t with dist = dist}
                           with ty = ty})
      else never
    else never

  -- Partial evaluation
  sem pevalBindThis =
  | TmAssume _ -> true

  sem pevalEval ctx k =
  | TmAssume r ->
    pevalBind ctx
      (lam dist. k (TmAssume {r with dist = dist}))
      r.dist
  | TmAssume ({ dist = TmDist ({ dist = dist } & td) } & t) ->
    pevalDistEval ctx
      (lam dist. k (TmAssume { t with dist = TmDist { td with dist = dist } }))
      dist

end


-- Observe gives a random variable conditioned on a specific value
lang Observe =
  Ast + Dist + PrettyPrint + Eq + Sym + TypeCheck + ANF + TypeLift + PEval

  syn Expr =
  | TmObserve { value: Expr,
                dist: Expr,
                ty: Type,
                info: Info }

  sem infoTm =
  | TmObserve t -> t.info

  sem tyTm =
  | TmObserve t -> t.ty

  sem withInfo (info: Info) =
  | TmObserve t -> TmObserve { t with info = info }

  sem withType (ty: Type) =
  | TmObserve t -> TmObserve { t with ty = ty }

  sem smapAccumL_Expr_Expr f acc =
  | TmObserve t ->
    match f acc t.value with (acc,value) in
    match f acc t.dist with (acc,dist) in
    (acc, TmObserve {{ t with value = value } with dist = dist})

  -- Pretty printing
  sem isAtomic =
  | TmObserve _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmObserve t ->
    let i = pprintIncr indent in
    match printArgs i env [t.value, t.dist] with (env,args) then
      (env, join ["observe", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmObserve r ->
    match lhs with TmObserve l then
      match eqExprH env free l.value r.value with Some free then
        eqExprH env free l.dist r.dist
      else None ()
    else None ()

  -- Type check
  sem typeCheckExpr (env : TCEnv) =
  | TmObserve t ->
    let value = typeCheckExpr env t.value in
    let dist = typeCheckExpr env t.dist in
    let tyDistRes = newvar env.currentLvl t.info in
    unify env [infoTm dist]
      (TyDist { info = t.info, ty = tyDistRes }) (tyTm dist);
    unify env [infoTm value] tyDistRes (tyTm value);
    TmObserve {{{ t with value = value }
                    with dist = dist }
                    with ty = tyWithInfo t.info tyunit_ }

  -- ANF
  sem normalize (k : Expr -> Expr) =
  | TmObserve ({ value = value, dist = dist } & t) ->
    normalizeName
      (lam value.
        normalizeName
          (lam dist.
            k (TmObserve {{ t with value = value }
                              with dist = dist }))
          dist)
      value

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmObserve t ->
    match typeLiftExpr env t.value with (env, value) then
      match typeLiftExpr env t.dist with (env, dist) then
        match typeLiftType env t.ty with (env, ty) then
          (env, TmObserve {{{ t with value = value }
                                with dist = dist }
                                with ty = ty })
        else never
      else never
    else never

  -- Partial evaluation
  sem pevalBindThis =
  | TmObserve _ -> true

  sem pevalEval ctx k =
  | TmObserve r ->
    pevalBind ctx
      (lam value.
        pevalBind ctx
          (lam dist. k (TmObserve {r with value=value, dist = dist}))
          r.dist)
      r.value
  | TmObserve ({ value = value, dist = TmDist ({ dist = dist } & td) } & t) ->
    pevalBind ctx
      (lam value.
        pevalDistEval ctx
          (lam dist.
             k (TmObserve {{ t with value = value }
                               with dist = TmDist { td with dist = dist}}))
          dist)
      value

  sem exprHasSideEffectH env lambdaCounting acc =
  | TmObserve _ -> true

end

-- Defines a weight term
lang Weight =
  Ast + PrettyPrint + Eq + Sym + TypeCheck + FloatTypeAst + ANF + TypeLift +
  PEval + Dist
  syn Expr =
  | TmWeight { weight: Expr, ty: Type, info: Info }

  sem infoTm =
  | TmWeight t -> t.info

  sem tyTm =
  | TmWeight t -> t.ty

  sem withInfo (info: Info) =
  | TmWeight t -> TmWeight { t with info = info }

  sem withType (ty: Type) =
  | TmWeight t -> TmWeight { t with ty = ty }

  sem smapAccumL_Expr_Expr f acc =
  | TmWeight t ->
    match f acc t.weight with (acc,weight) in
    (acc, TmWeight { t with weight = weight })

  -- Pretty printing
  sem isAtomic =
  | TmWeight _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmWeight t ->
    let i = pprintIncr indent in
    match printParen i env t.weight with (env,dist) then
      (env, join ["weight", pprintNewline i, dist])
    else never

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmWeight r ->
    match lhs with TmWeight l then
      eqExprH env free l.weight r.weight
    else None ()

  -- Type check
  sem typeCheckExpr (env : TCEnv) =
  | TmWeight t ->
    let weight = typeCheckExpr env t.weight in
    unify env [infoTm weight] (TyFloat { info = t.info }) (tyTm weight);
    TmWeight {{ t with weight = weight }
                  with ty = tyWithInfo t.info tyunit_ }

  -- ANF
  sem normalize (k : Expr -> Expr) =
  | TmWeight ({ weight = weight } & t) ->
    normalizeName (lam weight. k (TmWeight { t with weight = weight })) weight

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmWeight t ->
    match typeLiftExpr env t.weight with (env, weight) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmWeight {{ t with weight = weight }
                            with ty = ty })
      else never
    else never

  -- Partial evaluation
  sem pevalBindThis =
  | TmWeight _ -> true

  sem pevalEval ctx k =
  | TmWeight r ->
    pevalBind ctx
      (lam weight. k (TmWeight {r with weight = weight}))
      r.weight

  sem exprHasSideEffectH env lambdaCounting acc =
  | TmWeight _ -> true

end


-- Translations in between weight and observe terms
lang ObserveWeightTranslation = Observe + Weight
/-
  -- Translates ALL observe terms into weight terms.
  sem observeToWeight =
  | TmObserve -> unit_ -- TODO

  -- Translates SOME weight terms into observe terms.
  sem weightToObserve =
  | TmWeight -> unit_ -- TODO
-/
end


lang SolveODE =
  Ast + Dist + PrettyPrint + Eq + Sym + TypeCheck + ANF + TypeLift + PEval +
  ODESolverMethod

  syn Expr =
  | TmSolveODE { method: ODESolverMethod,
                 model: Expr,
                 init: Expr,
                 endTime: Expr,
                 ty: Type,
                 info: Info }

  sem infoTm =
  | TmSolveODE t -> t.info

  sem tyTm =
  | TmSolveODE t -> t.ty

  sem withInfo (info: Info) =
  | TmSolveODE t -> TmSolveODE { t with info = info }

  sem withType (ty: Type) =
  | TmSolveODE t -> TmSolveODE { t with ty = ty }

  sem smapAccumL_Expr_Expr f acc =
  | TmSolveODE t ->
    match
      odeSolverMethodSmapAccumL_Expr_Expr f acc t.method with (acc, method)
    in
    match f acc t.model with (acc, model) in
    match f acc t.init with (acc, init) in
    match f acc t.endTime with (acc, endTime) in
    (acc, TmSolveODE {
      t with method = method, model = model, init = init, endTime = endTime
    })

  -- Pretty printing
  sem isAtomic =
  | TmSolveODE _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmSolveODE t ->
    let i = pprintIncr indent in
    match printParen i env t.model with (env, model) in
    match printParen i env t.init with (env, init) in
    match printParen i env t.endTime with (env, endTime) in
    (env, join [
      "solveode",
      pprintNewline i, model,
      pprintNewline i, init,
      pprintNewline i, endTime
    ])

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmSolveODE r ->
    match lhs with TmSolveODE l then
      optionFoldlM (lam free. uncurry (eqExprH free env)) free
        [(l.model, r.model), (l.init, r.init), (l.endTime, r.endTime)]
    else None ()

  -- Type check
  sem typeCheckExpr (env : TCEnv) =
  | TmSolveODE t ->
    let model = typeCheckExpr env t.model in
    let init = typeCheckExpr env t.init in
    let endTime = typeCheckExpr env t.endTime in
    let tyModel = newvar env.currentLvl t.info in
    let tyfloat_ = ityfloat_ t.info in
    let tyseq_ = ityseq_ t.info tyfloat_ in
    unify env [infoTm model]
      (ityarrow_ t.info tyfloat_ (ityarrow_ t.info tyseq_ tyseq_))
      (tyTm model);
    unify env [infoTm init] tyseq_ (tyTm init);
    unify env [infoTm endTime] tyfloat_ (tyTm endTime);
    TmSolveODE { t with
                 model = model,
                 init = init,
                 endTime = endTime,
                 ty = tyseq_ }

  -- ANF
  sem normalize (k : Expr -> Expr) =
  | TmSolveODE t ->
    normalizeName (lam endTime.
      normalizeName (lam init.
        normalizeName (lam model.
          k (TmSolveODE { t with
                          model = model, init = init, endTime = endTime }))
          t.model)
        t.init)
      t.endTime

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmSolveODE t ->
    match typeLiftExpr env t.model with (env, model) in
    match typeLiftExpr env t.init with (env, init) in
    match typeLiftExpr env t.endTime with (env, endTime) in
    match typeLiftType env t.ty with (env, ty) in
    (env, TmSolveODE { t with
                       model = model,
                       init = init,
                       endTime = endTime,
                       ty = ty })

  -- Partial evaluation
  sem pevalBindThis =
  | TmSolveODE _ -> true

  sem pevalEval ctx k =
  | TmSolveODE r ->
    pevalBind ctx (lam model.
      pevalBind ctx (lam init.
        pevalBind ctx (lam endTime.
          k (TmSolveODE { r with
                          model = model,
                          init = init,
                          endTime = endTime }))
          r.endTime)
        r.init)
      r.model
end

lang Prune =
  Ast + Dist + PrettyPrint + Eq + Sym + TypeCheck + ANF + TypeLift + PEval

  syn Expr =
  | TmPrune { dist: Expr,
              ty: Type,
              info: Info }

  syn Type =
  | TyPruneInt { info : Info }

  sem infoTm =
  | TmPrune t -> t.info

  sem tyTm =
  | TmPrune t -> t.ty

  sem withInfo (info: Info) =
  | TmPrune t -> TmPrune { t with info = info }

  sem withType (ty: Type) =
  | TmPrune t -> TmPrune {t with ty = ty}

  sem smapAccumL_Expr_Expr f acc =
  | TmPrune t ->
    match f acc t.dist with (acc,dist) in
    (acc, TmPrune { t with dist = dist })

  sem smapAccumL_Expr_Type f acc =
  | TmPrune t ->
    match f acc t.ty with (acc,ty) in
    (acc, TmPrune {t with ty = ty })

  sem infoTy =
  | TyPruneInt t -> t.info

  sem unifyBase u env =
  | (TyPruneInt _, TyPruneInt _) -> u.empty
  sem tyWithInfo (info: Info) =
  | TyPruneInt t -> TyPruneInt {t with info = info}

  sem getTypeStringCode (indent: Int) (env: PprintEnv) =
  | TyPruneInt t ->
    (env, join ["PruneInt"])

  sem isAtomic =
  | TmPrune _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmPrune t ->
    let i = pprintIncr indent in
    match printParen i env t.dist with (env,dist) then
      (env, join ["prune", pprintNewline i, dist])
    else never

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmPrune r ->
    match lhs with TmPrune l then
      eqExprH env free l.dist r.dist
    else None ()

  sem eqTypeH (typeEnv : EqTypeEnv) (free : EqTypeFreeEnv) (lhs : Type) =
  | TyPruneInt r ->
    match unwrapType lhs with TyPruneInt _ then
      Some free
    else None ()

  sem symbolizeExpr (env: SymEnv) =
  | TmPrune t ->
    TmPrune {{ t with dist = symbolizeExpr env t.dist}
                    with ty = symbolizeType env t.ty}

  sem typeCheckExpr (env : TCEnv) =
  | TmPrune t ->
    let dist = typeCheckExpr env t.dist in
    let ty =
        match (inspectType (tyTm dist)) with TyDist ({ty=ty}&d) then
        switch inspectType ty
        case TyInt _ then
          TyPruneInt { info = t.info }
        case _ then
          error "invalid Type for prune"
        end
      else error "invalid Type for prune"
    in
    TmPrune {t with dist = dist, ty = ty}

  sem normalize (k : Expr -> Expr) =
  | TmPrune ({dist= dist} & t) ->
    normalizeName (lam dist. k (TmPrune { t with dist=dist})) dist

  sem typeLiftExpr (env:TypeLiftEnv) =
  | TmPrune t ->
    match typeLiftExpr env t.dist with (env, dist) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmPrune {{t with dist = dist}
                          with ty = ty})
      else never
    else never

  sem pevalIsValue =
  | TmPrune _ -> false

  sem pevalEval ctx k =
  | TmPrune r ->
    pevalBind ctx
      (lam dist. k (TmPrune { r with dist = dist})) r.dist
  | TmPrune ( {dist = TmDist ({dist = dist} & td)} & t) ->
    pevalDistEval ctx
      (lam dist. k (TmPrune { t with dist= TmDist { td with dist = dist} } )) dist

end

lang Pruned = Prune

  syn Expr =
  | TmPruned { prune: Expr,
              ty: Type,
              info: Info }

  sem infoTm =
  | TmPruned t -> t.info

  sem tyTm =
  | TmPruned t -> t.ty

  sem withInfo (info: Info) =
  | TmPruned t -> TmPruned { t with info = info }

  sem withType (ty: Type) =
  | TmPruned t -> TmPruned {t with ty = ty}

  sem smapAccumL_Expr_Expr f acc =
  | TmPruned t ->
    match f acc t.prune with (acc,prune) in
    (acc, TmPruned { t with prune = prune })

  sem isAtomic =
  | TmPruned _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmPruned t ->
    let i = pprintIncr indent in
    match printParen i env t.prune with (env,prune) then
      (env, join ["pruned", pprintNewline i, prune])
    else never

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmPruned r ->
    match lhs with TmPruned l then
      eqExprH env free l.prune r.prune
    else None ()

  sem symbolizeExpr (env: SymEnv) =
  | TmPruned t ->
    TmPruned {{ t with prune = symbolizeExpr env t.prune}
                    with ty = symbolizeType env t.ty}

   -- CPS
  sem cpsCont k =
  | TmLet ({ body = TmPruned _ } & t) ->
    TmLet { t with inexpr = cpsCont k t.inexpr }

  sem typeCheckExpr (env : TCEnv) =
  | TmPruned t ->
    let prune = typeCheckExpr env t.prune in
    let ty =
        switch inspectType (tyTm prune)
        case TyPruneInt _ then
          TyInt { info = t.info }
        case _ then
          error "invalid Type for pruned"
        end
    in
    TmPruned {t with prune = prune, ty = ty}

  sem normalize (k : Expr -> Expr) =
  | TmPruned ({prune = prune} & t) ->
    normalizeName (lam prune. k (TmPruned { t with prune = prune})) prune

  sem typeLiftExpr (env:TypeLiftEnv) =
  | TmPruned t ->
    match typeLiftExpr env t.prune with (env, prune) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmPruned {{t with prune = prune}
                          with ty = ty})
      else never
    else never

  sem pevalIsValue =
  | TmPruned _ -> false

  sem pevalEval ctx k =
  | TmPruned r ->
    pevalBind ctx
      (lam prune. k (TmPruned { r with prune = prune})) r.prune

end

lang Cancel = Observe
  syn Expr =
  | TmCancel {dist: Expr,
              value: Expr,
              ty: Type,
              info: Info }

  sem infoTm =
  | TmCancel t -> t.info

  sem tyTm =
  | TmCancel t -> t.ty

  sem withInfo (info: Info) =
  | TmCancel t -> TmCancel { t with info = info }

  sem withType (ty: Type) =
  | TmCancel t -> TmCancel {t with ty = ty}

  sem smapAccumL_Expr_Expr f acc =
  | TmCancel t ->
    match f acc t.value with (acc,value) in
    match f acc t.dist with (acc,dist) in

    (acc, TmCancel {{ t with value = value } with dist = dist})

  sem isAtomic =
  | TmCancel _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmCancel t ->
    let i = pprintIncr indent in
    match
      pprintCode i env
        (TmObserve {dist=t.dist,value=t.value,ty=t.ty,info=t.info})
      with (env,args)
    then
      (env, join ["cancel", pprintNewline i,
       "(", args ,")"])
    else never

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmCancel r ->
    match lhs with TmCancel l then
      match eqExprH env free l.value r.value with Some free then
        eqExprH env free l.dist r.dist
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeExpr (env: SymEnv) =
  | TmCancel t ->
    TmCancel {{{ t with value = symbolizeExpr env t.value }
                    with dist = symbolizeExpr env t.dist }
                    with ty = symbolizeType env t.ty }

  -- Type check
  sem typeCheckExpr (env : TCEnv) =
  | TmCancel t ->
    let value = typeCheckExpr env t.value in
    let dist = typeCheckExpr env t.dist in
    let tyDistRes = newvar env.currentLvl t.info in
    unify env [infoTm dist]
      (TyDist { info = t.info, ty = tyDistRes }) (tyTm dist);
    unify env [infoTm value] tyDistRes (tyTm value);
    TmCancel {{{ t with value = value }
                    with dist = dist }
                    with ty = tyWithInfo t.info tyunit_ }

  -- ANF
  sem normalize (k : Expr -> Expr) =
  | TmCancel ({ value = value, dist = dist } & t) ->
    normalizeName
      (lam value.
        normalizeName
          (lam dist.
            k (TmCancel {{ t with value = value }
                              with dist = dist }))
          dist)
      value

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmCancel t ->
    match typeLiftExpr env t.value with (env, value) then
      match typeLiftExpr env t.dist with (env, dist) then
        match typeLiftType env t.ty with (env, ty) then
          (env, TmCancel {{{ t with value = value }
                                with dist = dist }
                                with ty = ty })
        else never
      else never
    else never

  -- Partial evaluation
  sem pevalIsValue =
  | TmCancel _ -> false

  sem pevalEval ctx k =
  | TmCancel r ->
    pevalBind ctx
      (lam value.
        pevalBind ctx
          (lam dist. k (TmCancel {r with value=value, dist = dist}))
          r.dist)
      r.value
  | TmCancel ({ value = value, dist = TmDist ({ dist = dist } & td) } & t) ->
    pevalBind ctx
      (lam value.
        pevalDistEval ctx
          (lam dist.
             k (TmCancel {{ t with value = value }
                               with dist = TmDist { td with dist = dist}}))
          dist)
      value

  sem exprHasSideEffectH env lambdaCounting acc =
  | TmCancel _ -> true

  sem getValueCancel =
  | TmObserve t -> t.value

  sem getDistCancel =
  | TmObserve t -> t.dist
end

-- Assume defines a new random variable
lang Diff =
  Ast + PrettyPrint + Eq + Sym + TypeCheck + ANF + TypeLift + PEval

  syn Expr =
  | TmDiff { fn: Expr,
             arg: Expr,
             ty: Type,
             info: Info }

  sem infoTm =
  | TmDiff t -> t.info

  sem tyTm =
  | TmDiff t -> t.ty

  sem withInfo (info: Info) =
  | TmDiff t -> TmDiff { t with info = info }

  sem withType (ty: Type) =
  | TmDiff t -> TmDiff { t with ty = ty }

  sem smapAccumL_Expr_Expr f acc =
  | TmDiff t ->
    match f acc t.fn with (acc, fn) in
    match f acc t.arg with (acc, arg) in
    (acc, TmDiff { t with fn = fn, arg = arg })

  -- Pretty printing
  sem isAtomic =
  | TmDiff _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmDiff t ->
    let i = pprintIncr indent in
    match printParen i env t.fn with (env, fn) in
    match printParen i env t.arg with (env, arg) in
    (env, join ["diff", pprintNewline i, fn, pprintNewline i, arg])

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmDiff r ->
    match lhs with TmDiff l then
      optionBind (eqExprH env free l.fn r.fn)
        (lam free. eqExprH env free l.arg r.arg)
    else None ()

  -- Type check
  sem typeCheckExpr (env : TCEnv) =
  | TmDiff t ->
    let fn = typeCheckExpr env t.fn in
    let arg = typeCheckExpr env t.arg in
    let tyParam = newpolyvar env.currentLvl t.info in
    let tyRes = newpolyvar env.currentLvl t.info in
    unify env [infoTm t.fn] (ityarrow_ (infoTm fn) tyParam tyRes) (tyTm fn);
    unify env [infoTm t.arg] tyParam (tyTm arg);
    let argErrorMsg =
      "* Cannot differentiate functions with a function as parameter type\n"
    in
    let resErrorMsg =
      "* Cannot differentiate functions with a function as result type\n"
    in
    let endMsg = "* When type checking the expression\n" in
    switch (inspectType tyParam, inspectType tyRes)
    case (TyArrow _, TyArrow _) then
      let msg = join [argErrorMsg, resErrorMsg, endMsg] in
      errorSingle [t.info] msg
    case (TyArrow _, _) then
      let msg = join [argErrorMsg, endMsg] in
      errorSingle [t.info] msg
    case (_, TyArrow _) then
      let msg = join [resErrorMsg, endMsg] in
      errorSingle [t.info] msg
    case _ then
      let tyDiff = ityarrow_ t.info tyParam tyRes in
      TmDiff { t with fn = fn, arg = arg, ty = tyDiff }
    end

  -- ANF
  sem normalize (k : Expr -> Expr) =
  | TmDiff t ->
    normalizeName (lam fn.
      normalizeName (lam arg.
        k (TmDiff { t with fn = fn, arg = arg }))
        t.arg)
      t.fn

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmDiff t ->
    match typeLiftExpr env t.fn with (env, fn) in
    match typeLiftExpr env t.arg with (env, arg) in
    match typeLiftType env t.ty with (env, ty) in
    (env, TmDiff { t with fn = fn, arg = arg, ty = ty })

  -- Partial evaluation
  sem pevalBindThis =
  | TmDiff _ -> false

  sem pevalEval ctx k =
  | TmDiff r ->
    pevalBind ctx (lam fn.
      pevalBind ctx (lam arg.
        k (TmDiff { r with fn = fn, arg = arg }))
        r.arg)
      r.fn
end

lang Delay =
  Ast + Dist + PrettyPrint + Eq + Sym + TypeCheck + ANF + TypeLift + PEval

  syn Expr =
  | TmDelay { dist: Expr,
              ty: Type,
              info: Info }

  syn Type =
  | TyDelayInt { info : Info }
  | TyDelayFloat { info : Info }
  | TyDelaySeqF { info : Info }

  sem infoTm =
  | TmDelay t -> t.info

  sem tyTm =
  | TmDelay t -> t.ty

  sem withInfo (info: Info) =
  | TmDelay t -> TmDelay { t with info = info }

  sem withType (ty: Type) =
  | TmDelay t -> TmDelay {t with ty = ty}

  sem smapAccumL_Expr_Expr f acc =
  | TmDelay t ->
    match f acc t.dist with (acc,dist) in
    (acc, TmDelay { t with dist = dist })

  sem smapAccumL_Expr_Type f acc =
  | TmDelay t ->
    match f acc t.ty with (acc,ty) in
    (acc, TmDelay {t with ty = ty })

  sem infoTy =
  | TyDelayInt t -> t.info
  | TyDelayFloat t -> t.info
  | TyDelaySeqF t -> t.info

  sem unifyBase u env =
  | (TyDelayInt _, TyDelayInt _) -> u.empty
  | (TyDelayFloat _, TyDelayFloat _) -> u.empty
  | (TyDelaySeqF _, TyDelaySeqF _) -> u.empty

  sem tyWithInfo (info: Info) =
  | TyDelayInt t -> TyDelayInt {t with info = info}
  | TyDelayFloat t -> TyDelayFloat {t with info = info}
  | TyDelaySeqF t -> TyDelaySeqF {t with info = info}

  sem getTypeStringCode (indent: Int) (env: PprintEnv) =
  | TyDelayInt t ->
    (env, join ["DelayInt"])
  | TyDelayFloat t ->
    (env, join ["DelayFloat"])
  | TyDelaySeqF t ->
    (env, join ["DelaySeqF"])

  sem isAtomic =
  | TmDelay _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmDelay t ->
    let i = pprintIncr indent in
    match printParen i env t.dist with (env,dist) then
      (env, join ["delay", pprintNewline i, dist])
    else never

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmDelay r ->
    match lhs with TmDelay l then
      eqExprH env free l.dist r.dist
    else None ()

  sem eqTypeH (typeEnv : EqTypeEnv) (free : EqTypeFreeEnv) (lhs : Type) =
  | TyDelayInt r ->
    match unwrapType lhs with TyDelayInt _ then
      Some free
    else None ()
  | TyDelayFloat r ->
    match unwrapType lhs with TyDelayFloat _ then
      Some free
    else None ()
  | TyDelaySeqF r ->
    match unwrapType lhs with TyDelaySeqF _ then
      Some free
    else None ()

  sem symbolizeExpr (env: SymEnv) =
  | TmDelay t ->
    TmDelay {{ t with dist = symbolizeExpr env t.dist}
                    with ty = symbolizeType env t.ty}

  sem typeCheckExpr (env : TCEnv) =
  | TmDelay t ->
    let dist = typeCheckExpr env t.dist in
    let ty =
        match (inspectType (tyTm dist)) with TyDist ({ty=ty}&d) then
        switch inspectType ty
        case TyInt _ then
          TyDelayInt { info = t.info }
        case TyFloat _ then
          TyDelayFloat { info = t.info }
        case TySeq {ty = TyFloat _} then
          TyDelaySeqF { info = t.info }
        case _ then
          error "invalid Type for delay"
        end
      else error "invalid Type for delay"
    in
    TmDelay {t with dist = dist, ty = ty}

  sem normalize (k : Expr -> Expr) =
  | TmDelay ({dist= dist} & t) ->
    normalizeName (lam dist. k (TmDelay { t with dist=dist})) dist

  sem typeLiftExpr (env:TypeLiftEnv) =
  | TmDelay t ->
    match typeLiftExpr env t.dist with (env, dist) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmDelay {{t with dist = dist}
                          with ty = ty})
      else never
    else never

  sem pevalIsValue =
  | TmDelay _ -> false

  sem pevalEval ctx k =
  | TmDelay r ->
    pevalBind ctx
      (lam dist. k (TmDelay { r with dist = dist})) r.dist
  | TmDelay ( {dist = TmDist ({dist = dist} & td)} & t) ->
    pevalDistEval ctx
      (lam dist. k (TmDelay { t with dist= TmDist { td with dist = dist} } )) dist

end

lang Delayed = Delay

  syn Expr =
  | TmDelayed { delay: Expr,
              ty: Type,
              info: Info }

  sem infoTm =
  | TmDelayed t -> t.info

  sem tyTm =
  | TmDelayed t -> t.ty

  sem withInfo (info: Info) =
  | TmDelayed t -> TmDelayed { t with info = info }

  sem withType (ty: Type) =
  | TmDelayed t -> TmDelayed {t with ty = ty}

  sem smapAccumL_Expr_Expr f acc =
  | TmDelayed t ->
    match f acc t.delay with (acc,delay) in
    (acc, TmDelayed { t with delay = delay })

  sem isAtomic =
  | TmDelayed _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmDelayed t ->
    let i = pprintIncr indent in
    match printParen i env t.delay with (env, delay) then
      (env, join ["delayed", pprintNewline i, delay])
    else never

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmDelayed r ->
    match lhs with TmDelayed l then
      eqExprH env free l.delay r.delay
    else None ()

  sem symbolizeExpr (env: SymEnv) =
  | TmDelayed t ->
    TmDelayed {{ t with delay = symbolizeExpr env t.delay}
                    with ty = symbolizeType env t.ty}

   -- CPS
  sem cpsCont k =
  | TmLet ({ body = TmDelayed _ } & t) ->
    TmLet { t with inexpr = cpsCont k t.inexpr }

  sem typeCheckExpr (env : TCEnv) =
  | TmDelayed t ->
    let delay = typeCheckExpr env t.delay in
    let ty =
        switch inspectType (tyTm delay)
        case TyDelayInt _ then
          TyInt { info = t.info }
        case TyDelayFloat _ then
          TyFloat { info = t.info }
        case TyDelaySeqF _ then
          TySeq { ty = TyFloat {info = t.info}, info = t.info }
        case _ then
          error "invalid Type for delayed"
        end
    in
    TmDelayed {t with delay = delay, ty = ty}

  sem normalize (k : Expr -> Expr) =
  | TmDelayed ({delay = delay} & t) ->
    normalizeName (lam delay. k (TmDelayed { t with delay = delay})) delay

  sem typeLiftExpr (env:TypeLiftEnv) =
  | TmDelayed t ->
    match typeLiftExpr env t.delay with (env, delay) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmDelayed {{t with delay = delay}
                          with ty = ty})
      else never
    else never

  sem pevalIsValue =
  | TmDelayed _ -> false

  sem pevalEval ctx k =
  | TmDelayed r ->
    pevalBind ctx
      (lam delay. k (TmDelayed { r with delay = delay})) r.delay
end

-----------------
-- AST BUILDER --
-----------------

let infer_ = use Infer in
  lam d. lam m.
  TmInfer {method = d, model = m, ty = tyunknown_, info = NoInfo ()}

let assume_ = use Assume in
  lam d. TmAssume {dist = d, ty = tyunknown_, info = NoInfo ()}

let observe_ = use Observe in
  lam v. lam d.
  TmObserve {value = v, dist = d, ty = tyunknown_, info = NoInfo ()}

let weight_ = use Weight in
  lam w. TmWeight {weight = w, ty = tyunknown_, info = NoInfo ()}

 let solveode_ = use SolveODE in
   lam m. lam i. lam t.
    TmSolveODE {
      method = ODESolverDefault { stepSize = float_ 0.01 },
      model = m,
      init = i,
      endTime = t,
      ty = tyunknown_,
      info = NoInfo ()
    }

let prune_ = use Prune in
  lam d. TmPrune {dist = d, ty = tyunknown_ , info = NoInfo ()}

let pruned_ = use Pruned in
  lam p. TmPruned {prune = p, ty = tyunknown_ , info = NoInfo ()}

let cancel_ = use Cancel in
  lam o. lam v. TmCancel {dist = o, ty = tyunknown_ , info = NoInfo (), value = v}

let typruneint_ = use Prune in
  TyPruneInt {info = NoInfo ()}

let diff_ = use Diff in
   lam fn. lam arg.
     TmDiff { fn = fn, arg = arg, ty = tyunknown_, info = NoInfo () }

let delay_ = use Delay in
  lam d. TmDelay {dist = d, ty = tyunknown_ , info = NoInfo ()}

let delayed_ = use Delayed in
  lam d. TmDelayed {delay = d, ty = tyunknown_ , info = NoInfo ()}

let tydelayint_ = use Delay in
  TyDelayInt {info = NoInfo ()}

let tydelayfloat_ = use Delay in
  TyDelayFloat {info = NoInfo ()}

let tydelayseqf_ = use Delay in
  TyDelaySeqF {info = NoInfo ()}
---------------------------
-- LANGUAGE COMPOSITIONS --
---------------------------

lang CorePPL =
  Ast + Assume + Observe + Weight + Infer + ObserveWeightTranslation + DistAll +
  Pruned + Cancel
end

let pplKeywords = [
  "assume", "observe", "weight", "resample", "cancel",
  "Uniform", "Bernoulli", "Poisson", "Beta", "Gamma", "Categorical",
  "Multinomial", "Dirichlet", "Exponential", "Empirical", "Gaussian",
  "Binomial", "Wiener"
]

lang CoreDPL = Ast + SolveODE + Diff + Delayed end

let dplKeywords = [
  "solveode", "diff"
]

let pruneKeywords = [
  "prune", "pruned"
]

let delayKeywords = [
  "delay", "delayed"
]

let mexprPPLKeywords = join [mexprKeywords, pplKeywords, dplKeywords, pruneKeywords, delayKeywords]

lang MExprPPL =
  CorePPL + CoreDPL + ElementaryFunctions +
  MExprAst + MExprPrettyPrint + MExprEq + MExprSym +
  MExprTypeCheck + MExprTypeLift + MExprArity

  sem mexprPPLToString =
  | expr -> exprToStringKeywords mexprPPLKeywords expr

end

lang Test = MExprPPL + MExprANF
end

mexpr

use Test in

let tmAssume = assume_ (bern_ (float_ 0.7)) in
let tmObserve = observe_ (float_ 1.5) (beta_ (float_ 1.0) (float_ 2.0)) in
let tmWeight = weight_ (float_ 1.5) in
let tmODE =
  ulams_ ["t", "x"]
    (seq_ [
      get_ (var_ "x") (int_ 0),
      subf_ (negf_ (get_ (var_ "x") (int_ 1))) (get_ (var_ "x") (int_ 0))
    ])
in
let tmX0 = seq_ [float_ 1., float_ 0.] in
let tmTEnd = float_ 1. in
let tmSolveODE = solveode_ tmODE tmX0 tmTEnd in
let tmPrune =
  prune_ (categorical_ (seq_ [float_ 0.5,float_ 0.3,float_ 0.2]))
in
let tmPrune2 =
  prune_ (categorical_ (seq_ [float_ 0.4,float_ 0.4,float_ 0.2]))
in
let tmPruned = pruned_ tmPrune in
let tmPruned2 = pruned_ tmPrune2 in
let tmCancel = cancel_ (bern_ (float_ 0.7)) true_ in
let tmCancel2 = cancel_ (bern_ (float_ 0.7)) false_  in
let tmDiff = diff_ (ulam_ "x" (var_ "x")) (float_ 1.) in
let tmDelay =
  delay_ (categorical_ (seq_ [float_ 0.5,float_ 0.3,float_ 0.2]))
in
let tmDelay2 =
  delay_ (categorical_ (seq_ [float_ 0.4,float_ 0.4,float_ 0.2]))
in
let tmDelayed = delayed_ tmDelay in
let tmDelayed2 = delayed_ tmDelay2 in

------------------------
-- PRETTY-PRINT TESTS --
------------------------
-- TODO(dlunde,2021-04-28): TmInfer test

let _toStr = utestDefaultToString (lam x. x) (lam x. x) in

utest mexprPPLToString tmAssume
with strJoin "\n" [
  "assume",
  "  (Bernoulli",
  "     0.7)"
] using eqString else _toStr in

utest mexprPPLToString tmObserve
with strJoin "\n" [
  "observe",
  "  1.5 (Beta",
  "     1. 2.)"
] using eqString else _toStr in

utest mexprPPLToString tmWeight
with strJoin "\n" [
  "weight",
  "  1.5"
] using eqString else _toStr in

utest mexprPPLToString tmSolveODE
with strJoin "\n" [
  "solveode",
  "  (lam t.",
  "     lam x.",
  "       [ get x 0,",
  "         subf (negf (get x 1)) (get x 0) ])",
  "  [ 1., 0. ]",
  "  1."
] using eqString else _toStr in

utest mexprPPLToString tmPrune
with strJoin "\n" [
  "prune",
  "  (Categorical",
  "     [ 0.5, 0.3, 0.2 ])"
] using eqString else _toStr in

utest mexprPPLToString tmPruned
with strJoin "\n" [
  "pruned",
  "  (prune",
  "     (Categorical",
  "        [ 0.5, 0.3, 0.2 ]))"
] using eqString else _toStr in

utest mexprPPLToString tmCancel
with strJoin "\n" [
  "cancel",
  "  (observe",
  "    true (Bernoulli",
  "       0.7))"
] using eqString else _toStr in

utest mexprPPLToString tmDelay
with strJoin "\n" [
  "delay",
  "  (Categorical",
  "     [ 0.5, 0.3, 0.2 ])"
] using eqString else _toStr in

utest mexprPPLToString tmDelayed
with strJoin "\n" [
  "delayed",
  "  (delay",
  "     (Categorical",
  "        [ 0.5, 0.3, 0.2 ]))"
] using eqString else _toStr in

--------------------
-- EQUALITY TESTS --
--------------------
-- TODO(dlunde,2021-04-28): TmInfer test

let _toStr = utestDefaultToString mexprPPLToString mexprPPLToString in
let neqExpr = lam l. lam r. not (eqExpr l r) in

utest tmAssume with tmAssume using eqExpr else _toStr in
utest tmAssume with (assume_ (bern_ (float_ 0.6))) using neqExpr else _toStr in

utest tmObserve with tmObserve using eqExpr else _toStr in
utest tmObserve with (observe_ (float_ 1.5) (beta_ (float_ 1.0) (float_ 2.1)))
  using neqExpr else _toStr
in

utest tmWeight with tmWeight using eqExpr else _toStr in
utest tmWeight with ((weight_ (float_ 1.6))) using neqExpr else _toStr in

utest tmSolveODE with tmSolveODE using eqExpr else _toStr in
utest tmSolveODE with solveode_ tmODE tmX0 (float_ 2.)
  using neqExpr else _toStr
in

utest tmPrune with tmPrune using eqExpr else _toStr in
utest tmPruned with tmPruned using eqExpr else _toStr in
utest tmCancel with tmCancel using eqExpr else _toStr in
utest tmPrune with tmPrune2 using neqExpr else _toStr in
utest tmPruned with tmPruned2 using neqExpr else _toStr in
utest tmCancel with tmCancel2 using neqExpr else _toStr in
utest tmDiff with tmDiff using eqExpr else _toStr in
utest tmDiff with diff_ (ulam_ "x" (var_ "x")) (float_ 2.)
  using neqExpr else _toStr
in
utest tmDelay with tmDelay using eqExpr else _toStr in
utest tmDelayed with tmDelayed using eqExpr else _toStr in
utest tmDelay with tmDelay2 using neqExpr else _toStr in
utest tmDelayed with tmDelayed2 using neqExpr else _toStr in

----------------------
-- SMAP/SFOLD TESTS --
----------------------
-- TODO(dlunde,2021-04-28): TmInfer test

let _tmsToStr = lam tms. strJoin "," (map mexprPPLToString tms) in
let _seqToStr = utestDefaultToString _tmsToStr _tmsToStr in

let tmVar = var_ "x" in
let mapVar = (lam. tmVar) in
let foldToSeq = lam a. lam e. cons e a in

utest smap_Expr_Expr mapVar tmAssume with assume_ tmVar using eqExpr
  else _toStr
in
utest sfold_Expr_Expr foldToSeq [] tmAssume
with [ bern_ (float_ 0.7) ] using eqSeq eqExpr else _seqToStr in

utest smap_Expr_Expr mapVar tmObserve with observe_ tmVar tmVar using eqExpr
  else _toStr
in
utest sfold_Expr_Expr foldToSeq [] tmObserve
with [
  (beta_ (float_ 1.0) (float_ 2.0)),
  (float_ 1.5)
] using eqSeq eqExpr else _seqToStr in

utest smap_Expr_Expr mapVar tmWeight with weight_ tmVar using eqExpr
  else _toStr
in
utest sfold_Expr_Expr foldToSeq [] tmWeight
with [ float_ 1.5 ] using eqSeq eqExpr else _seqToStr in

utest smap_Expr_Expr mapVar tmSolveODE
  with solveode_ tmVar tmVar tmVar
  using eqExpr else _toStr
in
utest sfold_Expr_Expr foldToSeq [] tmSolveODE
with [ tmTEnd, tmX0, tmODE, float_ 0.01 ] using eqSeq eqExpr else _seqToStr in


utest sfold_Expr_Expr foldToSeq [] tmPrune
  with [ categorical_ (seq_ [float_ 0.5,float_ 0.3,float_ 0.2]) ]
  using eqSeq eqExpr else _seqToStr
in

utest sfold_Expr_Expr foldToSeq [] tmPruned
  with [ prune_ (categorical_ (seq_ [float_ 0.5,float_ 0.3,float_ 0.2])) ]
  using eqSeq eqExpr else _seqToStr
in

utest sfold_Expr_Expr foldToSeq [] tmCancel
with [ bern_ (float_ 0.7), true_ ] using eqSeq eqExpr else _seqToStr in
utest smap_Expr_Expr mapVar tmDiff
  with diff_ tmVar tmVar
  using eqExpr else _toStr
in
utest sfold_Expr_Expr foldToSeq [] tmDiff
  with [ float_ 1., ulam_ "x" (var_ "x") ]
  using eqSeq eqExpr else _seqToStr
in

utest sfold_Expr_Expr foldToSeq [] tmDelay
  with [ categorical_ (seq_ [float_ 0.5,float_ 0.3,float_ 0.2]) ]
  using eqSeq eqExpr else _seqToStr
in

utest sfold_Expr_Expr foldToSeq [] tmDelayed
  with [ delay_ (categorical_ (seq_ [float_ 0.5,float_ 0.3,float_ 0.2])) ]
  using eqSeq eqExpr else _seqToStr
in
---------------------
-- SYMBOLIZE TESTS --
---------------------
-- TODO(dlunde,2021-04-28): TmInfer test

utest symbolize tmAssume with tmAssume using eqExpr in
utest symbolize tmObserve with tmObserve using eqExpr in
utest symbolize tmWeight with tmWeight using eqExpr in
utest symbolize tmSolveODE with tmSolveODE using eqExpr in
utest symbolize tmPrune with tmPrune using eqExpr in
utest symbolize tmPruned with tmPruned using eqExpr in
utest symbolize tmCancel with tmCancel using eqExpr in
utest symbolize tmDiff with tmDiff using eqExpr in
utest symbolize tmDelay with tmDelay using eqExpr in
utest symbolize tmDelayed with tmDelayed using eqExpr in


----------------------
-- TYPE CHECK TESTS --
----------------------
-- TODO(dlunde,2021-04-28): TmInfer test

utest tyTm (typeCheck tmAssume) with tybool_ using eqType in
utest tyTm (typeCheck tmObserve) with tyunit_ using eqType in
utest tyTm (typeCheck tmWeight) with tyunit_ using eqType in
utest tyTm (typeCheck tmSolveODE) with tyseq_ tyfloat_ using eqType in
utest tyTm (typeCheck tmAssume) with tybool_ using eqType in
utest tyTm (typeCheck tmPrune) with typruneint_ using eqType in
utest tyTm (typeCheck tmCancel) with tyunit_ using eqType in
utest tyTm (typeCheck tmDiff) with tyarrow_ tyfloat_ tyfloat_ using eqType in
utest tyTm (typeCheck tmDelay) with tydelayint_ using eqType in

---------------
-- ANF TESTS --
---------------
-- TODO(dlunde,2021-05-10): TmInfer test

let _anf = compose normalizeTerm symbolize in

utest _anf tmAssume with bindall_ [
  ulet_ "t" (bern_ (float_ 0.7)),
  ulet_ "t1" (assume_ (var_ "t")),
  var_ "t1"
] using eqExpr else _toStr in
utest _anf tmObserve with bindall_ [
  ulet_ "t" (beta_ (float_ 1.0) (float_ 2.0)),
  ulet_ "t1" (observe_ (float_ 1.5) (var_ "t")),
  var_ "t1"
] using eqExpr else _toStr in
utest _anf tmWeight with bindall_ [
  ulet_ "t" (weight_ (float_ 1.5)),
  var_ "t"
] using eqExpr else _toStr in
utest _anf (solveode_ (ulams_ ["t", "x"] (seq_ [float_ 1.])) tmX0 tmTEnd)
  with bindall_ [
    ulet_ "t" tmX0,
    ulet_ "t1"
      (solveode_
         (ulams_ ["t", "x"]
            (bind_ (ulet_ "t" (seq_ [float_ 1.])) (var_ "t"))) (var_ "t")
         tmTEnd),
    var_ "t1"
] using eqExpr else _toStr in
utest _anf tmPrune with bindall_ [
  ulet_ "t" (seq_ [float_ 0.5,float_ 0.3,float_ 0.2]),
  ulet_ "t1" (categorical_ (var_ "t")),
  ulet_ "t2" (prune_ (var_ "t1")),
  var_ "t2"
] using eqExpr else _toStr in
utest _anf tmPruned with bindall_ [
  ulet_ "t" (seq_ [float_ 0.5,float_ 0.3,float_ 0.2]),
  ulet_ "t1" (categorical_ (var_ "t")),
  ulet_ "t2" (prune_ (var_ "t1")),
  ulet_ "t3" (pruned_ (var_ "t2")),
  var_ "t3"
] using eqExpr else _toStr in
utest _anf tmCancel with bindall_ [
  ulet_ "t" (bern_ (float_ 0.7)),
  ulet_ "t1" (cancel_ (var_ "t") true_),
  var_ "t1"
] using eqExpr else _toStr in
utest _anf tmDiff
  with bindall_ [
    ulet_ "t" tmDiff,
    var_ "t"
] using eqExpr else _toStr in
utest _anf tmDelay with bindall_ [
  ulet_ "t" (seq_ [float_ 0.5,float_ 0.3,float_ 0.2]),
  ulet_ "t1" (categorical_ (var_ "t")),
  ulet_ "t2" (delay_ (var_ "t1")),
  var_ "t2"
] using eqExpr else _toStr in
utest _anf tmDelayed with bindall_ [
  ulet_ "t" (seq_ [float_ 0.5,float_ 0.3,float_ 0.2]),
  ulet_ "t1" (categorical_ (var_ "t")),
  ulet_ "t2" (delay_ (var_ "t1")),
  ulet_ "t3" (delayed_ (var_ "t2")),
  var_ "t3"
] using eqExpr else _toStr in

---------------------
-- TYPE-LIFT TESTS --
---------------------
-- TODO(dlunde,2021-05-10): TmInfer test

utest (typeLift tmAssume).1 with tmAssume using eqExpr in
utest (typeLift tmObserve).1 with tmObserve using eqExpr in
utest (typeLift tmWeight).1 with tmWeight using eqExpr in
utest (typeLift tmSolveODE).1 with tmSolveODE using eqExpr in
utest (typeLift tmPrune).1 with tmPrune using eqExpr in
utest (typeLift tmPruned).1 with tmPruned using eqExpr in
utest (typeLift tmCancel).1 with tmCancel using eqExpr in
utest (typeLift tmDiff).1 with tmDiff using eqExpr in
utest (typeLift tmDelay).1 with tmDelay using eqExpr in
utest (typeLift tmDelayed).1 with tmDelayed using eqExpr in

()
