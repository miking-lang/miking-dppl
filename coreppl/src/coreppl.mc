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

include "peval/peval.mc"

include "string.mc"
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
    unify env [infoTm model] (ityarrow_ t.info (tyWithInfo t.info tyunit_) tyRes) (tyTm model);
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
    unify env [infoTm dist] (TyDist { info = t.info, ty = tyDistRes }) (tyTm dist);
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
  ODESolverMethodBase

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
    TmSolveODE
      { model = m, init = i, endTime = t, ty = tyunknown_, info = NoInfo () }

---------------------------
-- LANGUAGE COMPOSITIONS --
---------------------------

lang CorePPL =
  Ast + Assume + Observe + Weight + Infer + ObserveWeightTranslation + DistAll
end

let pplKeywords = [
  "assume", "observe", "weight", "resample", "plate", "Uniform", "Bernoulli",
  "Poisson", "Beta", "Gamma", "Categorical", "Multinomial", "Dirichlet",
  "Exponential", "Empirical", "Gaussian", "Binomial", "Wiener"
]

lang CoreDPL = Ast + SolveODE end

let dplKeywords = [
  "solveode"
]

let mexprPPLKeywords = join [mexprKeywords, pplKeywords, dplKeywords]

lang MExprPPL =
  CorePPL + CoreDPL +
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
  "  1.5",
  "  (Beta",
  "     1.",
  "     2.)"
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
  "       [ get",
  "           x",
  "           0,",
  "         subf",
  "           (negf",
  "              (get",
  "                 x",
  "                 1))",
  "           (get",
  "              x",
  "              0) ])",
  "  [ 1.,",
  "    0. ]",
  "  1."
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
with [ tmTEnd, tmX0, tmODE ] using eqSeq eqExpr else _seqToStr in

---------------------
-- SYMBOLIZE TESTS --
---------------------
-- TODO(dlunde,2021-04-28): TmInfer test

utest symbolize tmAssume with tmAssume using eqExpr in
utest symbolize tmObserve with tmObserve using eqExpr in
utest symbolize tmWeight with tmWeight using eqExpr in
utest symbolize tmSolveODE with tmSolveODE using eqExpr in


----------------------
-- TYPE CHECK TESTS --
----------------------
-- TODO(dlunde,2021-04-28): TmInfer test

utest tyTm (typeCheck tmAssume) with tybool_ using eqType in
utest tyTm (typeCheck tmObserve) with tyunit_ using eqType in
utest tyTm (typeCheck tmWeight) with tyunit_ using eqType in
utest tyTm (typeCheck tmSolveODE) with tyseq_ tyfloat_ using eqType in

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

---------------------
-- TYPE-LIFT TESTS --
---------------------
-- TODO(dlunde,2021-05-10): TmInfer test

utest (typeLift tmAssume).1 with tmAssume using eqExpr in
utest (typeLift tmObserve).1 with tmObserve using eqExpr in
utest (typeLift tmWeight).1 with tmWeight using eqExpr in
utest (typeLift tmSolveODE).1 with tmSolveODE using eqExpr in

()
