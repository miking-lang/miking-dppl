
include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "mexpr/info.mc"
include "mexpr/eq.mc"
include "mexpr/type-check.mc"
include "mexpr/anf.mc"
include "mexpr/type-lift.mc"
include "mexpr/const-arity.mc"
include "mexpr/const-types.mc"

include "peval/peval.mc"

include "string.mc"
include "seq.mc"
include "utest.mc"

lang Dist = PrettyPrint + Eq + Sym + TypeCheck + ANF + TypeLift +
            TyConst + ConstPrettyPrint + ConstArity + PEval

  syn Expr =
  | TmDist { dist : Dist,
             ty : Type,
             info : Info }

  syn Type =
  | TyDist { info : Info,
             ty : Type }

  syn Dist = -- Intentionally left blank

  -- Setters/Getters
  sem infoTm =
  | TmDist t -> t.info

  sem tyTm =
  | TmDist t -> t.ty

  sem withInfo (info : Info) =
  | TmDist t -> TmDist { t with info = info }

  sem withType (ty : Type) =
  | TmDist t -> TmDist { t with ty = ty }

  sem infoTy =
  | TyDist t -> t.info

  sem tyWithInfo (info : Info) =
  | TyDist t -> TyDist {t with info = info}

  -- Each distribution should implement the following three semantic functions.

  -- Shallow map/fold over distribution parameters.
  sem smapAccumL_Dist_Expr : all a. (a -> Expr -> (a, Expr)) -> a -> Dist -> (a, Dist)

  -- Returns the parameter and sample type of a distribution. E.g.,
  -- `distTy _ (Gaussian _)` = `([], [TyFloat _, TyFloat _], [TyFloat _])`.
  sem distTy : Info -> Dist ->
    ([Name],                    -- The names of all type variables appearing in
                                -- the distribution type.
     [Type],                    -- The parameter types with the same order as as
                                -- the order `smapAccumL_Dist_Expr`
                                -- traverses the parameters.
     Type)                      -- The type of the support.

  -- Returns the distribution name
  sem distName : Dist -> String

  -- End of semantic functions that should be implemented by all
  -- distributions. The remaining term related semantic functions are
  -- implemented using the above semantic functions.

  -- Shallow map/fold
  sem smap_Dist_Expr : (Expr -> Expr) -> Dist -> Dist
  sem smap_Dist_Expr f =| d ->
    (smapAccumL_Dist_Expr (lam. lam tm. ((), f tm)) () d).1

  sem sfold_Dist_Expr : all a. (a -> Expr -> a) -> a -> Dist -> a
  sem sfold_Dist_Expr f acc =| d ->
    (smapAccumL_Dist_Expr (lam acc. lam tm. (f acc tm, tm)) acc d).0

  sem smapAccumL_Expr_Expr f acc =
  | TmDist t ->
    -- NOTE(oerikss, 2024-11-07): We shallow recurse through dist expressions by
    -- default. This is more convenient than requiring each semantic function
    -- that nees to map/recurse through all expressions in an AST to manually
    -- call `smapAccumL_InferMethod_Expr`. If you want to avoid this behaviour
    -- you can instead manually match on `TmDist` and do something different.
    match smapAccumL_Dist_Expr f acc t.dist with (acc, dist) in
    (acc, TmDist { t with dist = dist })

  sem smapAccumL_Type_Type f acc =
  | TyDist t ->
    match f acc t.ty with (acc, ty) in
    (acc, TyDist {t with ty = ty})

  -- Returns the parameters of a distribution
  sem distParams : Dist -> [Expr]
  sem distParams =| d -> sfold_Dist_Expr snoc [] d

  -- Sets the parameters of a distribution. The order of the parameters should
  -- be the same as returned by `distParams`.
  sem distWithParams : Dist -> [Expr] -> Dist
  sem distWithParams d =| tms ->
    (smapAccumL_Dist_Expr
       (lam tms. lam.
        match tms with [tm] ++ tms then (tms, tm)
        else error "Illformed distribution parameters")
       tms d).1

  -- Pretty printing
  sem isAtomic =
  | TmDist _ -> false

  sem pprintCode (indent : Int) (env : PprintEnv) =
  | TmDist t ->
    let aindent = pprintIncr indent in
    match printArgs aindent env (distParams t.dist) with (env, params) in
    let dist = distName t.dist in
    if lti (length params) env.optSingleLineLimit then
      (env, join [dist, " ", params])
    else
      (env, join [dist, pprintNewline aindent, params])

  sem getTypeStringCode (indent : Int) (env : PprintEnv) =
  | TyDist t ->
     match getTypeStringCode indent env t.ty with (env, ty) in
    (env, join ["Dist(", ty, ")"])

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmDist r ->
    match lhs with TmDist l then
      if eqi (constructorTag l.dist) (constructorTag r.dist) then
        optionFoldlM (lam free. lam t. eqExprH env free t.0 t.1) free
          (zip (distParams l.dist) (distParams r.dist))
      else None ()
    else None ()

  sem eqTypeH (typeEnv : EqTypeEnv) (free : EqTypeFreeEnv) (lhs : Type) =
  | TyDist r ->
    match unwrapType lhs with TyDist l then
      eqTypeH typeEnv free l.ty r.ty
    else None ()

  sem cmpTypeH : (Type, Type) -> Int
  sem cmpTypeH =
  | (TyDist l, TyDist r) -> cmpType l.ty r.ty

  -- Type checking
  sem typeCheckExpr (env : TCEnv) =
  | TmDist t ->
    match distTy t.info t.dist with (vars, paramTys, ty) in
    -- NOTE(oerikss, 2024-10-18): We gather the parameter types and the support
    -- type in an arrow type so that we can instantiate all type variables.
    let ty =
      inst t.info env.currentLvl
        (foldr ntyall_ (tyarrow_ (tytuple_ paramTys) ty) vars)
    in
    -- We then destruct this arrow type and unify the parameter types.
    match ty with
      TyArrow {from = TyRecord {fields = paramTys}, to = ty}
    then
      let paramTys =
        optionGetOrElse (lam. error "impossible") (record2tuple paramTys)
      in
      match
        smapAccumL_Dist_Expr
          (lam tys. lam p.
            match tys with [ty] ++ tys then
              let p = typeCheckExpr env p in
              unify env [t.info, infoTm p] ty (tyTm p); (tys, p)
            else error "Illformed distribution parameter type")
          paramTys t.dist
        with (_, dist)
      in
      TmDist { t with ty = TyDist { info = t.info, ty = ty }, dist = dist }
    else error "impossible"

  sem unifyBase : all u. Unifier u -> UnifyEnv -> (Type, Type) -> u
  sem unifyBase u env =
  | (TyDist t1, TyDist t2) -> unifyTypes u env (t1.ty, t2.ty)

  -- ANF
  sem normalizeDist : (Dist -> Expr) -> Dist -> Expr
  sem normalizeDist k =| dist ->
    mapK (lam tm. lam k. normalizeName k tm) (distParams dist)
      (lam params. k (distWithParams dist params))

  sem normalize (k : Expr -> Expr) =
  | TmDist t -> normalizeDist (lam dist. k (TmDist { t with dist = dist })) t.dist

  -- CPS
  sem cpsCont k =
  | TmLet ({ body = TmDist _ } & t) ->
    TmLet { t with inexpr = cpsCont k t.inexpr }

  -- Partial Evaluation
  sem pevalDistEval : PEvalCtx -> (Dist -> Expr) -> Dist -> Expr
  sem pevalDistEval ctx k =| dist ->
    mapK (lam tm. lam k. pevalBind ctx k tm) (distParams dist)
      (lam params. k (distWithParams dist params))

  -- Builtin operations on distributions
  syn Const =
  | CDistEmpiricalSamples {}
  | CDistEmpiricalDegenerate {}
  | CDistEmpiricalNormConst {}
  | CDistEmpiricalAcceptRate {}
  | CDistExpectation {}

  sem getConstStringCode (indent : Int) =
  | CDistEmpiricalSamples _ -> "distEmpiricalSamples"
  | CDistEmpiricalDegenerate _ -> "distEmpiricalDegenerate"
  | CDistEmpiricalNormConst _ -> "distEmpiricalNormConst"
  | CDistEmpiricalAcceptRate _ -> "distEmpiricalAcceptRate"
  | CDistExpectation _ -> "expectation"

  sem _tydist =
  | a -> TyDist {info = NoInfo (), ty = a}

  sem tyConstBase d =
  | CDistEmpiricalSamples _ ->
    tyall_ "a" (tyarrow_ (_tydist (tyvar_ "a"))
                  (tytuple_ [tyseq_ (tyvar_ "a"), tyseq_ tyfloat_]))
  | CDistEmpiricalDegenerate _ -> tyall_ "a" (tyarrow_ (_tydist (tyvar_ "a")) tybool_)
  | CDistEmpiricalNormConst _ -> tyall_ "a" (tyarrow_ (_tydist (tyvar_ "a")) tyfloat_)
  | CDistEmpiricalAcceptRate _ -> tyall_ "a" (tyarrow_ (_tydist (tyvar_ "a")) tyfloat_)
  | CDistExpectation _ -> tyarrow_ (_tydist tyfloat_) tyfloat_

  sem constArity =
  | CDistEmpiricalSamples _ -> 1
  | CDistEmpiricalDegenerate _ -> 1
  | CDistEmpiricalNormConst _ -> 1
  | CDistEmpiricalAcceptRate _ -> 1
  | CDistExpectation _ -> 1

end

lang UniformDist = Dist
  syn Dist =
  | DUniform { a : Expr, b : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DUniform t ->
    match f acc t.a with (acc, a) in
    match f acc t.b with (acc, b) in
    (acc, DUniform { t with a = a, b = b })

  sem distTy info =
  | DUniform _ ->
    let f = ityfloat_ info in ([], [f, f], f)

  sem distName =
  | DUniform _ -> "Uniform"
end

lang BernoulliDist = Dist
  syn Dist =
  | DBernoulli { p : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DBernoulli t ->
    match f acc t.p with (acc, p) in
    (acc, DBernoulli { t with p = p })

  sem distTy info =
  | DBernoulli _ -> ([], [ityfloat_ info], itybool_ info)

  sem distName =
  | DBernoulli _ -> "Bernoulli"
end

lang PoissonDist = Dist
  syn Dist =
  | DPoisson { lambda : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DPoisson t ->
    match f acc t.lambda with (acc, lambda) in
    (acc, DPoisson { t with lambda = lambda })

  sem distTy info =
  | DPoisson _ -> ([], [ityfloat_ info], ityint_ info)

  sem distName =
  | DPoisson _ -> "Poisson"
end

lang BetaDist = Dist
  syn Dist =
  | DBeta { a : Expr, b : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DBeta t ->
    match f acc t.a with (acc, a) in
    match f acc t.b with (acc, b) in
    (acc, DBeta { t with a = a, b = b })

  sem distTy info =
  | DBeta _ ->
    let f = ityfloat_ info in ([], [f, f], f)

  sem distName =
  | DBeta _ -> "Beta"
end

lang GammaDist = Dist
  syn Dist =
  | DGamma { k : Expr, theta : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DGamma t ->
    match f acc t.k with (acc, k) in
    match f acc t.theta with (acc, theta) in
    (acc, DGamma { t with k = k, theta = theta })

  sem distTy info =
  | DGamma _ ->
    let f = ityfloat_ info in ([], [f, f], f)

  sem distName =
  | DGamma _ -> "Gamma"
end

-- DCategorical {p=p} is equivalent to DMultinomial {n=1, p=p}
lang CategoricalDist = Dist
  syn Dist =
  -- p has type [Float]: the list of probabilities
  | DCategorical { p : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DCategorical t ->
    match f acc t.p with (acc, p) in
    (acc, DCategorical { t with p = p })

  sem distTy info =
  | DCategorical _ -> ([], [ityseq_ info (ityfloat_ info)], ityint_ info)

  sem distName =
  | DCategorical _ -> "Categorical"
end

lang MultinomialDist = Dist
  syn Dist =
  -- n has type Int : the number of trials
  -- p has type [Float]: the list of probabilities
  | DMultinomial { n : Expr, p : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DMultinomial t ->
    match f acc t.n with (acc, n) in
    match f acc t.p with (acc, p) in
    (acc, DMultinomial { t with n = n, p = p })

  sem distTy info =
  | DMultinomial _ ->
    let i = ityint_ info in
    let s = ityseq_ info in
    ([], [i, s (ityfloat_ info)], s i)

  sem distName =
  | DMultinomial _ -> "Multinomial"
end

lang DirichletDist = Dist
  syn Dist =
  -- a has type [Float]: the list of concentration parameters
  | DDirichlet { a : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DDirichlet t ->
    match f acc t.a with (acc, a) in
    (acc, DDirichlet { t with a = a })

  sem distTy info =
  | DDirichlet _ ->
    let f = ityfloat_ info in
    let s = ityseq_ info in
    ([], [s f], s f)

  sem distName =
  | DDirichlet _ -> "Dirichlet"
end

lang ExponentialDist = Dist
  syn Dist =
  | DExponential { rate : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DExponential t ->
    match f acc t.rate with (acc, rate) in
    (acc, DExponential { t with rate = rate })

  sem distTy info =
  | DExponential _ ->
    let f = ityfloat_ info in ([], [f], f)

  sem distName =
  | DExponential _ -> "Exponential"
end

lang EmpiricalDist = Dist
  syn Dist =
  -- samples has type [(Float,a)]: A set of weighted samples over type a
  | DEmpirical { samples : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DEmpirical t ->
    match f acc t.samples with (acc, samples) in
    (acc, DEmpirical { t with samples = samples })

  sem distTy info =
  | DEmpirical _ ->
    let n = nameSym "a" in
    let v = TyVar { ident = n, info = info } in
    ([n], [ityseq_ info (tytuple_ [ityfloat_ info, v])], v)

  sem distName =
  | DEmpirical _ -> "Empirical"
end

lang GaussianDist = Dist
  syn Dist =
  | DGaussian { mu : Expr, sigma : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DGaussian t ->
    match f acc t.mu with (acc, mu) in
    match f acc t.sigma with (acc, sigma) in
    (acc, DGaussian { t with mu = mu, sigma = sigma })

  sem distTy info =
  | DGaussian _ ->
    let f = ityfloat_ info in ([], [f, f], f)

  sem distName =
  | DGaussian _ -> "Gaussian"
end

lang BinomialDist = Dist
  syn Dist =
  | DBinomial { n : Expr, p : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DBinomial t ->
    match f acc t.n with (acc, n) in
    match f acc t.p with (acc, p) in
    (acc, DBinomial { t with n = n, p = p })

  sem distTy info =
  | DBinomial _ ->
    let f = ityfloat_ info in
    let i = ityint_ info in
    ([], [i, f], i)

  sem distName =
  | DBinomial _ -> "Binomial"
end

lang WienerDist = Dist
  syn Dist =
  | DWiener { a : Expr }

  sem smapAccumL_Dist_Expr f acc =
  | DWiener t ->
    match f acc t.a with (acc, a) in
    (acc, DWiener { t with a = a })

  sem distTy info =
  | DWiener _ ->
    let f = ityfloat_ info in
    ([], [tyunit_], ityarrow_ info f f)

  sem distName =
  | DWiener _ -> "Wiener"
end

-----------------
-- AST BUILDER --
-----------------

let dist_ = use Dist in
  lam d. TmDist {dist = d, ty = tyunknown_, info = NoInfo ()}

let tydist_ = use Dist in
  lam ty. _tydist ty

let uniform_ = use UniformDist in
  lam a. lam b. dist_ (DUniform {a = a, b = b})

let bern_ = use BernoulliDist in
  lam p. dist_ (DBernoulli {p = p})

let poisson_ = use PoissonDist in
  lam lambda. dist_ (DPoisson {lambda = lambda})

let beta_ = use BetaDist in
  lam a. lam b. dist_ (DBeta {a = a, b = b})

let gamma_ = use GammaDist in
  lam k. lam theta. dist_ (DGamma {k = k, theta = theta})

let categorical_ = use CategoricalDist in
  lam p. dist_ (DCategorical {p = p})

let multinomial_ = use MultinomialDist in
  lam n. lam p. dist_ (DMultinomial {n = n, p = p})

let dirichlet_ = use DirichletDist in
  lam a. dist_ (DDirichlet {a = a})

let exp_ = use ExponentialDist in
  lam rate. dist_ (DExponential {rate = rate})

let empirical_ = use EmpiricalDist in
  lam lst. dist_ (DEmpirical {samples = lst})

let gaussian_ = use GaussianDist in
  lam mu. lam sigma. dist_ (DGaussian {mu = mu, sigma = sigma})

let binomial_ = use BinomialDist in
  lam n. lam p. dist_ (DBinomial {n = n, p = p})

let wiener_ = use WienerDist in dist_ (DWiener { a = unit_ })

---------------------------
-- LANGUAGE COMPOSITIONS --
---------------------------

lang DistAll =
  UniformDist + BernoulliDist + PoissonDist + BetaDist + GammaDist +
  CategoricalDist + MultinomialDist + DirichletDist +  ExponentialDist +
  EmpiricalDist + GaussianDist + BinomialDist + WienerDist
end

lang Test =
  DistAll + MExprAst + MExprPrettyPrint + MExprEq + MExprSym + MExprTypeCheck
  + MExprANF + MExprTypeLift
end

mexpr


use Test in

let tmUniform = uniform_ (float_ 1.0) (float_ 2.0) in
let tmBernoulli = bern_ (float_ 0.5) in
let tmPoisson = poisson_ (float_ 0.5) in
let tmBeta = beta_ (float_ 1.0) (float_ 2.0) in
let tmGamma = gamma_ (float_ 1.0) (float_ 2.0) in
let tmCategorical =
  categorical_ (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]) in
let tmMultinomial =
  multinomial_ (int_ 5) (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]) in
let tmExponential = exp_ (float_ 1.0) in
let tmEmpirical = empirical_ (seq_ [
    utuple_ [float_ 1.0, float_ 1.5],
    utuple_ [float_ 3.0, float_ 1.3]
  ]) in
let tmDirichlet = dirichlet_ (seq_ [float_ 1.3, float_ 1.3, float_ 1.5]) in
let tmGaussian = gaussian_ (float_ 0.0) (float_ 1.0) in
let tmBinomial = binomial_ (int_ 5) (float_ 0.5) in
let tmWiener = wiener_ in

------------------------
-- PRETTY-PRINT TESTS --
------------------------

utest mexprToString tmUniform with strJoin "\n" [
  "Uniform 1. 2."
] using eqString in

utest mexprToString tmBernoulli with strJoin "\n" [
  "Bernoulli 0.5"
] using eqString in

utest mexprToString tmPoisson with strJoin "\n" [
  "Poisson 0.5"
] using eqString in

utest mexprToString tmBeta with strJoin "\n" [
  "Beta 1. 2."
] using eqString in

utest mexprToString tmGamma with strJoin "\n" [
  "Gamma 1. 2."
] using eqString in

utest mexprToString tmCategorical with strJoin "\n" [
  "Categorical [ 0.3, 0.2, 0.5 ]"
] using eqString in

utest mexprToString tmMultinomial with strJoin "\n" [
  "Multinomial 5 [ 0.3, 0.2, 0.5 ]"
] using eqString in

utest mexprToString tmExponential with strJoin "\n" [
  "Exponential 1."
] using eqString in

utest mexprToString tmEmpirical with strJoin "\n" [
  "Empirical [ (1., 1.5),",
  "    (3., 1.3) ]"
] using eqString in

utest mexprToString tmDirichlet with strJoin "\n" [
  "Dirichlet [ 1.3, 1.3, 1.5 ]"
] using eqString in

utest mexprToString tmGaussian with strJoin "\n" [
  "Gaussian 0. 1."
] using eqString in

utest mexprToString tmBinomial with strJoin "\n" [
  "Binomial 5 0.5"
] using eqString in

utest mexprToString tmWiener with "Wiener {}"
using eqString in

--------------------
-- EQUALITY TESTS --
--------------------

utest tmUniform with tmUniform using eqExpr in
utest eqExpr tmUniform (uniform_ (float_ 1.0) (float_ 1.0)) with false in

utest tmBernoulli with tmBernoulli using eqExpr in
utest eqExpr tmBernoulli (bern_ (float_ 0.4)) with false in

utest tmPoisson with tmPoisson using eqExpr in
utest eqExpr tmPoisson (poisson_ (float_ 0.4)) with false in

utest tmBeta with tmBeta using eqExpr in
utest eqExpr tmBeta (beta_ (float_ 1.0) (float_ 1.0)) with false in

utest tmGamma with tmGamma using eqExpr in
utest eqExpr tmGamma (gamma_ (float_ 1.0) (float_ 1.0)) with false in

utest tmCategorical with tmCategorical using eqExpr in
utest eqExpr tmCategorical
  (categorical_ (seq_ [float_ 0.2, float_ 0.2, float_ 0.5])) with false in
utest eqExpr tmCategorical
  (categorical_ (seq_ [float_ 0.3, float_ 0.2, float_ 0.6])) with false in

utest tmMultinomial with tmMultinomial using eqExpr in
utest eqExpr tmMultinomial
  (multinomial_ (int_ 4) (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]))
with false in
utest eqExpr tmMultinomial
  (multinomial_ (int_ 5) (seq_ [float_ 0.3, float_ 0.3, float_ 0.5]))
with false in

utest tmExponential with tmExponential using eqExpr in
utest eqExpr tmExponential (exp_ (float_ 1.1)) with false in

utest tmEmpirical with tmEmpirical using eqExpr in
utest eqExpr tmEmpirical (empirical_ (seq_ [
    utuple_ [float_ 2.0, float_ 1.5],
    utuple_ [float_ 3.0, float_ 1.3]
  ]))
with false in
utest eqExpr tmEmpirical (empirical_ (seq_ [
    utuple_ [float_ 2.0, float_ 1.5],
    utuple_ [float_ 3.0, float_ 1.4]
  ]))
with false in

utest tmDirichlet with tmDirichlet using eqExpr in
utest eqExpr tmDirichlet
  (dirichlet_ (seq_ [float_ 1.2, float_ 0.5, float_ 1.5])) with false in

utest tmGaussian with tmGaussian using eqExpr in
utest eqExpr tmGaussian
  (gaussian_ (float_ 1.0) (float_ 1.0)) with false in

utest tmBinomial with tmBinomial using eqExpr in
utest eqExpr tmBinomial (binomial_ (int_ 4) (float_ 0.5)) with false in

utest tmWiener with tmWiener using eqExpr in
utest eqExpr tmWiener tmBinomial with false in

----------------------
-- SMAP/SFOLD TESTS --
----------------------

let tmVar = var_ "x" in
let mapVar = (lam. tmVar) in
let foldToSeq = lam a. lam e. cons e a in

utest smap_Expr_Expr mapVar tmUniform with uniform_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmUniform
with [ float_ 2.0, float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmBernoulli with bern_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmBernoulli
with [ float_ 0.5 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmPoisson with poisson_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmPoisson
with [ float_ 0.5 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmBeta with beta_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmBeta
with [ float_ 2.0, float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmGamma with gamma_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmGamma
with [ float_ 2.0, float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmCategorical with categorical_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmCategorical
with [ seq_ [float_ 0.3, float_ 0.2, float_ 0.5] ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmMultinomial
with multinomial_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmMultinomial
with [ seq_ [float_ 0.3, float_ 0.2, float_ 0.5], int_ 5 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmExponential with exp_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmExponential
with [ float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmEmpirical with empirical_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmEmpirical
with [
  seq_ [ utuple_ [float_ 1.0, float_ 1.5], utuple_ [float_ 3.0, float_ 1.3] ]
] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmDirichlet with dirichlet_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmDirichlet
with [ seq_ [float_ 1.3, float_ 1.3, float_ 1.5] ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmGaussian with gaussian_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmGaussian
with [ float_ 1.0, float_ 0.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmBinomial with binomial_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmBinomial
with [ float_ 0.5, int_ 5] using eqSeq eqExpr in

---------------------
-- SYMBOLIZE TESTS --
---------------------

utest symbolize tmUniform with tmUniform using eqExpr in
utest symbolize tmBernoulli with tmBernoulli using eqExpr in
utest symbolize tmPoisson with tmPoisson using eqExpr in
utest symbolize tmBeta with tmBeta using eqExpr in
utest symbolize tmGamma with tmGamma using eqExpr in
utest symbolize tmCategorical with tmCategorical using eqExpr in
utest symbolize tmMultinomial with tmMultinomial using eqExpr in
utest symbolize tmExponential with tmExponential using eqExpr in
utest symbolize tmEmpirical with tmEmpirical using eqExpr in
utest symbolize tmDirichlet with tmDirichlet using eqExpr in
utest symbolize tmGaussian with tmGaussian using eqExpr in
utest symbolize tmBinomial with tmBinomial using eqExpr in


-------------------------
-- TYPE-ANNOTATE TESTS --
-------------------------

utest tyTm (typeCheck tmUniform) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmBernoulli) with tydist_ tybool_ using eqType in
utest tyTm (typeCheck tmPoisson) with tydist_ tyint_ using eqType in
utest tyTm (typeCheck tmBeta) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmGamma) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmCategorical) with tydist_ tyint_ using eqType in
utest tyTm (typeCheck tmMultinomial) with tydist_ (tyseq_ tyint_) using eqType in
utest tyTm (typeCheck tmExponential) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmEmpirical) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmDirichlet) with tydist_ (tyseq_ tyfloat_) using eqType in
utest tyTm (typeCheck tmGaussian) with tydist_ tyfloat_ using eqType in
utest tyTm (typeCheck tmBinomial) with tydist_ tyint_ using eqType in
utest tyTm (typeCheck tmWiener) with tydist_ (tyarrow_ tyfloat_ tyfloat_)
  using eqType
in

---------------
-- ANF TESTS --
---------------

let toStr = utestDefaultToString expr2str expr2str in

let _anf = compose normalizeTerm symbolize in

utest _anf tmUniform with bind_ (ulet_ "t" tmUniform) (var_ "t") using eqExpr in
utest _anf tmBernoulli with bind_ (ulet_ "t" tmBernoulli) (var_ "t") using eqExpr in
utest _anf tmPoisson with bind_ (ulet_ "t" tmPoisson) (var_ "t") using eqExpr in
utest _anf tmBeta with bind_ (ulet_ "t" tmBeta) (var_ "t") using eqExpr in
utest _anf tmGamma with bind_ (ulet_ "t" tmGamma) (var_ "t") using eqExpr in
utest _anf tmCategorical with bindall_ [
  ulet_ "t" (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]),
  ulet_ "t1" (categorical_ (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmMultinomial with bindall_ [
  ulet_ "t" (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]),
  ulet_ "t1" (multinomial_ (int_ 5) (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmExponential with bind_ (ulet_ "t" tmExponential) (var_ "t") using eqExpr in
utest _anf tmEmpirical with bindall_ [
  ulet_ "t" (utuple_ [float_ 1.0, float_ 1.5]),
  ulet_ "t1" (utuple_ [float_ 3.0, float_ 1.3]),
  ulet_ "t2" (seq_ [(var_ "t"), (var_ "t1")]),
  ulet_ "t3" (empirical_ (var_ "t2")),
  var_ "t3"
] using eqExpr else toStr in
-- print (mexprToString (_anf tmEmpirical)); print "\n";
utest _anf tmDirichlet with bindall_ [
  ulet_ "t" (seq_ [float_ 1.3, float_ 1.3, float_ 1.5]),
  ulet_ "t1" (dirichlet_ (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmGaussian with bind_ (ulet_ "t" tmGaussian) (var_ "t") using eqExpr in
utest _anf tmBinomial with bind_ (ulet_ "t" tmBinomial) (var_ "t") using eqExpr in
utest _anf tmWiener with bind_ (ulet_ "t" tmWiener) (var_ "t") using eqExpr in

---------------------
-- TYPE-LIFT TESTS --
---------------------

utest (typeLift tmUniform).1 with tmUniform using eqExpr in
utest (typeLift tmBernoulli).1 with tmBernoulli using eqExpr in
utest (typeLift tmPoisson).1 with tmPoisson using eqExpr in
utest (typeLift tmBeta).1 with tmBeta using eqExpr in
utest (typeLift tmGamma).1 with tmGamma using eqExpr in
utest (typeLift tmCategorical).1 with tmCategorical using eqExpr in
utest (typeLift tmMultinomial).1 with tmMultinomial using eqExpr in
utest (typeLift tmExponential).1 with tmExponential using eqExpr in
utest (typeLift tmEmpirical).1 with tmEmpirical using eqExpr in
utest (typeLift tmDirichlet).1 with tmDirichlet using eqExpr in
utest (typeLift tmGaussian).1 with tmGaussian using eqExpr in
utest (typeLift tmBinomial).1 with tmBinomial using eqExpr in
utest (typeLift tmWiener).1 with tmWiener using eqExpr in

()

