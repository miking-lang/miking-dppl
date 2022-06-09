-- Functions for converting between CorePPL distribution and MExpr
-- runtime distributions. The MExpr distributions are defined in the runtime file
-- common/dists.mc

include "../coreppl.mc"
include "mexpr/ast-builder.mc"

-- TODO(dlunde,2022-05-11): The common case where the user writes, e.g., assume
-- (Bernoulli x), can also be optimized to not create an intermediate record.
lang TransformDist = MExprPPL
  sem transformTmDist: Expr -> Expr
  sem transformTmDist =
  | TmDist t -> transformDist (withInfo t.info) t.dist
  | t -> t

  sem transformDist: (Expr -> Expr) -> Dist -> Expr
  sem transformDist i =
  | DGamma { k = k, theta = theta } -> i (appf2_ (i (var_ "distGamma")) k theta)
  | DExponential { rate = rate } -> i (appf1_ (i (var_ "distExponential")) rate)
  | DPoisson { lambda = lambda } -> i (appf1_ (i (var_ "distPoisson")) lambda)
  | DBinomial { n = n, p = p } -> i (appf2_ (i (var_ "distBinomial")) n p)
  | DBernoulli { p = p } -> i (appf1_ (i (var_ "distBernoulli")) p)
  | DBeta { a = a, b = b } -> i (appf2_ (i (var_ "distBeta")) a b)
  | DGaussian { mu = mu, sigma = sigma } -> i (appf2_ (i (var_ "distGaussian")) mu sigma)
  | DMultinomial { n = n, p = p } -> i (appf2_ (i (var_ "distMultinomial")) n p)
  | DCategorical { p = p } -> i (appf1_ (i (var_ "distCategorical")) p)
  | DDirichlet { a = a } -> i (appf1_ (i (var_ "distDirichlet")) a)
  | DUniform { a = a, b = b } -> i (appf2_ (i (var_ "distUniform")) a b)

  -- We need to remove TyDist after transforming to MExpr dists (the new MExpr
  -- types will be inferred by the type checker)
  sem removeTyDist: Expr -> Expr
  sem removeTyDist =
  | e ->
    recursive let stripTyDist: Type -> Type =
      lam ty.
        match ty with TyDist t then tyWithInfo t.info tyunknown_
        else smap_Type_Type stripTyDist ty
    in
    let e = smap_Expr_Type stripTyDist e in
    smap_Expr_Expr removeTyDist e
end
