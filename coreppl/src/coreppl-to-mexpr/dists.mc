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
  | TmDist t -> withInfo t.info (transformDist t.dist)
  | t -> t

  sem transformDist: Dist -> Expr
  sem transformDist =
  | DGamma { k = k, theta = theta } -> appf2_ (var_ "distGamma") k theta
  | DExponential { rate = rate } -> appf1_ (var_ "distExponential") rate
  | DPoisson { lambda = lambda } -> appf1_ (var_ "distPoisson") lambda
  | DBinomial { n = n, p = p } -> appf2_ (var_ "distBinomial") n p
  | DBernoulli { p = p } -> appf1_ (var_ "distBernoulli") p
  | DBeta { a = a, b = b } -> appf2_ (var_ "distBeta") a b
  | DGaussian { mu = mu, sigma = sigma } -> appf2_ (var_ "distGaussian") mu sigma
  | DMultinomial { n = n, p = p } -> appf2_ (var_ "distMultinomial") n p
  | DCategorical { p = p } -> appf1_ (var_ "distCategorical") p
  | DDirichlet { a = a } -> appf1_ (var_ "distDirichlet") a
  | DUniform { a = a, b = b } -> appf2_ (var_ "distUniform") a b
end
