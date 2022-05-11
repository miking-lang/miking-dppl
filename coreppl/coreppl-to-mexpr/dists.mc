-- Functions for converting between CorePPL distribution and MExpr
-- runtime distributions. The MExpr distributions are defined in the runtime file
-- common/dists.mc

include "mexpr/ast-builder.mc"

let dist = use MExprPPL in lam sampleFun. lam obsFun. lam args.
  let args: [(Name, Expr)] = map (lam arg. (nameSym "arg", arg)) args in
  let argLets = map (lam arg. nulet_ arg.0 arg.1) args in
  let argNameVars = map (lam arg. nvar_ arg.0) args in
  bindall_ (concat argLets [
    (urecord_ [
      ("sample",
       ulam_ "x" (appSeq_ (var_ sampleFun) argNameVars)),
      ("logObserve",
       ulam_ "obs" (appSeq_ (var_ obsFun) (snoc argNameVars (var_ "obs"))))
    ])
  ])

-- TODO(dlunde,2022-05-11): The common case where the user writes, e.g., assume
-- (Bernoulli x), can also be optimized to not create an intermediate record.
lang TransformDist = MExprPPL
  sem transformTmDist: Expr -> Expr
  sem transformTmDist =
  | TmDist t -> withInfo t.info (transformDist t.dist)
  | t -> t

  sem transformDist: Dist -> Expr
  sem transformDist =
  | DBinomial { n = n, p = p } -> dist "binomialSample" "binomialLogPmf" [p, n]
  | DBernoulli { p = p } -> dist "bernoulliSample" "bernoulliLogPmf" [p]
  | DBeta { a = a, b = b } -> dist "betaSample" "betaLogPdf" [a, b]
  | DGaussian { mu = mu, sigma = sigma } -> dist "gaussianSample" "gaussianLogPdf" [mu, sigma]
  | DMultinomial { n = n, p = p } -> dist "multinomialSample" "multinomialLogPmf" [n, p]
  | DCategorical { p = p } -> dist "categoricalSample" "categoricalLogPmf" [p]
  | DDirichlet { a = a } -> dist "dirichletSample" "dirichletLogPdf" [a]
  | DUniform { a = a, b = b } -> dist "uniformContinuousSample" "uniformContinuousLogPdf" [a,b]
end
