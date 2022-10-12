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

  -- TODO(larshum, 2022-10-12): This code assumes the MLang transformation
  -- performs name mangling as of writing. Therefore, it will break after we
  -- make the change.
  sem transformDist: (Expr -> Expr) -> Dist -> Expr
  sem transformDist i =
  | DGamma { k = k, theta = theta } ->
    i (conapp_
        "RuntimeDist_DistGamma"
        (i (urecord_ [("shape", k), ("scale", theta)])))
  | DExponential { rate = rate } ->
    i (conapp_
        "RuntimeDist_DistExponential"
        (i (urecord_ [("rate", rate)])))
  | DPoisson { lambda = lambda } ->
    i (conapp_
        "RuntimeDist_DistPoisson"
        (i (urecord_ [("lambda", lambda)])))
  | DBinomial { n = n, p = p } ->
    i (conapp_
        "RuntimeDist_DistBinomial"
        (i (urecord_ [("n", n), ("p", p)])))
  | DBernoulli { p = p } ->
    i (conapp_
        "RuntimeDist_DistBernoulli"
        (i (urecord_ [("p", p)])))
  | DBeta { a = a, b = b } ->
    i (conapp_
        "RuntimeDist_DistBeta"
        (i (urecord_ [("a", a), ("b", b)])))
  | DGaussian { mu = mu, sigma = sigma } ->
    i (conapp_
        "RuntimeDist_DistGaussian"
        (i (urecord_ [("mu", mu), ("sigma", sigma)])))
  | DMultinomial { n = n, p = p } ->
    i (conapp_
        "RuntimeDist_DistMultinomial"
        (i (urecord_ [("n", n), ("p", p)])))
  | DCategorical { p = p } ->
    i (conapp_
        "RuntimeDist_DistCategorical"
        (i (urecord_ [("p", p)])))
  | DUniform { a = a, b = b } ->
    i (conapp_
        "RuntimeDist_DistUniform"
        (i (urecord_ [("a", a), ("b", b)])))

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
