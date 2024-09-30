-- Functions for converting between CorePPL distribution and MExpr
-- runtime distributions. The MExpr distributions are defined in the runtime file
-- common/dists.mc

include "../coreppl.mc"
include "../ad.mc"
include "mexpr/ast-builder.mc"

-- TODO(dlunde,2022-05-11): The common case where the user writes, e.g., assume
-- (Bernoulli x), can also be optimized to not create an intermediate record.
lang TransformDist = MExprPPL + DualNumDist
  sem transformTmDist: Expr -> Expr
  sem transformTmDist =
  | TmDist t -> transformDist (withInfo t.info) t.dist
  | TmConst {val = c &
      ( CDistEmpiricalSamples _
      | CDistEmpiricalDegenerate _
      | CDistEmpiricalNormConst _
      | CDistEmpiricalAcceptRate _
      | CDistExpectation _
      )
    } ->
    var_ (getConstStringCode 0 c)
  | t -> t

  -- TODO(larshum, 2022-10-12): This code assumes the MLang transformation
  -- performs name mangling as of writing. Therefore, it will break after we
  -- make the change.
  sem transformDist: (Expr -> Expr) -> Dist -> Expr
  sem transformDist i =
  | DGamma { k = k, theta = theta } ->
    i (conapp_
        "RuntimeDistElementary_DistGamma"
        (i (urecord_ [("shape", k), ("scale", theta)])))
  | DExponential { rate = rate } ->
    i (conapp_
        "RuntimeDistElementary_DistExponential"
        (i (urecord_ [("rate", rate)])))
  | DPoisson { lambda = lambda } ->
    i (conapp_
        "RuntimeDistElementary_DistPoisson"
        (i (urecord_ [("lambda", lambda)])))
  | DBinomial { n = n, p = p } ->
    i (conapp_
        "RuntimeDistElementary_DistBinomial"
        (i (urecord_ [("n", n), ("p", p)])))
  | DBernoulli { p = p } ->
    i (conapp_
        "RuntimeDistElementary_DistBernoulli"
        (i (urecord_ [("p", p)])))
  | DBeta { a = a, b = b } ->
    i (conapp_
        "RuntimeDistElementary_DistBeta"
        (i (urecord_ [("a", a), ("b", b)])))
  | DGaussian { mu = mu, sigma = sigma } ->
    i (conapp_
        "RuntimeDistElementary_DistGaussian"
        (i (urecord_ [("mu", mu), ("sigma", sigma)])))
  | DMultinomial { n = n, p = p } ->
    i (conapp_
        "RuntimeDistElementary_DistMultinomial"
        (i (urecord_ [("n", n), ("p", p)])))
  | DDirichlet { a = a } ->
    i (conapp_
        "RuntimeDistElementary_DistDirichlet"
        (i (urecord_ [("a", a)])))
  | DCategorical { p = p } ->
    i (conapp_
        "RuntimeDistElementary_DistCategorical"
        (i (urecord_ [("p", p)])))
  | DUniform { a = a, b = b } ->
    i (conapp_
        "RuntimeDistElementary_DistUniform"
         (i (urecord_ [("a", a), ("b", b)])))
  | DWiener {} ->
    i (conapp_
         "RuntimeDistElementary_DistWiener" (i unit_))
  | DEmpirical { samples = samples } ->
    i (app_ (var_ "vRuntimeDistEmpirical_constructDistEmpiricalHelper") samples)
  | DDual d ->
    i (conapp_ "RuntimeDistElementaryDual_DistDual" (transformDist i d))

  -- We need to replace occurrences of TyDist after transforming to MExpr
  -- distributions.
  sem replaceTyDist: Expr -> Expr
  sem replaceTyDist =
  | t ->
    let t = smap_Expr_Type toRuntimeTyDist t in
    let t = smap_Expr_TypeLabel toRuntimeTyDist t in
    let t = smap_Expr_Pat replaceTyDistPat t in
    let t = smap_Expr_Expr replaceTyDist t in
    withType (toRuntimeTyDist (tyTm t)) t

  sem toRuntimeTyDist : Type -> Type
  sem toRuntimeTyDist =
  | TyDist t ->
    TyApp {
      lhs = TyCon {ident = nameNoSym "RuntimeDistBase_Dist", info = t.info, data = tyunknown_},
      --lhs = TyCon {ident = nameNoSym "Dist", info = t.info},
      rhs = t.ty,
      info = t.info}
  | ty -> smap_Type_Type toRuntimeTyDist ty

  sem replaceTyDistPat : Pat -> Pat
  sem replaceTyDistPat =
  | p ->
    let p = smap_Pat_Pat replaceTyDistPat p in
    withTypePat (toRuntimeTyDist (tyPat p)) p
end
