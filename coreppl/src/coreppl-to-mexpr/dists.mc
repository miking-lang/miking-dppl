-- Functions for converting between CorePPL distribution and MExpr
-- runtime distributions. The MExpr distributions are defined in the runtime file
-- common/dists.mc

include "../coreppl.mc"
include "mexpr/ast-builder.mc"
include "mlang/loader.mc"
include "inference-interface.mc"

lang TransformDistBase = MExprPPL
  -- Transforms TmDist and constant functions operating on distributions to
  -- their runtime representation.
  sem transformTmDist: InferenceSymEnv -> Expr -> Expr
  sem transformTmDist env =
  | TmDist t ->
    transformTmDistH (withInfo t.info) env (smap_Dist_Expr (transformTmDist env) t.dist)
  | tm & TmConst t -> transformDistConst tm env t.val
  | t -> t

  -- Extensible helper for `transformTmDist`. `transformTmDistH i dist` should
  -- transform distributions `dist`, where `i` sets the info of a term to the
  -- info of the TmDist from where this functions is called.
  sem transformTmDistH: (Expr -> Expr) -> InferenceSymEnv -> Dist -> Expr

  -- Transforms constant functions operating on distributions to their runtime
  -- representation.
  sem transformDistConst : Expr -> InferenceSymEnv -> Const -> Expr
  sem transformDistConst constTm env =
  | _ -> constTm
end

-- TODO(dlunde,2022-05-11): The common case where the user writes, e.g., assume
-- (Bernoulli x), can also be optimized to not create an intermediate record.
lang TransformDist = TransformDistBase + InferenceInterface

  sem transformDistConst constTm env =
  | c & (CDistEmpiricalSamples _
       | CDistEmpiricalDegenerate _
       | CDistEmpiricalNormConst _
       | CDistEmpiricalAcceptRate _
       | CDistExpectation _) ->
    withInfo (infoTm constTm) (appFromEnv env (getConstStringCode 0 c) [])

  -- TODO(larshum, 2022-10-12): This code assumes the MLang transformation
  -- performs name mangling as of writing. Therefore, it will break after we
  -- make the change.
  sem transformTmDistH i env =
  | DGamma { k = k, theta = theta } ->
    let cname = _getConExn "RuntimeDistElementary_DistGamma" env.env in
    i (nconapp_ cname (i (autoty_record_ [("shape", k), ("scale", theta)])))
  | DExponential { rate = rate } ->
    let cname = _getConExn "RuntimeDistElementary_DistExponential" env.env in
    i (nconapp_ cname (i (autoty_record_ [("rate", rate)])))
  | DPoisson { lambda = lambda } ->
    let cname = _getConExn "RuntimeDistElementary_DistPoisson" env.env in
    i (nconapp_ cname (i (autoty_record_ [("lambda", lambda)])))
  | DBinomial { n = n, p = p } ->
    let cname = _getConExn "RuntimeDistElementary_DistBinomial" env.env in
    i (nconapp_ cname (i (autoty_record_ [("n", n), ("p", p)])))
  | DBernoulli { p = p } ->
    let cname = _getConExn "RuntimeDistElementary_DistBernoulli" env.env in
    i (nconapp_ cname (i (autoty_record_ [("p", p)])))
  | DBeta { a = a, b = b } ->
    let cname = _getConExn "RuntimeDistElementary_DistBeta" env.env in
    i (nconapp_ cname (i (autoty_record_ [("a", a), ("b", b)])))
  | DChi2 { df = df } ->
    let cname = _getConExn "RuntimeDistElementary_DistChi2" env.env in
    i (nconapp_ cname (i (autoty_record_ [("df", df)])))
  | DGaussian { mu = mu, sigma = sigma } ->
    let cname = _getConExn "RuntimeDistElementary_DistGaussian" env.env in
    i (nconapp_ cname (i (autoty_record_ [("mu", mu), ("sigma", sigma)])))
  | DGeometric { p = p } ->
    let cname = _getConExn "RuntimeDistElementary_DistGeometric" env.env in
    i (nconapp_ cname (i (autoty_record_ [("p", p)])))
  | DMultinomial { n = n, p = p } ->
    let cname = _getConExn "RuntimeDistElementary_DistMultinomial" env.env in
    i (nconapp_ cname (i (autoty_record_ [("n", n), ("p", p)])))
  | DDirichlet { a = a } ->
    let cname = _getConExn "RuntimeDistElementary_DistDirichlet" env.env in
    i (nconapp_ cname (i (autoty_record_ [("a", a)])))
  | DCategorical { p = p } ->
    let cname = _getConExn "RuntimeDistElementary_DistCategorical" env.env in
    i (nconapp_ cname (i (autoty_record_ [("p", p)])))
  | DUniform { a = a, b = b } ->
    let cname = _getConExn "RuntimeDistElementary_DistUniform" env.env in
    i (nconapp_ cname (i (autoty_record_ [("a", a), ("b", b)])))
  | DReciprocal { a = a, b = b } ->
    let cname = _getConExn "RuntimeDistElementary_DistReciprocal" env.env in
    i (nconapp_ cname (i (autoty_record_ [("a", a), ("b", b)])))
  | DUniformDiscrete { a = a, b = b } ->
    let cname = _getConExn "RuntimeDistElementary_DistUniformDiscrete" env.env in
    i (nconapp_ cname (i (autoty_record_ [("a", a), ("b", b)])))
  | DWiener { cps = cps, a = a } ->
    let cname = _getConExn "RuntimeDistElementary_DistWiener" env.env in
    i (nconapp_ cname
        (i (autoty_record_ [("cps", if cps then i true_ else i false_), ("a", a)])))

  | DEmpirical { samples = samples } ->
    i (appFromEnv env "vRuntimeDistEmpirical_constructDistEmpiricalHelper" [samples])

  -- We need to replace occurrences of TyDist after transforming to MExpr
  -- distributions.
  sem replaceTyDist: InferenceSymEnv -> Expr -> Expr
  sem replaceTyDist env =
  | t ->
    let t = smap_Expr_Type (toRuntimeTyDist env) t in
    let t = smap_Expr_TypeLabel (toRuntimeTyDist env) t in
    let t = smap_Expr_Pat (replaceTyDistPat env) t in
    let t = smap_Expr_Expr (replaceTyDist env) t in
    withType (toRuntimeTyDist env (tyTm t)) t

  sem replaceTyDistDecl : InferenceSymEnv -> Decl -> Decl
  sem replaceTyDistDecl env =
  | d ->
    let d = smap_Decl_Expr (replaceTyDist env) d in
    let d = smap_Decl_Type (toRuntimeTyDist env) d in
    d

  sem toRuntimeTyDist : InferenceSymEnv -> Type -> Type
  sem toRuntimeTyDist env =
  | TyDist t ->
    -- let cname = _getTyConExn "RuntimeDistBase_Dist" env.env in
    let cname = _getTyConExn "Dist" env.env in
    TyApp {
      lhs = TyCon {ident = cname, info = t.info, data = tyunknown_},
      rhs = t.ty,
      info = t.info}
  | ty -> smap_Type_Type (toRuntimeTyDist env) ty

  sem replaceTyDistPat : InferenceSymEnv -> Pat -> Pat
  sem replaceTyDistPat env =
  | p ->
    let p = smap_Pat_Pat (replaceTyDistPat env) p in
    withTypePat (toRuntimeTyDist env (tyPat p)) p
end
