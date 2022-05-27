include "../dists.mc"
include "../../inference-common/smc.mc"
include "mexpr/ast-builder.mc"

lang MExprPPLTraceMCMC = MExprPPL + Resample + TransformDist

  -- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr
  -- AST types here. Optimally, the type would be Options -> CorePPLExpr ->
  -- MExprExpr or similar.
  sem compile : Expr -> Expr
  sem compile =
  | t ->

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformProb t in

    t

  sem transformProb =
  | TmAssume t -> withInfo t.info (app_ (var_ "sampleMH" ) t.dist)

  -- NOTE(dlunde,2022-05-16): Note that we cannot stop immediately when the
  -- weight becomes 0 (-inf in log-space). For this, we need CPS, PCFGs, or
  -- maybe some type of exception handler.
  | TmObserve t ->
    let weight = withInfo t.info (app_ (recordproj_ "logObserve" t.dist) t.value) in
    withInfo t.info (appf1_ (var_ "updateWeight") weight)
  | TmWeight t ->
    withInfo t.info (appf1_ (var_ "updateWeight") t.weight)
  | TmResample t -> withInfo t.info unit_
  | t -> t

end

let compilerTraceMCMC = use MExprPPLTraceMCMC in
  ("trace-mcmc/runtime.mc", compile)
