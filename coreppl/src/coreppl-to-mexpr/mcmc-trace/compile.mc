include "../dists.mc"
include "../inference-interface.mc"
include "../../inference/smc.mc"
include "../../dppl-arg.mc"
include "mexpr/ast-builder.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLTraceMCMC = MExprPPL + Resample + TransformDist + PhaseStats + InferenceInterface

  ----------------
  -- TRACE MCMC --
  ----------------

  -- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr
  -- AST types here. Optimally, the type would be Options -> CorePPLExpr ->
  -- MExprExpr or similar.
  sem compile : InferenceInterface -> Expr
  sem compile =
  | x ->
    let log = mkPhaseLogState x.options.debugDumpPhases x.options.debugPhases in
    let t = x.extractNormal () in
    endPhaseStatsExpr log "extract-normal-one" t;

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr (transformTmDist x.dists) t in
    endPhaseStatsExpr log "transform-tm-dist-one" t;

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr (transformProb x.dists x.runtime) t in
    endPhaseStatsExpr log "transform-prob-one" t;

    t

  sem transformProb env runtime =
  | TmAssume t ->
    let i = withInfo t.info in
    i (appFromEnv runtime "sampleMH" [t.dist])

  -- NOTE(dlunde,2022-05-16): Note that we cannot stop immediately when the
  -- weight becomes 0 (-inf in log-space). For this, we need CPS, PCFGs, or
  -- maybe some type of exception handler.
  | TmResample t -> withInfo t.info unit_
  | TmObserve t ->
    let i = withInfo t.info in
    let weight = i (appFromEnv env "logObserve" [t.dist, t.value]) in
    i (appFromEnv runtime "updateWeight" [weight])
  | TmWeight t ->
    let i = withInfo t.info in
    i (appFromEnv runtime "updateWeight" [t.weight])
  | t -> t


  ----------------------
  -- TRACE MCMC (CPS) --
  ----------------------

  -- TODO(dlunde,2022-08-22)

end

let compilerTraceMCMC = lam options. use MExprPPLTraceMCMC in
  ("mcmc-trace/runtime.mc", compile)
