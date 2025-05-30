include "../dists.mc"
include "../inference-interface.mc"
include "../../inference/smc.mc"
include "../../dppl-arg.mc"
include "mexpr/ast-builder.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLTraceMCMC = MExprPPL + Resample + TransformDist + PhaseStats + InferenceInterface + TraceMCMCMethod

  ----------------
  -- TRACE MCMC --
  ----------------

  -- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr
  -- AST types here. Optimally, the type would be Options -> CorePPLExpr ->
  -- MExprExpr or similar.
  sem compile : TraceMCMCConfig -> InferenceInterface -> Expr
  sem compile config =
  | x ->
    let log = mkPhaseLogState x.options.debugDumpPhases x.options.debugPhases in
    let t = x.extractNormal (lam x. x) in
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
end

lang TraceMCMCCompilerPicker = TraceMCMCMethod
  -- NOTE(vipa, 2025-04-04): We always get the same runtimes, thus we
  -- look at nothing here
  sem _cmpInferMethod = | (TraceMCMC _, TraceMCMC _) -> 0

  sem pickRuntime = | TraceMCMC _ -> ("mcmc-trace/runtime.mc", mapEmpty cmpString)

  sem pickCompiler = | TraceMCMC x -> use MExprPPLTraceMCMC in compile x
end
