include "../dists.mc"
include "../inference-interface.mc"
include "../../inference/smc.mc"
include "../../dppl-arg.mc"
include "mexpr/ast-builder.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLNaiveMCMC = MExprPPL + Resample + TransformDist + PhaseStats + InferenceInterface + NaiveMCMCMethod

  ----------------
  -- NAIVE MCMC --
  ----------------

  -- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr
  -- AST types here. Optimally, the type would be Options -> CorePPLExpr ->
  -- MExprExpr or similar.
  sem compile : NaiveMCMCConfig -> InferenceInterface -> Expr
  sem compile config =
  | x ->
    let log = mkPhaseLogState x.options.debugDumpPhases x.options.debugPhases (lam. []) in  -- NOTE(vipa, 2026-03-10): These fragments aren't built to be extended, meaning they won't get the fragments needed to process the invariants, thus we process no invariants here
    let t = x.extractNormal (lam x. x) in
    endPhaseStatsExpr log "extract-normal-one" t;

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr (transformTmDist x.dists) t in
    endPhaseStatsExpr log "transform-tm-dist-one" t;

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr (transformProb x.stateName x.dists x.runtime) t in
    endPhaseStatsExpr log "transform-prob-one" t;

    t

  sem transformProb stateName env runtime =
  | TmAssume t ->
    let i = withInfo t.info in
    i (appFromEnv env "sample" [t.dist])
  | TmResample t -> withInfo t.info unit_
  | TmObserve t ->
    let i = withInfo t.info in
    let weight = i (appFromEnv env "logObserve" [t.dist, t.value]) in
    i (appFromEnv runtime "updateWeight" [weight, i (nvar_ stateName)])
  | TmWeight t ->
    let i = withInfo t.info in
    i (appFromEnv runtime "updateWeight" [t.weight, i (nvar_ stateName)])
  | t -> t
end

lang NaiveMCMCCompilerPicker = NaiveMCMCMethod
  sem _cmpInferMethod = | (NaiveMCMC _, NaiveMCMC _) -> 0

  sem pickRuntime = | NaiveMCMC _ -> ("mcmc-naive/runtime.mc", mapEmpty cmpString)

  sem pickCompiler = | NaiveMCMC x ->
    use MExprPPLNaiveMCMC in compile x
end
