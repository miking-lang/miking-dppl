include "../dists.mc"
include "../../inference/smc.mc"
include "../../dppl-arg.mc"
include "mexpr/ast-builder.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLTraceMCMC = MExprPPL + Resample + TransformDist + PhaseStats

  ----------------
  -- TRACE MCMC --
  ----------------

  -- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr
  -- AST types here. Optimally, the type would be Options -> CorePPLExpr ->
  -- MExprExpr or similar.
  sem compile : Options -> (Expr,Expr) -> Expr
  sem compile options =
  | (t,_) ->
    let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in
    endPhaseStatsExpr log "transform-tm-dist-one" t;

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformProb t in
    endPhaseStatsExpr log "transform-prob-one" t;

    t

  sem transformProb =
  | TmAssume t ->
    let i = withInfo t.info in
    i (app_ (i (var_ "sampleMH")) t.dist)

  -- NOTE(dlunde,2022-05-16): Note that we cannot stop immediately when the
  -- weight becomes 0 (-inf in log-space). For this, we need CPS, PCFGs, or
  -- maybe some type of exception handler.
  | TmObserve t ->
    let i = withInfo t.info in
    let weight = i (appf2_ (i (var_ "logObserve")) t.dist t.value) in
    i (appf1_ (i (var_ "updateWeight")) weight)
  | TmWeight t ->
    let i = withInfo t.info in
    i (appf1_ (i (var_ "updateWeight")) t.weight)
  | TmResample t -> withInfo t.info unit_
  | t -> t


  ----------------------
  -- TRACE MCMC (CPS) --
  ----------------------

  -- TODO(dlunde,2022-08-22)

end

let compilerTraceMCMC = lam options. use MExprPPLTraceMCMC in
  ("mcmc-trace/runtime.mc", compile options)
