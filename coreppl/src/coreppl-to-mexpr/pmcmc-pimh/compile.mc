include "../dists.mc"
include "../../inference/smc.mc"
include "../../cfa.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLPIMH =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA +
  SMCCommon + PhaseStats

  sem compile: Options -> (Expr,Expr) -> Expr
  sem compile options =
  | (_,t) ->
    let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in
    -- Static analysis and CPS transformation
    let t =
      let cont = (ulam_ "x" (conapp_ "End" (var_ "x"))) in
      match options.cps with "partial" then
        let checkpoint = lam t.
          match t with TmLet { ident = ident, body = body } then
            match body with TmResample _ then true else false
          else
            errorSingle [infoTm t] "Impossible"
        in
        let checkPointNames: Set Name = extractCheckpoint (checkpointCfa checkpoint t) in
        cpsPartialCont (lam n. setMem n checkPointNames) cont t
      else match options.cps with "full" then
        cpsFullCont cont t
      else
        error (join ["Invalid CPS option:", options.cps])
    in
    endPhaseStatsExpr log "cps-one" t;
    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in
    endPhaseStatsExpr log "transform-tm-dist-one" t;
    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformProb t in
    endPhaseStatsExpr log "transform-prob-one" t;
    t
end

let compilerPIMH = lam options. use MExprPPLPIMH in
  ("pmcmc-pimh/runtime.mc", compile options)
