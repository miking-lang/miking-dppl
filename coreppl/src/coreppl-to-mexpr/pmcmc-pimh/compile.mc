include "../dists.mc"
include "../inference-interface.mc"
include "../../inference/smc.mc"
include "../../cfa.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLPIMH =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA +
  SMCCommon + PhaseStats + InferenceInterface

  sem compile: InferenceInterface -> Expr
  sem compile =
  | x ->
    let log = mkPhaseLogState x.options.debugDumpPhases x.options.debugPhases in
    let t = x.extractNoHigherOrderConsts () in
    endPhaseStatsExpr log "extract-no-higher-order-consts-one" t;
    -- Static analysis and CPS transformation
    let t =
      let cEnd = _getConExn "End" x.runtime.env in
      let cont = (ulam_ "x" (nconapp_ cEnd (var_ "x"))) in
      match x.options.cps with "partial" then
        let checkpoint = lam t.
          match t with TmLet { ident = ident, body = body } then
            match body with TmResample _ then true else false
          else
            errorSingle [infoTm t] "Impossible"
        in
        let checkPointNames: Set Name = extractCheckpoint (checkpointCfa checkpoint t) in
        cpsPartialCont (lam n. setMem n checkPointNames) cont t
      else match x.options.cps with "full" then
        cpsFullCont cont t
      else
        error (join ["Invalid CPS option:", x.options.cps])
    in
    endPhaseStatsExpr log "cps-one" t;
    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr (transformTmDist x.dists) t in
    endPhaseStatsExpr log "transform-tm-dist-one" t;
    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr (transformProb x.stateName x.dists x.runtime) t in
    endPhaseStatsExpr log "transform-prob-one" t;
    t
end

let compilerPIMH = lam options. use MExprPPLPIMH in
  ("pmcmc-pimh/runtime.mc", compile)
