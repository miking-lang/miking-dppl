include "../dists.mc"
include "../inference-interface.mc"
include "../../inference/smc.mc"
include "../../cfa.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLAPF =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA
  + SMCCommon + PhaseStats + InferenceInterface + APFMethod

  sem compile: APFConfig -> InferenceInterface -> Expr
  sem compile config =
  | x ->
    let log = mkPhaseLogState x.options.debugDumpPhases x.options.debugPhases in
    let t = x.extractNoHigherOrderConsts (lam x. x) in
    endPhaseStatsExpr log "extract-no-higher-order-consts-one" t;

    -- Automatic resampling annotations
    let t =
      match config.resample with "likelihood" then addResample (lam. true) t
      else match config.resample with "manual" then t
      else match config.resample with "align"  then

        -- Do static analysis for stochastic value flow and alignment
        let unaligned: Set Name = extractUnaligned (alignCfa t) in
        let isAligned: Name -> Bool = lam n. not (setMem n unaligned) in

        addResample isAligned t

      else error "Invalid resample option"
    in
    endPhaseStatsExpr log "resample-one" t;

    -- Static analysis and CPS transformation
    let t =
      let nEnd = _getConExn "End" x.runtime.env in
      let cont = (ulam_ "x" (nconapp_ nEnd (var_ "x"))) in
      match config.cps with "partial" then
        let checkpoint = lam t.
          match t with TmLet { ident = ident, body = body } then
            match body with TmResample _ then true else false
          else
            errorSingle [infoTm t] "Impossible"
        in
        let checkPointNames: Set Name = extractCheckpoint (checkpointCfa checkpoint t) in
        cpsPartialCont (lam n. setMem n checkPointNames) cont t
      else match config.cps with "full" then
        cpsFullCont cont t
      else
        error (join ["Invalid CPS option:", config.cps])
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

lang APFCompilerPicker = APFMethod
  -- NOTE(vipa, 2025-04-04): Nothing changes the runtimes we fetch,
  -- thus we look at nothing here
  sem _cmpInferMethod = | (APF _, APF _) -> 0

  sem pickRuntime = | APF _ -> ("smc-apf/runtime.mc", mapEmpty cmpString)

  sem pickCompiler = | APF x -> use MExprPPLAPF in compile x
end

let compilerAPF = lam options. use MExprPPLAPF in
  ("smc-apf/runtime.mc", compile)
