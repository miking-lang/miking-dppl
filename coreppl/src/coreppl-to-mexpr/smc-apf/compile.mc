include "../dists.mc"
include "../inference-interface.mc"
include "../../inference/smc.mc"
include "../../cfa.mc"
include "../pruning/compile.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLAPF =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA
  + SMCCommon + PhaseStats + InferenceInterface + DPPLPruning + APFMethod

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

    let t = match mapLookup "prune" x.extraEnvs with Some pruneEnv
      then prune pruneEnv t
      else t in
    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr (transformTmDist x.dists) t in
    endPhaseStatsExpr log "transform-tm-dist-one" t;
    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr (transformProb x.stateName x.dists x.runtime) t in
    endPhaseStatsExpr log "transform-prob-one" t;
    t
end

lang APFCompilerPicker = APFMethod
  -- NOTE(vipa, 2025-04-04): We fetch a prune runtime if pruning is
  -- set, thus we only compare prune here
  sem _cmpInferMethod = | (APF a, APF b) ->
    cmpBool a.prune b.prune

  sem pickRuntime = | APF x ->
    let extras = mapEmpty cmpString in
    let extras = if x.prune
      then mapInsert "prune" "pruning/runtime.mc" extras
      else extras in
    ("smc-apf/runtime.mc", extras)

  sem pickCompiler = | APF x -> use MExprPPLAPF in compile x
end
