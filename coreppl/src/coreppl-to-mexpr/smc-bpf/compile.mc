include "../dists.mc"
include "../inference-interface.mc"
include "../../inference/smc.mc"
include "../../cfa.mc"
include "../pruning/compile.mc"
include "../delayed-sampling/compile.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLBPF =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA + PhaseStats
  + SMCCommon + InferenceInterface + DPPLPruning + DPPLDelayedSampling

  sem transformStopFirstAssume: InferenceSymEnv -> Expr -> Option Expr
  sem transformStopFirstAssume env =

  -- Terms that cannot execute an assume internally (in ANF)
  | TmLet ({body = TmVar _ | TmLam _ | TmConst _ | TmSeq _ | TmRecord _} & r) ->
      match transformStopFirstAssume env r.inexpr with Some inexpr then
        Some (TmLet { r with inexpr = inexpr })
      else None ()

  | TmRecLets r ->
    match transformStopFirstAssume env r.inexpr with Some inexpr then
      Some (TmRecLets { r with inexpr = inexpr })
    else None ()

  | TmExt r ->
    match transformStopFirstAssume env r.inexpr with Some inexpr then
      Some (TmExt {r with inexpr = inexpr})
    else None ()

  | TmType r ->
    match transformStopFirstAssume env r.inexpr with Some inexpr then
      Some (TmType {r with inexpr = inexpr})
    else None ()

  | TmConDef r ->
    match transformStopFirstAssume env r.inexpr with Some inexpr then
      Some (TmConDef {r with inexpr = inexpr})
    else None ()

  -- Allow tail call match with single branch (e.g., `match ... with ... in ...`)
  | TmMatch ({ thn = thn, els = TmLet { body = TmNever _ } & els } & r)->
    match transformStopFirstAssume env thn with Some thn then
      Some (TmMatch { r with thn = thn, els = withInfo (infoTm els) never_ })
    else None ()

  -- If we reach an assume, do the transformation
  | TmLet { ident = ident, body = TmAssume r, inexpr = inexpr, info = info } ->
    let i = withInfo info in
    Some (i (appFromEnv env "stopFirstAssume" [r.dist, i (nulam_ ident inexpr)]))

  | t -> None ()

  sem compile: BPFConfig -> InferenceInterface -> Expr
  sem compile config =
  | x ->
    let log = mkPhaseLogState x.options.debugDumpPhases x.options.debugPhases in
    let t = x.extractNoHigherOrderConsts (lam x. x) in
    endPhaseStatsExpr log "extract-no-higher-order-consts-one" t;

    -- printLn ""; printLn "--- INITIAL ANF PROGRAM ---";
    -- match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);

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
      let cEnd = _getConExn "End" x.runtime.env in
      let cont = (ulam_ "x" (nconapp_ cEnd (var_ "x"))) in
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

    -- printLn ""; printLn "--- BEFORE transformStopFirstAssume ---";
    -- match pprintCode 0 env t with (env,str) in
    -- printLn (str);

    let t = switch (mapLookup "prune" x.extraEnvs, mapLookup "delayed-sampling" x.extraEnvs)
      case (Some pruneEnv, _) then prune pruneEnv t
      case (_, Some delayEnv) then delayedSampling delayEnv t
      case _ then t
      end in
    -- Attempt to identify and stop at first assume to potentially reuse
    -- previous empirical distribution (see runtime)
    let t =
      match transformStopFirstAssume x.runtime t with Some t then t
      else
        let i = withInfo (infoTm t) in
        i (appFromEnv x.runtime "stopInit" [i (ulam_ "" t)])
    in
    endPhaseStatsExpr log "transform-stop-first-assume-one" t;

    -- printLn ""; printLn "--- AFTER ---";
    -- match pprintCode 0 env t with (env,str) in
    -- printLn (str);

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr (transformTmDist x.dists) t in
    endPhaseStatsExpr log "transform-tm-dist-one" t;

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr (transformProb x.stateName x.dists x.runtime) t in
    endPhaseStatsExpr log "transform-prob-one" t;

    t

end

lang BPFCompilerPicker = BPFMethod
  -- NOTE(vipa, 2025-04-02): The presence or absence of prune and
  -- dynamic delay are the only things that change what runtimes we
  -- fetch, thus we only check those in comparisons
  sem _cmpInferMethod = | (BPF a, BPF b) ->
    let res = cmpBool a.prune b.prune in
    if neqi res 0 then res else
    cmpBool a.dynamicDelay b.dynamicDelay

  sem pickRuntime = | BPF x ->
    let extras = mapEmpty cmpString in
    let extras = if x.prune
      then mapInsert "prune" "pruning/runtime.mc" extras
      else extras in
    let extras = if x.dynamicDelay
      then mapInsert "delayed-sampling" "delayed-sampling/runtime.mc" extras
      else extras in
    ("smc-bpf/runtime.mc", extras)

  sem pickCompiler = | BPF x -> use MExprPPLBPF in compile x
end
