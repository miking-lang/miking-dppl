include "../dists.mc"
include "../inference-interface.mc"
include "../../inference/smc.mc"
include "../../cfa.mc"
include "../delayed-sampling/compile.mc"
include "../pruning/compile.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"
include "mexpr/phase-stats.mc"

lang MExprPPLImportance =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA + PhaseStats + InferenceInterface + DPPLDelayedSampling + DPPLPruning

  -------------------------
  -- IMPORTANCE SAMPLING --
  -------------------------

  sem compile : ImportanceConfig -> InferenceInterface -> Expr
  sem compile config =
  | x ->
    let log = mkPhaseLogState x.options.debugDumpPhases x.options.debugPhases in
    let t = x.extractNormal (lam x. x) in
    endPhaseStatsExpr log "extract-normal-one" t;

    let t = switch (mapLookup "prune" x.extraEnvs, mapLookup "delayed-sampling" x.extraEnvs)
      case (Some pruneEnv, _) then prune pruneEnv t
      case (_, Some delayEnv) then delayedSampling delayEnv t
      case _ then t
      end in
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
  | TmCancel t ->
    let i = withInfo t.info in
    let weight = i (appFromEnv env "logObserve" [t.dist, t.value]) in
    i (appFromEnv runtime "updateWeight" [negf_ weight, i (nvar_ stateName)])
  | t -> t


  -------------------------------
  -- IMPORTANCE SAMPLING (CPS) --
  -------------------------------

  -- CPS compile
  sem exprCps env k =
  -- Do nothing at assumes or resamples
  | TmLet ({ body = TmAssume _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmResample _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmDist _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmDist (d & { dist = DWiener w })} & t) ->
    if not (transform env t.ident) then
      TmLet { t with inexpr = exprCps env k t.inexpr }
    else
      TmLet {
        t with
        body = TmDist { d with dist = DWiener { w with cps = true }},
        inexpr = exprCps env k t.inexpr
      }

  -- This is where we use the continuation (weight and observe)
  | TmLet { ident = ident, body = w & TmWeight { weight = weight },
            inexpr = inexpr} & t ->
    let i = withInfo (infoTm t) in
    let k =
      if tailCall t then
        match k with Some k then k
        else error "Something went wrong with partial CPS transformation"
      else i (nulam_ ident (exprCps env k inexpr))
    in
    -- NOTE(vipa, 2025-01-16): This will be fixed in
    -- `transformProbCps` later, because we don't have access to the
    -- environments here
    i (appf2_ w weight k)

  -- This is where we use the continuation (weight and observe)
  | TmLet { ident = ident, body = obs & TmObserve { value = value, dist = dist },
            inexpr = inexpr } & t ->
    let i = withInfo (infoTm t) in
    let k =
      if tailCall t then
        match k with Some k then k
        else error "Something went wrong with partial CPS transformation"
      else i (nulam_ ident (exprCps env k inexpr))
    in
    -- NOTE(vipa, 2025-01-16): This will be fixed in `transformProb`
    -- later, because we don't have access to the environments here
    let weight = i (appf2_ obs dist value) in
    i (appf2_ (i (TmWeight {weight = unit_, ty = tyunknown_, info = infoTm t})) weight k)

  -- NOTE(2023-08-08,dlunde): Many TmTypes are shared with non-PPL code and
  -- transformed versions are removed when removing duplicate code.
  -- Therefore, we have to simply replace TyCon and TyApp with Unknown here.
  sem tyCps env =
  | (TyCon { info = info } | TyApp { info = info } ) ->
    let i = tyWithInfo info in i tyunknown_

  sem transformProbCps env runtime =
  | TmAssume t ->
    let i = withInfo t.info in
    i (appFromEnv env "sample" [t.dist])
  | TmResample t -> withInfo t.info unit_

  | TmApp
    { lhs = TmApp
      { lhs = TmObserve obs
      , rhs = dist
      },
      rhs = value
    } -> appFromEnv env "logObserve" [dist, value]
  | TmApp
    { lhs = TmApp
      { lhs = TmWeight w
      , rhs = weight
      },
      rhs = k
    } -> appFromEnv runtime "updateWeight" [weight, k]
  -- Should already have been removed by CPS!
  | (TmObserve _ | TmWeight _) & tm ->
    errorSingle [infoTm tm] "Impossible in importance sampling with CPS"
  | t -> t

  sem compileCps : ImportanceConfig -> InferenceInterface -> Expr
  sem compileCps config =
  | x ->
    let log = mkPhaseLogState x.options.debugDumpPhases x.options.debugPhases in
    let t = x.extractNoHigherOrderConsts (lam x. x) in
    endPhaseStatsExpr log "extract-no-higher-order-consts-one" t;

    -- printLn ""; printLn "--- INITIAL ANF PROGRAM ---";
    -- match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);

    -- Static analysis and CPS transformation

    -- let t1 = wallTimeMs () in
    let t =

      let cont =
        let n = _getConExn "End" x.runtime.env in
        let x = nameSym "x" in
        (nulam_ x (nconapp_ n (nvar_ x))) in

      match config.cps with "partial" then
        let checkpoint = lam t.
          match t with TmLet { ident = ident, body = body } then
            match body with TmWeight _ | TmObserve _ then true else false
          else errorSingle [infoTm t] "Impossible"
        in
        let checkPointNames: Set Name =
          extractCheckpoint (checkpointCfa checkpoint t) in
        -- match checkpointCfaDebug checkpoint env t with (env,res) in
        -- let checkPointNames: Set Name = extractCheckpoint res in

        -- printLn ""; printLn "--- CHECKPOINT ANALYSIS RESULT ---";
        -- match mapAccumL pprintEnvGetStr env (setToSeq checkPointNames) with (env,strings) in
        -- printLn (join [ "[", strJoin "," strings, "]"]);

        cpsPartialCont (lam n. setMem n checkPointNames) cont t

      else match config.cps with "full" then cpsFullCont cont t

      else error ( join [ "Invalid CPS option:", config.cps ])

    in
    endPhaseStatsExpr log "cps-one" t;
    -- let t2 = wallTimeMs () in
    -- printLn (float2string (subf t2 t1));

    -- printLn ""; printLn "--- AFTER CPS ---";
    -- match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);
    let t = if or config.prune config.dynamicDelay then error "Importance sampling with CPS does not support pruning or dynamic delayed sampling" else t in
    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr (transformTmDist x.dists) t in
    endPhaseStatsExpr log "transform-tm-dist-one" t;

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr (transformProbCps x.dists x.runtime) t in
    endPhaseStatsExpr log "transform-prob-cps-one" t;

    t
end

lang ImportanceCompilerPicker = ImportanceSamplingMethod
  -- NOTE(vipa, 2025-04-04): Runtime selection only looks at
  -- dynamicDelay and cps, thus we only compare those here
  sem _cmpInferMethod = | (Importance a, Importance b) ->
    let res = cmpBool a.prune b.prune in
    if neqi res 0 then res else
    let res = cmpBool a.dynamicDelay b.dynamicDelay in
    if neqi res 0 then res else
    cmpBool (eqString a.cps "none") (eqString b.cps "none")

  sem pickRuntime = | Importance x ->
    let extras = mapEmpty cmpString in
    let extras = if x.prune
      then mapInsert "prune" "pruning/runtime.mc" extras
      else extras in
    let extras = if x.dynamicDelay
      then mapInsert "delayed-sampling" "delayed-sampling/runtime.mc" extras
      else extras in
    if eqString x.cps "none"
    then ("is-lw/runtime.mc", extras)
    else ("is-lw/runtime-cps.mc", extras)

  sem pickCompiler = | Importance x ->
    use MExprPPLImportance in
    if eqString x.cps "none"
    then compile x
    else compileCps x
end
