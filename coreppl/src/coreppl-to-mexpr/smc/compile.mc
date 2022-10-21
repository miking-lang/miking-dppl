include "../dists.mc"
include "../../inference-common/smc.mc"
include "../../cfa.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"

-- TODO(dlunde,2022-06-29): This file does not currently implement SMC and is used as a sandbox for experimenting with CFA (suspension/checkpoint and alignment) and CPS

lang MExprPPLSMC =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA

  -- CPS
  sem exprCps env k =
  -- Do nothing at assumes or resamples
  | TmLet ({ body = TmAssume _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmResample _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmDist _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }

  -- This is where we use the continuation (weight and observe)
  | TmLet ({ ident = ident, body = TmWeight w,
            inexpr = inexpr} & r) & t ->
    if not (transform env ident) then
      -- TODO: Remove me, this is not needed for importance sampling
      TmLet { r with body = (var_ "TMP"), inexpr = exprCps env k inexpr }
    else
      let i = withInfo (infoTm t) in
      let k = if tailCall t then
          match k with Some k then k
          else match k with None () then error "Something went wrong with partial CPS transformation"
          else never
        else i (nulam_ ident (exprCps env k inexpr)) in
      i (appf2_ (i (var_ "updateWeight")) w.weight k)

  -- This is where we use the continuation (weight and observe)
  | TmLet ({ ident = ident, body = TmObserve o, inexpr = inexpr } & r) & t ->
    if not (transform env ident) then
      -- TODO: Remove me, this is not needed for importance sampling
      TmLet { r with body = (var_ "TMP"), inexpr = exprCps env k inexpr }
    else
      let i = withInfo (infoTm t) in
      let k = if tailCall t then
          match k with Some k then k
          else match k with None () then error "Something went wrong with partial CPS transformation"
          else never
        else i (nulam_ ident (exprCps env k inexpr)) in
      let weight = i (app_ (i (recordproj_ "logObserve" o.dist)) o.value) in
      i (appf2_ (i (var_ "updateWeight")) weight k)

  sem compile : Options -> Expr -> Expr
  sem compile options =
  | t ->

    -- ANF transformation (required for analysis and CPS)
    let t = normalizeTerm t in

    -- printLn ""; printLn "--- INITIAL ANF PROGRAM ---";
    match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);

    -- Alignment analysis
    let alignRes = alignCfa t in
    let unalignedNames: Set Name = extractUnaligned alignRes in
    -- let unalignedNames: Set Name = setEmpty nameCmp in
    -- printLn ""; printLn "--- UNALIGNED NAMES ---";
    match mapAccumL pprintEnvGetStr env (setToSeq unalignedNames) with (env,unalignedStrings) in
    -- printLn (join [ "[", strJoin "," unalignedStrings, "]"]);

    -- Checkpoint analysis, reuse previous alignment results to speed up the
    -- analysis
    let checkpoint = lam t.
      match t with TmLet { ident = ident, body = body } then
        match body with TmWeight _ | TmObserve _ then
          not (setMem ident unalignedNames)
        else false
      else errorSingle [infoTm t] "Impossible"
    in
    let graph = addCheckpointConstraints checkpoint alignRes t in
    -- match (solveCfaDebug env graph) with (env,res) in
    -- let checkpointNames: Set Name = extractCheckpoint res in
    let checkpointNames: Set Name = extractCheckpoint (solveCfa graph) in
    -- printLn ""; printLn "--- CHECKPOINT ANALYSIS RESULT ---";
    match mapAccumL pprintEnvGetStr env (setToSeq checkpointNames) with (env,strings) in
    -- printLn (join [ "[", strJoin "," strings, "]"]);

    -- CPS transformation
    let t =
      cpsPartialCont checkpointNames (ulam_ "x" (conapp_ "End" (var_ "x"))) t in

    -- printLn ""; printLn "--- CPS ---";
    match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in
    let t = replaceTyDist t in

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformProb t in

    t

  sem transformProb =
  | TmAssume t ->
    let i = withInfo t.info in
    i (app_ (i (recordproj_ "sample" t.dist)) (i unit_))
  | TmResample t -> withInfo t.info unit_

  -- Should already have been removed by CPS!
  | (TmObserve t | TmWeight t) -> errorSingle [t.info] "Impossible in importance-cps"
  | t -> t

end

let compilerSMC = lam options. use MExprPPLSMC in
  ("smc/runtime.mc", compile options)
