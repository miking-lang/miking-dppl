include "../dists.mc"
include "../common.mc"
include "../../inference-common/smc.mc"
include "../../cfa.mc"

include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"

lang MExprPPLImportance =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA
  + MExprPPLCommon

  -------------------------
  -- IMPORTANCE SAMPLING --
  -------------------------

  -- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr
  -- AST types here. Optimally, the type would be Options -> CorePPLExpr ->
  -- MExprExpr or similar.
  sem compile : Options -> Set String -> Expr -> Expr
  sem compile options externals =
  | t ->

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformProb t in

    t

  sem transformProb =
  | TmAssume t ->
    let i = withInfo t.info in
    i (app_ (i (var_ "sample")) t.dist)

  | TmObserve t ->
    let i = withInfo t.info in
    let weight = i (appf2_ (i (var_ "logObserve")) t.dist t.value) in
    i (appf2_ (i (var_ "updateWeight")) weight (i (var_ "state")))
  | TmWeight t ->
    let i = withInfo t.info in
    i (appf2_ (i (var_ "updateWeight")) t.weight (i (var_ "state")))
  | TmResample t -> withInfo t.info unit_
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

  -- This is where we use the continuation (weight and observe)
  | TmLet { ident = ident, body = TmWeight { weight = weight },
            inexpr = inexpr} & t ->
    let i = withInfo (infoTm t) in
    let k =
      if tailCall t then
        match k with Some k then k
        else error "Something went wrong with partial CPS transformation"
      else i (nulam_ ident (exprCps env k inexpr))
    in
    i (appf2_ (i (var_ "updateWeight")) weight k)

  -- This is where we use the continuation (weight and observe)
  | TmLet { ident = ident, body = TmObserve { value = value, dist = dist },
            inexpr = inexpr } & t ->
    let i = withInfo (infoTm t) in
    let k =
      if tailCall t then
        match k with Some k then k
        else error "Something went wrong with partial CPS transformation"
      else i (nulam_ ident (exprCps env k inexpr))
    in
    let weight = i (appf2_ (i (var_ "logObserve")) dist value) in
    i (appf2_ (i (var_ "updateWeight")) weight k)

  sem transformProbCps =
  | TmAssume t ->
    let i = withInfo t.info in
    i (app_ (i (var_ "sample")) t.dist)
  | TmResample t -> withInfo t.info unit_

  -- Should already have been removed by CPS!
  | (TmObserve t | TmWeight t) ->
    errorSingle [t.info] "Impossible in importance sampling with CPS"
  | t -> t

  sem compileCps : Options -> Set String -> Expr -> Expr
  sem compileCps options externals =
  | t ->

    -- Read in native versions of higher-order constants and replace usage of
    -- the constants with the native version. Require because of CPS which
    -- cannot handle higher-order constants.
    let t = replaceHigherOrderConstants t in
    -- Also symbolize the new replacements to avoid CFA inaccuracy
    let t = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } t
    in


    -- ANF transformation (required for CPS)
    let t = normalizeTerm t in

    -- printLn ""; printLn "--- INITIAL ANF PROGRAM ---";
    -- match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);

    -- Static analysis and CPS transformation
    let t =

      let cont = (ulam_ "x" (conapp_ "End" (var_ "x"))) in

      match options.cps with "partial" then
        let checkpoint = lam t.
          match t with TmLet { ident = ident, body = body } then
            match body with TmWeight _ | TmObserve _ then true else false
          else errorSingle [infoTm t] "Impossible"
        in
        let checkPointNames: Set Name =
          extractCheckpoint (checkpointCfa checkpoint t) in
        -- printLn ""; printLn "--- CHECKPOINT ANALYSIS RESULT ---";
        -- match mapAccumL pprintEnvGetStr env (setToSeq checkPointNames) with (env,strings) in
        -- printLn (join [ "[", strJoin "," strings, "]"]);
        cpsPartialCont checkPointNames cont t

      else match options.cps with "full" then cpsFullCont cont t

      else error ( join [ "Invalid CPS option:", options.cps ])

    in

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformProbCps t in

    t

end

let compilerImportance = lam options. use MExprPPLImportance in
  match options.cps with "partial" | "full" then
    ("importance/runtime-cps.mc", compileCps options)
  else match options.cps with "none" then
    ("importance/runtime.mc", compile options)
  else
    error ( join [ "Unknown CPS option:", options.cps ])
