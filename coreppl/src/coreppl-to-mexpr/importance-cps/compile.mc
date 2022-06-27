include "../dists.mc"
include "../../inference-common/smc.mc"
include "../../cfa.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"

lang MExprPPLImportanceCPS =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA

  -- Weight and observe are checkpoints
  sem checkpoint =
  | TmWeight _ -> true
  | TmObserve _ -> true

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
  | TmLet { ident = ident, body = TmWeight { weight = weight },
            inexpr = inexpr} & t ->
    let i = withInfo (infoTm t) in
    let k = if tailCall t then
        match k with Some k then k
        else error "Something went wrong with partial CPS transformation"
      else i (nulam_ ident (exprCps env k inexpr)) in
    i (appf2_ (i (var_ "updateWeight")) weight k)

  -- This is where we use the continuation (weight and observe)
  | TmLet { ident = ident, body = TmObserve { value = value, dist = dist },
            inexpr = inexpr } & t ->
    let i = withInfo (infoTm t) in
    let k = if tailCall t then
        match k with Some k then k
        else error "Something went wrong with partial CPS transformation"
      else i (nulam_ ident (exprCps env k inexpr)) in
    let weight = i (app_ (i (recordproj_ "logObserve" dist)) value) in
    i (appf2_ (i (var_ "updateWeight")) weight k)

  sem compile : Expr -> Expr
  sem compile =
  | t ->

    -- ANF transformation (required for CPS)
    let t = normalizeTerm t in

    -- printLn (mexprToString t);

    -- Static analysis
    let cfaRes = cfa t in

    -- Get checkpoint analysis result
    let checkPointNames: Set Name = extractCheckpoint cfaRes in
    -- printLn (join [ "[", strJoin "," (map nameGetStr (setToSeq checkPointNames)), "]"]);

    -- CPS transformation
    let t =
      cpsPartialCont checkPointNames (ulam_ "x" (conapp_ "End" (var_ "x"))) t in

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in
    let t = removeTyDist t in

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

let compilerImportanceCPS = use MExprPPLImportanceCPS in
  ("importance-cps/runtime.mc", compile)
