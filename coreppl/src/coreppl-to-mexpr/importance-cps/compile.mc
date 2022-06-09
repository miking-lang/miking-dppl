include "../dists.mc"
include "../../inference-common/smc.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"

lang MExprPPLImportanceCPS = MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll

  -- CPS
  sem exprCps k =
  -- Do nothing at assumes or resamples
  | TmLet ({ body = TmAssume _ } & t) ->
    TmLet { t with inexpr = exprCps k t.inexpr }
  | TmLet ({ body = TmResample _ } & t) ->
    TmLet { t with inexpr = exprCps k t.inexpr }
  | TmLet ({ body = TmDist _ } & t) ->
    TmLet { t with inexpr = exprCps k t.inexpr }

  -- This is where we use the continuation (weight and observe)
  | TmLet { ident = ident, body = TmWeight { weight = weight },
            inexpr = inexpr} & t ->
    let i = withInfo (infoTm t) in
    let k = if tailCall t then k else i (nulam_ ident (exprCps k inexpr)) in
    i (appf2_ (i (var_ "updateWeight")) weight k)

  | TmLet { ident = ident, body = TmObserve { value = value, dist = dist },
            inexpr = inexpr } & t ->
    let i = withInfo (infoTm t) in
    let k = if tailCall t then k else nulam_ ident (exprCps k inexpr) in
    let weight = i (app_ (i (recordproj_ "logObserve" dist)) value) in
    i (appf2_ (i (var_ "updateWeight")) weight k)

  sem compile : Expr -> Expr
  sem compile =
  | t ->

    -- ANF transformation (required for CPS)
    let t = normalizeTerm t in

    -- CPS transformation
    let t = cpsCont (ulam_ "x" (conapp_ "End" (var_ "x"))) t in

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
