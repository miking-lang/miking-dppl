include "../dists.mc"
include "../../inference-common/smc.mc"
include "../../cfa.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"

lang MExprPPLBPF =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA

  -- CPS compile
  sem exprCps env k =
  | TmLet ({ body = TmAssume _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmObserve _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmWeight _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmDist _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet { ident = ident, body = TmResample {},
            inexpr = inexpr } & t ->
    let i = withInfo (infoTm t) in
    let k =
      if tailCall t then
        match k with Some k then
          k
        else
          error "Something went wrong with partial CPS transformation"
      else
        i (nulam_ ident (exprCps env k inexpr))
    in
      i (appf1_ (i (var_ "resample")) k)

  sem transformProb =
  | TmAssume t ->
    let i = withInfo t.info in
    i (app_ (i (var_ "sample")) t.dist)
  | TmResample t -> errorSingle [t.info] "Impossible"
  | TmObserve t ->
    let i = withInfo t.info in
    let weight = i (appf2_ (i (var_ "logObserve")) t.dist t.value) in
    i (appf2_ (i (var_ "updateWeight")) weight (i (var_ "state")))
  | TmWeight t ->
    let i = withInfo t.info in
    i (appf2_ (i (var_ "updateWeight")) t.weight (i (var_ "state")))
  | t -> t

  sem compile: Options -> (Expr,Expr) -> Expr
  sem compile options =
  | (_,t) ->

    -- printLn ""; printLn "--- INITIAL ANF PROGRAM ---";
    -- match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);

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
    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in
    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformProb t in
    t
end

let compilerBPF = lam options. use MExprPPLBPF in
  ("smc-bpf/runtime.mc", compile options)
