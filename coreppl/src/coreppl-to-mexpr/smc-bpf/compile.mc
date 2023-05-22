include "../dists.mc"
include "../../inference/smc.mc"
include "../../cfa.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"

lang MExprPPLBPF =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA
  + SMCCommon

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

  sem transformStopFirstAssume: Expr -> Option Expr
  sem transformStopFirstAssume =

  -- Terms that cannot execute an assume internally (in ANF)
  | TmLet ({body = TmVar _ | TmLam _ | TmConst _ | TmSeq _ | TmRecord _} & r) ->
      match transformStopFirstAssume r.inexpr with Some inexpr then
        Some (TmLet { r with inexpr = inexpr })
      else None ()

  | TmRecLets r ->
    match transformStopFirstAssume r.inexpr with Some inexpr then
      Some (TmRecLets { r with inexpr = inexpr })
    else None ()

  | TmExt r ->
    match transformStopFirstAssume r.inexpr with Some inexpr then
      Some (TmExt {r with inexpr = inexpr})
    else None ()

  | TmType r ->
    match transformStopFirstAssume r.inexpr with Some inexpr then
      Some (TmType {r with inexpr = inexpr})
    else None ()

  | TmConDef r ->
    match transformStopFirstAssume r.inexpr with Some inexpr then
      Some (TmConDef {r with inexpr = inexpr})
    else None ()

  -- Allow tail call match with single branch (e.g., `match ... with ... in ...`)
  | TmMatch ({ thn = thn, els = TmLet { body = TmNever _ } & els } & r)->
    match transformStopFirstAssume thn with Some thn then
      Some (TmMatch { r with thn = thn, els = withInfo (infoTm els) never_ })
    else None ()

  -- If we reach an assume, do the transformation
  | TmLet { ident = ident, body = TmAssume r, inexpr = inexpr, info = info } ->
    let i = withInfo info in
    Some (i (appf2_ (i (var_ "stopFirstAssume")) r.dist (i (nulam_ ident inexpr))))

  | t -> None ()

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

    -- Automatic resampling annotations
    let t =
      match options.resample with "likelihood" then addResample (lam. true) t
      else match options.resample with "manual" then t
      else match options.resample with "align"  then

        -- Do static analysis for stochastic value flow and alignment
        let unaligned: Set Name = extractUnaligned (alignCfa t) in
        let isAligned: Name -> Bool = lam n. not (setMem n unaligned) in

        addResample isAligned t

      else error "Invalid resample option"
    in

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

    -- printLn ""; printLn "--- BEFORE transformStopFirstAssume ---";
    -- match pprintCode 0 env t with (env,str) in
    -- printLn (str);

    -- Attempt to identify and stop at first assume to potentially reuse
    -- previous empirical distribution (see runtime)
    let t =
      match transformStopFirstAssume t with Some t then t
      else
        let i = withInfo (infoTm t) in
        i (app_ (i (var_ "stopInit")) (i (ulam_ "" t)))
    in

    -- printLn ""; printLn "--- AFTER ---";
    -- match pprintCode 0 env t with (env,str) in
    -- printLn (str);

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformProb t in

    t

end

let compilerBPF = lam options. use MExprPPLBPF in
  ("smc-bpf/runtime.mc", compile options)
