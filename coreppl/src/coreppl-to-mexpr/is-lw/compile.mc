include "../dists.mc"
include "../../inference/smc.mc"
include "../../cfa.mc"

include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"

lang MExprPPLImportance =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA

  -------------------------
  -- IMPORTANCE SAMPLING --
  -------------------------

  -- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr
  -- AST types here. Optimally, the type would be Options -> CorePPLExpr ->
  -- MExprExpr or similar.
  sem compile : Options -> (Expr,Expr) -> Expr
  sem compile options =
  | (t,_) ->

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

  -- NOTE(2023-08-08,dlunde): Many TmTypes are shared with non-PPL code and
  -- transformed versions are removed when removing duplicate code.
  -- Therefore, we have to simply replace TyCon and TyApp with Unknown here.
  sem tyCps env =
  | (TyCon { info = info } | TyApp { info = info } ) ->
    let i = tyWithInfo info in i tyunknown_

  sem transformProbCps =
  | TmAssume t ->
    let i = withInfo t.info in
    i (app_ (i (var_ "sample")) t.dist)
  | TmResample t -> withInfo t.info unit_

  -- Should already have been removed by CPS!
  | (TmObserve _ | TmWeight _) & tm ->
    errorSingle [infoTm tm] "Impossible in importance sampling with CPS"
  | t -> t

  sem compileCps : Options -> (Expr,Expr) -> Expr
  sem compileCps options =
  | (_,t) ->

    -- printLn ""; printLn "--- INITIAL ANF PROGRAM ---";
    -- match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);

    -- Static analysis and CPS transformation

    -- let t1 = wallTimeMs () in
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
        -- match checkpointCfaDebug checkpoint env t with (env,res) in
        -- let checkPointNames: Set Name = extractCheckpoint res in

        -- printLn ""; printLn "--- CHECKPOINT ANALYSIS RESULT ---";
        -- match mapAccumL pprintEnvGetStr env (setToSeq checkPointNames) with (env,strings) in
        -- printLn (join [ "[", strJoin "," strings, "]"]);

        cpsPartialCont (lam n. setMem n checkPointNames) cont t

      else match options.cps with "full" then cpsFullCont cont t

      else error ( join [ "Invalid CPS option:", options.cps ])

    in
    -- let t2 = wallTimeMs () in
    -- printLn (float2string (subf t2 t1));

    -- printLn ""; printLn "--- AFTER CPS ---";
    -- match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformProbCps t in

    t

end

let compilerImportance = lam options. use MExprPPLImportance in
  match options.cps with "partial" | "full" then
    ("is-lw/runtime-cps.mc", compileCps options)
  else match options.cps with "none" then
    ("is-lw/runtime.mc", compile options)
  else
    error ( join [ "Unknown CPS option:", options.cps ])
