include "../dists.mc"
include "../../inference-common/smc.mc"
include "../../cfa.mc"
include "mexpr/ast-builder.mc"
include "mexpr/cps.mc"

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

  sem compile : Expr -> Expr
  sem compile =
  | t ->

    -- ANF transformation (required for CPS)
    let t = normalizeTerm t in

    printLn ""; printLn "--- INITIAL PROGRAM ---";
    match pprintCode 0 pprintEnvEmpty t with (env,str) in
    printLn (str);

    -- Static analysis
    let checkpoint = lam t.
      match t with TmLet { ident = ident, body = body } then
        match body with TmWeight _ | TmObserve _ then true else false
      else errorSingle [infoTm t] "Impossible"
    in
    let checkpointNames: Set Name =
      extractCheckpoint (checkpointCfa checkpoint t) in
    -- printLn (join [ "[", strJoin "," (map nameGetStr (setToSeq checkpointNames)), "]"]);
    printLn ""; printLn "--- CHECKPOINT ANALYSIS RESULT ---";
    match mapAccumL pprintEnvGetStr env (setToSeq checkpointNames) with (env,strings) in
    printLn (join [ "[", strJoin "," strings, "]"]);

    -- TODO: Remove, temporary
    -- Get alignment analysis result
    let unalignedNames: Set Name = extractUnaligned cfaRes in
    printLn ""; printLn "--- UNALIGNED NAMES ---";
    match mapAccumL pprintEnvGetStr env (setToSeq unalignedNames) with (env,unalignedStrings) in
    printLn (join [ "[", strJoin "," unalignedStrings, "]"]);

    printLn ""; printLn "--- CPS INPUT ---";
    -- let unalignedNames = foldPre_Expr_Expr (lam acc. lam e.
    --     match e with TmLet { ident = ident, body = TmLam t } then
    --       if setMem t.ident unalignedNames then setInsert ident acc
    --       else acc
    --     else match e with TmRecLets { bindings = bindings } then
    --       foldl (lam acc. lam b: RecLetBinding.
    --         match b.body with TmLam t then
    --           if setMem t.ident unalignedNames then setInsert b.ident acc
    --           else acc
    --         else errorSingle [infoTm b.body] "Not a lambda in recursive let body"
    --       ) acc bindings
    --     else acc
    --   ) unalignedNames t in
    let checkpointNames: Set Name = setSubtract checkpointNames unalignedNames in
    match mapAccumL pprintEnvGetStr env (setToSeq checkpointNames) with (env,tmp) in
    printLn (join [ "[", strJoin "," tmp, "]"]);

    -- CPS transformation
    let t =
      cpsPartialCont checkpointNames (ulam_ "x" (conapp_ "End" (var_ "x"))) t in

    printLn ""; printLn "--- CPS ---";
    match pprintCode 0 pprintEnvEmpty t with (env,str) in
    printLn (str);

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

let compilerSMC = lam options. use MExprPPLSMC in
  ("smc/runtime.mc", compile options)
