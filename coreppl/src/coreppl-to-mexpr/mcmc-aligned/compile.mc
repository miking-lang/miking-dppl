include "../dists.mc"
include "../../inference-common/smc.mc"
include "../../cfa.mc"
include "../../dppl-arg.mc"
include "mexpr/ast-builder.mc"

lang MExprPPLAlignedMCMC =
  MExprPPL + Resample + TransformDist + MExprANFAll + MExprPPLCFA

  -------------------------
  -- STATIC ALIGNED MCMC --
  -------------------------
  -- NOTE: Assumes must be transformed based on alignment (some samples are
  -- just drawn directly, and some are reused)

  sem compile : Options -> Expr -> Expr
  sem compile options =
  | t ->

    -- Check that options are within the proper range
    let gProb = options.mcmcAlignedGlobalProb in
    let mProb = options.mcmcAlignedGlobalModProb in
    if or (ltf gProb 0.0) (gtf gProb 1.0) then
      error "--mcmc-aligned-global-prob must be between 0.0 and 1.0"
    else ();
    if or (ltf mProb 0.0) (gtf mProb 1.0) then
      error "--mcmc-aligned-global-mod-prob must be between 0.0 and 1.0"
    else ();

    -- ANF transformation (required for analysis)
    let t = normalizeTerm t in

    -- Alignment analysis
    let alignRes = alignCfa t in
    let unalignedNames: Set Name = extractUnaligned alignRes in

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr (transformProb unalignedNames) t in

    t

  -- Compile CorePPL constructs to MExpr
  sem transformProb unalignedNames =

  | TmLet ({ ident = ident, body = TmAssume t, inexpr = inexpr } & r) ->
    let i = withInfo r.info in
    TmLet { r with
      body = i (app_ (i (var_
          (if setMem ident unalignedNames then "sampleUnaligned"
           else "sampleAligned")
        )) t.dist)
    }

  | TmObserve t ->
    let i = withInfo t.info in
    let weight = i (app_ (i (recordproj_ "logObserve" t.dist)) t.value) in
    i (appf1_ (i (var_ "updateWeight")) weight)

  | TmWeight t ->
    let i = withInfo t.info in
    i (appf1_ (i (var_ "updateWeight")) t.weight)

  | TmResample t -> withInfo t.info unit_

  | t -> t

  -------------------------------
  -- STATIC ALIGNED MCMC (CPS) --
  -------------------------------
  -- NOTE(dlunde,2022-08-22): CPS must differentiate between aligned and
  -- unaligned assumes


  ------------------------------------------
  -- DYNAMIC ("LIGHTWEIGHT") ALIGNED MCMC --
  ------------------------------------------


  ------------------------------------------------
  -- DYNAMIC ("LIGHTWEIGHT") ALIGNED MCMC (CPS) --
  ------------------------------------------------

end

let compilerAlignedMCMC = lam options. use MExprPPLAlignedMCMC in
  ("mcmc-aligned/runtime.mc", compile options)
