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

    -- ANF transformation (required for analysis)
    let t = normalizeTerm t in

    -- Alignment analysis
    let alignRes = alignCfa t in
    let unalignedNames: Set Name = extractUnaligned alignRes in

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Transform samples, observes, and weights to MExpr
    -- - Replace aligned calls to "assume" with call to runtime function
    -- - Replace unaligned calls to "assume" with a direct sample
    -- - At all calls to "assume", increment trace length counter
    let t = mapPre_Expr_Expr (transformProb unalignedNames) t in

    t

  -- Compile CorePPL constructs to MExpr
  sem transformProb unalignedNames =

  | TmLet ({ ident = ident, body = TmAssume t, inexpr = inexpr } & r) ->
    let i = withInfo r.info in
    if setMem ident unalignedNames then
      -- For unaligned assumes, just draw samples directly
      TmLet { r with
        body = i (app_ (i (recordproj_ "sample" t.dist)) (i unit_))
      }
    else
      -- Delegate aligned assumes to "sampleAligned" function in runtime
      TmLet { r with body = i (app_ (i (var_ "sampleAligned")) t.dist) }

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
