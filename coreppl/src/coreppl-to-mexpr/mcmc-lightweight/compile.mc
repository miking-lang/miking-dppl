include "name.mc"
include "mexpr/const-arity.mc"

include "../dists.mc"
include "../../inference-common/smc.mc"
include "../../cfa.mc"
include "../../dppl-arg.mc"
include "mexpr/ast-builder.mc"

let addrName = nameSym "addr"
let sym: Ref Int = ref 0

lang MExprPPLLightweightMCMC =
  MExprPPL + Resample + TransformDist + MExprANFAll + MExprPPLCFA + MExprArity

  -------------------------
  -- STATIC ALIGNED MCMC --
  -------------------------
  -- NOTE: Assumes must be transformed based on alignment (some samples are
  -- just drawn directly, and some are reused)

  sem compileAligned : Options -> Expr -> Expr
  sem compileAligned options =
  | t ->

    -- ANF transformation (required for analysis)
    let t = normalizeTerm t in

    -- Alignment analysis
    let alignRes = alignCfa t in
    let unalignedNames: Set Name = extractUnaligned alignRes in

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr (transformProbAligned unalignedNames) t in

    t

  -- Compile CorePPL constructs to MExpr
  sem transformProbAligned unalignedNames =

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

  ------------------------------------------
  -- DYNAMIC ("LIGHTWEIGHT") ALIGNED MCMC --
  ------------------------------------------
  sem compile : Options -> Expr -> Expr
  sem compile options =
  | t ->

    -- Addressing transform combined with CorePPL->MExpr transform
    let t = transform (setEmpty nameCmp) t in

    -- Type addressing transform
    -- TODO: We must also translate all the function types (but, ignore condefs as in CPS)

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Initialize addr to the empty list (not rope) at the
    -- beginning of the program.
    let t = bind_ (nulet_ addrName (var_ "emptyList")) t in

    t

  sem addr: Info -> Expr
  sem addr =
  | i ->
    let i = withInfo i in
    let s = deref sym in
    modref sym (addi s 1);
    cons_ (i (int_ s)) (i (nvar_ addrName))

  sem transform: Set Name -> Expr -> Expr
  sem transform externalIds =

  | t -> smap_Expr_Expr (transform externalIds) t

  | TmLam _ & t ->
    match smap_Expr_Expr (transform externalIds) t with TmLam r & t in
    let i = withInfo r.info in
    i (nlam_ addrName tyint_ t)

  | TmConst r & t ->
    -- Wrap constant functions in lambdas where they are introduced
    if gti (constArity r.val) 0 then
      let i = withInfo r.info in
      i (nlam_ addrName tyint_ t)
    else t

  | TmExt r & t ->
    TmExt { r with inexpr = transform (setInsert r.ident externalIds) r.inexpr }

  | TmApp _ & t ->

    -- NOTE(dlunde,2022-09-07): the somewhat obscure code below code replaces
    -- an application a b c d with
    --
    --   (tr a) s1 (tr b) s2 (tr c) s3,
    --
    -- where s1-s3 are unique symbols and tr represents recursively applying
    -- the transformation. However, if a is a variable that refers to an
    -- external (which are guaranteed to always be fully applied), it instead
    -- simply results in
    --
    --   (tr a) (tr b) (tr c)
    --
    -- as externals cannot be curried or sent as first-class values (nor can
    -- they evaluate any `assume`s internally).

    let transformApp = lam app.
      match app with TmApp r then
        let i = withInfo r.info in
        TmApp {r with lhs = i (app_ r.lhs (addr r.info))}
      else error "Impossible"
    in

    recursive let rec = lam app.
      match app with TmApp r then

        -- Always transform the argument (the rhs)
        let r = { r with rhs = transform externalIds r.rhs } in

        -- Recurse over lhs if it's an app
        match r.lhs with TmApp _ then
          match rec r.lhs with (lhs, extApp) in
          let app = TmApp { r with lhs = lhs } in
          (if extApp then app else transformApp app, extApp)

        -- Base case
        else match r.lhs with TmVar { ident = ident } then
          if setMem ident externalIds
            then (TmApp r,true) else (transformApp (TmApp r), false)

        -- Base case (also remember to transform the lhs)
        else
          let app = TmApp { r with lhs = transform externalIds r.lhs } in
          (transformApp app, false)

      else error "Impossible"
    in

    match rec t with (t,_) in t

  | TmAssume r ->
    let i = withInfo r.info in
    i (appf2_ (i (var_ "sample")) (addr r.info) r.dist)

  | TmObserve r ->
    let i = withInfo r.info in
    let weight = i (app_ (i (recordproj_ "logObserve" r.dist)) r.value) in
    i (appf1_ (i (var_ "updateWeight")) weight)

  | TmWeight r ->
    let i = withInfo r.info in
    i (appf1_ (i (var_ "updateWeight")) r.weight)

  | TmResample r -> withInfo r.info unit_

  -------------------------------
  -- STATIC ALIGNED MCMC (CPS) --
  -------------------------------
  -- NOTE(dlunde,2022-08-22): CPS must differentiate between aligned and
  -- unaligned assumes

  ------------------------------------------------
  -- DYNAMIC ("LIGHTWEIGHT") ALIGNED MCMC (CPS) --
  ------------------------------------------------

end

let compilerLightweightMCMC = lam options. use MExprPPLLightweightMCMC in

  -- Check that options are within the proper range
  let gProb = options.mcmcLightweightGlobalProb in
  let mProb = options.mcmcLightweightGlobalModProb in
  if or (ltf gProb 0.0) (gtf gProb 1.0) then
  error "--mcmc-lightweight-global-prob must be between 0.0 and 1.0"
  else ();
  if or (ltf mProb 0.0) (gtf mProb 1.0) then
  error "--mcmc-lightweight-global-mod-prob must be between 0.0 and 1.0"
  else ();

  if options.align then
    ("mcmc-lightweight/runtime-aligned.mc", compileAligned options)
  else
    ("mcmc-lightweight/runtime.mc", compile options)
