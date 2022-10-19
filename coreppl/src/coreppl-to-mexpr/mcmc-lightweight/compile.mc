include "name.mc"
include "mexpr/const-arity.mc"

include "../dists.mc"
include "../common.mc"
include "../../inference-common/smc.mc"
include "../../cfa.mc"
include "../../dppl-arg.mc"
include "mexpr/ast-builder.mc"
include "mexpr/type.mc"
include "mexpr/const-types.mc"

let addrName = nameSym "addr"
let sym: Ref Int = ref 0

lang MExprPPLLightweightMCMC =
  MExprPPL + Resample + TransformDist + MExprANFAll + MExprPPLCFA + MExprArity
  + MExprPPLCommon

  -------------------------
  -- STATIC ALIGNED MCMC --
  -------------------------
  -- NOTE: Assumes must be transformed based on alignment (some samples are
  -- just drawn directly, and some are reused)

  sem compileAligned : Options -> Expr -> Expr
  sem compileAligned options =
  | t ->

    -- Read in native versions of higher-order constants and replace usage of
    -- the constants with the native version. Simplifies CFA analysis (no need
    -- to handle higher-order constants).
    let t = replaceHigherOrderConstants t in
    -- Also symbolize the new replacements to avoid CFA inaccuracy
    let t = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } t
    in

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

    let weight = i (appf2_ (i (var_ "RuntimeDist_logObserve")) t.dist t.value) in
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

    -- Read in native versions of higher-order constants and replace usage of
    -- the constants with the native version. Simplifies addressing transform
    -- (no need to handle higher-order constants).
    let t = replaceHigherOrderConstants t in
    -- Also symbolize the new replacements to avoid CFA inaccuracy
    let t = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } t
    in

    -- Addressing transform combined with CorePPL->MExpr transform
    let t = transform (setEmpty nameCmp) t in

    -- Type addressing transform
    let t = mapPre_Expr_Expr exprTyTransform t in

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

  sem transformConst: Int -> Expr -> Expr
  sem transformConst arity =
  | e ->
    let i = withInfo (infoTm e) in
    recursive let vars = lam acc. lam arity.
      if lti arity 1 then acc
      else
        let arg = nameNoSym (concat "a" (int2string arity)) in
        vars (cons arg acc) (subi arity 1)
    in
    let varNames: [Name] = vars [] arity in
    let inner = foldl (lam acc. lam v. i (app_ acc (nvar_ v))) e varNames in
    foldr (lam v. lam acc.
        i (nlam_ addrName (tyseq_ tyint_) (i (nulam_ v acc)))
      ) inner varNames

  sem transform: Set Name -> Expr -> Expr
  sem transform externalIds =

  | t -> smap_Expr_Expr (transform externalIds) t

  | TmLam _ & t ->
    match smap_Expr_Expr (transform externalIds) t with TmLam r & t in
    let i = withInfo r.info in
    i (nlam_ addrName (tyseq_ tyint_) t)

  | TmConst r & t ->
    if isHigherOrderFunType (tyConst r.val) then
      -- TODO(dlunde,2022-09-19): Add support for higher-order constant functions
      errorSingle [r.info]
        "Higher-order constant functions not yet supported in addressing transform"
    else
      transformConst (constArity r.val) t

  | TmExt r & t ->
    TmExt { r with inexpr = transform (setInsert r.ident externalIds) r.inexpr }

  | TmApp _ & t ->

    -- NOTE(dlunde,2022-09-07): the somewhat obscure code below code replaces
    -- an application a b c d with
    --
    --   (tr a) s1 (tr b) s2 (tr c) s3 (tr d),
    --
    -- where s1-s3 are unique symbols and tr represents recursively applying
    -- the transformation. However, if a is a variable that refers to an
    -- external (which are guaranteed to always be fully applied), it instead
    -- simply results in
    --
    --   (tr a) (tr b) (tr c) (tr d)
    --
    -- as externals cannot be curried or sent as first-class values (nor can
    -- they evaluate any `assume`s internally).
    --
    -- Lastly, we also optimize (full _and_ partial) constant applications. For
    -- example, assume a is a constant of arity 4. The result is then
    --
    --   lam addr. lam e. a (tr b) (tr c) (tr d) e

    let transformApp = lam app.
      match app with TmApp r then
        let i = withInfo r.info in
        TmApp {r with lhs = i (app_ r.lhs (addr r.info))}
      else error "Impossible"
    in

    recursive let rec = lam app. lam numArgs.
      match app with TmApp r then

        -- Always transform the argument (the rhs)
        let r = { r with rhs = transform externalIds r.rhs } in

        -- Recurse over lhs if it's an app
        match r.lhs with TmApp _ then
          match rec r.lhs (addi 1 numArgs) with (lhs, constExtArgs) in
          let app = TmApp { r with lhs = lhs } in
          if gti constExtArgs 0 then
            (app, subi constExtArgs 1)
          else
            (transformApp app, 0)

        -- Base case: variables (including external applications)
        else match r.lhs with TmVar { ident = ident } then
          if setMem ident externalIds
            then (TmApp r, numArgs) else (transformApp (TmApp r), 0)

        -- Base case: constant application
        else match r.lhs with TmConst rc then
          if isHigherOrderFunType (tyConst rc.val) then
            -- TODO(dlunde,2022-09-19): Add support for higher-order constant functions
            errorSingle [rc.info]
              "Higher-order constant functions not yet supported in addressing transform"
          else
            (TmApp r, subi (constArity rc.val) 1)

        -- Base case: other (e.g., lambdas)
        -- OPT(dlunde,2022-09-08): Lambdas could also be optimized if applied
        -- directly when constructed (as they, at least partially, can't escape
        -- then).
        else
          let app = TmApp { r with lhs = transform externalIds r.lhs } in
          (transformApp app, 0)

      else error "Impossible"
    in

    match rec t 0 with (t,remainingConstArity) in
    transformConst remainingConstArity t

  | TmAssume r ->
    let i = withInfo r.info in
    let dist = transform externalIds r.dist in
    i (appf2_ (i (var_ "sample")) (addr r.info) dist)

  | TmObserve r ->
    let i = withInfo r.info in
    let dist = transform externalIds r.dist in
    let value = transform externalIds r.value in
    let weight = i (appf2_ (i (var_ "RuntimeDist_logObserve")) dist value) in
    i (appf1_ (i (var_ "updateWeight")) weight)

  | TmWeight r ->
    let i = withInfo r.info in
    let weight = transform externalIds r.weight in
    i (appf1_ (i (var_ "updateWeight")) weight)

  | TmResample r -> withInfo r.info unit_

  sem exprTyTransform =
  | t -> smap_Expr_Type tyTransform t
  | TmConDef r & t ->
    -- We do not transform the top-level arrow type of the condef (due to the
    -- nested smap_Type_Type), as data values are constructed as usual.
    -- NOTE(dlunde,2022-07-13): We currently leave TyAlls wrapping the
    -- top-level arrow type intact.
    -- NOTE(dlunde,2022-07-13): Issues can arise here if the top-level arrow
    -- type of a condef is a type variable that was defined earlier with
    -- TmType. It is then incorrectly transformed.
    recursive let rec = lam ty.
      match ty with TyAll b then TyAll { b with ty = rec b.ty }
      else match ty with TyArrow _ & t then smap_Type_Type tyTransform t
      else errorSingle [r.info]
        "Error in mcmc-lightweight compile: Problem with TmConDef in exprTyTransform"
    in smap_Expr_Type rec t

  sem tyTransform =
  | t -> smap_Type_Type tyTransform t
  -- Function type a -> b becomes [Int] -> a -> b
  | TyArrow r ->
    let i = tyWithInfo r.info in
    let from = tyTransform r.from in
    let to = tyTransform r.to in
    (i (tyarrow_ (i (tyseq_ (i tyint_)))
        (TyArrow { r with from = from, to = to })))

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
  error "--mcmc-lw-gprob must be between 0.0 and 1.0"
  else ();
  if or (ltf mProb 0.0) (gtf mProb 1.0) then
  error "--mcmc-lw-mprob must be between 0.0 and 1.0"
  else ();

  if options.align then
    ("mcmc-lightweight/runtime-aligned.mc", compileAligned options)
  else
    ("mcmc-lightweight/runtime.mc", compile options)
