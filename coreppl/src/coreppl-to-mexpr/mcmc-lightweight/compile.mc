include "name.mc"
include "mexpr/const-arity.mc"
include "mexpr/cps.mc"

include "../dists.mc"
include "../../inference/smc.mc"
include "../../cfa.mc"
include "../../dppl-arg.mc"
include "mexpr/ast-builder.mc"
include "mexpr/type.mc"
include "mexpr/const-types.mc"

let addrName = nameSym "addr"
let sym: Ref Int = ref 0
let uniqueSym = lam.
  let s = deref sym in
  modref sym (addi s 1);
  s

lang MExprPPLLightweightMCMC =
  MExprPPL + Resample + TransformDist + MExprCPS + MExprANFAll + MExprPPLCFA + MExprArity

  -------------------------
  -- STATIC ALIGNED MCMC --
  -------------------------
  sem compileAligned : Options -> (Expr,Expr) -> Expr
  sem compileAligned options =
  | (t,_) ->

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
    TmLet { r with body =
      if setMem ident unalignedNames then
        i (appf2_ (i (var_ "sampleUnaligned")) (i (int_ (uniqueSym ()))) t.dist)
      else
        i (appf1_ (i (var_ "sampleAligned")) t.dist)
    }

  | TmObserve t ->
    let i = withInfo t.info in
    let weight = i (appf2_ (i (var_ "logObserve")) t.dist t.value) in
    i (appf1_ (i (var_ "updateWeight")) weight)

  | TmWeight t ->
    let i = withInfo t.info in
    i (appf1_ (i (var_ "updateWeight")) t.weight)

  | TmResample t -> withInfo t.info unit_

  | t -> t

  ------------------------------------------
  -- DYNAMIC ("LIGHTWEIGHT") ALIGNED MCMC --
  ------------------------------------------
  sem compile : Options -> (Expr,Expr) -> Expr
  sem compile options =
  | (_,t) ->

    -- Addressing transform combined with CorePPL->MExpr transform
    let t = transformAddr (setEmpty nameCmp) t in

    -- Type addressing transform
    let t = mapPre_Expr_Expr exprTyTransform t in

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    -- Initialize addr to the empty list (not rope) at the
    -- beginning of the program.
    let t = bind_ (nulet_ addrName (var_ "emptyAddress")) t in

    t

  sem addr: Info -> Expr
  sem addr =
  | i ->
    let i = withInfo i in
    let s = uniqueSym () in
    i (appf2_ (i (var_ "constructAddress")) (i (nvar_ addrName)) (i (int_ s)))

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
        i (nlam_ addrName (tycon_ "Address") (i (nulam_ v acc)))
      ) inner varNames

  sem transformAddr: Set Name -> Expr -> Expr
  sem transformAddr externalIds =

  | t -> smap_Expr_Expr (transformAddr externalIds) t

  | TmLam _ & t ->
    match smap_Expr_Expr (transformAddr externalIds) t with TmLam r & t in
    let i = withInfo r.info in
    i (nlam_ addrName (tycon_ "Address") t)

  | TmConst r & t ->
    if isHigherOrderFunType (tyConst r.val) then
      -- TODO(dlunde,2022-09-19): Add support for higher-order constant functions
      errorSingle [r.info]
        "Higher-order constant functions not yet supported in addressing transformAddr"
    else
      transformConst (constArity r.val) t

  -- NOTE(dlunde,2022-10-24): We keep track of externals currently in scope.
  -- Note that the ANF transformation "replaces" externals with their eta
  -- expansions. Hence, we must also remove externals from scope after a `let`
  -- defines them anew.
  | TmExt r & t ->
    TmExt { r with inexpr = transformAddr (setInsert r.ident externalIds) r.inexpr }
  | TmLet r ->
    let body = transformAddr externalIds r.body in
    let inexpr = transformAddr (setRemove r.ident externalIds) r.inexpr in
    TmLet { r with body = body, inexpr = inexpr }

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
        let r = { r with rhs = transformAddr externalIds r.rhs } in

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
          let app = TmApp { r with lhs = transformAddr externalIds r.lhs } in
          (transformApp app, 0)

      else error "Impossible"
    in

    match rec t 0 with (t,remainingConstArity) in
    transformConst remainingConstArity t

  | TmAssume r ->
    let i = withInfo r.info in
    let dist = transformAddr externalIds r.dist in
    i (appf2_ (i (var_ "sample")) (addr r.info) dist)

  | TmObserve r ->
    let i = withInfo r.info in
    let dist = transformAddr externalIds r.dist in
    let value = transformAddr externalIds r.value in
    let weight = i (appf2_ (i (var_ "logObserve")) dist value) in
    i (appf1_ (i (var_ "updateWeight")) weight)

  | TmWeight r ->
    let i = withInfo r.info in
    let weight = transformAddr externalIds r.weight in
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
  -- Don't touch the types of externals
  | TmExt r -> TmExt r

  sem tyTransform =
  | t -> smap_Type_Type tyTransform t
  | TyArrow r & t ->
    let i = tyWithInfo r.info in
    let from = tyTransform r.from in
    let to = tyTransform r.to in
    (i (tyarrow_ (i (tycon_ "Address"))
        (TyArrow { r with from = from, to = to })))
  -- NOTE(2023-08-08,dlunde): Many TmTypes are shared with non-PPL code and
  -- transformed versions are removed when removing duplicate code.
  -- Therefore, we have to simply replace TyCon and TyApp with Unknown here.
  | (TyCon { info = info } | TyApp { info = info } ) ->
    let i = tyWithInfo info in i tyunknown_

  -------------------------------
  -- STATIC ALIGNED MCMC (CPS) --
  -------------------------------
  -- Extension to Expr specific for this compiler. Used to track
  -- unaligned/aligned assumes in CPS.
  syn Expr =
  | TmAssumeUnaligned { dist: Expr, ty: Type, info: Info }

  sem exprCps env k =

  -- This is where we use the continuation (aligned assumes)
  | TmLet ({ ident = ident, body = TmAssume { dist = dist },
            inexpr = inexpr } & r) & t ->
    let i = withInfo (infoTm t) in
    if not (transform env ident) then
      -- Unaligned TmAssume should not appear here due to transform
      errorSingle [r.info] "Impossible in exprCps"
    else
      let k =
        if tailCall t then
          match k with Some k then k
          else error "Something went wrong with partial CPS transformation"
        else i (nulam_ ident (exprCps env k inexpr))
      in
      i (appf2_ (i (var_ "sampleAligned")) dist k)

  -- Ignore unaligned assumes for now
  | TmLet ({ body = TmAssumeUnaligned _ } & t) ->
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
  | TmLet ({ body = TmWeight _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmObserve _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }

  -- NOTE(2023-08-08,dlunde): Many TmTypes are shared with non-PPL code and
  -- transformed versions are removed when removing duplicate code.
  -- Therefore, we have to simply replace TyCon and TyApp with Unknown here.
  sem tyCps env =
  | (TyCon { info = info } | TyApp { info = info } ) ->
    let i = tyWithInfo info in i tyunknown_

  sem transformPreAlignedCps unalignedNames =
  | TmLet ({ ident = ident, body = TmAssume r} & b) & t ->
    if setMem ident unalignedNames then
      TmLet {b with body = TmAssumeUnaligned r}
    else t

  | t -> t

  -- Compile CorePPL constructs to MExpr
  sem transformPostAlignedCps =
  | TmAssumeUnaligned t ->
    let i = withInfo t.info in
    i (appf2_ (i (var_ "sampleUnaligned")) (i (int_ (uniqueSym ()))) t.dist)
  | TmAssume r -> errorSingle [r.info] "Some TmAssume's were not replaced in CPS"
  | TmObserve t ->
    let i = withInfo t.info in
    let weight = i (appf2_ (i (var_ "logObserve")) t.dist t.value) in
    i (appf1_ (i (var_ "updateWeight")) weight)

  | TmWeight t ->
    let i = withInfo t.info in
    i (appf1_ (i (var_ "updateWeight")) t.weight)

  | TmResample t -> withInfo t.info unit_

  | t -> t

  sem compileAlignedCps : Options -> (Expr,Expr) -> Expr
  sem compileAlignedCps options =
  | (_,t) ->

    -- printLn ""; printLn "--- INITIAL ANF PROGRAM ---";
    -- match pprintCode 0 pprintEnvEmpty t with (env,str) in
    -- printLn (str);
    -- dprint t;

    -- Alignment analysis
    let alignRes = alignCfa t in
    let unalignedNames: Set Name = extractUnaligned alignRes in

    -- printLn ""; printLn "--- UNALIGNED NAMES ---";
    -- match mapAccumL pprintEnvGetStr env (setToSeq unalignedNames) with (env,strings) in
    -- printLn (join [ "[", strJoin "," strings, "]"]);

    -- CPS transformation
    let t =

      let cont = (ulam_ "x" (var_ "x")) in

      match options.cps with "partial" then
        -- Partial checkpoint/suspension analysis
        let checkpoint = lam t.
          match t with TmLet { ident = ident, body = TmAssume _ } then
            not (setMem ident unalignedNames)
          else false
        in
        let checkPointNames: Set Name =
          extractCheckpoint (checkpointCfa checkpoint t) in

        -- printLn ""; printLn "--- CHECKPOINT ANALYSIS RESULT ---";
        -- match mapAccumL pprintEnvGetStr env (setToSeq checkPointNames) with (env,strings) in
        -- printLn (join [ "[", strJoin "," strings, "]"]);

        let t = mapPre_Expr_Expr (transformPreAlignedCps unalignedNames) t in
        cpsPartialCont (lam n. setMem n checkPointNames) cont t

      else match options.cps with "full" then
        let t = mapPre_Expr_Expr (transformPreAlignedCps unalignedNames) t in
        cpsFullCont cont t

      else error ( join [ "Invalid CPS option:", options.cps ])

    in

    -- Transform samples, observes, and weights to MExpr
    let t = mapPre_Expr_Expr transformPostAlignedCps t in

    -- Transform distributions to MExpr distributions
    let t = mapPre_Expr_Expr transformTmDist t in

    t

  ------------------------------------------------
  -- DYNAMIC ("LIGHTWEIGHT") ALIGNED MCMC (CPS) --
  ------------------------------------------------
  -- TODO(dlunde,2022-11-03)

end

let compilerLightweightMCMC = lam options. use MExprPPLLightweightMCMC in

  -- Check that options are within the proper range
  let gProb = options.mcmcLightweightGlobalProb in
  (if or (ltf gProb 0.0) (gtf gProb 1.0) then
  error "--mcmc-lw-gprob must be between 0.0 and 1.0"
  else ());

  switch (options.align, options.cps)
    case (true,"none") then
      ("mcmc-lightweight/runtime-aligned.mc", compileAligned options)
    case (true,_) then
      ("mcmc-lightweight/runtime-aligned-cps.mc", compileAlignedCps options)
    case (false,"none") then
      ("mcmc-lightweight/runtime.mc", compile options)
    case (false,_) then
      -- TODO(2023-05-17,dlunde): Currently the same as non-CPS. Switch to
      -- CPS-version when implemented.
      ("mcmc-lightweight/runtime.mc", compile options)
  end
