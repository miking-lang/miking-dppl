include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type.mc"
include "mexpr/utils.mc"
include "mexpr/free-vars.mc"
include "mexpr/phase-stats.mc"
include "mexpr/generate-pprint.mc"
include "mexpr/resymbolize.mc"
include "sys.mc"
include "map.mc"
include "mlang/loader.mc"

include "../coreppl.mc"
include "../inference/smc.mc"
include "../parser.mc"
include "../dppl-arg.mc"
include "../static-delay.mc"


include "extract.mc"
include "dists.mc"
include "inference-interface.mc"

include "pruning/compile.mc"
include "delayed-sampling/compile.mc"

lang DPPLReplace = Ast
  sem replaceDpplKeywords : {path : String, env : SymEnv} -> Expr -> Expr
  sem replaceDpplKeywords distEnv =
  | t ->
    let t = smap_Expr_Expr (replaceDpplKeywords distEnv) t in
    let t = smap_Expr_Type replaceDpplKeywordsType t in
    let t = smap_Expr_TypeLabel replaceDpplKeywordsType t in
    let t = smap_Expr_Pat replaceDpplKeywordsPat t in
    withType (replaceDpplKeywordsType (tyTm t)) t

  sem replaceDpplKeywordsType : Type -> Type
  sem replaceDpplKeywordsType =
  | ty -> smap_Type_Type replaceDpplKeywordsType ty

  sem replaceDpplKeywordsPat : Pat -> Pat
  sem replaceDpplKeywordsPat =
  | p ->
    let p = smap_Pat_Pat replaceDpplKeywordsPat p in
    withTypePat (replaceDpplKeywordsType (tyPat p)) p
end

lang DPPLKeywordReplace = DPPLReplace + DPPLParser + SymGetters
  sem _makeError : Info -> String -> Expr
  sem _makeError info =
  | keywordStr ->
    let msg = join ["Cannot use ", keywordStr, " outside of inferred model"] in
    let toStr = lam msg.
      map
        (lam ch. TmConst {val = CChar {val = ch},
                          ty = TyChar {info = info}, info = info})
        msg
    in
    TmApp {
      lhs = TmConst {val = CError (), ty = TyUnknown {info = info}, info = info},
      rhs = TmSeq {tms = toStr msg,
                   ty = TySeq {ty = TyChar {info = info}, info = info},
                   info = info},
      ty = TyUnknown {info = info}, info = info}

  sem replaceDpplKeywords distEnv =
  | TmAssume t -> 
    app_ (nvar_ (_getVarExn "sample" distEnv)) (replaceDpplKeywords distEnv t.dist)
  | TmObserve t -> _makeError t.info "observe"
  | TmWeight t -> _makeError t.info "weight"
  | TmResample t -> _makeError t.info "resample"
  | TmCancel t -> _makeError t.info "cancel"
end

lang DPPLDelayedReplace = DPPLReplace + DPPLParser
  sem replaceDpplKeywords distEnv =
  | TmDelay t ->
    replaceDpplKeywords distEnv (TmAssume {
      dist = t.dist, ty = t.ty, info = t.info, driftKernel = None ()
    })
  | TmDelayed t -> replaceDpplKeywords distEnv t.delay

   sem replaceDpplKeywordsType : Type -> Type
   sem replaceDpplKeywordsType =
  | TyDelayInt t -> TyInt {info = t.info}
  | TyDelayFloat t -> TyFloat {info = t.info}
  | TyDelaySeqF t -> TySeq {info = t.info, ty = TyFloat {info = t.info}}
end

lang DPPLPrunedReplace = DPPLReplace + SymGetters + DPPLParser
  sem replaceCancel env =
  | (TmCancel t) ->
    let i = withInfo t.info in
    TmWeight { weight = negf_ (appf2_ (withInfo t.info (uconst_ (CDistLogObserve ())))
 t.dist t.value),
               info = t.info,
               ty = t.ty}
  | t -> smap_Expr_Expr (replaceCancel env) t

  sem replaceDpplKeywords distEnv =
  | TmPrune t -> replaceDpplKeywords distEnv (assume_ t.dist)
  | TmPruned t -> replaceDpplKeywords distEnv t.prune

  sem replaceDpplKeywordsType : Type -> Type
  sem replaceDpplKeywordsType =
  | TyPruneInt t -> TyInt {info = t.info}
end

-- Provides runtime implementations for elementary functions that are not MExpr
-- intrisics.
lang ElementaryFunctionsTransform = ElementaryFunctions
  -- The given function should look up the symbolized name for the
  -- given elementary function.
  sem elementaryFunctionsTransformExpr : (String -> Name) -> Expr -> Expr
  sem elementaryFunctionsTransformExpr stringToName =
  | tm & TmConst r -> elementaryFunctionsTransformConst stringToName tm r.val
  | tm -> smap_Expr_Expr (elementaryFunctionsTransformExpr stringToName) tm

  sem elementaryFunctionsTransformConst : (String -> Name) -> Expr -> Const -> Expr
  sem elementaryFunctionsTransformConst stringToName tm =
  | CSin _ -> withInfo (infoTm tm) (nvar_ (stringToName "sin"))
  | CCos _ -> withInfo (infoTm tm) (nvar_ (stringToName "cos"))
  | CSqrt _ -> withInfo (infoTm tm) (nvar_ (stringToName "sqrt"))
  | CExp _ -> withInfo (infoTm tm) (nvar_ (stringToName "exp"))
  | CLog _ -> withInfo (infoTm tm) (nvar_ (stringToName "log"))
  | CPow _ -> withInfo (infoTm tm) (nvar_ (stringToName "pow"))
  | _ -> tm

  sem _elementaryFunctionsTransformRuntimeIds =| _ -> [
    "sin",
    "cos",
    "sqrt",
    "exp",
    "log",
    "pow"
  ]
end

lang ReplaceHigherOrderConstants
  = SeqOpAst
  sem _replaceHigherOrderConstant : Const -> Option String
  sem _replaceHigherOrderConstant =
  | CMap _ -> Some "map"
  | CMapi _ -> Some "mapi"
  | CIter _ -> Some "iter"
  | CIteri _ -> Some "iteri"
  | CFoldl _ -> Some "foldl"
  | CFoldr _ -> Some "foldr"
  | CCreate _ | CCreateList _ -> Some "create"
  | _ -> None ()

  sem replaceHigherOrderConstants : {path : String, env : SymEnv} -> Expr -> Expr
end

lang ReplaceHigherOrderConstantsLoadedPreviously = ReplaceHigherOrderConstants + SymGetters
  sem replaceHigherOrderConstants env =
  | tm -> smap_Expr_Expr (replaceHigherOrderConstants env) tm
  | tm & TmConst x ->
    match _replaceHigherOrderConstant x.val with Some name then
      withType x.ty (withInfo x.info (nvar_ (_getVarExn name env)))
    else tm
end

-- NOTE(larshum, 2025-10-27): Within a model, we want to resymbolize all
-- locally bound names in bindings and patterns. However, we do not resymbolize
-- types, as all models refer to the types defined at top-level (and the types
-- defined within a model are removed at a later stage).
lang DPPLResymbolizeModel =
  Resymbolize + ResymbolizeVar + ResymbolizeLam + ResymbolizeMatch +
  ResymbolizeDecl + ResymbolizeLetDecl + ResymbolizeRecLetsDecl +
  ResymbolizeNamedPat + ResymbolizeSeqEdgePat

  sem resymbolizeExpr : Map Name Name -> Expr -> Expr
  sem resymbolizeExpr nameMap =
  | t -> smap_Expr_Expr (resymbolizeExpr nameMap) t

  sem resymbolizeDecl : Map Name Name -> Decl -> (Map Name Name, Decl)
  sem resymbolizeDecl nameMap =
  | d -> (nameMap, smap_Decl_Expr (resymbolizeExpr nameMap) d)

  sem resymbolizePat : Map Name Name -> Pat -> (Map Name Name, Pat)
  sem resymbolizePat nameMap =
  | p -> smapAccumL_Pat_Pat resymbolizePat nameMap p

  sem resymbolizeType : Map Name Name -> Type -> Type
  sem resymbolizeType nameMap =
  | ty -> ty
end

lang CompileModels = ReplaceHigherOrderConstants + PhaseStats + MExprANFAll + DPPLExtract + InferenceInterface + DPPLDelayedSampling + DPPLResymbolizeModel
  type CompileEnvs =
    { higherOrderSymEnv : {path : String, env : SymEnv}
    , distEnv : {path : String, env : SymEnv}
    , externalMathEnv : {path : String, env : SymEnv}
    }
  sem compileModels
    : TransformationOptions
    -> Map Name FinalOrderedLamLiftSolution
    -> CompileEnvs
    -> Map InferMethod {env : {path : String, env : SymEnv}, stateType : Type, extraEnvs : Map String {path : String, env : SymEnv}}
    -> Map Name ModelRepr
    -> Map Name Decl
  sem compileModels options lamliftSols envs runtimes =
  | models ->
    mapMapWithKey
      (lam id. lam model.
        match model with {extractAst = extractAst, method = method, params = params} in
        match mapLookup method runtimes with Some entry then
          let extractAst = lam f. transformModelAst envs options method (extractAst f) in
          let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in
          let ast = compileModel options lamliftSols envs entry id {model with extractAst = extractAst} in
          endPhaseStatsExpr log "compile-model-one" (bind_ ast unit_);
          let ast = smap_Decl_Expr removeModelDefinitions ast in
          endPhaseStatsExpr log "remove-model-definitions-one" (bind_ ast unit_);
          ast
        else
          match pprintInferMethod 0 pprintEnvEmpty method with (_, methodStr) in
          error (join ["Runtime definition missing for (", methodStr, ")"]))
      models

  sem transformModelAst : CompileEnvs -> TransformationOptions -> InferMethod -> Expr -> Expr
  sem transformModelAst envs options method =
  | modelAst ->
    -- Transform the model AST, if the flag is set
    let ast = if options.staticDelay
      then staticDelay (_getVarExn "externalSqrt" envs.externalMathEnv) modelAst
      else modelAst in
    -- Replace pruning constructs with normal constructs unless
    -- pruning is used by the infer method
    let ast = if retainPruning method
      then ast
      else
        use DPPLPrunedReplace in replaceDpplKeywords envs.distEnv (replaceCancel envs.distEnv ast) in
    let ast = if retainDynamicDelayedSampling method
      then ast
      else use DPPLDelayedReplace in replaceDpplKeywords envs.distEnv ast in
    -- Optionally print the model AST
    (if options.printModel then
      printLn (mexprPPLToString ast)
    else ());

    ast

  sem compileModel
    : TransformationOptions
    -> Map Name FinalOrderedLamLiftSolution
    -> CompileEnvs
    -> {env : {path : String, env : SymEnv}, stateType : Type, extraEnvs : Map String {path : String, env : SymEnv}}
    -> Name
    -> ModelRepr
    -> Decl
  sem compileModel options lamliftSols envs entry modelId =
  | {extractAst = extractAst, params = modelParams, method = method} ->
    let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in

    -- ANF
    let extractAst = lam f. normalizeTerm (extractAst f) in

    -- ANF with higher-order intrinsics replaced with alternatives in
    -- seq-native.mc
    -- TODO(dlunde,2022-10-24): @Lars I'm not sure how I should combine this
    -- with your updates.

    -- Apply inference-specific transformation
    let stateVarId = nameNoSym "state" in
    let interface =
      { extractNormal = lam f. extractAst f
      , extractNoHigherOrderConsts = lam f. extractAst (lam ast. replaceHigherOrderConstants envs.higherOrderSymEnv (f ast))
      , options = options
      , runtime = {env = entry.env, lamliftSols = lamliftSols}
      , dists = {env = envs.distEnv, lamliftSols = lamliftSols}
      , extraEnvs = mapMap (lam env. {env = env, lamliftSols = lamliftSols}) entry.extraEnvs
      , stateName = stateVarId
      } in
    let ast = pickCompiler method interface in
    endPhaseStatsExpr log "compile-inference-one" ast;

    let ast = resymbolizeBindings ast in
    endPhaseStatsExpr log "resymbolize-bindings-one" ast;

    -- Bind the model code in a let-expression, which we can insert in the main
    -- AST.
    let ast =
      nulet_ modelId
        (nlams_ (snoc modelParams (stateVarId, entry.stateType)) ast) in
    endPhaseStatsExpr log "insert-model-params-one" (bind_ ast unit_);

    -- Replace any occurrences of TyDist in the program with the runtime
    -- distribution type. This needs to be performed after the previous step as
    -- captured parameters may have type TyDist.
    let ast = replaceTyDistDecl {env = envs.distEnv, lamliftSols = lamliftSols} ast in
    endPhaseStatsExpr log "replace-ty-dist-one" (bind_ ast unit_);

    ast

  -- Removes all definitions of types, constructors, and externals from the
  -- model AST.
  --
  -- NOTE(larshum, 2022-10-22): We assume that the model code does not contain
  -- local definitions, but that any that are included are due to the
  -- extraction including all dependencies. Under this assumption, the
  -- definition is also present in the CorePPL program, and thus we can safely
  -- remove it from the model code.
  sem removeModelDefinitions : Expr -> Expr
  sem removeModelDefinitions =
  | TmDecl (x & {decl = DeclType _}) -> removeModelDefinitions x.inexpr
  | TmDecl (x & {decl = DeclConDef _}) -> removeModelDefinitions x.inexpr
  | TmDecl (x & {decl = DeclExt _}) -> removeModelDefinitions x.inexpr
  | t -> smap_Expr_Expr removeModelDefinitions t
end

lang InsertModels = VarAst + DeclAst
  -- We insert each model right before its first use.  This is simple
  -- but correct, as we only need them to be placed after the
  -- corresponding runtime code.
  sem insertModels : Map Name Decl -> Expr -> Expr
  sem insertModels models =
  | TmDecl x ->
    let usedModels = sfold_Decl_Expr (modelsUsedInBody models) (mapEmpty nameCmp) x.decl in
    let usedModels = mapIntersectWith (lam a. lam. a) models usedModels in
    let laterModels = mapDifference models usedModels in
    let inexpr = insertModels laterModels x.inexpr in
    bindall_ (snoc (mapValues usedModels) x.decl) inexpr
  | tm ->
    bindall_ (mapValues models) tm

  sem modelsUsedInBody : all x. Map Name x -> Set Name -> Expr -> Set Name
  sem modelsUsedInBody models acc =
  | TmVar t ->
    if mapMem t.ident models
    then setInsert t.ident acc
    else acc
  | t -> sfold_Expr_Expr (modelsUsedInBody models) acc t
end

-- This fragment extends the loader core language (the format each
-- added Decl must be in) to coreppl
lang CPPLLoader
  = MCoreLoader
  + ResolveType + SubstituteUnknown
  + ReplaceHigherOrderConstantsLoadedPreviously + CompileModels + InsertModels
  + ElementaryFunctionsTransform + DPPLPrunedReplace
  + DPPLKeywordReplace + DPPLDelayedReplace + DPPLParser
  syn Hook =
  | CPPLHook
    { options : TransformationOptions
    , runtimes : Ref (Map InferMethod {env : {path : String, env : SymEnv}, stateType : Type, extraEnvs : Map String {path : String, env : SymEnv}})
    , envs :
      { higherOrderSymEnv : {path : String, env : SymEnv}
      , distEnv : {path : String, env : SymEnv}
      , externalMathEnv : {path : String, env : SymEnv}
      }
    }

  sem enableCPPLCompilation : TransformationOptions -> Loader -> Loader
  sem enableCPPLCompilation options = | loader ->
    if hasHook (lam x. match x with CPPLHook _ then true else false) loader then loader else

    match includeFileExn "." "stdlib::ext/math-ext.mc" loader with (externalMathEnv, loader) in

    let preSymEnv = _getSymEnv loader in
    -- NOTE(vipa, 2024-12-12): We load these constants but keep them
    -- outside the symbolization environment. We later insert direct
    -- references to these names instead of constants, when needed.
    -- WARNING: This hides names from seq-native.mc and its transitive
    -- dependencies from symbolize forever, even if some later file
    -- includes one of them directly. This would work better if we
    -- didn't use the implicit symbolize environment, rather that we
    -- used the SymEnv for each included file directly, which is *not*
    -- hidden and does not need to be.
    match includeFileExn "." "stdlib::seq-native.mc" loader with (symEnv, loader) in
    let loader = _setSymEnv preSymEnv loader in

    -- NOTE(vipa, 2024-12-12): Insert compileOptions declaration
    -- before everything else.
    -- TODO(vipa, 2024-12-12): This could technically get captured, we
    -- should pass the name directly to places
    let compileOptions = DeclLet
      { body = urecord_
        -- NOTE(dlunde,2022-11-04): Emulating option type
        [ ("seedIsSome", match options.seed with Some seed then bool_ true else bool_ false)
        , ("seed", match options.seed with Some seed then int_ seed else int_ 0)
        ]
      , ident = nameNoSym "compileOptions"
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , info = NoInfo ()
      } in
    let loader = _addDeclExn loader compileOptions in

    -- NOTE(vipa, 2024-12-17): Load the runtime distribution
    -- support. This places the related built-in functions in the
    -- running symbolize environment
    match includeFileExn "." "coreppl::coreppl-to-mexpr/runtime-dists.mc" loader with (distEnv, loader) in
    let distBuiltins =
      [ ("distEmpiricalSamples", CDistEmpiricalSamples ())
      , ("distEmpiricalDegenerate", CDistEmpiricalDegenerate ())
      , ("distEmpiricalNormConst", CDistEmpiricalNormConst ())
      , ("distEmpiricalAcceptRate", CDistEmpiricalAcceptRate ())
      , ("expectation", CDistExpectation ())
      , ("logObserve", CDistLogObserve ())
      ] in
    let f = lam loader. lam pair.
      let decl = DeclLet
        { ident = nameNoSym pair.0
        , tyAnnot = tyunknown_
        , tyBody = tyunknown_
        , body = ulam_ "x" (app_ (uconst_ pair.1) (var_ "x"))
        , info = NoInfo ()
        } in
      _addDeclExn loader decl in
    let loader = foldl f loader distBuiltins in

    let distAlias = DeclType
      { ident = nameNoSym "Dist"
      , params = [nameNoSym "ty"]
      , tyIdent = TyDist {info = NoInfo (), ty = tyvar_ "ty"}
      , info = NoInfo ()
      } in
    let loader = _addDeclExn loader distAlias in

    let hook = CPPLHook
      { options = options
      , runtimes = ref (mapEmpty cmpInferMethod)
      , envs =
        { higherOrderSymEnv = symEnv
        , distEnv = distEnv
        , externalMathEnv = externalMathEnv
        }
      } in
    addHook loader hook

  sem _preSymbolize loader decl = | CPPLHook x ->
    let requiredRuntimes =
      recursive let findRuntimes = lam acc. lam tm.
        let acc = match tm with TmInfer t
          then setInsert t.method acc
          else acc in
        sfold_Expr_Expr findRuntimes acc tm in
      sfold_Decl_Expr findRuntimes (setEmpty cmpInferMethod) decl in
    let f = lam loader. lam inferMethod.
      if mapMem inferMethod (deref x.runtimes) then loader else
      match pickRuntime inferMethod with (runtime, extraEnvs) in
      match includeFileExn "." (join ["coreppl::coreppl-to-mexpr/", runtime]) loader with (symEnv, loader) in
      let f = lam loader. lam. lam path.
        match includeFileExn "." (join ["coreppl::coreppl-to-mexpr/", path]) loader with (env, loader) in
        (loader, env) in
      match mapMapAccum f loader extraEnvs with (loader, extraEnvs) in

      let entry =
        let stateName = _getTyConExn "State" symEnv in
        let tcEnv = _getTCEnv loader in
        match mapFindExn stateName tcEnv.tyConEnv with (_, params, _) in
        let stateType = tyapps_ (ntycon_ stateName) (map (lam. tyunknown_) params) in
        let stateType = substituteUnknown (NoInfo ()) tcEnv (Poly ()) stateType in
        let stateType = resolveType (NoInfo ()) tcEnv false stateType in
        { env = symEnv
        , stateType = stateType
        , extraEnvs = extraEnvs
        } in
      modref x.runtimes (mapInsert inferMethod entry (deref x.runtimes));
      loader in
    (setFold f loader requiredRuntimes, decl)

  sem _postBuildFullAst loader ast = | CPPLHook hook ->
    let options = hook.options in
    let runtimes = deref hook.runtimes in
    let envs = hook.envs in
    let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in
    let ast = removeMetaVarExpr ast in
    endPhaseStatsExpr log "remove-meta-var" ast;
    let runtimeRunNames = mapMap (lam entry. _getVarExn "run" entry.env) runtimes in
    let ast = elementaryFunctionsTransformExpr (lam str. _getVarExn str envs.externalMathEnv) ast in
    endPhaseStatsExpr log "elementary-functions-transform" ast;
    match extractInfer options runtimeRunNames ast with (ast, lamliftSols, models) in
    endPhaseStatsExpr log "extract-infer" ast;
    let models = compileModels options lamliftSols envs runtimes models in
    let ast = mapPre_Expr_Expr (transformTmDist {env = envs.distEnv, lamliftSols = lamliftSols}) ast in
    endPhaseStatsExpr log "replace-tm-dist" ast;
    let ast = replaceDpplKeywords envs.distEnv ast in
    endPhaseStatsExpr log "replace-dppl-keywords" ast;
    let ast = insertModels models ast in
    endPhaseStatsExpr log "insert-models" ast;
    ast

  syn Hook =
  | DefaultInferMethodHook
    { inferMethod : InferMethod
    }

  sem enableDefaultInferMethod : InferMethod -> Loader -> Loader
  sem enableDefaultInferMethod inferMethod = | loader ->
    if hasHook (lam x. match x with DefaultInferMethodHook _ then true else false) loader then loader else
    addHook loader (DefaultInferMethodHook {inferMethod = inferMethod})

  sem _preSymbolize loader decl = | DefaultInferMethodHook x ->
    (loader, smap_Decl_Expr (replaceDefaultInferMethod x.inferMethod) decl)
end

lang ODELoader = SolveODE + MCoreLoader + MExprSubstitute
  -- Make transformations related to solveode. This pass removes all solveode
  -- terms and returns a transformed term and an ODE related runtime. the
  -- tranformed program can be treated like a normal probabilistic program.
  sem transformTmSolveODE : {path : String, env : SymEnv} -> Expr -> Expr
  sem transformTmSolveODE symEnv =
  | TmSolveODE r ->
    let method = odeDefaultMethod r.method in
    let fn = nvar_ (_getVarExn (odeSolverName method) symEnv) in
    let args = concat (odeSolverArgs method) [r.model, r.init, r.endTime] in
    appSeq_ fn args
  | tm -> smap_Expr_Expr (transformTmSolveODE symEnv) tm

  sem odeSolverName : ODESolverMethod -> String
  sem odeSolverName =
  | RK4 _ -> "odeSolverRK4Solve"
  | EF _ -> "odeSolverEFSolve"
  | EFA _ -> "odeSolverEFASolve"
  | method -> error (join [
    nameGetStr (odeSolverMethodName method),
    " does not have an implementation in the ODE solver runtime"
  ])

  -- Maps ODE solver method to its method arguments.
  sem odeSolverArgs : ODESolverMethod -> [Expr]
  sem odeSolverArgs =
  | ODESolverDefault r | RK4 r | EF r -> [r.add, r.smul, r.stepSize]
  | EFA r -> [r.add, r.smul, r.stepSize, r.n]

  -- Replaces default ODE solver methods with a concrete method.
  sem odeDefaultMethod : ODESolverMethod -> ODESolverMethod
  sem odeDefaultMethod =
  | ODESolverDefault d -> RK4 d
  | m -> m

  syn Hook =
  | ODEHook ()
  sem _preSymbolize loader decl = | ODEHook _ ->
    recursive let hasTmSolveODE = lam acc. lam tm.
      match tm with TmSolveODE _ then true
      else sfold_Expr_Expr hasTmSolveODE acc tm
    in
    if sfold_Decl_Expr hasTmSolveODE false decl then
      match includeFileExn "." "coreppl::coreppl-to-mexpr/runtime-ode.mc" loader
        with (symEnv, loader)
      in
      (loader, smap_Decl_Expr (transformTmSolveODE symEnv) decl)
    else
      (loader, decl)
end

lang ADLoader = MCoreLoader + CorePPL + Delayed + Diff +
  ElementaryFunctions + PrettyPrint + TyConst

  ------------------------------------------------------------------------------
  -- Hooks
  ------------------------------------------------------------------------------

  type ADConfig = {insertFloatAssertions : Bool}

  type ADHookEnv = {
    adRuntimeEnv : {path : String, env : SymEnv},
    config : ADConfig
  }

  syn Hook =
  | ADHook ADHookEnv

  sem _postTypecheck loader decl =| ADHook env ->
    adPostTypecheck env loader decl

  sem prepareADRuntime : Loader -> ADConfig -> (Hook, Loader)
  sem prepareADRuntime loader =| config ->
    match includeFileExn "." "coreppl::coreppl-to-mexpr/runtime-ad.mc" loader
      with (adRuntimeEnv, loader) in
    (ADHook {
      adRuntimeEnv = adRuntimeEnv,
      config = config
    }, loader)

  ------------------------------------------------------------------------------
  -- Transformations and Static Assertions
  ------------------------------------------------------------------------------

  sem _unwrapTypes =| ty -> smap_Type_Type _unwrapTypes (unwrapType ty)
  sem _tyTm =| e -> _unwrapTypes (tyTm e)

  sem adPostTypecheck : ADHookEnv -> Loader -> Decl -> (Loader, Decl)
  sem adPostTypecheck env loader =| decl ->
    match adPostTypecheckH env loader decl with (loader, decl) in
    let f = compose (adLiftExpr env) adAssertWellTypedDiff in
    (loader, smap_Decl_Expr f decl)

  sem adPostTypecheckH : ADHookEnv -> Loader -> Decl -> (Loader, Decl)
  sem adPostTypecheckH env loader =
  | d & DeclExt r ->
    let loader =
      if env.config.insertFloatAssertions then
        let tyIdent = _unwrapTypes r.tyIdent in
        if _hasFloatExprsMap tyIdent then
          let i = r.info in
          let decl_let = lam body. DeclLet {
            ident = r.ident,
            tyAnnot = TyUnknown { info = i },
            tyBody = r.tyIdent,
            body = body,
            info = i
          } in
          -- NOTE(oerikss, 2025-03-03): We use an intermediate eta expanded
          -- alias because externals needs to be fully applied.
          let _ps =
            recursive let recur = lam ty.
              switch ty
              case TyArrow r then snoc (recur r.to) (nameSym "p", r.from)
              case TyAll r then recur r.ty
              case _ then []
              end
            in
            recur tyIdent
          in
          let decl1 =
            let ps =
              map
                (lam _p.
                  -- Asserts parameters only if needed
                  _mapFloatExprsExpr i (adAssertFloat env i) (lam x. x) _p.1
                    (_var_ i _p.1 _p.0))
                _ps in
            let body =
              foldr (lam p. lam fn. _app_ i fn p) (_var_ i r.tyIdent r.ident)
                ps in
            let body =
              foldl (lam body. lam _p. _lam_ i _p.0 _p.1 body) body _ps in
            decl_let body in
          _queueAddDecl loader decl1
        else loader
      else loader in
    (loader, d)
  | decl -> (loader, decl)

  sem adAssertWellTypedDiff : Expr -> Expr
  sem adAssertWellTypedDiff =
  | e & TmDiff r ->
    recursive let isIsomorficToRn = lam ty.
      switch ty
      case TyFloat _ then true
      case TyRecord _ | TySeq _ then
        sfold_Type_Type (lam acc. lam ty. and acc (isIsomorficToRn ty)) true ty
      case _ then type2str ty; false
      end in
    if isIsomorficToRn (_tyTm r.arg) then
      if isIsomorficToRn (_unwrapTypes r.ty) then
        smap_Expr_Expr adAssertWellTypedDiff e
      else
        errorSingle [infoTm r.fn]
          "* The parameter type is not isomorphic to a tuple of floats"
    else
      errorSingle [infoTm r.fn]
        "* The return type is not isomorphic to a tuple of floats"
  | e -> smap_Expr_Expr adAssertWellTypedDiff e

  sem adLiftExpr : ADHookEnv -> Expr -> Expr
  sem adLiftExpr env =
  | TmDiff r ->
    match _tyTm r.fn with TyArrow tyr then
      let i = r.info in
      let _var_ = _var_ i in
      let _lam_ = _lam_ i in
      let _app_ = _app_ i in
      let _let_ = _let_ i in
      let ityfloat_ = ityfloat_ i in
      let _eps = nameSym "eps" in
      let eps = _var_ ityfloat_ _eps in
      let _pri = nameSym "pri" in
      let pri = _var_ tyr.from _pri in
      let _tgn = nameSym "tgn" in
      let tgn = _var_ tyr.from _tgn in
      let _res = nameSym "res" in
      let res = _var_ tyr.to _res in
      _let_ _eps
        (_app_ (adGetVarExn env i (ityarrow_ i tyunit_ ityfloat_) "geneps")
           (_unit_ i))
        (_let_ _pri (adLiftExpr env r.arg)
           (_let_ _tgn (adLiftExpr env r.darg)
              (_let_ _res
                 (_app_
                    (adLiftExpr env r.fn)
                    (adTypeDirectedDual env i eps pri tgn tyr.from))
                 (adTypeDirectedTangent env i eps res tyr.to))))
    else
      printErrorLn (type2str r.ty);
      error (_tyAssertErrMsg "adLiftExpr")
  | e & TmConst r -> adLiftConst env e r.val
  | e & (TmInfer _
       | TmDist _
       | TmObserve _
       | TmWeight _
       | TmPrune _
       | TmPruned _
       | TmCancel _
       | TmDelay _
       | TmDelayed _ ) ->
    if env.config.insertFloatAssertions then
      let f = lam e.
        let ty = _tyTm e in
        if _hasFloatExprsMap ty then
          let i = infoTm e in
          let _x = nameSym "x" in
          _let_ i _x e
            (_mapFloatExprsExpr i (adAssertFloat env i) (lam x. x) ty
               (_var_ i ty _x))
        else e
      in
      smap_Expr_Expr (compose f (adLiftExpr env)) e
    else smap_Expr_Expr (adLiftExpr env) e
  | e -> smap_Expr_Expr (adLiftExpr env) e

  sem adLiftConst : ADHookEnv -> Expr -> Const -> Expr
  sem adLiftConst env e =
  | CAddf _ -> adliftConstH env e "addf"
  | CMulf _ -> adliftConstH env e "mulf"
  | CSubf _ -> adliftConstH env e "subf"
  | CDivf _ -> adliftConstH env e "divf"
  | CNegf _ -> adliftConstH env e "negf"
  | CEqf _ -> adliftConstH env e "eqf"
  | CNeqf _ -> adliftConstH env e "neqf"
  | CLtf _ -> adliftConstH env e "ltf"
  | CLeqf _ -> adliftConstH env e "leqf"
  | CGtf _ -> adliftConstH env e "gtf"
  | CGeqf _ -> adliftConstH env e "geqf"
  | CSin _ -> adliftConstH env e "sin"
  | CCos _ -> adliftConstH env e "cos"
  | CExp _ -> adliftConstH env e "exp"
  | CLog _ -> adliftConstH env e "log"
  | CSqrt _ -> adliftConstH env e "sqrt"
  | CPow _ -> adliftConstH env e "pow"
  | CFloat2string _ -> adliftConstH env e "float2string"
  | const ->
    if env.config.insertFloatAssertions then
      let i = infoTm e in
      _mapFloatExprsExpr i (lam x. x) (adAssertFloat env i) (tyConst const) e
    else e

  sem adliftConstH env e =| name -> adGetVarExn env (infoTm e) (_tyTm e) name

  sem adGetVarExn env i ty =| name ->
    withType ty (withInfo i (nvar_ (_getVarExn name env.adRuntimeEnv)))

  sem adDual : ADHookEnv -> Info -> Expr -> Expr -> Expr -> Expr
  sem adDual env i eps pri =| tgn ->
    let ityfloat_ = ityfloat_ i in
    let dual =
      adGetVarExn env i
        (foldr1 (ityarrow_ i) [ityfloat_, ityfloat_, ityfloat_, ityfloat_])
        "dual" in
    _appf3_ i dual eps pri tgn

  sem adTangent : ADHookEnv -> Info -> Expr -> Expr -> Expr
  sem adTangent env i eps =| e ->
    let ityfloat_ = ityfloat_ i in
    let tangent =
      adGetVarExn env i
        (foldr1 (ityarrow_ i) [ityfloat_, ityfloat_, ityfloat_])
        "tangent" in
    _appf2_ i tangent eps e

  sem adTypeDirectedDual
    : ADHookEnv -> Info -> Expr -> Expr -> Expr -> Type -> Expr
  sem adTypeDirectedDual env i eps pri tgn =
  | TyFloat _ ->
    (switch map _tyTm [eps, pri, tgn]
     case [TyFloat _, TyFloat _, TyFloat _] then ()
     case tys then
      iter printErrorLn (map type2str tys);
      error (_tyAssertErrMsg "adTypeDirectedDual")
     end);
    adDual env i eps pri tgn
  | TySeq r ->
    _map2SeqExpr i
      (lam pri. lam tgn. adTypeDirectedDual env i eps pri tgn r.ty)
      pri tgn
  | TyRecord r ->
    let fs =
      map
        (lam t.
          let f = lam pri. lam tgn. adTypeDirectedDual env i eps pri tgn t.1 in
          (sidToString t.0, t.1, f))
        (mapBindings r.fields) in
    _map2RecordExpr i fs pri tgn
  | ty ->
    printErrorLn (type2str ty); error (_tyAssertErrMsg "adTypeDirectedDual")

  sem adTypeDirectedTangent
    : ADHookEnv -> Info -> Expr -> Expr -> Type -> Expr
  sem adTypeDirectedTangent env i eps e =
  | TyFloat _ ->
    (switch map _tyTm [eps, e]
     case [TyFloat _, TyFloat _] then ()
     case tys then
      iter printErrorLn (map type2str tys);
      error (_tyAssertErrMsg "adTypeDirectedTangent")
     end);
    adTangent env i eps e
  | TySeq r -> _mapSeqExpr i (lam e. adTypeDirectedTangent env i eps e r.ty) e
  | TyRecord r ->
    _mapRecordExprOverField i (adTypeDirectedTangent env i eps) r.fields e
  | ty ->
    printErrorLn (type2str ty); error (_tyAssertErrMsg "adTypeDirectedTangent")

  sem adAssertFloat : ADHookEnv -> Info -> Expr -> Expr
  sem adAssertFloat env i =| e ->
    let assertfloat =
      adGetVarExn env i (ityarrow_ i (ityfloat_ i) (ityfloat_ i))
        "assertFloat" in
    _app_ i assertfloat e

  sem _tyAssertErrMsg =| fn -> concat "Failed a type assertion in " fn

  sem _tyHasFloat =| ty -> _tyHasFloatH false ty
  sem _tyHasFloatH acc =
  | TyFloat _ -> true
  | ty -> sfold_Type_Type _tyHasFloatH acc ty

  ------------------------------------------------------------------------------
  -- AD transformation specific ty-preserving term builders.
  ------------------------------------------------------------------------------

  sem _tmBuildErrMsg =| caller ->
    concat "failed to build type preserving term, expected in " caller

  sem _var_ i ty =| n -> withInfo i (withType ty (nvar_ n))

  sem _lam_ i n ty =| e ->
    let ty = ityarrow_ i ty (tyTm e) in
    tmLam i ty n (TyUnknown { info = i }) e

  sem _app_ i f =| e ->
    let ty =
      match _tyTm f with TyArrow r then r.to
      else printErrorLn (type2str (_tyTm f)); error (_tmBuildErrMsg "_app_") in
    withInfo i (withType ty (app_ f e))

  sem _appf2_ i f e1 =| e2 -> _app_ i (_app_ i f e1) e2

  sem _appf3_ i f e1 e2 =| e3 -> _app_ i (_appf2_ i f e1 e2) e3

  sem _let_ i n e1 =| e2 ->
    withInfo i (withType (_tyTm e2) (bind_ (nulet_ n e1) e2))

  sem _unit_ =| i -> withInfo i (withType tyunit_ unit_)

  sem _recordproj_ i e =| key ->
    let fields =
      match _tyTm e with TyRecord r then r.fields else
        error (_tmBuildErrMsg "_recordproj_") in
    let ty = mapFindExn (stringToSid key) fields in
    withInfo i (withType ty (recordproj_ key e))

  sem _get_ i e =| j ->
    let ty = match _tyTm e with TySeq r then r.ty
             else error (_tmBuildErrMsg "_get_") in
    (match _tyTm j with ! TyInt _ then error (_tmBuildErrMsg "_get_") else ());
    let c =
      let ty = ityarrow_ i (_tyTm e) (ityarrow_ i (ityint_ i) ty) in
      withInfo i (const_ ty (CGet ())) in
    _appf2_ i c e j

  sem _map_ i f =| e ->
    let ty = match _tyTm f with TyArrow r then r.to
             else error (_tmBuildErrMsg "_map_") in
    let c =
      let ty = ityarrow_ i (_tyTm f) (ityarrow_ i (_tyTm e) (ityseq_ i ty)) in
      withInfo i (const_ ty (CMap ())) in
    _appf2_ i c f e

  sem _mapi_ i f =| e ->
    let ty = match _tyTm f with TyArrow { to = TyArrow r } then r.to
             else error (_tmBuildErrMsg "_mapi_") in
    let c =
      let ty = ityarrow_ i (_tyTm f) (ityarrow_ i (_tyTm e) (ityseq_ i ty)) in
      withInfo i (const_ ty (CMapi ())) in
    _appf2_ i c f e

  sem _mapSeqExpr i f =| e ->
    let ty = match _tyTm e with TySeq r then r.ty
             else error (_tmBuildErrMsg "_mapSeqExpr") in
    let _x = nameSym "x" in
    _map_ i (_lam_ i _x ty (f (_var_ i ty _x))) e

  sem _map2SeqExpr i f e1 =| e2 ->
    let ty =
      match _tyTm e1 with TySeq r then r.ty else
        error (_tmBuildErrMsg "_map2SeqExpr") in
    let _x = nameSym "x" in
    let _i = nameSym "i" in
    let _lam_ = _lam_ i in
    let _var_ = _var_ i in
    let ityint_ = ityint_ i in
    _mapi_ i
      (_lam_ _i ityint_
         (_lam_ _x ty
            (f (_var_ ty _x) (_get_ i e2 (_var_ ityint_ _i)))))
      e1

  sem _mapRecordExpr i fs =| e ->
    let ty = tyRecord i (map (lam t. (t.0, t.1)) fs) in
    tmRecord i ty
      (map (lam t. (t.0, t.2 (_recordproj_ i (withType ty e) t.0))) fs)

  sem _map2RecordExpr i fs e1 =| e2 ->
    tmRecord i (tyRecord i (map (lam t. (t.0, t.1)) fs))
      (map
         (lam t. (t.0, t.2 (_recordproj_ i e1 t.0) (_recordproj_ i e2 t.0)))
         fs)

  sem _mapRecordExprOverField i f fields =| e ->
    let fs = map (lam t. (sidToString t.0, t.1, (lam e. f e t.1)))
               (mapBindings fields) in
    _mapRecordExpr i fs e

  sem _mapFloatExprs i from to =
  | TyFloat _ -> Some from
  | TySeq r -> optionMap (_mapSeqExpr i) (_mapFloatExprs i from to r.ty)
  | ty & TyRecord r ->
    if not (_tyHasFloat ty) then None ()
    else
      let bs =
        (map
           (lam t.
             (sidToString t.0,
              t.1,
              optionGetOr (lam x. x) (_mapFloatExprs i from to t.1)))
           (mapBindings r.fields))
      in
      Some (_mapRecordExpr i bs)
  | TyArrow r ->
    let f = lam from. lam to.
      Some (
        lam e.
          let _x = nameSym "x" in
          let ty = _tyTm e in
          _lam_ i _x ty (to (_app_ i e (from (_var_ i ty _x)))))
    in
    switch (_mapFloatExprs i to from r.from, _mapFloatExprs i from to r.to)
    case (Some from, Some to) then f from to
    case (Some from, None _) then f from (lam x. x)
    case (None _, Some to) then f (lam x. x) to
    case (None _, None _) then None ()
    end
  | TyAll r -> _mapFloatExprs i from to r.ty
  | TyAlias r -> _mapFloatExprs i from to r.content
  | _ -> None ()

  sem _hasFloatExprsMap =| ty ->
    optionIsSome (_mapFloatExprs (NoInfo ()) (lam x. x) (lam x. x) ty)

  sem _mapFloatExprsExpr i from to ty =| e ->
    optionMapOr e (lam map. map e) (_mapFloatExprs i from to ty)
end

lang CorePPLFileTypeLoader = CPPLLoader + GeneratePprintLoader + MExprGeneratePprint + ODELoader + DTCTypeOf + ADLoader
  syn FileType =
  | FCorePPL {isModel : Bool}

  syn Hook =
  | CorePPLFileHook {options : CPPLFileOptions, method : InferMethod}

  sem _insertBackcompatInfer : CPPLFileOptions -> InferMethod -> Expr -> Loader -> Loader
  sem _insertBackcompatInfer options method modelBody = | loader ->
    let modelName = nameSym "_model" in
    let decl = DeclLet
      { ident = modelName
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , body = lam_ "" tyunit_ modelBody
      , info = infoTm modelBody
      } in
    let loader = _addDeclExn loader decl in

    match includeFileExn "." "stdlib::common.mc" loader with (commonEnv, loader) in

    let particlesName = nameSym "particles" in
    let decl = DeclLet
      { ident = particlesName
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , body = int_ options.defaultParticles
      , info = NoInfo ()
      } in
    let loader = _addSymbolizedDeclExn loader decl in
    let loader =
      let symEnv = _getSymEnv loader in
      let symEnv = symbolizeUpdateVarEnv symEnv
        (mapInsert (nameGetStr particlesName) particlesName symEnv.currentEnv.varEnv) in
      _setSymEnv symEnv loader in
    match includeFileTypeExn (FCorePPL {isModel = false}) "." "coreppl::coreppl-to-mexpr/top.mc" loader
      with (topEnv, loader) in

    let retTy = match unwrapType (mapFindExn modelName (_getTCEnv loader).varEnv)
      with TyArrow {to = retTy} then retTy
      else errorSingle [infoTm modelBody] "A model without an explicit 'infer' must be monomorphic" in
    -- NOTE(vipa, 2025-01-27): We special-case a string return value,
    -- which is returned as is, without escaping. Everything else is
    -- printed with proper mexpr syntax
    match
      let isStringTy = match unwrapType retTy with TySeq x
        then match unwrapType x.ty with TyChar _ then true else false
        else false in
      if isStringTy then (loader, ulam_ "x" (var_ "x")) else
      match pprintFunctionsFor [retTy] loader with (loader, [retTyPrint]) in
      (loader, retTyPrint)
    with (loader, retTyPrint) in

    let inferCode =
      let distName = nameSym "d" in
      let basicPrint = switch (method, options.printAcceptanceRate)
        case (Importance _ | BPF _ | APF _, _) then
          app_ (nvar_ (_getVarExn "printNormConst" topEnv)) (nvar_ distName)
        case (NaiveMCMC _ | TraceMCMC _ | LightweightMCMC _ | PIMH _, true) then
          app_ (nvar_ (_getVarExn "printAcceptRate" topEnv)) (nvar_ distName)
        case (NaiveMCMC _ | TraceMCMC _ | LightweightMCMC _ | PIMH _, false) then
          unit_
        case _ then
          error "Inference algorithm not supported in global mode"
        end in
      let printSamples = if options.printSamples
        then appf2_ (nvar_ (_getVarExn "printSamples" topEnv)) retTyPrint (nvar_ distName)
        else unit_ in
      TmDecl
      { decl = DeclLet
        { ident = distName
        , tyAnnot = tyunknown_
        , tyBody = tyunknown_
        , body = TmInfer
          { method = setRuns (nvar_ (_getVarExn "particles" topEnv)) method
          , model = nvar_ modelName
          , ty = tyunknown_
          , info = NoInfo ()
          }
        , info = NoInfo ()
        }
      , info = NoInfo ()
      , inexpr = semi_ basicPrint printSamples
      , ty = tyunknown_
      } in
    let decl = DeclLet
      { ident = nameSym ""
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , info = NoInfo ()
      , body = appf2_ (nvar_ (_getVarExn "repeat" commonEnv))
        (ulam_ "" inferCode)
        (nvar_ (_getVarExn "sweeps" topEnv))
      } in
    _addDeclExn loader decl

  sem _loadFile path = | (FCorePPL {isModel = isModel}, loader & Loader x) ->
    -- NOTE(vipa, 2024-12-12): Return if we've already included this
    -- file
    match mapLookup path x.includedFiles with Some symEnv then (symEnv, loader) else
    let args =
      { _defaultBootParserParseMCoreFileArg ()
      -- NOTE(vipa, 2024-12-03): It's important to not remove dead
      -- code, because that code might end up not-dead later, at which
      -- point it would end up included then, out of order and in
      -- various ways messing with assumptions made in the loader.
      with eliminateDeadCode = false
      -- NOTE(vipa, 2024-12-03): This largely lets us error later,
      -- which gives better error messages.
      , allowFree = true
      , keywords = pplKeywords
      , builtin = cpplBuiltin
      } in
    let ast = parseMCoreFile args path in
    let ast = use DPPLParser in makeKeywords ast in

    match optionGetOrElse (lam. error "missing CorePPLFileHook")
      (getHookOpt (lam h. match h with CorePPLFileHook x then Some (x.options, x.method) else None ()) loader)
    with (options, method) in

    -- NOTE(oerikss, 2025-03-14): If the user requested it, we type-check with
    -- the DPPL type-checker.
    (if options.dpplTypeCheck then
      typeOfExn (decorateTypesExn (decorateTerms (symbolize ast))); ()
     else ());

    recursive let f = lam decls. lam ast.
      match ast with TmDecl {decl = decl, inexpr = ast}
      then f (snoc decls decl) ast
      else (decls, ast) in
    -- NOTE(oerikss, 2025-03-14): We need to erase any type decorations specific
    -- to the new DPPL typechecker.
    match f [] (eraseDecorations ast) with (decls, expr) in

    let hasTerm : (Expr -> Bool) -> Bool = lam p.
      recursive let hasTerm = lam acc. lam e.
        if acc then acc else
          if p e then true
          else sfold_Expr_Expr hasTerm false e in
      let hasTermD = lam acc. lam d.
        if acc then acc else
          sfold_Decl_Expr hasTerm false d in
      or (foldl hasTermD false decls) (hasTerm false expr) in

    let hasInfer =
      hasTerm (lam e. match e with TmInfer _ then true else false) in
    let needsAddedInfer = and isModel (not hasInfer) in

    let hasSolve =
      hasTerm (lam e. match e with TmSolveODE _ then true else false) in

    let hasDiff = hasTerm (lam e. match e with TmDiff _ then true else false) in
    (if and hasDiff (not isModel) then
      error "found a `diff` outside model code which we cannot handle."
     else ());

    let loader =
      switch (hasDiff, hasSolve, needsAddedInfer)
      case (false, _, false) | (false, true, true) then
        -- NOTE(vipa, 2025-02-26): Simple case, no AD, and no need to add an infer
        let decls = if isModel
          then snoc decls (declWithInfo (infoTm expr) (nulet_ (nameSym "") expr))
          else decls in
        _addDeclsByFile loader decls
      case (false, false, true) then
        -- NOTE(vipa, 2025-02-26): No AD or solve, but we do need to add an infer
        match partition (lam d. match infoDecl d with Info {filename = f} then eqString f path else false) decls
          with (inFile, beforeFile) in
        let loader = _addDeclsByFile loader beforeFile in
        let modelBody = bindall_ inFile expr in
        _insertBackcompatInfer options method modelBody loader
      case _ then
        -- NOTE(vipa, 2025-02-26): When using AD we make a simplifying
        -- assumption: we make the model code exist "in its own world",
        -- i.e., we get duplication of dependencies between the model
        -- and the rest of what's in the loader. This means that we can
        -- lift everything in the model indiscriminately without
        -- affecting other code and without more complicated data-flow
        -- analysis.

        -- NOTE(vipa, 2025-02-26): This is the initial separate world,
        -- where we *might* load the ode-runtime, and where we *will* do
        -- adtransform
        let odeHook = getHookOpt
          (lam x. match x with h & ODEHook _ then Some h else None ())
          loader in
        let hooks = match odeHook with Some hook
          then [hook]
          else [] in
        match prepareADRuntime loader { insertFloatAssertions = not options.dpplTypeCheck }
          with (adHook, loader) in
        let hooks = snoc hooks adHook in
        let separateLoader = mkLoader
          (_getSymEnv loader)
          (_getTCEnv loader)
          hooks in

      let decls = snoc decls (declWithInfo (infoTm expr) (nulet_ (nameSym "") expr)) in
        let separateLoader = _addDeclsByFile separateLoader decls in

        -- NOTE(vipa, 2025-02-26): Now we're going to put the decls we
        -- got in the original loader, letting it run its various
        -- hooks. This means we, e.g., type-check twice. Afterwards we
        -- reset SymEnv and includedFiles to what they were before,
        -- which has the effect of making these decls quite isolated
        -- from other decls.
        let prevSymEnv = _getSymEnv loader in
        let prevIncluded = match loader with Loader {includedFiles = x} in x in
        -- NOTE(vipa, 2025-02-26): We use normal `addDecl` to ensure
        -- all hooks are run
        let loader = foldl _addDeclExn loader (getDecls separateLoader) in
        match loader with Loader x in
        let loader = Loader {x with symEnv = prevSymEnv, includedFiles = prevIncluded} in
        loader
      end in

    match loader with Loader x in
    match mapLookup path x.includedFiles with Some env
    then (env, loader)
    else (_symEnvEmpty, Loader {x with includedFiles = mapInsert path _symEnvEmpty x.includedFiles})
end
