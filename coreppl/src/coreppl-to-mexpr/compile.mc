include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type.mc"
include "mexpr/utils.mc"
include "mexpr/free-vars.mc"
include "mexpr/phase-stats.mc"
include "mexpr/generate-pprint.mc"
include "sys.mc"
include "map.mc"
include "mlang/loader.mc"

include "../ad.mc"
include "../coreppl.mc"
include "../inference/smc.mc"
include "../parser.mc"
include "../dppl-arg.mc"
include "../static-delay.mc"
include "../src-location.mc"


include "extract.mc"
include "backcompat.mc"
include "dists.mc"
include "runtimes.mc"
include "inference-interface.mc"

include "pruning/compile.mc"
include "delayed-sampling/compile.mc"

lang DPPLKeywordReplace = DPPLParser
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

  sem replaceDpplKeywords : Expr -> Expr
  sem replaceDpplKeywords =
  | TmAssume t -> _makeError t.info "assume"
  | TmObserve t -> _makeError t.info "observe"
  | TmWeight t -> _makeError t.info "weight"
  | TmResample t -> _makeError t.info "resample"
  | t -> smap_Expr_Expr replaceDpplKeywords t
end

lang DPPLTransformDist = DPPLParser + TransformDist
  -- Used to transform away distributions in the main AST.
  sem transformDistributions : Expr -> Expr
  sem transformDistributions =
  | t ->
    let t = mapPre_Expr_Expr transformTmDist t in
    replaceTyDist t
end

lang ODETransform = DPPLParser + MExprSubstitute + MExprFindSym + LoadRuntime
  -- Make transformations related to solveode. This pass removes all solveode
  -- terms and returns a transformed term and an ODE related runtime. the
  -- tranformed program can be treated like a normal probabilistic program.
  sem odeTransform : Options -> Expr -> (Option Expr, Expr)
  sem odeTransform options =| tm ->
    -- translate all Default { ... } ODE solve methods
    let tm = replaceDefaultODESolverMethod options tm in

    -- Extracts solver methods present in term
    recursive let extractMethods = lam acc. lam tm.
      match tm with TmSolveODE r then setInsert r.method acc
      else sfold_Expr_Expr extractMethods acc tm
    in

    let methods = setToSeq (extractMethods (setEmpty cmpODESolverMethod) tm) in

    if null methods then
      -- There are no solveode terms in the term.
      (None (), tm)
    else
      -- load ODE solver runtime
      let runtime = symbolize (loadRuntimeFile true "runtime-ode-wrapper.mc") in

      -- collect the names of used ODE solvers runtime names
      let names =
        findNamesOfStringsExn (map odeODESolverMethodToRuntimeName methods)
          runtime
      in
      let namesMap = mapFromSeq cmpODESolverMethod (zip methods names) in

      -- replace solveode terms with applications of its runtime implementation.
      recursive let applyRuntimeSolvers = lam tm.
        match tm with TmSolveODE r then
          let tm =
            let solver = nvar_ (mapFindExn r.method namesMap) in
            foldl (lam f. lam a.  withInfo r.info (app_ f a)) solver
              (concat
                 (odeODESolverMethodToSolverArgs r.method)
                 [r.model, r.init, r.endTime])
          in smap_Expr_Expr applyRuntimeSolvers tm
        else smap_Expr_Expr applyRuntimeSolvers tm
      in

      (Some runtime, applyRuntimeSolvers tm)

  -- Maps ODE solver methods to the name bound to their implementation in
  -- the runtime.
  sem odeODESolverMethodToRuntimeName : ODESolverMethod -> String
  sem odeODESolverMethodToRuntimeName =
  | RK4 _ -> "odeSolverRK4Solve"
  | EF _ -> "odeSolverEFSolve"
  | EFA _ -> "odeSolverEFASolve"
  | method -> error (join [
    nameGetStr (odeSolverMethodName method),
    " does not have an implementation in the ODE solver runtime"
  ])

  -- Maps ODE solver method to its method arguments.
  sem odeODESolverMethodToSolverArgs : ODESolverMethod -> [Expr]
  sem odeODESolverMethodToSolverArgs =
  | ODESolverDefault r | RK4 r | EF r -> [r.add, r.smul, r.stepSize]
  | EFA r -> [r.add, r.smul, r.stepSize, r.n]
end

lang DPPLTransformCancel = DPPLParser
  sem replaceCancel =
  | (TmCancel t) ->
    let i = withInfo t.info in
    TmWeight { weight = negf_ (appf2_ (i (var_ "logObserve")) t.dist t.value),
               info = t.info,
               ty = t.ty}
  | t -> smap_Expr_Expr replaceCancel t
end

lang DPPLDelayedReplace = DPPLParser
   sem replaceDelayKeywords =
   | TmDelay t -> TmAssume { dist = t.dist, ty = t.ty, info = t.info }
   | TmDelayed t -> t.delay
   | t -> smap_Expr_Expr replaceDelayKeywords t

  sem replaceDelayTypes =
  | t ->
    let t = smap_Expr_Type toRuntimeDelayTyVar t in
    let t = smap_Expr_TypeLabel toRuntimeDelayTyVar t in
    let t = smap_Expr_Pat replaceDelayTyVarPat t in
    let t = smap_Expr_Expr replaceDelayTypes t in
    withType (toRuntimeDelayTyVar (tyTm t)) t

  sem toRuntimeDelayTyVar : Type -> Type
  sem toRuntimeDelayTyVar =
  | TyDelayInt t -> TyInt {info=t.info}
  | TyDelayFloat t -> TyFloat {info=t.info}
  | TyDelaySeqF t -> TySeq {info=t.info,ty=TyFloat {info=t.info}}
  | ty -> smap_Type_Type toRuntimeDelayTyVar ty

  sem replaceDelayTyVarPat : Pat -> Pat
  sem replaceDelayTyVarPat =
  | p ->
    let p = smap_Pat_Pat replaceDelayTyVarPat p in
    withTypePat (toRuntimeDelayTyVar (tyPat p)) p
 end

lang ADTransform =
  DPPLParser +
  LoadRuntime +
  DualNumRuntimeBase +
  DualNumLift +
  LiftedDist +
  TransformDistBase

  sem adHasDiff : Expr -> Bool
  sem adHasDiff =| tm -> adHasDiffExpr false tm

  sem adHasDiffExpr : Bool -> Expr -> Bool
  sem adHasDiffExpr hasDiff =
  | TmDiff _ -> true
  | tm -> sfold_Expr_Expr adHasDiffExpr hasDiff tm

  type ADTransformEnv = {
    lty : Type -> Type,
    s2n : String -> Name
  }

  syn DualNumRuntimeEnv =
  | Env ADTransformEnv

  sem adProvideRuntimeImplementation : Options -> Expr -> (Expr, Expr)
  sem adProvideRuntimeImplementation options =| tm ->
    let runtimeFile = "runtime-ad-wrapper.mc" in

    -- load AD runtime
    let runtime = symbolize (loadRuntimeFile true runtimeFile) in

    -- Define function that maps string identifiers to names
    let s2n = makeRuntimeNameMap runtime (_adTransformRuntimeIds ()) in

    -- Define function that constructs dual number types
    let tyDualName = s2n "Dual" in
    let lty = lam ty. TyApp {
      lhs = TyCon { ident = tyDualName, data = tyunknown_, info = infoTy ty },
      rhs = ty,
      info = infoTy ty
    } in

    let tm = dualnumTransformAPIExpr (Env { lty = lty, s2n = s2n }) tm in

    (runtime, tm)

  sem dualnumTransformAPIConst env tm =
  | CGenEpsilon _ -> withInfo (infoTm tm) (_var env "dualnumGenEpsilon")
  | CLtEpsilon _ -> withInfo (infoTm tm) (_var env "dualnumLtEpsilon")
  | CCreatePrimal _ -> withInfo (infoTm tm) (_var env "dualnumCreatePrimal")
  | CCreateDual _ -> withInfo (infoTm tm) (_var env "dualnumCreateDual")
  | CIsDualNum _ -> withInfo (infoTm tm) (_var env "dualnumIsDualNum")
  | CEpsilon _ -> withInfo (infoTm tm) (_var env "dualnumEpsilon")
  | CPrimal _ -> withInfo (infoTm tm) (_var env "dualnumPrimal")
  | CPrimalRec _ -> withInfo (infoTm tm) (_var env "dualnumPrimalRec")
  | CUnboxPrimalExn _ -> withInfo (infoTm tm) (_var env "dualnumUnboxPrimalExn")
  | CPertubation _ -> withInfo (infoTm tm) (_var env "dualnumPertubationExn")
  | CLifted (CFloat r) ->
    let i = withInfo (infoTm tm) in
    withInfo (infoTm tm) (nconapp_ (_name env "Primal") (i (float_ r.val)))
  | CLifted (CAddf _) -> withInfo (infoTm tm) (_var env "addn")
  | CLifted (CMulf _) -> withInfo (infoTm tm) (_var env "muln")
  | CLifted (CNegf _) -> withInfo (infoTm tm) (_var env "negn")
  | CLifted (CSubf _) -> withInfo (infoTm tm) (_var env "subn")
  | CLifted (CDivf _) -> withInfo (infoTm tm) (_var env "divn")
  | CLifted (CEqf _) -> withInfo (infoTm tm) (_var env "eqn")
  | CLifted (CNeqf _) -> withInfo (infoTm tm) (_var env "neqn")
  | CLifted (CLtf _) -> withInfo (infoTm tm) (_var env "ltn")
  | CLifted (CLeqf _) -> withInfo (infoTm tm) (_var env "leqn")
  | CLifted (CGtf _) -> withInfo (infoTm tm) (_var env "gtn")
  | CLifted (CGeqf _) -> withInfo (infoTm tm) (_var env "geqn")
  | CLifted (CSin _) -> withInfo (infoTm tm) (_var env "sinn")
  | CLifted (CCos _) -> withInfo (infoTm tm) (_var env "cosn")
  | CLifted (CSqrt _) -> withInfo (infoTm tm) (_var env "sqrtn")
  | CLifted (CExp _) -> withInfo (infoTm tm) (_var env "expn")
  | CLifted (CLog _) -> withInfo (infoTm tm) (_var env "logn")
  | CLifted (CPow _) -> withInfo (infoTm tm) (_var env "pown")
  | CLifted const -> withInfo (infoTm tm) (uconst_ const)
  | _ -> tm

  sem dualnumTransformTypeAPI env =
  | TyDualNum r -> match env with Env env in env.lty (TyFloat r)

  sem _name : DualNumRuntimeEnv -> String -> Name
  sem _name env =| str ->
    match env with Env env in env.s2n str

  sem _var : DualNumRuntimeEnv -> String -> Expr
  sem _var env =| str -> nvar_ (_name env str)

  sem _adTransformRuntimeIds =| _ -> [
    "Dual",
    "Primal",
    "dualnumCreatePrimal",
    "dualnumCreateDual",
    "dualnumIsDualNum",
    "dualnumLtEpsilon",
    "dualnumGenEpsilon",
    "dualnumPrimal",
    "dualnumPrimalRec",
    "dualnumUnboxPrimalExn",
    "dualnumPertubationExn",
    "addn",
    "muln",
    "eqn",
    "neqn",
    "ltn",
    "leqn",
    "gtn",
    "geqn",
    "negn",
    "subn",
    "divn",
    "sinn",
    "cosn",
    "sqrtn",
    "expn",
    "logn",
    "pown"
  ]
end

-- Provides runtime implementations for elementary functions that are not MExpr
-- intrisics.
lang ElementaryFunctionsTransform = ElementaryFunctions + LoadRuntime

  -- `elementaryFunctionsTransform options tm` returns a tuple where the first
  -- element contains runt-time implementations (typically in the form of
  -- external) for elementary functions that are not part of MExpr. The second
  -- element is the term `tm` where elementary function constants are replaced
  -- by variables referencing implementations in the runtime.
  sem elementaryFunctionsTransform : Options -> Expr -> (Expr, Expr)
  sem elementaryFunctionsTransform options =| tm ->
    let runtimeFile = "runtime-elementary-wrapper.mc" in

    -- load elementary functions runtime
    let runtime = symbolize (loadRuntimeFile true runtimeFile) in

    -- Define function that maps string identifiers to names
    let stringToName =
      makeRuntimeNameMap runtime (_elementaryFunctionsTransformRuntimeIds ())
    in

    (runtime, elementaryFunctionsTransformExpr stringToName tm)

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
  | CCreate _ -> Some "create"
  | _ -> None ()

  sem replaceHigherOrderConstants : {path : String, env : SymEnv} -> Expr -> Expr
end

lang ReplaceHigherOrderConstantsLoadDirectly
  = ReplaceHigherOrderConstants + LoadRuntime + Sym + MExprANFAll
  sem _replaceHigherOrderConstantExpr: Expr -> Expr
  sem _replaceHigherOrderConstantExpr =
  | TmConst r ->
    match _replaceHigherOrderConstant r.val with Some t then
      withType r.ty (withInfo r.info (var_ t))
    else TmConst r
  | t -> t

  sem replaceHigherOrderConstants env =
  | t ->
    let t = mapPre_Expr_Expr _replaceHigherOrderConstantExpr t in
    let replacements = loadRuntimeFile false "runtime-const.mc" in
    let replacements = normalizeTerm replacements in
    let t = bind_ replacements t in
    let t = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } t
    in
    t
end

lang ReplaceHigherOrderConstantsLoadedPreviously = ReplaceHigherOrderConstants + SymGetters
  sem replaceHigherOrderConstants env =
  | tm -> smap_Expr_Expr (replaceHigherOrderConstants env) tm
  | tm & TmConst x ->
    match _replaceHigherOrderConstant x.val with Some name then
      withType x.ty (withInfo x.info (nvar_ (_getVarExn name env)))
    else tm
end

lang CompileModels = ReplaceHigherOrderConstants + DPPLDelayedSampling + DPPLPruning + PhaseStats + LoadRuntime + DPPLDelayedReplace + DPPLTransformCancel + MExprANFAll + DPPLExtract + InferenceInterface
  sem compileModels
    : Options
    -> Map Name FinalOrderedLamLiftSolution
    -> {higherOrderSymEnv : {path : String, env : SymEnv}, distEnv : {path : String, env : SymEnv}}
    -> Map InferMethod {env : {path : String, env : SymEnv}, stateType : Type}
    -> Map Name ModelRepr
    -> Map Name Expr
  sem compileModels options lamliftSols envs runtimes =
  | models ->
    mapMapWithKey
      (lam id. lam model.
        match model with {extractAst = extractAst, method = method, params = params} in
        match loadCompiler options method with (_, compile) in
        match mapLookup method runtimes with Some entry then
          let extractAst = lam f. transformModelAst options (extractAst f) in
          let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in
          let ast = compileModel options compile lamliftSols envs entry id {model with extractAst = extractAst} in
          endPhaseStatsExpr log "compile-model-one" ast;
          let ast = removeModelDefinitions ast in
          endPhaseStatsExpr log "remove-model-definitions-one" ast;
          ast
        else
          match pprintInferMethod 0 pprintEnvEmpty method with (_, methodStr) in
          error (join ["Runtime definition missing for (", methodStr, ")"]))
      models

  sem transformModelAst : Options -> Expr -> Expr
  sem transformModelAst options =
  | modelAst ->
    -- Transform the model AST, if the flag is set
    let ast =
      if options.staticDelay then
        staticDelay modelAst
      else modelAst in
    -- Apply pruning to the model AST, if the flag is set
    let ast =
      if options.prune then
        prune ast
      else ast in
    let ast = replaceCancel ast in
    let ast =
      if options.dynamicDelay then
        delayedSampling ast
      else ast in
    let ast = replaceDelayTypes (replaceDelayKeywords ast) in
    -- Optionally print the model AST
    (if options.printModel then
      printLn (mexprPPLToString ast)
    else ());

    ast

  sem compileModel
    : Options
    -> (InferenceInterface -> Expr)
    -> Map Name FinalOrderedLamLiftSolution
    -> {higherOrderSymEnv : {path : String, env : SymEnv}, distEnv : {path : String, env : SymEnv}}
    -> {env : {path : String, env : SymEnv}, stateType : Type}
    -> Name
    -> ModelRepr
    -> Expr
  sem compileModel options compile lamliftSols envs entry modelId =
  | {extractAst = extractAst, params = modelParams} ->
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
      { extractNormal = lam. extractAst (lam x. x)
      , extractNoHigherOrderConsts = lam. extractAst (replaceHigherOrderConstants envs.higherOrderSymEnv)
      , options = options
      , runtime = {env = entry.env, lamliftSols = lamliftSols}
      , dists = {env = envs.distEnv, lamliftSols = lamliftSols}
      , stateName = stateVarId
      } in
    let ast = compile interface in
    endPhaseStatsExpr log "compile-inference-one" ast;

    -- Bind the model code in a let-expression, which we can insert in the main
    -- AST.
    let ast =
      nulet_ modelId
        (nlams_ (snoc modelParams (stateVarId, entry.stateType)) ast) in
    endPhaseStatsExpr log "insert-model-params-one" ast;

    -- Replace any occurrences of TyDist in the program with the runtime
    -- distribution type. This needs to be performed after the previous step as
    -- captured parameters may have type TyDist.
    let ast = replaceTyDist {env = envs.distEnv, lamliftSols = lamliftSols} ast in
    endPhaseStatsExpr log "replace-ty-dist-one" ast;

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
  | TmType t -> removeModelDefinitions t.inexpr
  | TmConDef t -> removeModelDefinitions t.inexpr
  | TmExt t -> removeModelDefinitions t.inexpr
  | t -> smap_Expr_Expr removeModelDefinitions t
end

lang InsertModels = VarAst + ExprAsDecl
  -- We insert each model right before its first use.  This is simple
  -- but correct, as we only need them to be placed after the
  -- corresponding runtime code.
  sem insertModels : Map Name Expr -> Expr -> Expr
  sem insertModels models = | tm ->
    match exprAsDecl tm with Some (decl, inexpr) then
      let usedModels = sfold_Decl_Expr (modelsUsedInBody models) (mapEmpty nameCmp) decl in
      let usedModels = mapIntersectWith (lam a. lam. a) models usedModels in
      let laterModels = mapDifference models usedModels in
      let inexpr = insertModels laterModels inexpr in
      bindall_ (snoc (mapValues usedModels) (declAsExpr inexpr decl))
    else
      bindall_ (snoc (mapValues models) tm)

  sem modelsUsedInBody : Map Name Expr -> Set Name -> Expr -> Set Name
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
  + LoadRuntime + ReplaceHigherOrderConstantsLoadedPreviously + CompileModels + InsertModels
  + DPPLPruningTransform + DPPLTransformCancel
  + ElementaryFunctionsTransform
  + DPPLKeywordReplace + DPPLDelayedReplace + DPPLParser
  syn Hook =
  | CPPLHook
    { options : Options
    , runtimes : Ref (Map InferMethod {env : {path : String, env : SymEnv}, stateType : Type})
    }

  sem mkCPPLLoader : [Hook] -> Options ->
    { runtimes : Ref (Map InferMethod {env : {path : String, env : SymEnv}, stateType : Type})
    , envs :
      { higherOrderSymEnv : {path : String, env : SymEnv}
      , distEnv : {path : String, env : SymEnv}
      , externalMathEnv : {path : String, env : SymEnv}
      }
    , loader : Loader
    }
  sem mkCPPLLoader hooks = | options ->
    let runtimes = ref (mapEmpty cmpInferMethod) in
    let cppl = CPPLHook
      { options = options
      , runtimes = runtimes
      } in
    let loader = mkLoader symEnvDefault typcheckEnvDefault (cons cppl hooks) in

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
        [ ("resample", str_ options.resample)
        , ("cps", str_ options.cps)
        , ("printSamples", bool_ options.printSamples)
        , ("earlyStop", bool_ options.earlyStop)
        , ("mcmcLightweightGlobalProb", float_ options.mcmcLightweightGlobalProb)
        , ("mcmcLightweightReuseLocal", bool_ options.mcmcLightweightReuseLocal)
        , ("printAcceptanceRate", bool_ options.printAcceptanceRate)
        , ("subsample", bool_ options.subsample)
        , ("subsampleSize", int_ options.subsampleSize)
        -- NOTE(dlunde,2022-11-04): Emulating option type
        , ("seedIsSome", match options.seed with Some seed then bool_ true else bool_ false)
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

    { runtimes = runtimes
    , loader = loader
    , envs =
      { higherOrderSymEnv = symEnv
      , distEnv = distEnv
      , externalMathEnv = externalMathEnv
      }
    }

  sem _preSymbolize loader decl = | CPPLHook x ->
    let decl = smap_Decl_Expr (replaceDefaultInferMethod x.options) decl in
    let requiredRuntimes =
      recursive let findRuntimes = lam acc. lam tm.
        let acc = match tm with TmInfer t
          then setInsert t.method acc
          else acc in
        sfold_Expr_Expr findRuntimes acc tm in
      sfold_Decl_Expr findRuntimes (setEmpty cmpInferMethod) decl in
    let f = lam loader. lam inferMethod.
      if mapMem inferMethod (deref x.runtimes) then loader else
      match loadCompiler x.options inferMethod with (runtime, _) in
      match includeFileExn "." (join ["coreppl::coreppl-to-mexpr/", runtime]) loader with (symEnv, loader) in
      let loader = (includeFileExn "." "coreppl::coreppl-to-mexpr/pruning/runtime.mc" loader).1 in
      let loader = (includeFileExn "." "coreppl::coreppl-to-mexpr/delayed-sampling/runtime.mc" loader).1 in
      let entry =
        let stateName = _getTyConExn "State" symEnv in
        let tcEnv = _getTCEnv loader in
        match mapFindExn stateName tcEnv.tyConEnv with (_, params, _) in
        let stateType = tyapps_ (ntycon_ stateName) (map (lam. tyunknown_) params) in
        let stateType = substituteUnknown (NoInfo ()) tcEnv (Poly ()) stateType in
        let stateType = resolveType (NoInfo ()) tcEnv false stateType in
        { env = symEnv
        , stateType = stateType
        } in
      modref x.runtimes (mapInsert inferMethod entry (deref x.runtimes));
      loader in
    (setFold f loader requiredRuntimes, decl)

  sem extractAsMExprExn
    : Options
    -> { higherOrderSymEnv : {path : String, env : SymEnv}
       , distEnv : {path : String, env : SymEnv}
       , externalMathEnv : {path : String, env : SymEnv}
       }
    -> Map InferMethod {env : {path : String, env : SymEnv}, stateType : Type}
    -> Loader
    -> Expr
  sem extractAsMExprExn options envs runtimes = | loader ->
    let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in
    let ast = buildFullAst loader in
    endPhaseStatsExpr log "build-full-ast" ast;
    let ast = removeMetaVarExpr ast in
    endPhaseStatsExpr log "remove-meta-var" ast;
    let runtimeRunNames = mapMap (lam entry. _getVarExn "run" entry.env) runtimes in
    let ast = elementaryFunctionsTransformExpr (lam str. _getVarExn str envs.externalMathEnv) ast in
    endPhaseStatsExpr log "elementary-functions-transform" ast;
    match extractInfer options runtimeRunNames ast with (ast, lamliftSols, models) in
    endPhaseStatsExpr log "extract-infer" ast;
    let models = compileModels options lamliftSols {higherOrderSymEnv = envs.higherOrderSymEnv, distEnv = envs.distEnv} runtimes models in
    let ast = mapPre_Expr_Expr (transformTmDist {env = envs.distEnv, lamliftSols = lamliftSols}) ast in
    let ast = replaceCancel ast in
    let ast = removePrunes ast in
    let ast = replaceTyPruneInt ast in
    let ast = replaceDelayKeywords ast in
    let ast = replaceDelayTypes ast in
    let ast = replaceDpplKeywords ast in
    endPhaseStatsExpr log "replace-all-the-things" ast;
    let ast = insertModels models ast in
    endPhaseStatsExpr log "insert-models" ast;
    ast
end

lang CorePPLFileTypeLoader = CPPLLoader + GeneratePprintLoader + MExprGeneratePprint
  syn FileType =
  | FCorePPL {isModel : Bool}

  sem _insertBackcompatInfer : Expr -> Loader -> Loader
  sem _insertBackcompatInfer modelBody = | loader ->
    let options = (withHookState (lam l. lam h. match h with CPPLHook x then Some (l, x.options) else None ()) loader).1 in

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
      , body = int_ options.particles
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
      let basicPrint = switch (options.method, options.printAcceptanceRate)
        case ("is-lw" | "smc-bpf" | "smc-apf", _) then
          app_ (nvar_ (_getVarExn "printNormConst" topEnv)) (nvar_ distName)
        case ("mcmc-naive" | "mcmc-trace" | "mcmc-lightweight" | "pmcmc-pimh", true) then
          app_ (nvar_ (_getVarExn "printAcceptRate" topEnv)) (nvar_ distName)
        case ("mcmc-naive" | "mcmc-trace" | "mcmc-lightweight" | "pmcmc-pimh", false) then
          unit_
        case _ then
          error "Inference algorithm not supported in global mode"
        end in
      let printSamples = if options.printSamples
        then appf2_ (nvar_ (_getVarExn "printSamples" topEnv)) retTyPrint (nvar_ distName)
        else unit_ in
      TmLet
      { ident = distName
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , body = TmInfer
        { method = setRuns (nvar_ (_getVarExn "particles" topEnv)) (inferMethodFromOptions options options.method)
        , model = nvar_ modelName
        , ty = tyunknown_
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
    recursive let f = lam decls. lam ast.
      match exprAsDecl ast with Some (decl, ast)
      then f (snoc decls decl) ast
      else (decls, ast) in
    match f [] ast with (decls, expr) in
    let hasInfer =
      recursive let hasInfer = lam acc. lam e.
        if acc then acc else
        match e with TmInfer _ then true else
        sfold_Expr_Expr hasInfer false e in
      let hasInferD = lam acc. lam d.
        if acc then acc else
        sfold_Decl_Expr hasInfer false d in
      or (foldl hasInferD false decls) (hasInfer false expr) in

    let loader =
      if or (not isModel) hasInfer then
        _addDeclsByFile loader decls
      else
        -- NOTE(vipa, 2025-01-22): No infer, need to transform the
        -- program to insert one
        match partition (lam d. match infoDecl d with Info {filename = f} then eqString f path else false) decls
          with (inFile, beforeFile) in
        let loader = _addDeclsByFile loader beforeFile in
        let modelBody = foldr (lam d. lam e. declAsExpr e d) expr inFile in
        _insertBackcompatInfer modelBody loader
    in

    match loader with Loader x in
    match mapLookup path x.includedFiles with Some env
    then (env, loader)
    else (_symEnvEmpty, Loader {x with includedFiles = mapInsert path _symEnvEmpty x.includedFiles})
end

-- TODO(vipa, 2024-12-04): does it actually work to do odetransform
-- before symbolize and typecheck? it's done after in the old pipeline
-- lang ODELoader = MCoreLoader + MExprSubstitute + ODETransform
--   syn Hook =
--   | ODEHook
--     { options : Options
--     }
--   sem _preSymbolize loader decl = | ODEHook x ->
--     let decl = smap_Decl_Expr (replaceDefaultODESolverMethod x.options) decl in
--     match smapAccumL_Decl_Expr _odeExtractSolve (mapEmpty cmpODESolverMethod) decl
--       with (methods, decl) in
--     -- Check for ode terms, exit early otherwise
--     if mapIsEmpty methods then (loader, decl) else
--     -- Load runtime and substitute proper references to functions in
--     -- the runtime
--     match includeFileExn "." "coreppl::coreppl-to-mexpr/runtime-ode-wrapper.mc" loader with (symEnv, loader) in
--     let substitutions = mapFoldWithKey
--       (lam acc. lam to. lam from.
--         mapInsert from (mapFindExn (odeODESolverMethodToRuntimeName to) symEnv.currentEnv.varEnv) acc)
--       (mapEmpty nameCmp)
--       methods in
--     (loader, smap_Decl_Expr (substituteIdentifiers substitutions) decl)
-- end

-- lang ADLoader = MCoreLoader + ADTransform
--   syn Hook =
--   | ADHook
--     { env : DualNumRuntimeEnv
--     , liftableDecls : Ref {var : Map Name Decl, tyCon : Map Name Decl}
--     , liftedDecls : Ref {var : Map Name Name}
--     }
--   sem prepareADRuntime : Loader -> Loader
--   sem prapareADRuntime = | loader ->
--     match includeFileExn "." "coreppl::coreppl-to-mexpr/runtime-ad-wrapper.mc" loader
--       with (symEnv, loader) in

--     -- NOTE(vipa, 2024-12-04): Originally this function (s2n) was
--     -- limited to looking at the names in _adTransformRuntimeIds (),
--     -- and looked at both names and types at once, hence the chained
--     -- mapFindOrElse
--     let s2n = lam str. mapFindOrElse
--       (lam.
--         mapFindOrElse
--           (lam. error (join ["\"", str, "\" is not present in this runtime."]))
--           str
--           symEnv.currentEnv.tyConEnv)
--       str
--       symEnv.currentEnv.varEnv in

--     let tyDualName = s2n "Dual" in
--     let lty = lam ty. TyApp {
--       lhs = TyCon { ident = tyDualName, data = tyunknown_, info = infoTy ty },
--       rhs = ty,
--       info = infoTy ty
--     } in

--     let env = Env {lty = lty, s2n = s2n} in
--     addHook loader (ADHook env)
-- end

-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile =
  MExprPPL + Resample + Externals + DPPLParser + DPPLExtract + LoadRuntime +
  StaticDelay + DPPLKeywordReplace + DPPLTransformDist + MExprSubstitute +
  MExprANFAll + CPPLBackcompat +
  ODETransform + DPPLTransformCancel + DPPLPruning +
  ElementaryFunctionsTransform + DPPLDelayedReplace + DPPLDelayedSampling
  + ReplaceHigherOrderConstantsLoadDirectly
  + CompileModels
  + InsertModels
  + MExprAsDecl
  + ADTransform
  + PhaseStats

  -- sem mexprCpplCompile : Options -> Bool -> Expr -> Expr
  -- sem mexprCpplCompile options noInfer =
  -- | ast ->
  --   let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in
  --   -- First replace externals that implements elementary functions with
  --   -- appropriate constants
  --   let ast = replaceExternalElementaryFunctions ast in
  --   endPhaseStatsExpr log "replace-elementary" ast;

  --   -- Secondly translate all Default {} inference methods
  --   let ast = replaceDefaultInferMethod options ast in
  --   endPhaseStatsExpr log "replace-default-infer" ast;

  --   -- Load the inference runtimes used in the provided AST, and collect
  --   -- identifiers of common methods within the runtimes.
  --   let inferRuntimes = loadRuntimes options ast in

  --   -- If no infers are found, the entire AST is the model code, so we transform
  --   -- it as:
  --   --
  --   -- let d = infer <method> (lam. <model>) in
  --   -- let printRes = ... in
  --   -- printRes <pp> d
  --   --
  --   -- where <method> = inference method chosen according to options
  --   --       <model> = the entire AST
  --   --       <pp> = the pretty-print function used to print the result
  --   match
  --     if noInfer then programModelTransform options ast
  --     else (inferRuntimes, ast)
  --   with (inferRuntimes, ast) in
  --   endPhaseStatsExpr log "load-runtimes" ast;

  --   -- Combine the required runtime ASTs to one AST and eliminate duplicate
  --   -- definitions due to files having common dependencies. The result is an
  --   -- updated map of runtime entries, a combined runtime AST and a
  --   -- symbolization environment.
  --   let inferRuntimes = combineInferRuntimes options inferRuntimes in
  --   endPhaseStatsExpr log "combine-runtimes" ast;

  --   mexprCompile options inferRuntimes ast


  -- sem mexprCompile : Options -> InferRuntimes -> Expr -> Expr
  -- sem mexprCompile options runtimes =
  -- | corepplAst ->
  --   let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in
  --   -- Symbolize and type-check the CorePPL AST.
  --   let corepplAst = symbolize corepplAst in
  --   endPhaseStatsExpr log "symbolize" corepplAst;
  --   let corepplAst = typeCheck corepplAst in
  --   endPhaseStatsExpr log "type-check" corepplAst;

  --   -- Transform solveode terms and add the ODE solver runtime code and add it
  --   -- to the program.
  --   let corepplAst =
  --     switch odeTransform options corepplAst
  --     case (Some odeRuntime, corepplAst) then
  --       eliminateDuplicateExternals (bind_ odeRuntime corepplAst)
  --     case (None _, corepplAst) then corepplAst
  --     end
  --   in
  --   endPhaseStatsExpr log "ode-transform" corepplAst;

  --   -- Does the program contain differentiation?
  --   let hasDiff = adHasDiff corepplAst in

  --   -- Transform diff terms and lift to dual numbers if necessary.
  --   let corepplAst =
  --     if hasDiff then typeCheck (dualnumLiftExpr corepplAst)
  --     else corepplAst
  --   in
  --   endPhaseStatsExpr log "lift-dual-nums" corepplAst;

  --   -- Extract the infer expressions to separate ASTs, one per inference
  --   -- method. The result consists of the provided AST, updated such that
  --   -- each infer is replaced with a call to the 'run' function provided by
  --   -- the chosen runtime. It also consists of one AST per inference method
  --   -- used in the program.
  --   let runtimeRunNames = mapMap (lam entry. entry.runId) runtimes.entries in
  --   match extractInfer options runtimeRunNames corepplAst with
  --     (corepplAst, models)
  --   in
  --   endPhaseStatsExpr log "extract-infer" corepplAst;

  --   -- Compile the model ASTs.
  --   let runtimeEntries = mapMap (lam entry. {symEnv = entry.topSymEnv, stateType = entry.stateType}) runtimes.entries in
  --   let modelAsts = compileModels options runtimes models in
  --   endPhaseStatsExpr log "compile-models" corepplAst;

  --   -- Transform distributions in the CorePPL AST to use MExpr code.
  --   let corepplAst = replaceDelayTypes (replaceDelayKeywords (replaceTyPruneInt (removePrunes ((replaceCancel corepplAst))))) in
  --   endPhaseStatsExpr log "replace-delay-types" corepplAst;
  --   let corepplAst = transformDistributions corepplAst in
  --   endPhaseStatsExpr log "transform-distributions" corepplAst;

  --   -- Symbolize any free occurrences in the CorePPL AST and in any of the
  --   -- models using the symbolization environment of the runtime AST.
  --   let runtimeSymEnv = addTopNames symEnvEmpty runtimes.ast in
  --   let corepplAst = symbolizeExpr runtimeSymEnv corepplAst in
  --   endPhaseStatsExpr log "add-top-names" corepplAst;

  --   -- Replace uses of DPPL keywords in the main AST, i.e. outside of models,
  --   -- with errors. This code is unreachable unless the inferred models are
  --   -- also used outside of infers, which is an error.
  --   -- TODO(larshum, 2022-10-07): Detect such errors statically.
  --   let corepplAst = replaceDpplKeywords corepplAst in
  --   endPhaseStatsExpr log "replace-dppl-keywords" corepplAst;

  --   -- Combine the CorePPL AST with the runtime AST, after extracting the
  --   -- models, and eliminate duplicated external definitions.
  --   let mainAst = bind_ runtimes.ast corepplAst in
  --   match eliminateDuplicateExternalsWithSummary mainAst
  --     with (replaced, mainAst)
  --   in
  --   endPhaseStatsExpr log "eliminate-duplicate-externals" mainAst;

  --   -- Apply the replacements performed by the duplicate duplicated external
  --   -- elimination on the model ASTs.
  --   let modelAsts = replaceIdentifiers replaced modelAsts in

  --   -- Insert all models into the main AST at the first point where any of the
  --   -- models are used.
  --   let prog = insertModels modelAsts mainAst in
  --   endPhaseStatsExpr log "insert-models" prog;

  --   -- TODO(dlunde,2023-05-22): Does not work, currently (the program does not
  --   -- type check at this stage). It does, however, type check after generating
  --   -- the code and compiling it with Miking.
  --   -- Type-check if options is set
  --   -- (if options.debugMExprCompile then
  --   --   -- Check that the combined program type checks
  --   --   typeCheck prog; ()
  --   -- else ());

  --   -- Provide a dual number runtime implementations
  --   let prog =
  --     if hasDiff then
  --       match adProvideRuntimeImplementation options prog with
  --         (adRuntime, prog)
  --       in
  --       eliminateDuplicateExternals (bind_ adRuntime prog)
  --     else prog
  --   in
  --   endPhaseStatsExpr log "provide-dual-number-impl" prog;

  --   -- Finally we provide runtime implementations for elementary functions that
  --   -- are not MExpr intrinsics.
  --   match elementaryFunctionsTransform options prog with
  --     (elementaryRuntime, prog)
  --   in
  --   endPhaseStatsExpr log "transform-elementary" prog;
  --   let prog = eliminateDuplicateExternals (bind_ elementaryRuntime prog) in
  --   endPhaseStatsExpr log "eliminate-duplicate-externals-2" prog;

  --   -- Return complete program
  --   prog


  sem replaceIdentifiers : Map Name Name -> Map Name Expr -> Map Name Expr
  sem replaceIdentifiers replaced =
  | modelAsts ->
    mapMapWithKey
      (lam. lam modelAst. substituteIdentifiers replaced modelAst)
      modelAsts

end

-- let mexprCompile = use MExprCompile in mexprCompile

-- lang TestCompileLang =
--   MExprCompile + CPPLBackcompat + MExprFindSym + DPPLParser
-- end

-- mexpr

-- use TestCompileLang in

-- let parse = parseMExprPPLString in

-- -- TODO(dlunde,2022-10-19): We should also add a `simpleInfer` test that uses
-- -- the new infer keyword.
-- let simple = parse "
-- let x = assume (Beta 10.0 5.0) in
-- let obs = true in
-- observe obs (Bernoulli x);
-- x
-- " in
-- let simple = symbolize simple in

-- let truefn = lam. lam. true in

-- let dummyOptions = default in

-- let compile = lam options. lam methodStr. lam ast.
--   let options = {options with method = methodStr} in
--   match programModelTransform options ast with (runtimes, ast) in
--   let runtimeData = combineInferRuntimes options runtimes in
--   mexprCompile options runtimeData ast
-- in

-- -- Simple tests that ensure compilation of a simple model throws no errors

-- -- Likelihood weighting
-- utest compile {dummyOptions with cps = "none"}
--         "is-lw" simple with () using truefn in
-- utest compile {dummyOptions with cps = "partial"}
--         "is-lw" simple with () using truefn in
-- utest compile {dummyOptions with cps = "full"}
--         "is-lw" simple with () using truefn in

-- -- APF
-- utest compile dummyOptions "smc-apf" simple with () using truefn in

-- -- BPF
-- utest compile dummyOptions "smc-bpf" simple with () using truefn in

-- -- Naive MCMC
-- utest compile dummyOptions "mcmc-naive" simple with () using truefn in

-- -- Trace MCMC
-- utest compile dummyOptions "mcmc-trace" simple with () using truefn in

-- -- Lightweight MCMC
-- utest compile dummyOptions "mcmc-lightweight" simple with () using truefn in
-- utest compile {dummyOptions with align = true, cps = "none"}
--         "mcmc-lightweight" simple with () using truefn in
-- utest compile {dummyOptions with align = true, cps = "partial"}
--         "mcmc-lightweight" simple with () using truefn in
-- utest compile {dummyOptions with align = true, cps = "full"}
--         "mcmc-lightweight" simple with () using truefn in

-- utest compile dummyOptions "pmcmc-pimh" simple with () using truefn in

-- ()
