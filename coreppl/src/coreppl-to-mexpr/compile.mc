include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type.mc"
include "mexpr/utils.mc"
include "mexpr/free-vars.mc"
include "sys.mc"
include "map.mc"

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
  | method -> error (join [
    nameGetStr (odeSolverMethodName method),
    " does not have an implementation in the ODE solver runtime"
  ])

  -- Maps ODE solver method to its method arguments.
  sem odeODESolverMethodToSolverArgs : ODESolverMethod -> [Expr]
  sem odeODESolverMethodToSolverArgs =
  | ODESolverDefault r | RK4 r | EF r -> [r.add, r.smul, r.stepSize]
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
  DualNumLift

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

-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile =
  MExprPPL + Resample + Externals + DPPLParser + DPPLExtract + LoadRuntime +
  StaticDelay + DPPLKeywordReplace + DPPLTransformDist + MExprSubstitute +
  MExprANFAll + CPPLBackcompat +
  ODETransform + DPPLTransformCancel + DPPLPruning +
  ElementaryFunctionsTransform + DPPLDelayedReplace + DPPLDelayedSampling
  + ADTransform

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

  sem mexprCpplCompile : Options -> Bool -> Expr -> Expr
  sem mexprCpplCompile options noInfer =
  | ast ->
    -- First replace externals that implements elementary functions with
    -- appropriate constants
    let ast = replaceExternalElementaryFunctions ast in

    -- Secondly translate all Default {} inference methods
    let ast = replaceDefaultInferMethod options ast in

    -- Load the inference runtimes used in the provided AST, and collect
    -- identifiers of common methods within the runtimes.
    let inferRuntimes = loadRuntimes options ast in

    -- If no infers are found, the entire AST is the model code, so we transform
    -- it as:
    --
    -- let d = infer <method> (lam. <model>) in
    -- let printRes = ... in
    -- printRes <pp> d
    --
    -- where <method> = inference method chosen according to options
    --       <model> = the entire AST
    --       <pp> = the pretty-print function used to print the result
    match
      if noInfer then programModelTransform options ast
      else (inferRuntimes, ast)
    with (inferRuntimes, ast) in

    -- Combine the required runtime ASTs to one AST and eliminate duplicate
    -- definitions due to files having common dependencies. The result is an
    -- updated map of runtime entries, a combined runtime AST and a
    -- symbolization environment.
    let inferRuntimes = combineInferRuntimes options inferRuntimes in

    mexprCompile options inferRuntimes ast


  sem mexprCompile : Options -> InferRuntimes -> Expr -> Expr
  sem mexprCompile options runtimes =
  | corepplAst ->
    -- Symbolize and type-check the CorePPL AST.
    let corepplAst = symbolize corepplAst in
    let corepplAst = typeCheck corepplAst in

    -- Transform solveode terms and add the ODE solver runtime code and add it
    -- to the program.
    let corepplAst =
      switch odeTransform options corepplAst
      case (Some odeRuntime, corepplAst) then
        eliminateDuplicateExternals (bind_ odeRuntime corepplAst)
      case (None _, corepplAst) then corepplAst
      end
    in

    -- Does the program contain differentiation?
    let hasDiff = adHasDiff corepplAst in

    -- Transform diff terms and lift to dual numbers if necessary.
    let corepplAst =
      if hasDiff then typeCheck (dualnumLiftExpr corepplAst)
      else corepplAst
    in

    -- Extract the infer expressions to separate ASTs, one per inference
    -- method. The result consists of the provided AST, updated such that
    -- each infer is replaced with a call to the 'run' function provided by
    -- the chosen runtime. It also consists of one AST per inference method
    -- used in the program.
    match extractInfer options runtimes.entries corepplAst with
      (corepplAst, models)
    in

    -- Compile the model ASTs.
    let modelAsts = compileModels options runtimes models in

    -- Transform distributions in the CorePPL AST to use MExpr code.
    let corepplAst = replaceDelayTypes (replaceDelayKeywords (replaceTyPruneInt (removePrunes ((replaceCancel corepplAst))))) in
    let corepplAst = transformDistributions corepplAst in

    -- Symbolize any free occurrences in the CorePPL AST and in any of the
    -- models using the symbolization environment of the runtime AST.
    let runtimeSymEnv = addTopNames symEnvEmpty runtimes.ast in
    let corepplAst = symbolizeExpr runtimeSymEnv corepplAst in

    -- Replace uses of DPPL keywords in the main AST, i.e. outside of models,
    -- with errors. This code is unreachable unless the inferred models are
    -- also used outside of infers, which is an error.
    -- TODO(larshum, 2022-10-07): Detect such errors statically.
    let corepplAst = replaceDpplKeywords corepplAst in

    -- Combine the CorePPL AST with the runtime AST, after extracting the
    -- models, and eliminate duplicated external definitions.
    let mainAst = bind_ runtimes.ast corepplAst in
    match eliminateDuplicateExternalsWithSummary mainAst
      with (replaced, mainAst)
    in

    -- Apply the replacements performed by the duplicate duplicated external
    -- elimination on the model ASTs.
    let modelAsts = replaceIdentifiers replaced modelAsts in

    -- Insert all models into the main AST at the first point where any of the
    -- models are used.
    let prog = insertModels modelAsts mainAst in

    -- TODO(dlunde,2023-05-22): Does not work, currently (the program does not
    -- type check at this stage). It does, however, type check after generating
    -- the code and compiling it with Miking.
    -- Type-check if options is set
    -- (if options.debugMExprCompile then
    --   -- Check that the combined program type checks
    --   typeCheck prog; ()
    -- else ());

    -- Provide a dual number runtime implementations
    let prog =
      if hasDiff then
        match adProvideRuntimeImplementation options prog with
          (adRuntime, prog)
        in
        eliminateDuplicateExternals (bind_ adRuntime prog)
      else prog
    in

    -- Finally we provide runtime implementations for elementary functions that
    -- are not MExpr intrinsics.
    match elementaryFunctionsTransform options prog with
      (elementaryRuntime, prog)
    in
    let prog = eliminateDuplicateExternals (bind_ elementaryRuntime prog) in

    -- Return complete program
    prog

  sem compileModels
    : Options -> InferRuntimes -> Map Name ModelRepr -> Map Name Expr
  sem compileModels options runtimes =
  | models ->
    mapMapWithKey
      (lam id. lam model.
        match model with {ast = ast, method = method, params = params} in
        match loadCompiler options method with (_, compile) in
        match mapLookup method runtimes.entries with Some entry then
          let ast = transformModelAst options ast in
          let ast = compileModel compile entry id {model with ast = ast} in
          removeModelDefinitions ast
        else
          match pprintInferMethod 0 pprintEnvEmpty method with (_, methodStr) in
          error (join ["Runtime definition missing for (", methodStr, ")"]))
      models

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

  sem _replaceHigherOrderConstant: Const -> Option Expr
  sem _replaceHigherOrderConstant =
  | CMap _ -> Some (var_ "map")
  | CMapi _ -> Some (var_ "mapi")
  | CIter _ -> Some (var_ "iter")
  | CIteri _ -> Some (var_ "iteri")
  | CFoldl _ -> Some (var_ "foldl")
  | CFoldr _ -> Some (var_ "foldr")
  | CCreate _ -> Some (var_ "create")
  | _ -> None ()

  sem _replaceHigherOrderConstantExpr: Expr -> Expr
  sem _replaceHigherOrderConstantExpr =
  | TmConst r ->
    match _replaceHigherOrderConstant r.val with Some t then
      withType r.ty (withInfo r.info t)
    else TmConst r
  | t -> t

  sem replaceHigherOrderConstants: Expr -> Expr
  sem replaceHigherOrderConstants =
  | t ->
    let t = mapPre_Expr_Expr _replaceHigherOrderConstantExpr t in
    let replacements = loadRuntimeFile false "runtime-const.mc" in
    let replacements = normalizeTerm replacements in
    let t = bind_ replacements t in
    let t = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } t
    in
    t

  sem compileModel
    : ((Expr,Expr) -> Expr) -> InferRuntimeEntry -> Name -> ModelRepr -> Expr
  sem compileModel compile entry modelId =
  | {ast = modelAst, params = modelParams} ->

    -- ANF
    let modelAst = normalizeTerm modelAst in

    -- ANF with higher-order intrinsics replaced with alternatives in
    -- seq-native.mc
    -- TODO(dlunde,2022-10-24): @Lars I'm not sure how I should combine this
    -- with your updates.
    let modelAstNoHigherOrderConstants = replaceHigherOrderConstants modelAst in

    -- Apply inference-specific transformation
    let ast = compile (modelAst, modelAstNoHigherOrderConstants) in

    -- Bind the model code in a let-expression, which we can insert in the main
    -- AST.
    let stateVarId = nameNoSym "state" in
    let ast =
      nulet_ modelId
        (nlams_ (snoc modelParams (stateVarId, entry.stateType)) ast) in

    -- Replace any occurrences of TyDist in the program with the runtime
    -- distribution type. This needs to be performed after the previous step as
    -- captured parameters may have type TyDist.
    let ast = replaceTyDist ast in

    -- Symbolize the AST using the symbolization environment of the runtime
    -- corresponding to the inference method used for this model. This ensures
    -- that we refer to the functions defined in that particular runtime.
    symbolizeExpr entry.topSymEnv ast

  sem replaceIdentifiers : Map Name Name -> Map Name Expr -> Map Name Expr
  sem replaceIdentifiers replaced =
  | modelAsts ->
    mapMapWithKey
      (lam. lam modelAst. substituteIdentifiers replaced modelAst)
      modelAsts

  -- We insert all models before the first binding where any of them are used.
  -- This is simple but correct, as we only need them to be placed after the
  -- runtime code.
  sem insertModels : Map Name Expr -> Expr -> Expr
  sem insertModels models =
  | TmLet t ->
    if modelUsedInBody models false t.body then
      bindall_ (snoc (mapValues models) (TmLet t))
    else TmLet {t with inexpr = insertModels models t.inexpr}
  | TmRecLets t ->
    let modelUsedInBinding = lam bind. modelUsedInBody models false bind.body in
    if any identity (map modelUsedInBinding t.bindings) then
      bindall_ (snoc (mapValues models) (TmRecLets t))
    else TmRecLets {t with inexpr = insertModels models t.inexpr}
  | TmType t -> TmType {t with inexpr = insertModels models t.inexpr}
  | TmConDef t -> TmConDef {t with inexpr = insertModels models t.inexpr}
  | TmUtest t ->
    if or (modelUsedInBody models false t.test)
          (modelUsedInBody models false t.expected) then
      bindall_ (snoc (mapValues models) (TmUtest t))
    else
      TmUtest {t with next = insertModels models t.next}
  | TmExt t -> TmExt {t with inexpr = insertModels models t.inexpr}
  | t -> bindall_ (snoc (mapValues models) t)

  sem modelUsedInBody : Map Name Expr -> Bool -> Expr -> Bool
  sem modelUsedInBody models acc =
  | TmVar t ->
    if acc then acc
    else if mapMem t.ident models then true
    else false
  | t -> sfold_Expr_Expr (modelUsedInBody models) acc t
end

let mexprCompile = use MExprCompile in mexprCompile

lang TestCompileLang =
  MExprCompile + CPPLBackcompat + MExprFindSym + DPPLParser
end

mexpr

use TestCompileLang in

let parse = parseMExprPPLString in

-- TODO(dlunde,2022-10-19): We should also add a `simpleInfer` test that uses
-- the new infer keyword.
let simple = parse "
let x = assume (Beta 10.0 5.0) in
let obs = true in
observe obs (Bernoulli x);
x
" in
let simple = symbolize simple in

let truefn = lam. lam. true in

let dummyOptions = default in

let compile = lam options. lam methodStr. lam ast.
  let options = {options with method = methodStr} in
  match programModelTransform options ast with (runtimes, ast) in
  let runtimeData = combineInferRuntimes options runtimes in
  mexprCompile options runtimeData ast
in

-- Simple tests that ensure compilation of a simple model throws no errors

-- Likelihood weighting
utest compile {dummyOptions with cps = "none"}
        "is-lw" simple with () using truefn in
utest compile {dummyOptions with cps = "partial"}
        "is-lw" simple with () using truefn in
utest compile {dummyOptions with cps = "full"}
        "is-lw" simple with () using truefn in

-- APF
utest compile dummyOptions "smc-apf" simple with () using truefn in

-- BPF
utest compile dummyOptions "smc-bpf" simple with () using truefn in

-- Naive MCMC
utest compile dummyOptions "mcmc-naive" simple with () using truefn in

-- Trace MCMC
utest compile dummyOptions "mcmc-trace" simple with () using truefn in

-- Lightweight MCMC
utest compile dummyOptions "mcmc-lightweight" simple with () using truefn in
utest compile {dummyOptions with align = true, cps = "none"}
        "mcmc-lightweight" simple with () using truefn in
utest compile {dummyOptions with align = true, cps = "partial"}
        "mcmc-lightweight" simple with () using truefn in
utest compile {dummyOptions with align = true, cps = "full"}
        "mcmc-lightweight" simple with () using truefn in

utest compile dummyOptions "pmcmc-pimh" simple with () using truefn in

()
