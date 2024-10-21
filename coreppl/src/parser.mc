include "digraph.mc"
include "mexpr/boot-parser.mc"
include "mexpr/keyword-maker.mc"
include "mexpr/builtin.mc"

include "coreppl.mc"
include "inference/smc.mc"

-- Include the inference method definition definition files.
include "inference/smc-apf.mc"
include "inference/smc-bpf.mc"
include "inference/is-lw.mc"
include "inference/mcmc-lightweight.mc"
include "inference/mcmc-naive.mc"
include "inference/mcmc-trace.mc"
include "inference/pmcmc-pimh.mc"
include "ode-solver-method.mc"

lang DPPLParser =
  BootParser + MExprPrettyPrint + MExprPPL + Resample +
  KeywordMaker +

  ImportanceSamplingMethod + BPFMethod + APFMethod +
  LightweightMCMCMethod  + NaiveMCMCMethod + TraceMCMCMethod +
  PIMHMethod +

  ODESolverMethod

  sem _interpretMethod : Expr -> (Info, String, Map SID Expr)
  sem _interpretMethod =
  | TmConApp {ident = ident, body = TmRecord r, info = info} ->
    (info, nameGetStr ident, r.bindings)
  | t -> errorSingle [infoTm t] "Expected a constructor application"

  -- Interprets the argument to infer which encodes the inference method and
  -- its configuration parameters.
  sem interpretInferMethod : Expr -> InferMethod
  sem interpretInferMethod =| tm ->
    match _interpretMethod tm with (info, ident, bindings) in
    inferMethodFromCon info bindings ident

  -- Interprets the argument to solveode which encodes the solver method and
  -- its configuration parameters.
  sem interpretODESolverMethod : Expr -> ODESolverMethod
  sem interpretODESolverMethod =| tm ->
    match _interpretMethod tm with (info, ident, bindings) in
    odeSolverMethodFromCon info bindings ident

  sem replaceDefaultInferMethod : Options -> Expr -> Expr
  sem replaceDefaultInferMethod options =
  | expr ->
    let mf = lam expr.
      match expr with TmInfer ({ method = Default d } & t) then
        TmInfer { t with method = setRuns d.runs
                          (inferMethodFromOptions options options.method) }
      else expr
    in
    mapPre_Expr_Expr mf expr

  sem replaceDefaultODESolverMethod : Options -> Expr -> Expr
  sem replaceDefaultODESolverMethod options =
  | expr ->
    let mf = lam expr.
      match expr with TmSolveODE ({ method = ODESolverDefault d } & r) then
        TmSolveODE {
          r with
          method = RK4 d,
          model = replaceDefaultODESolverMethod options r.model,
          init = replaceDefaultODESolverMethod options r.init,
          endTime = replaceDefaultODESolverMethod options r.endTime
        }
      else expr
    in
    mapPre_Expr_Expr mf expr

  -- Replaces elementary external functions with their corresponding constant.
  sem replaceExternalElementaryFunctions : Expr -> Expr
  sem replaceExternalElementaryFunctions =| tm ->
    replaceExternalElementaryFunctionsExpr (mapEmpty nameCmp) tm

  sem replaceExternalElementaryFunctionsExpr : Map Name Const -> Expr -> Expr
  sem replaceExternalElementaryFunctionsExpr env =
  | TmExt r ->
    optionMapOrElse
      (lam. TmExt {
        r with inexpr = replaceExternalElementaryFunctionsExpr env r.inexpr
      })
      (lam const.
        replaceExternalElementaryFunctionsExpr
          (mapInsert r.ident const env)
          r.inexpr)
      (externalIdentToConst (nameGetStr r.ident))
  | tm & TmVar r -> optionMapOr tm uconst_ (mapLookup r.ident env)
  | tm -> smap_Expr_Expr (replaceExternalElementaryFunctionsExpr env) tm

  sem externalIdentToConst : String -> Option Const
  sem externalIdentToConst =
  | "externalSin" -> Some (CSin ())
  | "externalCos" -> Some (CCos ())
  | "externalSqrt" -> Some (CSqrt ())
  | "externalExp" -> Some (CExp ())
  | "externalLog" -> Some (CLog ())
  | "externalPow" -> Some (CPow ())
  | _ -> None ()

  -- Keyword maker
  sem isKeyword =
  | TmAssume _ -> true
  | TmObserve _ -> true
  | TmWeight _ -> true
  | TmInfer _ -> true
  | TmDist _ -> true
  | TmSolveODE _ -> true
  | TmPrune _ -> true
  | TmPruned _ -> true
  | TmCancel _ -> true
  | TmDiff _ -> true
  | TmDelay _ -> true
  | TmDelayed _ -> true

  sem matchKeywordString (info: Info) =
  | "assume" -> Some (1, lam lst. TmAssume {dist = get lst 0,
                                            ty = TyUnknown {info = info},
                                            info = info})
  | "observe" -> Some (2, lam lst. TmObserve {value = get lst 0,
                                              dist = get lst 1,
                                              ty = TyUnknown {info = info},
                                              info = info})
  | "weight" -> Some (1, lam lst. TmWeight {weight = get lst 0,
                                            ty = TyUnknown {info = info},
                                            info = info})
  | "resample" -> Some (0, lam lst. TmResample {ty = TyUnknown {info = info},
                                                info = info})
  | "infer" -> Some (2, lam lst. TmInfer {method = interpretInferMethod (get lst 0),
                                          model = get lst 1,
                                          ty = TyUnknown {info = info},
                                          info = info})
  | "Uniform" -> Some (2, lam lst. TmDist {dist = DUniform {a = get lst 0, b = get lst 1},
                                           ty = TyUnknown {info = info},
                                           info = info})
  | "Bernoulli" -> Some (1, lam lst. TmDist {dist = DBernoulli {p = get lst 0},
                                        ty = TyUnknown {info = info},
                                        info = info})
  | "Poisson" -> Some (1, lam lst. TmDist {dist = DPoisson {lambda = get lst 0},
                                           ty = TyUnknown {info = info},
                                           info = info})
  | "Beta" -> Some (2, lam lst. TmDist {dist = DBeta {a = get lst 0, b = get lst 1},
                                        ty = TyUnknown {info = info},
                                        info = info})
  | "Gamma" -> Some (2, lam lst. TmDist {dist = DGamma {k = get lst 0, theta = get lst 1},
                                         ty = TyUnknown {info = info},
                                         info = info})
  | "Categorical" -> Some (1, lam lst. TmDist {dist = DCategorical {p = get lst 0},
                                        ty = TyUnknown {info = info},
                                        info = info})
  | "Multinomial" -> Some (2, lam lst. TmDist {dist = DMultinomial {n = get lst 0, p = get lst 1},
                                        ty = TyUnknown {info = info},
                                        info = info})
  | "Dirichlet" -> Some (1, lam lst. TmDist {dist = DDirichlet {a = get lst 0},
                                        ty = TyUnknown {info = info},
                                        info = info})
  | "Exponential" -> Some (1, lam lst. TmDist {dist = DExponential {rate = get lst 0},
                                        ty = TyUnknown {info = info},
                                        info = info})
  | "Empirical" -> Some (1, lam lst. TmDist {dist = DEmpirical {samples = get lst 0},
                                        ty = TyUnknown {info = info},
                                        info = info})
  | "Gaussian" -> Some (2, lam lst. TmDist {dist = DGaussian {mu = get lst 0, sigma = get lst 1},
                                        ty = TyUnknown {info = info},
                                        info = info})
  | "Binomial" -> Some (2, lam lst. TmDist {dist = DBinomial {n = get lst 0, p = get lst 1},
                                        ty = TyUnknown {info = info},
                                        info = info})
  | "Wiener" -> Some (1, lam lst. TmDist {dist = DWiener {a = get lst 0},
                                       ty = TyUnknown {info = info},
                                       info = info})
  | "solveode" -> Some (4, lam lst. TmSolveODE {method = interpretODESolverMethod (get lst 0),
                                             model = get lst 1,
                                             init = get lst 2,
                                             endTime = get lst 3,
                                             ty = TyUnknown {info = info},
                                             info = info})
  | "diff" -> Some (2, lam lst. TmDiff {fn = get lst 0,
                                     arg = get lst 1,
                                     ty = TyUnknown {info = info},
                                     info = info})
  | "prune" -> Some (1, lam lst. TmPrune {dist = get lst 0,
                                          ty = TyUnknown {info = info},
                                          info = info})
  | "pruned" -> Some (1, lam lst. TmPruned {prune = get lst 0,
                                          ty = TyUnknown {info = info},
                                          info = info})
  | "cancel" -> Some (1, lam lst. TmCancel {dist = getDistCancel (get lst 0),
                                          value = getValueCancel (get lst 0),
                                          ty = TyUnknown {info = info},
                                          info = info})
  | "delay" -> Some (1, lam lst. TmDelay {dist = get lst 0,
                                          ty = TyUnknown {info = info},
                                          info = info})
  | "delayed" -> Some (1, lam lst. TmDelayed {delay = get lst 0,
                                          ty = TyUnknown {info = info},
                                          info = info})

  sem isTypeKeyword =
  | TyDist _ -> true
  | TyPruneInt _ -> true
  | TyDelayInt _ -> true
  | TyDelayFloat _ -> true
  | TyDelaySeqF _ -> true

  sem matchTypeKeywordString (info: Info) =
  | "Dist" -> Some(1, lam lst. TyDist { info = info, ty = get lst 0 })
  | "PruneInt" -> Some(0, lam lst. TyPruneInt { info = info})
  | "DelayInt" -> Some(0, lam lst. TyDelayInt { info = info})
  | "DelayFloat" -> Some(0, lam lst. TyDelayFloat { info = info})
  | "DelaySeqF" -> Some(0, lam lst. TyDelaySeqF { info = info})

end

-- Extend builtins with CorePPL builtins
let builtin = use MExprPPL in concat
  [ ("distEmpiricalSamples", CDistEmpiricalSamples ())
  , ("distEmpiricalDegenerate", CDistEmpiricalDegenerate ())
  , ("distEmpiricalNormConst", CDistEmpiricalNormConst ())
  , ("distEmpiricalAcceptRate", CDistEmpiricalAcceptRate ())
  , ("expectation", CDistExpectation ())
    -- External elementary functions
  , ("sin", CSin ())
  , ("cos", CCos ())
  , ("sqrt", CSqrt ())
  , ("exp", CExp ())
  , ("log", CLog ())
  , ("pow", CPow ())
  ] builtin

let defaultBootParserParseCorePPLFileArg =
  {defaultBootParserParseMCoreFileArg with
     keywords = pplKeywords,
     allowFree = true,
     builtin = builtin}

let parseMCorePPLFile = lam keepUtests. lam filename.
  use DPPLParser in
  -- Read and parse the mcore file
  let config =
    {defaultBootParserParseCorePPLFileArg with keepUtests = keepUtests} in
  let ast = parseMCoreFile config filename in
  let ast = symbolizeAllowFree ast in
  let ast = makeKeywords ast in
  ast

let parseMCorePPLFileLib = lam keepUtests. lam filename.
  use DPPLParser in
  -- Read and parse the mcore file
  let config = {defaultBootParserParseCorePPLFileArg with
                  keepUtests = keepUtests,
                  eliminateDeadCode = false} in
  let ast = parseMCoreFile config filename in
  makeKeywords ast

-- Similar to getAst, but calls parseMExprString instead
let parseMExprPPLString = lam cpplstr.
  use DPPLParser in
  let ast = parseMExprStringKeywordsExn pplKeywords cpplstr in
  makeKeywords ast
