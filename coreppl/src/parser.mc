include "digraph.mc"
include "mexpr/boot-parser.mc"
include "mexpr/keyword-maker.mc"
include "mexpr/builtin.mc"

include "coreppl.mc"
include "pgm.mc"
include "inference/smc.mc"

-- Include the inference method definition definition files.
include "inference/smc-apf.mc"
include "inference/smc-bpf.mc"
include "inference/is-lw.mc"
include "inference/mcmc-lightweight.mc"
include "inference/mcmc-naive.mc"
include "inference/mcmc-trace.mc"
include "inference/pmcmc-pimh.mc"

lang DPPLParser =
  BootParser + MExprPrettyPrint + MExprPPL + Resample +
  ProbabilisticGraphicalModel + KeywordMaker +

  ImportanceSamplingMethod + BPFMethod + APFMethod +
  LightweightMCMCMethod  + NaiveMCMCMethod + TraceMCMCMethod +
  PIMHMethod

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

  -- Keyword maker
  sem isKeyword =
  | TmAssume _ -> true
  | TmObserve _ -> true
  | TmWeight _ -> true
  | TmInfer _ -> true
  | TmDist _ -> true
  | TmPlate _ -> true
  | TmSolveODE _ -> true

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
  | "plate" -> Some (2, lam lst. TmPlate {fun = get lst 0,
                                          lst = get lst 1,
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
  | "solveode" -> Some (3, lam lst. TmSolveODE {model = get lst 0,
                                             init = get lst 1,
                                             endTime = get lst 2,
                                             ty = TyUnknown {info = info},
                                             info = info})

  sem isTypeKeyword =
  | TyDist _ -> true

  sem matchTypeKeywordString (info: Info) =
  | "Dist" -> Some(1, lam lst. TyDist { info = info, ty = get lst 0 })

end

-- Extend builtins with CorePPL builtins
let builtin = use MExprPPL in concat
  [ ("distEmpiricalSamples", CDistEmpiricalSamples ())
  , ("distEmpiricalDegenerate", CDistEmpiricalDegenerate ())
  , ("distEmpiricalNormConst", CDistEmpiricalNormConst ())
  , ("distEmpiricalAcceptRate", CDistEmpiricalAcceptRate ())
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
