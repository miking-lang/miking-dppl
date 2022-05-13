

include "mexpr/boot-parser.mc"
include "mexpr/keyword-maker.mc"
include "coreppl.mc"
include "pgm.mc"
include "inference-common/smc.mc"


lang DPPLParser = BootParser + MExprPrettyPrint + MExprPPL + Resample + ProbabilisticGraphicalModel + KeywordMaker

  -- Keyword maker
  sem isKeyword =
  | TmAssume _ -> true
  | TmObserve _ -> true
  | TmWeight _ -> true
  | TmDist _ -> true
  | TmPlate _ -> true

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
end

let getAst = lam filename.
  use DPPLParser in
  -- Read and parse the mcore file
  let config = {{defaultBootParserParseMCoreFileArg
                 with keepUtests = false}
                 with keywords = pplKeywords} in
  makeKeywords [] (parseMCoreFile config filename)

-- Similar to getAst, but calls parseMExprString instead
let parseMExprPPLString = lam cpplstr.
  use DPPLParser in
  makeKeywords [] (parseMExprString pplKeywords cpplstr)


