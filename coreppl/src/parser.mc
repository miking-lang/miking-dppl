include "digraph.mc"
include "mexpr/boot-parser.mc"
include "mexpr/keyword-maker.mc"

include "coreppl.mc"
include "pgm.mc"
include "inference-common/smc.mc"
include "coreppl-to-mexpr/importance/method.mc"

lang DPPLParser =
  BootParser + MExprPrettyPrint + MExprPPL + Resample +
  ProbabilisticGraphicalModel + KeywordMaker + ImportanceSamplingMethod

  -- Keyword maker
  sem isKeyword =
  | TmAssume _ -> true
  | TmObserve _ -> true
  | TmWeight _ -> true
  | TmInfer _ -> true
  | TmDist _ -> true
  | TmPlate _ -> true

  sem parseMethod : Expr -> Option InferMethod
  sem parseMethod =
  | TmRecord t ->
    recursive let mexprStringToString : [Expr] -> Option String = lam tms.
      if null tms then Some ""
      else match tms with [TmConst {val = CChar {val = c}}] ++ t then
        optionMap (lam x. cons c x) (mexprStringToString t)
      else None ()
    in
    let methodSid = stringToSid "method" in
    match mapLookup methodSid t.bindings with Some (TmSeq {tms = chars}) then
      match mexprStringToString chars with Some method then
        -- TODO(larshum, 2022-10-05): Add support for inference method specific
        -- parsing of parameters here.
        Some (parseInferMethod method)
      else None ()
    else None ()
  | _ -> None ()

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
  | "infer" ->
    let method = lam arg.
      match parseMethod arg with Some inferMethod then inferMethod
      else errorSingle [infoTm arg] "Invalid inference configuration parameter"
    in
    Some (2, lam lst. TmInfer {method = method (get lst 0),
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
end

let parseMCorePPLFile = lam filename.
  use DPPLParser in
  -- Read and parse the mcore file
  let config = {{{defaultBootParserParseMCoreFileArg
                  with keepUtests = false}
                  with keywords = pplKeywords}
                  with allowFree = true} in
  makeKeywords [] (parseMCoreFile config filename)


let parseMCorePPLFileNoDeadCodeElimination = lam filename.
  use DPPLParser in
  -- Read and parse the mcore file
  let config = {{{{defaultBootParserParseMCoreFileArg
                   with keepUtests = false}
                   with keywords = pplKeywords}
                   with eliminateDeadCode = false}
                   with allowFree = true} in
  makeKeywords [] (parseMCoreFile config filename)

-- Similar to getAst, but calls parseMExprString instead
let parseMExprPPLString = lam cpplstr.
  use DPPLParser in
  makeKeywords [] (parseMExprStringKeywords pplKeywords cpplstr)

