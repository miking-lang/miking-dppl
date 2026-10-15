include "../infer-method.mc"

lang NaiveMCMCMethod = InferMethodBase
  type NaiveMCMCConfig =
    { iterations : Expr -- : Int
    , maxInitReSamp : Int
    }
  syn InferMethod =
  | NaiveMCMC NaiveMCMCConfig

  sem pprintInferMethod indent env =
  | NaiveMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    let maxInitReSamp = int2string t.maxInitReSamp in
    (env, join ["(NaiveMCMC {iterations = ", iterations, ", maxInitReSamp = ", maxInitReSamp, "})"])

  sem inferMethodFromCon info bindings =
  | "NaiveMCMC" ->
    let expectedFields = [
      ("iterations", int_ _particlesDefault)
    , ("maxInitReSamp", int_ _mcmcMaxInitialRejectSamplingDefault)
    ] in
    match getFields info bindings expectedFields with [iterations, maxInitReSamp] in
    NaiveMCMC { iterations = iterations, maxInitReSamp = _exprAsIntExn maxInitReSamp }

  sem inferMethodConfig info =
  | NaiveMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations)
    , ("maxInitReSamp", int_ t.maxInitReSamp)
    ]

  sem typeCheckInferMethod env info sampleType =
  | NaiveMCMC t ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify env [info, infoTm iterations] int (tyTm iterations);
    NaiveMCMC { t with
      iterations = iterations
    }

  sem smapAccumL_InferMethod_Expr f acc =
  | NaiveMCMC r ->
    match f acc r.iterations with (acc, iterations) in
    (acc, NaiveMCMC {r with iterations = iterations})

  sem setRuns expr =
  | NaiveMCMC r -> NaiveMCMC {r with iterations = expr}
end

let mcmcNaiveOptions : OptParser (use NaiveMCMCMethod in InferMethod) =
  use NaiveMCMCMethod in
  let mk = lam iterations. lam maxInitReSamp. NaiveMCMC
    { iterations = int_ iterations
    , maxInitReSamp = maxInitReSamp
    } in
  let method = optMap2 mk _particles _mcmcMaxInitialRejectSampling in
  optMap2 (lam. lam x. x) (_methodFlag false "mcmc-naive") method
