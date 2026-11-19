include "../infer-method.mc"

lang TraceMCMCMethod = InferMethodBase
  type TraceMCMCConfig =
    { iterations : Expr -- : Int
    , maxInitReSamp : Expr -- : Int
    }
  syn InferMethod =
  | TraceMCMC TraceMCMCConfig

  sem pprintInferMethod indent env =
  | TraceMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    match pprintCode i env t.maxInitReSamp with (env, maxInitReSamp) in
    (env, join ["(TraceMCMC {iterations = ", iterations, ", maxInitReSamp = ", maxInitReSamp, "})"])

  sem inferMethodFromCon info bindings =
  | methodStr & "TraceMCMC" ->
    let expectedFields = [
      ("iterations", int_ _particlesDefault)
    , ("maxInitReSamp", int_ _mcmcMaxInitialRejectSamplingDefault)
    ] in
    match getFields info bindings expectedFields with [iterations, maxInitReSamp] in
    TraceMCMC { iterations = iterations, maxInitReSamp = maxInitReSamp }

  sem inferMethodConfig info =
  | TraceMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations)
    , ("maxInitReSamp", t.maxInitReSamp)
    ]

  sem typeCheckInferMethod env info sampleType =
  | TraceMCMC t ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify env [info, infoTm iterations] int (tyTm iterations);
    let maxInitReSamp = typeCheckExpr env t.maxInitReSamp in
    TraceMCMC { t with
      iterations = iterations
    , maxInitReSamp = maxInitReSamp
    }

  sem smapAccumL_InferMethod_Expr f acc =
  | TraceMCMC r ->
    match f acc r.iterations with (acc, iterations) in
    match f acc r.maxInitReSamp with (acc, maxInitReSamp) in
    (acc, TraceMCMC {r with iterations = iterations, maxInitReSamp = maxInitReSamp})

  sem setRuns expr =
  | TraceMCMC r -> TraceMCMC {r with iterations = expr}
end

let mcmcTraceOptions : OptParser (use TraceMCMCMethod in InferMethod) =
  use TraceMCMCMethod in
  let mk = lam iterations. lam maxInitReSamp. TraceMCMC
    { iterations = int_ iterations
    , maxInitReSamp = int_ maxInitReSamp
    } in
  let method = optMap2 mk _particles _mcmcMaxInitialRejectSampling in
 optMap2 (lam. lam x. x) (_methodFlag false "mcmc-trace") method
