include "../infer-method.mc"

lang TraceMCMCMethod = InferMethodBase
  type TraceMCMCConfig =
    { iterations : Expr -- : Int
    , maxInitReSamp : Int
    }
  syn InferMethod =
  | TraceMCMC TraceMCMCConfig

  sem pprintInferMethod indent env =
  | TraceMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    let maxInitReSamp = int2string t.maxInitReSamp in
    (env, join ["(TraceMCMC {iterations = ", iterations, ", maxInitReSamp = ", maxInitReSamp, "})"])

  sem inferMethodFromCon info bindings =
  | methodStr & "TraceMCMC" ->
    let expectedFields = [
      ("iterations", int_ _particlesDefault)
    , ("maxInitReSamp", int_ _mcmcMaxInitialRejectSamplingDefault)
    ] in
    match getFields info bindings expectedFields with [iterations, maxInitReSamp] in
    TraceMCMC { iterations = iterations, maxInitReSamp = _exprAsIntExn maxInitReSamp }

  sem inferMethodConfig info =
  | TraceMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations)
    , ("maxInitReSamp", int_ t.maxInitReSamp)
    ]

  sem typeCheckInferMethod env info sampleType =
  | TraceMCMC t ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify env [info, infoTm iterations] int (tyTm iterations);
    TraceMCMC { t with
      iterations = iterations
    }

  sem smapAccumL_InferMethod_Expr f acc =
  | TraceMCMC r ->
    match f acc r.iterations with (acc, iterations) in
    (acc, TraceMCMC {r with iterations = iterations})

  sem setRuns expr =
  | TraceMCMC r -> TraceMCMC {r with iterations = expr}
end

let mcmcTraceOptions : OptParser (use TraceMCMCMethod in InferMethod) =
  use TraceMCMCMethod in
  let mk = lam iterations. lam maxInitReSamp. TraceMCMC
    { iterations = int_ iterations
    , maxInitReSamp = maxInitReSamp
    } in
  let method = optMap2 mk _particles _mcmcMaxInitialRejectSampling in
 optMap2 (lam. lam x. x) (_methodFlag false "mcmc-trace") method
