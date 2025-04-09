include "../infer-method.mc"

lang TraceMCMCMethod = InferMethodBase
  type TraceMCMCConfig =
    { iterations : Expr -- : Int
    }
  syn InferMethod =
  | TraceMCMC TraceMCMCConfig

  sem pprintInferMethod indent env =
  | TraceMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    (env, join ["(TraceMCMC {iterations = ", iterations, "})"])

  sem inferMethodFromCon info bindings =
  | methodStr & "TraceMCMC" ->
    let expectedFields = [
      ("iterations", int_ _particlesDefault)
    ] in
    match getFields info bindings expectedFields with [iterations] in
    TraceMCMC { iterations = iterations }

  sem inferMethodConfig info =
  | TraceMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations)
    ]

  sem typeCheckInferMethod env info sampleType =
  | TraceMCMC t ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify env [info, infoTm iterations] int (tyTm iterations);
    TraceMCMC {
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
  let mk = lam iterations. TraceMCMC
    { iterations = int_ iterations
    } in
  let method = optMap mk _particles in
  optMap2 (lam. lam x. x) (_methodFlag false "mcmc-trace") method
