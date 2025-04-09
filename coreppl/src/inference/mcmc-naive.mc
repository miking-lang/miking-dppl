include "../infer-method.mc"

lang NaiveMCMCMethod = InferMethodBase
  type NaiveMCMCConfig =
    { iterations : Expr -- : Int
    }
  syn InferMethod =
  | NaiveMCMC NaiveMCMCConfig

  sem pprintInferMethod indent env =
  | NaiveMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    (env, join ["(NaiveMCMC {iterations = ", iterations, "})"])

  sem inferMethodFromCon info bindings =
  | "NaiveMCMC" ->
    let expectedFields = [
      ("iterations", int_ _particlesDefault)
    ] in
    match getFields info bindings expectedFields with [iterations] in
    NaiveMCMC { iterations = iterations }

  sem inferMethodConfig info =
  | NaiveMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations)
    ]

  sem typeCheckInferMethod env info sampleType =
  | NaiveMCMC t ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify env [info, infoTm iterations] int (tyTm iterations);
    NaiveMCMC {
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
  let mk = lam iterations. NaiveMCMC
    { iterations = int_ iterations
    } in
  let method = optMap mk _particles in
  optMap2 (lam. lam x. x) (_methodFlag false "mcmc-naive") method
