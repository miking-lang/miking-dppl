include "../coreppl.mc"
include "../dppl-arg.mc"

lang NaiveMCMCMethod = MExprPPL
  syn InferMethod =
  | NaiveMCMC {
      iterations : Expr -- Type Int
    }

  sem pprintInferMethod indent env =
  | NaiveMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    (env, join ["NaiveMCMC {iterations = ", iterations, "}"])

  sem inferMethodFromCon info bindings =
  | "NaiveMCMC" ->
    let expectedFields = [
      ("iterations", int_ default.particles)
    ] in
    match getFields info bindings expectedFields with [iterations] in
    NaiveMCMC { iterations = iterations }

  sem inferMethodFromOptions options =
  | "mcmc-naive" ->
    NaiveMCMC {
      -- Reusing particles option for now for iterations, maybe we need a
      -- better name
      iterations = int_ options.particles
    }

  sem inferMethodConfig info =
  | NaiveMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations)
    ]

  sem typeCheckInferMethod env info =
  | NaiveMCMC t ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify env [info, infoTm iterations] int (tyTm iterations);
    NaiveMCMC {
      iterations = iterations
    }

  sem symbolizeInferMethod env =
  | NaiveMCMC r ->
    NaiveMCMC {r with iterations = symbolizeExpr env r.iterations}

  sem setRuns expr =
  | NaiveMCMC r -> NaiveMCMC {r with iterations = expr}

end
