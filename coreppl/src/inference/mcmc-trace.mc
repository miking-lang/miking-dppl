include "../coreppl.mc"
include "../dppl-arg.mc"

lang TraceMCMCMethod = MExprPPL
  syn InferMethod =
  | TraceMCMC {
      iterations : Expr -- Type Int
    }

  sem pprintInferMethod indent env =
  | TraceMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    (env, join ["(TraceMCMC {iterations = ", iterations, "})"])

  sem inferMethodFromCon info bindings =
  | methodStr & "TraceMCMC" ->
    let expectedFields = [
      ("iterations", int_ default.particles)
    ] in
    match getFields info bindings expectedFields with [iterations] in
    TraceMCMC { iterations = iterations }

  sem inferMethodFromOptions options =
  | "mcmc-trace" ->
    TraceMCMC {
      -- Reusing particles option for now for iterations, maybe we need a
      -- better name
      iterations = int_ options.particles
    }

  sem inferMethodConfig info =
  | TraceMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations)
    ]

  sem inferMethodConfigType info =
  | TraceMCMC _ ->
    tyRecord info [
      ("iterations", ityint_ info)
    ]

  sem typeCheckInferMethod env info =
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
