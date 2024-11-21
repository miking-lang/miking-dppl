include "../coreppl.mc"
include "../dppl-arg.mc"

lang LightweightMCMCMethod = MExprPPL
  syn InferMethod =
  | LightweightMCMC {
      iterations : Expr, -- Type Int
      globalProb : Expr -- Type Float (range 0 to 1)
    }

  sem pprintInferMethod indent env =
  | LightweightMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    match pprintCode i env t.globalProb with (env, globalProb) in
    (env, join ["(LightweightMCMC {iterations = ", iterations,
                "globalProb =", globalProb, "})"])

  sem inferMethodFromCon info bindings =
  | "LightweightMCMC" ->
    let expectedFields = [
      ("iterations", int_ default.particles),
      ("globalProb", float_ default.mcmcLightweightGlobalProb)
    ] in
    match getFields info bindings expectedFields
    with [iterations, globalProb] in
    LightweightMCMC {
      iterations = iterations, globalProb = globalProb
    }

  sem inferMethodFromOptions options =
  | "mcmc-lightweight" ->
    LightweightMCMC {
      -- Reusing particles option for now for iterations, maybe we need a
      -- better name
      iterations = int_ options.particles,
      globalProb = float_ options.mcmcLightweightGlobalProb
    }

  sem inferMethodConfig info =
  | LightweightMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations),
      ("globalProb", t.globalProb)
    ]

  sem inferMethodConfigType info =
  | LightweightMCMC _ ->
    tyRecord info [
      ("iterations", ityint_ info),
      ("globalProb", ityfloat_ info)
    ]

  sem typeCheckInferMethod env info =
  | LightweightMCMC t ->
    let int = TyInt {info = info} in
    let bool = TyBool {info = info} in
    let float = TyFloat {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify env [info, infoTm iterations] int (tyTm iterations);
    let globalProb = typeCheckExpr env t.globalProb in
    unify env [info, infoTm globalProb] float (tyTm globalProb);
    LightweightMCMC {
      iterations = iterations,
      globalProb = globalProb
    }

  sem smapAccumL_InferMethod_Expr f acc =
  | LightweightMCMC r ->
    match f acc r.iterations with (acc, iterations) in
    match f acc r.globalProb with (acc, globalProb) in
    (acc,
     LightweightMCMC {r with iterations = iterations, globalProb = globalProb})

  sem setRuns expr =
  | LightweightMCMC r -> LightweightMCMC {r with iterations = expr}
end
