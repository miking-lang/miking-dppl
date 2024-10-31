include "../coreppl.mc"
include "../dppl-arg.mc"

lang DkMCMCMethod = MExprPPL
  syn InferMethod =
  | DkMCMC {
      iterations : Expr, -- Type Int
      globalProb : Expr -- Type Float (range 0 to 1)
    }

  sem pprintInferMethod indent env =
  | DkMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    match pprintCode i env t.globalProb with (env, globalProb) in
    (env, join ["(DkMCMC {iterations = ", iterations,
                "globalProb =", globalProb, "})"])

  sem inferMethodFromCon info bindings =
  | "DkMCMC" ->
    let expectedFields = [
      ("iterations", int_ default.particles),
      ("globalProb", float_ default.mcmcLightweightGlobalProb)
    ] in
    match getFields info bindings expectedFields
    with [iterations, globalProb] in
    DkMCMC {
      iterations = iterations, globalProb = globalProb
    }

  sem inferMethodFromOptions options =
  | "mcmc-lw-dk" ->
    DkMCMC {
      -- Reusing particles option for now for iterations, maybe we need a
      -- better name
      iterations = int_ options.particles,
      globalProb = float_ options.mcmcLightweightGlobalProb
    }

  sem inferMethodConfig info =
  | DkMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations),
      ("globalProb", t.globalProb)
    ]

  sem typeCheckInferMethod env info =
  | DkMCMC t ->
    let int = TyInt {info = info} in
    let bool = TyBool {info = info} in
    let float = TyFloat {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify env [info, infoTm iterations] int (tyTm iterations);
    let globalProb = typeCheckExpr env t.globalProb in
    unify env [info, infoTm globalProb] float (tyTm globalProb);
    DkMCMC {
      iterations = iterations,
      globalProb = globalProb
    }

  sem inferSmapAccumL_Expr_Expr f acc =
  | DkMCMC r ->
    match f acc r.iterations with (acc, iterations) in
    match f acc r.globalProb with (acc, globalProb) in
    (acc,
     DkMCMC {r with iterations = iterations, globalProb = globalProb})

  sem setRuns expr =
  | DkMCMC r -> DkMCMC {r with iterations = expr}
end
