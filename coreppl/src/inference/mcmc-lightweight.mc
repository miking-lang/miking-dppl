include "../coreppl.mc"
include "../dppl-arg.mc"

lang LightweightMCMCMethod = MExprPPL
  syn InferMethod =
  | LightweightMCMC {
      iterations : Expr, -- Type Int
      aligned : Expr, -- Type Bool
      globalProb : Expr -- Type Float (range 0 to 1)
    }

  sem pprintInferMethod indent env =
  | LightweightMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    match pprintCode i env t.aligned with (env, aligned) in
    match pprintCode i env t.globalProb with (env, globalProb) in
    (env, join ["LightweightMCMC {iterations = ", iterations,
                "aligned = ", aligned,
                "globalProb =", globalProb, "}"])

  sem inferMethodFromCon info bindings =
  | "LightweightMCMC" ->
    let expectedFields = [
      ("iterations", int_ default.particles),
      ("aligned", bool_ default.align),
      ("globalProb", float_ default.mcmcLightweightGlobalProb)
    ] in
    match getFields info bindings expectedFields
    with [iterations, aligned, globalProb] in
    LightweightMCMC {
      iterations = iterations, aligned = aligned, globalProb = globalProb
    }

  sem inferMethodFromOptions options =
  | "mcmc-lightweight" ->
    LightweightMCMC {
      -- Reusing particles option for now for iterations, maybe we need a
      -- better name
      iterations = int_ options.particles,
      aligned = bool_ options.align,
      globalProb = float_ options.mcmcLightweightGlobalProb
    }

  sem inferMethodConfig info =
  | LightweightMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations),
      ("aligned", t.aligned),
      ("globalProb", t.globalProb)
    ]

  sem typeCheckInferMethod env info =
  | LightweightMCMC t ->
    let int = TyInt {info = info} in
    let bool = TyBool {info = info} in
    let float = TyFloat {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify env [info, infoTm iterations] int (tyTm iterations);
    let aligned = typeCheckExpr env t.aligned in
    unify env [info, infoTm aligned] bool (tyTm aligned);
    let globalProb = typeCheckExpr env t.globalProb in
    unify env [info, infoTm globalProb] float (tyTm globalProb);
    LightweightMCMC {
      iterations = iterations,
      aligned = aligned,
      globalProb = globalProb
    }

  sem symbolizeInferMethod env =
  | LightweightMCMC r ->
    LightweightMCMC {r with
      iterations = symbolizeExpr env r.iterations,
      aligned = symbolizeExpr env r.aligned,
      globalProb = symbolizeExpr env r.globalProb
    }

  sem setRuns expr =
  | LightweightMCMC r -> LightweightMCMC {r with iterations = expr}
end
