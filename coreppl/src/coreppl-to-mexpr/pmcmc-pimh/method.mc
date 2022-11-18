include "../../coreppl.mc"
include "../../dppl-arg.mc"

lang PIMHMethod = MExprPPL
  syn InferMethod =
  | PIMH {
      particles : Expr, -- Type Int
      iterations : Expr -- Type Int
    }

  sem pprintInferMethod indent env =
  | PIMH {iterations = iterations, particles = particles} ->
    let i = pprintIncr indent in
    match pprintCode i env particles with (env, particles) in
    match pprintCode i env iterations with (env, iterations) in
    (env, join [
      "PIMH {interations = ", iterations, "particles = ", particles, "}"])

  sem inferMethodFromCon info bindings =
  | "PIMH" ->
    let expectedFields = [
      ("particles", int_ default.particles),
      ("iterations", int_ default.pmcmcParticles)
    ] in
    match getFields info bindings expectedFields with [particles, iterations] in
    PIMH {particles = particles, iterations = iterations}

  sem inferMethodFromOptions options =
  | "mexpr-pmcmc-pimh" ->
    PIMH {
      -- Reusing particles option for now for iterations, maybe we need a
      -- better name
      iterations = int_ options.particles,
      particles = int_ options.pmcmcParticles
    }

  sem inferMethodConfig info =
  | PIMH {iterations = iterations, particles = particles} ->
    fieldsToRecord info [("iterations",iterations), ("particles", particles)]

  sem typeCheckInferMethod env info =
  | PIMH {iterations = iterations, particles = particles} ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env iterations in
    let particles = typeCheckExpr env particles in
    unify [info, infoTm iterations] env (tyTm iterations) int;
    unify [info, infoTm particles] env (tyTm particles) int;
    PIMH {iterations = iterations, particles = particles}

  sem symbolizeInferMethod env =
  | PIMH r -> PIMH {r with
      iterations = symbolizeExpr env r.iterations,
      particles = symbolizeExpr env r.particles
    }

  sem setRuns expr =
  | PIMH r -> PIMH {r with iterations = expr}

end
