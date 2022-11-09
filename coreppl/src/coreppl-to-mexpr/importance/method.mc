include "../../coreppl.mc"
include "../../dppl-arg.mc"

lang ImportanceSamplingMethod = MExprPPL
  syn InferMethod =
  | Importance {particles : Expr}

  sem pprintInferMethod indent env =
  | Importance {particles = particles} ->
    let i = pprintIncr indent in
    match pprintCode i env particles with (env, particles) in
    (env, join ["Importance {particles = ", particles, "}"])

  sem inferMethodFromCon info bindings =
  | "Importance" ->
    let expectedFields = [
      ("particles", int_ default.particles)
    ] in
    match getFields info bindings expectedFields with [particles] in
    Importance {particles = particles}

  sem inferMethodFromOptions options =
  | "mexpr-importance" ->
    Importance {particles = int_ options.particles}

  sem inferMethodConfig info =
  | Importance {particles = particles} ->
    fieldsToRecord info [("particles", particles)]

  sem typeCheckInferMethod env info =
  | Importance {particles = particles} ->
    let int = TyInt {info = info} in
    let particles = typeCheckExpr env particles in
    unify [info, infoTm particles] env (tyTm particles) int;
    Importance {particles = particles}

  sem symbolizeInferMethod env =
  | Importance r -> Importance {r with particles = symbolizeExpr env r.particles}

  sem setRuns expr =
  | Importance r -> Importance {r with particles = expr}

end
