include "../../coreppl.mc"

lang APFMethod = MExprPPL
  syn InferMethod =
  | APF {particles : Expr}

  sem pprintInferMethod indent env =
  | APF {particles = particles} ->
    let i = pprintIncr indent in
    match pprintCode i env particles with (env, particles) in
    (env, join ["APF {particles = ", particles, "}"])

  sem inferMethodFromCon info bindings =
  | "APF" ->
    match getFields info bindings ["particles"] with [particles] in
    APF {particles = particles}

  sem inferMethodFromOptions options =
  | "mexpr-apf" ->
    APF {particles = int_ options.particles}

  sem inferMethodConfig info =
  | APF {particles = particles} ->
    fieldsToRecord info [("particles", particles)]

  sem typeCheckInferMethod env info =
  | APF {particles = particles} ->
    let int = TyInt {info = info} in
    let particles = typeCheckExpr env particles in
    unify [info, infoTm particles] env (tyTm particles) int;
    APF {particles = particles}

  sem symbolizeInferMethod env =
  | APF r -> APF {r with particles = symbolizeExpr env r.particles}

  sem setRuns expr =
  | APF r -> APF {r with particles = expr}

end
