include "../coreppl.mc"
include "../dppl-arg.mc"

lang APFMethod = MExprPPL
  syn InferMethod =
  | APF {particles : Expr}

  sem pprintInferMethod indent env =
  | APF {particles = particles} ->
    let i = pprintIncr indent in
    match pprintCode i env particles with (env, particles) in
    (env, join ["(APF {particles = ", particles, "})"])

  sem inferMethodFromCon info bindings =
  | "APF" ->
    let expectedFields = [
      ("particles", int_ default.particles)
    ] in
    match getFields info bindings expectedFields with [particles] in
    APF {particles = particles}

  sem inferMethodFromOptions options =
  | "smc-apf" ->
    APF {particles = int_ options.particles}

  sem inferMethodConfig info =
  | APF {particles = particles} ->
    fieldsToRecord info [("particles", particles)]

  sem inferMethodConfigType info =
  | APF _ ->
    tyRecord info [("particles", ityint_ info)]

  sem typeCheckInferMethod env info =
  | APF {particles = particles} ->
    let int = TyInt {info = info} in
    let particles = typeCheckExpr env particles in
    unify env [info, infoTm particles] int (tyTm particles);
    APF {particles = particles}

  sem smapAccumL_InferMethod_Expr f acc =
  | APF r ->
    match f acc r.particles with (acc, particles) in
    (acc, APF {r with particles = particles})

  sem setRuns expr =
  | APF r -> APF {r with particles = expr}

end
