include "../coreppl.mc"
include "../dppl-arg.mc"

lang BPFMethod = MExprPPL
  syn InferMethod =
  | BPF {particles : Expr}

  sem pprintInferMethod indent env =
  | BPF {particles = particles} ->
    let i = pprintIncr indent in
    match pprintCode i env particles with (env, particles) in
    (env, join ["(BPF {particles = ", particles, "})"])

  sem inferMethodFromCon info bindings =
  | "BPF" ->
    let expectedFields = [
      ("particles", int_ default.particles)
    ] in
    match getFields info bindings expectedFields with [particles] in
    BPF {particles = particles}

  sem inferMethodFromOptions options =
  | "smc-bpf" ->
    BPF {particles = int_ options.particles}

  sem inferMethodConfig info =
  | BPF {particles = particles} ->
    fieldsToRecord info [("particles", particles)]

  sem inferMethodConfigType info =
  | BPF _ ->
    tyRecord info [("particles", ityint_ info)]

  sem typeCheckInferMethod env info =
  | BPF {particles = particles} ->
    let int = TyInt {info = info} in
    let particles = typeCheckExpr env particles in
    unify env [info, infoTm particles] int (tyTm particles);
    BPF {particles = particles}

  sem inferSmapAccumL_Expr_Expr env =
  | BPF r ->
    match f acc r.particles with (acc, particles) in
    (acc, BPF {r with particles = particles})

  sem setRuns expr =
  | BPF r -> BPF {r with particles = expr}

end
