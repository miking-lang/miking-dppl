include "../coreppl.mc"
include "../dppl-arg.mc"

lang BPFMethod = MExprPPL
  syn InferMethod =
  | BPF {particles : Expr
        ,resampleFrac : Expr}

  sem pprintInferMethod indent env =
  | BPF {particles = particles, resampleFrac = resampleFrac} ->
    let i = pprintIncr indent in
    match pprintCode i env particles with (env, particles) in
    match pprintCode i env resampleFrac with (env, resampleFrac) in
    (env, join ["(BPF {particles = ", particles,
                "resampleFrac =", resampleFrac, "})"])

  sem inferMethodFromCon info bindings =
  | "BPF" ->
    let expectedFields = [
      ("particles", int_ defaultArgs.particles),
      ("resampleFrac", float_ defaultArgs.resampleFrac)
    ] in
    match getFields info bindings expectedFields with [particles, resampleFrac] in
    BPF {particles = particles, resampleFrac = resampleFrac}

  sem inferMethodFromOptions options =
  | "smc-bpf" ->
    BPF {particles = int_ options.particles,
        resampleFrac = float_ options.resampleFrac}

  sem inferMethodConfig info =
  | BPF {particles = particles, resampleFrac = resampleFrac} ->
    fieldsToRecord info [
      ("particles", particles),
      ("resampleFrac", resampleFrac)
    ]


  sem typeCheckInferMethod env info sampleType =
  | BPF {particles = particles, resampleFrac = resampleFrac} ->
    let int = TyInt {info = info} in
    let float = TyFloat {info = info} in
    let particles = typeCheckExpr env particles in
    unify env [info, infoTm particles] int (tyTm particles);
    let resampleFrac = typeCheckExpr env resampleFrac in
    unify env [info, infoTm resampleFrac] float (tyTm resampleFrac);
    BPF {particles = particles, resampleFrac = resampleFrac}

  sem smapAccumL_InferMethod_Expr env =
  | BPF r ->
    match f acc r.particles with (acc, particles) in
    match f acc r.resampleFrac with (acc, resampleFrac) in
    (acc, BPF {r with particles = particles, resampleFrac = resampleFrac})

  sem setRuns expr =
  | BPF r -> BPF {r with particles = expr}

end
