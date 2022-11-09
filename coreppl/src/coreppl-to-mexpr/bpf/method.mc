include "../../coreppl.mc"
include "../../dppl-arg.mc"

lang BPFMethod = MExprPPL
  syn InferMethod =
  | BPF {particles : Expr}

  sem pprintInferMethod indent env =
  | BPF {particles = particles} ->
    let i = pprintIncr indent in
    match pprintCode i env particles with (env, particles) in
    (env, join ["BPF {particles = ", particles, "}"])

  sem inferMethodFromCon info bindings =
  | "BPF" ->
    let expectedFields = [
      ("particles", int_ default.particles)
    ] in
    match getFields info bindings expectedFields with [particles] in
    BPF {particles = particles}

  sem inferMethodFromOptions options =
  | "mexpr-bpf" ->
    BPF {particles = int_ options.particles}

  sem inferMethodConfig info =
  | BPF {particles = particles} ->
    fieldsToRecord info [("particles", particles)]

  sem typeCheckInferMethod env info =
  | BPF {particles = particles} ->
    let int = TyInt {info = info} in
    let particles = typeCheckExpr env particles in
    unify [info, infoTm particles] env (tyTm particles) int;
    BPF {particles = particles}

  sem symbolizeInferMethod env =
  | BPF r -> BPF {r with particles = symbolizeExpr env r.particles}

  sem setRuns expr =
  | BPF r -> BPF {r with particles = expr}

end
