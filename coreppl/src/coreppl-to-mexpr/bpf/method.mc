include "mexpr/ast.mc"
include "../../infer-method.mc"

lang BPFMethod = InferMethodBase + MExprAst
  syn InferMethod =
  | BPF {particles : Expr}

  sem pprintInferMethod indent env =
  | BPF {particles = particles} ->
    let i = pprintIncr indent in
    match pprintCode i env particles with (env, particles) in
    (env, join ["BPF {particles = ", particles, "}"])

  sem inferMethodFromCon info bindings =
  | "BPF" ->
    BPF {particles = getRequiredField info bindings "particles"}

  sem inferMethodFromOptions options =
  | "mexpr-bpf" ->
    BPF {particles = int_ options.particles}

  sem typeCheckInferMethod env info =
  | BPF {particles = particles} ->
    let int = TyInt {info = info} in
    let particles = typeCheckExpr env particles in
    unify [info, infoTm particles] env (tyTm particles) int;
    BPF {particles = particles}
end
