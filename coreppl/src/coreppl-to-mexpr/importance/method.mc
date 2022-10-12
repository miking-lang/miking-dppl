include "mexpr/ast.mc"
include "../../infer-method.mc"

lang ImportanceSamplingMethod = InferMethodBase + MExprAst
  syn InferMethod =
  | Importance {particles : Expr}

  sem pprintInferMethod indent env =
  | Importance {particles = particles} ->
    let i = pprintIncr indent in
    match pprintCode i env particles with (env, particles) in
    (env, join ["Importance {particles = ", particles, "}"])

  sem inferMethodFromCon info bindings =
  | methodStr & "Importance" ->
    Importance {particles = getRequiredField info bindings "particles"}

  sem inferMethodFromOptions options =
  | "mexpr-importance" ->
    Importance {particles = int_ options.particles}

  sem typeCheckInferMethod env info =
  | Importance {particles = particles} ->
    let int = TyInt {info = info} in
    let particles = typeCheckExpr env particles in
    unify [info, infoTm particles] env (tyTm particles) int;
    Importance {particles = particles}
end
