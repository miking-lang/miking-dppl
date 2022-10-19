include "mexpr/ast.mc"
include "../../infer-method.mc"

lang TraceMCMCMethod = InferMethodBase + MExprAst
  syn InferMethod =
  | TraceMCMC {
      iterations : Expr -- Type Int
    }

  sem pprintInferMethod indent env =
  | TraceMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    (env, join ["TraceMCMC {iterations = ", iterations, "}"])

  sem inferMethodFromCon info bindings =
  | methodStr & "TraceMCMC" ->
    match getFields info bindings ["iterations"]
    with [iterations] in
    TraceMCMC { iterations = iterations }

  sem inferMethodFromOptions options =
  | "mexpr-mcmc-trace" ->
    TraceMCMC {
      -- Reusing particles option for now for iterations, maybe we need a
      -- better name
      iterations = int_ options.particles
    }

  sem inferMethodConfig info =
  | TraceMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations)
    ]

  sem typeCheckInferMethod env info =
  | TraceMCMC t ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify [info, infoTm iterations] env (tyTm iterations) int;
    TraceMCMC {
      iterations = iterations
    }
end
