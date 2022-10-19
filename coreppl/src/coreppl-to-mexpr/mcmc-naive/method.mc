include "mexpr/ast.mc"
include "../../infer-method.mc"

lang NaiveMCMCMethod = InferMethodBase + MExprAst
  syn InferMethod =
  | NaiveMCMC {
      iterations : Expr, -- Type Int
    }

  sem pprintInferMethod indent env =
  | NaiveMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.iterations with (env, iterations) in
    (env, join ["NaiveMCMC {iterations = ", iterations, "}"])

  sem inferMethodFromCon info bindings =
  | methodStr & "NaiveMCMC" ->
    match getFields info bindings ["iterations"]
    with [iterations] in
    NaiveMCMC { iterations = iterations }

  sem inferMethodFromOptions options =
  | "mexpr-mcmc-lightweight" ->
    NaiveMCMC {
      -- Reusing particles option for now for iterations, maybe we need a
      -- better name
      iterations = int_ options.particles
    }

  sem inferMethodConfig info =
  | NaiveMCMC t ->
    fieldsToRecord info [
      ("iterations", t.iterations)
    ]

  sem typeCheckInferMethod env info =
  | NaiveMCMC t ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env t.iterations in
    unify [info, infoTm iterations] env (tyTm iterations) int;
    NaiveMCMC {
      iterations = iterations,
    }
end
