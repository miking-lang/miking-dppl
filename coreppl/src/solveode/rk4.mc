include "../ode-solver-method.mc"

lang RK4Method = ODESolverMethodBase
  syn ODESolverMethod =
  | RK4 { stepSize: Expr }

  sem odeSolverMethodToString =
  | RK4 _ -> "RK4"

  sem eqODESolverMethod env free =
  | RK4 r -> eqExprH env free r.stepSize

  sem pprintODESolverMethod indent env =
  | RK4 { stepSize = stepSize } ->
    let i = pprintIncr indent in
    match pprintCode i env stepSize with (env, stepSize) in
    (env, join ["(RK4 {stepSize = ", stepSize, "})"])

  sem odeSolverMethodFromCon info bindings =
  | "RK4" ->
    let expectedFields = [
      ("stepSize", float_ default.stepSize)
    ] in
    match getFields info bindings expectedFields with [stepSize] in
    RK4 { stepSize = stepSize }

  sem odeSolverMethodConfig info =
  | RK4 { stepSize = stepSize } ->
    fieldsToRecord info [("stepSize", stepSize)]

  sem odeSolverMethodFromOptions options stepSize =
  | "rk4" -> RK4 {stepSize = stepSize}

  sem typeCheckODESolverMethod env info =
  | RK4 { stepSize = stepSize } ->
    let float = TyFloat {info = info} in
    let stepSize = typeCheckExpr env stepSize in
    unify env [info, infoTm stepSize] float (tyTm stepSize);
    RK4 { stepSize = stepSize }

  sem odeSolverMethodSmapAccumL_Expr_Expr f acc =
  | RK4 r ->
    match f acc r.stepSize with (acc, stepSize) in
    (acc, RK4 {r with stepSize = stepSize})
end
