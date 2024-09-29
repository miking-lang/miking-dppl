include "option.mc"
include "stringid.mc"
include "mexpr/info.mc"
include "mexpr/pprint.mc"
include "mexpr/type-check.mc"
include "mexpr/symbolize.mc"
include "mexpr/eq.mc"

include "method-helper.mc"
include "dppl-arg.mc"

-- Defines the basic components required in the implementation of an ODE solver
-- method.
--
-- To define a new ODE solver method constructor, the following semantic
-- functions must be implemented for the constructor.
lang ODESolverMethod = PrettyPrint + TypeCheck + Sym + Eq + MethodHelper
  syn ODESolverMethod =
  | ODESolverDefault { stepSize: Expr }
  | RK4 { stepSize: Expr }
  | EF { stepSize: Expr }

  -- NOTE(oerikss, 2024-03-08): Compares the inference methods tags only.
  sem cmpODESolverMethod : ODESolverMethod -> ODESolverMethod -> Int
  sem cmpODESolverMethod lhs =
  | rhs -> subi (constructorTag lhs) (constructorTag rhs)

  sem eqODESolverMethod
    : EqEnv -> EqEnv -> ODESolverMethod -> ODESolverMethod -> Option EqEnv
  sem eqODESolverMethod env free =
  | ODESolverDefault r
  | RK4 r
  | EF r -> eqExprH env free r.stepSize

  sem odeSolverMethodToString : ODESolverMethod -> String
  sem odeSolverMethodToString =
  | ODESolverDefault _ -> "Default"
  | RK4 _ -> "RK4"
  | EF _ -> "EF"

  sem pprintODESolverMethod
    : Int -> PprintEnv -> ODESolverMethod -> (PprintEnv, String)
  sem pprintODESolverMethod indent env =
  | m & (ODESolverDefault r
       | RK4 r
       | EF r) ->
    _pprintODESolverStepSize1 (odeSolverMethodToString m) indent env r.stepSize

  sem _pprintODESolverStepSize1 method indent env =| stepSize ->
    let i = pprintIncr indent in
    match pprintCode i env stepSize with (env, stepSize) in
    (env, join ["(", method" {stepSize = ", stepSize,"})"])

  -- Constructs an ODE solver method from the arguments of a TmConApp.
  sem odeSolverMethodFromCon : Info -> Map SID Expr -> String -> ODESolverMethod
  sem odeSolverMethodFromCon info bindings =
  | "Default" ->
    ODESolverDefault (_odeSolverMethodExpectStepSize1 info bindings)
  | "RK4" -> RK4 (_odeSolverMethodExpectStepSize1 info bindings)
  | "EF" -> EF (_odeSolverMethodExpectStepSize1 info bindings)
  | s -> errorSingle [info] (concat "Unknown ODE solver method: " s)

  sem _odeSolverMethodExpectStepSize1 info =| bindings ->
    let expectedFields = [
      ("stepSize", float_ default.stepSize)
    ] in
    match getFields info bindings expectedFields with [stepSize] in
    { stepSize = stepSize }

  -- Produces a record expression containing the configuration parameters of the
  -- ODE solver method. This record is passed to the inference runtime function.
  sem odeSolverMethodConfig : Info -> ODESolverMethod -> Expr
  sem odeSolverMethodConfig info =
  | ODESolverDefault r
  | RK4 r
  | EF r ->
    fieldsToRecord info [("stepSize", r.stepSize)]

  -- Constructs a ODE solver method from command-line options.
  sem odeSolverMethodFromOptions : Options -> Expr -> String -> ODESolverMethod
  sem odeSolverMethodFromOptions options stepSize =
  | "rk4" -> RK4 {stepSize = stepSize}
  | "ef" -> EF {stepSize = stepSize}
  | s -> error (concat "Unknown ODE solver method string: " s)

  -- Type checks the ODE solver method. This ensures that the provided arguments
  -- have the correct types.
  sem typeCheckODESolverMethod
    : TCEnv -> Info -> ODESolverMethod -> ODESolverMethod
  sem typeCheckODESolverMethod env info =
  | ODESolverDefault r ->
    ODESolverDefault (_typeCheckODESolverStepSize1 env info r.stepSize)
  | RK4 r ->
    RK4 (_typeCheckODESolverStepSize1 env info r.stepSize)
  | EF r ->
    EF (_typeCheckODESolverStepSize1 env info r.stepSize)

  sem _typeCheckODESolverStepSize1 env info =| stepSize ->
    let float = TyFloat {info = info} in
    let stepSize = typeCheckExpr env stepSize in
    unify env [info, infoTm stepSize] float (tyTm stepSize);
    { stepSize = stepSize }

  -- Map/Accum over expressions in ODE solver method.
  sem odeSolverMethodSmapAccumL_Expr_Expr
    : all a. (a -> Expr -> (a, Expr)) -> a -> ODESolverMethod -> (a, ODESolverMethod)
  sem odeSolverMethodSmapAccumL_Expr_Expr f acc =
  | ODESolverDefault r ->
    match f acc r.stepSize with (acc, stepSize) in
    (acc, ODESolverDefault {r with stepSize = stepSize})
  | RK4 r ->
    match f acc r.stepSize with (acc, stepSize) in
    (acc, RK4 {r with stepSize = stepSize})
  | EF r ->
    match f acc r.stepSize with (acc, stepSize) in
    (acc, EF {r with stepSize = stepSize})
end
