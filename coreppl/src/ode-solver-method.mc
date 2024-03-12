include "option.mc"
include "stringid.mc"
include "mexpr/info.mc"
include "mexpr/pprint.mc"
include "mexpr/type-check.mc"
include "mexpr/symbolize.mc"

include "method-helper.mc"
include "dppl-arg.mc"

-- Defines the basic components required in the implementation of an ODE solver
-- method.
--
-- To define a new ODE solver method constructor, the following semantic
-- functions must be implemented for the constructor.
lang ODESolverMethodBase = PrettyPrint + TypeCheck + Sym + MethodHelper
  syn ODESolverMethod =
  | Default { stepSize: Expr }

  -- NOTE(oerikss, 2024-03-08): Compares the inference methods tags only.
  sem cmpODESolverMethod : ODESolverMethod -> ODESolverMethod -> Int
  sem cmpODESolverMethod lhs =
  | rhs -> subi (constructorTag lhs) (constructorTag rhs)

  sem eqODESolverMethod : ODESolverMethod -> ODESolverMethod -> Bool
  sem eqODESolverMethod lhs =
  | rhs -> eqi (cmpODESolverMethod lhs rhs) 0

  sem pprintODESolverMethod
    : Int -> PprintEnv -> ODESolverMethod -> (PprintEnv, String)
  sem pprintODESolverMethod indent env =
  | Default { stepSize = stepSize } ->
    let i = pprintIncr indent in
    match pprintCode i env stepSize with (env, stepSize) in
    (env, join ["(Default {stepSize = ", stepSize, "})"])

  -- Constructs an ODE solver method from the arguments of a TmConApp.
  sem inferMethodFromCon : Info -> Map SID Expr -> String -> ODESolverMethod
  sem inferMethodFromCon info bindings =
  | "Default" ->
    let expectedFields = [
      ("stepSize", float_ default.stepSize)
    ] in
    match getFields info bindings expectedFields with [stepSize] in
    Default { stepSize = stepSize }
  | s -> errorSingle [info] (concat "Unknown ODE solver method: " s)

  -- Produces a record expression containing the configuration parameters of the
  -- ODE solver method. This record is passed to the inference runtime function.
  sem odeSolverMethodConfig : Info -> ODESolverMethod -> Expr
  sem odeSolverMethodConfig info =
  | Default { stepSize = stepSize } -> fieldsToRecord info [("stepSize", stepSize)]

  -- Type checks the ODE solver method. This ensures that the provided arguments
  -- have the correct types.
  sem typeCheckODESolverMethod : TCEnv -> Info -> ODESolverMethod -> ODESolverMethod
  sem typeCheckODESolverMethod env info =
  | Default { stepSize = stepSize } ->
    let float = TyFloat {info = info} in
    let stepSize = typeCheckExpr env stepSize in
    unify env [info, infoTm stepSize] float (tyTm stepSize);
    Default { stepSize = stepSize }

  -- Symbolizes infer methods.
  sem symbolizeODESolverMethod : SymEnv -> ODESolverMethod -> ODESolverMethod
  sem symbolizeODESolverMethod env =
  | Default r -> Default {r with stepSize = symbolizeExpr env r.stepSize}
end
