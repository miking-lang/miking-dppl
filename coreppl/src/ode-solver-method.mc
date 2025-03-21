include "option.mc"
include "stringid.mc"
include "mexpr/info.mc"
include "mexpr/pprint.mc"
include "mexpr/type-check.mc"
include "mexpr/symbolize.mc"
include "mexpr/eq.mc"
include "mexpr/ast-builder.mc"

include "method-helper.mc"
include "dppl-arg.mc"

-- ODE solver method names
let odeDefault = nameSym "Default"
let odeRK4 = nameSym "RK4"
let odeEF = nameSym "EF"
let odeEFA = nameSym "EFA"

-- Defines the basic components required in the implementation of an ODE solver
-- method.
--
-- To define a new ODE solver method constructor, the following semantic
-- functions must be implemented for the constructor.
lang ODESolverMethod = PrettyPrint + TypeCheck + Sym + Eq + MethodHelper

  type MethodArg = { stepSize : Expr, add : Expr, smul : Expr }

  syn ODESolverMethod =
  | ODESolverDefault MethodArg
  | RK4 MethodArg
  | EF MethodArg
  | EFA { n : Expr, stepSize : Expr, add : Expr, smul : Expr }

  -- Map/Accum over expressions in ODE solver method.
  sem smapAccumL_ODESolverMethod_Expr
    : all a. (a -> Expr -> (a, Expr)) -> a -> ODESolverMethod -> (a, ODESolverMethod)
  sem smapAccumL_ODESolverMethod_Expr f acc =
  | ODESolverDefault r ->
    match _smapAccumL_ODESolverMethod_Expr f acc r with (acc, r) in
    (acc, ODESolverDefault r)
  | RK4 r ->
    match _smapAccumL_ODESolverMethod_Expr f acc r with (acc, r) in
    (acc, RK4 r)
  | EF r ->
    match _smapAccumL_ODESolverMethod_Expr f acc r with (acc, r) in
    (acc, EF r)
  | EFA r ->
    match f acc r.n with (acc, n) in
    match
      _smapAccumL_ODESolverMethod_Expr
        f acc { stepSize = r.stepSize, add = r.add, smul = r.smul }
      with (acc, rr)
    in
    (acc, EFA { n = n, stepSize = rr.stepSize, add = rr.add, smul = rr.smul })

  sem _smapAccumL_ODESolverMethod_Expr f acc =| r ->
    match f acc r.stepSize with (acc, stepSize) in
    match f acc r.add with (acc, add) in
    match f acc r.smul with (acc, smul) in
    (acc, { r with stepSize = stepSize, add = add, smul = smul })

  sem sfold_ODESolverMetod_Expr : all a. (a -> Expr -> a) -> a -> ODESolverMethod -> a
  sem sfold_ODESolverMetod_Expr f acc =| m ->
    (smapAccumL_ODESolverMethod_Expr (lam acc. lam tm. (f acc tm, tm)) acc m).0

  -- NOTE(oerikss, 2024-03-08): Compares the inference methods tags only.
  sem cmpODESolverMethod : ODESolverMethod -> ODESolverMethod -> Int
  sem cmpODESolverMethod lhs =
  | rhs -> subi (constructorTag lhs) (constructorTag rhs)

  sem eqODESolverMethod
    : EqEnv -> EqEnv -> ODESolverMethod -> ODESolverMethod -> Option EqEnv
  sem eqODESolverMethod env free =
  | ODESolverDefault r
  | RK4 r
  | EF r -> optionFoldlM (eqExprH env) free [r.stepSize, r.add, r.smul]
  | EFA r -> optionFoldlM (eqExprH env) free [r.n, r.stepSize, r.add, r.smul]

  sem odeSolverMethodName : ODESolverMethod -> Name
  sem odeSolverMethodName =
  | ODESolverDefault _ -> odeDefault
  | RK4 _ -> odeRK4
  | EF _ -> odeEF
  | EFA _ -> odeEFA

  -- Constructs an ODE solver method as a TmConApp.
  sem odeSolverMethodToCon : Info -> ODESolverMethod -> Expr
  sem odeSolverMethodToCon info =| m ->
    (nconapp_ (odeSolverMethodName m) (odeSolverMethodConfig info m))

  -- Constructs an ODE solver method from the arguments of a TmConApp.
  sem odeSolverMethodFromCon : Info -> Map SID Expr -> String -> ODESolverMethod
  sem odeSolverMethodFromCon info bindings =
  | "Default" ->
    ODESolverDefault (_odeSolverMethodFromCon1 info bindings)
  | "RK4" -> RK4 (_odeSolverMethodFromCon1 info bindings)
  | "EF" -> EF (_odeSolverMethodFromCon1 info bindings)
  | "EFA" -> EFA (_odeSolverMethodFromCon2 info bindings)
  | s -> errorSingle [info] (concat "Unknown ODE solver method: " s)

  sem _odeSolverMethodFromCon1 info =| bindings ->
    match getFields info bindings [
      ("stepSize", withInfo info (float_ 0.05)),
      ("add", withInfo info (uconst_ (CAddf ()))),
      ("smul", withInfo info (uconst_ (CMulf ())))
    ] with [stepSize, add, smul]
    then { stepSize = stepSize, add = add, smul = smul }
    else error "impossible"

  sem _odeSolverMethodFromCon2 info =| bindings ->
    match getFields info bindings [
      ("n", withInfo info (int_ 10)),
      ("stepSize", withInfo info (float_ 0.05)),
      ("add", withInfo info (uconst_ (CAddf ()))),
      ("smul", withInfo info (uconst_ (CMulf ())))
    ] with [n, stepSize, add, smul]
    then { n = n, stepSize = stepSize, add = add, smul = smul }
    else error "impossible"

  -- Produces a record expression containing the configuration parameters of the
  -- ODE solver method. This record is passed to the ODE solver runtime
  -- function.
  sem odeSolverMethodConfig : Info -> ODESolverMethod -> Expr
  sem odeSolverMethodConfig info =
  | ODESolverDefault r
  | RK4 r
  | EF r -> fieldsToRecord info [
    ("stepSize", r.stepSize),
    ("add", r.add),
    ("smul", r.smul)
  ]
  | EFA r -> fieldsToRecord info [
    ("n", r.n),
    ("stepSize", r.stepSize),
    ("add", r.add),
    ("smul", r.smul)
  ]

  -- Type checks the ODE solver method. This ensures that the provided arguments
  -- have the correct types.
  sem typeCheckODESolverMethod
    : TCEnv -> Info -> Type -> ODESolverMethod -> ODESolverMethod
  sem typeCheckODESolverMethod env info tyState =
  | ODESolverDefault r ->
    ODESolverDefault (_typeCheckODESolverMethod env info tyState r)
  | RK4 r -> RK4 (_typeCheckODESolverMethod env info tyState r)
  | EF r -> EF (_typeCheckODESolverMethod env info tyState r)
  | EFA r ->
    let n = typeCheckExpr env r.n in
    unify env [info, infoTm n] (TyInt {info = info}) (tyTm n);
    let rr = _typeCheckODESolverMethod env info tyState {
      stepSize = r.stepSize, add = r.add, smul = r.smul
    } in
    EFA { r with n = n, stepSize = rr.stepSize, add = rr.add, smul = rr.smul }

  sem _typeCheckODESolverMethod env info tyState =| r ->
    let float = TyFloat {info = info} in
    let arrow = ityarrow_ info in
    let stepSize = typeCheckExpr env r.stepSize in
    let add = typeCheckExpr env r.add in
    let smul = typeCheckExpr env r.smul in
    unify env [info, infoTm stepSize] float (tyTm stepSize);
    unify env [info, infoTm add]
      (arrow tyState (arrow tyState tyState)) (tyTm add);
    unify env [info, infoTm smul]
      (arrow float (arrow tyState tyState)) (tyTm smul);
    { stepSize = stepSize, add = add, smul = smul }
end
