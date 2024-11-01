include "option.mc"
include "stringid.mc"
include "mexpr/info.mc"
include "mexpr/pprint.mc"
include "mexpr/type-check.mc"

include "method-helper.mc"
include "dppl-arg.mc"

-- Defines the basic components required in the implementation of an inference
-- method.
--
-- To define a new inference method constructor, the following semantic
-- functions must be implemented for the constructor:
-- 1. pprintInferMethod
-- 2. inferMethodFromCon
-- 3. inferMethodFromOptions
-- 4. inferMethodConfig
-- 5. typeCheckInferMethod
-- 6. inferSmapAccumL
lang InferMethodBase =
  PrettyPrint + TypeCheck + Sym + MethodHelper

  syn InferMethod =
  | Default { runs: Expr }

  -- NOTE(larshum, 2022-10-11): Compares the inference methods tags only.
  sem cmpInferMethod : InferMethod -> InferMethod -> Int
  sem cmpInferMethod lhs =
  | rhs -> subi (constructorTag lhs) (constructorTag rhs)

  sem eqInferMethod : InferMethod -> InferMethod -> Bool
  sem eqInferMethod lhs =
  | rhs -> eqi (cmpInferMethod lhs rhs) 0

  sem pprintInferMethod : Int -> PprintEnv -> InferMethod -> (PprintEnv, String)
  sem pprintInferMethod indent env =
  | Default { runs = runs } ->
    let i = pprintIncr indent in
    match pprintCode i env runs with (env, runs) in
    (env, join ["(Default {runs = ", runs, "})"])

  -- Constructs an inference method from the arguments of a TmConApp.
  sem inferMethodFromCon : Info -> Map SID Expr -> String -> InferMethod
  sem inferMethodFromCon info bindings =
  | "Default" ->
    let expectedFields = [
      ("runs", int_ default.particles)
    ] in
    match getFields info bindings expectedFields with [runs] in
    Default { runs = runs }
  | s -> errorSingle [info] (concat "Unknown inference method: " s)

  -- Constructs an inference method from command-line options.
  sem inferMethodFromOptions : Options -> String -> InferMethod
  sem inferMethodFromOptions options =
  | s -> error (concat "Unknown inference method string: " s)

  -- Produces a record expression containing the configuration parameters of
  -- the inference method. This record is passed to the inference runtime
  -- function.
  sem inferMethodConfig : Info -> InferMethod -> Expr
  sem inferMethodConfig info =
  | Default { runs = runs } -> fieldsToRecord info [("runs", runs)]

  -- Produces the expected type of `inferMethodConfig`
  sem inferMethodConfigType : Info -> InferMethod -> Type
  sem inferMethodConfigType info =
  | Default _ -> tyRecord info [("runs", ityint_ info)]

  -- Type checks the inference method. This ensures that the provided arguments
  -- have the correct types.
  sem typeCheckInferMethod : TCEnv -> Info -> InferMethod -> InferMethod
  sem typeCheckInferMethod env info =
  | Default { runs = runs } ->
    let int = TyInt {info = info} in
    let runs = typeCheckExpr env runs in
    unify env [info, infoTm runs] int (tyTm runs);
    Default { runs = runs }

  -- Overrides the number of runs/iterations/particles in the InferMethod
  sem setRuns : Expr -> InferMethod -> InferMethod

  -- Map/Accum over expressions in inference method
  sem inferSmapAccumL_Expr_Expr
    : all a. (a -> Expr -> (a, Expr)) -> a -> InferMethod -> (a, InferMethod)
  sem inferSmapAccumL_Expr_Expr f acc =
  | Default r ->
    match f acc r.runs with (acc, runs) in (acc, Default {r with runs = runs})
  | m -> printLn (pprintInferMethod 0 pprintEnvEmpty m).1; error "fail"

  sem inferSmap_Expr_Expr : (Expr -> Expr) -> InferMethod -> InferMethod
  sem inferSmap_Expr_Expr f =| m ->
    (inferSmapAccumL_Expr_Expr (lam. lam tm. ((), f tm)) () m).1

  sem inferSfold_Expr_Expr : all a. (a -> Expr -> a) -> a -> InferMethod -> a
  sem inferSfold_Expr_Expr f acc =| m ->
    (inferSmapAccumL_Expr_Expr (lam acc. lam tm. (f acc tm, tm)) acc m).0
end
