include "option.mc"
include "stringid.mc"
include "mexpr/info.mc"
include "mexpr/pprint.mc"
include "mexpr/type-check.mc"

include "dppl-arg.mc"

lang InferMethodHelper
  sem getRequiredField : Info -> Map SID Expr -> String -> Expr
  sem getRequiredField info bindings =
  | fieldStr ->
    optionGetOrElse
      (lam.
        errorSingle [info]
          (concat "Missing configuration parameter: " fieldStr))
      (mapLookup (stringToSid fieldStr) bindings)
end

-- Defines the basic components required in the implementation of an inference
-- method.
--
-- To define a new inference method constructor, one must implement the
-- following semantic functions for handling that constructor:
-- 1. pprintInferMethod
-- 2. inferMethodFromCon
-- 3. inferMethodFromOptions
-- 4. typeCheckInferMethod
lang InferMethodBase = PrettyPrint + TypeCheck + InferMethodHelper
  syn InferMethod =

  -- NOTE(larshum, 2022-10-11): Compares the inference methods tags only.
  sem cmpInferMethod : InferMethod -> InferMethod -> Int
  sem cmpInferMethod lhs =
  | rhs -> subi (constructorTag lhs) (constructorTag rhs)

  sem eqInferMethod : InferMethod -> InferMethod -> Bool
  sem eqInferMethod lhs =
  | rhs -> eqi (cmpInferMethod lhs rhs) 0

  sem pprintInferMethod : Int -> PprintEnv -> InferMethod -> (PprintEnv, String)

  -- Constructs an inference method from the arguments of a TmConApp.
  sem inferMethodFromCon : Info -> Map SID Expr -> String -> InferMethod

  -- Constructs an inference method from command-line options.
  sem inferMethodFromOptions : Options -> String -> InferMethod

  -- Type checks the inference method. This ensures that the provided arguments
  -- have the correct types.
  sem typeCheckInferMethod : TCEnv -> Info -> InferMethod -> InferMethod
end
