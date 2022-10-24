include "option.mc"
include "stringid.mc"
include "mexpr/ast.mc"
include "mexpr/info.mc"
include "mexpr/pprint.mc"
include "mexpr/type-check.mc"

include "dppl-arg.mc"

lang InferMethodHelper = MExprAst
  -- Accesses a required field from the bindings, and fails if it is not
  -- present in the provided bindings.
  sem getRequiredField : Info -> Map SID Expr -> String -> Expr
  sem getRequiredField info bindings =
  | fieldStr ->
    optionGetOrElse
      (lam.
        errorSingle [info]
          (concat "Missing configuration parameter: " fieldStr))
      (mapLookup (stringToSid fieldStr) bindings)

  -- Extracts the fields corresponding to the provided sequence of strings, and
  -- ensures that no additional fields have been defined.
  sem getFields : Info -> Map SID Expr -> [String] -> [Expr]
  sem getFields info bindings =
  | fieldStrs ->
    let fieldSids = setOfSeq cmpSID (map stringToSid fieldStrs) in
    let bindingSids = mapMapWithKey (lam. lam. ()) bindings in
    -- NOTE(larshum, 2022-10-12): If the provided fields are exactly the ones
    -- we expect.
    if setEq fieldSids bindingSids then
      map (getRequiredField info bindings) fieldStrs
    else
      -- NOTE(larshum, 2022-10-12): We expected to find the provided fields.
      -- Any fields not there, but in the bindings are missing, while those
      -- that are found only in the provided bindings are unknown (not
      -- declared).
      let printFields = lam msgPrefix. lam set.
        if setIsEmpty set then ""
        else
          join [msgPrefix, strJoin "," (map sidToString (setToSeq set)), "\n"]
      in
      let missingFields = setSubtract fieldSids bindingSids in
      let unknownFields = setSubtract bindingSids fieldSids in
      let msg = join [
        "Invalid inference method configuration parameter.\n",
        printFields "Missing fields: " missingFields,
        printFields "Unexpected fields: " unknownFields
      ] in
      errorSingle [info] msg

  -- Converts the provided sequence of string-expression tuples to a record
  -- expression, which is later passed to the runtime.
  sem fieldsToRecord : Info -> [(String, Expr)] -> Expr
  sem fieldsToRecord info =
  | fields ->
    let bindings =
      mapFromSeq cmpSID
        (map (lam f. match f with (str, e) in (stringToSid str, e)) fields) in
    TmRecord {bindings = bindings, ty = TyUnknown {info = info}, info = info}
end

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

  -- Produces a record expression containing the configuration parameters of
  -- the inference method. This record is passed to the inference runtime
  -- function.
  sem inferMethodConfig : Info -> InferMethod -> Expr

  -- Type checks the inference method. This ensures that the provided arguments
  -- have the correct types.
  sem typeCheckInferMethod : TCEnv -> Info -> InferMethod -> InferMethod
end
