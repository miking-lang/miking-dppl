include "option.mc"
include "stringid.mc"
include "mexpr/ast.mc"
include "mexpr/info.mc"
include "mexpr/pprint.mc"
include "mexpr/type-check.mc"

include "dppl-arg.mc"

lang InferMethodHelper = MExprAst
  -- Extracts the fields corresponding to the provided sequence of strings, and
  -- ensures that no additional fields have been defined.
  sem getFields : Info -> Map SID Expr -> [(String, Expr)] -> [Expr]
  sem getFields info bindings =
  | fields ->
    -- Construct a record containing the default values for all fields that may
    -- occur.
    match unzip fields with (fieldStrs, defaults) in
    let defaultRecord =
      mapFromSeq cmpSID (zip (map stringToSid fieldStrs) defaults) in

    -- If the user-provided record defines fields that are not defined in the
    -- default record, it means they are trying to set options that are not
    -- supported.
    let mapToSet = lam m. mapMapWithKey (lam. lam. ()) m in
    let unknownFields = setSubtract (mapToSet bindings) (mapToSet defaultRecord) in
    if mapIsEmpty unknownFields then

      -- Update the record by replacing the default values with values provided
      -- by the user.
      let record =
        mapFoldWithKey
          (lam record. lam sid. lam expr.
            if mapMem sid record then mapInsert sid expr record
            else error "Unexpected field: " (sidToString sid))
          defaultRecord bindings in

      -- Return the values after updating according to the user-provided
      -- values, in the same order as they were provided as input. The use of
      -- filterOption will never result in an error as all fieldStrs are part
      -- of the default record.
      filterOption (map (lam s. mapLookup (stringToSid s) record) fieldStrs)

    else
      let msg = join
        [ "Unexpected fields: "
        , strJoin "," (map sidToString (setToSeq unknownFields)), "\n" ] in
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
  | Default {}

  -- NOTE(larshum, 2022-10-11): Compares the inference methods tags only.
  sem cmpInferMethod : InferMethod -> InferMethod -> Int
  sem cmpInferMethod lhs =
  | rhs -> subi (constructorTag lhs) (constructorTag rhs)

  sem eqInferMethod : InferMethod -> InferMethod -> Bool
  sem eqInferMethod lhs =
  | rhs -> eqi (cmpInferMethod lhs rhs) 0

  sem pprintInferMethod : Int -> PprintEnv -> InferMethod -> (PprintEnv, String)
  sem pprintInferMethod indent env =
  | Default {} -> (env, join ["Default {}"])

  -- Constructs an inference method from the arguments of a TmConApp.
  sem inferMethodFromCon : Info -> Map SID Expr -> String -> InferMethod
  sem inferMethodFromCon info bindings =
  | "Default" -> Default {}
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
  | Default {} -> fieldsToRecord info []

  -- Type checks the inference method. This ensures that the provided arguments
  -- have the correct types.
  sem typeCheckInferMethod : TCEnv -> Info -> InferMethod -> InferMethod
  sem typeCheckInferMethod env info =
  | Default {} -> Default {}

  -- Symbolizes infer methods.
  sem symbolizeInferMethod : SymEnv -> InferMethod -> InferMethod
  sem symbolizeInferMethod env =
  | Default {} -> Default {}

  -- Overrides the number of runs/iterations/particles in the InferMethod
  sem setRuns : Expr -> InferMethod -> InferMethod
end
