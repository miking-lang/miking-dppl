include "mexpr/info.mc"
include "mexpr/ast.mc"
include "error.mc"
include "seq.mc"
include "set.mc"


lang MethodHelper = MExprAst
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
