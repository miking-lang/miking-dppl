include "option.mc"
include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"

lang InferParser = MExprAst
  type InferParserEnv = {
    method : String,
    args : Map SID Expr,
    info : Info
  }

  sem mexprCharLiteralToChar : Expr -> Option Char
  sem mexprCharLiteralToChar =
  | TmConst {val = CChar {val = c}} -> Some c
  | _ -> None ()

  sem mexprStringLiteralToString : Expr -> Option String
  sem mexprStringLiteralToString =
  | TmSeq {tms = chars} -> optionMapM mexprCharLiteralToChar chars
  | _ -> None ()

  sem mexprIntLiteralToInt : Expr -> Option Int
  sem mexprIntLiteralToInt =
  | TmConst {val = CInt {val = i}} -> Some i
  | _ -> None ()

  sem _parseFail : all a. InferParserEnv -> String -> a
  sem _parseFail env =
  | fieldStr ->
    let msg = join [
      "Could not parse required field ", fieldStr, " from configuration for ",
      "infer using method ", env.method
    ] in
    errorSingle [env.info] msg

  sem parseOptionalFieldString : InferParserEnv -> String -> Option String
  sem parseOptionalFieldString env =
  | fieldStr ->
    let fieldSid = stringToSid fieldStr in
    optionJoin
      (optionMap mexprStringLiteralToString (mapLookup fieldSid env.args))

  sem parseRequiredFieldString : InferParserEnv -> String -> String
  sem parseRequiredFieldString env =
  | fieldStr ->
    optionGetOrElse
      (lam. _parseFail env fieldStr)
      (parseOptionalFieldString env fieldStr)

  sem parseOptionalFieldInt : InferParserEnv -> String -> Option Int
  sem parseOptionalFieldInt env =
  | fieldStr ->
    let fieldSid = stringToSid fieldStr in
    optionJoin
      (optionMap mexprIntLiteralToInt (mapLookup fieldSid env.args))

  sem parseRequiredFieldInt : InferParserEnv -> String -> Int
  sem parseRequiredFieldInt env =
  | fieldStr ->
    optionGetOrElse
      (lam. _parseFail env fieldStr)
      (parseOptionalFieldInt env fieldStr)

  sem parseInferMethod : Expr -> InferMethod
  sem parseInferMethod =
  | TmRecord t ->
    let env = {method = "", args = t.bindings, info = t.info} in
    match parseOptionalFieldString env "method" with Some methodStr then
      let env = {env with method = methodStr} in
      parseInferMethodH env methodStr
    else errorSingle [t.info] "Method must be provided in infer configuration"
  | t -> errorSingle [infoTm t] "Expected infer configuration as record literal"

  -- This function should be implemented for each supported inference method.
  sem parseInferMethodH : InferParserEnv -> String -> InferMethod
  sem parseInferMethodH env =
  | s -> errorSingle [env.info] (concat "Unknown inference method: " s)
end

mexpr

use InferParser in

utest mexprCharLiteralToChar (char_ 'a') with Some 'a' using optionEq eqc in
utest mexprCharLiteralToChar (str_ "ab") with None () using optionEq eqString in
utest mexprCharLiteralToChar (int_ 4) with None () using optionEq eqi in

utest mexprStringLiteralToString (str_ "abc") with Some "abc" using optionEq eqString in
utest mexprStringLiteralToString (seq_ [char_ 'a', int_ 3]) with None () using optionEq eqString in
utest mexprStringLiteralToString (char_ 'a') with None () using optionEq eqString in
utest mexprStringLiteralToString (seq_ [char_ 'a', char_ 'x']) with Some "ax" using optionEq eqString in

utest mexprIntLiteralToInt (int_ 3) with Some 3 using optionEq eqi in
utest mexprIntLiteralToInt (char_ 'a') with None () using optionEq eqc in
utest mexprIntLiteralToInt (addi_ (int_ 2) (int_ 3)) with None () using optionEq eqi in

()
