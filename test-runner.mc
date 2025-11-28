include "coreppl::coreppl-to-mexpr/pval-graph/state-transformation.mc"

let _idx0 = stringToSid "0"
let _idx1 = stringToSid "1"

lang TestLang = DPPLParser + MExprLowerNestedPatterns + MExprConstantFold + InlineSingleUse + PValStateTransformation
end

mexpr

use TestLang in

let args =
  { _defaultBootParserParseMCoreFileArg ()
  with eliminateDeadCode = false
  , allowFree = true
  , keywords = pplKeywords
  , builtin = cpplBuiltin
  } in
let ast = parseMCoreFile args "test.mc" in
let ast = use DPPLParser in makeKeywords ast in
let ast = symbolizeExpr symEnvDefault ast in
let ast = typeCheck ast in
let ast = lowerAll ast in
let ast = inlineSingleUseLets ast in
let initState =
  { specializations = mapEmpty nameCmp
  } in
let initScope =
  { functionDefinitions = mapEmpty nameCmp
  , valueScope = mapEmpty nameCmp
  , conScope = mapEmpty nameCmp
  } in
match specializeExpr initScope initState ast with (_, (ast, _)) in
printLn (pprintCode 0 pprintEnvEmpty ast).1;
printLn "==========================";
let initTransEnv =
  { currStateName = nameSym "st"
  , functions = mapEmpty nameCmp
  , p_pure = nameSym "p_pure"
  , p_map = nameSym "p_map"
  , p_apply = nameSym "p_apply"
  , p_bind = nameSym "p_bind"
  , p_assume = nameSym "p_assume"
  , p_select = nameSym "p_select"
  , p_weight = nameSym "p_weight"
  , p_export = nameSym "p_export"
  , p_traverseSeq = nameSym "p_traverseSeq"
  } in
match pvalTrans initTransEnv ast with (wrap, tm) in
printLn (pprintCode 0 pprintEnvEmpty (wrap tm)).1;

()
