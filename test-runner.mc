include "coreppl::coreppl-to-mexpr/pval-graph/remove-second-class-lambdas.mc"
include "coreppl::coreppl-to-mexpr/pval-graph/idealized-transformation.mc"
include "coreppl::coreppl-to-mexpr/pval-graph/state-transformation.mc"

lang TestLang = DPPLParser + MExprLowerNestedPatterns + MExprLambdaLift + InlineSingleUse + RemoveSecondClassFunctions + IdealizedPValTransformation + PValStateTransformation
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
let ast = liftLambdas ast in
let initEnv =
  { depth = 0
  , bindings = mapEmpty nameCmp
  } in
let initState =
  let cmpF = lam l. lam r.
    let res = eitherCmp cmpConst nameCmp l.fName r.fName in
    if neqi 0 res then res else
    subi l.argCount r.argCount in
  let cmpGen = lam l. lam r.
    let res = nameCmp l.fName r.fName in
    if neqi 0 res then res else
    seqCmp (optionCmp cmpF) l.args r.args in
  { generated = mapEmpty cmpGen
  , toInsert = mapEmpty subi
  , pendingRecursiveDefinitions = []
  , currMaxDepthUsed = pureRMaxInt 0
  , depths = mapEmpty nameCmp
  } in
match remSecLamExpr initEnv initState ast with (_, ast) in
printLn (pprintCode 0 pprintEnvEmpty ast).1;

()
