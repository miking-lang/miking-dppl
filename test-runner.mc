include "mexpr/deadcode.mc"
include "coreppl::coreppl-to-mexpr/pval-graph/remove-second-class-lambdas.mc"
include "coreppl::coreppl-to-mexpr/pval-graph/idealized-transformation.mc"
include "coreppl::coreppl-to-mexpr/pval-graph/state-transformation.mc"

lang TestLang = DPPLParser + MExprLowerNestedPatterns + MExprLambdaLift + InlineSingleUse + RemoveSecondClassFunctions + IdealizedPValTransformation + PValStateTransformation + MExprDeadcodeElimination
  sem tmHasSideEffect nmap acc =
  | (TmWeight _ | TmObserve _) -> true
end

lang TyAnnotFull = DPPLParser + MExprPrettyPrint + PprintTyAnnot + HtmlAnnotator + MetaVarTypePrettyPrint
end

mexpr

use TestLang in

let debug = false in

let pprintLn =
  if debug then
    lam env. lam label. lam tm.
      match pprintCode 0 env tm with (env, tm) in
      printLn (join ["=== ", label, " ===\n"]);
      printLn tm;
      printLn "\n\n";
      env
  else lam env. lam. lam. env in

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
let ast = deadcodeElimination ast in
let ast = liftLambdas ast in
let pprintEnv = pprintEnvEmpty in
let pprintEnv = pprintLn pprintEnv "lam-lift" ast in
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
-- printLn (use TyAnnotFull in pprintAst ast);
let pprintEnv = pprintLn pprintEnv "remove second-class lambdas" ast in
let ast = typeCheck ast in  -- NOTE(vipa, 2025-12-02): This is
                            -- regrettable, but it's kinda hard to
                            -- substitute in correct types inside
                            -- polymorphic functions that have been
                            -- specialized. We want something better
                            -- here later.
let initState =
  { specializations = mapEmpty nameCmp
  } in
let initScope =
  { functionDefinitions = mapEmpty nameCmp
  , depth = 0
  , valueScope = mapEmpty nameCmp
  , conScope = mapEmpty nameCmp
  } in
match specializeExpr initScope initState ast with (_, (ast, _)) in
let pprintEnv = pprintLn pprintEnv "idealized transformation" ast in
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
  , mapAccumL = nameSym "mapAccumL"
  , storeAssume = nameSym "simpleStoreAssume"
  , storeSubmodel = nameSym "simpleStoreSubmodel"
  , storeExport = nameSym "simpleStoreExport"
  , storeWeight = nameSym "simpleStoreWeight"
  , initSubmodel = app_ (nvar_ (nameSym "simpleInit")) unit_
  } in
match pvalTrans initTransEnv ast with tm in
let pprintEnv = pprintLn pprintEnv "state transformation" tm in

(if debug then () else
  printLn (expr2str tm));

()
