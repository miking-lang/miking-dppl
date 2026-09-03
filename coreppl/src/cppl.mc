-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "parser.mc"
include "static-delay.mc"
include "dppl-arg.mc"

-- Backends
include "coreppl-to-mexpr/compile.mc"
include "inference/mcmc.mc"

include "bool.mc"
include "option.mc"
include "string.mc"
include "common.mc"
include "mexpr/ast.mc"
include "mexpr/utils.mc"
include "mexpr/generate-utest.mc"
include "ocaml/mcore.mc"

include "coreppl-to-mexpr/is-lw/compile.mc"
include "coreppl-to-mexpr/smc-bpf/compile.mc"
include "coreppl-to-mexpr/smc-apf/compile.mc"
include "coreppl-to-mexpr/mcmc-naive/compile.mc"
include "coreppl-to-mexpr/mcmc-trace/compile.mc"
include "coreppl-to-mexpr/mcmc-lightweight/compile.mc"
include "coreppl-to-mexpr/pmcmc-pimh/compile.mc"
include "coreppl-to-mexpr/pval-graph/compile.mc"

lang CPPLLang = CorePPLFileTypeLoader
  + MExprAst + UtestLoader + ODELoader + MExprGenerateEq
  + MExprLowerNestedPatterns + MCoreCompileLang
  + PhaseStats + MExprGeneratePprint + GeneratePprintMissingCase
  + BPFCompilerPicker + APFCompilerPicker + ImportanceCompilerPicker
  + NaiveMCMCCompilerPicker + TraceMCMCCompilerPicker + PIMHCompilerPicker
  + LightweightMCMCCompilerPicker
  + SimplePValGraphCompiler
  + UnboundErrorAttr + DefinedAttr + WithoutInfoAttr
  + MLangTypeAlias + MLangSyn + MLangSem + TyUseSym + MExprPatAnalysis
  + IncludeLoader + MCoreKeywordMaker + MCoreLoader
  + MExprDeadcodeElimination + MExprSym + DeclUseSym + RemoveMetaVar
end

mexpr

use CPPLLang in

let options = optParseWithHelp {optParserHelpDef cpplName with description = cpplDescription} options (tail argv) in
-- Read and parse the file
let filename = stdlibResolveFileOr (lam x. error x) "." options.frontend.input in
let isFromModelFileOrDynamic = lam x.
  if x.static
  then match x.info with Info x
    then eqString x.filename filename
    else false
  else true in

let log = mkPhaseLogState options.transformations.debugDumpPhases options.transformations.debugPhases options.transformations.invariantsToCheck in

let loader = mkLoader typcheckEnvDefault
  [ ODEHook ()
  , StripUtestHook ()
  ] in
let loader = addHook loader (CorePPLFileHook {options = options.cpplFiles, method = options.defaultMethod}) in
let loader = enableDefaultInferMethod options.defaultMethod loader in
let loader = enableCPPLCompilation options.transformations loader in
let loader = enableUtestGeneration (if options.frontend.test then isFromModelFileOrDynamic else lam. false) loader in
let loader = enablePprintGeneration loader in
endPhaseStatsProg log "mk-cppl-loader" {decls = getDecls loader, expr = unit_};

let fileMode = switch options.fileMode
  case "main" then CPPLMain ()
  case "ad" then CPPLMainAD ()
  case "implicit-infer" then CPPLMainImplicitInfer ()
  end in

let loader = (includeFileTypeExn (FCorePPL {mode = fileMode}) "." filename loader).1 in
endPhaseStatsProg log "include-file" {decls = getDecls loader, expr = unit_};

let ast = buildFullAst loader in
endPhaseStatsExpr log "build-full-ast" ast;

let ast = removeMetaVarExpr ast in
endPhaseStatsExpr log "remove-meta-var" ast;

let ast = deadcodeElimination ast in
endPhaseStatsExpr log "deadcode-elimination" ast;

let ast = forceLazyExpr ast in
endPhaseStatsExpr log "force-lazy" ast;

let ocamlCompile : [String] -> [String] -> String -> String = lam libs. lam clibs. lam prog.
  let opts =
    { defaultCompileOptions
    with libraries = libs
    , cLibraries = clibs
    } in
  (if options.frontend.outputMl then
    writeFile "program.ml" prog
   else ());
  let res = ocamlCompileWithConfig opts prog in
  sysMoveFile res.binaryPath options.frontend.output;
  sysChmodWriteAccessFile options.frontend.output;
  res.cleanup ();
  options.frontend.output in
let hooks = mkEmptyHooks ocamlCompile in

let ast = lowerAll ast in
endPhaseStatsExpr log "lower-all" ast;

let ast = removeOpaqueExpr ast in
endPhaseStatsExpr log "remove-opaque" ast;

(if options.frontend.printMCore then
  printLn (expr2str ast)
 else ());

if options.frontend.exitBefore then exit 0 else

let res = compileMCore ast hooks in
endPhaseStatsExpr log "compile-mcore" ast;
res
