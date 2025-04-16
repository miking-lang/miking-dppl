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

lang CPPLLang = CorePPLFileTypeLoader
  + MExprAst + UtestLoader + ODELoader + MExprGenerateEq
  + MExprLowerNestedPatterns + MCoreCompileLang
  + PhaseStats + MExprGeneratePprint
  + BPFCompilerPicker + APFCompilerPicker + ImportanceCompilerPicker
  + NaiveMCMCCompilerPicker + TraceMCMCCompilerPicker + PIMHCompilerPicker
  + LightweightMCMCCompilerPicker
end

mexpr

use CPPLLang in

let options = optParseWithHelp cpplName cpplDescription options (tail argv) in
-- Read and parse the file
let filename = stdlibResolveFileOr (lam x. error x) "." options.frontend.input in
let isFromModelFileOrStatic = lam x.
  if x.static then true else
  match x.info with Info x
  then eqString x.filename filename
  else false in

let log = mkPhaseLogState options.transformations.debugDumpPhases options.transformations.debugPhases in

let loader = mkLoader symEnvDefault typcheckEnvDefault
  [ ODEHook ()
  , StripUtestHook ()
  ] in
let loader = addHook loader (CorePPLFileHook {options = options.cpplFiles, method = options.defaultMethod}) in
let loader = enableDefaultInferMethod options.defaultMethod loader in
let loader = enableCPPLCompilation options.transformations loader in
let loader = enableUtestGeneration (if options.frontend.test then isFromModelFileOrStatic else lam. false) loader in
let loader = enablePprintGeneration loader in
endPhaseStatsExpr log "mk-cppl-loader" unit_;

let loader = (includeFileTypeExn (FCorePPL {isModel = true}) "." filename loader).1 in
endPhaseStatsExpr log "include-file" unit_;

let ast = buildFullAst loader in
endPhaseStatsExpr log "build-full-ast" ast;

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

if options.frontend.exitBefore then exit 0 else

let res = compileMCore ast hooks in
endPhaseStatsExpr log "compile-mcore" ast;
res
