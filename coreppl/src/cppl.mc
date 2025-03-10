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

lang CPPLLang = CorePPLFileTypeLoader
  + MExprAst + UtestLoader + ODELoader + MExprGenerateEq
  + AutoDriftKernelHook
  + MExprLowerNestedPatterns + MCoreCompileLang
  + PhaseStats + MExprGeneratePprint
end

mexpr

use CPPLLang in

match optParseWithHelp cpplName cpplDescription backcompatOptions (tail argv)
  with (filename, options) in
-- Read and parse the file
let filename = stdlibResolveFileOr (lam x. error x) "." filename in
let isFromModelFileOrStatic = lam x.
  if x.static then true else
  match x.info with Info x
  then eqString x.filename filename
  else false in

let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in

let loader = mkLoader symEnvDefault typcheckEnvDefault
  [ ODEHook {options = options}
  , StripUtestHook ()
  ] in
let loader = if options.driftKernel
  then addHook loader (AutoDriftKernelHook {driftScale = options.driftScale})
  else loader in
let loader = enableCPPLCompilation options loader in
let loader = enableUtestGeneration (if options.test then isFromModelFileOrStatic else lam. false) loader in
let loader = enablePprintGeneration loader in
endPhaseStatsExpr log "mk-cppl-loader" unit_;

let loader = (includeFileTypeExn (FCorePPL {isModel = true, dpplTypeCheck = options.dpplTypeCheck}) "." filename loader).1 in
endPhaseStatsExpr log "include-file" unit_;

let ast = buildFullAst loader in
endPhaseStatsExpr log "build-full-ast" ast;

let ocamlCompile : [String] -> [String] -> String -> String = lam libs. lam clibs. lam prog.
  let opts =
    { defaultCompileOptions
    with libraries = libs
    , cLibraries = clibs
    } in
  (if options.outputMl then
    writeFile "program.ml" prog
   else ());
  let res = ocamlCompileWithConfig opts prog in
  sysMoveFile res.binaryPath options.output;
  sysChmodWriteAccessFile options.output;
  res.cleanup ();
  options.output in
let hooks = mkEmptyHooks ocamlCompile in

let ast = lowerAll ast in
endPhaseStatsExpr log "lower-all" ast;

if options.exitBefore then exit 0 else

let res = compileMCore ast hooks in
endPhaseStatsExpr log "compile-mcore" ast;
res
