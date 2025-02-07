-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "parser.mc"
include "static-delay.mc"
include "dppl-arg.mc"

-- Backends
include "coreppl-to-mexpr/compile.mc"

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
  + MExprLowerNestedPatterns + MCoreCompileLang
  + PhaseStats + MExprGeneratePprint
end

mexpr

use CPPLLang in

-- Use the arg.mc library to parse arguments
let result = argParse default config in
match result with ParseOK r then
  let options: Options = r.options in
  -- Print menu if not exactly one file argument
  if neqi (length r.strings) 1 then
    print (menu ());
    if gti (length r.strings) 1 then exit 1 else exit 0
  else

    -- Read and parse the file
    let filename = head r.strings in

    let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in

    let loader = mkLoader symEnvDefault typcheckEnvDefault
      [ ODEHook {options = options}
      , StripUtestHook ()
      ] in
    let loader = enableCPPLCompilation options loader in
    let loader = enableUtestGeneration loader in
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
      (if options.outputMc then
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

else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
