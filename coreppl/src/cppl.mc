-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "parser.mc"
include "static-delay.mc"
include "dppl-arg.mc"
include "build.mc"
include "src-location.mc"

-- Backends
include "coreppl-to-mexpr/compile.mc"
include "coreppl-to-rootppl/compile.mc"

include "bool.mc"
include "option.mc"
include "string.mc"
include "common.mc"
include "mexpr/ast.mc"
include "mexpr/duplicate-code-elimination.mc"
include "mexpr/utils.mc"
include "mexpr/generate-utest.mc"
include "ocaml/mcore.mc"

lang CPPLLang = CorePPLFileTypeLoader
  + MExprAst + UtestLoader + ODELoader + MExprGenerateEq
  + MExprLowerNestedPatterns + MCoreCompileLang
  + PhaseStats + MExprGeneratePprint

  -- Check if a CorePPL program uses infer
  sem hasInfer =
  | expr -> hasInferH false expr

  sem hasInferH acc =
  | TmInfer _ -> true
  | expr -> sfold_Expr_Expr hasInferH acc expr

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

    -- if eqString options.target "rootppl" then

    --   let ast = parseMCorePPLFile options.test filename in

    --   let noInfer = not (hasInfer ast) in

    --   ---------------------
    --   -- RootPPL backend --
    --   ---------------------

    --   -- Handle the RootPPL backend in the old way, without using infers.
    --   if noInfer then
    --     let ast =
    --       if options.staticDelay then staticDelay ast
    --       else ast
    --     in
    --     let ast = rootPPLCompile options ast in
    --     buildRootPPL options ast
    --   else error "Use of infer is not supported by RootPPL backend"

    -- else

      --------------------
      -- Miking backend --
      --------------------
      let log = mkPhaseLogState options.debugDumpPhases options.debugPhases in

      let res = mkCPPLLoader [
        StripUtestHook (),
        ODEHook { options = options }
      ] options
      in
      let loader = enableUtestGeneration res.loader in
      let loader = enablePprintGeneration loader in
      endPhaseStatsExpr log "mk-cppl-loader" unit_;

      let loader = (includeFileTypeExn (FCorePPL {isModel = true}) "." filename loader).1 in
      let loader = insertUtestExitCheck loader in
      endPhaseStatsExpr log "include-file" unit_;

      let ast = extractAsMExprExn options res.envs (deref res.runtimes) loader in
      endPhaseStatsExpr log "extract-as-mexpr" ast;

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
