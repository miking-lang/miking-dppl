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

lang CPPLLang =
  MExprAst + MExprCompile + TransformDist + MExprEliminateDuplicateCode +
  MExprSubstitute + MExprPPL

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
    let ast = parseMCorePPLFile options.test filename in

    let noInfer = not (hasInfer ast) in

    if eqString options.target "rootppl" then

      ---------------------
      -- RootPPL backend --
      ---------------------

      -- Handle the RootPPL backend in the old way, without using infers.
      if noInfer then
        let ast =
          if options.staticDelay then staticDelay ast
          else ast
        in
        let ast = rootPPLCompile options ast in
        buildRootPPL options ast
      else error "Use of infer is not supported by RootPPL backend"

    else

      --------------------
      -- Miking backend --
      --------------------

      -- Compile the CorePPL AST using the provided options
      let ast = mexprCpplCompile options noInfer ast in

      -- Exit before producing the output files, if the flag is set
      if options.exitBefore then exit 0
      else buildMExpr options ast

else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
