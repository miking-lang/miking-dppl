-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "parser.mc"
include "transformation.mc"
include "dppl-arg.mc"
include "extract.mc"
include "build.mc"
include "src-location.mc"
include "coreppl-to-mexpr/backcompat.mc"
include "coreppl-to-mexpr/compile.mc"
include "coreppl-to-rootppl/compile.mc"

include "option.mc"
include "string.mc"
include "common.mc"
include "mexpr/ast.mc"
include "mexpr/duplicate-code-elimination.mc"
include "mexpr/utils.mc"

lang CPPLLang =
  MExprAst + DPPLExtract + MExprCompile + TransformDist +
  MExprEliminateDuplicateCode + MExprSubstitute + CPPLBackcompat
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
    exit 0
  else
    -- Read and parse the file
    let filename = head r.strings in
    let ast = parseMCorePPLFile filename in

    -- Load the runtimes used in the provided AST, and collect identifiers of
    -- common methods within the runtimes.
    let runtimes = loadRuntimes options ast in

    -- Handle the RootPPL backend in the old way, without using infers.
    if eqString options.method "rootppl-smc" then
      if mapIsEmpty runtimes then
        let ast =
          if options.transform then transform ast
          else ast
        in
        let ast = rootPPLCompile options ast in
        buildRootPPL options ast
      else error "Use of infer is not supported by RootPPL backend"
    else

    -- If no runtimes are found, it means there are no uses of 'infer' in the
    -- program. In this case, the entire AST is the model code, so we transform
    -- it as:
    --
    -- let d = infer <method> (lam. <model>) in
    -- let printRes = ... in
    -- printRes <pp> d
    --
    -- where <method> = inference method chosen according to options
    --       <model> = the entire AST
    --       <pp> = the pretty-print function used to print the result
    match
      if mapIsEmpty runtimes then programModelTransform options ast
      else (runtimes, ast)
    with (runtimes, ast) in

    -- Combine the required runtime ASTs to one AST and eliminate duplicate
    -- definitions due to files having common dependencies. The result is an
    -- updated map of runtime entries, a combined runtime AST and a
    -- symbolization environment.
    let runtimeData = combineRuntimes options runtimes in

    -- Compile the CorePPL AST using the provided options and runtime data.
    let ast = mexprCompile options runtimeData ast in

    -- Exit before producing the output files, if the flag is set
    if options.exitBefore then exit 0
    else buildMExpr options ast
else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
