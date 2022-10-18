-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "option.mc"
include "string.mc"
include "parser.mc"
include "transformation.mc"
include "dppl-arg.mc"
include "extract.mc"
include "inference.mc"
include "common.mc"
include "build.mc"

include "mexpr/ast.mc"
include "mexpr/duplicate-code-elimination.mc"

lang CPPLLang =
  MExprAst + DPPLExtract + MExprCompile + TransformDist + MExprEliminateDuplicateCode

  -- Compiles such that the entire program is considered the model.
  sem compileSingle : Options -> Expr -> ()
  sem compileSingle options =
  | ast ->
    -- Transform the model, if the flag is set
    let ast =
      if options.transform then
        transform ast
      else ast in

    -- Optionally print the model
    (if options.printModel then
      printLn (mexprPPLToString ast)
    else ());

    -- Exit before inference, if the flag is set
    if options.exitBefore then ()
    else
      -- Perform the actual inference
      performInference options ast

  -- Used to transform away distributions in the main AST.
  -- TODO(larshum, 2022-10-12): Replace the types with references to the 'Dist'
  -- type defined in the runtime instead of removing them.
  sem transformDistributions : Expr -> Expr
  sem transformDistributions =
  | t ->
    let t = mapPre_Expr_Expr transformTmDist t in
    removeTyDist t

  sem _makeError : Info -> String -> Expr
  sem _makeError info =
  | keywordStr ->
    let msg = join ["Cannot use ", keywordStr, " outside of inferred model"] in
    let toStr = lam msg.
      map
        (lam ch. TmConst {val = CChar {val = ch},
                          ty = TyChar {info = info}, info = info})
        msg
    in
    TmApp {
      lhs = TmConst {val = CError (), ty = TyUnknown {info = info}, info = info},
      rhs = TmSeq {tms = toStr msg,
                   ty = TySeq {ty = TyChar {info = info}, info = info},
                   info = info},
      ty = TyUnknown {info = info}, info = info}

  sem replaceDpplKeywords : Expr -> Expr
  sem replaceDpplKeywords =
  | TmAssume t -> _makeError t.info "assume"
  | TmObserve t -> _makeError t.info "observe"
  | TmWeight t -> _makeError t.info "weight"
  | TmResample t -> _makeError t.info "resample"
  | t -> smap_Expr_Expr replaceDpplKeywords t
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

    -- If no runtimes are found, it means there are no uses of 'infer' in the
    -- program. In this case, we compile using the old behaviour.
    if mapIsEmpty runtimes then
      compileSingle options ast
    else

      -- Combine the runtime ASTs to one AST and eliminate duplicate
      -- definitions due to files having common dependencies. The result is an
      -- updated map of runtime entries, a combined runtime AST and a
      -- symbolization environment.
      match combineRuntimes options runtimes with (runtimes, runtimeAst, runtimeSymEnv) in

      -- Transform distributions in the main AST to use MExpr code, before
      -- symbolizing it using the symbolization environment produced by the
      -- runtime.
      -- TODO(larshum, 2022-10-18): Restrict the contents of the runtime
      -- symbolization environment to only include the identifiers we want to
      -- expose, such as 'Dist', 'sample' and 'logObserve'. Currently, it
      -- includes all top-level identifiers.
      let ast = transformDistributions ast in
      let ast = symbolizeExpr runtimeSymEnv ast in

      -- Extract the infer expressions to separate ASTs, one per inference
      -- method. The result consists of the provided AST, updated such that
      -- each infer is replaced with a call to the 'run' function provided by
      -- the chosen runtime. It also consists of one AST per inference method
      -- used in the program.
      match extractInfer runtimes ast with (ast, modelAsts) in

      -- Combine the main AST with the runtime AST, after extracting the
      -- models, and eliminate duplicate code due to common dependencies.
      let ast = bind_ runtimeAst ast in
      let ast = eliminateDuplicateCode ast in

      -- Replace uses of DPPL keywords in the main AST, i.e. outside of models,
      -- with errors. This code is unreachable unless the inferred models are
      -- also used outside of infers, which is an error.
      -- TODO(larshum, 2022-10-07): Detect such errors statically.
      let ast = replaceDpplKeywords ast in

      -- Process the model ASTs and insert them in the original AST.
      let ast = mexprCompile options runtimes ast modelAsts in

      -- Exit before producing the output files, if the flag is set
      if options.exitBefore then exit 0
      else buildMExpr options ast
else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
