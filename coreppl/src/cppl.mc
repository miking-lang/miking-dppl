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

include "mexpr/ast.mc"
include "mexpr/duplicate-code-elimination.mc"

lang CPPLLang = MExprAst + DPPLExtract + MExprCompile
  sem compileRootPPL : Options -> Expr -> ()
  sem compileRootPPL options =
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

  sem _makeError : Info -> String -> Expr
  sem _makeError info =
  | msg ->
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

  sem replaceDpplKeywordsWithError : Expr -> Expr
  sem replaceDpplKeywordsWithError =
  | TmAssume t -> _makeError t.info "Cannot use assume outside of model code"
  | TmObserve t -> _makeError t.info "Cannot use observe outside of model code"
  | TmWeight t -> _makeError t.info "Cannot use weight outside of model code"
  | t -> smap_Expr_Expr replaceDpplKeywordsWithError t
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

    -- NOTE(larshum, 2022-09-07): RootPPL is compiled using the old behaviour,
    -- where the entire file is the model. Otherwise, we expect use of 'infer'
    -- within the program.
    if options.useRootppl then
      compileRootPPL options ast
    else
      let ast = symbolize ast in

      -- Load the runtimes used in the provided AST, and collect identifiers of
      -- common methods within the runtimes.
      let runtimes = loadRuntimes options ast in

      -- Combine the runtimes with the main AST and eliminate duplicate
      -- definitions due to files having common dependencies.
      let ast = combineRuntimes options runtimes ast in

      -- Extract the infer expressions to separate ASTs, one per inference
      -- method. The result consists of the provided AST, updated such that
      -- each infer is replaced with a call to the 'run' function provided by
      -- the chosen runtime. It also consists of one AST per inference method
      -- used in the program.
      match extractInfer runtimes ast with (ast, modelAsts) in

      -- Replace any remaining uses of DPPL specific keywords, like assume or
      -- observe, with an error, as they cannot be used outside of the model
      -- code.
      let ast = replaceDpplKeywordsWithError ast in

      -- Process the model ASTs and insert them in the original AST.
      let ast = mexprCompile options runtimes ast modelAsts in

      -- Exit before performing the inference, if the flag is set
      if options.exitBefore then exit 0
      else
        -- Output the compiled mexpr code
        let outName = "out.mc" in
        writeFile outName (use MExpr in concat "mexpr\n" (mexprToString ast));

        -- Output the compiled OCaml code (unless --skip-final is specified)
        if options.skipFinal then ()
        else sysRunCommand ["mi", "compile", outName] "" "."; ()
else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
