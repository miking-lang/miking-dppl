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

lang CPPLLang = MExprAst + DPPLExtract + MExprCompile + TransformDist
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

  sem replaceDpplKeywords : Expr -> Expr
  sem replaceDpplKeywords =
  | TmAssume t ->
    -- NOTE(larshum, 2022-10-05): It is allowed to use assume outside of model
    -- code because it is independent of the inference method.
    let i = withInfo t.info in
    i (app_ (i (recordproj_ "sample" t.dist)) (i unit_))
  | TmObserve t -> _makeError t.info "Cannot use observe outside of model code"
  | TmWeight t -> _makeError t.info "Cannot use weight outside of model code"
  | TmResample t -> _makeError t.info "Cannot use resample outside of model code"
  | t -> t
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
    let ast = symbolize ast in

    -- Load the runtimes used in the provided AST, and collect identifiers of
    -- common methods within the runtimes.
    let runtimes = loadRuntimes options ast in

    -- If no runtimes are found, it means there are no uses of 'infer' in the
    -- program. In this case, we compile using the old behaviour.
    if mapIsEmpty runtimes then
      compileSingle options ast
    else

      -- Combine the runtimes with the main AST and eliminate duplicate
      -- definitions due to files having common dependencies.
      let ast = combineRuntimes options runtimes ast in

      -- Extract the infer expressions to separate ASTs, one per inference
      -- method. The result consists of the provided AST, updated such that
      -- each infer is replaced with a call to the 'run' function provided by
      -- the chosen runtime. It also consists of one AST per inference method
      -- used in the program.
      match extractInfer runtimes ast with (ast, modelAsts) in

      -- Replace uses of DPPL-specific keywords with corresponding code. The
      -- assume is replaced with a sample of the corresponding distribution.
      -- Keywords such as observe and weight result in errors because they
      -- cannot be used outside of the inferred model.
      let ast = mapPre_Expr_Expr transformTmDist ast in
      let ast = removeTyDist ast in
      let ast = mapPre_Expr_Expr replaceDpplKeywords ast in

      -- Process the model ASTs and insert them in the original AST.
      let ast = mexprCompile options runtimes ast modelAsts in

      -- Exit before producing the output files, if the flag is set
      if options.exitBefore then exit 0
      else buildMExpr options ast
else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
