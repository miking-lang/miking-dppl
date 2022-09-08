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

lang CPPLLang = MExprAst + DPPLExtract
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
      use DPPLParser in printLn (mexprPPLToString ast)
    else ());

    -- Exit before inference, if the flag is set
    if options.exitBefore then ()
    else
      -- Perform the actual inference
      performInference options ast

  sem transformModelAst : Options -> (Expr, InferData) -> Expr
  sem transformModelAst options =
  | (ast, inferData) ->
    -- Transform the model, if the flag is set
    let ast =
      if options.transform then
        transform ast
      else ast in

    -- Optionally print the model
    (if options.printModel then
      use DPPLParser in printLn (mexprPPLToString ast)
    else ());

    -- Do not apply the inference compilation, if the flag is set
    if options.exitBefore then ast
    else
      -- Apply the inference-specific compilation on the model AST, using the
      -- inference method specified for the model.
      let options = {options with method = inferData.method} in
      mexprCompile options inferData ast
end

let compileRootPPL = use CPPLLang in compileRootPPL

let transformModelAst = use CPPLLang in transformModelAst

mexpr

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
    match options.method with "rootppl-smc" then
      compileRootPPL options ast
    else
      -- Extract the infer expressions from the AST, producing a separate AST
      -- for each model, paired with a string representing the chosen inference
      -- method.
      match extractInfer ast with (ast, modelAsts) in

      -- Apply the transformations on each model
      let modelAsts = map (transformModelAst options) modelAsts in

      -- Combine the model ASTs with the original AST
      -- NOTE(larshum, 2022-09-07): This produces a lot of duplicated code in
      -- the AST, if the infer expression is used multiple times.
      let ast = bindall_ (snoc modelAsts ast) in

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
