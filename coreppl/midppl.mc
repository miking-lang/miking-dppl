-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "option.mc"
include "string.mc"
include "parser.mc"
include "transformation.mc"
include "dppl-arg.mc"
include "inference.mc"
include "common.mc"

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
    let ast =
      -- TODO(dlunde,2022-05-09): This is a temporary hack required because we
      -- can only run deadcode elimination when parsing (it's implemented on
      -- the OCaml side). For methods other than rootppl-smc, we want to allow
      -- parsing programs with free variables, and can therefore not use dead
      -- code elimination as this requires boot symbolization (which throws an
      -- error on unbound variables).
      match options.method with "rootppl-smc" then getAst false filename
      else getAst true filename
    in

    -- Transform the model, if the flag is selected
    let ast =
      if options.transform then
        transform ast
      else ast in

    -- Optionally print the model
    (if options.printModel then
      use DPPLParser in printLn (mexprPPLToString ast)
    else ());

    -- Exit before inference, it the flag is selected
    if options.exitBefore then exit 0
    else
      -- Perform the actual inference
      performInference options ast
else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
