-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "option.mc"
include "string.mc"
include "dppl-parser.mc"
include "dppl-arg.mc"
include "inference.mc"
include "transformation.mc"
include "common.mc"

mexpr

-- Use the arg.mc library to parse arguments
let result = argParse default config in
match result with ParseOK r then
  let options: Options = r.options in
  -- Print menu if not exactly one file argument
  if neqi (length r.strings) 1 then
    print (menu ());
    exit 1
  else
    -- Read and parse the file
    let filename = head r.strings in
    let ast = getAst filename in

    -- Transform the model, if the flag is selected
    let ast =
      if options.transform then
        transform ast
      else ast in

    -- Optionally print the model
    (if options.printModel then
      use DPPLParser in printLn (expr2str ast)
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
