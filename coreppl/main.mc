-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "arg.mc"
include "option.mc"
include "string.mc"
include "dppl-parser.mc"
include "inference.mc"

-- Options type
type Options = {
  method: String,
  particles : Int,
  printModel: Bool,
  exitBefore: Bool
}

-- Default values for options
let default = {
  method = "",
  particles = 5000,
  printModel = false,
  exitBefore = false
}

-- Options configuration
let config = [
  ([("-m", " ", "<method>")],
    "The selected inference method. The supported methods are: importance, rootppl-smc.",
    lam p. {p.options with method = argToString p}),
  ([("-p", " ", "<particles>")],
    join ["The number of particles. The default is 5000. This option is used if one ",
          "of the following methods are used: importance, rootppl-smc."],
    lam p. {p.options with particles = argToIntMin p 1}),
  ([("--print-model", "", "")],
    "The parsed model is pretty printed before inference.",
    lam p. {p.options with printModel = true}),
  ([("--exit-before", "", "")],
    "Exit before inference takes place. ",
    lam p. {p.options with exitBefore = true})
]

-- Menu
let menu = lam. join [
  "Usage: midppl file.mc [<options>]\n\n",
  "Options:\n",
  argHelpOptions config,
  "\n"
]

mexpr

-- Use the arg.mc library to parse arguments
let result = argParse default config in
match result with ParseOK r then
  -- Print menu if not exactly one file argument
  if neqi (length r.strings) 1 then
    print (menu ());
    exit 1
  else
    -- Read and parse the file
    let filename = head r.strings in
    let ast = getAst filename r.options.printModel in
    -- Exit before inference, it the flag is selected
    if r.options.exitBefore then exit 0
    else
      -- Perform the actual inference
      performInference r.options ast
else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
