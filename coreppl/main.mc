-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "arg.mc"
include "option.mc"
include "string.mc"
include "dppl-parser.mc"
include "inference.mc"
include "transformation.mc"

-- Options type
type Options = {
  method: String,
  particles : Int,
  printModel: Bool,
  exitBefore: Bool,
  transform: Bool
}

-- Default values for options
let default = {
  method = "",
  particles = 5000,
  printModel = false,
  exitBefore = false,
  transform = false
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
    lam p. {p.options with exitBefore = true}),
  ([("--transform", "", "")],
    "The model is transformed to an efficient representation if possible.",
    lam p. {p.options with transform = true})
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
    let ast = getAst filename in

    -- Transform the model, if the flag is selected
    let ast =
      if r.options.transform then
        transform ast
      else ast in

    -- Optionally print the model
    (if r.options.printModel then
      use DPPLParser in print (expr2str ast)
    else ());

    -- Exit before inference, it the flag is selected
    if r.options.exitBefore then exit 0
    else
      -- Perform the actual inference
      performInference r.options ast
else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
