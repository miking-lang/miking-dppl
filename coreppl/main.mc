-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "arg.mc"
include "option.mc"
include "string.mc"

-- Options type
type Options = {
  particles : Int
}

-- Default values for options
let default = {
  particles = 5000
}

-- Options configuration
let config = [
  (["--print-model"], ["="], "Debug print the model after parsing.",
    lam o:Options. lam v. {o with particles = argInt v})
  (["--particles"], ["="], "Number of particles for importance sampling. Default 5000.",
    lam o:Options. lam v. {o with particles = argInt v})
]

-- Menu
let menu = lam. join [
  "Usage: midppl file.pmc [<options>]\n\n",
  "Options:\n",
  argHelpOptions config,
  "\n"
]

mexpr

-- Use the arg.mc library to parse arguments
let result = argParse default config in
match result with ParseOK r then
  -- Help the type annotator
  let r : ArgResult = r in
  let options : Options = r.options in
  -- Print menu if no file arguments
  if eqi (length r.strings) 0 then
    print (menu ());
    exit 1
  else
  -- Parsing OK. Run program
    print (join ["files: ", int2string (length r.strings),
                 " particles: ", int2string options.particles, "\n"])
else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
