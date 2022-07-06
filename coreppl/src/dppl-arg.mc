include "arg.mc"

-- Options type
type Options = {
  method: String,
  particles : Int, -- NOTE(dlunde,2022-06-28): Currently not used as it is provided at runtime.
  printModel: Bool,
  printMCore: Bool,
  exitBefore: Bool,
  skipFinal: Bool,
  transform: Bool,

  -- Where to resample in SMC
  resample: String,

  -- Whether or not to print the actual result samples in compiled programs
  printSamples: Bool,

  -- Option for the `rootppl-smc` method.
  stackSize: Int,

  -- Options for the `mexpr-*` methods.
  cps: String,

  -- Whether or not to apply early stopping
  earlyStop: Bool
}

-- Default values for options
let default = {
  method = "",
  particles = 5000,
  resample = "manual",
  printModel = false,
  printMCore = false,
  exitBefore = false,
  skipFinal = false,
  transform = false,
  printSamples = true,
  stackSize = 10000,
  cps = "partial",
  earlyStop = true
}

-- Options configuration
let config = [
  ([("-m", " ", "<method>")],
    "The selected inference method. The supported methods are: mexpr-importance, rootppl-smc.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with method = argToString p}),
  ([("-p", " ", "<particles>")],
    join ["The number of particles. The default is 5000. This option is used if one ",
          "of the following methods are used: mexpr-importance, rootppl-smc."],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with particles = argToIntMin p 1}),
  ([("--resample", " ", "<method>")],
    "The selected resample placement method, for inference algorithms where applicable. The supported methods are: likelihood (resample immediately after all likelihood updates), align (resample after aligned likelihood updates), and manual (default, sample only at manually defined resampling locations).",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with resample = argToString p}),
  ([("--print-model", "", "")],
    "The parsed model is pretty printed before inference.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with printModel = true}),
  ([("--print-mcore", "", "")],
    "Print the generated MCore program before execution.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with printMCore = true}),
  ([("--exit-before", "", "")],
    "Exit before compiling.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with exitBefore = true}),
  ([("--skip-final", "", "")],
    "Do not perform the final compilation step (e.g., MExpr to OCaml).",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with skipFinal = true}),
  ([("--transform", "", "")],
    "The model is transformed to an efficient representation if possible.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with transform = true}),
  ([("--no-print-samples", "", "")],
    "Do not print the final samples when compiling with the rootppl-smc method.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with printSamples = false}),
  ([("--stack-size", " ", "<size>")],
    join ["The stack size used by RootPPL. The default is 10000 (bytes)."],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with stackSize = argToIntMin p 1}),
  ([("--cps", " ", "<option>")],
    "Configuration of CPS transformation (only applicable to certain inference algorithms). The supported options are: none, partial (default, usually the best), and full.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with cps = argToString p}),
  ([("--no-early-stop", "", "")],
    "Disables early stopping in certain inference algorithms.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with earlyStop = false})
]

-- Menu
let menu = lam. join [
  "Usage: cppl file.mc [<options>]\n\n",
  "Options:\n",
  argHelpOptions config,
  "\n"
]

