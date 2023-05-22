include "arg.mc"

-- Options type
type Options = {
  method : String,
  target : String,
  particles : Int,
  printModel: Bool,
  printMCore: Bool,
  exitBefore: Bool,
  skipFinal: Bool,
  transform: Bool,

  -- Where to resample in SMC
  resample: String,

  -- Whether or not to use alignment (for certain inference algorithms)
  align: Bool,

  -- Whether or not to print the actual result samples in compiled programs
  printSamples: Bool,

  -- Option for the `rootppl` target.
  stackSize: Int,

  -- Whether or not to apply CPS transformations
  cps: String,

  -- Whether or not to apply early stopping
  earlyStop: Bool,

  -- Debug compilation to MExpr
  debugMExprCompile: Bool,

  -- Lightweight MCMC options
  mcmcLightweightGlobalProb: Float,
  mcmcLightweightReuseLocal: Bool, -- Currently has no effect

  -- MCMC options,
  printAcceptanceRate: Bool,

  -- PMCMC particle count
  pmcmcParticles: Int,

  -- The random seed to use
  seed: Option Int
}

-- Default values for options
let default = {
  method = "is-lw",
  target = "mexpr",
  particles = 5000,
  resample = "manual",
  align = false,
  printModel = false,
  printMCore = false,
  exitBefore = false,
  skipFinal = false,
  transform = false,
  printSamples = true,
  stackSize = 10000,
  cps = "partial",
  earlyStop = true,
  debugMExprCompile = true,
  mcmcLightweightGlobalProb = 0.1,
  mcmcLightweightReuseLocal = true,
  printAcceptanceRate = false,
  pmcmcParticles = 2,
  seed = None ()
}

-- Options configuration
let config = [
  -- TODO(dlunde,2022-11-14): Could we automatically generate the list of available inference algorithms instead of hardcoding it?
  ([("-m", " ", "<method>")],
    join [
      "The selected inference method. The supported methods are: is-lw, smc-bpf, smc-apf, mcmc-lightweight, mcmc-trace, mcmc-naive, pmcmc-pimh. Default: ",
      default.method
    ],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with method = argToString p}),
  ([("-t", " ", "<target>")],
    join [
      "The compilation target. The supported targets are: mexpr, rootppl. Default: ",
      default.target
    ],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with target = argToString p}),
  ([("-p", " ", "<particles>")],
    join [
      "The number of particles (i.e., samples or iterations). The default is ", (int2string default.particles)
    ],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with particles = argToIntMin p 1}),
  ([("--resample", " ", "<method>")],
    join [
      "The selected resample placement method, for inference algorithms where applicable. The supported methods are: likelihood (resample immediately after all likelihood updates), align (resample after aligned likelihood updates, forces --align), and manual (sample only at manually defined resampling locations). Default: ",
      default.resample, "."
    ],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with resample = argToString p}),
  ([("--align", "", "")],
    "Whether or not to align the model for certain inference algorithms.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with align = true}),
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
    "Do not print the final samples in the compiled program.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with printSamples = false}),
  ([("--stack-size", " ", "<size>")],
    join [
      "The stack size used by RootPPL. The default is ",
      (int2string default.stackSize), " (bytes)."
    ],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with stackSize = argToIntMin p 1}),
  ([("--cps", " ", "<option>")],
    join ["Configuration of CPS transformation (only applicable to certain inference algorithms). The supported options are: none, partial, and full. Default: ", default.cps, "."],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with cps = argToString p}),
  ([("--no-early-stop", "", "")],
    "Disables early stopping in certain inference algorithms.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with earlyStop = false}),
  ([("--no-debug-compile", "", "")],
    "Turn on debugging for the compiler.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with debugMExprCompile = false}),
  ([("--mcmc-lw-gprob", " ", "<value>")],
    join [
      "The probability of performing a global MH step (non-global means only modify a single sample in the previous trace). Default: ",
      float2string default.mcmcLightweightGlobalProb, "."
    ],
    lam p : ArgPart Options. let o : Options = p.options in {o with mcmcLightweightGlobalProb = argToFloat p }),
  ([("--no-reuse-local", "", "")],
    "Do not try to reuse local variables in lightweight MCMC with --align option.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with mcmcLightweightReuseLocal = false}),
  ([("--print-accept-rate", "", "")],
    "Prints the acceptance rate of MCMC algorithms.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with printAcceptanceRate = true}),
  ([("--pmcmcParticles", " ", "<particles>")],
    join [
      "The number of particles for the smc proposal computation. The default is ",
      (int2string default.pmcmcParticles),
      ". This option is used if one of the following methods are used: pmcmc-*."
    ],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with pmcmcParticles = argToIntMin p 1}),
  ([("--seed", " ", "<seed>")],
    "The random seed to use. Initialized randomly if option is omitted.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with seed = Some (argToInt p)})
]

-- Menu
let menu = lam. join [
  "Usage: cppl file.mc [<options>]\n\n",
  "Options:\n",
  argHelpOptions config,
  "\n"
]
