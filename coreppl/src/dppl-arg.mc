include "arg.mc"
include "set.mc"

-- Options type
type Options = {

  -- Inference algorithm
  method : String,

  -- Whether or not to include utests
  test : Bool,

  -- Number of samples/particles
  particles : Int,

  -- General output and debug options
  printModel: Bool,
  printMCore: Bool,
  exitBefore: Bool,
  skipFinal: Bool,
  outputMl: Bool,
  output: String,
  debugPhases: Bool,
  debugDumpPhases: Set String,

  -- Apply static delayed sampling transformation
  staticDelay: Bool,

  -- Apply dynamic delayed sampling
  dynamicDelay: Bool,

  -- Prune algorithm
  prune: Bool,

  -- Where to resample in SMC
  resample: String,

  -- Whether or not to use alignment (for certain inference algorithms)
  align: Bool,

  -- Whether or not to print the actual result samples in compiled programs
  printSamples: Bool,

  -- Whether or not to apply CPS transformations
  cps: String,

  -- Whether or not to apply early stopping
  earlyStop: Bool,

  -- Lightweight MCMC options
  mcmcLightweightGlobalProb: Float,
  mcmcLightweightReuseLocal: Bool, -- Currently has no effect

  -- MCMC options,
  printAcceptanceRate: Bool,

  -- PMCMC particle count
  pmcmcParticles: Int,

  -- The random seed to use
  seed: Option Int,

  -- (temporary option, the end-goal is that we should only ever use peval)
  extractSimplification: String,

  -- Whether to subsample the posterior distribution
  -- used in conjuction with smc-apf and smc-bpf and without no-print-samples
  subsample: Bool,

  -- Used in conjuction with subsample, how many subsamples to take
  subsampleSize: Int,

  -- Drift kernel activation and scale
  driftKernel: Bool,
  driftScale: Float,

  -- Use DPPL frontend with (co)effect decorations
  dpplTypeCheck: Bool
}

-- Default values for options
let defaultArgs = {
  method = "is-lw",
  test = false,
  particles = 5000,
  resample = "manual",
  align = false,
  printModel = false,
  printMCore = false,
  exitBefore = false,
  skipFinal = false,
  debugPhases = false,
  debugDumpPhases = setEmpty cmpString,
  outputMl = false,
  output = "out",
  staticDelay = false,
  dynamicDelay = false,
  prune = false,
  printSamples = true,
  cps = "full",
  earlyStop = true,
  mcmcLightweightGlobalProb = 0.1,
  mcmcLightweightReuseLocal = true,
  printAcceptanceRate = false,
  pmcmcParticles = 2,
  seed = None (),
  extractSimplification = "none",
  subsample = false,
  subsampleSize = 1,
  driftKernel = false,
  driftScale = 1.0,
  dpplTypeCheck = false
}

-- Options configuration
let config = [
  -- TODO(dlunde,2022-11-14): Could we automatically generate the list of available inference algorithms instead of hardcoding it?
  ([("-m", " ", "<method>")],
    join [
      "The selected inference method. The supported methods are: is-lw, smc-bpf, smc-apf, mcmc-lightweight, mcmc-trace, mcmc-naive, pmcmc-pimh. Default: ",
      defaultArgs.method
    ],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with method = argToString p}),
  ([("--test", "", "")],
    "Include utests",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with test = true}),
  ([("-p", " ", "<particles>")],
    join [
      "The number of particles (i.e., samples or iterations). The default is ", (int2string defaultArgs.particles)
    ],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with particles = argToIntMin p 1}),
  ([("--resample", " ", "<method>")],
    join [
      "The selected resample placement method, for inference algorithms where applicable. The supported methods are: likelihood (resample immediately after all likelihood updates), align (resample after aligned likelihood updates, forces --align), and manual (sample only at manually defined resampling locations). Default: ",
      defaultArgs.resample, "."
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
  ([("--debug-phases", "", "")],
    "Show debug and profiling information about each pass",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with debugPhases = true}),
  ([("--debug-phase", " ", "<phase>")],
    "Print a json representation of the AST after the given pass. Can be given multiple times.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with debugDumpPhases = setInsert (argToString p) o.debugDumpPhases}),
  ([("--output-ml", "", "")],
    "Write intermediate OCaml output to 'program.ml' in cwd when compiling",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with outputMl = true}),
  ([("--output", " ", "<file>")],
    "Write output to <file> when compiling",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with output = argToString p}),
  ([("--static-delay", "", "")],
    "The model is transformed to an efficient representation if possible.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with staticDelay = true}),
  ([("--dynamic-delay", "", "")],
    "Runs dynamic delayed sampling on the model.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with dynamicDelay = true}),
  ([("--prune", "", "")],
    "The model is pruned if possible.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with prune = true}),
  ([("--no-print-samples", "", "")],
    "Do not print the final samples in the compiled program.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with printSamples = false}),
  ([("--cps", " ", "<option>")],
    join ["Configuration of CPS transformation (only applicable to certain inference algorithms). The supported options are: none, partial, and full. Default: ", defaultArgs.cps, "."],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with cps = argToString p}),
  ([("--no-early-stop", "", "")],
    "Disables early stopping in certain inference algorithms.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with earlyStop = false}),
  ([("--mcmc-lw-gprob", " ", "<value>")],
    join [
      "The probability of performing a global MH step (non-global means only modify a single sample in the previous trace). Default: ",
      float2string defaultArgs.mcmcLightweightGlobalProb, "."
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
      (int2string defaultArgs.pmcmcParticles),
      ". This option is used if one of the following methods are used: pmcmc-*."
    ],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with pmcmcParticles = argToIntMin p 1}),
  ([("--seed", " ", "<seed>")],
    "The random seed to use. Initialized randomly if option is omitted.",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with seed = Some (argToInt p)}),
  ([("--extract-simplification", " ", "<option>")],
    join ["Temporary flag that decides the simplification approach after extraction in the MExpr compiler backend. The supported options are: none, inline, and peval. Default: ", defaultArgs.extractSimplification, ". Eventually, we will remove this option and only use peval."],
    lam p: ArgPart Options.
      let o: Options = p.options in {o with extractSimplification = argToString p}),
  ([("--subsample", "", "")],
    "Whether to subsample the posterior distribution. Use in conjuction with -m smc-apf or smc-bpf and without --no-print-samples",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with subsample = true}),

  ([("-n", " ", "<subsample size>")],
   join [
        "The number of subsamples to draw if --subsample is selected. Default: ",
    int2string defaultArgs.subsampleSize, "."
       ],
       lam p: ArgPart Options.
        let o: Options = p.options in {o with subsampleSize = argToIntMin p 1}),

  ([("--kernel", "", "")],
    "Use drift Kernel in MCMC. Use in conjuction with -m mcmc-lightweight",
    lam p: ArgPart Options.
      let o: Options = p.options in {o with driftKernel = true}),

  ([("--drift", " ", "<value>")],
    join [
          "Floating point number which corresponds to the standard deviation (sigma) of the normal distribution that will be used for the automatic drift kernel. Default: ",
          float2string defaultArgs.driftScale, "."
      ],
      lam p : ArgPart Options. let o : Options = p.options in {o with driftScale = argToFloatMin p 0. }),
  ([("--dppl-typecheck", "", "")],
   "Use (co)effect type checker for tracking non-determinism and differentiability.",
   lam p: ArgPart Options.
     let o: Options = p.options in {o with dpplTypeCheck = true})
]

-- Menu
let menu = lam. join [
  "Usage: cppl file.mc [<options>]\n\n",
  "Options:\n",
  argHelpOptions config,
  "\n"
]
