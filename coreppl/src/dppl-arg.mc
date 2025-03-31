include "optparse-applicative.mc"
include "set.mc"

type FrontendOptions =
  { printModel : Bool
  , printMCore : Bool
  , exitBefore : Bool
  , outputMl : Bool
  , input : String
  , output : String
  , test : Bool
  }

type CPPLFileOptions =
  { dpplTypeCheck : Bool
  -- NOTE(vipa, 2025-03-28): These are relevant only to programs
  -- without `infer`
  , printSamples : Bool
  , printAcceptanceRate : Bool
  }

-- TODO(vipa, 2025-03-31): These should be split by inference method,
-- most likely by extending each InferenceMethod record with extra
-- fields for everything they end up using
type ModelOptionsTemp =
  { method : String
  , align : Bool
  , cps : String
  , driftKernel : Bool
  , driftScale : Float
  , dynamicDelay : Bool
  , earlyStop : Bool
  , mcmcLightweightGlobalProb : Float
  , particles : Int
  , pmcmcParticles : Int
  , prune : Bool
  , resample : String
  , resampleFrac : Float
  , subsample : Bool
  , subsampleSize : Int
  }

type TransformationOptions =
  { extractSimplification : String
  , staticDelay : Bool
  , debugDumpPhases : Set String
  , debugPhases : Bool
  , seed : Option Int
  , defaultMethod : ModelOptionsTemp
  }

type SeparatedOptions =
  { frontend : FrontendOptions
  , cpplFiles : CPPLFileOptions
  , transformations : TransformationOptions
  }

let frontendOptions : OptParser FrontendOptions =
  let mk = lam printModel. lam printMCore. lam exitBefore. lam outputMl. lam output. lam test. lam input.
    { printModel = printModel
    , printMCore = printMCore
    , exitBefore = exitBefore
    , outputMl = outputMl
    , output = output
    , test = test
    , input = input
    } in
  let printModel = optFlag
    { optFlagDef with long = "print-model"
    , description = "The parsed model is pretty printed before inference."
    } in
  let printMCore = optFlag
    { optFlagDef with long = "print-mcore"
    , description = "Print the generated MCore program before execution."
    } in
  let exitBefore = optFlag
    { optFlagDef with long = "exit-before"
    , description = "Exit before compiling."
    } in
  let outputMl = optFlag
    { optFlagDef with long = "output-ml"
    , description = "Write intermediate OCaml output to 'program.ml' in cwd when compiling."
    } in
  let output = optArg
    { optArgDefString with long = "output"
    , arg = "<file>"
    , description = "Write output to <file> when compiling"
    } in
  let test = optFlag
    { optFlagDef with long = "test"
    , description = "Include utests."
    } in
  let input = optPos
    { optPosDefString with arg = "<program>"
    , description = "The CorePPL program to compile."
    } in
  optApply (optApply (optMap5 mk printModel printMCore exitBefore outputMl output) test) input

let cpplFileOptions : OptParser CPPLFileOptions =
  let mk = lam dpplTypeCheck. lam dpplTypeCheck. lam printSamples. lam printAcceptanceRate.
    { dpplTypeCheck = dpplTypeCheck
    , printSamples = printSamples
    , printAcceptanceRate = printAcceptanceRate
    } in
  let dpplTypeCheck = optFlag
    { optFlagDef with long = "dppl-typecheck"
    , description = "Use (co)effect type checker for tracking non-determinism and differentiability."
    } in
  let printSamples = optMap (lam x. not x) (optFlag
    { optFlagDef with long = "no-print-samples"
    , description = "Do not print the final samples in the compiled program."
    }) in
  let printAcceptanceRate = optFlag
    { optFlagDef with long = "print-accept-rate"
    , description = "Prints the acceptance rate of MCMC algorithms."
    } in
  optMap4 mk dpplTypeCheck dpplTypeCheck printSamples printAcceptanceRate

let _modelOptionsTempDefault : ModelOptionsTemp =
  { method = ""
  , align = false
  , cps = "full"
  , driftKernel = false
  , driftScale = 1.0
  , dynamicDelay = false
  , earlyStop = true
  , mcmcLightweightGlobalProb = 0.1
  , particles = 5000
  , pmcmcParticles = 2
  , prune = false
  , resample = "manual"
  , resampleFrac = 0.7
  , subsample = false
  , subsampleSize = 1
  }
let _align : OptParser Bool = optFlag
  { optFlagDef with long = "align"
  , description = "Whether or not to align the model for certain inference algorithms."
  }
let _cps : OptParser String =
  let default = "full" in
  let opt = optArg
    { optArgDefString with long = "cps"
    , description = concat "Configuration of CPS transformation (only applicable to certain inference algorithms). The supported options are: none, partial, and full. Default: " default
    } in
  optOr opt (optPure default)
let _driftKernel : OptParser Bool = optFlag
  { optFlagDef with long = "kernel"
  , description = "Use drift Kernel in MCMC."
  }
let _driftScale : OptParser Float =
  let default = 1.0 in
  let opt = optArg
    { optArgDefFloat with long = "drift"
    , description = concat "Floating point number which corresponds to the standard deviation (sigma) of the normal distribution that will be used for the automatic drift kernel. Default: " (float2string default)
    } in
  optOr opt (optPure default)
let _dynamicDelay : OptParser Bool = optFlag
  { optFlagDef with long = "dynamic-delay"
  , description = "Runs dynamic delayed sampling on the model."
  }
let _earlyStop : OptParser Bool = optMap (lam x. not x) (optFlag
  { optFlagDef with long = "no-early-stop"
  , description = "Disables early stopping in certain inference algorithms."
  })
let _mcmcLightweightGlobalProb : OptParser Float =
  let default = 0.1 in
  let opt = optArg
    { optArgDefFloat with long = "mcmc-lw-gprob"
    , description = concat "The probability of performing a global MH step (non-global means only modify a single sample in the previous trace). Default: " (float2string default)
    } in
  optOr opt (optPure default)
let _particles : OptParser Int =
  let default = 5000 in
  let opt = optArg
    { optArgDefInt with long = "particles", short = "p"
    , description = concat "The number of particles (i.e., samples or iterations). The default is " (int2string default)
    } in
  optOr opt (optPure default)
let _pmcmcParticles : OptParser Int =
  let default = 2 in
  let opt = optArg
    { optArgDefInt with long = "pmcmcParticles"
    , description = concat "The number of particles for the smc proposal computation. The default is " (int2string default)
    } in
  optOr opt (optPure default)
let _prune : OptParser Bool = optFlag
  { optFlagDef with long = "prune"
  , description = "The model is pruned if possible."
  }
let _resample : OptParser String =
  let default = "manual" in
  let opt = optArg
    { optArgDefString with long = "resample"
    , description = concat "The selected resample placement method, for inference algorithms where applicable. The supported methods are: likelihood (resample immediately after all likelihood updates), align (resample after aligned likelihood updates, forces --align), and manual (sample only at manually defined resampling locations). Default: " default
    } in
  optOr opt (optPure default)
let _resampleFrac : OptParser Float =
  let default = 0.7 in
  let opt = optArg
    { optArgDefFloat with long = "resampleFrac"
    , description = concat "Floating point number to trigger resampling for SMC-BPF when ESS is less than resampleFrac × particleCount. Default: " (float2string default)
    } in
  optOr opt (optPure default)
let _subsample : OptParser Bool = optFlag
  { optFlagDef with long = "subsample"
  , description = "Whether to subsample the posterior distribution."
  }
let _subsampleSize : OptParser Int =
  let default = 1 in
  let opt = optArg
    { optArgDefInt with long = "subsample-size", short = "n"
    , description = concat "The number of subsamples to draw if --subsample is selected. Default: " (int2string default)
    } in
  optOr opt (optPure default)

let _methodFlag : Bool -> String -> OptParser String = lam default. lam str.
  let opt = optMap (lam. str) (optSpecificArg {optExactArg str with short = "m"}) in
  if default then optOr opt (optPure str) else opt

let isLightweightOptions : OptParser ModelOptionsTemp =
  let mk = lam method. lam cps. lam dynamicDelay. lam particles. lam earlyStop.
    { _modelOptionsTempDefault with method = method
    , cps = cps
    , dynamicDelay = dynamicDelay
    , particles = particles
    , earlyStop = earlyStop
    } in
  let m = _methodFlag true "is-lw" in
  optMap5 mk m _cps _dynamicDelay _particles _earlyStop

let mcmcLightweightOptions : OptParser ModelOptionsTemp =
  let mk = lam method. lam particles. lam align. lam cps. lam mcmcLightweightGlobalProb. lam driftKernel. lam driftScale.
    { _modelOptionsTempDefault with method = method
    , align = align
    , cps = cps
    , mcmcLightweightGlobalProb = mcmcLightweightGlobalProb
    , driftKernel = driftKernel
    , driftScale = driftScale
    , particles = particles
    } in
  let m = _methodFlag false "mcmc-lightweight" in
  (optApply (optApply (optMap5 mk m _particles _align _cps _mcmcLightweightGlobalProb) _driftKernel) _driftScale)

let mcmcNaiveOptions : OptParser ModelOptionsTemp =
  let mk = lam method. lam particles.
    { _modelOptionsTempDefault with method = method
    , particles = particles
    } in
  let m = _methodFlag false "mcmc-naive" in
  optMap2 mk m _particles

let mcmcTraceOptions : OptParser ModelOptionsTemp =
  let mk = lam method. lam particles.
    { _modelOptionsTempDefault with method = method
    , particles = particles
    } in
  let m = _methodFlag false "mcmc-trace" in
  optMap2 mk m _particles

let pmcmcPimhOptions : OptParser ModelOptionsTemp =
  let mk = lam method. lam cps. lam particles. lam pmcmcParticles.
    { _modelOptionsTempDefault with method = method
    , cps = cps
    , particles = particles
    , pmcmcParticles = pmcmcParticles
    } in
  let m = _methodFlag false "pmcmc-pimh" in
  optMap4 mk m _cps _particles _pmcmcParticles

let smcApfOptions : OptParser ModelOptionsTemp =
  let mk = lam method. lam cps. lam resample. lam particles. lam subsample. lam subsampleSize.
    { _modelOptionsTempDefault with method = method
    , cps = cps
    , resample = resample
    , particles = particles
    , subsample = subsample
    , subsampleSize = subsampleSize
    } in
  let m = _methodFlag false "smc-apf" in
  optApply (optMap5 mk m _cps _resample _particles _subsample) _subsampleSize

let smcBpfOptions : OptParser ModelOptionsTemp =
  let mk = lam cps. lam resample. lam resampleFrac. lam particles. lam subsample. lam subsampleSize. lam prune. lam dynamicDelay.
    { _modelOptionsTempDefault with cps = cps
    , resample = resample
    , resampleFrac = resampleFrac
    , particles = particles
    , subsample = subsample
    , subsampleSize = subsampleSize
    , prune = prune
    , dynamicDelay = dynamicDelay
    } in
  let m = _methodFlag false "smc-bpf" in
  optMap2 (lam. lam x. x) m (optApply (optApply (optApply (optMap5 mk _cps _resample _resampleFrac _particles _subsample) _subsampleSize) _prune) _dynamicDelay)

let inferenceMethodOptions : OptParser ModelOptionsTemp = foldl1 optOr
  [ isLightweightOptions
  , mcmcLightweightOptions
  , mcmcNaiveOptions
  , mcmcTraceOptions
  , pmcmcPimhOptions
  , smcApfOptions
  , smcBpfOptions
  -- , optArg -- NOTE(vipa, 2025-03-31): Each inference method handles
  --          -- its own identifier, thus all that's left here is
  --          -- documentation and failing if we get here
  --   { optArgDef with short = "m"
  --   , arg = "<method>"
  --   , parse = lam str.
  --     Left (join ["Unknown inference method: ", str])
  --   , description = "The selected inference method."
  --   }
  ]


let transformationOptions : OptParser ModelOptionsTemp -> OptParser TransformationOptions = lam method.
  let mk = lam extractSimplification. lam staticDelay. lam debugPhases. lam debugDumpPhases. lam defaultMethod. lam seed.
    { extractSimplification = extractSimplification
    , staticDelay = staticDelay
    , debugPhases = debugPhases
    , debugDumpPhases = debugDumpPhases
    , defaultMethod = defaultMethod
    , seed = seed
    } in
  let extractSimplification =
    let default = "none" in
    let opt = optArg
      { optArgDefString with long = "extract-simplification"
      , arg = "<option>"
      , description = join
        [ "Temporary flag that decides the simplification approach after extraction"
        , "in the MExpr compiler backend. The supported options are: none, inline,"
        , "and peval. Default: ", default, ". Eventually, we will remove this option"
        , "and only use peval."
        ]
      } in
    optOr opt (optPure default) in
  let staticDelay = optFlag
    { optFlagDef with long = "static-delay"
    , description = "The model is transformed to an efficient representation if possible."
    } in
  let debugPhases = optFlag
    { optFlagDef with long = "debug-phases"
    , description = "Show debug and profiling information about each pass"
    } in
  let debugDumpPhases =
    let opt = optArg
      { optArgDefString with long = "debug-log-phases"
      , arg = "<phases>"
      , description = "Print a json representation of the AST after the given (comma-separated) passes."
      } in
    optOr (optMap (lam str. setOfSeq cmpString (strSplit "," str)) opt) (optPure (setEmpty cmpString)) in
  let seed = optOptional (optArg
    { optArgDefInt with long = "seed"
    , arg = "<seed>"
    , description = "The random seed to use. Initialized randomly if option is omitted."
    }) in
  optApply (optMap5 mk extractSimplification staticDelay debugPhases debugDumpPhases method) seed

let options : OptParser SeparatedOptions =
  let mk = lam frontend. lam cpplFiles. lam transformations.
    { frontend = frontend
    , cpplFiles = cpplFiles
    , transformations = transformations
    } in
  optMap3 mk frontendOptions cpplFileOptions (transformationOptions inferenceMethodOptions)

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

  -- Trigger resampling for SMC-BPF when ESS is less than resampleFrac × particleCount
  resampleFrac: Float,

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

let mkBackcompatOptions : SeparatedOptions -> (String, Options) = lam options.
  ( options.frontend.input
  , { particles = options.transformations.defaultMethod.particles
    , method = options.transformations.defaultMethod.method
    , align = options.transformations.defaultMethod.align
    , cps = options.transformations.defaultMethod.cps
    , driftKernel = options.transformations.defaultMethod.driftKernel
    , driftScale = options.transformations.defaultMethod.driftScale
    , dynamicDelay = options.transformations.defaultMethod.dynamicDelay
    , earlyStop = options.transformations.defaultMethod.earlyStop
    , mcmcLightweightGlobalProb = options.transformations.defaultMethod.mcmcLightweightGlobalProb
    , particles = options.transformations.defaultMethod.particles
    , pmcmcParticles = options.transformations.defaultMethod.pmcmcParticles
    , prune = options.transformations.defaultMethod.prune
    , resample = options.transformations.defaultMethod.resample
    , resampleFrac = options.transformations.defaultMethod.resampleFrac
    , subsample = options.transformations.defaultMethod.subsample
    , subsampleSize = options.transformations.defaultMethod.subsampleSize
    , printModel = options.frontend.printModel
    , printMCore = options.frontend.printMCore
    , exitBefore = options.frontend.exitBefore
    , outputMl = options.frontend.outputMl
    , output = options.frontend.output
    , test = options.frontend.test
    , dpplTypeCheck = options.cpplFiles.dpplTypeCheck
    , printSamples = options.cpplFiles.printSamples
    , printAcceptanceRate = options.cpplFiles.printAcceptanceRate
    , extractSimplification = options.transformations.extractSimplification
    , staticDelay = options.transformations.staticDelay
    , debugDumpPhases = options.transformations.debugDumpPhases
    , debugPhases = options.transformations.debugPhases
    , seed = options.transformations.seed
    }
  )

let backcompatOptions : OptParser (String, Options) = optMap mkBackcompatOptions options
let cpplName = "cppl"
let cpplDescription = ""
