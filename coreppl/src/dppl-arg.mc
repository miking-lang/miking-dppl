include "optparse-applicative.mc"
include "set.mc"
include "infer-method.mc"

include "inference/is-lw.mc"
include "inference/smc-bpf.mc"
include "inference/smc-apf.mc"
include "inference/mcmc-lightweight.mc"
include "inference/mcmc-naive.mc"
include "inference/mcmc-trace.mc"
include "inference/pmcmc-pimh.mc"

type FrontendOptions =
  { printMCore : Bool
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
  , defaultParticles : Int
  }

-- Options that are relevant regardless of the chosen inference method
type TransformationOptions =
  { extractSimplification : String
  , staticDelay : Bool
  , debugDumpPhases : Set String
  , debugPhases : Bool
  , printModel : Bool
  , seed : Option Int
  }

type SeparatedOptions =
  { frontend : FrontendOptions
  , cpplFiles : CPPLFileOptions
  , transformations : TransformationOptions
  , defaultMethod : use InferMethodBase in InferMethod
  }

let frontendOptions : OptParser FrontendOptions =
  let mk = lam printMCore. lam exitBefore. lam outputMl. lam output. lam test. lam input.
    { printMCore = printMCore
    , exitBefore = exitBefore
    , outputMl = outputMl
    , output = output
    , test = test
    , input = input
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
  let output =
    let default = "out" in
    let opt = optArg
      { optArgDefString with long = "output"
      , arg = "<file>"
      , description = concat "Write output to <file> when compiling. Default: " default
      } in
    optOr opt (optPure default) in
  let test = optFlag
    { optFlagDef with long = "test"
    , description = "Include utests."
    } in
  let input = optPos
    { optPosDefString with arg = "<program>"
    , description = "The CorePPL program to compile."
    } in
  optApply (optMap5 mk printMCore exitBefore outputMl output test) input

let cpplFileOptions : OptParser CPPLFileOptions =
  let mk = lam dpplTypeCheck. lam dpplTypeCheck. lam printSamples. lam printAcceptanceRate. lam defaultParticles.
    { dpplTypeCheck = dpplTypeCheck
    , printSamples = printSamples
    , printAcceptanceRate = printAcceptanceRate
    , defaultParticles = defaultParticles
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
  let defaultParticles =
    let default = 5000 in
    let opt = optArg
      { optArgDefInt with long = "default-particles", short = "p"
      , description = concat "The number of particles (i.e., samples or iterations). Takes precedence over --particles in a program without infers. The default is " (int2string default)
      } in
    optOr opt (optPure default) in
  optMap5 mk dpplTypeCheck dpplTypeCheck printSamples printAcceptanceRate defaultParticles

let inferenceMethodOptions : OptParser (use InferMethodBase in InferMethod) = foldl1 optOr
  [ isLwOptions
  , smcApfOptions
  , smcBpfOptions
  , mcmcNaiveOptions
  , mcmcTraceOptions
  , pmcmcPimhOptions
  , mcmcLightweightOptions
  ]

let transformationOptions : OptParser TransformationOptions =
  let mk = lam printModel. lam extractSimplification. lam staticDelay. lam debugPhases. lam debugDumpPhases. lam seed.
    { printModel = printModel
    , extractSimplification = extractSimplification
    , staticDelay = staticDelay
    , debugPhases = debugPhases
    , debugDumpPhases = debugDumpPhases
    , seed = seed
    } in
  let printModel = optFlag
    { optFlagDef with long = "print-model"
    , description = "The parsed model is pretty printed before inference."
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
  optApply (optMap5 mk printModel extractSimplification staticDelay debugPhases debugDumpPhases) seed

let options : OptParser SeparatedOptions =
  let mk = lam frontend. lam cpplFiles. lam transformations. lam defaultMethod.
    { frontend = frontend
    , cpplFiles = cpplFiles
    , transformations = transformations
    , defaultMethod = defaultMethod
    } in
  optMap4 mk frontendOptions cpplFileOptions transformationOptions inferenceMethodOptions

let cpplName = "cppl"
let cpplDescription = ""
