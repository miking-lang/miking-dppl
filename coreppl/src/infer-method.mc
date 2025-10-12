include "option.mc"
include "stringid.mc"
include "optparse-applicative.mc"
include "mexpr/info.mc"
include "mexpr/pprint.mc"
include "mexpr/type-check.mc"

include "method-helper.mc"

-- Support for sharing flags between inference methods
let _alignDefault : Bool = false
let _align : OptParser Bool = optMap (xor _alignDefault) (optFlag
  { optFlagDef with long = "align"
  , description = "Whether or not to align the model for certain inference algorithms."
  })
let _debugAlignment : OptParser (Option String) = optOptional (optArg
  { optArgDefString with long = "debug-alignment-html"
  , description = "Output an interactive .html file showing alignment results to the given file."
  , arg = "FILE"
  })
let _cpsDefault : String = "full"
let _cps : OptParser String =
  let opt = optArg
    { optArgDefString with long = "cps"
    , description = concat "Configuration of CPS transformation (only applicable to certain inference algorithms). The supported options are: none, partial, and full. Default: " _cpsDefault
    } in
  optOr opt (optPure _cpsDefault)
let _driftKernelDefault : Bool = false
let _driftKernel : OptParser Bool = optMap (xor _driftKernelDefault) (optFlag
  { optFlagDef with long = "kernel"
  , description = "Use drift Kernel in MCMC."
  })
let _pigeonsDefault : Bool = false
let _pigeons : OptParser Bool = optMap (xor _pigeonsDefault) (optFlag
  { optFlagDef with long = "pigeons"
  , description = "Let Pigeons.jl control inference via stdio."
  })
let _pigeonsGlobalDefault : Bool = true
let _pigeonsGlobal : OptParser Bool = optMap (xor _pigeonsGlobalDefault) (optFlag
  { optFlagDef with long = "pigeons-no-global"
  , description = "Requires --pigeons. Do not use global moves when sampling at temperature 0.0"
  })
let _pigeonsExploreStepsDefault : Int = 1
let _pigeonsExploreSteps : OptParser Int =
  let opt = optArg
    { optArgDefInt with long = "pigeons-explore-steps"
    , description = concat "Requires --pigeons. The number of local MCMC steps to take before communicating with Pigeons.jl. Default: " (int2string _pigeonsExploreStepsDefault)
    } in
  optOr opt (optPure _pigeonsExploreStepsDefault)
let _driftScaleDefault : Float = 1.0
let _driftScale : OptParser Float =
  let opt = optArg
    { optArgDefFloat with long = "drift"
    , description = concat "Floating point number which corresponds to the standard deviation (sigma) of the normal distribution that will be used for the automatic drift kernel. Default: " (float2string _driftScaleDefault)
    } in
  optOr opt (optPure _driftScaleDefault)
let _dynamicDelayDefault : Bool = false
let _dynamicDelay : OptParser Bool = optMap (xor _dynamicDelayDefault) (optFlag
  { optFlagDef with long = "dynamic-delay"
  , description = "Runs dynamic delayed sampling on the model."
  })
let _earlyStopDefault : Bool = true
let _earlyStop : OptParser Bool = optMap (xor _earlyStopDefault) (optFlag
  { optFlagDef with long = "no-early-stop"
  , description = "Disables early stopping in certain inference algorithms."
  })
let _mcmcLightweightGlobalProbDefault : Float = 0.1
let _mcmcLightweightGlobalProb : OptParser Float =
  let opt = optArg
    { optArgDefFloat with long = "mcmc-lw-gprob"
    , description = concat "The probability of performing a global MH step (non-global means only modify a single sample in the previous trace). Default: " (float2string _mcmcLightweightGlobalProbDefault)
    } in
  optOr opt (optPure _mcmcLightweightGlobalProbDefault)
let _particlesDefault : Int = 5000
let _particles : OptParser Int =
  let opt = optArg
    { optArgDefInt with long = "particles", short = "p"
    , description = concat "The number of particles (i.e., samples or iterations). The default is " (int2string _particlesDefault)
    } in
  optOr opt (optPure _particlesDefault)
let _pmcmcParticlesDefault : Int = 2
let _pmcmcParticles : OptParser Int =
  let opt = optArg
    { optArgDefInt with long = "pmcmcParticles"
    , description = concat "The number of particles for the smc proposal computation. The default is " (int2string _pmcmcParticlesDefault)
    } in
  optOr opt (optPure _pmcmcParticlesDefault)
let _pruneDefault : Bool = false
let _prune : OptParser Bool = optMap (xor _pruneDefault) (optFlag
  { optFlagDef with long = "prune"
  , description = "The model is pruned if possible."
  })
let _resampleDefault : String = "manual"
let _resample : OptParser String =
  let opt = optArg
    { optArgDefString with long = "resample"
    , description = concat "The selected resample placement method, for inference algorithms where applicable. The supported methods are: likelihood (resample immediately after all likelihood updates), align (resample after aligned likelihood updates, forces --align), and manual (sample only at manually defined resampling locations). Default: " _resampleDefault
    } in
  optOr opt (optPure _resampleDefault)
let _resampleFracDefault : Float = 0.7
let _resampleFrac : OptParser Float =
  let opt = optArg
    { optArgDefFloat with long = "resample-frac"
    , description = concat "Floating point number to trigger resampling for SMC-BPF when ESS is less than resampleFrac × particleCount. Default: " (float2string _resampleFracDefault)
    } in
  optOr opt (optPure _resampleFracDefault)
let _subsampleDefault : Bool = false
let _subsample : OptParser Bool = optMap (xor _subsampleDefault) (optFlag
  { optFlagDef with long = "subsample"
  , description = "Whether to subsample the posterior distribution."
  })
let _subsampleSizeDefault : Int = 1
let _subsampleSize : OptParser Int =
  let opt = optArg
    { optArgDefInt with long = "subsample-size", short = "n"
    , description = concat "The number of subsamples to draw if --subsample is selected. Default: " (int2string _subsampleSizeDefault)
    } in
  optOr opt (optPure _subsampleSizeDefault)

let _methodFlag : Bool -> String -> OptParser String = lam default. lam str.
  let opt = optMap (lam. str) (optSpecificArg {optExactArg str with short = "m"}) in
  if default then optOr opt (optPure str) else opt

-- Defines the basic components required in the implementation of an inference
-- method.
--
-- To define a new inference method constructor, the following semantic
-- functions must be implemented for the constructor:
-- 1. pprintInferMethod
-- 2. inferMethodFromCon
-- 3. _cmpInferMethod
-- 4. pickRuntime
-- 5. pickCompiler
-- 6. inferMethodConfig
-- 7. typeCheckInferMethod
-- 8. inferSmapAccumL
--
-- Additionally, if the inference method handles pruning or dynamic
-- delayed sampling, then `retainPruning` or
-- `retainDynamicDelayedSampling` should return true.
lang InferMethodBase =
  PrettyPrint + TypeCheck + Sym + MethodHelper

  syn InferMethod =
  | Default { runs: Expr }

  -- `pickRuntime` will be run once per unique `InferMethod`, where
  -- uniqueness is determined by this compare function. Essentially,
  -- this means that anything that the `pickRuntime` function looks at
  -- should be compared here, at least. Looking at more fields is not
  -- wrong, it just runs `pickRuntime` more times and stores the
  -- results.
  sem cmpInferMethod : InferMethod -> InferMethod -> Int
  sem cmpInferMethod lhs = | rhs -> _cmpInferMethod (lhs, rhs)
  sem _cmpInferMethod : (InferMethod, InferMethod) -> Int
  sem _cmpInferMethod =
  | (lhs, rhs) ->
    let res = subi (constructorTag lhs) (constructorTag rhs) in
    if eqi res 0
    then error "Compiler error: missing case for same constructorTag in _cmpInferMethod"
    else res

  sem pickRuntime : InferMethod -> (String, Map String String)
  sem pickCompiler : InferMethod -> InferenceInterface -> Expr

  sem eqInferMethod : InferMethod -> InferMethod -> Bool
  sem eqInferMethod lhs =
  | rhs -> eqi (cmpInferMethod lhs rhs) 0

  sem pprintInferMethod : Int -> PprintEnv -> InferMethod -> (PprintEnv, String)
  sem pprintInferMethod indent env =
  | Default { runs = runs } ->
    let i = pprintIncr indent in
    match pprintCode i env runs with (env, runs) in
    (env, join ["(Default {runs = ", runs, "})"])

  -- Constructs an inference method from the arguments of a TmConApp.
  sem inferMethodFromCon : Info -> Map SID Expr -> String -> InferMethod
  sem inferMethodFromCon info bindings =
  | "Default" ->
    let expectedFields = [
      ("runs", int_ _particlesDefault)
    ] in
    match getFields info bindings expectedFields with [runs] in
    Default { runs = runs }
  | s -> errorSingle [info] (concat "Unknown inference method: " s)

  -- Produces a record expression containing the configuration parameters of
  -- the inference method. This record is passed to the inference runtime
  -- function.
  sem inferMethodConfig : Info -> InferMethod -> Expr
  sem inferMethodConfig info =
  | Default { runs = runs } -> fieldsToRecord info [("runs", runs)]

  -- Produces the expected type of `inferMethodConfig`
  -- Type checks the inference method. This ensures that the provided arguments
  -- have the correct types.
  sem typeCheckInferMethod : TCEnv -> Info -> Type -> InferMethod -> InferMethod
  sem typeCheckInferMethod env info sampleType =
  | Default { runs = runs } ->
    let int = TyInt {info = info} in
    let runs = typeCheckExpr env runs in
    unify env [info, infoTm runs] int (tyTm runs);
    Default { runs = runs }

  sem retainPruning : InferMethod -> Bool
  sem retainPruning = | _ -> false
  sem retainDynamicDelayedSampling : InferMethod -> Bool
  sem retainDynamicDelayedSampling = | _ -> false

  -- Overrides the number of runs/iterations/particles in the InferMethod
  sem setRuns : Expr -> InferMethod -> InferMethod

  -- Map/Accum over expressions in inference method
  sem smapAccumL_InferMethod_Expr
    : all a. (a -> Expr -> (a, Expr)) -> a -> InferMethod -> (a, InferMethod)
  sem smapAccumL_InferMethod_Expr f acc =
  | Default r ->
    match f acc r.runs with (acc, runs) in (acc, Default {r with runs = runs})
  | m -> printLn (pprintInferMethod 0 pprintEnvEmpty m).1; error "fail"

  sem inferSmap_Expr_Expr : (Expr -> Expr) -> InferMethod -> InferMethod
  sem inferSmap_Expr_Expr f =| m ->
    (smapAccumL_InferMethod_Expr (lam. lam tm. ((), f tm)) () m).1

  sem inferSfold_Expr_Expr : all a. (a -> Expr -> a) -> a -> InferMethod -> a
  sem inferSfold_Expr_Expr f acc =| m ->
    (smapAccumL_InferMethod_Expr (lam acc. lam tm. (f acc tm, tm)) acc m).0

  sem _exprAsStringExn : Expr -> String
  sem _exprAsStringExn =
  | TmSeq x ->
    let f = lam c. match c with TmConst {val = CChar {val = c}} then Some c else None () in
    match optionMapM f x.tms with Some str then str else
    errorSingle [x.info] "This must be a literal string"
  | tm -> errorSingle [infoTm tm] "This must be a literal string"

  sem _exprAsBoolExn : Expr -> Bool
  sem _exprAsBoolExn =
  | TmConst {val = CBool {val = b}} -> b
  | tm -> errorSingle [infoTm tm] "This must be a literal boolean"

  sem _exprAsIntExn : Expr -> Int
  sem _exprAsIntExn =
  | TmConst {val = CInt {val = i}} -> i
  | tm -> errorSingle [infoTm tm] "This must be a literal integer"

  sem _exprAsFloatExn : Expr -> Float
  sem _exprAsFloatExn =
  | TmConst {val = CFloat {val = f}} -> f
  | tm -> errorSingle [infoTm tm] "This must be a literal float"

end
