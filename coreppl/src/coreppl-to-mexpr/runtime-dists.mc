-- Runtime support for first-class distributions in coreppl-to-mexpr compiler.

include "math.mc"
include "seq.mc"
include "ext/dist-ext.mc"

-- Base interface
lang RuntimeDistBase
  syn Dist a =

  sem sample : all a. Dist a -> a

  sem logObserve : all a. Dist a -> (a -> Float)
end

-- Elementary distributions
lang RuntimeDistElementary = RuntimeDistBase
  syn Dist a =
  | DistGamma {shape : Float, scale : Float}
  | DistExponential {rate : Float}
  | DistPoisson {lambda : Float}
  | DistBinomial {n : Int, p : Float}
  | DistBernoulli {p : Float}
  | DistBeta {a : Float, b : Float}
  | DistGaussian {mu : Float, sigma : Float}
  | DistMultinomial {n : Int, p : [Float]}
  | DistCategorical {p : [Float]}
  | DistDirichlet {a : [Float]}
  | DistUniform {a : Float, b : Float}

  sem sample =
  | DistGamma t -> unsafeCoerce (gammaSample t.shape t.scale)
  | DistExponential t -> unsafeCoerce (exponentialSample t.rate)
  | DistPoisson t -> unsafeCoerce (poissonSample t.lambda)
  | DistBinomial t -> unsafeCoerce (binomialSample t.p t.n)
  | DistBernoulli t -> unsafeCoerce (bernoulliSample t.p)
  | DistBeta t -> unsafeCoerce (betaSample t.a t.b)
  | DistGaussian t -> unsafeCoerce (gaussianSample t.mu t.sigma)
  | DistMultinomial t -> unsafeCoerce (multinomialSample t.p t.n)
  | DistCategorical t -> unsafeCoerce (categoricalSample t.p)
  | DistDirichlet t -> unsafeCoerce (dirichletSample t.a)
  | DistUniform t -> unsafeCoerce (uniformContinuousSample t.a t.b)

  sem logObserve =
  | DistGamma t -> unsafeCoerce (gammaLogPdf t.shape t.scale)
  | DistExponential t -> unsafeCoerce (exponentialLogPdf t.rate)
  | DistPoisson t -> unsafeCoerce (poissonLogPmf t.lambda)
  | DistBinomial t -> unsafeCoerce (binomialLogPmf t.p t.n)
  | DistBernoulli t -> unsafeCoerce (bernoulliLogPmf t.p)
  | DistBeta t -> unsafeCoerce (betaLogPdf t.a t.b)
  | DistGaussian t -> unsafeCoerce (gaussianLogPdf t.mu t.sigma)
  | DistMultinomial t ->
    unsafeCoerce (lam o.
      if eqi t.n (foldl1 addi o) then multinomialLogPmf t.p o
      else negf inf)
  | DistCategorical t -> unsafeCoerce (categoricalLogPmf t.p)
  | DistDirichlet t -> unsafeCoerce (dirichletLogPdf t.a)
  | DistUniform t -> unsafeCoerce (uniformContinuousLogPdf t.a t.b)
end

-- Empirical distribution
lang RuntimeDistEmpirical = RuntimeDistBase
  syn Dist a =
  | DistEmpirical {
      cumulativeWeights : [Float],
      logWeights : [Float],
      samples : [a],
      degenerate : Bool,

      -- NOTE(dlunde,2022-10-18): Monte Carlo inference often returns futher
      -- information (not only the samples). This field is used for this.
      extra : EmpiricalExtra
    }

  -- TODO(dlunde,2022-10-18): Would probably be nice to specify this per
  -- inference algorithm (in their respective subfolders)
  syn EmpiricalExtra =
  | EmpNorm { normConst: Float }
  | EmpMCMC { acceptRate: Float }

  sem constructDistEmpiricalHelper =
  | samples ->
    match unzip samples with (logWeights, samples) in
    let extra = EmpNorm {normConst = 0.0} in
    constructDistEmpirical samples logWeights extra

  -- DistEmpirical should always be created via this function
  sem constructDistEmpirical samples logWeights =
  | extra ->

    -- print "INPUT: "; printLn (strJoin "," (map float2string logWeights));

    -- Compute LSE
    let maxLogWeight = foldl (lam acc. lam lw. if geqf lw acc then lw else acc)
                         (negf inf) logWeights in

    let degenerate = eqf maxLogWeight (negf inf) in

    -- print "MAX: "; printLn (float2string maxLogWeight);
    let lse =
      addf maxLogWeight
        (log (foldl (lam acc. lam lw. addf acc (exp (subf lw maxLogWeight)))
                0. logWeights))
    in
    -- print "LSE: "; printLn (float2string lse);

    -- Normalize logweights (so that the new LSE = 0)
    let logWeights = map (lam lw. subf lw lse) logWeights in
    -- print "LOG-NORMALIZED: "; printLn (strJoin "," (map float2string logWeights));

    -- Computes cumulative (non-log) weights
    let f = lam acc. lam x.
      let acc = addf acc (exp x) in
      (acc, acc)
    in
    match mapAccumL f 0.0 logWeights with (_, cumulativeWeights) in
    -- print "CUMULATIVE NORMALIZED: "; printLn (strJoin "," (map float2string cumulativeWeights));

    DistEmpirical {
      cumulativeWeights = cumulativeWeights, logWeights = logWeights,
      degenerate = degenerate, samples = samples, extra = extra
    }

  sem empiricalSamples =
  | DistEmpirical t -> (t.samples, t.logWeights)
  | _ -> ([], [])

  sem empiricalDegenerate =
  | DistEmpirical t -> t.degenerate
  | _ -> false

  sem empiricalNormConst =
  | DistEmpirical t ->
    match t.extra with EmpNorm { normConst = normConst } then normConst else nan
  | _ -> nan

  sem empiricalAcceptRate =
  | DistEmpirical t ->
    match t.extra with EmpMCMC { acceptRate = acceptRate }
    then acceptRate else nan
  | _ -> nan

  sem sample =
  | DistEmpirical t ->
    -- NOTE(larshum, 2023-05-03): Sample by choosing a value in range [0, y)
    -- and finding the index of the maximal cumulative weight which is less
    -- than the chosen value. The sample at this index is returned. Note that
    -- we use the final cumulative weight rather than 1 for y, to prevent
    -- rounding errors from skewing the probability of the last sample.
    let x = uniformContinuousSample 0.0 (last t.cumulativeWeights) in

    -- NOTE(larshum, 2023-05-03): The sampled value 'x' cannot be greater than
    -- the last element of the sequence of cumulative weights, so we should
    -- always be able to find an index for which the comparison function
    -- returns a non-negative number.
    let cmp = lam y. if ltf (subf y x) 0.0 then negi 1 else 0 in
    match lowerBoundBinarySearch cmp t.cumulativeWeights with Some idx then
      unsafeCoerce (get t.samples idx)
    else
      error "Sampling from empirical distribution failed"

  sem logObserve =
  -- TODO(dlunde,2022-10-18): Implement this?
  | DistEmpirical t -> error "Log observe not supported for empirical distribution"
end

lang RuntimeDist = RuntimeDistElementary + RuntimeDistEmpirical
end

-- We include the below definitions to produce non-mangled functions, which we
-- can refer to in the runtime handler without hard-coding the mangled prefix.
let distEmpiricalSamples : all a. Dist a -> ([a], [Float]) =
  use RuntimeDist in
  empiricalSamples

let distEmpiricalDegenerate : all a. Dist a -> Bool =
  use RuntimeDist in
  empiricalDegenerate

let distEmpiricalNormConst : all a. Dist a -> Float =
  use RuntimeDist in
  empiricalNormConst

let distEmpiricalAcceptRate : all a. Dist a -> Float =
  use RuntimeDist in
  empiricalAcceptRate

let sample : all a. Dist a -> a =
  use RuntimeDist in
  sample

let logObserve : all a. Dist a -> a -> Float =
  use RuntimeDist in
  logObserve
