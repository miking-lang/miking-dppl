-- Runtime support for first-class distributions in coreppl-to-mexpr compiler.

include "math.mc"
include "ext/dist-ext.mc"

type Dist a = { sample: () -> a, logObserve: a -> Float }

let distGamma : Float -> Float -> Dist Float = lam shape. lam scale.
  { sample = lam. gammaSample shape scale, logObserve = gammaLogPdf shape scale }
let distExponential : Float -> Dist Float = lam rate.
  { sample = lam. exponentialSample rate, logObserve = exponentialLogPdf rate }
let distPoisson : Float -> Dist Int = lam lambda.
  { sample = lam. poissonSample lambda, logObserve = poissonLogPmf lambda }
let distBinomial : Int -> Float -> Dist Int = lam n. lam p.
  { sample = lam. binomialSample p n, logObserve = binomialLogPmf p n }
let distBernoulli : Float -> Dist Bool = lam p.
  { sample = lam. bernoulliSample p, logObserve = bernoulliLogPmf p }
let distBeta : Float -> Float -> Dist Float = lam a. lam b.
  { sample = lam. betaSample a b, logObserve = betaLogPdf a b }
let distGaussian : Float -> Float -> Dist Float = lam mu. lam sigma.
  { sample = lam. gaussianSample mu sigma, logObserve = gaussianLogPdf mu sigma }
let distMultinomial : Int -> [Float] -> Dist [Int] = lam n. lam p.
  { sample = lam. multinomialSample p n,
    logObserve = lam o.
      if eqi n (foldl1 addi o) then multinomialLogPmf p o else negf (inf) }
let distCategorical: [Float] -> Dist Int = lam p.
  { sample = lam. categoricalSample p, logObserve = categoricalLogPmf p }
let distDirichlet: [Float] -> Dist [Float] = lam a.
  { sample = lam. dirichletSample a, logObserve = dirichletLogPdf a }
let distUniform: Float -> Float -> Dist Float = lam a. lam b.
  { sample = lam. uniformContinuousSample a b, logObserve = uniformContinuousLogPdf a b }
