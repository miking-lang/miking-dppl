-- Runtime support for first-class distributions in coreppl-to-mexpr compiler.

include "ext/dist-ext.mc"

type Dist a = { sample: () -> a, logObserve: a -> Float }

let distBinomial : Int -> Float -> Dist Int = lam n. lam p.
  { sample = lam. binomialSample p n, logObserve = binomialLogPmf p n }
let distBernoulli : Float -> Dist Bool = lam p.
  { sample = lam. bernoulliSample p, logObserve = bernoulliLogPmf p }
let distBeta : Float -> Float -> Dist Float = lam a. lam b.
  { sample = lam. betaSample a b, logObserve = betaLogPdf a b }
let distGaussian : Float -> Float -> Dist Float = lam mu. lam sigma.
  { sample = lam. gaussianSample mu sigma, logObserve = gaussianLogPdf mu sigma }
let distMultinomial : Int -> [Float] -> Dist [Float] = lam n. lam p.
  { sample = lam. multinomialSample n p, logObserve = multinomialLogPmf n p }
let distCategorical: [Float] -> Dist Int = lam p.
  { sample = lam. categoricalSample p, logObserve = categoricalLogPmf p }
let distDirichlet: [Float] -> Dist Float = lam a.
  { sample = lam. dirichletSample a, logObserve = dirichletLogPdf a }
let distUniform: Float -> Float -> Dist Float = lam a. lam b.
  { sample = lam. uniformContinuousSample a b, logObserve = uniformContinuousLogPdf a b }
