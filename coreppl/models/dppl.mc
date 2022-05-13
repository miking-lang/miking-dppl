

include "ext/dist-ext.mc"

-- Fools the code elimination and includes all distributions
-- (works because some of the functions below have side effects)
let _includeBinomial = binomialPmf; binomialLogPmf; binomialSample; bernoulliPmf
let _includeBernoulli = bernoulliLogPmf; bernoulliSample
let _includeBeta = betaPdf; betaLogPdf; betaSample
let _includeGaussian = gaussianPdf; gaussianLogPdf; gaussianSample
let _includeUniform = uniformSample
let _includeRandom = randomSample
