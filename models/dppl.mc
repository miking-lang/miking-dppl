

include "ext/dist-ext.mc"

-- Fool the code elimination and include all distributions
-- (works because some of the below has side effects)
let _includeBinomial = binomialPmf; binomialLogPmf; binomialSample; bernoulliPmf;
     bernoulliLogPmf; bernoulliSample
let _includeBeta = betaPdf; betaLogPdf; betaSample
let _includeGaussian = gaussianPdf; gaussianLogPdf; gaussianSample
let _includeUniform = uniformSample
let _includeRandom = randomSample
