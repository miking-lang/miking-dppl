include "./bayesian-parameter-estimation.mc"

mexpr

match distEmpiricalSamples #var"Dist_θ" with (samples, weights) in
writeFile "bayesian-parameter-estimation-run.json"
  (jsonObject [
    ("weights", floatSeqToJson weights),
    ("samples", floatSeqToJson samples)
  ])
