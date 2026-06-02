include "./bayesian-parameter-estimation.mc"

mexpr

match distEmpiricalSamples #var"Dist_θ" with (samples, weights) in
writeFile (get argv 1)
  (jsonObject [
    ("weights", floatSeqToJson weights),
    ("samples", floatSeqToJson samples)
  ])
