include "./bayesian-parameter-estimation-ivp-solution.mc"

mexpr

match distEmpiricalSamples #var"Dist_z(future)" with (samples, weights) in
writeFile (get argv 1)
  (jsonObject [
    ("weights", floatSeqToJson weights),
    ("samples", floatSeqToJson samples)
  ])
