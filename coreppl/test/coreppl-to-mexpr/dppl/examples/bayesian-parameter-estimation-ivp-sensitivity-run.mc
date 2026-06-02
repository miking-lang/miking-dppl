include "./bayesian-parameter-estimation-ivp-sensitivity.mc"

mexpr

match distEmpiricalSamples #var"Dist_z/dθ(future)" with (samples, weights) in
writeFile (get argv 1)
  (jsonObject [
    ("weights", floatSeqToJson weights),
    ("samples", floatSeqToJson samples)
  ])
