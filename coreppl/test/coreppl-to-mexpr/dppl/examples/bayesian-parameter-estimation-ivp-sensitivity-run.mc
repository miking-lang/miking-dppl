include "./bayesian-parameter-estimation-ivp-sensitivity.mc"

mexpr

match distEmpiricalSamples #var"Dist_z/dθ(future)" with (samples, weights) in
writeFile "bayesian-parameter-estimation-ivp-sensitivity-run.json"
  (jsonObject [
    ("weights", floatSeqToJson weights),
    ("samples", floatSeqToJson samples)
  ])
