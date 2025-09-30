include "./bayesian-parameter-estimation-ivp-solution.mc"

mexpr

match distEmpiricalSamples #var"Dist_z(future)" with (samples, weights) in
writeFile "bayesian-parameter-estimation-ivp-solution-run.json"
  (jsonObject [
    ("weights", floatSeqToJson weights),
    ("samples", floatSeqToJson samples)
  ])
