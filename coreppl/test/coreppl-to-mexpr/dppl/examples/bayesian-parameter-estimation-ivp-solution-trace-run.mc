include "bayesian-parameter-estimation-ivp-solution-trace.mc"

mexpr

let true_y_trace = trace true_y (x0, y0) timesExt in

match distEmpiricalSamples #var"Dist_y_trace" with (samples, weights) in
writeFile (get argv 1)
  (jsonObject [
    ("weights", (seqToJson (map floatToJson weights))),
    ("xs", (seqToJson (map floatToJson timesExt))),
    ("trueTrace",
     floatSeqToJson2 (map (lam x : (Float, [Float]). x.1) true_y_trace)),
    ("trace",
     floatSeqToJson3 (map (lam x : [(Float, [Float])]. map (lam t : (Float, [Float]). t.1) x) samples))
  ])
