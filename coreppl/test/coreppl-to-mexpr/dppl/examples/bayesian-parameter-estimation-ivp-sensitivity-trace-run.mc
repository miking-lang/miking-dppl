include "bayesian-parameter-estimation-ivp-sensitivity-trace.mc"

mexpr

let #var"true_dy/dθ_trace" =
  diff (lam #var"true_θ" : FloatA. trace (y #var"true_θ") (x0, y0) timesExt)
    #var"true_θ" 1. in

match distEmpiricalSamples #var"Dist_dy/dθ_trace" with (samples, weights) in
writeFile (get argv 1)
  (jsonObject [
    ("weights", (seqToJson (map floatToJson weights))),
    ("xs", (seqToJson (map floatToJson timesExt))),
    ("trueTrace",
     floatSeqToJson2 (map (lam x : (Float, [Float]). x.1) #var"true_dy/dθ_trace")),
    ("trace",
     floatSeqToJson3 (map (lam x : [(Float, [Float])]. map (lam t : (Float, [Float]). t.1) x) samples))
  ])
