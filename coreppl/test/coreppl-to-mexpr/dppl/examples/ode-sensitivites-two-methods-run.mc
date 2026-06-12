include "ode-sensitivites-two-methods.mc"

mexpr

match distEmpiricalSamples #var"Dist_dy/dθ" with (samples, weights) in
writeFile (get argv 1)
  (jsonObject [
    ("xs", seqToJson (map floatToJson times)),
    ("samples",
     floatSeqToJson4
       (map
          (lam x : [[(Float, [Float])]].
            map
              (lam x : [(Float, [Float])].
                map
                  (lam t : (Float, [Float]). t.1)
                  x)
              x)
          samples))
  ])
