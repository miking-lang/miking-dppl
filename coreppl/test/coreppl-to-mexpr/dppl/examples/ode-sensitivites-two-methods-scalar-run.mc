include "ode-sensitivites-two-methods-scalar.mc"

mexpr

match distEmpiricalSamples #var"Dist_sθ" with (samples, weights) in
writeFile "ode-sensitivites-two-methods-scalar-run.json"
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
