include "tumor-inhibitor-rode.mc"

mexpr

match distEmpiricalSamples #var"Dist_RODE" with (samples, weights) in
writeFile "tumor-inhibitor-rode-run.json"
  (let write = lam j : Int.
    seqToJson
      (create (length samples)
         (lam i : Int.
           seqToJson
             (map (lam s : (Float, (Float, Float, Float)).
               match s.1 with (x0, x1, x2) in floatSeqToJson [x0, x1, x2])
                (get (get samples i) j))))
   in
   jsonObject [
     ("xs", floatSeqToJson times),
     ("sol", write 0),
     ("sens", write 1),
     ("ws", write 2)
   ])
