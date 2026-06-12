include "rode.mc"

mexpr

match distEmpiricalSamples #var"Dist_RODE" with (samples, weights) in
writeFile (get argv 1)
  (let write = lam j : Int.
    seqToJson
      (create (length samples)
         (lam i : Int.
           seqToJson
             (map (lam s : (Float, Float). floatToJson s.1)
                (get (get samples i) j))))
   in
   jsonObject [
     ("xs", floatSeqToJson times),
     ("ys", write 0),
     ("ws", write 1)
   ])
