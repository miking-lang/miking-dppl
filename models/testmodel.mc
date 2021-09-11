

include "seq.mc"
include "ext/dist-ext.mc"

recursive
let sumSample = lam n:Int. lam acc:Float.
  if eqi n 0 then acc else sumSample (subi n 1) (addf acc (betaSample 2. 2.))
end

mexpr
  print (join ["Hello: ", float2string (sumSample 100000 0.) , "\n"])
