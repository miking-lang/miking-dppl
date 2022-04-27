
include "bool.mc"
include "math.mc"

-- Help function that is needed in models
recursive
let lnFactorial: Int -> Float = lam n: Int.
  if eqi n 1 then 0.
  else addf (log (int2float n)) (lnFactorial (subi n 1))
end
