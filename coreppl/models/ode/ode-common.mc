-- Applies a function of two arguments elementwise over two sequences.
let map2 = lam f. lam seq1. lam seq2.
  create (length seq1) (lam i. f (get seq1 i) (get seq2 i))

let join = lam seq. foldl concat [] seq

-- Computes an ODE solution trace for the ODE `f`, inital values `xs0` and
-- time-steps `dts`.
let odeTrace : (Float -> [Float] -> [Float]) -> [Float] -> [Float] -> [[Float]] =
  lam f. lam xs0. lam dts.
    reverse
      (foldl
         (lam trace. lam dt.
           cons (solveode (RK4 { stepSize = 1e-3 }) f (head trace) dt) trace)
         [xs0] dts)
