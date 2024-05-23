include "common.mc"

let model: () -> [Float] = lam.
  let f = lam t. lam xs.
    let x = get xs 0 in
    let v = get xs 1 in
    [v, (negf x)]
  in
  let x0 = [1., 0.] in
  solveode (RK4 { stepSize = 1e-3 }) f x0 3.

mexpr
model ()
