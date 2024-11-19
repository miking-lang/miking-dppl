include "common.mc"
include "seq.mc"

type Method
con RungeKutta : () -> Method
con EulerForward : () -> Method
con Default : () -> Method

let model: Method -> () -> [Float] = lam m. lam.
  let f = lam t. lam xs.
    let x = get xs 0 in
    let v = get xs 1 in
    [v, (negf x)]
  in
  let x0 = [1., 0.] in
  switch m
  case RungeKutta _ then
    solveode
      (RK4 { stepSize = 1e-3, add = zipWith addf, smul = lam s. map (mulf s) })
      f x0 3.
  case EulerForward _ then
    solveode
      (EF { stepSize = 1e-3, add = zipWith addf, smul = lam s. map (mulf s) })
      f x0 3.
  case _ then
    solveode
      (Default { stepSize = 1e-3, add = zipWith addf, smul = lam s. map (mulf s) })
      f x0 3.
  end
