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
  let t0 = 0. in
  -- NOTE(vipa, 2026-06-10): We insert a resample to allow SMC
  -- algorithms to run this model, even though it doesn't matter at
  -- all for this particular model
  resample;
  let x0 = [1., 0.] in
  switch m
  case RungeKutta _ then
    (solveode
       (RK4 { stepSize = 1e-3, add = zipWith addf, smul = lam s. map (mulf s) })
       f (t0, x0) 3.).1
  case EulerForward _ then
    (solveode
       (EF { stepSize = 1e-3, add = zipWith addf, smul = lam s. map (mulf s) })
       f (t0, x0) 3.).1
  case _ then
    (solveode
       (Default { stepSize = 1e-3, add = zipWith addf, smul = lam s. map (mulf s) })
       f (t0, x0) 3.).1
  end
