let dt = 1.
let ts = create 100 (lam i. mulf (int2float i) dt)
let model : () -> [(Float, [Float])] = lam.
  let w = assume (Wiener ()) in
  map (lam t. (t, [w t])) ts

mexpr
model ()
