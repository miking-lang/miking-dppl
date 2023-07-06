include "seq.mc"

let model: () -> String = lam.
  let data = [(1.1,1.1), (2.4, 1.7), (3.7, 1.9), (4.4, 2.7)] in
  let k = assume (Gaussian 0.0 5.0) in
  let m = assume (Gaussian 0.0 5.0) in
  let sigma = assume (Gamma 1.0 1.0) in
  iter (lam point.
    observe point.1 (Gaussian (addf m (mulf k point.0)) sigma)
  ) data;
  join [(float2string k), " ", (float2string m)]

mexpr
model ()
