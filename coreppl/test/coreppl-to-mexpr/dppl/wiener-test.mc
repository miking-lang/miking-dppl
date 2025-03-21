include "./lib.mc"

let model = lam t : ().
  let w = assume (Wiener ()) in
  w 1.

mexpr

let eq = eqfApprox 1e-1 in

utest
  let dist = infer (Importance { particles = 1000 }) model in
  expectation dist
  with 0.
  using eq
in
utest
  let dist = infer (BPF { particles = 1000 }) model in
  expectation dist
  with 0.
  using eq
in
utest
  let dist = infer (APF { particles = 1000 }) model in
  expectation dist
  with 0.
  using eq
in

()
