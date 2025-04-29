let model: () -> Int = lam.
  let geo = assume (Geometric 1.) in
  observe geo (Binomial 1 1.);
  geo

mexpr
model ()
