let model: () -> Float = lam.
  let theta = assume (Beta 3.0 5.0) in
  iter (lam obs. observe obs (Bernoulli theta)) [true, false, false, true, true, true, true, false, false, true];
  theta

mexpr
model ()