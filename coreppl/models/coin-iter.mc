let model: () -> Float = lam.
  let theta = assume (Beta 10.0 10.0) in
  iter (lam obs. observe obs (Bernoulli theta)) [true,false,true];
  theta

mexpr
model ()
