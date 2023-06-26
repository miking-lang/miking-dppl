let model = lam.
  let theta = assume (Beta 10.0 10.0) in
  observe true (Bernoulli theta);
  observe false (Bernoulli theta);
  observe true (Bernoulli theta);
  theta

mexpr
model ()
