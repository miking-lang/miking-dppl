let model: () -> Float = lam.
  let k = 2. in
  let theta = 3. in
  let y = 2 in

  let x = assume (Gamma k theta) in
  observe y (Poisson x); -- statement
  x

mexpr
model ()