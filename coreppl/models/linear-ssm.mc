let model = lam.
  let obs = [0.4,0.9,-0.1] in
  let xs = [assume (Gaussian 0. 1.)] in
  observe (get obs 0) (Gaussian (get xs 0) 1.);
  iteri (lam o. lam i.
    let x = assume (Gaussian (mulf 3. (get xs i)) 1.) in
    observe o (Gaussian x 1.)) (tail obs);
  get x 0

mexpr
model ()
