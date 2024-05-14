let model: () -> Float = lam.
  let a = assume (Gaussian 0.5 1.) in
  let f = lam x. addf (mulf a (pow x 2.)) x in
  let df = lam x. let t = diff f x in t 1. in
  observe (df 1.) (Gaussian 3. 1.);
  observe (df 2.) (Gaussian 5. 1.);
  observe (df 3.) (Gaussian 7. 1.);
  a

mexpr
model ()
