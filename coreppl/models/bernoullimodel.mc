mexpr
let n = 10 in
let theta = 5.0 in
recursive let assumeLst =
  lam n. lam samples. if eqi n 0 then samples else assumeLst (subi n 1) (cons (assume (Bernoulli theta)) samples)
in
assumeLst n theta
