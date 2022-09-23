mexpr
let theta = assume (Beta 10.0 10.0) in
map (lam obs. observe obs (Bernoulli theta)) [true,false,true];
theta
