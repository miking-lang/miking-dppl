mexpr
let theta = assume (Beta 10.0 10.0) in
let r = map (lam obs. observe obs (Bernoulli theta)) [true,false,true] in
let res = theta in
res
