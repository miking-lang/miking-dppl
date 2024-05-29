mexpr
let a = assume (Gaussian 0. 1.) in
iter (lam obs. observe obs (Gaussian a 1.)) [0.3,0.4,0.5]