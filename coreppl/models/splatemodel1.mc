mexpr
let params = [(10.0,10.0), (15.0,1.0), (21.0,10.0)] in
let rvs = plate (lam x. assume (Beta x.0 x.1)) params in
let obs = true in
plate (lam x. observe obs (Bernoulli x)) rvs;
rvs
