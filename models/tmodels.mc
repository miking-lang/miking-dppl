mexpr
let x = assume (Beta 10.0 5.0) in
let obs = true in
observe obs (Bernoulli x);
x
