mexpr
let x = assume (Beta 10.0 5.0) in
let obs = 10.0 in
observe obs (Exponential x);
x
