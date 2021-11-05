
include "seq.mc"

mexpr

let x = assume (Beta 10.0 8.0) in
let obs = true in
observe obs (Bernoulli x);
observe true (Bernoulli x);
x
