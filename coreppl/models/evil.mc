include "ext/dist-ext.mc"
include "ext/math-ext.mc"

mexpr

let x = log (betaSample 1.0 2.0) in
let y = assume (Beta 10.0 5.0) in
let obs = true in
observe obs (Bernoulli y);
y
