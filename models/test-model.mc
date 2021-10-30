
--include "dppl.mc"
include "seq.mc"
include "string.mc"

mexpr

let x = assume (Beta 10.0 5.0) in
let y = assume (Bernoulli 0.6) in
let obs = true in
--observe obs (Bernoulli x);
print (join ["x = ", float2string x, "\n"]);
print (join ["y = ", int2string y, "\n"]);
[x]
