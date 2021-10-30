
--include "dppl.mc"
include "seq.mc"

mexpr

let x = assume (Beta 10.0 5.0) in
let obs = true in
--observe obs (Bernoulli x);
print (join ["x = ", float2string x, "\n"]);
[x]
