

include "seq.mc"
include "string.mc"
include "dppl.mc"

mexpr

let x = assume (Beta 10.0 5.0) in
let y = assume (Bernoulli 0.7) in

let obs = true in
--observe obs (Bernoulli x);

print (join ["Beta: ", float2string x, "\n"]);
print (join ["Bernoulli: ", int2string y, "\n"])
