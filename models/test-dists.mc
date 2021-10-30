

include "seq.mc"
include "string.mc"
--include "dppl.mc"

mexpr

let x = assume (Beta 10.0 5.0) in
let y = assume (Bernoulli 0.5) in

let obs = true in
observe obs (Bernoulli x);
let dummy = observe obs (Bernoulli 0.8) in

print (join ["Beta: ", float2string x, "\n"]);
print (join ["Bernoulli: ", int2string y, "\n"]);
print (join ["Dummy pdf: ", float2string dummy, "\n"])
