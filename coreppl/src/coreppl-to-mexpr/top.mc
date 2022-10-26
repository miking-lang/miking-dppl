-- This file containing _CorePPL_ (not MExpr) code defines functions used when
-- compiling CorePPL in "global" mode, where `infer` is not used and the entire
-- program is the model.

-- Needed for int2string
include "string.mc"

-- Needed for bool2string
include "bool.mc"

-- Needed for repeat
include "common.mc"

-- Print samples
let printSamples = lam printFun. lam dist.
  recursive let rec = lam weights. lam samples.
    match (weights, samples) with ([w] ++ weights, [s] ++ samples) then
      print (printFun s);
      print " ";
      print (float2string w);
      print "\n";
      rec weights samples
    else ()
  in
  match distEmpiricalSamples dist with (samples, weights) in
  rec weights samples

-- Print normalizing constant
let printNormConst = lam dist.
  print (float2string (distEmpiricalNormConst dist)); print "\n"

-- Print accept rate
let printAcceptRate = lam dist.
  print (float2string (distEmpiricalAcceptRate dist)); print "\n"

-- The number of particles/samples/executions and sweeps from the program argument
let particles = if leqi (length argv) 1 then particles else string2int (get argv 1)

let sweeps = if leqi (length argv) 2 then 1 else string2int (get argv 2)
