include "../models/coin.mc"
include "test.mc"

include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let eqfe = eqfApprox 1e-2 in

-- Test first-class infer
let dist = infer (Importance {particles = 1000}) model in
match distEmpiricalSamples dist with (vs,ws) in
let trueMean = divf 12.0 23.0 in
utest logWeightedMean ws vs with trueMean using eqfe in


-- Test command line

()
