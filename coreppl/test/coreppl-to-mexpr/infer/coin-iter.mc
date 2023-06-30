include "../../../models/coin-iter.mc"
include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Determines unit test sensitivity
let eqfe = eqfApprox 2e-2 in

let _test = lam dist.
  match distEmpiricalSamples dist with (vs,ws) in
  logWeightedMean ws vs
in

utest _test (infer (Importance { particles = 1000 }) model) with coinTrueMean using eqfe in
utest _test (infer (BPF { particles = 1000 }) model) with coinTrueMean using eqfe in
utest _test (infer (APF { particles = 1000 }) model) with coinTrueMean using eqfe in
utest _test (infer (PIMH { particles = 10, iterations = 100 }) model) with coinTrueMean using eqfe in
utest _test (infer (TraceMCMC { iterations = 1000 }) model) with coinTrueMean using eqfe in
utest _test (infer (NaiveMCMC { iterations = 1000 }) model) with coinTrueMean using eqfe in
utest _test (infer (LightweightMCMC { iterations = 1000, aligned = true, globalProb = 0.1 }) model) with coinTrueMean using eqfe in
utest _test (infer (LightweightMCMC { iterations = 1000, aligned = false, globalProb = 0.1 }) model) with coinTrueMean using eqfe in

()
