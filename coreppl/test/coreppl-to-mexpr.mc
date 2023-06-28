-- Tests for models/coin.mc (MExpr backend)

include "../models/coin.mc"
include "test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Determines unit test sensitivity
let eqfe = eqfApprox 1e-2 in


-------------------------------------------------
-- Compile from scratch using top-level models --
-------------------------------------------------

let _floatMean = lam cpplRes.
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
in

-- models/coin.mc
utest _floatMean (testCpplMExpr true "coin.mc" "-m is-lw --cps none" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr true "coin.mc" "-m is-lw --cps partial" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr true "coin.mc" "-m is-lw --cps partial --no-early-stop" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr true "coin.mc" "-m is-lw --cps full" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr true "coin.mc" "-m is-lw --cps full --no-early-stop" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr true "coin.mc" "-m smc-bpf" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr true "coin.mc" "-m smc-bpf" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr true "coin.mc" "-m smc-apf" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr false "coin.mc" "-m pmcmc-pimh" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr false "coin.mc" "-m mcmc-trace" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr false "coin.mc" "-m mcmc-naive" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr false "coin.mc" "-m mcmc-lightweight --align" "1000") with coinTrueMean using eqfe in
utest _floatMean (testCpplMExpr false "coin.mc" "-m mcmc-lightweight" "1000") with coinTrueMean using eqfe in

-- models/sprinkler.mc

-- models/ssm.mc

-- models/diversification-models/crbd.mc

-- models/diversification-models/clads2.mc

-- models/latent-dirichlet-allocation/lda.mc

-- models/vector-borne-disease/vbd.mc

-----------------------------------
-- Infers within CorePPL program --
-----------------------------------

let _floatMeanD = lam dist.
  match distEmpiricalSamples dist with (vs,ws) in
  logWeightedMean ws vs
in

-- -- models/coin.mc
let r = infer (Importance { particles = 1000 }) coinModel in
utest _floatMeanD r
with coinTrueMean using eqfe in
utest _floatMeanD (infer (BPF { particles = 1000 }) coinModel)
with coinTrueMean using eqfe in
-- utest _floatMeanD (infer (APF { particles = 1000 }) coinModel)
-- with coinTrueMean using eqfe in
-- utest _floatMeanD (infer (PIMH { particles = 10, iterations = 100 }) coinModel)
-- with coinTrueMean using eqfe in
-- utest _floatMeanD (infer (TraceMCMC { iterations = 1000 }) coinModel)
-- with coinTrueMean using eqfe in
-- utest _floatMeanD (infer (NaiveMCMC { iterations = 1000 }) coinModel)
-- with coinTrueMean using eqfe in
-- utest _floatMeanD (infer (LightweightMCMC { iterations = 1000,
--                                             aligned = true,
--                                             globalProb = 0.1 }) coinModel)
-- with coinTrueMean using eqfe in
-- utest _floatMeanD (infer (LightweightMCMC { iterations = 1000,
--                                             aligned = false,
--                                             globalProb = 0.1 }) coinModel)
-- with coinTrueMean using eqfe in

()
