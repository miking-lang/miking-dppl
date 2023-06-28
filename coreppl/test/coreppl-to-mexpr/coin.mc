-- Tests for models/coin.mc (MExpr backend)

include "../../models/coin.mc"
include "../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Determines unit test sensitivity
let eqfe = eqfApprox 2e-2 in


-------------------------------------------------
-- Compile from scratch using top-level models --
-------------------------------------------------

let _floatMean = lam cpplRes.
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
in

-- models/coin.mc
let _testCoin =
  lam smc. lam compileArgs.
  _floatMean (testCpplMExpr smc "coin.mc" compileArgs "1000")
in

utest _testCoin true "-m is-lw --cps none" with coinTrueMean using eqfe in
utest _testCoin true "-m is-lw --cps partial" with coinTrueMean using eqfe in
utest _testCoin true "-m is-lw --cps partial --no-early-stop" with coinTrueMean using eqfe in
utest _testCoin true "-m is-lw --cps full" with coinTrueMean using eqfe in
utest _testCoin true "-m is-lw --cps full --no-early-stop" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-bpf --cps partial --resample manual" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-bpf --cps partial --resample align" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-bpf --cps partial --resample likelihood" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-bpf --cps full --resample manual" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-bpf --cps full --resample align" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-bpf --cps full --resample likelihood" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-apf --cps partial --resample manual" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-apf --cps partial --resample align" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-apf --cps partial --resample likelihood" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-apf --cps full --resample manual" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-apf --cps full --resample align" with coinTrueMean using eqfe in
utest _testCoin true "-m smc-apf --cps full --resample likelihood" with coinTrueMean using eqfe in
utest _testCoin false "-m pmcmc-pimh" with coinTrueMean using eqfe in
utest _testCoin false "-m mcmc-trace" with coinTrueMean using eqfe in
utest _testCoin false "-m mcmc-naive" with coinTrueMean using eqfe in
utest _testCoin false "-m mcmc-lightweight --align" with coinTrueMean using eqfe in
utest _testCoin false "-m mcmc-lightweight" with coinTrueMean using eqfe in

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
utest _floatMeanD (infer (Importance { particles = 1000 }) model)
with coinTrueMean using eqfe in
utest _floatMeanD (infer (BPF { particles = 1000 }) model)
with coinTrueMean using eqfe in
utest _floatMeanD (infer (APF { particles = 1000 }) model)
with coinTrueMean using eqfe in
utest _floatMeanD (infer (PIMH { particles = 10, iterations = 100 }) model)
with coinTrueMean using eqfe in
utest _floatMeanD (infer (TraceMCMC { iterations = 1000 }) model)
with coinTrueMean using eqfe in
utest _floatMeanD (infer (NaiveMCMC { iterations = 1000 }) model)
with coinTrueMean using eqfe in
utest _floatMeanD (infer (LightweightMCMC { iterations = 1000,
                                            aligned = true,
                                            globalProb = 0.1 }) model)
with coinTrueMean using eqfe in
utest _floatMeanD (infer (LightweightMCMC { iterations = 1000,
                                            aligned = false,
                                            globalProb = 0.1 }) model)
with coinTrueMean using eqfe in

()
