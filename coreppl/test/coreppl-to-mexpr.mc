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

let _floatLMean = lam cpplRes.
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
in

-- models/coin.mc
let res = testCpplMExpr true "coin.mc" "-m is-lw" "1000" in
utest _floatLMean res with coinTrueMean using eqfe in
let res = testCpplMExpr true "coin.mc" "-m smc-bpf" "1000" in
utest _floatLMean res with coinTrueMean using eqfe in
let res = testCpplMExpr true "coin.mc" "-m smc-apf" "1000" in
utest _floatLMean res with coinTrueMean using eqfe in
let res = testCpplMExpr false "coin.mc" "-m pmcmc-pimh" "1000" in
utest _floatLMean res with coinTrueMean using eqfe in
let res = testCpplMExpr false "coin.mc" "-m mcmc-trace" "1000" in
utest _floatLMean res with coinTrueMean using eqfe in
let res = testCpplMExpr false "coin.mc" "-m mcmc-naive" "1000" in
utest _floatLMean res with coinTrueMean using eqfe in
let res = testCpplMExpr false "coin.mc" "-m mcmc-lightweight --align" "1000" in
utest _floatLMean res with coinTrueMean using eqfe in
let res = testCpplMExpr false "coin.mc" "-m mcmc-lightweight" "1000" in
utest _floatLMean res with coinTrueMean using eqfe in


-----------------------------------
-- Infers within CorePPL program --
-----------------------------------

let _floatLMeanC = lam dist.
  match distEmpiricalSamples dist with (vs,ws) in
  logWeightedMean ws vs
in

-- models/coin.mc
let d = infer (Importance {particles = 1000}) model in
utest _floatLMeanC d with coinTrueMean using eqfe in
let d = infer (BPF {particles = 1000}) model in
utest _floatLMeanC d with coinTrueMean using eqfe in
let d = infer (APF {particles = 1000}) model in
utest _floatLMeanC d with coinTrueMean using eqfe in
let d = infer (PIMH {particles = 10, iterations = 100}) model in
utest _floatLMeanC d with coinTrueMean using eqfe in
let d = infer (TraceMCMC { iterations = 1000 }) model in
utest _floatLMeanC d with coinTrueMean using eqfe in
let d = infer (NaiveMCMC { iterations = 1000 }) model in
utest _floatLMeanC d with coinTrueMean using eqfe in
let d = infer (LightweightMCMC { iterations = 1000, aligned = true, globalProb = 0.1 }) model in
utest _floatLMeanC d with coinTrueMean using eqfe in
let d = infer (LightweightMCMC { iterations = 1000, aligned = false, globalProb = 0.1 }) model in
utest _floatLMeanC d with coinTrueMean using eqfe in

()
