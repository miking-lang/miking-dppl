-- Tests for models/sprinkler.mc (MExpr backend)

include "../../models/sprinkler.mc"
include "../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Determines unit test sensitivity
let eqfe = eqfApprox 1e-1 in


-------------------------------------------------
-- Compile from scratch using top-level models --
-------------------------------------------------

let _booleanProb: [Bool] -> [Float] -> Float = lam samples. lam lweights.
  let samples: [Float] =
    map (lam b. if b then 1. else 0.) samples in
  logWeightedMean lweights samples
in
let _mean = lam cpplRes.
  _booleanProb (map string2bool cpplRes.samples) cpplRes.lweights
in
let _test =
  lam smc. lam compileArgs.
  _mean (testCpplMExpr smc "sprinkler.mc" compileArgs "1000")
in

utest _test true "-m is-lw --cps none" with sprinklerTrueProb using eqfe in
utest _test true "-m is-lw --cps partial" with sprinklerTrueProb using eqfe in
utest _test true "-m is-lw --cps partial --no-early-stop" with sprinklerTrueProb using eqfe in
utest _test true "-m is-lw --cps full" with sprinklerTrueProb using eqfe in
utest _test true "-m is-lw --cps full --no-early-stop" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-bpf --cps partial --resample manual" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-bpf --cps partial --resample align" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-bpf --cps partial --resample likelihood" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-bpf --cps full --resample manual" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-bpf --cps full --resample align" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-bpf --cps full --resample likelihood" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-apf --cps partial --resample manual" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-apf --cps partial --resample align" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-apf --cps partial --resample likelihood" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-apf --cps full --resample manual" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-apf --cps full --resample align" with sprinklerTrueProb using eqfe in
utest _test true "-m smc-apf --cps full --resample likelihood" with sprinklerTrueProb using eqfe in
utest _test false "-m pmcmc-pimh" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-trace" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-naive" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-lightweight --align" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-lightweight" with sprinklerTrueProb using eqfe in

-----------------------------------
-- Infers within CorePPL program --
-----------------------------------

let _meanD = lam dist.
  match distEmpiricalSamples dist with (vs,ws) in
  _booleanProb vs ws
in

-- -- models/coin.mc
utest _meanD (infer (Importance { particles = 1000 }) model)
with sprinklerTrueProb using eqfe in
utest _meanD (infer (BPF { particles = 1000 }) model)
with sprinklerTrueProb using eqfe in
utest _meanD (infer (APF { particles = 1000 }) model)
with sprinklerTrueProb using eqfe in
utest _meanD (infer (PIMH { particles = 10, iterations = 1000 }) model)
with sprinklerTrueProb using eqfe in
utest _meanD (infer (TraceMCMC { iterations = 1000 }) model)
with sprinklerTrueProb using eqfe in
utest _meanD (infer (NaiveMCMC { iterations = 1000 }) model)
with sprinklerTrueProb using eqfe in
utest _meanD (infer (LightweightMCMC { iterations = 1000,
                                       aligned = true,
                                       globalProb = 0.1 }) model)
with sprinklerTrueProb using eqfe in
utest _meanD (infer (LightweightMCMC { iterations = 1000,
                                       aligned = false,
                                       globalProb = 0.1 }) model)
with sprinklerTrueProb using eqfe in

()
