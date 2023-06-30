include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Determines unit test sensitivity
let eqfe = eqfApprox 0.3 in
let teqf = lam t1. lam t2. if eqfe t1.0 t2.0 then eqfe t1.1 t2.1 else false in

let _test = lam smc. lam samples. lam b. lam compileArgs.
  let cpplRes = testCpplMExpr smc "regression.mc" compileArgs (int2string samples) in
  let cpplRes = burn b cpplRes in
  regressionMean cpplRes.lweights cpplRes.samples
in

utest _test true 1000 0 "-m is-lw --cps none" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m is-lw --cps partial" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m is-lw --cps partial --no-early-stop" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m is-lw --cps full" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m is-lw --cps full --no-early-stop" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-bpf --cps partial --resample manual" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-bpf --cps partial --resample align" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-bpf --cps partial --resample likelihood" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-bpf --cps full --resample manual" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-bpf --cps full --resample align" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-bpf --cps full --resample likelihood" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-apf --cps partial --resample manual" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-apf --cps partial --resample align" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-apf --cps partial --resample likelihood" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-apf --cps full --resample manual" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-apf --cps full --resample align" with regressionApproxTrue using teqf in
utest _test true 1000 0 "-m smc-apf --cps full --resample likelihood" with regressionApproxTrue using teqf in
utest _test false 1000 500 "-m pmcmc-pimh" with regressionApproxTrue using teqf in
utest _test false 10000 1000 "-m mcmc-trace" with regressionApproxTrue using teqf in
utest _test false 10000 1000 "-m mcmc-naive" with regressionApproxTrue using teqf in
utest _test false 1000 500 "-m mcmc-lightweight --align" with regressionApproxTrue using teqf in
utest _test false 1000 500 "-m mcmc-lightweight" with regressionApproxTrue using teqf in

()
