include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Determines unit test sensitivity
let eqfe = eqfApprox 2e-2 in

let _test = lam smc. lam compileArgs.
  let cpplRes = testCpplMExpr smc "coin-iter.mc" compileArgs "1000" in
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
in

utest _test true "-m is-lw --cps none" with coinTrueMean using eqfe in
utest _test true "-m is-lw --cps partial" with coinTrueMean using eqfe in
utest _test true "-m is-lw --cps partial --no-early-stop" with coinTrueMean using eqfe in
utest _test true "-m is-lw --cps full" with coinTrueMean using eqfe in
utest _test true "-m is-lw --cps full --no-early-stop" with coinTrueMean using eqfe in
utest _test true "-m smc-bpf --cps partial --resample manual" with coinTrueMean using eqfe in
utest _test true "-m smc-bpf --cps partial --resample align" with coinTrueMean using eqfe in
utest _test true "-m smc-bpf --cps partial --resample likelihood" with coinTrueMean using eqfe in
utest _test true "-m smc-bpf --cps full --resample manual" with coinTrueMean using eqfe in
utest _test true "-m smc-bpf --cps full --resample align" with coinTrueMean using eqfe in
utest _test true "-m smc-bpf --cps full --resample likelihood" with coinTrueMean using eqfe in
utest _test true "-m smc-apf --cps partial --resample manual" with coinTrueMean using eqfe in
utest _test true "-m smc-apf --cps partial --resample align" with coinTrueMean using eqfe in
utest _test true "-m smc-apf --cps partial --resample likelihood" with coinTrueMean using eqfe in
utest _test true "-m smc-apf --cps full --resample manual" with coinTrueMean using eqfe in
utest _test true "-m smc-apf --cps full --resample align" with coinTrueMean using eqfe in
utest _test true "-m smc-apf --cps full --resample likelihood" with coinTrueMean using eqfe in
utest _test false "-m pmcmc-pimh --cps partial" with coinTrueMean using eqfe in
utest _test false "-m pmcmc-pimh --cps full" with coinTrueMean using eqfe in
utest _test false "-m mcmc-trace" with coinTrueMean using eqfe in
utest _test false "-m mcmc-naive" with coinTrueMean using eqfe in
utest _test false "-m mcmc-lightweight --align --cps none" with coinTrueMean using eqfe in
utest _test false "-m mcmc-lightweight --align --cps partial" with coinTrueMean using eqfe in
utest _test false "-m mcmc-lightweight --align --cps full" with coinTrueMean using eqfe in
utest _test false "-m mcmc-lightweight" with coinTrueMean using eqfe in

()
