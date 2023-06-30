include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Determines unit test sensitivity
let eqfe = eqfApprox 1e-1 in

let _test = lam smc. lam compileArgs.
  let cpplRes = testCpplMExpr smc "sprinkler.mc" compileArgs "1000" in
  sprinklerProb (map string2bool cpplRes.samples) cpplRes.lweights
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
utest _test false "-m pmcmc-pimh --cps partial" with sprinklerTrueProb using eqfe in
utest _test false "-m pmcmc-pimh --cps full" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-trace" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-naive" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-lightweight --align --cps none" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-lightweight --align --cps partial" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-lightweight --align --cps full" with sprinklerTrueProb using eqfe in
utest _test false "-m mcmc-lightweight" with sprinklerTrueProb using eqfe in

()
