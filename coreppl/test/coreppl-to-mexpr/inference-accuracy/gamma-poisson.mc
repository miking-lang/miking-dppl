include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let s = 10e-3 in
let eq = eqCoin s in
let t = testCpplMExpr "gamma-poisson.mc" 200000 in
let res: [(Int, String)]  = [
  (0  ,"-m 'is-lw' --cps none"),
  (0  ,"-m 'is-lw' --cps partial"),
  (0  ,"-m 'is-lw' --cps partial --no-early-stop"),
  (0  ,"-m 'is-lw' --cps full"),
  (0  ,"-m 'is-lw' --cps full --no-early-stop"),
  (0  ,"-m 'smc-bpf' --cps partial --resample manual"),
  (0  ,"-m 'smc-bpf' --cps partial --resample align"),
  (0  ,"-m 'smc-bpf' --cps partial --resample likelihood"),
  (0  ,"-m 'smc-bpf' --cps full --resample manual"),
  (0  ,"-m 'smc-bpf' --cps full --resample align"),
  (0  ,"-m 'smc-bpf' --cps full --resample likelihood"),
  (0  ,"-m 'smc-apf' --cps partial --resample manual"),
  (0  ,"-m 'smc-apf' --cps partial --resample align"),
  (0  ,"-m 'smc-apf' --cps partial --resample likelihood"),
  (0  ,"-m 'smc-apf' --cps full --resample manual"),
  (0  ,"-m 'smc-apf' --cps full --resample align"),
  (0  ,"-m 'smc-apf' --cps full --resample likelihood"),
  (500,"-m 'pmcmc-pimh' --cps partial"),
  (500,"-m 'pmcmc-pimh' --cps full"),
  (500,"-m 'mcmc-trace'"),
  (500,"-m 'mcmc-naive'"),
  (500,"-m 'mcmc-lightweight' --align --cps none"),
  (500,"-m 'mcmc-lightweight' --align --cps partial"),
  (500,"-m 'mcmc-lightweight' --align --cps full"),
  (500,"-m 'mcmc-lightweight'"),
  (500, "-m 'mcmc-lightweight' --align --cps none --kernel --drift 0.05 --mcmc-lw-gprob 0.1"),
  (500, "-m 'mcmc-lightweight' --align --cps partial --kernel --drift 0.05 --mcmc-lw-gprob 0.1"),
  (500, "-m 'mcmc-lightweight' --align --cps full --kernel --drift 0.05 --mcmc-lw-gprob 0.1")
] in
-- these values come from outside calculation on a GammaPoisson conjugate prior for n = 100, alpha = 100, beta = 1, p = [obs] => Gamma(shape = alpha+sum(p), rate = beta+n)
let quantile: [(Float, Float, Float)] = [(100.2653, 104.2089, 0.95), 
(100.5786, 103.8882, 0.9), 
(100.9405, 103.5191, 0.8),  
(101.3801, 103.0735, 0.6),  
(101.9698, 102.4795, 0.2)]
in

iter (((testPattern t) eq) quantile) res