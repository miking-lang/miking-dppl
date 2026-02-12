include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let s = 10e-3 in
let eq = eqCoin s in
let t = testCpplMExpr "coin-iter-alter.mc" 40000 in
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
  (500, "-m 'mcmc-lightweight' --align --cps none --kernel --drift 2. --mcmc-lw-gprob 0.1"),
  (500, "-m 'mcmc-lightweight' --align --cps partial --kernel --drift 2. --mcmc-lw-gprob 0.1"),
  (500, "-m 'mcmc-lightweight' --align --cps full --kernel --drift 2. --mcmc-lw-gprob 0.1")
] in
-- these values come from outside calculation on a BetaBernouilli conjugate prior for alpha = 3, beta = 5 => Beta( 9, 9)
let quantile: [(Float, Float, Float)] = [(0.2781183, 0.7218817, 0.95), 
(0.3108296, 0.6891704, 0.9), 
(0.3503948, 0.6496052, 0.8),  
(0.4004409, 0.5995591, 0.6),  
(0.4697533, 0.5302467, 0.2)]  
in

iter (((testPattern t) eq) quantile) res