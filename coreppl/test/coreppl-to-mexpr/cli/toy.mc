include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Tollerances will need to be adjusted 
-- Reusing the ClaDS biolerplate

let en = eqCladsSynthetic 1e-1 1e-1 in 
let e = eqCladsSyntheticMean 1e-1 in
let r = resCladsSynthetic in

let rhs = toyTruth in
let t = testCpplMExpr "toy/gamma-poisson.mc" in

printLn "Type I error may occur in tests";

-- utest r (t 1000 0 "-m is-lw --cps none"                            ) with rhs using en in
-- utest r (t 1000 0 "-m is-lw --cps partial"                         ) with rhs using en in
-- utest r (t 1000 0 "-m is-lw --cps partial --no-early-stop"         ) with rhs using en in
-- utest r (t 1000 0 "-m is-lw --cps full"                            ) with rhs using en in
-- utest r (t 1000 0 "-m is-lw --cps full --no-early-stop"            ) with rhs using en in
-- utest r (t 1000 0 "-m smc-bpf --cps partial --resample manual"     ) with rhs using en in
-- utest r (t 1000 0 "-m smc-bpf --cps partial --resample align"      ) with rhs using en in
-- utest r (t 1000 0 "-m smc-bpf --cps partial --resample likelihood" ) with rhs using lam. lam. true in
-- utest r (t 1000 0 "-m smc-bpf --cps full --resample manual"        ) with rhs using en in
-- utest r (t 1000 0 "-m smc-bpf --cps full --resample align"         ) with rhs using en in
-- utest r (t 1000 0 "-m smc-bpf --cps full --resample likelihood"    ) with rhs using lam. lam. true in
-- utest r (t 1000 0 "-m smc-apf --cps partial --resample manual"     ) with rhs using en in
-- utest r (t 1000 0 "-m smc-apf --cps partial --resample align"      ) with rhs using en in
-- utest r (t 1000 0 "-m smc-apf --cps partial --resample likelihood" ) with rhs using lam. lam. true in
-- utest r (t 1000 0 "-m smc-apf --cps full --resample manual"        ) with rhs using en in
-- utest r (t 1000 0 "-m smc-apf --cps full --resample align"         ) with rhs using en in
-- utest r (t 1000 0 "-m smc-apf --cps full --resample likelihood"    ) with rhs using lam. lam. true in
-- utest r (t 1000 500 "-m pmcmc-pimh --cps partial"                  ) with rhs using e in
-- utest r (t 1000 500 "-m pmcmc-pimh --cps full"                     ) with rhs using e in
-- utest r (t 10000 500 "-m mcmc-trace"                               ) with rhs using e in
-- utest r (t 10000 500 "-m mcmc-naive"                               ) with rhs using e in
-- utest r (t 1000 500 "-m mcmc-lightweight --align --cps none"       ) with rhs using e in
-- utest r (t 1000 500 "-m mcmc-lightweight --align --cps partial"    ) with rhs using e in
-- utest r (t 1000 500 "-m mcmc-lightweight --align --cps full"       ) with rhs using e in
-- utest r (t 1000 500 "-m mcmc-lightweight"                          ) with rhs using e in
utest r (t 10000 500 "-m mcmc-lightweight --align --cps none --mcmc-lw-gprob 0.0"      ) with rhs using e in
utest r (t 100000 500 "-m mcmc-lw-dk --align --cps none --mcmc-lw-gprob 0.0 --drift 0.1") with rhs using e in





()
