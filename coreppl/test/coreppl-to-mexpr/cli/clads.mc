include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

--------------------
-- Synthetic data --
--------------------

let en = eqCladsSynthetic 1e0 2e0 in
let e = eqCladsSyntheticMean 1e0 in
let rhs = cladsSyntheticTruth in
let r = resCladsSynthetic in
let t = testCpplMExpr "diversification-models/clads2-synthetic.mc" in

-- NOTE(2023-06-30,dlunde): SMC with '--resample likelihood' peforms really
-- poorly for this model, which is why we simply give lam. lam. true as the
-- test equality function.
utest r (t 1000 0 "-m is-lw --cps none"                            ) with rhs using en in
utest r (t 1000 0 "-m is-lw --cps partial"                         ) with rhs using en in
utest r (t 1000 0 "-m is-lw --cps partial --no-early-stop"         ) with rhs using en in
utest r (t 1000 0 "-m is-lw --cps full"                            ) with rhs using en in
utest r (t 1000 0 "-m is-lw --cps full --no-early-stop"            ) with rhs using en in
utest r (t 1000 0 "-m smc-bpf --cps partial --resample manual"     ) with rhs using en in
utest r (t 1000 0 "-m smc-bpf --cps partial --resample align"      ) with rhs using en in
utest r (t 1000 0 "-m smc-bpf --cps partial --resample likelihood" ) with rhs using lam. lam. true in
utest r (t 1000 0 "-m smc-bpf --cps full --resample manual"        ) with rhs using en in
utest r (t 1000 0 "-m smc-bpf --cps full --resample align"         ) with rhs using en in
utest r (t 1000 0 "-m smc-bpf --cps full --resample likelihood"    ) with rhs using lam. lam. true in
utest r (t 1000 0 "-m smc-apf --cps partial --resample manual"     ) with rhs using en in
utest r (t 1000 0 "-m smc-apf --cps partial --resample align"      ) with rhs using en in
utest r (t 1000 0 "-m smc-apf --cps partial --resample likelihood" ) with rhs using lam. lam. true in
utest r (t 1000 0 "-m smc-apf --cps full --resample manual"        ) with rhs using en in
utest r (t 1000 0 "-m smc-apf --cps full --resample align"         ) with rhs using en in
utest r (t 1000 0 "-m smc-apf --cps full --resample likelihood"    ) with rhs using lam. lam. true in
utest r (t 1000 500 "-m pmcmc-pimh --cps partial"                  ) with rhs using e in
utest r (t 1000 500 "-m pmcmc-pimh --cps full"                     ) with rhs using e in
utest r (t 10000 500 "-m mcmc-trace"                               ) with rhs using e in
utest r (t 1000 500 "-m mcmc-naive"                                ) with rhs using e in
utest r (t 1000 500 "-m mcmc-lightweight --align --cps none"       ) with rhs using e in
utest r (t 1000 500 "-m mcmc-lightweight --align --cps partial"    ) with rhs using e in
utest r (t 1000 500 "-m mcmc-lightweight --align --cps full"       ) with rhs using e in
utest r (t 1000 500 "-m mcmc-lightweight"                          ) with rhs using e in

let lhs = r (t 1000 500 "-m mcmc-lw-dk --align --cps none --drift 0.1 --mcmc-lw-gprob 0.0") in
printLn (float2string lhs.mean);
utest lhs with rhs using e in


-----------------
-- Alcedinidae --
-----------------

let e = eqCladsAlcedinidae 5. in
let rhs = cladsAlcedinidaeTruth in
let r = resNormConst in
let t = testCpplMExpr "diversification-models/clads2-alcedinidae.mc" 10000 0 in

-- utest r (t "-m smc-bpf --cps partial --resample align" ) with rhs using e in
-- utest r (t "-m smc-bpf --cps full --resample align"    ) with rhs using e in
-- utest r (t "-m smc-apf --cps partial --resample align" ) with rhs using e in
-- utest r (t "-m smc-apf --cps full --resample align"    ) with rhs using e in

()
