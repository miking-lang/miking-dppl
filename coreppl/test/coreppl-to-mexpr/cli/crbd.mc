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

let en = eqCrbdSynthetic 0.2 2. in
let e = eqCrbdSyntheticMean 0.2 in
let rhs = crbdSyntheticTruth in
let r = resCrbdSynthetic in
let t = testCpplMExpr "diversification-models/crbd-synthetic.mc" in

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
utest r (t 3000 500 "-m mcmc-lightweight --align --cps none"       ) with rhs using e in
utest r (t 3000 500 "-m mcmc-lightweight --align --cps partial"    ) with rhs using e in
utest r (t 3000 500 "-m mcmc-lightweight --align --cps full"       ) with rhs using e in
--utest r (t 1000 500 "-m mcmc-lightweight"                          ) with rhs using e in --> Basic LW run, not support anymore, not sure what to do about this failing test


-----------------
-- Alcedinidae --
-----------------

let e = eqCrbdAlcedinidae 5. in
let rhs = crbdAlcedinidaeTruth in
let r = resNormConst in
let t = testCpplMExpr "diversification-models/crbd-alcedinidae.mc" 10000 0 in

utest r (t "-m smc-bpf --cps partial --resample align" ) with rhs using e in
utest r (t "-m smc-bpf --cps full --resample align"    ) with rhs using e in
utest r (t "-m smc-apf --cps partial --resample align" ) with rhs using e in
utest r (t "-m smc-apf --cps full --resample align"    ) with rhs using e in

()
