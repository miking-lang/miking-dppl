include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- We only check the normalizing constant for vbd.mc (and hence do not test any
-- MCMC approaches).

let e = eqVbd 50e0 in
let rhs = vbdTruth in
let r = resNormConst in
let t = testCpplMExpr "vector-borne-disease/vbd.mc" in

-- Not really possible to get a decent estimate with importance sampling, just
-- check that it compiles and runs.
utest r (t 100 0 "-m is-lw --cps none"                            ) with rhs using lam.lam.true in
utest r (t 100 0 "-m is-lw --cps partial"                         ) with rhs using lam.lam.true in
utest r (t 100 0 "-m is-lw --cps partial --no-early-stop"         ) with rhs using lam.lam.true in
utest r (t 100 0 "-m is-lw --cps full"                            ) with rhs using lam.lam.true in
utest r (t 100 0 "-m is-lw --cps full --no-early-stop"            ) with rhs using lam.lam.true in

utest r (t 10000 0 "-m smc-bpf --cps partial --resample manual"     ) with rhs using e in
utest r (t 10000 0 "-m smc-bpf --cps partial --resample align"      ) with rhs using e in
utest r (t 10000 0 "-m smc-bpf --cps partial --resample likelihood" ) with rhs using e in
utest r (t 10000 0 "-m smc-bpf --cps full --resample manual"        ) with rhs using e in
utest r (t 10000 0 "-m smc-bpf --cps full --resample align"         ) with rhs using e in
utest r (t 10000 0 "-m smc-bpf --cps full --resample likelihood"    ) with rhs using e in
utest r (t 10000 0 "-m smc-apf --cps partial --resample manual"     ) with rhs using e in
utest r (t 10000 0 "-m smc-apf --cps partial --resample align"      ) with rhs using e in
utest r (t 10000 0 "-m smc-apf --cps partial --resample likelihood" ) with rhs using e in
utest r (t 10000 0 "-m smc-apf --cps full --resample manual"        ) with rhs using e in
utest r (t 10000 0 "-m smc-apf --cps full --resample align"         ) with rhs using e in
utest r (t 10000 0 "-m smc-apf --cps full --resample likelihood"    ) with rhs using e in

()
