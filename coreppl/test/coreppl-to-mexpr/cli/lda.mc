include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"

mexpr

let s = 0.3 in
let e = eqLda s in
let rhs = ldaTruth in
let r = resLda in
let t = testCpplMExpr "latent-dirichlet-allocation/lda-simple.mc" 30000 in

utest r (t 0  "-m is-lw --cps none"                            ) with rhs using e in
utest r (t 0  "-m is-lw --cps partial"                         ) with rhs using e in
utest r (t 0  "-m is-lw --cps partial --no-early-stop"         ) with rhs using e in
utest r (t 0  "-m is-lw --cps full"                            ) with rhs using e in
utest r (t 0  "-m is-lw --cps full --no-early-stop"            ) with rhs using e in
utest r (t 0  "-m smc-bpf --cps partial --resample manual"     ) with rhs using e in
utest r (t 0  "-m smc-bpf --cps partial --resample align"      ) with rhs using e in
utest r (t 0  "-m smc-bpf --cps partial --resample likelihood" ) with rhs using e in
utest r (t 0  "-m smc-bpf --cps full --resample manual"        ) with rhs using e in
utest r (t 0  "-m smc-bpf --cps full --resample align"         ) with rhs using e in
utest r (t 0  "-m smc-bpf --cps full --resample likelihood"    ) with rhs using e in
utest r (t 0  "-m smc-apf --cps partial --resample manual"     ) with rhs using e in
utest r (t 0  "-m smc-apf --cps partial --resample align"      ) with rhs using e in
utest r (t 0  "-m smc-apf --cps partial --resample likelihood" ) with rhs using e in
utest r (t 0  "-m smc-apf --cps full --resample manual"        ) with rhs using e in
utest r (t 0  "-m smc-apf --cps full --resample align"         ) with rhs using e in
utest r (t 0  "-m smc-apf --cps full --resample likelihood"    ) with rhs using e in
utest r (t 500 "-m pmcmc-pimh --cps partial"                    ) with rhs using e in
utest r (t 500 "-m pmcmc-pimh --cps full"                       ) with rhs using e in
utest r (t 500 "-m mcmc-trace"                                  ) with rhs using e in
utest r (t 500 "-m mcmc-naive"                                  ) with rhs using e in

-- We need to increase the global step probability. Otherwise, lightweight MCMC
-- easily gets stuck in a single mode.
utest r (t 500 "-m mcmc-lightweight --mcmc-lw-gprob 0.5 --align --cps none"    ) with rhs using e in
utest r (t 500 "-m mcmc-lightweight --mcmc-lw-gprob 0.5 --align --cps partial" ) with rhs using e in
utest r (t 500 "-m mcmc-lightweight --mcmc-lw-gprob 0.5 --align --cps full"    ) with rhs using e in
utest r (t 500 "-m mcmc-lightweight --mcmc-lw-gprob 0.5"                       ) with rhs using e in

()
