include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let s = 0.3 in
let e = eqRegression s s in
let rhs = regressionTruth in
let r = resRegression in
let t = testCpplMExpr "regression.mc" in

utest r (t 1000  0 "-m is-lw --cps none"                                              ) with rhs using e in
utest r (t 1000  0 "-m is-lw --cps partial"                                           ) with rhs using e in
utest r (t 1000  0 "-m is-lw --cps partial --no-early-stop"                           ) with rhs using e in
utest r (t 1000  0 "-m is-lw --cps full"                                              ) with rhs using e in
utest r (t 1000  0 "-m is-lw --cps full --no-early-stop"                              ) with rhs using e in
utest r (t 1000  0 "-m smc-bpf --cps partial --resample manual"                       ) with rhs using e in
utest r (t 1000  0 "-m smc-bpf --cps partial --resample align"                        ) with rhs using e in
utest r (t 1000  0 "-m smc-bpf --cps partial --resample likelihood"                   ) with rhs using e in
utest r (t 1000  0 "-m smc-bpf --cps full --resample manual"                          ) with rhs using e in
utest r (t 1000  0 "-m smc-bpf --cps full --resample align"                           ) with rhs using e in
utest r (t 1000  0 "-m smc-bpf --cps full --resample likelihood"                      ) with rhs using e in
utest r (t 1000  0 "-m smc-apf --cps partial --resample manual"                       ) with rhs using e in
utest r (t 1000  0 "-m smc-apf --cps partial --resample align"                        ) with rhs using e in
utest r (t 1000  0 "-m smc-apf --cps partial --resample likelihood"                   ) with rhs using e in
utest r (t 1000  0 "-m smc-apf --cps full --resample manual"                          ) with rhs using e in
utest r (t 1000  0 "-m smc-apf --cps full --resample align"                           ) with rhs using e in
utest r (t 1000  0 "-m smc-apf --cps full --resample likelihood"                      ) with rhs using e in
utest r (t 1000  500 "-m pmcmc-pimh"                                                  ) with rhs using e in
utest r (t 10000 1000 "-m mcmc-trace"                                                 ) with rhs using e in
utest r (t 10000 1000 "-m mcmc-naive"                                                 ) with rhs using e in
utest r (t 10000 1000 "-m mcmc-lightweight --align --cps partial --mcmc-lw-gprob 0.1" ) with rhs using e in
utest r (t 10000 1000 "-m mcmc-lightweight --align --cps none --mcmc-lw-gprob 0.1"    ) with rhs using e in

()
