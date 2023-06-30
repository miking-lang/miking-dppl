include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let s = 1e-1 in
let e = eqSprinkler s in
let rhs = sprinklerTruth in
let r = resSprinkler in
let t = testCpplMExpr "sprinkler.mc" 1000 in

utest r (t 0   "-m is-lw --cps none"                            ) with rhs using e in
utest r (t 0   "-m is-lw --cps partial"                         ) with rhs using e in
utest r (t 0   "-m is-lw --cps partial --no-early-stop"         ) with rhs using e in
utest r (t 0   "-m is-lw --cps full"                            ) with rhs using e in
utest r (t 0   "-m is-lw --cps full --no-early-stop"            ) with rhs using e in
utest r (t 0   "-m smc-bpf --cps partial --resample manual"     ) with rhs using e in
utest r (t 0   "-m smc-bpf --cps partial --resample align"      ) with rhs using e in
utest r (t 0   "-m smc-bpf --cps partial --resample likelihood" ) with rhs using e in
utest r (t 0   "-m smc-bpf --cps full --resample manual"        ) with rhs using e in
utest r (t 0   "-m smc-bpf --cps full --resample align"         ) with rhs using e in
utest r (t 0   "-m smc-bpf --cps full --resample likelihood"    ) with rhs using e in
utest r (t 0   "-m smc-apf --cps partial --resample manual"     ) with rhs using e in
utest r (t 0   "-m smc-apf --cps partial --resample align"      ) with rhs using e in
utest r (t 0   "-m smc-apf --cps partial --resample likelihood" ) with rhs using e in
utest r (t 0   "-m smc-apf --cps full --resample manual"        ) with rhs using e in
utest r (t 0   "-m smc-apf --cps full --resample align"         ) with rhs using e in
utest r (t 0   "-m smc-apf --cps full --resample likelihood"    ) with rhs using e in
utest r (t 500 "-m pmcmc-pimh --cps partial"                    ) with rhs using e in
utest r (t 500 "-m pmcmc-pimh --cps full"                       ) with rhs using e in
utest r (t 500 "-m mcmc-trace"                                  ) with rhs using e in
utest r (t 500 "-m mcmc-naive"                                  ) with rhs using e in
utest r (t 500 "-m mcmc-lightweight --align --cps none"         ) with rhs using e in
utest r (t 500 "-m mcmc-lightweight --align --cps partial"      ) with rhs using e in
utest r (t 500 "-m mcmc-lightweight --align --cps full"         ) with rhs using e in
utest r (t 500 "-m mcmc-lightweight"                            ) with rhs using e in

()
