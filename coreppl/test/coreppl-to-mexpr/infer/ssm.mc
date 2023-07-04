include "../../../models/ssm.mc"
include "../../cppl-test.mc"
include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let s = 150. in
let e = eqSsm s in
let rhs = ssmTruth in
let r = resSsm in
let c = cpplResOfDist float2string in

utest r (c 0   (infer (Importance { particles = 1000 }) model))                                          with rhs using e in
utest r (c 0   (infer (BPF { particles = 1000 }) model))                                                 with rhs using e in
utest r (c 0   (infer (APF { particles = 1000 }) model))                                                 with rhs using e in
utest r (c 500 (infer (PIMH { particles = 2, iterations = 1000 }) model))                                with rhs using e in
utest r (c 500 (infer (TraceMCMC { iterations = 1000 }) model))                                          with rhs using e in
utest r (c 500 (infer (NaiveMCMC { iterations = 1000 }) model))                                          with rhs using e in
utest r (c 500 (infer (LightweightMCMC { iterations = 1000, globalProb = 0.1 }) model)) with rhs using e in

()
