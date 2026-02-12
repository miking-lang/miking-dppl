include "../../../models/sprinkler.mc"
include "../../cppl-test.mc"
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
let c = cpplResOfDist bool2string in
let rB = resampleBehavior 0.1 in

utest r (c 0 (infer (Default {}) model))                                                                 with rhs using e in
utest r (c 0 (infer (Importance { particles = 1000 }) model))                                            with rhs using e in
utest r (c 0 (infer (BPF { particles = 1000 }) model))                                                   with rhs using e in
utest r (c 0 (infer (APF { particles = 1000 }) model))                                                   with rhs using e in
utest r (c 500 (infer (PIMH { particles = 10, iterations = 100 }) model))                                with rhs using e in
utest r (c 500 (infer (TraceMCMC { iterations = 1000 }) model))                                          with rhs using e in
utest r (c 500 (infer (NaiveMCMC { iterations = 1000 }) model))                                          with rhs using e in
utest r (c 500 (infer (LightweightMCMC { continue = (lam. 2000, lam x. lam. lam. (subi x 1, geqi x 0)), resampleBehavior = rB }) model)) with rhs using e in

()
