include "../../../models/diversification-models/clads2-synthetic.mc"
include "../../cppl-test.mc"
include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let en = eqCladsSynthetic 1e0 2e0 in
let e = eqCladsSyntheticMean 1e0 in
let rhs = cladsSyntheticTruth in
let r = resCladsSynthetic in
let c = cpplResOfDist float2string in
let rB = resampleBehavior 0.1 in

utest r (c 0   (infer (Default {}) model))                                              with rhs using en in
utest r (c 0   (infer (Importance { particles = 1000 }) model))                         with rhs using en in
utest r (c 0   (infer (BPF { particles = 2000 }) model))                                with rhs using en in
utest r (c 0   (infer (APF { particles = 1000 }) model))                                with rhs using en in
utest r (c 500 (infer (PIMH { particles = 2, iterations = 1000 }) model))               with rhs using e in
utest r (c 500 (infer (TraceMCMC { iterations = 10000 }) model))                        with rhs using e in
utest r (c 500 (infer (NaiveMCMC { iterations = 1000 }) model))                         with rhs using e in
utest r (c 500 (infer (LightweightMCMC { continue = (lam. 1000, lam x. lam. lam. (subi x 1, geqi x 0)), resampleBehavior = rB }) model)) with rhs using e in

()
