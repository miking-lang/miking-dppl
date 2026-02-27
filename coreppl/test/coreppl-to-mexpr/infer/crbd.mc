include "../../../models/diversification-models/crbd-synthetic.mc"
include "../../cppl-test.mc"
include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let en = eqCrbdSynthetic 0.2 2. in
let e = eqCrbdSyntheticMean 0.2 in
let rhs = crbdSyntheticTruth in
let r = resCrbdSynthetic in
let c = cpplResOfDist float2string in
let rB = resampleBehavior 0.1 in

utest r (c 0   (infer (Default {}) model))                                              with rhs using en in
utest r (c 0   (infer (Importance { particles = 1000 }) model))                         with rhs using en in
utest r (c 0   (infer (BPF { particles = 1000 }) model))                                with rhs using en in
utest r (c 0   (infer (APF { particles = 1000 }) model))                                with rhs using en in
utest r (c 500 (infer (PIMH { particles = 2, iterations = 1000 }) model))               with rhs using e in
utest r (c 500 (infer (TraceMCMC { iterations = 10000 }) model))                        with rhs using e in
utest r (c 500 (infer (NaiveMCMC { iterations = 10000 }) model))                        with rhs using e in
-- utest r (c 500 (infer (LightweightMCMC { continue = (lam. 2000, lam x. lam. lam. (subi x 1, geqi x 0)), resampleBehavior = rB }) model)) with rhs using e in

()
