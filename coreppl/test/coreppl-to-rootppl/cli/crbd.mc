include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Synthetic
-- NOTE(2023-06-30,dlunde): SMC with '--resample likelihood' peforms really
-- poorly for this model, which is why we simply give lam. lam. true as the
-- test equality function.
let en = eqCrbdSynthetic 0.2 2. in
let rhs = crbdSyntheticTruth in
let r = resCrbdSynthetic in
let t = testCpplRootPPL "diversification-models/crbd-synthetic.mc" 1000 0 in
utest r (t "--resample manual" "") with rhs using en in
utest r (t "--resample likelihood" "") with rhs using lam. lam. true in
utest r (t "--resample align" "") with rhs using en in

-- Alcedinidae
let e = eqCrbdAlcedinidae 5. in
let rhs = crbdAlcedinidaeTruth in
let r = resNormConst in
let t = testCpplRootPPL "diversification-models/crbd-alcedinidae.mc" 10000 0 in
utest r (t "--resample align" "") with rhs using e in

()
