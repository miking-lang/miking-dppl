include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

--------------------
-- models/coin.mc --
--------------------
let s = 2e-2 in
let e = eqCoin s in
let rhs = coinTruth in
let r = resCoin in
let t = testCpplRootPPL "coin.mc" 1000 0 in
utest r (t "--resample manual" "") with rhs using e in
utest r (t "--resample likelihood" "") with rhs using e in
utest r (t "--resample align" "") with rhs using e in

-------------------------
-- models/sprinkler.mc --
-------------------------
let s = 1e-1 in
let e = eqSprinkler s in
let rhs = sprinklerTruth in
let r = resSprinklerInt in
let t = testCpplRootPPL "sprinkler.mc" 1000 0 in
utest r (t "--resample manual" "") with rhs using e in
utest r (t "--resample likelihood" "") with rhs using e in
utest r (t "--resample align" "") with rhs using e in

--------------------------------------------
-- models/diversification-models/crbd*.mc --
--------------------------------------------

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
