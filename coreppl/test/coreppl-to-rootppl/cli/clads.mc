include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Synthetic
-- NOTE(2023-07-03,dlunde): SMC with '--resample likelihood' peforms really
-- poorly for this model, which is why we simply give lam. lam. true as the
-- test equality function and also why we need to provide a bigger stack size
-- than the default.
let en = eqCladsSynthetic 1e0 2e0 in
let rhs = cladsSyntheticTruth in
let r = resCladsSynthetic in
let t = testCpplRootPPL "diversification-models/clads2-synthetic.mc" 1000 0 in
utest r (t "--resample manual" "") with rhs using en in
utest r (t "--resample likelihood" "--stack-size 100000") with rhs using lam. lam. true in
utest r (t "--resample align" "") with rhs using en in

-- Alcedinidae
let e = eqCladsAlcedinidae 5. in
let rhs = cladsAlcedinidaeTruth in
let r = resNormConst in
let t = testCpplRootPPL "diversification-models/clads2-alcedinidae.mc" 10000 0 in
utest r (t "--resample align" "") with rhs using e in

()
