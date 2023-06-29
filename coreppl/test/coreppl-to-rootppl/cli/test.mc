include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Determines unit test sensitivity
let eqfe = eqfApprox 2e-2 in

let _testCoin = lam.
  let cpplRes = testCpplRootPPL "coin.mc" "" "" "1000" in
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
in

utest _testCoin () with coinTrueMean using eqfe in

()
