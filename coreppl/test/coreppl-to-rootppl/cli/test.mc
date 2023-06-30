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
let eqfe = eqfApprox 2e-2 in
let _testCoin = lam.
  let cpplRes = testCpplRootPPL "coin.mc" "" "" "1000" in
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
in
utest _testCoin () with coinTrueMean using eqfe in

-------------------------
-- models/sprinkler.mc --
-------------------------
let eqfe = eqfApprox 1e-1 in
let _testSprinkler = lam.
  let cpplRes = testCpplRootPPL "sprinkler.mc" "" "" "1000" in
  let intStringToBool = lam s. if eqString s "0" then false else true in
  sprinklerProb (map intStringToBool cpplRes.samples) cpplRes.lweights
in
utest _testSprinkler () with sprinklerTrueProb using eqfe in

()
