include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let s = 2e-2 in
let e = eqCoin s in
let rhs = coinTruth in
let r = resCoin in
let t = testCpplRootPPL "coin.mc" 1000 0 in
utest r (t "--resample manual" "") with rhs using e in
utest r (t "--resample likelihood" "") with rhs using e in
utest r (t "--resample align" "") with rhs using e in

()
