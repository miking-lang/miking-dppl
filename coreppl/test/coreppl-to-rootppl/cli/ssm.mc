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
let t = testCpplRootPPL "ssm.mc" 1000 0 in
utest r (t "--resample manual" "") with rhs using e in
utest r (t "--resample likelihood" "") with rhs using e in
utest r (t "--resample align" "") with rhs using e in

()
