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
let r = resSprinklerInt in
let t = testCpplRootPPL "sprinkler.mc" 1000 0 in
utest r (t "--resample manual" "") with rhs using e in
utest r (t "--resample likelihood" "") with rhs using e in
utest r (t "--resample align" "") with rhs using e in

()
