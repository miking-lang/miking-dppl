include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let e = eqVbd 50e0 in
let rhs = vbdTruth in
let r = resNormConst in
let t = testCpplRootPPL "vector-borne-disease/vbd.mc" 10000 0 in
utest r (t "--resample manual" "--stack-size 100000") with rhs using e in
utest r (t "--resample likelihood" "--stack-size 100000") with rhs using e in
utest r (t "--resample align" "--stack-size 100000") with rhs using e in

()
