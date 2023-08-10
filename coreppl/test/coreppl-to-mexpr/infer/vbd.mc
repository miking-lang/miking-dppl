include "../../../models/vector-borne-disease/vbd.mc"
include "../../cppl-test.mc"
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
let c = cpplResOfDist (lam. "") in

utest r (c 0 (infer (Default {}) model))                      with rhs using lam.lam.true in
utest r (c 0 (infer (Importance { particles = 1000 }) model)) with rhs using lam.lam.true in
utest r (c 0 (infer (BPF { particles = 10000 }) model))       with rhs using e in
utest r (c 0 (infer (APF { particles = 10000 }) model))       with rhs using e in

()
