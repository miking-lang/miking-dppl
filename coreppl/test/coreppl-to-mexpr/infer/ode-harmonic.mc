include "../../../models/ode/harmonic.mc"
include "../../cppl-test.mc"
include "../../test.mc"

mexpr

let s = 2e-2 in
let e = eqODEHarmonic s in
let rhs = odeHarmonicTruth in
let r = resODEHarmonic in

let f = lam sample: [Float].
  let j = strJoin " " in
  j (map float2string sample)
in
let c = cpplResOfDist f in

utest r (c 0   (infer (Default {}) model))
with rhs using e in
utest r (c 0   (infer (Importance { particles = 1000 }) model))
with rhs using e in
utest r (c 0   (infer (BPF { particles = 1000 }) model))
with rhs using e in
utest r (c 0   (infer (APF { particles = 1000 }) model))
with rhs using e in
utest r (c 500 (infer (PIMH { particles = 10, iterations = 100 }) model))
with rhs using e in
utest r (c 500 (infer (TraceMCMC { iterations = 1000 }) model))
with rhs using e in
utest r (c 500 (infer (NaiveMCMC { iterations = 1000 }) model))
with rhs using e in
utest r (c 500
  (infer (LightweightMCMC { iterations = 1000, globalProb = 0.1 }) model))
with rhs using e in

()
