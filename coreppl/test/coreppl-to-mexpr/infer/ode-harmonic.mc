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
let rB = resampleBehavior 0.1 in

let _model = lam. model (EulerForward ()) () in
utest r (c 0 (infer (Importance { particles = 1 }) _model))
with rhs using e in

let _model = lam. model (Default ()) () in
utest r (c 0 (infer (Importance { particles = 1 }) _model))
with rhs using e in

let _model = lam. model (RungeKutta ()) () in
utest r (c 0 (infer (Importance { particles = 1 }) _model))
with rhs using e in
utest r (c 0 (infer (BPF { particles = 1 }) _model))
with rhs using e in
utest r (c 0 (infer (APF { particles = 2 }) _model))
with rhs using e in
utest r (c 0 (infer (PIMH { particles = 2, iterations = 1 }) _model))
with rhs using e in
utest r (c 0 (infer (TraceMCMC { iterations = 1 }) _model))
with rhs using e in
utest r (c 0 (infer (NaiveMCMC { iterations = 1 }) _model))
with rhs using e in
utest r (c 0
  (infer (LightweightMCMC { continue = (lam. 1, lam x. lam. lam. (subi x 1, geqi x 0)), resampleBehavior = rB }) _model))
with rhs using e in

()
