include "math.mc"

let model1 = lam.
  let a = assume (Gamma 2. 3.) in
  let b = assume (Exponential 2.) in
  let c = assume (Beta 2. 2.) in
  let d = assume (Gaussian 1. 2.) in
  let e = assume (Uniform 2. 3.) in
  foldl addf 0. [a, b, c, d, e]

let model2 = lam.
  let a = expectation (Gamma 2. 3.) in
  let b = expectation (Exponential 2.) in
  let c = expectation (Beta 2. 2.) in
  let d = expectation (Gaussian 1. 2.) in
  let e = expectation (Uniform 2. 3.) in
  foldl addf 0. [a, b, c, d, e]

mexpr

let expected = foldl addf 0. [6., 0.5, 0.5, 1., 2.5] in

let d = infer (Importance { particles = 1000 }) model1 in
utest expectation d with expected using eqfApprox 1e-1 in

let d = infer (BPF { particles = 1000 }) model1 in
utest expectation d with expected using eqfApprox 1e-1 in

let d = infer (APF { particles = 1000 }) model1 in
utest expectation d with expected using eqfApprox 1e-1 in

let d = infer (PIMH { particles = 1000 }) model1 in
utest expectation d with expected using eqfApprox 1e-1 in

let d = infer (TraceMCMC { iterations = 100000 }) model1 in
utest expectation d with expected using eqfApprox 1e-1 in

let d = infer (NaiveMCMC { iterations = 1000 }) model1 in
utest expectation d with expected using eqfApprox 1e-1 in

let d =
  infer (LightweightMCMC { iterations = 100000, globalProb = 0.1 }) model1
in
utest expectation d with expected using eqfApprox 1e-1 in

let d = infer (Importance { particles = 1 }) model2 in
utest expectation d with expected in

let d = infer (BPF { particles = 1 }) model2 in
utest expectation d with expected in

let d = infer (APF { particles = 2 }) model2 in
utest expectation d with expected in

let d = infer (PIMH { particles = 1 }) model2 in
utest expectation d with expected in

let d = infer (TraceMCMC { iterations = 1 }) model2 in
utest expectation d with expected in

let d = infer (NaiveMCMC { iterations = 1 }) model2 in
utest expectation d with expected in

let d =
  infer (LightweightMCMC { iterations = 1, globalProb = 0.1 }) model2
in
utest expectation d with expected in


()
