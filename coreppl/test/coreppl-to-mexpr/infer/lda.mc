include "../../../models/latent-dirichlet-allocation/lda-simple.mc"
include "../../cppl-test.mc"
include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

let s = 0.3 in
let e = eqLda s in
let rhs = ldaTruth in
let r = resLda in

let f = lam sample: [[Float]].
  let j = strJoin " " in
  j (map (lam r. j (map float2string r)) sample)
in
let c = cpplResOfDist f in

utest r (c 0   (infer (Importance { particles = 30000 }) model))           with rhs using e in
utest r (c 0   (infer (BPF { particles = 30000 }) model))                  with rhs using e in
utest r (c 0   (infer (APF { particles = 30000 }) model))                  with rhs using e in
utest r (c 500 (infer (PIMH { particles = 10, iterations = 30000 }) model)) with rhs using e in
utest r (c 500 (infer (TraceMCMC { iterations = 30000 }) model))           with rhs using e in
utest r (c 500 (infer (NaiveMCMC { iterations = 30000 }) model))           with rhs using e in

-- We need to increase the global step probability. Otherwise, lightweight MCMC
-- easily gets stuck in a single mode.
utest r (c 500 (infer (LightweightMCMC { iterations = 30000, globalProb = 0.7 }) model))  with rhs using e in

()
