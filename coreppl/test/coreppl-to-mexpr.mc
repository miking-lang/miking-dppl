-- Tests for models/coin.mc (MExpr backend)

include "../models/coin.mc"
include "test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"
include "stats.mc"

mexpr

-- Determines unit test sensitivity
let eqfe = eqfApprox 1e-2 in

let _test = lam dist.
  match distEmpiricalSamples dist with (vs,ws) in
  logWeightedMean ws vs
in

-- Test infers
let d = infer (Importance {particles = 1000}) model in
utest _test d with coinTrueMean using eqfe in

let d = infer (BPF {particles = 1000}) model in
utest _test d with coinTrueMean using eqfe in

let d = infer (APF {particles = 1000}) model in
utest _test d with coinTrueMean using eqfe in

let d = infer (PIMH {particles = 10, iterations = 100}) model in
utest _test d with coinTrueMean using eqfe in

let d = infer (TraceMCMC { iterations = 1000 }) model in
utest _test d with coinTrueMean using eqfe in

let d = infer (NaiveMCMC { iterations = 1000 }) model in
utest _test d with coinTrueMean using eqfe in

let d = infer (LightweightMCMC { iterations = 1000, aligned = true, globalProb = 0.1 }) model in
utest _test d with coinTrueMean using eqfe in

let d = infer (LightweightMCMC { iterations = 1000, aligned = false, globalProb = 0.1 }) model in
utest _test d with coinTrueMean using eqfe in

-- Test command line
-- The makefile sets the MIDPPL_BASE and MIDPPL_SRC variables before running
-- this file (so that they point to the local directory).
let _cppl =
  match sysGetEnv "CPPL_NAME" with Some cppl then concat "build/" cppl else
  error "CPPL_NAME not defined"
in
let _m = lam name. join ["coreppl/models/", name] in
let res = sysRunCommand [_cppl, _m "coin.mc", "--output ENFIL" ] "" "." in
()
