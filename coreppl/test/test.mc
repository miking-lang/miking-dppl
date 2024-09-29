-- Common functionality for the test suite

include "seq.mc"
include "math.mc"
include "common.mc"
include "option.mc"
include "sys.mc"
include "string.mc"
include "stats.mc"

type CpplRes = {
  samples: [String],
  lweights: [Float],
  extra: Option Float
}

let dpplPath =
  match sysGetEnv "MIDPPL_PATH" with Some p then p
  else error "MIDPPL_PATH not defined"

let cppl =
  match sysGetEnv "CPPL_NAME" with Some cppl then join [dpplPath, "/build/", cppl]
  else error "CPPL_NAME not defined"

let rppl =
  match sysGetEnv "ROOTPPL_BIN" with Some rppl then join [dpplPath, "/", rppl]
  else error "ROOTPPL_BIN not defined"

let parseRun: String -> CpplRes = lam output.
  let output = strSplit "\n" output in
  let extra =
    let lhead = head output in
    if any (eqChar ' ') lhead then None ()
    else Some (string2float lhead)
  in
  let output = match extra with Some _ then tail output else output in
  let res = foldl (lam acc. lam l.
      if eqi (length l) 0 then acc else
        let l = strSplit " " l in
        let sample: String = strJoin " " (init l) in
        let lweight: Float = string2float (last l) in
        (snoc acc.0 sample, snoc acc.1 lweight)
    ) ([],[]) output
  in
  { samples = res.0, lweights = res.1, extra = extra }

let sysRunCommandWithUtest = lam args. lam s. lam wd.
  let run = sysRunCommand args s wd in
  utest run.returncode with 0 using eqi else lam. lam. (strJoin "\n" [
    join ["Command ", strJoin " " args, " failed with code ", int2string run.returncode],
    "---------->> STDOUT <<----------",
    run.stdout,
    "---------->> STDERR <<----------",
    run.stderr,
    "--------------------------------"
  ]) in
  run

let burnCpplRes: Int -> CpplRes -> CpplRes = lam burn. lam cpplRes.
  let samples = subsequence cpplRes.samples burn (length cpplRes.samples) in
  let lweights =  subsequence cpplRes.lweights burn (length cpplRes.lweights) in
  {cpplRes with samples = samples, lweights = lweights}

-- Compile and run CorePPL program and get back a list of weighted string
-- samples. MExpr backend.
let testCpplMExpr: String -> Int -> Int -> String -> CpplRes =
  lam model. lam samples. lam burn. lam compileArgs.
    let m = join [dpplPath, "/coreppl/models/", model] in
    let wd = sysTempDirMake () in
    let run = sysRunCommandWithUtest [cppl, "--seed 0", compileArgs, m ] "" wd in
    let run = sysRunCommandWithUtest [ "./out", (int2string samples) ] "" wd in
    sysDeleteDir wd;
    burnCpplRes burn (parseRun run.stdout)

-- Compile and run CorePPL program and get back a list of weighted string
-- samples. RootPPL backend.
let testCpplRootPPL: String -> Int -> Int -> String -> String -> CpplRes =
  lam model. lam samples. lam burn. lam cpplCompileArgs. lam rpplCompileArgs.
    let m = join [dpplPath, "/coreppl/models/", model] in
    let wd = sysTempDirMake () in
    sysRunCommandWithUtest [
        cppl, "--seed 0", "-t rootppl", "--skip-final", cpplCompileArgs, m
      ] "" wd;
    sysRunCommandWithUtest [
        rppl, "--seed 0", "--stack-size 10000", rpplCompileArgs, "out.cu"
      ] "" wd;
    let run = sysRunCommandWithUtest [ "./a.out", (int2string samples) ] "" wd in
    sysDeleteDir wd;
    burnCpplRes burn (parseRun run.stdout)

-- Normalizing constant testing
let resNormConst: CpplRes -> Float = lam cpplRes.
  match cpplRes.extra with Some nc then nc else
  error "Normalizing constant does not exist in cpplRes"

-- models/coin.mc
let resCoin: CpplRes -> Float = lam cpplRes.
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
let coinTruth: Float = divf 12.0 23.0
let eqCoin: Float -> Float -> Float -> Bool = eqfApprox

-- models/sprinkler.mc
let resSprinkler: CpplRes -> Float = lam cpplRes.
  let samples: [Float] =
    map (lam s. if eqString "true" s then 1. else 0.) cpplRes.samples in
  logWeightedMean cpplRes.lweights samples
let resSprinklerInt: CpplRes -> Float = lam cpplRes.
  let samples: [Float] =
    map (compose int2float string2int) cpplRes.samples in
  logWeightedMean cpplRes.lweights samples
let sprinklerTruth = divf 891. 2491.
let eqSprinkler: Float -> Float -> Float -> Bool = eqfApprox

-- models/regression.mc
let resRegression: CpplRes -> (Float, Float) = lam cpplRes.
  let samples = map (strSplit " ") cpplRes.samples in
  let lweights = cpplRes.lweights in
  (
    logWeightedMean lweights (map (lam t. string2float (get t 0)) samples),
    logWeightedMean lweights (map (lam t. string2float (get t 1)) samples)
  )
let regressionTruth = (0.43,0.621)
let eqRegression: Float -> Float -> (Float, Float) -> (Float,Float) -> Bool =
  lam s1. lam s2. lam t1. lam t2.
    if eqfApprox s1 t1.0 t2.0 then eqfApprox s2 t1.1 t2.1 else false

-- models/ssm.mc
let resSsm: CpplRes -> Float = lam cpplRes.
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
let ssmTruth: Float = 3060.
let eqSsm: Float -> Float -> Float -> Bool = eqfApprox

-- models/diversification-models/crbd*.mc
type CRBDSyntheticRes = { mean: Float, normConst: Float }
let resCrbdSynthetic: CpplRes -> CRBDSyntheticRes = lam cpplRes. {
  mean = logWeightedMean cpplRes.lweights (map string2float cpplRes.samples),
  normConst = match cpplRes.extra with Some nc then nc else nan
}
let crbdSyntheticTruth = {
  mean = 0.34,
  normConst = negf 16.0669187911
}
let eqCrbdSynthetic:
  Float -> Float -> CRBDSyntheticRes -> CRBDSyntheticRes -> Bool =
  lam s1. lam s2. lam v1. lam v2.
    if eqfApprox s1 v1.mean v2.mean then
      eqfApprox s2 v1.normConst v2.normConst
    else false
let eqCrbdSyntheticMean:
  Float -> CRBDSyntheticRes -> CRBDSyntheticRes -> Bool =
  lam s1. lam v1. lam v2. eqfApprox s1 v1.mean v2.mean

let crbdAlcedinidaeTruth = negf 304.75
let eqCrbdAlcedinidae = eqfApprox

-- models/diversification-models/clads*.mc
type CLADSSyntheticRes = { mean: Float, normConst: Float }
let resCladsSynthetic: CpplRes -> CLADSSyntheticRes = lam cpplRes. {
  mean = logWeightedMean cpplRes.lweights (map string2float cpplRes.samples),
  normConst = match cpplRes.extra with Some nc then nc else nan
}
let cladsSyntheticTruth = {
  mean = 1.02987310776,
  normConst = negf 15.4824162848
}
let eqCladsSynthetic:
  Float -> Float -> CLADSSyntheticRes -> CLADSSyntheticRes -> Bool =
  lam s1. lam s2. lam v1. lam v2.
    if eqfApprox s1 v1.mean v2.mean then
      eqfApprox s2 v1.normConst v2.normConst
    else false
let eqCladsSyntheticMean:
  Float -> CLADSSyntheticRes -> CLADSSyntheticRes -> Bool =
  lam s1. lam v1. lam v2. eqfApprox s1 v1.mean v2.mean

let cladsAlcedinidaeTruth = negf 308.755153131
let eqCladsAlcedinidae = eqfApprox

-- models/vector-borne-disease/vbd.mc
let vbdTruth = negf 368.174997594
let eqVbd = eqfApprox

-- models/latent-dirichlet-allocation/lda.mc
let resLda: CpplRes -> [Float] = lam cpplRes.
  let samples = map (lam s. map string2float (strSplit " " s)) cpplRes.samples in
  let theta0 = map (lam t. get t 0) samples in
  let theta1 = map (lam t. get t 1) samples in
  let theta2 = map (lam t. get t 2) samples in
  [
    logWeightedMean cpplRes.lweights theta0,
    logWeightedMean cpplRes.lweights theta1,
    logWeightedMean cpplRes.lweights theta2
  ]
let ldaTruth: [Float] = [0.5,0.5,0.5]
let eqLda: Float -> [Float] -> [Float] -> Bool = lam s. eqSeq (eqfApprox s)

--models/ode-harmonic.mc
let tend = 3.
let resODEHarmonic: CpplRes -> [Float] = lam cpplRes.
  let samples = map (lam s. map string2float (strSplit " " s)) cpplRes.samples in
  let x = map (lam t. get t 0) samples in
  let v = map (lam t. get t 1) samples in
  [
    logWeightedMean cpplRes.lweights x,
    logWeightedMean cpplRes.lweights v
  ]
let odeHarmonicTruth: [Float] = (lam t. [cos t, negf (sin t)]) tend
let eqODEHarmonic: Float -> [Float] -> [Float] -> Bool =
  lam eps. eqSeq (eqfApprox eps)

-- models/diff-regression.mc
let resDiffReg: CpplRes -> Float = lam cpplRes.
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
let diffRegTruth: Float = 1.
let eqDiffReg: Float -> Float -> Float -> Bool = eqfApprox

-- models/diff-confusion.mc
let resDiffConf: CpplRes -> Float = lam cpplRes.
  logWeightedMean cpplRes.lweights (map string2float cpplRes.samples)
let diffConfTruth: Float = 2.
let eqDiffConf: Float -> Float -> Float -> Bool = eqfApprox
