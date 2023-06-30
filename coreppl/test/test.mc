-- Common functionality for the test suite

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

let parseRun: Bool -> String -> CpplRes = lam extra. lam output.
  let output = strSplit "\n" output in
  let extra = if extra then Some (string2float (head output)) else None () in
  let output = tail output in
  let res = foldl (lam acc. lam l.
      if eqi (length l) 0 then acc else
        let l = strSplit " " l in
        let sample: String = strJoin " " (init l) in
        let lweight: Float = string2float (last l) in
        (snoc acc.0 sample, snoc acc.1 lweight)
    ) ([],[]) output
  in
  { samples = res.0, lweights = res.1, extra = extra }

let burn: Int -> CpplRes -> CpplRes = lam b. lam cpplRes.
  let samples = subsequence cpplRes.samples b (length cpplRes.samples) in
  let lweights =  subsequence cpplRes.lweights b (length cpplRes.lweights) in
  {cpplRes with samples = samples, lweights = lweights}

-- Compile and run CorePPL program and get back a list of weighted string
-- samples. MExpr backend.
let testCpplMExpr: Bool -> String -> String -> String -> CpplRes =
  lam extra. lam model. lam compileArgs. lam runArgs.
    let m = join [dpplPath, "/coreppl/models/", model] in
    let wd = sysTempDirMake () in
    sysRunCommand [cppl, "--seed 0", compileArgs, m ] "" wd;
    let run = sysRunCommand [ "./out", runArgs ] "" wd in
    sysDeleteDir wd;
    parseRun extra run.stdout

-- Compile and run CorePPL program and get back a list of weighted string
-- samples. RootPPL backend.
let testCpplRootPPL: String -> String -> String -> String -> CpplRes =
  lam model. lam cpplCompileArgs. lam rpplCompileArgs. lam runArgs.
    let m = join [dpplPath, "/coreppl/models/", model] in
    let wd = sysTempDirMake () in
    sysRunCommand [
        cppl, "--seed 0", "-t rootppl", "--skip-final", cpplCompileArgs, m
      ] "" wd;
    sysRunCommand [
        rppl, "--seed 0", "--stack-size 10000", rpplCompileArgs, "out.cu"
      ] "" wd;
    let run = sysRunCommand [ "./a.out", runArgs ] "" wd in
    sysDeleteDir wd;
    parseRun true run.stdout

-- models/coin.mc
let coinTrueMean = divf 12.0 23.0

-- models/sprinkler.mc
let sprinklerTrueProb = divf 891. 2491.
let sprinklerProb: [Bool] -> [Float] -> Float = lam samples. lam lweights.
  let samples: [Float] =
    map (lam b. if b then 1. else 0.) samples in
  logWeightedMean lweights samples

-- models/regression.mc
let regressionApproxTrue = (0.43,0.621)
let regressionMean: [Float] -> [String] -> (Float,Float) =
  lam lweights. lam samples.
    let samples = map (strSplit " ") samples in
    (
      logWeightedMean lweights (map (lam t. string2float (get t 0)) samples),
      logWeightedMean lweights (map (lam t. string2float (get t 1)) samples)
    )
