-- Common functionality for the test suite

include "common.mc"
include "option.mc"
include "sys.mc"
include "string.mc"

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

let parseRun: Bool -> String -> CpplRes = lam extra. lam output.
  let output = strSplit "\n" output in
  let extra = if extra then Some (string2float (head output)) else None () in
  let output = tail output in
  let res = foldl (lam acc. lam l.
      if eqi (length l) 0 then acc else
        let l = strSplit " " l in
        let sample: String = join (init l) in
        let lweight: Float = string2float (last l) in
        (snoc acc.0 sample, snoc acc.1 lweight)
    ) ([],[]) output
  in
  { samples = res.0, lweights = res.1, extra = extra }

-- Compile and run CorePPL program and get back a list of weighted string samples.
-- Parameters: Command line options (e.g., backend, inference algorithm, etc.)
let testCpplMExpr: Bool -> String -> String -> String -> CpplRes =
  lam extra. lam model. lam compileArgs. lam runArgs.
    let m = join [dpplPath, "/coreppl/models/", model] in
    let wd = sysTempDirMake () in
    let cmpl = sysRunCommand [cppl, compileArgs, m ] "" wd in
    let run = sysRunCommand [ "./out", runArgs ] "" wd in
    sysDeleteDir wd;
    parseRun extra run.stdout


-- models/coin.mc
let coinTrueMean = divf 12.0 23.0

