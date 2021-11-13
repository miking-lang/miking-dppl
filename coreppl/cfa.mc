include "coreppl.mc"
include "dppl-parser.mc"

include "mexpr/cfa.mc"


-- NOTE What to include
-- Stochastic value flow
-- Needs to include analysis for application of intrinsic functions.

lang StochCFA = CFA + InitConstraint + Assume

  syn AbsVal =
  | AVStoch {}

  sem generateConstraints =
  | TmLet { ident = ident, body = TmAssume _ } ->
    [ CstrInit { lhs = AVStoch {}, rhs = ident } ]

  sem absValToString (env: PprintEnv) =
  | AVStoch {} ->
    (env, "stoch")

  -- TODO Add abstract values for intrinsic functions, and an app constraint
  -- for intrinsic function applications. Needed to track stochastic integers,
  -- floats, and booleans.

  -- TODO Add app constraint for tracking stochastic functions (CstrLamApp not
  -- enough)

  -- TODO How do we detect if the condition is stochastic in a match? Not obvious

end

lang MExprPPLCFA = MExprCFA + StochCFA

  sem _constraintGenFuns =
  | _ -> [generateConstraints]

  sem _appConstraintGenFuns =
  | _ -> [generateLamAppConstraints]

end

lang Test = MExprPPLCFA + MExprPPL + DPPLParser
end

mexpr
use Test in

-- Test functions --
let parse = parseMExprPPLString in
let test: Bool -> Expr -> [String] -> [[AbsVal]] =
  lam debug: Bool. lam t: Expr. lam vars: [String].
    if debug then
      -- Version with debug printouts
      let tANF = normalizeTerm t in
      match pprintCode 0 pprintEnvEmpty t with (_,tStr) in
      printLn "\n--- ORIGINAL PROGRAM ---";
      printLn tStr;
      match pprintCode 0 pprintEnvEmpty tANF with (env,tANFStr) in
      printLn "\n--- ANF ---";
      printLn tANFStr;
      match cfaDebug env tANF with (env,cfaRes) in
      match cfaGraphToString env cfaRes with (_, resStr) in
      printLn "\n--- FINAL CFA GRAPH ---";
      printLn resStr;
      map (lam var: String.
        (var, _dataLookup (nameNoSym var) cfaRes)
      ) vars

    else
      -- Version without debug printouts
      let tANF = normalizeTerm t in
      let cfaRes = cfa tANF in
      map (lam var: String.
        (var, _dataLookup (nameNoSym var) cfaRes)
      ) vars
in

-- Tests stochastic value flow
let t = parse "
  let x = assume (Beta 1.0 1.0) in
  let sum1 = addf 1.0 1.0 in
  let sum2 = addf x 1.0 in
  let res = addf sum1 sum2 in
  res
------------------------" in
utest test true t [] with [] in

()
