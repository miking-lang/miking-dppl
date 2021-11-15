-- Alignment analysis for CorePPL.

include "coreppl.mc"
include "dppl-parser.mc"

include "mexpr/cfa.mc"



lang StochCFA = CFA + InitConstraint + Assume

  syn AbsVal =
  | AVStoch {}
  | AVConst { arity: Int }

  sem absValToString (env: PprintEnv) =
  | AVStoch {} -> (env, "stoch")
  | AVConst { arity = arity } -> (env, join ["const", int2string arity])

  sem cmpAbsValH =
  | (AVStoch _, AVStoch _) -> 0
  | (AVConst lhs, AVConst rhs) -> subi lhs.arity rhs.arity

  sem generateStochConstraints =
  | _ -> []

  -- Stochastic values
  | TmLet { ident = ident, body = TmAssume _ } ->
    [ CstrInit { lhs = AVStoch {}, rhs = ident } ]

  -- Constants. NOTE(dlunde,2021-11-15): We probably want to handle various
  -- constants differently as well, but for now this should be enough. Handling
  -- of constants could potentially also be generalized as part of the default
  -- cfa framework in Miking.
  | TmLet { ident = ident, body = TmConst { val = c } } ->
    let arity = constArity c in
    if eqi arity 0 then []
    else [ CstrInit { lhs = AVConst { arity = arity }, rhs = ident } ]

  syn Constraint =

  -- {stoch} ⊆ lhs ⇒ (stoch ⊆ res)
  | CstrStochApp { lhs: Name, res: Name }

  -- {const} ⊆ lhs AND {stoch ⊆ rhs} ⇒ {stoch} ⊆ res
  | CstrConstStochApp { lhs: Name, rhs: Name, res: Name }

  -- {const with arity > 0} ⊆ lhs ⇒ {const with arity-1} ⊆ res
  | CstrConstApp { lhs: Name, res: Name }

  sem initConstraint (graph: CFAGraph) =
  | CstrStochApp r & cstr -> _initConstraintName r.lhs graph cstr
  | CstrConstStochApp r & cstr ->
    let graph = _initConstraintName r.lhs graph cstr in
    _initConstraintName r.rhs graph cstr
  | CstrConstApp r & cstr -> _initConstraintName r.lhs graph cstr

  sem propagateConstraint (update: (Name,AbsVal)) (graph: CFAGraph) =
  | CstrStochApp { lhs = lhs, res = res } ->
    match update.1 with AVStoch _ & av then _addData graph av res else graph
  | CstrConstStochApp { lhs = lhs, rhs = rhs, res = res } ->
    -- NOTE(dlunde,2021-11-15): We could split the constraint into two to avoid
    -- the below.
    if nameEq update.0 lhs then
      match update.1 with AVConst _ then
        let s = _dataLookup rhs graph in
        if setMem (AVStoch {}) s then
          _addData graph (AVStoch {}) res
        else graph
      else graph
    else if nameEq update.0 rhs then
      match update.1 with AVStoch _ then
        let s = _dataLookup rhs graph in
        if setAny (lam av. match av with AVConst _ then true else false) s then
          _addData graph (AVStoch {}) res
        else graph
      else graph
    else graph
  | CstrConstApp { lhs = lhs, res = res } ->
    match update.1 with AVConst r then
      if gti r.arity 1 then
        _addData graph (AVConst { r with arity = subi r.arity 1 }) res
      else graph
    else graph

  sem constraintToString (env: PprintEnv) =
  | CstrStochApp { lhs = lhs, res = res } ->
    match pprintVarName env lhs with (env,lhs) in
    match pprintVarName env res with (env,res) in
    (env, join [
      "{stoch} ⊆ ", lhs, " ⇒ {stoch} ⊆ ", res ])
  | CstrConstStochApp { lhs = lhs, rhs = rhs, res = res } ->
    match pprintVarName env lhs with (env,lhs) in
    match pprintVarName env rhs with (env,rhs) in
    match pprintVarName env res with (env,res) in
    (env, join [
      "{const} ⊆ ", lhs, " AND {stoch} ⊆ ", rhs, " ⇒ {stoch} ⊆ ", res ])
  | CstrConstApp { lhs = lhs, res = res } ->
    match pprintVarName env lhs with (env,lhs) in
    match pprintVarName env res with (env,res) in
    (env, join [
      "{const with arity > 0} ⊆ ", lhs, " ⇒ {const with arity-1} ⊆ ", res ])

  sem genereateStochAppConstraints (lhs: Name) (rhs: Name) =
  | res /- : Name -/ -> [
      CstrStochApp { lhs = lhs, res = res},
      CstrConstStochApp { lhs = lhs, rhs = rhs, res = res},
      CstrConstApp { lhs = lhs, res = res}
    ]


  -- TODO(dlunde,2021-11-15): How do we detect if the condition is stochastic
  -- in a match?

end

lang MExprPPLCFA = MExprCFA + StochCFA

  sem _constraintGenFuns =
  | _ -> [generateConstraints, generateStochConstraints]

  sem _appConstraintGenFuns =
  | _ -> [generateLamAppConstraints, genereateStochAppConstraints]

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

-- Custom equality function for testing lambda control flow only
let eqTestStoch = eqSeq (lam t1:(String,Set AbsVal). lam t2:(String,Bool).
  if eqString t1.0 t2.0 then
    let t11 = setFold
      (lam acc. lam av. match av with AVStoch _ then true else acc)
      false t1.1
    in
    eqBool t11 t2.1
  else false
) in
--------------------

-- Tests stochastic value flow

let t = parse "
  let x = assume (Beta 1.0 1.0) in
  let f = addf in
  let sum1 = addf 1.0 1.0 in
  let sum2 = f x 1.0 in
  let res = addf sum1 sum2 in
  res
------------------------" in
utest test false t ["x","f","sum1","sum2","res"] with [
  ("x", true),
  ("f", false),
  ("sum1", false),
  ("sum2", true),
  ("res", true)
] using eqTestStoch in

let t = parse "
  let f = (lam x. addf x 1) in
  let g = (lam y. addf y 2) in
  let s = assume (Beta 1.0 1.0) in
  let res = addf (f s) (g 1) in
  res
------------------------" in
utest test false t ["f","g","s","res","x","y"] with [
  ("f", false),
  ("g", false),
  ("s", true),
  ("res", true),
  ("x", true),
  ("y", false)
] using eqTestStoch in

let t = parse "
  type T in
  con C: Float -> T in
  let x = assume (Beta 1.0 1.0) in
  let d = { a = [C x], b = 3 } in
  let res = match d with { a = [C a], b = b } then
      a
    else
      1.0
  in res
------------------------" in
utest test false t ["x","d","res","a","b"] with [
  ("x", true),
  ("d", false),
  ("res", true),
  ("a", true),
  ("b", false)
] using eqTestStoch in

-- Random function TODO(dlunde,2021-11-15): This does not yet work
--let t = parse "
--  let f =
--    if assume (Bernoulli 0.5) then
--      (lam x. addi x 1)
--    else
--      (lam y. addi y 2)
--  in
--  let res = f 5 in
--  res
--------------------------" in
--utest test true t ["f","x","y","res"] with [
--  ("f", true),
--  ("x", false),
--  ("y", false),
--  ("res", true)
--] using eqTestStoch in

()
