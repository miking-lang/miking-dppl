-- Alignment analysis for CorePPL.
--
-- TODO(dlunde,2021-11-16): Compute dynamic control flow

include "coreppl.mc"
include "dppl-parser.mc"

include "mexpr/cfa.mc"

lang MExprPPLAlign = MExprCFA + MExprPPL

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
  -- {stoch} ⊆ lhs ⇒ {stoch} ⊆ rhs
  | CstrStochDirect { lhs: Name, rhs: Name }

  sem initConstraint (graph: CFAGraph) =
  | CstrStochApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrConstStochApp r & cstr ->
    let graph = initConstraintName r.lhs graph cstr in
    initConstraintName r.rhs graph cstr
  | CstrConstApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrStochDirect r & cstr -> initConstraintName r.lhs graph cstr

  sem propagateConstraint (update: (Name,AbsVal)) (graph: CFAGraph) =
  | CstrStochApp { lhs = lhs, res = res } ->
    match update.1 with AVStoch _ & av then addData graph av res else graph
  | CstrConstStochApp { lhs = lhs, rhs = rhs, res = res } ->
    if nameEq update.0 lhs then
      match update.1 with AVConst _ then
        let s = dataLookup rhs graph in
        if setMem (AVStoch {}) s then
          addData graph (AVStoch {}) res
        else graph
      else graph
    else if nameEq update.0 rhs then
      match update.1 with AVStoch _ then
        let s = dataLookup rhs graph in
        if setAny (lam av. match av with AVConst _ then true else false) s then
          addData graph (AVStoch {}) res
        else graph
      else graph
    else graph
  | CstrConstApp { lhs = lhs, res = res } ->
    match update.1 with AVConst r then
      if gti r.arity 1 then
        addData graph (AVConst { r with arity = subi r.arity 1 }) res
      else graph
    else graph
  | CstrStochDirect { lhs = lhs, rhs = rhs } ->
    match update.1 with AVStoch _ & av then addData graph av rhs else graph

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
  | CstrStochDirect { lhs = lhs, rhs = rhs } ->
    match pprintVarName env rhs with (env,rhs) in
    match pprintVarName env lhs with (env,lhs) in
    (env, join [ "{stoch} ⊆ ", lhs, " ⇒ {stoch} ⊆ ", rhs ])

  sem generateStochAppConstraints (lhs: Name) (rhs: Name) =
  | res /- : Name -/ -> [
      CstrStochApp { lhs = lhs, res = res},
      CstrConstStochApp { lhs = lhs, rhs = rhs, res = res},
      CstrConstApp { lhs = lhs, res = res}
    ]

  sem generateStochMatchResConstraints (res: Name) (target: Name) =
  | ( PatSeqTot _
    | PatSeqEdge _
    | PatCon _
    | PatInt _
    | PatChar _
    | PatBool _
    ) & pat -> [
      -- We only generate this constraint where a match can fail, causing a
      -- stochastic branch if the failed value is stochastic.
      CstrStochDirect { lhs = target, rhs = res }
    ]
  | ( PatAnd p
    | PatOr p
    | PatNot p
    ) -> infoErrorExit p.info "Pattern currently not supported"
  | _ -> []

  sem generateStochMatchConstraints (res: Name) (target: Name) =
  | pat ->
    recursive let f = lam acc. lam pat.
      let acc = match pat with PatNamed { ident = PName name }
                             | PatSeqEdge { middle = PName name }
                then cons name acc else acc in
      sfold_Pat_Pat f acc pat
    in
    let pnames = f [] pat in
    foldl (lam acc. lam name.
      cons (CstrStochDirect { lhs = target, rhs = name}) acc
    ) [] pnames

  sem constraintGenFuns =
  | _ -> [generateConstraints, generateStochConstraints]

  sem appConstraintGenFuns =
  | _ -> [generateLamAppConstraints, generateStochAppConstraints]

  sem matchConstraintGenFuns =
  | _ -> [
      generateMatchConstraints,
      generateStochMatchResConstraints,
      generateStochMatchConstraints
    ]

end

lang Test = MExprPPLAlign + DPPLParser
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
        (var, dataLookup (nameNoSym var) cfaRes)
      ) vars

    else
      -- Version without debug printouts
      let tANF = normalizeTerm t in
      let cfaRes = cfa tANF in
      map (lam var: String.
        (var, dataLookup (nameNoSym var) cfaRes)
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

-- Random function
let t = parse "
  let f =
    if assume (Bernoulli 0.5) then
      (lam x. addi x 1)
    else
      (lam y. addi y 2)
  in
  let res = f 5 in
  res
------------------------" in
utest test false t ["f","x","y","res"] with [
  ("f", true),
  ("x", false),
  ("y", false),
  ("res", true)
] using eqTestStoch in

-- Complicated stochastic match
let t = parse "
  let v1 = { a = [assume (Bernoulli 0.5)], b = 2} in
  let m1 =
    match v1 with { a = [true], b = b1 } then false
    else true
  in
  let v2 = { a = [false], b = assume (Beta 1.0 1.0) } in
  let m2 =
    match v2 with { a = [true], b = b2 } then false
    else true
  in
  ()
------------------------" in
utest test false t ["v1","m1","b1","v2","m2","b2"] with [
  ("v1", false),
  ("m1", true),
  ("b1", false),
  ("v2", false),
  ("m2", false),
  ("b2", true)
] using eqTestStoch in

-- Stochastic composite data
let t = parse "
  let d =
    match assume (Bernoulli 0.5) with true then
      let t1 = 1 in
      [t1,2,3]
    else
      [4,5,6]
  in
  let res =
    match d with [a] ++ rest ++ [] then false
    else true
  in
  res
------------------------" in
utest test false t ["d", "res", "rest","a","t1"] with [
  ("d", true),
  ("res", true),
  ("rest",true),
  ("a",true),
  ("t1",false)
] using eqTestStoch in

-- Stochastic flow over names
let t = parse "
  let res =
    match assume (Bernoulli 0.5) with true then
      let t = 1 in
      t
    else
      2
  in
  res
------------------------" in
utest test false t ["t", "res"] with [
  ("t", false),
  ("res",true)
] using eqTestStoch in

()
