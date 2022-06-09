-- Alignment analysis for CorePPL.

include "coreppl.mc"
include "parser.mc"

include "mexpr/cfa.mc"

lang PPLCFA = MExprCFA + MExprPPL

  type MCGF = Name -> Name -> Pat -> [Constraint]

  ----------------------------------------------------------------------------
  -- TODO(dlunde,2022-05-30): It would be nice if we could achieve the
  -- below in a more modular way (e.g., alignment part of analysis is defined
  -- in the alignment fragment, stochastic part in the stochastic fragment,
  -- etc.)
  ----------------------------------------------------------------------------
  sem generateStochMatchResConstraints : MCGF
  sem generateStochMatchConstraints : MCGF
  sem mcgfs : () -> [MCGF]
  sem mcgfs =
  | _ -> [
      generateMatchConstraints,
      generateStochMatchResConstraints,
      generateStochMatchConstraints
    ]
  sem generateStochConstraints : Expr -> [Constraint]
  sem generateAlignConstraints : Expr -> [Constraint]
  sem cgfs : [MCGF] -> [Expr -> [Constraint]]
  sem cgfs =
  | mcgfs -> [
      generateConstraints,
      generateConstraintsMatch mcgfs,
      generateStochConstraints,
      generateAlignConstraints
    ]
  ----------------------------------------------------------------------------

  -- For a given expression, returns all variables directly bound in that
  -- expression (all top-level let bindings).
  sem exprNames: Expr -> [Name]
  sem exprNames =
  | t -> exprNamesAcc [] t

  sem exprNamesAcc: [Name] -> Expr -> [Name]
  sem exprNamesAcc acc =
  | TmVar t -> acc
  | TmLet t -> exprNamesAcc (cons t.ident acc) t.inexpr
  | TmRecLets t ->
      foldl (lam acc. lam bind : RecLetBinding. cons bind.ident acc)
        acc t.bindings
  | TmType t -> exprNamesAcc acc t.inexpr
  | TmConDef t -> exprNamesAcc acc t.inexpr
  | TmUtest t -> exprNamesAcc acc t.next
  | TmExt t -> exprNamesAcc acc t.inexpr
  | t -> errorSingle [infoTm t] "Error in exprNames for CFA"

  -- Whether a pattern can fail
  sem patFail =
  | ( PatSeqTot _
    | PatSeqEdge _
    | PatCon _
    | PatInt _
    | PatChar _
    | PatBool _
    | PatRecord _
    ) & pat -> true
  | PatAnd p -> if patFail p.lpat then true else patFail p.rpat
  | PatOr p -> if patFail p.lpat then patFail p.rpat else false
  | PatNot p -> true
  | PatNamed _ -> false

  -- Type: Expr -> CFAGraph
  sem initGraph (graphData: Option GraphData) =
  | t ->

    -- Initial graph
    let graph: CFAGraph = emptyCFAGraph in

    -- Initialize match constraint generating functions
    let graph = { graph with mcgfs = mcgfs () } in

    -- Initialize constraint generating functions
    let cgfs = cgfs graph.mcgfs in

    -- Recurse over program and generate constraints
    let cstrs: [Constraint] = collectConstraints cgfs [] t in

    -- Initialize all collected constraints
    let graph = foldl initConstraint graph cstrs in

    -- Return graph
    graph

end

lang StochCFA = PPLCFA

  syn AbsVal =
  | AVStoch {}

  sem absValToString (env: PprintEnv) =
  | AVStoch {} -> (env, "stoch")

  sem cmpAbsValH =
  | (AVStoch _, AVStoch _) -> 0

  syn Constraint =
  -- {const} ⊆ lhs ⇒ ({stoch} ⊆ rhs ⇒ {stoch} ⊆ res)
  | CstrConstStochApp { lhs: Name, rhs: Name, res: Name }

  sem initConstraint (graph: CFAGraph) =
  | CstrConstStochApp r & cstr -> initConstraintName r.lhs graph cstr

  sem cstrStochDirect (lhs: Name) =
  | rhs -> CstrDirectAv {
      lhs = lhs, lhsav = AVStoch {}, rhs = rhs, rhsav = AVStoch {}
    }

  sem propagateConstraint (update: (Name,AbsVal)) (graph: CFAGraph) =
  | CstrConstStochApp { lhs = lhs, rhs = rhs, res = res } ->
    match update.1 with AVConst _ then
      initConstraint graph (cstrStochDirect rhs res)
    else graph

  -- This function is called from the base Miking CFA fragment when the
  -- constant application is complete (all arguments have been given).
  -- For the stochastic value flow, this is uninteresting. Consequently, as a
  -- default, we simply do nothing.
  sem propagateConstraintConst res args graph =
  | c -> graph

  sem constraintToString (env: PprintEnv) =
  | CstrConstStochApp { lhs = lhs, rhs = rhs, res = res } ->
    match pprintVarName env lhs with (env,lhs) in
    match pprintVarName env rhs with (env,rhs) in
    match pprintVarName env res with (env,res) in
    (env, join [
      "{const} ⊆ ", lhs, " AND {stoch} ⊆ ", rhs, " ⇒ {stoch} ⊆ ", res ])

  sem generateStochConstraints =
  | _ -> []
  -- Stochastic values
  | TmLet { ident = ident, body = TmAssume _ } ->
    [ CstrInit { lhs = AVStoch {}, rhs = ident } ]
  -- The below ensures all constants are tracked as needed for stoch
  -- propagation. In the base CFA fragment in Miking, constant constraints are
  -- only generated from a subset of constants. Here, we need to track _all_
  -- constants. NOTE(dlunde,2022-05-18): There is some duplication here, as
  -- some constants (e.g., constants for sequences) are generated both here and
  -- in the base Miking CFA.
  | TmLet { ident = ident, body = TmConst { val = c } } ->
    let arity = constArity c in
    if eqi arity 0 then []
    else [ CstrInit { lhs = AVConst { const = c, args = []}, rhs = ident } ]
  | TmLet { ident = ident, body = TmApp app} ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar r then [
        cstrStochDirect l.ident ident,
        CstrConstStochApp { lhs = l.ident, rhs = r.ident, res = ident}
      ]
      else errorSingle [infoTm app.rhs] "Not a TmVar in application"
    else errorSingle [infoTm app.lhs] "Not a TmVar in application"

  sem generateStochMatchResConstraints (id: Name) (target: Name) =
  -- Result of match is stochastic if match can fail stochastically
  | pat -> if patFail pat then [cstrStochDirect target id] else []

  -- Ensures all extracted pattern components of a stochastic value are also
  -- stochastic. For example, all elements of a stochastic list are stochastic.
  sem generateStochMatchConstraints (id: Name) (target: Name) =
  | pat ->
    recursive let f = lam acc. lam pat.
      let acc = match pat with PatNamed { ident = PName name }
                             | PatSeqEdge { middle = PName name }
                then cons name acc else acc in
      sfold_Pat_Pat f acc pat
    in
    let pnames = f [] pat in
    foldl (lam acc. lam name.
      cons (cstrStochDirect target name) acc
    ) [] pnames

end

lang AlignCFA = StochCFA

  syn AbsVal =
  | AVUnaligned {}

  sem absValToString (env: PprintEnv) =
  | AVUnaligned {} -> (env, "unaligned")

  sem cmpAbsValH =
  | (AVUnaligned _, AVUnaligned _) -> 0

  -- Alignment handling is custom (should not propagate as part of regular
  -- direct constraints)
  sem isDirect =
  | AVUnaligned _ -> false

  syn Constraint =
  -- {stoch} ⊆ target ⇒ for all n in names, {unaligned} ⊆ n
  | CstrStochAlign { target: Name, names: [Name] }
  -- {unaligned} ⊆ id ⇒ for all n in names, {unaligned} ⊆ n
  | CstrAlign { id: Name, names: [Name] }
  -- {unaligned} ⊆ id ⇒ ({lam x. b} ⊆ lhs ⇒ {unaligned} ⊆ x)
  | CstrAlignApp { id: Name, lhs: Name }
  -- {stoch} ⊆ lhs ⇒ ({lam x. b} ⊆ lhs ⇒ {unaligned} ⊆ x)
  | CstrStochAlignApp { lhs: Name }
  -- {lam x. b} ⊆ lhs ⇒ {unaligned} ⊆ x
  | CstrAlignLamApp { lhs: Name }

  sem initConstraint (graph: CFAGraph) =
  | CstrStochAlign r & cstr -> initConstraintName r.target graph cstr
  | CstrAlign r & cstr -> initConstraintName r.id graph cstr
  | CstrAlignApp r & cstr -> initConstraintName r.id graph cstr
  | CstrStochAlignApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrAlignLamApp r & cstr -> initConstraintName r.lhs graph cstr

  sem propagateConstraint (update: (Name,AbsVal)) (graph: CFAGraph) =
  | CstrStochAlign { target = target, names = names } ->
    match update.1 with AVStoch _ then
      foldl (lam graph. lam name. addData graph (AVUnaligned {}) name)
        graph names
    else graph
  | CstrAlign { id = id, names = names } ->
    match update.1 with AVUnaligned _ then
      foldl (lam graph. lam name. addData graph (AVUnaligned {}) name)
        graph names
    else graph
  | CstrAlignApp { id = id, lhs = lhs } ->
    match update.1 with AVUnaligned _ then
      initConstraint graph (CstrAlignLamApp { lhs = lhs })
    else graph
  | CstrStochAlignApp { lhs = lhs } ->
    match update.1 with AVStoch _ then
      initConstraint graph (CstrAlignLamApp { lhs = lhs })
    else graph
  | CstrAlignLamApp { lhs = lhs } ->
    match update.1 with AVLam { ident = x, body = b } then
      addData graph (AVUnaligned {}) x
    else graph

  sem constraintToString (env: PprintEnv) =
  | CstrStochAlign { target = target, names = names } ->
    match pprintVarName env target with (env,target) in
    match mapAccumL pprintVarName env names with (env,names) in
    (env, join [
      "{stoch} ⊆ ", target, " ⇒ {unaligned} ⊆ ", strJoin "," names
    ])
  | CstrAlign { id = id, names = names } ->
    match pprintVarName env id with (env,id) in
    match mapAccumL pprintVarName env names with (env,names) in
    (env, join [
      "{unaligned} ⊆ ", id, " ⇒ {unaligned} ⊆ ", strJoin "," names
    ])
  | CstrAlignApp { id = id, lhs = lhs } ->
    match constraintToString env (CstrAlignLamApp { lhs = lhs })
    with (env,rhs) in
    match pprintVarName env id with (env,id) in
    match pprintVarName env lhs with (env,lhs) in
    (env, join [ "{unaligned} ⊆ ", id, " ⇒ (", rhs, ")" ])
  | CstrStochAlignApp { lhs = lhs } ->
    match constraintToString env (CstrAlignLamApp { lhs = lhs })
    with (env,rhs) in
    match pprintVarName env lhs with (env,lhs) in
    (env, join [ "{stoch} ⊆ ", lhs, " ⇒ (", rhs, ")" ])
  | CstrAlignLamApp { lhs = lhs } ->
    match pprintVarName env lhs with (env,lhs) in
    (env, join [ "{lam >x<. >b<} ⊆ ", lhs, " ⇒ {unaligned} ⊆ >x<"])

  sem generateAlignConstraints  =
  | _ -> []
  | TmLet { ident = ident, body = TmLam t } ->
    [ CstrAlign { id = t.ident, names = exprNames t.body} ]
  | TmLet { ident = ident, body = TmMatch t } ->
    let innerNames = concat (exprNames t.thn) (exprNames t.els) in
    match t.target with TmVar tv then
      let cstrs =
        if patFail t.pat then
          [CstrStochAlign { target = tv.ident, names = innerNames }]
        else []
      in
      cons (CstrAlign { id = ident, names = innerNames }) cstrs
    else errorSingle [infoTm t.target] "Not a TmVar in match target"
  | TmLet { ident = ident, body = TmApp app} ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar r then
        [ CstrAlignApp { id = ident, lhs = l.ident },
          CstrStochAlignApp { lhs = l.ident }
        ]
      else errorSingle [infoTm app.rhs] "Not a TmVar in application"
    else errorSingle [infoTm app.lhs] "Not a TmVar in application"

end

lang MExprPPLCFA = StochCFA + AlignCFA
end

let extractUnaligned = use MExprPPLCFA in
  lam cfaRes: CFAGraph.
    mapFoldWithKey (lam acc: Set Name. lam k: Name. lam v: Set AbsVal.
        if setAny (lam av. match av with AVUnaligned _ then true else false) v
        then setInsert k acc
        else acc
      ) (setEmpty nameCmp) cfaRes.data

lang Test = MExprPPLCFA + MExprANFAll + DPPLParser
end

-----------
-- TESTS --
-----------

mexpr
use Test in

-- Test functions --
let _parse = parseMExprPPLString in
let _testBase: Option PprintEnv -> Expr -> (Option PprintEnv, CFAGraph) =
  lam env: Option PprintEnv. lam t: Expr.
    match env with Some env then
      -- Version with debug printouts
      let tANF = normalizeTerm t in
      match pprintCode 0 env t with (env,tStr) in
      printLn "\n--- ORIGINAL PROGRAM ---";
      printLn tStr;
      match pprintCode 0 env tANF with (env,tANFStr) in
      printLn "\n--- ANF ---";
      printLn tANFStr;
      match cfaDebug (None ()) (Some env) tANF with (Some env,cfaRes) in
      match cfaGraphToString env cfaRes with (env, resStr) in
      printLn "\n--- FINAL CFA GRAPH ---";
      printLn resStr;
      (Some env,cfaRes)

    else
      -- Version without debug printouts
      let tANF = normalizeTerm t in
      let cfaRes = cfa tANF in
      (None (), cfaRes)
in


---------------------------------
-- STOCHASTIC VALUE FLOW TESTS --
---------------------------------

let _test: Bool -> Expr -> [String] -> [(String,Bool)] =
  lam debug. lam t. lam vars.
    let env = if debug then Some pprintEnvEmpty else None () in
    match _testBase env t with (_, cfaRes) in
    map (lam var: String.
      let avs = dataLookup (nameNoSym var) cfaRes in
      let val = setFold
        (lam acc. lam av. match av with AVStoch _ then true else acc) false avs
      in (var, val)
    ) vars
in

-- Custom equality function for testing stochastic value flow only
let eqTest = eqSeq (lam t1:(String,Bool). lam t2:(String,Bool).
  if eqString t1.0 t2.0 then eqBool t1.1 t2.1
  else false
) in
--------------------

let t = _parse "
  let x = assume (Beta 1.0 1.0) in
  let f = addf in
  let sum1 = addf 1.0 1.0 in
  let sum2 = f x 1.0 in
  let res = addf sum1 sum2 in
  res
------------------------" in
utest _test false t ["x","f","sum1","sum2","res"] with [
  ("x", true),
  ("f", false),
  ("sum1", false),
  ("sum2", true),
  ("res", true)
] using eqTest in

let t = _parse "
  let f = (lam x. addf x 1) in
  let g = (lam y. addf y 2) in
  let s = assume (Beta 1.0 1.0) in
  let res = addf (f s) (g 1) in
  res
------------------------" in
utest _test false t ["f","g","s","res","x","y"] with [
  ("f", false),
  ("g", false),
  ("s", true),
  ("res", true),
  ("x", true),
  ("y", false)
] using eqTest in


let t = _parse "
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
utest _test false t ["x","d","res","a","b"] with [
  ("x", true),
  ("d", false),
  ("res", true),
  ("a", true),
  ("b", false)
] using eqTest in

-- Random function
let t = _parse "
  let f =
    if assume (Bernoulli 0.5) then
      (lam x. addi x 1)
    else
      (lam y. addi y 2)
  in
  let res = f 5 in
  res
------------------------" in
utest _test false t ["f","x","y","res"] with [
  ("f", true),
  ("x", false),
  ("y", false),
  ("res", true)
] using eqTest in

-- Complicated stochastic match
let t = _parse "
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
utest _test false t ["v1","m1","b1","v2","m2","b2"] with [
  ("v1", false),
  ("m1", true),
  ("b1", false),
  ("v2", false),
  ("m2", false),
  ("b2", true)
] using eqTest in

-- Stochastic composite data
let t = _parse "
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
utest _test false t ["d", "res", "rest","a","t1"] with [
  ("d", true),
  ("res", true),
  ("rest",true),
  ("a",true),
  ("t1",false)
] using eqTest in

-- Stochastic flow over names
let t = _parse "
  let res =
    match assume (Bernoulli 0.5) with true then
      let t = 1 in
      t
    else
      2
  in
  res
------------------------" in
utest _test false t ["t", "res"] with [
  ("t", false),
  ("res",true)
] using eqTest in

---------------------
-- ALIGNMENT TESTS --
---------------------

let _test: Bool -> Expr -> [String] -> [([Char], Bool)] =
  lam debug. lam t. lam vars.
    let tANF = normalizeTerm t in
    let env = if debug then Some pprintEnvEmpty else None () in
    match _testBase env tANF with (env, cfaRes) in
    let aRes: Set Name = extractUnaligned cfaRes in
    let sSet: Set String = setFold
      (lam acc. lam n. setInsert (nameGetStr n) acc)
      (setEmpty cmpString) aRes in
    map (lam var: String. (var, not (setMem var sSet))) vars
in


let t = _parse "
  let x = assume (Bernoulli 0.5) in
  let res =
    if x then
      let p1 = addf 1.0 1.0 in
      p1
    else
      let p2 = addf 2.0 2.0 in
      p2
  in res
------------------------" in
utest _test false t ["x","res","p1","p2"] with [
  ("x", true),
  ("res", true),
  ("p1", false),
  ("p2", false)
] using eqTest in

let t = _parse "
  let f1 = (lam x1. let w1 = weight 1.0 in w1) in
  let f2 = (lam x2. let w2 = weight 2.0 in w2) in
  let f3 = (lam x3. let w3 = weight 3.0 in w3) in
  recursive let top = lam i1.
    let m1 =
      match assume (Bernoulli 0.5) with true
      then f2 2
      else f3 3
    in
    let f4 =
      if eqi 0 i1
      then f1
      else f2
    in
    let res = f4 4 in
    if eqi 0 i1 then res else top (subi i1 1)
  in
  top 10
------------------------" in
utest _test false t ["f1","f2","f3","w1","w2","w3"] with [
  ("f1", true),
  ("f2", true),
  ("f3", true),
  ("w1", true),
  ("w2", false),
  ("w3", false)
] using eqTest in

let t = _parse "
  let f =
    match assume (Bernoulli 0.5) with true then
      (lam x. let w1 = 1 in w1)
    else
      (lam y. let w2 = 2 in w2)
  in
  let res = f 1 in
  res
------------------------" in
utest _test false t ["f","x","w1","y","w2","res"] with [
  ("f", true),
  ("x", false),
  ("w1", false),
  ("y", false),
  ("w2", false),
  ("res", true)
] using eqTest in

let t = _parse "
  recursive let f = lam x. lam y. x in
  f 1 2
------------------------" in
utest _test false t ["f"] with [
  ("f", true)
] using eqTest in

-- Matching on stochastic record
let t = _parse "
  let r =
    if assume (Bernoulli 0.5) then
      { a = 1, b = 2 }
    else
      { a = 2, b = 1 }
  in
  let res =
    match r with { a = 1, b = 2 } then
      let t1 = 1 in
      t1
    else
      let t2 = 1 in
      t2
  in
  res
------------------------" in
utest _test false t ["t1", "t2", "res"] with [
  ("t1", false),
  ("t2", false),
  ("res", true)
] using eqTest in


-- Test in `models/coreppl/crbd/crbd-unaligned.mc`
let t = symbolizeExpr symEnvEmpty
          (parseMCorePPLFile "coreppl/models/crbd/crbd-unaligned.mc") in
utest _test false t ["w1","w2","w3"] with [
  ("w1", false),
  ("w2", false),
  ("w3", true)
]
using eqTest in

()

