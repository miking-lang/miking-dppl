-- Alignment analysis for CorePPL.

include "coreppl.mc"
include "parser.mc"

include "mexpr/cfa.mc"

lang MExprPPLStochCFA = MExprCFA + MExprPPL

  syn AbsVal =
  | AVStoch {}

  sem absValToString (env: PprintEnv) =
  | AVStoch {} -> (env, "stoch")

  sem cmpAbsValH =
  | (AVStoch _, AVStoch _) -> 0

  syn Constraint =
  -- {const} ⊆ lhs AND {stoch} ⊆ rhs ⇒ {stoch} ⊆ res
  | CstrConstStochApp { lhs: Name, rhs: Name, res: Name }
  -- {stoch} ⊆ target ⇒ match condition is stochastic for match id
  | CstrStochMatchCond { target: Name, id: Name }

  sem initConstraint (graph: CFAGraph) =
  | CstrConstStochApp r & cstr ->
    let graph = initConstraintName r.lhs graph cstr in
    initConstraintName r.rhs graph cstr
  | CstrStochMatchCond r & cstr -> initConstraintName r.target graph cstr

  sem propagateConstraint (update: (Name,AbsVal)) (graph: CFAGraph) =
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
        let s = dataLookup lhs graph in
        if setAny (lam av. match av with AVConst _ then true else false) s then
          addData graph (AVStoch {}) res
        else graph
      else graph
    else graph
  | CstrStochMatchCond { target = target, id = id } ->
    match update.1 with AVStoch _ then
      { graph with stochMatches = setInsert id graph.stochMatches }
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
  | CstrStochMatchCond { target = target, id = id } ->
    match pprintVarName env target with (env,target) in
    match pprintVarName env id with (env,id) in
    (env, join [
      "{stoch} ⊆ ", target, " ⇒ match condition is stochastic for match ", id
    ])

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
      else infoErrorExit (infoTm app.rhs) "Not a TmVar in application"
    else infoErrorExit (infoTm app.lhs) "Not a TmVar in application"

  sem cstrStochDirect (lhs: Name) =
  | rhs -> CstrDirectAv {
      lhs = lhs, lhsav = AVStoch {}, rhs = rhs, rhsav = AVStoch {}
    }

  sem generateStochMatchResConstraints (id: Name) (target: Name) =
  | ( PatSeqTot _
    | PatSeqEdge _
    | PatCon _
    | PatInt _
    | PatChar _
    | PatBool _
    | PatRecord _
    ) & pat ->
    -- We only generate these constraint where a match can fail, causing a
    -- stochastic branch if the failed value is stochastic.
    [

      -- Result of match is stochastic if match can fail stochastically
      cstrStochDirect target id,

      -- match is unaligned if it can fail stochastically
      CstrStochMatchCond { target = target, id = id }

    ]
  | ( PatAnd p
    | PatOr p
    | PatNot p
    ) -> infoErrorExit p.info "Pattern currently not supported"
  | _ -> []

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

  -- Type: Expr -> CFAGraph
  sem initGraph (graphData: Option GraphData) =
  | t ->

    -- Initial graph
    let graph: CFAGraph = emptyCFAGraph in

    -- Initialize match constraint generating functions
    let graph = { graph with mcgfs = [
      generateMatchConstraints,
      generateStochMatchResConstraints,
      generateStochMatchConstraints
    ] } in

    -- Initialize constraint generating functions
    let cgfs = [
      generateConstraints,
      generateConstraintsMatch graph.mcgfs,
      generateStochConstraints
    ] in

    -- Recurse over program and generate constraints
    let cstrs: [Constraint] = collectConstraints cgfs [] t in

    -- Initialize all collected constraints
    let graph = foldl initConstraint graph cstrs in

    -- Return graph
    graph

end

-- TODO(dlunde,2022-04-21): I want to move this inside Align fragment as well,
-- but it doesn't seem to work yet.
type AlignFlow = {
  -- Unaligned names
  unaligned: [Name],
  -- LHS names of applications
  lhss: [Name]
}

lang Align = MExprPPLCFA

-- Types for keeping track of alignment flow
  type AlignAcc = {
    -- Map for matches (let key = match ...)
    mMap: Map Name AlignFlow,
    -- Map for lambdas (lam key. ...)
    lMap: Map Name AlignFlow,
    -- Current flow accumulator
    current: AlignFlow
  }
  sem emptyAlignFlow : () -> AlignFlow
  sem emptyAlignFlow =
  | _ -> { unaligned = [], lhss = [] }

  -- Type: Expr -> CFAGraph -> Set Name
  -- Returns a list of unaligned names for a program.
  sem alignment (cfaRes: CFAGraph) =
  | t -> match alignmentDebug (None ()) cfaRes t with (_,res) in res

  sem alignmentDebug (env: Option PprintEnv) (cfaRes: CFAGraph) =
  | t ->

    -- Construct unaligned name map (for matches and lambdas)
    let m = mapEmpty nameCmp in
    let acc: AlignAcc = { mMap = m, lMap = m, current = emptyAlignFlow () } in
    match alignMap acc t with acc in
    let acc: AlignAcc = acc in
    let uMatch = acc.mMap in
    let uLam = acc.lMap in

    let env = match env with Some env then
        match mapAccumL pprintVarName env (setToSeq cfaRes.stochMatches)
        with (env,stochMatches) in
        match alignFlowMapToString env uMatch with (env,uMatch) in
        match alignFlowMapToString env uLam with (env,uLam) in
        print "*** Stochastic matches ***\n";
        printLn (strJoin ", " stochMatches);
        print "*** Unaligned match map ***\n";
        printLn uMatch;
        print "*** Unaligned lambda map ***\n";
        printLn uLam;
        Some env
      else None ()
    in

    type AccIter = {
      unaligned: Set Name,
      processedLams: Set Name,
      processedLhss: Set Name,
      newLhss: [Name]
    } in

    let emptyAccIter = {
      unaligned = setEmpty nameCmp,
      processedLams = setEmpty nameCmp,
      processedLhss = setEmpty nameCmp,
      newLhss = []
    } in

    -- Main alignment propagation
    recursive let iter = lam acc: AccIter.
      match acc.newLhss with [h] ++ newLhss then
        let acc = { acc with newLhss = newLhss } in
        if setMem h acc.processedLhss then iter acc
        else
          let acc = { acc with processedLhss = setInsert h acc.processedLhss } in
          let avs = dataLookup h cfaRes in
          let acc = setFold (lam acc: AccIter. lam av.
              match av with AVLam { ident = ident } then
                if setMem ident acc.processedLams then acc
                else
                  let v: AlignFlow = mapFindExn ident uLam in
                  {{{ acc
                    with unaligned =
                      foldl (lam ua. lam n. setInsert n ua)
                        acc.unaligned v.unaligned }
                    with processedLams =
                      setInsert ident acc.processedLams }
                    with newLhss =
                      concat v.lhss acc.newLhss }
              else acc
            ) acc avs
          in
          iter acc
      else match acc.newLhss with [] then acc.unaligned
      else never
    in

    -- Initial recursion over stochastic applications
    recursive let recapp = lam acc: AccIter. lam t: Expr.
      match t with TmApp { lhs = TmVar { ident = ident } } then
        let avs = dataLookup ident cfaRes in
        if setMem (AVStoch {}) avs then
          let acc =
            { acc with processedLhss = setInsert ident acc.processedLhss } in
          setFold (lam acc: AccIter. lam av.
            match av with AVLam { ident = ident } then
              let v: AlignFlow = mapFindExn ident uLam in
              {{{ acc
                with unaligned =
                  foldl (lam ua. lam n. setInsert n ua)
                    acc.unaligned v.unaligned }
                with processedLams =
                  setInsert ident acc.processedLams }
                with newLhss =
                  concat v.lhss acc.newLhss }
            else acc
          ) acc avs
        else acc
      else sfold_Expr_Expr recapp acc t
    in

    -- Initial stochastic matches (provided by CFA analysis)
    let acc = setFold (lam acc: AccIter. lam matchId.
      let v: AlignFlow = mapFindExn matchId uMatch in
      {{ acc
        with unaligned =
          foldl (lam ua. lam n. setInsert n ua) acc.unaligned v.unaligned }
        with newLhss = concat v.lhss acc.newLhss }
    ) emptyAccIter cfaRes.stochMatches
    in

    let acc = recapp acc t in

    let res = iter acc in

    let env = match env with Some env then
        printLn "***UNALIGNED NAMES***";
        match mapAccumL pprintVarName env (setToSeq res) with (_,res) in
        printLn (strJoin ", " res);
        Some env
      else None ()
    in

    (env, res)

  sem alignFlowMapToString (env: PprintEnv) =
  | m ->
    let f = lam env. lam k. lam v: AlignFlow.
      match pprintVarName env k with (env, k) in
      match mapAccumL pprintVarName env v.unaligned with (env, unaligned) in
      match mapAccumL pprintVarName env v.lhss with (env, lhss) in
      let unaligned = strJoin ", " unaligned in
      let lhss = strJoin ", " lhss in
      (env, join [k, " ->\n  unaligned: ", unaligned, "\n  lhss: ", lhss])
    in
    match mapMapAccum f env m with (env, m) in
    (env, strJoin "\n" (mapValues m))

  sem alignMap (acc: AlignAcc) =
  | TmLet { ident = ident, body = TmApp a, inexpr = inexpr } ->
    -- a.rhs is a variable due to ANF, no need to recurse
    -- Add new values
    let c: AlignFlow = acc.current in
    let current = {{ c
      with unaligned = cons ident c.unaligned }
      with lhss =
        let lhs = match a.lhs with TmVar t then t.ident
          else infoErrorExit (infoTm a.lhs) "Not a TmVar in application" in
        cons lhs c.lhss }
    in
    let acc = { acc with current = current } in
    alignMap acc inexpr
  | TmLet { ident = ident, body = TmMatch m, inexpr = inexpr } ->
    -- m.target is a TmVar due to ANF, can safely be ignored here
    let c: AlignFlow = acc.current in
    let current = {
      c with unaligned = cons ident c.unaligned
    } in
    let acc = { acc with current = current } in
    -- Recursion
    let accI: AlignAcc = { acc with current = emptyAlignFlow () } in
    match alignMap accI m.thn with accI in
    match alignMap accI m.els with accI in
    let accI: AlignAcc = accI in
    -- Record inner map and define next current
    let mMap = mapInsert ident accI.current accI.mMap in
    let c: AlignFlow = acc.current in
    let ci: AlignFlow = accI.current in
    let current = {
      -- Flow in inner match is also part of outer lam/match
      unaligned = concat c.unaligned ci.unaligned,
      lhss = concat c.lhss ci.lhss
    } in
    let acc = {{ accI with mMap = mMap } with current = current } in
    alignMap acc inexpr
  | TmLet { ident = ident, body = TmLam b, inexpr = inexpr } ->
    let c: AlignFlow = acc.current in
    let current = {
      c with unaligned = cons ident c.unaligned
    } in
    let acc = { acc with current = current } in
    let accI: AlignAcc = { acc with current = emptyAlignFlow () } in
    match alignMap accI b.body with accI in
    let accI: AlignAcc = accI in
    let lMap = mapInsert b.ident accI.current accI.lMap in
    let acc = {{ accI with lMap = lMap } with current = current } in
    alignMap acc inexpr
  | TmRecLets { bindings = bindings, inexpr = inexpr } ->
    let acc = foldl (lam acc: AlignAcc. lam b: RecLetBinding.
        match b.body with TmLam t then
          let c: AlignFlow = acc.current in
          let current = {
            c with unaligned = cons b.ident c.unaligned
          } in
          let acc = { acc with current = current } in
          let accI: AlignAcc = { acc with current = emptyAlignFlow () } in
          match alignMap accI t.body with accI in
          let accI: AlignAcc = accI in
          let lMap = mapInsert t.ident accI.current accI.lMap in
          {{ accI with lMap = lMap } with current = current }
        else infoErrorExit (infoTm b.body) "Not a lambda in recursive let body"
      ) acc bindings in
    alignMap acc inexpr
  | TmLet { ident = ident, body = body, inexpr = inexpr } ->
    let c: AlignFlow = acc.current in
    let current = {
      c with unaligned = cons ident c.unaligned
    } in
    let acc = { acc with current = current } in
    alignMap acc inexpr
  | t -> sfold_Expr_Expr alignMap acc t

end

lang Test = Align + MExprANFAll + DPPLParser
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
    match alignmentDebug env cfaRes tANF with (_, aRes) in
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
  ("x", true),
  ("w1", false),
  ("y", true),
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

