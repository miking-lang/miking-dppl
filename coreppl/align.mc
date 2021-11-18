-- Alignment analysis for CorePPL.
--

include "coreppl.mc"
include "dppl-parser.mc"

include "mexpr/cfa.mc"

type UnalignedMap = Map Name [Name]
type UnalignedAccOutside = { mMap: UnalignedMap, lMap: UnalignedMap }
type UnalignedAccInside = { mMap: UnalignedMap, lMap: UnalignedMap, ns: [Name] }

lang MExprPPLAlign = MExprCFA + MExprPPL

  syn AbsVal =
  | AVStoch {}
  | AVUnaligned {}
  | AVConst { arity: Int }

  sem absValToString (env: PprintEnv) =
  | AVStoch {} -> (env, "stoch")
  | AVUnaligned {} -> (env, "unaligned")
  | AVConst { arity = arity } -> (env, join ["const", int2string arity])

  sem cmpAbsValH =
  | (AVStoch _, AVStoch _) -> 0
  | (AVUnaligned _, AVUnaligned _) -> 0
  | (AVConst lhs, AVConst rhs) -> subi lhs.arity rhs.arity

  syn Constraint =
  -- {stoch} ⊆ lhs ⇒ {stoch} ⊆ res
  | CstrStochApp { lhs: Name, res: Name }
  -- {const} ⊆ lhs AND {stoch} ⊆ rhs ⇒ {stoch} ⊆ res
  | CstrConstStochApp { lhs: Name, rhs: Name, res: Name }
  -- {const with arity > 0} ⊆ lhs ⇒ {const with arity-1} ⊆ res
  | CstrConstApp { lhs: Name, res: Name }
  -- {lam >x<. >b<} ⊆ lhs
  --   ⇒ forall n in unaligned(>x>).
  --        (({unaligned} ⊆ lhs ⇒ {unaligned} ⊆ n) AND
  --             ({stoch} ⊆ lhs ⇒ {unaligned} ⊆ n))
  | CstrUnalignedApp { lhs: Name, nameMap: Map Name [Name] }


  sem initConstraint (graph: CFAGraph) =
  | CstrStochApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrConstStochApp r & cstr ->
    let graph = initConstraintName r.lhs graph cstr in
    initConstraintName r.rhs graph cstr
  | CstrConstApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrUnalignedApp r & cstr -> initConstraintName r.lhs graph cstr

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
        let s = dataLookup lhs graph in
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
  | CstrUnalignedApp { lhs = lhs, nameMap = nameMap } ->
    match update.1 with AVLam { ident = x, body = b } then
      let names = mapFindExn x nameMap in
      -- let names = mapLookupOrElse (lam. []) x nameMap in

      foldl (lam graph. lam n.
        let graph =
           initConstraint graph (CstrDirectAv {
             lhs = lhs, lhsav = AVUnaligned {}, rhs = n, rhsav = AVUnaligned {}
           })
        in initConstraint graph (CstrDirectAv {
             lhs = lhs, lhsav = AVStoch {}, rhs = n, rhsav = AVUnaligned {}
           })
      ) graph names
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
  | CstrUnalignedApp { lhs = lhs, nameMap = nameMap } ->
    match pprintVarName env lhs with (env,lhs) in
    (env, join [
      "{lam >x<. >b<} ⊆ ", lhs, " ⇒ forall n in unaligned(>x>). (",
      "({unaligned} ⊆ ", lhs, " ⇒ {unaligned} ⊆ n) AND",
      "({stoch} ⊆ ", lhs, " ⇒ {unaligned} ⊆ n))"
    ])

  sem generateStochConstraints (unaligned: UnalignedMap) =
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
  | TmLet { ident = ident, body = TmApp app} ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar r then [
        CstrStochApp { lhs = l.ident, res = ident},
        CstrConstStochApp { lhs = l.ident, rhs = r.ident, res = ident},
        CstrConstApp { lhs = l.ident, res = ident},
        CstrUnalignedApp { lhs = l.ident, nameMap = unaligned }
      ]
      else infoErrorExit (infoTm app.rhs) "Not a TmVar in application"
    else infoErrorExit (infoTm app.lhs) "Not a TmVar in application"

  sem cstrStochDirect (lhs: Name) =
  | rhs -> CstrDirectAv {
      lhs = lhs, lhsav = AVStoch {}, rhs = rhs, rhsav = AVStoch {}
    }

  sem generateStochMatchResConstraints
    (unaligned: Map Name (Set Name)) (id: Name) (target: Name) =
  | ( PatSeqTot _
    | PatSeqEdge _
    | PatCon _
    | PatInt _
    | PatChar _
    | PatBool _
    ) & pat ->
    -- We only generate these constraint where a match can fail, causing a
    -- stochastic branch if the failed value is stochastic.
    join [

      -- Result of match is stochastic if match is stochastic
      [cstrStochDirect target id],

      -- Contents of match is unaligned if match is stochastic
      -- forall n in unaligned(id). {stoch} ⊆ target ⇒ {unaligned} ⊆ n
      map (lam n.
        (CstrDirectAv {
          lhs = target, lhsav = AVStoch {}, rhs = n, rhsav = AVUnaligned {}
        })
      ) (mapFindExn id unaligned)
    ]
  | ( PatAnd p
    | PatOr p
    | PatNot p
    ) -> infoErrorExit p.info "Pattern currently not supported"
  | _ -> []

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

  sem unalignedOutside (acc: UnalignedAccOutside) =
  | TmLet { ident = ident, body = TmMatch m, inexpr = inexpr } ->
    -- m.target is a TmVar due to ANF, can safely be ignored here
    let acc = { mMap = acc.mMap, lMap = acc.lMap, ns = [] } in
    match unalignedInside acc m.thn with acc in
    match unalignedInside acc m.els with acc in
    let acc: UnalignedAccInside = acc in
    let acc = { mMap = mapInsert ident acc.ns acc.mMap, lMap = acc.lMap } in
    unalignedOutside acc inexpr
  | TmLet { ident = ident, body = TmLam b, inexpr = inexpr } ->
    let acc = { mMap = acc.mMap, lMap = acc.lMap, ns = [] } in
    match unalignedInside acc b.body with acc in
    let acc: UnalignedAccInside = acc in
    let acc = { mMap = acc.mMap, lMap = mapInsert b.ident acc.ns acc.lMap } in
    unalignedOutside acc inexpr
  | TmRecLets { bindings = bindings, inexpr = inexpr } ->
    let acc = { mMap = acc.mMap, lMap = acc.lMap, ns = [] } in
    let acc = foldl (lam acc: UnalignedAccInside. lam b: RecLetBinding.
        match b.body with TmLam t then
          let acc = { mMap = acc.mMap, lMap = acc.lMap, ns = [] } in
          match unalignedInside acc t.body with acc in
          let acc: UnalignedAccInside = acc in
          { acc with lMap = mapInsert t.ident acc.ns acc.lMap }
        else infoErrorExit (infoTm b.body) "Not a lambda in recursive let body"
      ) acc bindings in
    let acc: UnalignedAccInside = acc in
    let acc = { mMap = acc.mMap, lMap = acc.lMap } in
    unalignedOutside acc inexpr
  | t -> sfold_Expr_Expr unalignedOutside acc t

  sem unalignedInside (acc: UnalignedAccInside) =
  | TmLet { ident = ident, body = TmApp a, inexpr = inexpr } ->
    let acc = { acc with ns = cons ident acc.ns } in
    let lhs = match a.lhs with TmVar t then t.ident
              else infoErrorExit (infoTm a.lhs) "Not a TmVar in application" in
    let acc = { acc with ns = cons lhs acc.ns } in
    -- a.rhs is a variable due to ANF, no need to recurse
    unalignedInside acc inexpr
  | TmLet { ident = ident, body = TmMatch m, inexpr = inexpr } ->
    -- Set up new accumulator
    let accI = { acc with ns = [] } in
    -- Recursive calls
    match unalignedInside accI m.thn with accI in
    match unalignedInside accI m.els with accI in
    let accI: UnalignedAccInside = accI in
    -- Add new binding to map
    let accI = { accI with mMap = mapInsert ident accI.ns accI.mMap } in
    -- Attach names in internal match to this accumulator as well
    let acc = { accI with ns = concat acc.ns accI.ns } in
    unalignedInside acc inexpr
  | TmLet { ident = ident, body = TmLam b, inexpr = inexpr } ->
    let accI = { acc with ns = [] } in
    match unalignedInside accI b.body with accI in
    let accI: UnalignedAccInside = accI in
    let accI = { accI with lMap = mapInsert b.ident accI.ns accI.lMap } in
    -- Restore original ns
    let acc = { accI with ns = acc.ns } in
    unalignedInside acc inexpr
  | TmRecLets { bindings = bindings, inexpr = inexpr } ->
    let accI: UnalignedAccInside =
      foldl (lam accI: UnalignedAccInside. lam b: RecLetBinding.
        match b.body with TmLam t then
          let accI = { accI with ns = [] } in
          match unalignedInside accI t.body with accI in
          let accI: UnalignedAccInside = accI in
          { accI with lMap = mapInsert t.ident accI.ns accI.lMap }
        else infoErrorExit (infoTm b.body) "Not a lambda in recursive let body"
      ) acc bindings in
    let acc = { accI with ns = acc.ns } in
    unalignedInside acc inexpr
  | TmLet { ident = ident, body = body, inexpr = inexpr } ->
    let acc = { acc with ns = cons ident acc.ns } in
    unalignedInside acc inexpr
  | t -> sfold_Expr_Expr unalignedInside acc t

  -- Type: Expr -> CFAGraph
  sem initGraph =
  | t ->

    -- Initial graph
    let graph = emptyCFAGraph in

    -- Construct unaligned name map (for matches and lambdas)
    let m = mapEmpty nameCmp in
    let acc = { mMap = m, lMap = m } in
    match unalignedOutside acc t with acc in
    let acc: UnalignedAccOutside = acc in
    let uMatch = acc.mMap in
    let uLam = acc.lMap in

    -- Initialize match constraint generating functions
    let graph = { graph with mcgfs = [
      generateMatchConstraints,
      generateStochMatchResConstraints uMatch,
      generateStochMatchConstraints
    ] } in

    -- Initialize constraint generating functions
    let cgfs = [
      generateConstraints,
      generateConstraintsMatch graph.mcgfs,
      generateStochConstraints uLam
    ] in

    -- Recurse over program and generate constraints
    let cstrs: [Constraint] = collectConstraints cgfs [] t in

    -- Initialize all collected constraints
    let graph = foldl initConstraint graph cstrs in

    -- Return graph
    graph

end

lang Test = MExprPPLAlign + MExprANFAll + DPPLParser
end

-----------
-- TESTS --
-----------

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
      match cfaDebug (Some env) tANF with (Some env,cfaRes) in
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


---------------------------------
-- STOCHASTIC VALUE FLOW TESTS --
---------------------------------

-- Custom equality function for testing stochastic value flow only
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

---------------------
-- ALIGNMENT TESTS --
---------------------
-- Custom equality function for testing alignment only
let eqTestAlign = eqSeq (lam t1:(String,Set AbsVal). lam t2:(String,Bool).
  if eqString t1.0 t2.0 then
    let t11 = setFold
      (lam acc. lam av. match av with AVUnaligned _ then false else acc)
      true t1.1
    in
    eqBool t11 t2.1
  else false
) in
--------------------

let t = parse "
  let x = assume (Beta 1.0 1.0) in
  let res = if x then
    let p1 = addf 1.0 1.0 in
    p1
  else
    let p2 = addf 2.0 2.0 in
    p2
  in res
------------------------" in
utest test false t ["x","res","p1","p2"] with [
  ("x", true),
  ("res", false),
  -- TODO(dlunde,2021-11-18): Intuitively, res should be aligned here. The
  -- problem is that the "unaligned" from p1 flows directly to res. It could
  -- be that this does not actually result in any problems, but I'm not sure.
  -- Could probably be fixed without too much effort.
  ("p1", false),
  ("p2", false)
] using eqTestAlign in

()
