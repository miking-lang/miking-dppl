-- Alignment analysis for CorePPL.

include "coreppl.mc"
include "parser.mc"

include "mexpr/cfa.mc"

lang PPLCFA = MExprCFA + MExprPPL

  ----------------------------------------------------------------------------
  -- TODO(dlunde,2022-05-30): It would be nice if we could achieve the
  -- below in a more modular way (e.g., alignment part of analysis is defined
  -- in the alignment fragment, stochastic part in the stochastic fragment,
  -- etc.).
  ----------------------------------------------------------------------------
  sem generateStochMatchResConstraints : MatchGenFun
  sem generateStochMatchConstraints : IndexMap -> MatchGenFun
  sem mcgfs : IndexMap -> [MatchGenFun]
  sem mcgfs =
  | im -> [
      generateMatchConstraints,
      generateStochMatchResConstraints,
      generateStochMatchConstraints im
    ]
  sem generateStochConstraints : IndexMap -> GenFun
  sem generateAlignConstraints : IndexMap -> GenFun
  sem generateCheckpointInitConstraints : IndexMap -> GenFun
  sem generateCheckpointConstraints : IndexMap -> GenFun
  sem cgfs : CFAGraph -> [Expr -> [Constraint]]
  sem cgfs =
  | graph -> [
      generateConstraints graph.im,
      generateConstraintsMatch graph.im graph.mcgfs,
      generateStochConstraints graph.im,
      generateAlignConstraints graph.im,
      generateCheckpointInitConstraints graph.im,
      generateCheckpointConstraints graph.im
    ]
  ----------------------------------------------------------------------------

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
    let graph: CFAGraph = emptyCFAGraph t in

    -- Initialize match constraint generating functions
    let graph = { graph with mcgfs = mcgfs graph.im } in

    -- Initialize constraint generating functions
    let cgfs: [Expr -> [Constraint]] = cgfs graph in

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

  sem absValToString graph (env: PprintEnv) =
  | AVStoch {} -> (env, "stoch")

  sem cmpAbsValH =
  | (AVStoch _, AVStoch _) -> 0

  syn Constraint =
  -- {const} ⊆ lhs ⇒ ({stoch} ⊆ rhs ⇒ {stoch} ⊆ res)
  | CstrConstStochApp { lhs: IName, rhs: IName, res: IName }
  -- {ext} ⊆ lhs ⇒ ({stoch} ⊆ rhs ⇒ {stoch} ⊆ res)
  | CstrExtStochApp { lhs: IName, rhs: IName, res: IName }

  sem initConstraint (graph: CFAGraph) =
  | CstrConstStochApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrExtStochApp r & cstr -> initConstraintName r.lhs graph cstr

  sem cstrStochDirect (lhs: IName) =
  | rhs -> CstrDirectAv {
      lhs = lhs, lhsav = AVStoch {}, rhs = rhs, rhsav = AVStoch {}
    }

  sem propagateConstraint (update: (IName,AbsVal)) (graph: CFAGraph) =
  | CstrConstStochApp { lhs = lhs, rhs = rhs, res = res } ->
    match update.1 with AVConst _ then
      initConstraint graph (cstrStochDirect rhs res)
    else graph
  | CstrExtStochApp { lhs = lhs, rhs = rhs, res = res } ->
    match update.1 with AVExt _ then
      initConstraint graph (cstrStochDirect rhs res)
    else graph

  -- This function is called from the base Miking CFA fragment when the
  -- constant application is complete (all arguments have been given).
  -- For the stochastic value flow, this is uninteresting. Consequently, as a
  -- default, we simply do nothing.
  sem propagateConstraintConst res args graph =
  | c -> graph

  sem constraintToString graph (env: PprintEnv) =
  | CstrConstStochApp { lhs = lhs, rhs = rhs, res = res } ->
    match pprintVarIName graph.im env lhs with (env,lhs) in
    match pprintVarIName graph.im env rhs with (env,rhs) in
    match pprintVarIName graph.im env res with (env,res) in
    (env, join [
      "{const} ⊆ ", lhs, " ⇒ {stoch} ⊆ ", rhs, " ⇒ {stoch} ⊆ ", res ])
  | CstrExtStochApp { lhs = lhs, rhs = rhs, res = res } ->
    match pprintVarIName graph.im env lhs with (env,lhs) in
    match pprintVarIName graph.im env rhs with (env,rhs) in
    match pprintVarIName graph.im env res with (env,res) in
    (env, join [
      "{ext} ⊆ ", lhs, " ⇒ {stoch} ⊆ ", rhs, " ⇒ {stoch} ⊆ ", res ])

  sem generateStochConstraints im =
  | _ -> []
  -- Stochastic values
  | TmLet ({ ident = ident, body = TmAssume _ } & b) ->
    [ CstrInit { lhs = AVStoch {}, rhs = name2int im b.info ident } ]
  -- The below ensures all constants are tracked as needed for stoch
  -- propagation. In the base CFA fragment in Miking, constant constraints are
  -- only generated from a subset of constants. Here, we need to track _all_
  -- constants. NOTE(dlunde,2022-05-18): There is some duplication here, as
  -- some constants (e.g., constants for sequences) are generated both here and
  -- in the base Miking CFA.
  | TmLet ({ ident = ident, body = TmConst { val = c } } & b) ->
    let ident = name2int im b.info ident in
    let arity = constArity c in
    if eqi arity 0 then []
    else [
      CstrInit {
        lhs = AVConst {
          id = ident, const = c, args = []
        },
        rhs = ident
      }
    ]
  -- Track all externals (similar to constants). If a stochastic value is
  -- supplied as argument to an external, we assume that the result is
  -- stochastic as well.
  | TmExt ({
      tyIdent = tyIdent, inexpr = TmLet { ident = ident, inexpr = inexpr }
    } & b) ->
    let ident = name2int im b.info ident in
    let arity = arityFunType tyIdent in
    if eqi arity 0 then []
    else [
      CstrInit {
        lhs = AVExt {
          ext = ident, arity = arity, args = []
        },
        rhs = ident
      }
    ]

  | TmLet ({ ident = ident, body = TmApp app } & b) ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar r then
      let ident = name2int im b.info ident in
      let lident = name2int im l.info l.ident in
      let rident = name2int im r.info r.ident in
      [
        cstrStochDirect lident ident,
        CstrConstStochApp { lhs = lident, rhs = rident, res = ident},
        CstrExtStochApp { lhs = lident, rhs = rident, res = ident}
      ]
      else errorSingle [infoTm app.rhs] "Not a TmVar in application"
    else errorSingle [infoTm app.lhs] "Not a TmVar in application"

  sem generateStochMatchResConstraints (id: IName) (target: IName) =
  -- Result of match is stochastic if match can fail stochastically
  | pat -> if patFail pat then [cstrStochDirect target id] else []

  -- Ensures all extracted pattern components of a stochastic value are also
  -- stochastic. For example, all elements of a stochastic list are stochastic.
  sem generateStochMatchConstraints im id target =
  | pat ->
    recursive let f = lam acc. lam pat.
      let acc = match pat with PatNamed { ident = PName name }
                             | PatSeqEdge { middle = PName name }
                then cons name acc else acc in
      sfold_Pat_Pat f acc pat
    in
    let pnames = f [] pat in
    foldl (lam acc. lam name.
      cons (cstrStochDirect target (name2int im (infoPat pat) name)) acc
    ) [] pnames

end

lang AlignCFA = PPLCFA + StochCFA

  syn AbsVal =
  | AVUnaligned {}

  sem absValToString graph (env: PprintEnv) =
  | AVUnaligned {} -> (env, "unaligned")

  sem cmpAbsValH =
  | (AVUnaligned _, AVUnaligned _) -> 0

  -- Alignment CFA is custom (should not propagate as part of regular
  -- direct constraints)
  sem isDirect =
  | AVUnaligned _ -> false

  syn Constraint =
  -- {lam x. b} ⊆ lhs ⇒ {unaligned} ⊆ x
  | CstrAlignLamApp { lhs: IName }

  sem initConstraint (graph: CFAGraph) =
  | CstrAlignLamApp r & cstr -> initConstraintName r.lhs graph cstr

  sem propagateConstraint (update: (IName,AbsVal)) (graph: CFAGraph) =
  | CstrAlignLamApp { lhs = lhs } ->
    match update.1 with AVLam { ident = x, body = b } then
      addData graph (AVUnaligned {}) x
    else graph

  sem constraintToString graph (env: PprintEnv) =
  | CstrAlignLamApp { lhs = lhs } ->
    match pprintVarIName graph.im env lhs with (env,lhs) in
    (env, join [ "{lam >x<. >b<} ⊆ ", lhs, " ⇒ {unaligned} ⊆ >x<"])

  sem cstrAlignDirect (lhs: IName) =
  | rhs -> CstrDirectAv {
      lhs = lhs, lhsav = AVUnaligned {}, rhs = rhs, rhsav = AVUnaligned {}
    }

  sem cstrStochAlignDirect (lhs: IName) =
  | rhs -> CstrDirectAv {
      lhs = lhs, lhsav = AVStoch {}, rhs = rhs, rhsav = AVUnaligned {}
    }

  -- For a given expression, returns all variables directly bound in that
  -- expression.
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

  sem generateAlignConstraints im =
  | _ -> []
  | TmLet ({ ident = ident, body = TmLam t } & b) ->
    let tident = name2int im t.info t.ident in
    map (lam n. cstrAlignDirect tident (name2int im b.info n)) (exprNames t.body)
  | TmRecLets ({ bindings = bindings } & rl) ->
    join (map (lam b: RecLetBinding.
      match b.body with TmLam t then
        let tident = name2int im t.info t.ident in
        map (lam n. cstrAlignDirect tident (name2int im rl.info n))
          (exprNames t.body)
      else errorSingle [infoTm b.body] "Not a lambda in recursive let body"
    ) bindings)
  | TmLet ({ ident = ident, body = TmMatch t } & b) ->
    let innerNames = concat (exprNames t.thn) (exprNames t.els) in
    match t.target with TmVar tv then
      let cstrs =
        if patFail t.pat then
          let tvident = name2int im tv.info tv.ident in
          map (lam n. cstrStochAlignDirect tvident (name2int im t.info n))
            innerNames
        else []
      in
      let ident = name2int im b.info ident in
      concat
        (map (lam n. cstrAlignDirect ident (name2int im b.info n)) innerNames)
        cstrs
    else errorSingle [infoTm t.target] "Not a TmVar in match target"
  | TmLet ({ ident = ident, body = TmApp app } & b) ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar _ then
        [
          CstrDirectAvCstrs {
            lhs = name2int im b.info ident, lhsav = AVUnaligned {},
            rhs = [CstrAlignLamApp { lhs = name2int im l.info l.ident }]
          },
          CstrDirectAvCstrs {
            lhs = name2int im b.info l.ident, lhsav = AVStoch {},
            rhs = [CstrAlignLamApp { lhs = name2int im l.info l.ident }]
          }
        ]
      else errorSingle [infoTm app.rhs] "Not a TmVar in application"
    else errorSingle [infoTm app.lhs] "Not a TmVar in application"

end

lang CheckpointCFA = PPLCFA

  syn AbsVal =
  | AVCheckpoint {}

  sem absValToString graph (env: PprintEnv) =
  | AVCheckpoint {} -> (env, "checkpoint")

  sem cmpAbsValH =
  | (AVCheckpoint _, AVCheckpoint _) -> 0

  -- Checkpoint CFA is custom (should not propagate as part of regular
  -- direct constraints)
  sem isDirect =
  | AVCheckpoint _ -> false

  syn Constraint =
  -- {lam x. b} ⊆ lhs ⇒ ({checkpoint} ⊆ x ⇒ {checkpoint} ⊆ res)
  | CstrCheckpointLamApp { lhs: IName, res: IName }
  -- const<id> ⊆ lhs ⇒ ({checkpoint} ⊆ id ⇒ {checkpoint} ⊆ res)
  | CstrCheckpointConstApp { lhs: IName, res: IName }
  -- {lam x. b} ⊆ lhs ⇒ {checkpoint} ⊆ x
  | CstrCheckpointLam { lhs: IName }
  -- const<id> ⊆ lhs ⇒ {checkpoint} ⊆ id
  | CstrCheckpointConst { lhs: IName }
  -- NOTE(dlunde,2022-06-15): We don't actually need to consider externals
  -- here. They are always fully applied, and can never propagate AVCheckpoint
  -- to other functions as a result.

  sem initConstraint (graph: CFAGraph) =
  | CstrCheckpointLamApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrCheckpointConstApp r & cstr -> initConstraintName r.lhs graph cstr
  | CstrCheckpointLam r & cstr -> initConstraintName r.lhs graph cstr
  | CstrCheckpointConst r & cstr -> initConstraintName r.lhs graph cstr

  sem propagateConstraint (update: (IName,AbsVal)) (graph: CFAGraph) =
  | CstrCheckpointLamApp { lhs = lhs, res = res } ->
    match update.1 with AVLam { ident = x } then
      initConstraint graph (cstrCheckpointDirect x res)
    else graph
  | CstrCheckpointConstApp { lhs = lhs, res = res } ->
    match update.1 with AVConst { id = id } then
      initConstraint graph (cstrCheckpointDirect id res)
    else graph
  | CstrCheckpointLam { lhs = lhs } ->
    match update.1 with AVLam { ident = x, body = b } then
      addData graph (AVCheckpoint {}) x
    else graph
  | CstrCheckpointConst { lhs = lhs } ->
    match update.1 with AVConst { id = id } then
      addData graph (AVCheckpoint {}) id
    else graph

  sem constraintToString graph (env: PprintEnv) =
  | CstrCheckpointLamApp { lhs = lhs, res = res } ->
    match pprintVarIName graph.im env lhs with (env,lhs) in
    match pprintVarIName graph.im env res with (env,res) in
    (env, join [ "{lam >x<. >b<} ⊆ ", lhs, " ⇒ {checkpoint} ⊆ >x< ⇒ {checkpoint} ⊆ ", res ])
  | CstrCheckpointConstApp { lhs = lhs, res = res } ->
    match pprintVarIName graph.im env lhs with (env,lhs) in
    match pprintVarIName graph.im env res with (env,res) in
    (env, join [ "{const<>x<>} ⊆ ", lhs, " ⇒ {checkpoint} ⊆ >x< ⇒ {checkpoint} ⊆ ", res ])
  | CstrCheckpointLam { lhs = lhs } ->
    match pprintVarIName graph.im env lhs with (env,lhs) in
    (env, join [ "{lam >x<. >b<} ⊆ ", lhs, " ⇒ {checkpoint} ⊆ >x<" ])
  | CstrCheckpointConst { lhs = lhs } ->
    match pprintVarIName graph.im env lhs with (env,lhs) in
    (env, join [ "const<>id<> ⊆ ", lhs, " ⇒ {checkpoint} ⊆ >id<" ])

  -- {checkpoint} ⊆ lhs ⇒ {checkpoint} ⊆ rhs
  sem cstrCheckpointDirect (lhs: IName) =
  | rhs -> CstrDirectAv {
      lhs = lhs, lhsav = AVCheckpoint {}, rhs = rhs, rhsav = AVCheckpoint {}
    }


  sem checkpoint: Expr -> Bool
  sem checkpoint =
  | _ -> false

  sem generateCheckpointInitConstraints im =
  | _ -> []
  | TmLet ({ ident = ident, body = b } & l) ->
    let ident = name2int im l.info ident in
    if checkpoint b then [ CstrInit { lhs = AVCheckpoint {}, rhs = ident } ]
    else []

  -- For a given expression, returns all variables directly bound in that
  -- expression as a result of applications, matches, or `let`s with bodies
  -- that are checkpoints (e.g., often weight/assume/observe).
  sem exprCheckpointNames: Expr -> [Name]
  sem exprCheckpointNames =
  | t -> exprCheckpointNamesAcc [] t
  sem exprCheckpointNamesAcc: [Name] -> Expr -> [Name]
  sem exprCheckpointNamesAcc acc =
  | TmVar t -> acc
  | TmLet t->
    if checkpoint t.body then
      exprCheckpointNamesAcc (cons t.ident acc) t.inexpr
    else
      exprCheckpointNamesAcc acc t.inexpr
  | TmLet { ident = ident, body = TmApp _ | TmMatch _, inexpr = inexpr} ->
    exprCheckpointNamesAcc (cons ident acc) inexpr
  | TmRecLets t ->
      foldl (lam acc. lam bind : RecLetBinding. cons bind.ident acc)
        acc t.bindings
  | TmType t -> exprCheckpointNamesAcc acc t.inexpr
  | TmConDef t -> exprCheckpointNamesAcc acc t.inexpr
  | TmUtest t -> exprCheckpointNamesAcc acc t.next
  | TmExt t -> exprCheckpointNamesAcc acc t.inexpr
  | t -> errorSingle [infoTm t] "Error in exprCheckpointNames for CFA"


  sem generateCheckpointConstraints im =
  | _ -> []
  | TmLet ({ ident = ident, body = TmLam t } & b) ->
    -- If any expression in the body of the lambda evaluates a checkpoint, the
    -- lambda evaluates a checkpoint
    let tident = name2int im t.info t.ident in
    let ident = name2int im b.info ident in
    let cstrs =
      map (lam lhs. cstrCheckpointDirect (name2int im t.info lhs) tident)
        (exprCheckpointNames t.body)
    in
    -- If the lambda evaluates a checkpoint, the ident is also said to contain
    -- a checkpoint (for symmetry with how constant functions are handled).
    cons (cstrCheckpointDirect tident ident) cstrs
  | TmRecLets ({ bindings = bindings } & rl) ->
    join (map (lam b: RecLetBinding.
      match b.body with TmLam t then
        -- Same as for lambda
        let tident = name2int im t.info t.ident in
        let bident = name2int im rl.info b.ident in
        let cstrs =
          map (lam lhs. cstrCheckpointDirect (name2int im t.info lhs) tident)
            (exprCheckpointNames t.body)
        in
        cons (cstrCheckpointDirect tident bident) cstrs
      else errorSingle [infoTm b.body] "Not a lambda in recursive let body"
    ) bindings)
  | TmLet ({ ident = ident, body = TmMatch t } & b) ->
    -- If any expression in one of the match branches evaluates a checkpoint, the
    -- match itself evaluates a checkpoint
    let ident = name2int im b.info ident in
    let innerNames =
      concat (exprCheckpointNames t.thn) (exprCheckpointNames t.els)
    in
    map (lam lhs. cstrCheckpointDirect (name2int im b.info lhs) ident) innerNames
  | TmLet ({ ident = ident, body = TmApp app } & b) ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar _ then
        let ident = name2int im b.info ident in
        let lident = name2int im l.info l.ident in
        [
          CstrCheckpointLamApp { lhs = lident, res = ident },
          CstrCheckpointConstApp { lhs = lident, res = ident },

          -- {checkpoint} ⊆ res ⇒
          --   ({lam x. b} ⊆ lhs ⇒ {checkpoint} ⊆ x
          --   AND const<id> ⊆ lhs ⇒ {checkpoint} ⊆ id)
          CstrDirectAvCstrs {
            lhs = ident, lhsav = AVCheckpoint (),
            rhs = [
              CstrCheckpointLam { lhs = lident },
              CstrCheckpointConst { lhs = lident }
            ]
          }
        ]
      else errorSingle [infoTm app.rhs] "Not a TmVar in application"
    else errorSingle [infoTm app.lhs] "Not a TmVar in application"

end

lang MExprPPLCFA = StochCFA + AlignCFA + CheckpointCFA
end

let extractUnaligned = use MExprPPLCFA in
  lam cfaRes: CFAGraph.
    tensorFoldi (lam acc: Set Name. lam i: [Int]. lam v: Set AbsVal.
        if setAny (lam av. match av with AVUnaligned _ then true else false) v
        then setInsert (int2name cfaRes.im (head i)) acc
        else acc
      ) (setEmpty nameCmp) cfaRes.data

let extractCheckpoint = use MExprPPLCFA in
  lam cfaRes: CFAGraph.
    tensorFoldi (lam acc: Set Name. lam i: [Int]. lam v: Set AbsVal.
        if setAny (lam av. match av with AVCheckpoint _ then true else false) v
        then setInsert (int2name cfaRes.im (head i)) acc
        else acc
      ) (setEmpty nameCmp) cfaRes.data

lang Test = MExprPPLCFA + MExprANFAll + DPPLParser

  -- Use weight as checkpoint for tests
  sem checkpoint =
  | TmWeight _ -> true

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
    let stringMap: Map String Int = mapFoldWithKey (
        lam acc: Map String Int. lam n: Name. lam i: Int.
          mapInsert (nameGetStr n) i acc
      ) (mapEmpty cmpString) cfaRes.im.name2int
    in
    let string2int: String -> Int = lam s. mapFindExn s stringMap in
    map (lam var: String.
      let avs = dataLookup (string2int var) cfaRes in
      let val = setFold
        (lam acc. lam av. match av with AVStoch _ then true else acc) false avs
      in (var, val)
    ) vars
in

-- Custom equality function for testing
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

-- Stochastic externals
let t = _parse "
  external log : Float -> Float in
  let x = log (assume (Beta 2.0 2.0)) in
  x
------------------------" in
-- printLn (mexprToString (normalizeTerm t));
utest _test false t ["x"] with [
  ("x", true)
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

-- TODO(dlunde,2022-06-15): Add missing tests for recursive lets!

let _test: Bool -> Expr -> [String] -> [([Char], Bool)] =
  lam debug. lam t. lam vars.
    let tANF = normalizeTerm t in
    let env = if debug then Some pprintEnvEmpty else None () in
    match _testBase env tANF with (env, cfaRes) in
    let aRes: Set Name = extractUnaligned cfaRes in
    map (lam var: String. (var, not (setMem (nameNoSym var) aRes))) vars
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
let _testWithSymbolize: Bool -> Expr -> [String] -> [([Char], Bool)] =
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
let t = symbolizeExpr symEnvEmpty
          (parseMCorePPLFile "coreppl/models/crbd/crbd-unaligned.mc") in
utest _testWithSymbolize false t ["w1","w2","w3"] with [
  ("w1", false),
  ("w2", false),
  ("w3", true)
]
using eqTest in

----------------------
-- CHECKPOINT TESTS --
----------------------

let _test: Bool -> Expr -> [String] -> [([Char], Bool)] =
  lam debug. lam t. lam vars.
    let tANF = normalizeTerm t in
    let env = if debug then Some pprintEnvEmpty else None () in
    match _testBase env tANF with (env, cfaRes) in
    let aRes: Set Name = extractCheckpoint cfaRes in
    map (lam var: String. (var, setMem (nameNoSym var) aRes)) vars
in

let t = _parse "
  let a = weight 1.0 in
  let b = assume (Bernoulli 0.5) in
  let c = addi 1 2 in
  c
------------------------" in
utest _test false t ["a","b","c"] with [
  ("a", true),
  ("b", false),
  ("c", false)
] using eqTest in

let t = _parse "
  let f = (lam x.
    let a = weight 1.0 in
    let b = assume (Bernoulli 0.5) in
    let c = addi 1 2 in
    c) in
  let g = (lam y.
    let a2 = assume (Bernoulli 0.5) in
    let b2 = addi 1 2 in
    b2) in
  let d = f 1 in
  let e = g 1 in
  e
------------------------" in
utest _test false t ["f","x","a","b","y","a2","b2","c","d","e"] with [
  ("f", true),
  ("x", true),
  ("a", true),
  ("b", false),
  ("y", false),
  ("a2", false),
  ("b2", false),
  ("c", false),
  ("d", true),
  ("e", false)
] using eqTest in

let t = _parse "
  let f = (lam x. let a = weight 1.0 in a) in
  let c1 = addi in
  let c2 = addi in
  let g = (lam y.
    let b = y 1 in
    b
  ) in
  let c = g f in
  let d = g c1 in
  let e = c1 1 in
  let r = c2 1 in
  r
------------------------" in
utest _test false t ["f","x","a","c1","c2","g","y","b","c","d","e","r"] with [
  ("f", true),
  ("x", true),
  ("a", true),
  ("c1", true),
  ("c2", false),
  ("g", true),
  ("y", true),
  ("b", true),
  ("c", true),
  ("d", true),
  ("e", true),
  ("r", false)
] using eqTest in

let t = _parse "
  let a =
    match 1 with _ then
      match 1 with _ then
        let mb = () in mb
      else ()
    else
      match 1 with _ then
        let ma = weight 1.0 in ma
      else ()
  in
  let b =
    match 1 with _ then
      match 1 with _ then
        let mc = () in mc
      else ()
    else
      match 1 with _ then ()
      else ()
  in
  let c = addi 1 2 in
  c
------------------------" in
utest _test false t ["a","b","c","ma","mb","mc"] with [
  ("a", true),
  ("b", false),
  ("c", false),
  ("ma", true),
  ("mb", false),
  ("mc", false)
] using eqTest in

let t = _parse "
  (lam x.
    let a = x () in
    let b = weight 1.0 in
    b
  )
------------------------" in
utest _test false t ["x","a","b"] with [
  ("x", true),
  ("a", false),
  ("b", true)
] using eqTest in

let t = _parse "
  recursive
    let f1 = lam a. let t = (lam b. let ti = weight 1.0 in b) in t
    let f2 = lam c. c
  in
  let t1 = 1 in
  let t2 = f1 t1 in
  let t3 = 2 in
  let x = t2 t3 in
  let t4 = 3 in
  let y = f2 t4 in
  let t5 = addi in
  let t6 = t5 x in
  let t7 = t6 y in
  (lam x. x) t7
" in
utest _test false t ["f1","t","a","b","c","x","t2"] with [
  ("f1", false),
  ("t", true),
  ("a", false),
  ("b", true),
  ("c", false),
  ("x", true),
  ("t2", false)
] using eqTest in

let t = _parse "
  let c1 = addf in
  let c2 = addf in
  let f1 = lam x1. lam x2. x2 in
  let f2 = lam y1. lam y2. let w = weight 1.0 in y2 in
  let apply1 = lam fa1. let app1 = fa1 1.0 2.0 in app1 in
  let apply2 = lam fa2. let app2 = fa2 1.0 2.0 in app2 in
  let d1 = apply1 c1 in
  let d2 = apply1 f1 in
  let d3 = apply2 c2 in
  apply2 f2
" in
utest _test false t [
  "c1","c2","f1","f2","apply1","apply2",
  "x1","x2","y1","y2","w","fa1","app1","fa2","app2"
] with [
  ("c1", false),
  ("c2", true),
  ("f1", false),
  ("f2", true),
  ("apply1", false),
  ("apply2", true),
  ("x1", false),
  ("x2", false),
  ("y1", true),
  ("y2", true),
  ("w", true),
  ("fa1", false),
  ("app1", false),
  ("fa2", true),
  ("app2", true)
] using eqTest in

()

