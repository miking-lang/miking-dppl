-- Control-flow analysis for CorePPL.

include "coreppl.mc"
include "parser.mc"
include "ad.mc"

include "mexpr/cfa.mc"
include "mexpr/type.mc"
include "mexpr/const-types.mc"

lang ConstAllCFA = MExprCFA + MExprPPL + DualNumLift

  -- The below ensures all constants are tracked as needed for the CorePPL
  -- analyses.  In the base CFA fragment in Miking, constraints for constants
  -- and externals are only generated from a subset of constants/externals.
  -- Here, we need to track _all_ of them.
  --
  -- NOTE(dlunde,2022-05-18): There is some duplication here, as some constants
  -- (e.g., constants for sequences) are generated both here and in the base
  -- Miking CFA. However, they analysis recognizes them as identical and only
  -- uses one copy.
  sem generateConstAllConstraints graph =
  | _ -> graph
  | TmLet ({ ident = ident, body = TmConst { val = const } } & b) ->
    let ident = name2intAcc graph.ia b.info ident in
    let arity = constArity const in
    if eqi arity 0 then graph
    else addNewConst graph ident const
  | TmExt ({
      tyIdent = tyIdent, inexpr = TmLet { ident = ident, inexpr = inexpr }
    } & b) ->
    let ident = name2intAcc graph.ia b.info ident in
    let arity = arityFunType tyIdent in
    if eqi arity 0 then graph
    else
      let cstr = CstrInit {
        lhs = AVExt { ext = ident, arity = arity, args = [] },
        rhs = ident
      } in
      { graph with cstrs = cons cstr graph.cstrs}

  -- NOTE(dlunde,2022-10-21): We do not support these CorePPL constants in PPL
  -- models/code. They are only used outside of inference to extract inference
  -- data.
  sem generateConstraintsConst graph info ident =
  | ( CDistEmpiricalSamples _
    | CDistEmpiricalDegenerate _
    | CDistEmpiricalNormConst _
    | CDistEmpiricalAcceptRate _ ) ->
    errorSingle [info] "Constant not supported in CorePPL CFA"
  | CDistExpectation _ -> graph

  sem addConstAllConstraints (graph: CFAGraphInit) =
  | t ->
    let cgfs = [ generateConstAllConstraints ] in
    let graph = collectConstraints cgfs graph t in
    graph

  -- Ensure new abstract constants added by generateConstAllConstraints do not
  -- cause errors in the default propagateConstraintConst
  sem propagateConstraintConst res args intermediates =
  | _ -> []

  -- Given a function that adds constraints for an application, apply it
  -- correctly within all higher-order intrinsics.
  sem propagateConstraintHigherOrderConst appConstraints res args intermediates =
  | CFoldl _ ->
    utest length args with 3 in
    let seq = get args 2 in
    let f = head args in
    let acc = get args 1 in
    match intermediates with [li,a1,a2,a3] in
    join [
      appConstraints f acc a1,
      appConstraints a1 li a2,
      appConstraints f a2 a3,
      appConstraints a3 li res,
      -- All internal applications can affect res as well
      appConstraints f acc res,
      appConstraints a1 li res,
      appConstraints f a2 res
    ]
  | CFoldr _ ->
    utest length args with 3 in
    let seq = get args 2 in
    let f = head args in
    let acc = get args 1 in
    match intermediates with [li,a1,a2] in
    join [
      appConstraints f li a1,
      appConstraints a1 acc a2,
      appConstraints a1 a2 res,
      -- All internal applications can affect res as well
      appConstraints f li res,
      appConstraints a1 acc res
    ]
  | CMap _ ->
    utest length args with 2 in
    let seq = get args 1 in
    let f = head args in
    match intermediates with [li,a1] in
    join [
      appConstraints f li a1,
      -- All internal applications can affect res as well
      appConstraints f li res
    ]
  | CMapi _ ->
    utest length args with 2 in
    let seq = get args 1 in
    let f = head args in
    match intermediates with [li,i,a1,a2] in
    join [
      appConstraints f i a1,
      appConstraints a1 li a2,
      -- All internal applications can affect res as well
      appConstraints f i res,
      appConstraints a1 li res
    ]
  | CIter _ ->
    utest length args with 2 in
    let seq = get args 1 in
    let f = head args in
    match intermediates with [li,empty] in
    join [
      appConstraints f li empty,
      -- All internal applications can affect res as well
      appConstraints f li res
    ]
  | CIteri _ ->
    utest length args with 2 in
    let seq = get args 1 in
    let f = head args in
    match intermediates with [li,i,a1,empty] in
    join [
      appConstraints f i a1,
      appConstraints a1 li empty,
      -- All internal applications can affect res as well
      appConstraints f i res,
      appConstraints a1 li res
    ]
  | ( CCreate _
    | CCreateList _
    | CCreateRope _
    ) ->
    utest length args with 2 in
    let f = get args 1 in
    match intermediates with [i,a1] in
    join [
      appConstraints f i a1,
      -- All internal applications can affect res as well
      appConstraints f i res
    ]
  | ( CTensorCreateInt _
    | CTensorCreateFloat _
    | CTensorCreate _ ) ->
    utest length args with 2 in
    let f = get args 1 in
    match intermediates with [i,a1] in
    join [
      appConstraints f i a1,
      -- All internal applications can affect res as well
      appConstraints f i res
    ]
  | CTensorIterSlice _ ->
    utest length args with 2 in
    let f = get args 0 in
    let tensor = get args 1 in
    match intermediates with [ti,i,a1,empty] in
    join [
      appConstraints f i a1,
      appConstraints a1 ti empty,
      -- All internal applications can affect res as well
      appConstraints f i res,
      appConstraints a1 ti res
    ]
  | _ -> []

end

lang StochCFA = MExprCFA + MExprPPL + ConstAllCFA

  syn AbsVal =
  | AVStoch {}

  sem absValToString im (env: PprintEnv) =
  | AVStoch {} -> (env, "stoch")

  sem cmpAbsValH =
  | (AVStoch _, AVStoch _) -> 0

  syn Constraint =
  -- {const} ⊆ lhs ⇒ ({stoch} ⊆ rhs ⇒ {stoch} ⊆ res)
  | CstrConstStochApp { lhs: IName, rhs: IName, res: IName }
  -- {ext} ⊆ lhs ⇒ ({stoch} ⊆ rhs ⇒ {stoch} ⊆ res)
  | CstrExtStochApp { lhs: IName, rhs: IName, res: IName }

  sem cmpConstraintH =
  | (CstrConstStochApp { lhs = lhs1, rhs = rhs1, res = res1 },
    CstrConstStochApp { lhs = lhs2, rhs = rhs2, res = res2 })
  | (CstrExtStochApp { lhs = lhs1, rhs = rhs1, res = res1 },
    CstrExtStochApp { lhs = lhs2, rhs = rhs2, res = res2 }) ->
    let d = subi lhs1 lhs2 in
    if eqi d 0 then
      let d = subi rhs1 rhs2 in
      if eqi d 0 then
        subi res1 res2
      else d
    else d

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

  sem constraintToString im (env: PprintEnv) =
  | CstrConstStochApp { lhs = lhs, rhs = rhs, res = res } ->
    match pprintVarIName im env lhs with (env,lhs) in
    match pprintVarIName im env rhs with (env,rhs) in
    match pprintVarIName im env res with (env,res) in
    (env, join [
      "{const} ⊆ ", lhs, " ⇒ {stoch} ⊆ ", rhs, " ⇒ {stoch} ⊆ ", res ])
  | CstrExtStochApp { lhs = lhs, rhs = rhs, res = res } ->
    match pprintVarIName im env lhs with (env,lhs) in
    match pprintVarIName im env rhs with (env,rhs) in
    match pprintVarIName im env res with (env,res) in
    (env, join [
      "{ext} ⊆ ", lhs, " ⇒ {stoch} ⊆ ", rhs, " ⇒ {stoch} ⊆ ", res ])

  sem appStochConstraints lhs rhs =
  | res -> [
      cstrStochDirect lhs res,
      CstrConstStochApp { lhs = lhs, rhs = rhs, res = res},
      CstrExtStochApp { lhs = lhs, rhs = rhs, res = res}
    ]

  sem generateStochConstraints graph =
  | _ -> graph
  -- Stochastic values
  | TmLet ({ ident = ident, body = TmAssume _ } & b) ->
    let cstr = CstrInit { lhs = AVStoch {}, rhs = name2intAcc graph.ia b.info ident } in
    { graph with cstrs = cons cstr graph.cstrs }

  | TmLet ({ ident = ident, body = TmApp app } & b) ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar r then
        let ident = name2intAcc graph.ia b.info ident in
        let lident = name2intAcc graph.ia l.info l.ident in
        let rident = name2intAcc graph.ia r.info r.ident in
        let cstrs = appStochConstraints lident rident ident in
        { graph with cstrs = concat cstrs graph.cstrs }
      else errorSingle [infoTm app.rhs] "Not a TmVar in application"
    else errorSingle [infoTm app.lhs] "Not a TmVar in application"

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


  sem generateStochMatchResConstraints (id: IName) (target: IName) =
  -- Result of match is stochastic if match can fail stochastically
  | pat -> if patFail pat then [cstrStochDirect target id] else []

  -- Ensures all extracted pattern components of a stochastic value are also
  -- stochastic. For example, all elements of a stochastic list are stochastic.
  sem generateStochMatchConstraints ia id target =
  | pat ->
    recursive let f = lam acc. lam pat.
      let acc = match pat with PatNamed { ident = PName name }
                             | PatSeqEdge { middle = PName name }
                then cons name acc else acc in
      sfold_Pat_Pat f acc pat
    in
    let pnames = f [] pat in
    foldl (lam acc. lam name.
      cons (cstrStochDirect target (name2intAcc ia (infoPat pat) name)) acc
    ) [] pnames

  sem addStochMatchConstraints =
  | graph ->
    { graph with mcgfs = concat [
      generateStochMatchResConstraints,
      generateStochMatchConstraints graph.ia
    ] graph.mcgfs }

  sem addStochConstraints (graph: CFAGraphInit) =
  | t ->
    let propagateStochConstraintConst =
      propagateConstraintHigherOrderConst appStochConstraints in
    let graph =
      {graph with cpfs = cons propagateStochConstraintConst graph.cpfs} in
    let cgfs: [GenFun] = [ generateStochConstraints ] in
    let graph = collectConstraints cgfs graph t in
    graph

  -- Standalone stochastic CFA
  sem stochCfa : Expr -> CFAGraph
  sem stochCfa =
  | t ->
    let graph = emptyCFAGraphInit t in
    let graph = addBaseMatchConstraints graph in
    let graph = addStochMatchConstraints graph in
    let graph = addBaseConstraints graph t in
    let graph = addStochConstraints graph t in
    let graph = addConstAllConstraints graph t in
    solveCfa graph

  sem stochCfaDebug : PprintEnv -> Expr -> (PprintEnv, CFAGraph)
  sem stochCfaDebug pprintenv =
  | t ->
    let graph = emptyCFAGraphInit t in
    let graph = addBaseMatchConstraints graph in
    let graph = addStochMatchConstraints graph in
    let graph = addBaseConstraints graph t in
    let graph = addStochConstraints graph t in
    let graph = addConstAllConstraints graph t in
    solveCfaDebug pprintenv graph

end

lang AlignCFA = MExprCFA + MExprPPL + StochCFA + ConstAllCFA

  syn AbsVal =
  | AVUnaligned {}

  sem absValToString im (env: PprintEnv) =
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

  sem cmpConstraintH =
  | (CstrAlignLamApp { lhs = lhs1 }, CstrAlignLamApp { lhs = lhs2 }) ->
    subi lhs1 lhs2

  sem initConstraint (graph: CFAGraph) =
  | CstrAlignLamApp r & cstr -> initConstraintName r.lhs graph cstr

  sem propagateConstraint (update: (IName,AbsVal)) (graph: CFAGraph) =
  | CstrAlignLamApp { lhs = lhs } ->
    match update.1 with AVLam { ident = x, body = b } then
      addData graph (AVUnaligned {}) x
    else graph

  sem constraintToString im (env: PprintEnv) =
  | CstrAlignLamApp { lhs = lhs } ->
    match pprintVarIName im env lhs with (env,lhs) in
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
  sem exprUnalignedNames: Expr -> [Name]
  sem exprUnalignedNames =
  | t -> exprUnalignedNamesAcc [] t
  sem exprUnalignedNamesAcc: [Name] -> Expr -> [Name]
  sem exprUnalignedNamesAcc acc =
  | TmVar t -> acc
  | TmLet t -> exprUnalignedNamesAcc (cons t.ident acc) t.inexpr
  | TmRecLets t ->
      exprUnalignedNamesAcc
        (foldl (lam acc. lam bind : RecLetBinding. cons bind.ident acc)
          acc t.bindings)
        t.inexpr
  | TmType t -> exprUnalignedNamesAcc acc t.inexpr
  | TmConDef t -> exprUnalignedNamesAcc acc t.inexpr
  | TmUtest t -> exprUnalignedNamesAcc acc t.next
  | TmExt t -> exprUnalignedNamesAcc acc t.inexpr
  | t -> errorSingle [infoTm t] "Error in exprUnalignedNames for CFA"

  sem appAlignConstraints lhs =
  | res -> [
      CstrDirectAvCstrs {
        lhs = res, lhsav = AVUnaligned {},
        rhs = [CstrAlignLamApp { lhs = lhs }]
      },
      CstrDirectAvCstrs {
        lhs = lhs, lhsav = AVStoch {},
        rhs = [CstrAlignLamApp { lhs = lhs }]
      }
    ]

  sem generateAlignConstraints graph =
  | _ -> graph
  | TmLet ({ ident = ident, body = TmLam t } & b) ->
    let tident = name2intAcc graph.ia t.info t.ident in
    let cstrs =
      map (lam n. cstrAlignDirect tident (name2intAcc graph.ia b.info n))
        (exprUnalignedNames t.body)
    in
    { graph with cstrs = concat cstrs graph.cstrs }
  | TmRecLets ({ bindings = bindings } & rl) ->
    let cstrs =
      join (map (lam b: RecLetBinding.
        match b.body with TmLam t then
          let tident = name2intAcc graph.ia t.info t.ident in
          map (lam n. cstrAlignDirect tident (name2intAcc graph.ia rl.info n))
            (exprUnalignedNames t.body)
        else errorSingle [infoTm b.body] "Not a lambda in recursive let body"
      ) bindings)
    in
    { graph with cstrs = concat cstrs graph.cstrs }
  | TmLet ({ ident = ident, body = TmMatch t } & b) ->
    let innerNames = concat (exprUnalignedNames t.thn) (exprUnalignedNames t.els) in
    match t.target with TmVar tv then
      let cstrs =
        if patFail t.pat then
          let tvident = name2intAcc graph.ia tv.info tv.ident in
          map (lam n. cstrStochAlignDirect tvident (name2intAcc graph.ia t.info n))
            innerNames
        else []
      in
      let ident = name2intAcc graph.ia b.info ident in
      let cstrs =
        concat
          (map (lam n. cstrAlignDirect ident (name2intAcc graph.ia b.info n)) innerNames)
          cstrs
      in
      { graph with cstrs = concat cstrs graph.cstrs }
    else errorSingle [infoTm t.target] "Not a TmVar in match target"
  | TmLet ({ ident = ident, body = TmApp app } & b) ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar _ then
        let res = name2intAcc graph.ia b.info ident in
        let lhs = name2intAcc graph.ia l.info l.ident in
        let cstrs = appAlignConstraints lhs res in
        { graph with cstrs = concat cstrs graph.cstrs }
      else errorSingle [infoTm app.rhs] "Not a TmVar in application"
    else errorSingle [infoTm app.lhs] "Not a TmVar in application"

  sem addAlignConstraints (graph: CFAGraphInit) =
  | t ->
    let propagateAlignConstraintConst =
      propagateConstraintHigherOrderConst
        (lam lhs. lam. appAlignConstraints lhs) in
    let graph =
      {graph with cpfs = cons propagateAlignConstraintConst graph.cpfs} in
    let cgfs: [GenFun] = [ generateAlignConstraints ] in
    let graph = collectConstraints cgfs graph t in
    graph

  -- Standalone alignment CFA (includes stochastic analysis)
  sem alignCfa : Expr -> CFAGraph
  sem alignCfa =
  | t ->
    let graph = emptyCFAGraphInit t in
    let graph = addBaseMatchConstraints graph in
    let graph = addStochMatchConstraints graph in
    let graph = addBaseConstraints graph t in
    let graph = addStochConstraints graph t in
    let graph = addAlignConstraints graph t in
    let graph = addConstAllConstraints graph t in
    solveCfa graph

  sem alignCfaDebug : PprintEnv -> Expr -> (PprintEnv, CFAGraph)
  sem alignCfaDebug pprintenv =
  | t ->
    let graph = emptyCFAGraphInit t in
    let graph = addBaseMatchConstraints graph in
    let graph = addStochMatchConstraints graph in
    let graph = addBaseConstraints graph t in
    let graph = addStochConstraints graph t in
    let graph = addAlignConstraints graph t in
    let graph = addConstAllConstraints graph t in
    solveCfaDebug pprintenv graph

  sem extractUnaligned =
  | graph ->
    tensorFoldi (lam acc: Set Name. lam i: [Int]. lam v: Set AbsVal.
        if setAny (lam av. match av with AVUnaligned _ then true else false) v
        then setInsert (int2name graph.im (head i)) acc
        else acc
      ) (setEmpty nameCmp) graph.data

end

lang CheckpointCFA = MExprCFA + MExprPPL + ConstAllCFA

  syn AbsVal =
  | AVCheckpoint {}

  sem absValToString im (env: PprintEnv) =
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

  sem cmpConstraintH =
  | (CstrCheckpointLamApp { lhs = lhs1, res = res1 },
    CstrCheckpointLamApp { lhs = lhs2,  res = res2 })
  | (CstrCheckpointConstApp { lhs = lhs1, res = res1 },
    CstrCheckpointConstApp { lhs = lhs2,  res = res2 }) ->
    let d = subi lhs1 lhs2 in
    if eqi d 0 then subi res1 res2 else d
  | (CstrCheckpointLam { lhs = lhs1 }, CstrCheckpointLam { lhs = lhs2 })
  | (CstrCheckpointConst { lhs = lhs1 }, CstrCheckpointConst { lhs = lhs2 }) ->
    subi lhs1 lhs2

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

  sem constraintToString im (env: PprintEnv) =
  | CstrCheckpointLamApp { lhs = lhs, res = res } ->
    match pprintVarIName im env lhs with (env,lhs) in
    match pprintVarIName im env res with (env,res) in
    (env, join [ "{lam >x<. >b<} ⊆ ", lhs, " ⇒ {checkpoint} ⊆ >x< ⇒ {checkpoint} ⊆ ", res ])
  | CstrCheckpointConstApp { lhs = lhs, res = res } ->
    match pprintVarIName im env lhs with (env,lhs) in
    match pprintVarIName im env res with (env,res) in
    (env, join [ "{const<>x<>} ⊆ ", lhs, " ⇒ {checkpoint} ⊆ >x< ⇒ {checkpoint} ⊆ ", res ])
  | CstrCheckpointLam { lhs = lhs } ->
    match pprintVarIName im env lhs with (env,lhs) in
    (env, join [ "{lam >x<. >b<} ⊆ ", lhs, " ⇒ {checkpoint} ⊆ >x<" ])
  | CstrCheckpointConst { lhs = lhs } ->
    match pprintVarIName im env lhs with (env,lhs) in
    (env, join [ "const<>id<> ⊆ ", lhs, " ⇒ {checkpoint} ⊆ >id<" ])

  -- {checkpoint} ⊆ lhs ⇒ {checkpoint} ⊆ rhs
  sem cstrCheckpointDirect (lhs: IName) =
  | rhs -> CstrDirectAv {
      lhs = lhs, lhsav = AVCheckpoint {}, rhs = rhs, rhsav = AVCheckpoint {}
    }

  sem generateCheckpointInitConstraints:
    (Expr -> Bool) -> CFAGraphInit -> Expr -> CFAGraphInit
  sem generateCheckpointInitConstraints checkpoint graph =
  | _ -> graph
  | TmLet ({ ident = ident, body = b } & l) & t ->
    let ident = name2intAcc graph.ia l.info ident in
    if checkpoint t then
      let cstr = CstrInit { lhs = AVCheckpoint {}, rhs = ident } in
      { graph with cstrs = cons cstr graph.cstrs }
    else graph

  -- For a given expression, returns all variables directly bound in that
  -- expression as a result of applications, matches, or `let`s with bodies
  -- that are checkpoints (e.g., often weight/assume/observe).
  sem exprCheckpointNames: (Expr -> Bool) -> Expr -> [Name]
  sem exprCheckpointNames checkpoint =
  | t -> exprCheckpointNamesAcc checkpoint [] t
  sem exprCheckpointNamesAcc: (Expr -> Bool) -> [Name] -> Expr -> [Name]
  sem exprCheckpointNamesAcc checkpoint acc =
  | TmVar t -> acc
  | TmLet t & tm ->
    if checkpoint tm then
      exprCheckpointNamesAcc checkpoint (cons t.ident acc) t.inexpr
    else
      exprCheckpointNamesAcc checkpoint acc t.inexpr
  | TmLet { ident = ident, body = TmApp _ | TmMatch _, inexpr = inexpr} ->
    exprCheckpointNamesAcc checkpoint (cons ident acc) inexpr
  | TmRecLets t ->
    exprCheckpointNamesAcc checkpoint
      (foldl (lam acc. lam bind : RecLetBinding. cons bind.ident acc)
        acc t.bindings)
      t.inexpr
  | TmType t -> exprCheckpointNamesAcc checkpoint acc t.inexpr
  | TmConDef t -> exprCheckpointNamesAcc checkpoint acc t.inexpr
  | TmUtest t -> exprCheckpointNamesAcc checkpoint acc t.next
  | TmExt t -> exprCheckpointNamesAcc checkpoint acc t.inexpr
  | t -> errorSingle [infoTm t] "Error in exprCheckpointNames for CFA"

  sem appCheckpointConstraints lhs =
  | res -> [
      CstrCheckpointLamApp { lhs = lhs, res = res },
      CstrCheckpointConstApp { lhs = lhs, res = res },

      -- {checkpoint} ⊆ res ⇒
      --   ({lam x. b} ⊆ lhs ⇒ {checkpoint} ⊆ x
      --   AND const<id> ⊆ lhs ⇒ {checkpoint} ⊆ id)
      CstrDirectAvCstrs {
        lhs = res, lhsav = AVCheckpoint (),
        rhs = [
          CstrCheckpointLam { lhs = lhs },
          CstrCheckpointConst { lhs = lhs }
        ]
      }
    ]

  sem generateCheckpointConstraints :
    (Expr -> Bool) -> CFAGraphInit -> Expr -> CFAGraphInit
  sem generateCheckpointConstraints checkpoint graph =
  | _ -> graph
  | TmLet ({ body = TmLam t } & b) ->
    -- If certain expressions in the body of the lambda evaluates a checkpoint, the
    -- lambda evaluates a checkpoint
    let tident = name2intAcc graph.ia t.info t.ident in
    let cstrs =
      map (lam lhs. cstrCheckpointDirect (name2intAcc graph.ia t.info lhs) tident)
        (exprCheckpointNames checkpoint t.body)
    in
    { graph with cstrs = concat cstrs graph.cstrs }
  | TmRecLets ({ bindings = bindings } & rl) ->
    let cstrs =
      join (map (lam b: RecLetBinding.
        match b.body with TmLam t then
          -- Same as for lambda
          let tident = name2intAcc graph.ia t.info t.ident in
          map (lam lhs. cstrCheckpointDirect (name2intAcc graph.ia t.info lhs) tident)
            (exprCheckpointNames checkpoint t.body)
        else errorSingle [infoTm b.body] "Not a lambda in recursive let body"
      ) bindings)
    in
    { graph with cstrs = concat cstrs graph.cstrs }
  | TmLet ({ ident = ident, body = TmMatch t } & b) ->
    -- If any expression in one of the match branches evaluates a checkpoint, the
    -- match itself evaluates a checkpoint
    let ident = name2intAcc graph.ia b.info ident in
    let innerNames =
      concat (exprCheckpointNames checkpoint t.thn)
        (exprCheckpointNames checkpoint t.els)
    in
    let cstrs =
      map (lam lhs. cstrCheckpointDirect (name2intAcc graph.ia b.info lhs) ident)
        innerNames
    in
    { graph with cstrs = concat cstrs graph.cstrs }
  | TmLet ({ ident = ident, body = TmApp app } & b) ->
    match app.lhs with TmVar l then
      match app.rhs with TmVar _ then
        let ident = name2intAcc graph.ia b.info ident in
        let lident = name2intAcc graph.ia l.info l.ident in
        let cstrs = appCheckpointConstraints lident ident in
        { graph with cstrs = concat cstrs graph.cstrs }
      else errorSingle [infoTm app.rhs] "Not a TmVar in application"
    else errorSingle [infoTm app.lhs] "Not a TmVar in application"

  sem addCheckpointConstraints (checkpoint: Expr -> Bool) (graph: CFAGraphInit) =
  | t ->
    let propagateCheckpointConstraintConst =
      propagateConstraintHigherOrderConst
        (lam lhs. lam rhs. appCheckpointConstraints lhs) in
    let graph =
      {graph with cpfs = cons propagateCheckpointConstraintConst graph.cpfs} in
    let cgfs: [GenFun] = [
      generateCheckpointInitConstraints checkpoint,
      generateCheckpointConstraints checkpoint
    ] in
    let graph = collectConstraints cgfs graph t in
    graph

  -- Standalone checkpoint/suspension CFA
  sem checkpointCfa : (Expr -> Bool) -> Expr -> CFAGraph
  sem checkpointCfa checkpoint =
  | t ->
    let graph = emptyCFAGraphInit t in
    let graph = addBaseMatchConstraints graph in
    let graph = addBaseConstraints graph t in
    let graph = addCheckpointConstraints checkpoint graph t in
    let graph = addConstAllConstraints graph t in
    solveCfa graph

  sem checkpointCfaDebug : (Expr -> Bool) -> PprintEnv -> Expr
                             -> (PprintEnv, CFAGraph)
  sem checkpointCfaDebug checkpoint pprintenv =
  | t ->
    let graph = emptyCFAGraphInit t in
    let graph = addBaseMatchConstraints graph in
    let graph = addBaseConstraints graph t in
    let graph = addCheckpointConstraints checkpoint graph t in
    let graph = addConstAllConstraints graph t in
    solveCfaDebug pprintenv graph

  sem extractCheckpoint =
  | graph ->
    tensorFoldi (lam acc: Set Name. lam i: [Int]. lam v: Set AbsVal.
        if setAny (lam av. match av with AVCheckpoint _ then true else false) v
        then setInsert (int2name graph.im (head i)) acc
        else acc
      ) (setEmpty nameCmp) graph.data

end

-- All CorePPL analyses
lang MExprPPLCFA = StochCFA + AlignCFA + CheckpointCFA
end

lang Test = MExprPPLCFA + MExprANFAll + DPPLParser
end

-----------
-- TESTS --
-----------

mexpr
use Test in

-- Test functions --
let _parse = parseMExprPPLString in
let _testBase:
  (PprintEnv -> Expr -> (PprintEnv, CFAGraph))
  -> (Expr -> CFAGraph)
  -> Option PprintEnv -> Expr
  -> (Option PprintEnv, CFAGraph) =
    lam cfaDebug.
    lam cfa.
    lam env: Option PprintEnv.
    lam t: Expr.
      match env with Some env then
        -- Version with debug printouts
        let tANF = normalizeTerm t in
        match pprintCode 0 env t with (env,tStr) in
        printLn "\n--- ORIGINAL PROGRAM ---";
        printLn tStr;
        match pprintCode 0 env tANF with (env,tANFStr) in
        printLn "\n--- ANF ---";
        printLn tANFStr;
        match cfaDebug env tANF with (env,cfaRes) in
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
    let cfaDebug = stochCfaDebug in
    let cfa = stochCfa in
    match _testBase cfaDebug cfa env t with (_, cfaRes) in
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

-- Stochastic values and higher-order constants
let t = _parse "
  let f = lam p. assume (Bernoulli p) in
  let ls = [0.1,0.2,0.3] in
  let t1 = map f ls in
  let t2 = get t1 0 in
  let t3 = foldl (lam acc1. lam e1.
                    if ltf e1 0.2 then assume (Bernoulli e1)
                    else false) 0. ls
  in
  let t4 = tensorCreateDense [3,3] (lam. assume (Bernoulli 0.5)) in
  let t5 = tensorGetExn t4 [0,0] in
  let t6 = if assume (Bernoulli p) then (lam x. 1.0) else (lam y. 2.0) in
  let t7 = foldl (lam acc2. t6) 0. ls in
  let t8 = tensorCreateCArrayInt [3,3] (lam. assume (Poisson 0.5)) in
  let t9 = tensorGetExn t8 [0,0] in
  let t10 = tensorCreateCArrayFloat [3,3] (lam. assume (Exponential 0.5)) in
  let t11 = tensorGetExn t10 [0,0] in
  let t12 = tensorIterSlice (lam. lam e. let t13 = e in ()) t10 in
  let t13 = foldr (lam e2. lam acc2.
                    if ltf e2 0.2 then assume (Bernoulli e2)
                    else false) 0. ls
  in
  let t14 = head (head (map (mapi (lam. f)) [ls,ls,ls])) in
  let t15 = head (head (map (map (lam. 1)) [ls,ls,ls])) in
  let dc1 = iter (lam t16. 1) [f 0.1] in
  let dc2 = iteri (lam. lam t17. 1) [0.1] in
  let t18 = head (create 1 f) in
  let t19 = head (createList 2 (lam. 1)) in
  let t20 = head (createRope 3 f) in
  ()
------------------------" in
utest _test false t ["p","ls","t1","t2","t3","t4","t5",
                     "t6","t7","t8","t9","t10","t11","t12","t13",
                     "t14","t15","t16","t17","t18","t19","t20"]
with [
  ("p",  false),
  ("ls", false),
  ("t1", false),
  ("t2", true),
  ("t3", true),
  ("t4", false),
  ("t5", true),
  ("t6", true),
  ("t7", true),
  ("t8", false),
  ("t9", true),
  ("t10", false),
  ("t11", true),
  ("t12", false),
  ("t13", true),
  ("t14", true),
  ("t15", false),
  ("t16", true),
  ("t17", false),
  ("t18", true),
  ("t19", false),
  ("t20", true)
] using eqTest in

---------------------
-- ALIGNMENT TESTS --
---------------------

let _test: Bool -> Expr -> [String] -> [([Char], Bool)] =
  lam debug. lam t. lam vars.
    let tANF = normalizeTerm t in
    let env = if debug then Some pprintEnvEmpty else None () in
    let cfaDebug = alignCfaDebug in
    let cfa = alignCfa in
    match _testBase cfaDebug cfa env tANF with (env, cfaRes) in
    let aRes: Set Name = extractUnaligned cfaRes in
    map (lam var: String. (var, not (setMem (nameNoSym var) aRes))) vars
in

let _testSymbolized: Bool -> Expr -> [String] -> [([Char], Bool)] =
  lam debug. lam t. lam vars.
    let tANF = normalizeTerm t in
    let env = if debug then Some pprintEnvEmpty else None () in
    let cfaDebug = alignCfaDebug in
    let cfa = alignCfa in
    match _testBase cfaDebug cfa env tANF with (env, cfaRes) in
    let aRes: Set Name = extractUnaligned cfaRes in
    let sSet: Set String = setFold
      (lam acc. lam n. setInsert (nameGetStr n) acc)
      (setEmpty cmpString) aRes in
    map (lam var: String. (var, not (setMem var sSet))) vars
in

let _testWithSymbolize: Bool -> Expr -> [String] -> [([Char], Bool)] =
  lam debug. lam t. lam vars.
  let t = symbolizeExpr symEnvEmpty t in
  _testSymbolized debug t vars
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

-- Alignment and higher-order constants
let t = _parse "
  let fs = lam f.
    if assume (Bernoulli 0.5) then (lam a1. f (); a1)
                              else (lam a2. a2)
  in
  let fu = lam g.
    if assume (Bernoulli 0.5) then let x = g () in ()
    else ()
  in
  let ls = [lam x1. x1, lam x2. x2, lam x3. x3] in
  foldl (lam. fs (lam. let s1 = () in ())) 0 ls;
  fu (lam. foldl (lam. lam. let s2 = () in ()) 0 ls);
  foldl (lam. lam. let s3 = () in ()) 0 ls;
  foldr (lam. fs (lam. let s4 = () in ())) 0 ls;
  fu (lam. foldr (lam. lam. let s5 = () in ()) 0 ls);
  foldr (lam. lam. let s6 = () in ()) 0 ls;
  map (fs (lam. let s7 = () in ())) ls;
  fu (lam. map (lam. let s8 = () in ()) ls);
  map (lam. let s9 = () in ()) ls;
  mapi (lam. fs (lam. let s10 = () in ())) ls;
  fu (lam. mapi (lam. lam.let s11 = () in ()) ls);
  mapi (lam. lam. let s12 = () in ()) ls;
  iter (fs (lam. let s13 = () in ())) ls;
  fu (lam. iter (lam. let s14 = () in ()) ls);
  iter (lam. let s15 = () in ()) ls;
  iteri (lam. fs (lam. let s16 = () in ())) ls;
  fu (lam. iteri (lam. lam.let s17 = () in ()) ls);
  iteri (lam. lam. let s18 = () in ()) ls;
  create 3 (fs (lam. let s19 = () in ()));
  fu (lam. create 3 (lam. let s20 = () in ()));
  create 3 (lam. let s21 = () in ());
  createList 3 (fs (lam. let s22 = () in ()));
  fu (lam. createList 3 (lam. let s23 = () in ()));
  createList 3 (lam. let s24 = () in ());
  createRope 3 (fs (lam. let s25 = () in ()));
  fu (lam. createRope 3 (lam. let s26 = () in ()));
  createRope 3 (lam. let s27 = () in ());
  tensorCreateDense [3,3] (fs (lam. let s28 = () in ()));
  fu (lam. tensorCreateDense [3,3] (lam. let s29 = () in ()));
  tensorCreateDense [3,3] (lam. let s30 = () in ());
  tensorCreateCArrayInt [3,3] (fs (lam. let s31 = () in 1));
  fu (lam. tensorCreateCArrayInt [3,3] (lam. let s32 = () in 1));
  tensorCreateCArrayInt [3,3] (lam. let s33 = () in 1);
  tensorCreateCArrayFloat [3,3] (fs (lam. let s34 = () in 1.));
  fu (lam. tensorCreateCArrayFloat [3,3] (lam. let s35 = () in 1.));
  let tensor = tensorCreateCArrayFloat [3,3] (lam. let s36 = () in 1.) in
  tensorIterSlice (lam. fs (lam. let s37 = () in ())) tensor;
  fu (lam. tensorIterSlice (lam. lam. let s38 = () in ()) tensor);
  tensorIterSlice (lam. lam. let s39 = () in ()) tensor;
  ()
------------------------" in
utest _testWithSymbolize false t [
  "s1","s2","s3","s4","s5","s6","s7","s8","s9", "s10",
  "s11","s12","s13","s14","s15","s16","s17","s18","s19","s20",
  "s21","s22","s23","s24","s25","s26","s27","s28","s29","s30",
  "s31","s32","s33","s34","s35","s36","s37","s38","s39"
] with [
  ("s1", false), ("s2", false), ("s3", true),
  ("s4", false), ("s5", false), ("s6", true),
  ("s7", false), ("s8", false), ("s9", true),
  ("s10", false), ("s11", false), ("s12", true),
  ("s13", false), ("s14", false), ("s15", true),
  ("s16", false), ("s17", false), ("s18", true),
  ("s19", false), ("s20", false), ("s21", true),
  ("s22", false), ("s23", false), ("s24", true),
  ("s25", false), ("s26", false), ("s27", true),
  ("s28", false), ("s29", false), ("s30", true),
  ("s31", false), ("s32", false), ("s33", true),
  ("s34", false), ("s35", false), ("s36", true),
  ("s37", false), ("s38", false), ("s39", true)
] using eqTest in

-- Test in `coreppl/models/diversification-models/crbd-synthetic.mc`
let t = parseMCorePPLFile false
          "coreppl/models/diversification-models/crbd-synthetic.mc" in
utest _testSymbolized false t ["w1","w2","w3", "w4", "w5"] with [
  ("w1", false),
  ("w2", false),
  ("w3", true),
  ("w4", true),
  ("w5", true)
]
using eqTest in

----------------------
-- CHECKPOINT TESTS --
----------------------

-- Custom checkpoint function determining source expressions that introduce
-- checkpoints: weights that are _not_ labeled by the variable "nocheck".
let checkpoint: Expr -> Bool = lam t.
  match t with TmLet { ident = ident, body = body } then
    match body with TmWeight _ then
      not (eqString (nameGetStr ident) "nocheck")
    else false
  else errorSingle [infoTm t] "Impossible"
in

let _test: Bool -> Expr -> [String] -> [([Char], Bool)] =
  lam debug. lam t. lam vars.
    let tANF = normalizeTerm t in
    let env = if debug then Some pprintEnvEmpty else None () in
    let cfaDebug = checkpointCfaDebug checkpoint in
    let cfa = checkpointCfa checkpoint in
    match _testBase cfaDebug cfa env tANF with (env, cfaRes) in
    let aRes: Set Name = extractCheckpoint cfaRes in
    map (lam var: String. (var, setMem (nameNoSym var) aRes)) vars
in

let _testSymbolized: Bool -> Expr -> [String] -> [([Char], Bool)] =
  lam debug. lam t. lam vars.
    let tANF = normalizeTerm t in
    let env = if debug then Some pprintEnvEmpty else None () in
    let cfaDebug = checkpointCfaDebug checkpoint in
    let cfa = checkpointCfa checkpoint in
    match _testBase cfaDebug cfa env tANF with (env, cfaRes) in
    let aRes: Set Name = extractCheckpoint cfaRes in
    let sSet: Set String = setFold
      (lam acc. lam n. setInsert (nameGetStr n) acc)
      (setEmpty cmpString) aRes in
    map (lam var: String. (var, setMem var sSet)) vars
in

let _testWithSymbolize: Bool -> Expr -> [String] -> [([Char], Bool)] =
  lam debug. lam t. lam vars.
  let t = symbolizeExpr symEnvEmpty t in
  _testSymbolized debug t vars
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
  ("f", false),
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
  ("f", false),
  ("x", true),
  ("a", true),
  ("c1", true),
  ("c2", false),
  ("g", false),
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
  ("t", false),
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
  ("f2", false),
  ("apply1", false),
  ("apply2", false),
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

-- Checkpoints and higher-order constants
let t = _parse "
  let ls = [lam x1. x1, lam x2. x2, lam x3. x3] in
  let s1 = foldl (lam s2. lam. weight 1.0) 0 ls in
  let s3 = foldl (lam. lam. ()) 0 ls in
  let s4 = foldr (lam s5. lam. weight 1.0) 0 ls in
  let s6 = foldr (lam. lam. ()) 0 ls in
  let s7 = map (lam. weight 1.0) ls in
  let s8 = map (lam. ()) ls in
  let s9 = map (map (lam. weight 1.0)) [ls,ls,ls] in
  let s10 = mapi (lam s11. lam. weight 1.0) ls in
  let s12 = mapi (lam. lam. ()) ls in
  let s13 = iter (lam. weight 1.0) ls in
  let s14 = iter (lam. ()) ls in
  let s15 = iter (iter (lam. weight 1.0)) [ls,ls,ls] in
  let s16 = iteri (lam s17. lam. weight 1.0) ls in
  let s18 = iteri (lam. lam. ()) ls in
  let s19 = create 3 (lam. weight 1.0) in
  let s20 = create 3 (lam. ()) in
  let s21 = createList 3 (lam. weight 1.0) in
  let s22 = createList 3 (lam. ()) in
  let s23 = createRope 3 (lam. weight 1.0) in
  let s24 = createRope 3 (lam. ()) in
  let s25 = tensorCreateDense [3,3] (lam. weight 1.0) in
  let s26 = tensorCreateDense [3,3] (lam. ()) in
  let s27 = tensorCreateCArrayInt [3,3] (lam. weight 1.0; 1) in
  let s28 = tensorCreateCArrayInt [3,3] (lam. 1) in
  let s29 = tensorCreateCArrayFloat [3,3] (lam. weight 1.0; 1.0) in
  let s30 = tensorCreateCArrayFloat [3,3] (lam. 1.0) in
  let tensor = s26 in
  let s31 = tensorIterSlice (lam s32. lam. weight 1.0) tensor in
  let s33 = tensorIterSlice (lam. lam. ()) tensor in
  ()
------------------------" in
utest _testWithSymbolize false t [
  "s1","s2","s3","s4","s5","s6","s7","s8","s9",
  "s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20",
  "s21","s22","s23","s24","s25","s26","s27","s28","s29","s30", "s31","s32","s33"
] with [
  ("s1", true), ("s2", true), ("s3", false),
  ("s4", true), ("s5", true), ("s6", false),
  ("s7", true), ("s8", false), ("s9", true),
  ("s10", true), ("s11", true), ("s12", false),
  ("s13", true), ("s14", false), ("s15", true),
  ("s16", true), ("s17", true), ("s18", false),
  ("s19", true), ("s20", false),
  ("s21", true), ("s22", false),
  ("s23", true), ("s24", false),
  ("s25", true), ("s26", false),
  ("s27", true), ("s28", false),
  ("s29", true), ("s30", false),
  ("s31", true), ("s32", true), ("s33", false)
] using eqTest in

()

