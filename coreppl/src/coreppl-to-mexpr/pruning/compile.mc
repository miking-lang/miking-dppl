include "mexpr/externals.mc"

include "../../parser.mc"
include "../dists.mc"

-- ANF for pruning --
lang MExprPPLPruningANF = MExprPPL + DPPLParser + Externals + MExprANFAll
  -- specialized normalize for assume and observe, cancel observe and prunes
  sem normalize (k:Expr -> Expr) =
  | TmAssume ({ dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeDist
      (lam dist. k (TmAssume { t with dist = TmDist { td with dist = dist } }))
      dist
  | TmPrune ({ dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeDist
      (lam dist. k (TmPrune { t with dist = TmDist { td with dist = dist } }))
      dist
  | TmPruned ({ prune = TmPrune ({ dist = dist } & td) } & t) ->
    (TmPruned { t with prune = normalize k (TmPrune td) })
  | TmObserve ({ value = value, dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeName
      (lam value.
        normalizeDist
          (lam dist.
             k (TmObserve {{ t with value = value }
                               with dist = TmDist { td with dist = dist}}))
          dist)
      value
  | TmCancel ({ value = value, dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeName
      (lam value.
        normalizeDist
          (lam dist.
             k (TmCancel {{ t with value = value }
                               with dist = TmDist { td with dist = dist}}))
          dist)
      value
 end

 lang TransformPruningDist = TransformDist + MExprPPL +DPPLParser

  -- a parameter of a pruned distribution can be either
  syn EnvParam =
  | PruneFParam () -- a distribution, e.g  Categorical (PruneFParam p1)
  | SeqFParam () -- a sequence, e.g. Categorical (SeqFParam [0.3,0.7])

  -- value of a pruned observe can be an IntValue or PrunedValue
  syn EnvValue =
  | PrunedValue () -- observe (PrunedValue (getSeq d_seq)) ..
  | IntValue ()  -- observe (IntValue 0) ..

  sem createEnvParam: Map Name EnvParam -> Expr -> Map Name EnvParam
  sem createEnvParam pruneEnv =
  | TmLet ({body = TmPruned p} & t) ->
    let env = mapInsert t.ident (PruneFParam ()) pruneEnv in
    createEnvParam (createEnvParam env t.body) t.inexpr
  | TmLet ({body=TmApp {lhs=TmVar v1, rhs=TmVar v2}}&t) ->
    match mapLookup v2.ident pruneEnv with Some (PruneFParam _) then
    let env = mapInsert t.ident (PruneFParam ()) pruneEnv in
      createEnvParam env t.inexpr
    else
      match mapLookup v1.ident pruneEnv with Some (PruneFParam _) then
      let env = mapInsert t.ident (PruneFParam ()) pruneEnv in
      createEnvParam env t.inexpr
    else
      createEnvParam pruneEnv t.inexpr
  | t -> sfold_Expr_Expr createEnvParam pruneEnv t

  sem createEnvValue: Map Name EnvValue -> Expr -> Map Name EnvValue
  sem createEnvValue env =
  | TmLet t -> let env = match tyTm (t.body) with TyInt _ then
          mapInsert t.ident (IntValue ()) env else
        match tyTm (t.body) with TyPruneInt _ then
          mapInsert t.ident (PrunedValue ()) env else
        env in
        createEnvValue (createEnvValue env t.body) t.inexpr
  | t -> sfold_Expr_Expr createEnvValue env t

  sem extractParam: Map Name EnvParam -> Expr -> Expr
  sem extractParam env =
  | TmDist ({dist=DCategorical ({p=p}&d)}&t)->
    match assignCons env p with Some x then x else (conapp_ "PruneGraph_SeqFParam" p)

  sem assignValueCons: (Map Name EnvValue) -> Expr -> Option Expr
  sem assignValueCons env =
  | TmVar v ->
    let varType = mapLookup v.ident env in
    match varType with Some varType then
      Some (assignValueConsH (TmVar v) varType)
    else None ()
  | t -> error "not in ANF-form"

  sem assignValueConsH t =
  | PrunedValue _ -> (conapp_ "PruneGraph_PrunedValue" t)
  | IntValue _ -> (conapp_ "PruneGraph_IntValue" t)

  -- assign the parameter construct for pruned distributions
  -- e.g. PCategorical (PruneFParam p1) where p1:PruneVar
  -- PCategorical (SeqFParam [0.25,0.25,0.25,0.25])
  sem assignCons: (Map Name EnvParam) -> Expr -> Option Expr
  sem assignCons env =
  | TmVar v ->
    let varType = mapLookup v.ident env in
    match varType with Some varType then
      Some (assignConsH (TmVar v) varType)
    else None ()
  | t -> error "not in ANF-form"

  sem assignConsH t =
  | PruneFParam _ -> (conapp_ "PruneGraph_PruneFParam" t)
  | SeqFParam _ -> (conapp_ "PruneGraph_SeqFParam" t)

end

lang DPPLPruningTransform = TransformPruningDist

  type Env = {
    prunedEnv:Map Name Expr,
    prunedFEnv:Map Name Expr,
    valueEnv:Map Name EnvValue,
    env:Map Name EnvParam
  }

  sem replaceTyPruneInt =
  | t ->
    let t = smap_Expr_Type toRuntimeTyPruneVar t in
    let t = smap_Expr_TypeLabel toRuntimeTyPruneVar t in
    let t = smap_Expr_Pat replaceTyPruneVarPat t in
    let t = smap_Expr_Expr replaceTyPruneInt t in
    withType (toRuntimeTyPruneVar (tyTm t)) t

  sem toRuntimeTyPruneVar : Type -> Type
  sem toRuntimeTyPruneVar =
  | TyPruneInt t -> (tycon_ "PruneGraph_PruneVar")
  | ty -> smap_Type_Type toRuntimeTyPruneVar ty

  sem replaceTyPruneVarPat : Pat -> Pat
  sem replaceTyPruneVarPat =
  | p ->
    let p = smap_Pat_Pat replaceTyPruneVarPat p in
    withTypePat (toRuntimeTyPruneVar (tyPat p)) p

  sem checkValidPrune env =
  | TmPrune {dist=TmDist {dist=DCategorical {p=p}}} ->
    match p with TmVar v in
    match mapLookup v.ident env.prunedFEnv with Some _ then
      error "Distribution of a pruned variable cannot have a pruned parameter"
    else ()

  sem replaceTmPrunes env =
  | TmPrune p ->
    checkValidPrune env (TmPrune p);
    --let dst = transformTmDistDs env.env p.dist in
    match p.dist with TmDist ({dist=DCategorical ({p=p}&d)}&t) in
    appf1_ (var_ "initializePruneRVar") p
  | TmLet ({body=TmPruned p} &t) ->
    let prunedEnv = mapInsert t.ident p.prune env.prunedEnv in
    (replaceTmPrunes {env with prunedEnv=prunedEnv} t.inexpr)
  | TmLet ({body=TmApp ({lhs=TmVar v1, rhs=TmVar v2}&a)} & t) ->
    match mapLookup v2.ident env.prunedFEnv with Some _ then
      error "Pruned variable shouldn't be applied as an argument anywhere than to a distribution"
    else match mapLookup v1.ident env.prunedFEnv with Some body then
      match mapLookup v2.ident env.prunedEnv with Some prune then 
        error "Cannot handle two pruned variable at the same time"
      else
        match body with TmApp ({lhs=TmApp ({lhs=_, rhs=TmLam l}&a2), rhs=_}&a1) in
        let lamBody = TmApp {{a with lhs=l.body} with rhs=TmVar v2} in
        let tbody = match inspectType (tyTm (t.body)) with TyArrow _ then
          nulam_ l.ident lamBody
        else TmApp {a1 with lhs=TmApp {a2 with rhs=nulam_ l.ident lamBody}} in
        let tbodye=TmApp {a1 with lhs=TmApp {a2 with rhs=nulam_ l.ident lamBody}} in
        let prunedFEnv = mapInsert t.ident tbodye env.prunedFEnv in
        TmLet {{t with body = tbody} with inexpr=(replaceTmPrunes {env with prunedFEnv=prunedFEnv} t.inexpr)}
    else
    match mapLookup v2.ident env.prunedEnv with Some prune then
        let lamId = nameSym "" in
        let lamBody = TmApp {a with rhs=nvar_ lamId} in
        let tbody = match inspectType (tyTm (t.body)) with TyArrow _ then
          (nulam_ lamId lamBody)
        else appf2_ (var_ "initializePruneFVar") (nulam_ lamId lamBody) prune in
        let tbodyd = appf2_ (var_ "initializePruneFVar") (nulam_ lamId lamBody) prune in
        let prunedFEnv = mapInsert t.ident tbodyd env.prunedFEnv in
        TmLet {{t with body = tbody} with inexpr=(replaceTmPrunes {env with prunedFEnv=prunedFEnv} t.inexpr)}
    else
      smap_Expr_Expr (replaceTmPrunes env) (TmLet t)
  | TmAssume ({dist=TmDist ({dist=DCategorical d}&td)}&t) ->
    if not (prunedObserve env (TmAssume t)) then (TmAssume t) else
      error "assume cannot take a pruned random variable"
  | (TmObserve {value=TmVar v,dist=TmDist d} | TmCancel {value=TmVar v,dist=TmDist d}) & t ->
    if not (prunedObserve env t) then t else
      let value = match mapLookup v.ident env.prunedEnv with Some prune then prune else TmVar v in
      let value = match assignValueCons env.valueEnv value with Some x then x else error "wrong type at observe value field" in
      let param = extractParam env.env (TmDist d) in
      let wid = nameSym "w" in
      let w = match t with TmObserve _ then
        nulet_ wid (appf3_ (var_ "observePrune") false_ value param) 
      else nulet_ wid (appf3_ (var_ "observePrune") true_ value param) in
      bind_ w (ulet_ "" (weight_ (nvar_ wid)))
  | t -> smap_Expr_Expr (replaceTmPrunes env) t

  sem prunedObserve env =
  | (TmObserve {value=TmVar v,dist=TmDist {dist=DCategorical d}} | TmCancel {value=TmVar v,dist=TmDist {dist=DCategorical d}}) & t  ->
      match mapLookup v.ident env.prunedEnv with Some _ then true else
      match d.p with TmVar v2 in
      match mapLookup v2.ident env.prunedFEnv with Some _ then true else false
  | TmAssume {dist=TmDist {dist=DCategorical d}}  ->
      match d.p with TmVar v2 in
      match mapLookup v2.ident env.prunedFEnv with Some _ then true else false
  | _ -> false

  sem checkPrunes =
  | TmPrune _ -> error "should have been removed"
  | TmPruned _ -> error "should have been removed"
  | t -> smap_Expr_Expr checkPrunes t

  sem removePrunes =
  | TmPrune _ -> unit_
  | TmPruned _ -> unit_
  | t -> smap_Expr_Expr removePrunes t

 end

lang DPPLPruning = DPPLPruningTransform
  sem prune =
  | prog -> 
    -- 1 -- apply ANF first
    let prog = use MExprPPLPruningANF in normalizeTerm prog in
    -- 2 -- get the types for distribution parameters
    -- type environment for distributions
    let env = createEnvParam (mapEmpty nameCmp) prog in
    let valueEnv = createEnvValue (mapEmpty nameCmp) prog in
    -- 3 -- whenever you encounter with a PruneInt type replace it with runtime PruneVar type
    let prog = replaceTyPruneInt prog in
    -- 4 -- whenever you encounter with a TmPrune change it to runtime variable PruneRVar
    -- 5 -- whenever you encounter with a TmPruned change it to runtime variable PruneFVar with a map in values
    let prog = replaceTmPrunes {prunedEnv=(mapEmpty nameCmp),prunedFEnv=(mapEmpty nameCmp),valueEnv=valueEnv,env=env} prog in
    -- for debugging --
    checkPrunes prog;
    prog
end
