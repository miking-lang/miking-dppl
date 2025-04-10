include "mexpr/externals.mc"

include "../../parser.mc"
include "../dists.mc"

lang MExprPPLPruningCPS = MExprPPL + DPPLParser + MExprCPS
  sem exprCps env k =
  | TmLet ({ body = TmCancel _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmPrune _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmPruned _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
 end

 lang TransformPruningDist = TransformDist + MExprPPL +DPPLParser

  -- a parameter of a pruned distribution can be either
  syn DistParam =
  | PruneFParam () -- a distribution, e.g  Categorical (PruneFParam p1)
  | SeqFParam () -- a sequence, e.g. Categorical (SeqFParam [0.3,0.7])

  -- value of a pruned observe can be an IntValue or PrunedValue
  syn ObsValue =
  | PrunedValue () -- observe (PrunedValue (getSeq d_seq)) ..
  | IntValue ()  -- observe (IntValue 0) ..

  sem createDistParam envs =
  | TmLet ({body = TmPruned p} & t) ->
    let prunedEnv = setInsert t.ident envs.prunedEnv in
    createDistParam {envs with prunedEnv=prunedEnv} t.inexpr
  | TmLet ({body=TmApp {lhs=TmVar v1, rhs=TmVar v2}}&t) ->
    if setMem v2.ident envs.prunedEnv then 
      let distParamEnv = mapInsert t.ident (PruneFParam ()) envs.distParamEnv in
      createDistParam {envs with distParamEnv=distParamEnv} t.inexpr
    else match mapLookup v2.ident envs.distParamEnv with Some (PruneFParam _) then
      let distParamEnv = mapInsert t.ident (PruneFParam ()) envs.distParamEnv in
      createDistParam {envs with distParamEnv=distParamEnv} t.inexpr
    else
      match mapLookup v1.ident envs.distParamEnv with Some (PruneFParam _) then
      let distParamEnv = mapInsert t.ident (PruneFParam ()) envs.distParamEnv in
      createDistParam {envs with distParamEnv=distParamEnv} t.inexpr
    else
      createDistParam envs t.inexpr
  | t -> sfold_Expr_Expr createDistParam envs t

  sem createObsValue: Map Name ObsValue -> Expr -> Map Name ObsValue
  sem createObsValue env =
  | TmLet t -> let env = 
    match tyTm (t.body) with TyInt _ then
      mapInsert t.ident (IntValue ()) env else
    match tyTm (t.body) with TyPruneInt _ then
      mapInsert t.ident (PrunedValue ()) env else env in
    createObsValue (createObsValue env t.body) t.inexpr
  | t -> sfold_Expr_Expr createObsValue env t

  sem createDistEnv: Map Name Expr -> Expr -> Map Name Expr
  sem createDistEnv distEnv =
  | TmLet ({body=TmDist ({dist=DCategorical _}&d)}&t) ->
    createDistEnv (mapInsert t.ident t.body distEnv) t.inexpr
  | t -> sfold_Expr_Expr createDistEnv distEnv t

  sem extractParam env runtimeEnv =
  | TmVar ({ident=id}&v) ->
    match mapLookup id env.distEnv with Some (TmDist ({dist=DCategorical ({p=p}&d)}&t)) in
    match assignCons env.paramEnv runtimeEnv p with Some x then x else (nconapp_ (_getConExn "PruneGraph_SeqFParam" runtimeEnv.env) p)

  sem assignValueCons env runtimeEnv =
  | TmVar v ->
    let varType = mapLookup v.ident env in
    match varType with Some varType then
      Some (assignValueConsH runtimeEnv.env (TmVar v) varType)
    else None ()
  | t -> error "not in ANF-form"

  sem assignValueConsH env t =
  | PrunedValue _ -> nconapp_ (_getConExn "PruneGraph_PrunedValue" env) t
  | IntValue _ -> nconapp_ (_getConExn "PruneGraph_IntValue" env) t

  -- assign the parameter construct for pruned distributions
  -- e.g. PCategorical (PruneFParam p1) where p1:PruneVar
  -- PCategorical (SeqFParam [0.25,0.25,0.25,0.25])
  sem assignCons env runtimeEnv =
  | TmVar v ->
    let varType = mapLookup v.ident env in
    match varType with Some varType then
      Some (assignConsH runtimeEnv.env (TmVar v) varType)
    else None ()
  | t -> error "not in ANF-form"

  sem assignConsH env t =
  | PruneFParam _ -> nconapp_ (_getConExn "PruneGraph_PruneFParam" env) t

end

lang DPPLPruningTransform = TransformPruningDist

  type Env = {
    prunedEnv:Map Name Expr,
    prunedFEnv:Map Name (Expr,Expr),
    valueEnv:Map Name ObsValue,
    env:Map Name DistParam,
    distEnv:Map Name Expr
  }

  sem replacePruneTypes env =
  | t ->
    let t = smap_Expr_Type (toRuntimePruneTyVar env) t in
    let t = smap_Expr_TypeLabel (toRuntimePruneTyVar env) t in
    let t = smap_Expr_Pat (replacePruneTyVarPat env) t in
    let t = smap_Expr_Expr (replacePruneTypes env) t in
    withType (toRuntimePruneTyVar env (tyTm t)) t

  sem toRuntimePruneTyVar env =
  | TyPruneInt t -> ntycon_ (_getTyConExn "PruneGraph_PruneVar" env.env)
  | ty -> smap_Type_Type (toRuntimePruneTyVar env) ty

  sem replacePruneTyVarPat env =
  | p ->
    let p = smap_Pat_Pat (replacePruneTyVarPat env) p in
    withTypePat (toRuntimePruneTyVar env (tyPat p)) p


  sem checkValidPrune env =
  | TmPrune p ->
    match p.dist with TmVar v in
    match mapLookup v.ident env.distEnv with Some (TmDist {dist=DCategorical {p=p}}) in
    match p with TmVar v in
    match mapLookup v.ident env.prunedFEnv with Some _ then
      error "Distribution of a pruned variable cannot have a pruned parameter"
    else p

  sem prunedCases env runtimeEnv tmApp =
  | (Some (lamExpr,prune1), Some (body,prune2)) ->
    error "TODO:Pruned variable shouldn't be applied as an argument anywhere than to a distribution"
  | (Some (lamExpr,prune), None ()) ->
    match tmApp with TmLet ({body=TmApp ({lhs=TmVar v1, rhs=TmVar v2}&a)} & t) in
    match mapLookup v2.ident env.prunedEnv with Some prune then
      error "Cannot handle two pruned variable at the same time"
    else
      -- add variable application to the lambda body
      let lamId = nameSym "s" in
      let lamExpr = nulam_ lamId (appf2_ lamExpr (nvar_ lamId) (TmVar v2)) in
      let tbody = match inspectType (tyTm (t.body)) with TyArrow _ then lamExpr
        else appFromEnv runtimeEnv "initializePruneFVar" [lamExpr,prune] in
      let prunedFEnv = mapInsert t.ident (tbody,prune) env.prunedFEnv in
      TmLet {{t with body = tbody} with inexpr=(replaceTmPrunes {env with prunedFEnv=prunedFEnv} runtimeEnv t.inexpr)}
  | (None (), Some body) -> error "Pruned variable shouldn't be applied as an argument anywhere than to a distribution"
  | (None (), None ()) ->
    match tmApp with TmLet ({body=TmApp ({lhs=TmVar v1, rhs=TmVar v2}&a)} & t) in
    -- if v2 is a prune variable, create a lambda expression as an argument to map over the each state of pruned variable
    match mapLookup v2.ident env.prunedEnv with Some prune then
      let lamId = nameSym "s" in
      let lamExpr = nulam_ lamId (TmApp {a with rhs=nvar_ lamId}) in
      let tbody = match inspectType (tyTm (t.body)) with TyArrow _ then --partial application
        lamExpr
      else appFromEnv runtimeEnv "initializePruneFVar" [lamExpr,prune] in
      --let tbodyd = appFromEnv runtimeEnv "initializePruneFVar" [nulam_ lamId lamBody,prune] in
      let prunedFEnv = mapInsert t.ident (tbody,prune) env.prunedFEnv in
      TmLet {{t with body = tbody} with inexpr=(replaceTmPrunes {env with prunedFEnv=prunedFEnv} runtimeEnv t.inexpr)}
    else
      smap_Expr_Expr (replaceTmPrunes env runtimeEnv) (TmLet t)

  sem replaceTmPrunes env runtimeEnv =
  -- If it is a prune variable
  | TmLet ({body=TmPrune p} &t) ->
    let param = checkValidPrune env (TmPrune p) in
    -- Replace it with the initializePruneRVar function
    TmLet {{t with body=appFromEnv runtimeEnv "initializePruneRVar" [param]} with inexpr = replaceTmPrunes env runtimeEnv t.inexpr}
  -- If it is annotated with pruned
  | TmLet ({body=TmPruned p} &t) ->
    -- add it to the pruned variables env
    let prunedEnv = mapInsert t.ident p.prune env.prunedEnv in
    (replaceTmPrunes {env with prunedEnv=prunedEnv} runtimeEnv t.inexpr)
  -- If it is a function application
  | TmLet ({body=TmApp ({lhs=TmVar v1, rhs=TmVar v2}&a)} & t) ->
    -- check if v1 is a previous pruned function application, 
    let var1 = mapLookup v1.ident env.prunedFEnv in
    -- check if v2 is a previous pruned function application
    let var2 = mapLookup v2.ident env.prunedFEnv in
    prunedCases env runtimeEnv (TmLet t) (var1,var2)
  | TmLet ({body=TmAssume t} &tl) ->
    if not (prunedObserve env (TmAssume t)) then TmLet {tl with inexpr = replaceTmPrunes env runtimeEnv tl.inexpr} else
      error "assume cannot take a pruned random variable"
  | TmLet ({body=(TmObserve {value=TmVar v,dist=dist} | TmCancel {value=TmVar v,dist=dist}) & t } &tl) ->
    if not (prunedObserve env t) then TmLet {tl with inexpr=(replaceTmPrunes env runtimeEnv tl.inexpr)}
 else
      let value = match mapLookup v.ident env.prunedEnv with Some prune then prune else TmVar v in
      let value = match assignValueCons env.valueEnv runtimeEnv value with Some x then x else error "wrong type at observe value field" in
      let param = extractParam env runtimeEnv dist in
      let wId = nameSym "w" in
      let w = match t with TmObserve _ then
        nulet_ wId (appFromEnv runtimeEnv "observePrune" [false_, value, param])
        --(appf3_ (var_ "observePrune") false_ value param)
      else nulet_ wId (appFromEnv runtimeEnv "observePrune" [true_, value, param]) in
      TmLet {{tl with body = bind_ w (ulet_ "" (weight_ (nvar_ wId)))} with inexpr=(replaceTmPrunes env runtimeEnv tl.inexpr)}
  | t -> smap_Expr_Expr (replaceTmPrunes env runtimeEnv) t

  sem prunedObserve env =
  | (TmObserve {value=TmVar v,dist=TmVar v2} | TmCancel {value=TmVar v,dist=TmVar v2}) & t  ->
      match mapLookup v2.ident env.distEnv with Some (TmDist {dist=DCategorical d}) then
        match mapLookup v.ident env.prunedEnv with Some _ then true else
        match d.p with TmVar v2 in
        match mapLookup v2.ident env.prunedFEnv with Some _ then true else false
      else match mapLookup v.ident env.prunedEnv with Some _ then true else false
  | TmAssume {dist=TmVar v2}  ->
      match mapLookup v2.ident env.distEnv with Some (TmDist {dist=DCategorical d}) then
        match d.p with TmVar v2 in
        match mapLookup v2.ident env.prunedFEnv with Some _ then true else false
      else false
  | _ -> false

  sem checkPrunes =
  | TmPrune _ -> error "should have been removed"
  | TmPruned _ -> error "should have been removed"
  | t -> smap_Expr_Expr checkPrunes t
 end

lang DPPLPruning = DPPLPruningTransform + MExprPPLPruningCPS
  sem prune runtimeEnv =
  | prog ->
    -- get the ANF applied program
    -- create constructors whether
    match createDistParam {prunedEnv=(setEmpty nameCmp), distParamEnv=(mapEmpty nameCmp)} prog
      with {prunedEnv=prunedEnv,distParamEnv=paramEnv} in
    let valueEnv = createObsValue (mapEmpty nameCmp) prog in
    -- to statically check whether pruning applied distribution is Categorical and 
    -- in extractParam to apply the runtime construct for the distribution parameter  
    let distEnv = createDistEnv (mapEmpty nameCmp) prog in
    let prog = replaceTmPrunes {prunedEnv=(mapEmpty nameCmp),prunedFEnv=(mapEmpty nameCmp),valueEnv=valueEnv,paramEnv=paramEnv,distEnv=distEnv} runtimeEnv prog in
    let prog = use DPPLPruningTransform in replacePruneTypes runtimeEnv prog in
    prog
end

