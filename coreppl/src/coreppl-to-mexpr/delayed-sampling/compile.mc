include "mexpr/externals.mc"
include "../../coreppl.mc"
include "../../parser.mc"
include "../dists.mc"
lang MExprPPLDelayedANF = MExprPPL + DPPLParser + Externals + MExprANFAll
  -- specialized normalize for assume and observe, and delay
  sem normalize (k:Expr -> Expr) =
  | TmAssume ({ dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeDist
      (lam dist. k (TmAssume { t with dist = TmDist { td with dist = dist } }))
      dist
  | TmDelay ({ dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeDist
      (lam dist. k (TmDelay { t with dist = TmDist { td with dist = dist } }))
      dist
  | TmDelayed ({ delay = TmDelay ({ dist = dist } & td) } & t) ->
    (TmDelayed { t with delay = normalize k (TmDelay td) })
  | TmObserve ({ value = value, dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeName
      (lam value.
        normalizeDist
          (lam dist.
             k (TmObserve {{ t with value = value }
                               with dist = TmDist { td with dist = dist}}))
          dist)
      value
  | TmApp ({lhs=TmApp ({lhs=TmConst ({val=CAddf ()}&c),rhs=v1}&a2),rhs=v2}&a1) ->
    normalizeName
      (lam v1. normalizeName (lam v2. k (TmApp {{a1 with lhs=TmApp {{a2 with lhs=TmConst c} with rhs=v1}} with rhs=v2})) v2) v1
  | TmApp ({lhs=TmApp ({lhs=TmConst ({val=CMulf ()}&c),rhs=v1}&a2),rhs=v2}&a1) ->
    normalizeName
      (lam v1. normalizeName (lam v2. k (TmApp {{a1 with lhs=TmApp {{a2 with lhs=TmConst c} with rhs=v1}} with rhs=v2})) v2) v1
 end

lang TransformDsDist = TransformDist + MExprPPL +DPPLParser

  -- a parameter of a distribution can be either
  syn DEnvParam =
  | DelayParam () -- a distribution, e.g Gaussian a 1. where a ~ Gaussian 3. 2.
  | AffineParam {v:Expr,meanScale:Expr,meanOffset:Expr}

  sem affineAddTransform env id v =
  | (Some (DelayParam ()), None ()) -> mapInsert id (AffineParam {v=v.0,meanScale=float_ 1.,meanOffset=v.1}) env
  | (None (), Some (DelayParam ())) -> mapInsert id (AffineParam {v=v.1,meanScale=float_ 1.,meanOffset=v.0}) env
  | (Some (AffineParam p), None ()) -> mapInsert id (AffineParam {v=p.v,meanScale=p.meanScale,meanOffset=addf_ p.meanOffset v.1}) env
  | (None (), Some (AffineParam p)) -> mapInsert id (AffineParam {v=p.v,meanScale=p.meanScale,meanOffset=addf_ p.meanOffset v.0}) env
 | _ -> env

  sem affineScaleTransform env id v =
  | (Some (DelayParam ()), None ()) -> mapInsert id (AffineParam {v=v.0,meanScale=v.1,meanOffset=float_ 0.}) env
  | (None (), Some (DelayParam ())) -> mapInsert id (AffineParam {v=v.1,meanScale=v.0,meanOffset=float_ 0.}) env
  | (Some (AffineParam p), None ()) -> mapInsert id (AffineParam {v=p.v,meanScale=mulf_ p.meanScale v.1,meanOffset=mulf_ p.meanOffset v.1}) env
  | (None (), Some (AffineParam p)) -> mapInsert id (AffineParam {v=p.v,meanScale=mulf_ p.meanScale v.1,meanOffset=mulf_ p.meanOffset v.0}) env
  | _ -> env

  sem createDEnvParam: Map Name DEnvParam -> Expr -> Map Name DEnvParam
  sem createDEnvParam env =
  | TmLet ({body = TmDelayed p} & t) ->
    let env = mapInsert t.ident (DelayParam ()) env in
    createDEnvParam (createDEnvParam env t.body) t.inexpr
  | TmLet ({body= TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CAddf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    let v1Type = mapLookup v1.ident env in
    let v2Type = mapLookup v2.ident env in
    let env = affineAddTransform env t.ident (TmVar v1,TmVar v2) (v1Type, v2Type) in
    createDEnvParam (createDEnvParam env t.body) t.inexpr
  | TmLet ({body= TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CMulf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    let v1Type = mapLookup v1.ident env in
    let v2Type = mapLookup v2.ident env in
    let env = affineScaleTransform env t.ident (TmVar v1,TmVar v2) (v1Type, v2Type) in
    createDEnvParam (createDEnvParam env t.body) t.inexpr
  | t -> sfold_Expr_Expr createDEnvParam env t


  sem transformDsDistributions: Map Name DEnvParam -> Expr -> Expr
  sem transformDsDistributions env =
  | t ->
    let t = mapPre_Expr_Expr (transformTmDistDs env) t in
    replaceTyDist t

  sem transformTmDistDs: Map Name DEnvParam -> Expr -> Expr
  sem transformTmDistDs env =
  | TmDist t -> transformDsDist (withInfo t.info) env t.dist
  | TmConst {val = c &
      ( CDistEmpiricalSamples _
      | CDistEmpiricalDegenerate _
      | CDistEmpiricalNormConst _
      | CDistEmpiricalAcceptRate _
      )
    } -> var_ (getConstStringCode 0 c)
  | t -> t

  sem assignDCons: (Map Name DEnvParam) -> Expr -> Option Expr
  sem assignDCons env =
  | TmVar v ->
    let varType = mapLookup v.ident env in
    match varType with Some varType then
      Some (assignDConsH (TmVar v) varType)
    else None ()
  | t -> error "not in ANF-form"

  sem assignDConsH t =
  | DelayParam _ -> (conapp_ "DelayedGraph_DelayParam" t)
  | AffineParam _ ->  t

  sem transformDsDist: (Expr -> Expr) -> (Map Name DEnvParam) -> Dist -> Expr
  sem transformDsDist i env =
  | DBeta {a = a, b = b} ->
    let a = match assignDCons env a with Some x then x else (conapp_ "DelayedGraph_FloatParam" a) in
    let b = match assignDCons env b with Some x then x else (conapp_ "DelayedGraph_FloatParam" b) in
    i (conapp_ "DelayedGraph_DsDistBeta" (i (urecord_ [("a", a), ("b", b)])))
  | DBernoulli {p = p} ->
    let p = match assignDCons env p with Some x then x else (conapp_ "DelayedGraph_FloatParam" p) in
    i (conapp_ "DelayedGraph_DsDistBernoulli" (i (urecord_ [("p", p)])))
  | DGaussian {mu = mu, sigma = sigma} ->
    match mu with TmVar v in
    let res = match mapLookup v.ident env with Some (AffineParam p) then
        let mu = match assignDCons env p.v with Some x then x else (conapp_ "DelayedGraph_FloatParam" p.v) in
        (mu,p.meanScale,p.meanOffset)
      else let mu = match assignDCons env mu with Some x then x else (conapp_ "DelayedGraph_FloatParam" mu) in
        (mu, float_ 1.,float_ 0.) in
    let sigma = match assignDCons env sigma with Some x then x else (conapp_ "DelayedGraph_FloatParam" sigma) in
    i (conapp_ "DelayedGraph_DsDistGaussian" (i (urecord_ [("mu", res.0), ("sigma", sigma), ("meanScale", res.1), ("meanOffset",res.2)])))
  | DCategorical {p = p} ->
    let p = match assignDCons env p with Some x then x else (conapp_ "DelayedGraph_SeqFParam" p) in
    i (conapp_ "DelayedGraph_DsDistCategorical" (i (urecord_ [("p", p)])))
  | DUniform {a = a, b = b} ->
    let a = match assignDCons env a with Some x then x else (conapp_ "DelayedGraph_FloatParam" a) in
    let b = match assignDCons env b with Some x then x else (conapp_ "DelayedGraph_FloatParam" b) in
    i (conapp_ "DelayedGraph_DsDistUniform" (i (urecord_ [("a", a), ("b", b)])))
  | DPoisson {lambda = lambda} ->
    match lambda with TmVar v in
    let normalParams = let lambda = match assignDCons env lambda with Some x then x else (conapp_ "DelayedGraph_FloatParam" lambda) in
        (lambda, float_ 1.) in
    let res = match mapLookup v.ident env with Some (AffineParam p) then
        match p.meanOffset with TmConst ({val=CFloat {val = v}}&t) then
          if eqf v 0. then 
            let lambda = match assignDCons env p.v with Some x then x else (conapp_ "DelayedGraph_FloatParam" p.v) in
            (lambda,p.meanScale)
          else normalParams
        else normalParams
      else normalParams in
    -- here match lambda with AffineParam though only scale, what happens if addition?
    i (conapp_ "DelayedGraph_DsDistPoisson" (i (urecord_ [("lambda", res.0),("scale",res.1)])))
  | DBinomial {n = n, p = p} ->
    let n = match assignDCons env n with Some x then x else (conapp_ "DelayedGraph_IntParam" n) in
    let p = match assignDCons env p with Some x then x else (conapp_ "DelayedGraph_FloatParam" p) in
    i (conapp_ "DelayedGraph_DsDistBinomial" (i (urecord_ [("n", n), ("p", p)])))
  | DGamma {k = shape, theta = scale} ->
    let shape = match assignDCons env shape with Some x then x else (conapp_ "DelayedGraph_FloatParam" shape) in
    let scale = match assignDCons env scale with Some x then x else (conapp_ "DelayedGraph_FloatParam" scale) in
    i (conapp_ "DelayedGraph_DsDistGamma" (i (urecord_ [("shape", shape), ("scale", scale)])))
  | DExponential {rate = rate} ->
    let rate = match assignDCons env rate with Some x then x else (conapp_ "DelayedGraph_FloatParam" rate) in
    i (conapp_ "DelayedGraph_DsDistExponential" (i (urecord_ [("rate", rate)])))
  | DDirichlet {a = a} -> let a = match assignDCons env a with Some x then x else (conapp_ "DelayedGraph_SeqFParam" a) in
    i (conapp_ "DelayedGraph_DsDistDirichlet" (i (urecord_ [("a", a)])))
  | DMultinomial {n = n, p = p} ->
    let n = match assignDCons env n with Some x then x else (conapp_ "DelayedGraph_IntParam" n) in
    let p = match assignDCons env p with Some x then x else (conapp_ "DelayedGraph_FloatParam" p) in
    i (conapp_ "DelayedGraph_DsDistMultinomial" (i (urecord_ [("n", n), ("p", p)])))
  | _ -> error "No support for that dist"
end

lang DPPLDelayedTransform = TransformDsDist

   sem replaceTyDelay =
  | t ->
    let t = smap_Expr_Type toRuntimeTyDelayVar t in
    let t = smap_Expr_TypeLabel toRuntimeTyDelayVar t in
    let t = smap_Expr_Pat replaceTyDelayVarPat t in
    let t = smap_Expr_Expr replaceTyDelay t in
    withType (toRuntimeTyDelayVar (tyTm t)) t

  sem toRuntimeTyDelayVar : Type -> Type
  sem toRuntimeTyDelayVar =
  | TyDelayInt t -> tycon_ "DelayedGraph_DelayVar"
  | TyDelayFloat _ -> tycon_ "DelayedGraph_DelayVar"
  | TyDelaySeqF _ -> tycon_ "DelayedGraph_DelayVar"
  | ty -> smap_Type_Type toRuntimeTyDelayVar ty

  sem replaceTyDelayVarPat : Pat -> Pat
  sem replaceTyDelayVarPat =
  | p ->
    let p = smap_Pat_Pat replaceTyDelayVarPat p in
    withTypePat (toRuntimeTyDelayVar (tyPat p)) p

  sem typeMatch =
  | TyArrow {from=TyDelayFloat _, to=_} -> true
  | TyArrow {from=TyDelayInt _, to=_} -> true
  | TyArrow {from=TyDelaySeqF _, to=_} -> true
  | _ -> false

  sem replaceWithValue env =
  | TmLet ({body = TmAssume _} &t) ->
    TmLet {t with inexpr = replaceWithValue env t.inexpr}
  | TmLet ({body = TmObserve _} &t) ->
    TmLet {t with inexpr = replaceWithValue env t.inexpr}
  | TmLet ({body=TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CAddf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    TmLet {t with inexpr = replaceWithValue env t.inexpr}
  | TmLet ({body=TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CMulf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    TmLet {t with inexpr = replaceWithValue env t.inexpr}
  | TmLet ({body=TmDelayed p} &t) ->
    TmLet {t with inexpr = replaceWithValue env t.inexpr}
  | TmApp ({lhs = lhs, rhs=TmVar v2}&a1) ->
    let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
    let varType = mapLookup v2.ident env in
    let var = match varType with Some (DelayParam _) then
      if typeMatch (tyTm lhs) then (TmVar v2) else 
      appf2_ (var_ "value") sampleT (conapp_ "DelayedGraph_DelayParam" (TmVar v2))
    else (TmVar v2) in
   TmApp {a1 with rhs=var} 
  | t -> smap_Expr_Expr (replaceWithValue env) t

  sem replaceReturn env =
  | TmLet t -> TmLet {t with inexpr=replaceReturn env t.inexpr}
  | TmRecLets t -> TmRecLets {t with inexpr=replaceReturn env t.inexpr}
  | TmType t -> TmType {t with inexpr=replaceReturn env t.inexpr}
  | TmExt t -> TmExt {t with inexpr=replaceReturn env t.inexpr}
  | TmConDef t -> TmConDef {t with inexpr=replaceReturn env t.inexpr}
  | TmVar t -> 
    let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
    let varType = mapLookup t.ident env in
    match varType with Some (DelayParam _) then
      appf2_ (var_ "value") sampleT (conapp_ "DelayedGraph_DelayParam" (TmVar t))
    else (TmVar t)
  | t -> t

  sem affineAddTransformBody tbody sampleT v =
  | (Some (DelayParam _), Some (DelayParam _|AffineParam _)) -> affineTransformBodyH sampleT addf_ v
  | (Some (AffineParam _), Some (AffineParam _ | DelayParam _)) -> affineTransformBodyH sampleT addf_ v
  | (Some (DelayParam _), None ()) -> (conapp_ "DelayedGraph_AffineParam" (urecord_ [("aV", conapp_ "DelayedGraph_DelayParam" v.0), ("meanScale",float_ 1. ), ("meanOffset",v.1)]))
  | (None (), Some (DelayParam _)) -> (conapp_ "DelayedGraph_AffineParam" (urecord_ [("aV", conapp_ "DelayedGraph_DelayParam" v.1), ("meanScale",float_ 1. ), ("meanOffset",v.0)]))
  | (Some (AffineParam p), None ()) -> (conapp_ "DelayedGraph_AffineParam" (urecord_ [("aV", conapp_ "DelayedGraph_DelayParam" p.v), ("meanScale",p.meanScale ), ("meanOffset",addf_ v.1 p.meanOffset)]))
  | (None (), Some (AffineParam p)) -> (conapp_ "DelayedGraph_AffineParam" (urecord_ [("aV", conapp_ "DelayedGraph_DelayParam" p.v), ("meanScale",p.meanScale ), ("meanOffset",addf_ v.0 p.meanOffset)]))
  | t -> tbody

  sem affineScaleTransformBody tbody sampleT v =
  | (Some (DelayParam _), Some (DelayParam _)) -> affineTransformBodyH sampleT mulf_ v
  | (Some (DelayParam _), Some (AffineParam _)) -> affineTransformBodyH sampleT mulf_ v
  | (Some (AffineParam _), Some (AffineParam _)) -> affineTransformBodyH sampleT mulf_ v
  | (Some (AffineParam _), Some (DelayParam _)) -> affineTransformBodyH sampleT mulf_ v
  | (Some (DelayParam _), None ()) -> (conapp_ "DelayedGraph_AffineParam" (urecord_ [("aV", conapp_ "DelayedGraph_DelayParam" v.0), ("meanScale",v.1 ), ("meanOffset",float_ 0.)]))
  | (None (), Some (DelayParam _)) -> (conapp_ "DelayedGraph_AffineParam" (urecord_ [("aV", conapp_ "DelayedGraph_DelayParam" v.1), ("meanScale", v.0 ), ("meanOffset",float_ 0.)]))
  | (Some (AffineParam p), None ()) -> (conapp_ "DelayedGraph_AffineParam" (urecord_ [("aV", conapp_ "DelayedGraph_DelayParam" p.v), ("meanScale",mulf_ p.meanScale v.1 ), ("meanOffset",mulf_ v.1 p.meanOffset)]))
  | (None (), Some (AffineParam p)) -> (conapp_ "DelayedGraph_AffineParam" (urecord_ [("aV", conapp_ "DelayedGraph_DelayParam" p.v), ("meanScale",mulf_ v.0 p.meanScale ), ("meanOffset",mulf_ v.0 p.meanOffset)]))
  | t -> tbody

  sem affineTransformBodyH sampleT op =
  | v -> op (appf2_ (var_ "value") sampleT (conapp_ "DelayedGraph_DelayParam" v.0)) (appf2_ (var_ "value") sampleT (conapp_ "DelayedGraph_DelayParam" v.1))

  sem replaceTmDelayed env =
  | TmDelay d -> 
    let param = transformTmDistDs env.env (d.dist) in
    appf1_ (var_ "createDelayVar") param
  | TmVar t ->
    match mapLookup t.ident env.delayedEnv with Some v then v else TmVar t --replace the delayed variables with delay variables 
  | TmLet ({body=TmDelayed p} &t) ->
    let delayedEnv = mapInsert t.ident p.delay env.delayedEnv in
    (replaceTmDelayed {env with delayedEnv=delayedEnv} t.inexpr)
  | (TmAssume ({dist=TmDist d}) | TmObserve ({dist=TmDist d})) & t ->
    if checkRVDist env false (TmDist d) then
      let param = transformTmDistDs env.env (TmDist d) in
      let param = (replaceTyDelay (replaceTmDelayed env param)) in
      let rvIdent = nameSym "" in
      let code = match t with TmAssume _ then appf1_ (var_ "createDelayVar") param
        else match t with TmObserve o then (appf2_ (var_ "createObsDelayVar") param o.value) else never in
      let vertex = nulet_ rvIdent code in
      let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
      let dst = appf2_ (var_ "transformDsDist") sampleT (appf1_ (var_ "getMargDist") (nvar_ rvIdent)) in
      bindall_
      [ vertex
      , ulet_ "" (appf2_ (var_ "valueDs") sampleT (nvar_ rvIdent))
      ,  match t with TmAssume a then (appf1_ (var_ "getValue") (nvar_ rvIdent)) else match t with TmObserve o then (TmObserve {o with dist = dst}) else never
      ]
    else t
  | TmLet ({body=TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CAddf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    let v1Type = mapLookup v1.ident env.env in
    let v2Type = mapLookup v2.ident env.env in
    match (match mapLookup v1.ident env.delayedEnv with Some v then
    (v,{env with delayedEnv=mapInsert t.ident v env.delayedEnv}) else (TmVar v1, env)) with (TmVar v1,env) in
    match (match mapLookup v2.ident env.delayedEnv with Some v then
    (v,{env with delayedEnv=mapInsert t.ident v env.delayedEnv}) else (TmVar v2, env)) with (TmVar v2,env) in
    let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
    let tbody = affineAddTransformBody t.body sampleT (TmVar v1, TmVar v2) (v1Type,v2Type) in
    let tbody = replaceTyDelay (replaceTmDelayed env tbody) in
    TmLet {{{t with body = tbody} with inexpr=(replaceTmDelayed env t.inexpr)} with tyAnnot = tyunknown_}
  | TmLet ({body=TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CMulf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    let v1Type = mapLookup v1.ident env.env in
    let v2Type = mapLookup v2.ident env.env in
    match (match mapLookup v1.ident env.delayedEnv with Some v then
    (v,{env with delayedEnv=mapInsert t.ident v env.delayedEnv}) else (TmVar v1, env)) with (TmVar v1,env) in
    match (match mapLookup v2.ident env.delayedEnv with Some v then
    (v,{env with delayedEnv=mapInsert t.ident v env.delayedEnv}) else (TmVar v2, env)) with (TmVar v2,env) in
    let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
    let tbody = affineScaleTransformBody t.body sampleT (TmVar v1, TmVar v2) (v1Type,v2Type) in
    let tbody = (replaceTyDelay (replaceTmDelayed env tbody)) in
    TmLet {{{t with body = tbody} with inexpr=(replaceTmDelayed env t.inexpr)} with tyAnnot = tyunknown_}
  
  | t -> smap_Expr_Expr (replaceTmDelayed env) t   

  sem checkRVDist env acc =
  | TmVar t -> match mapLookup t.ident env.delayedEnv with Some v then or acc true else 
    match mapLookup t.ident env.env with Some v then or acc true else or acc false
  | t -> sfold_Expr_Expr (checkRVDist env) acc t 

end

lang DPPLDelayedSampling = DPPLDelayedTransform
  sem delayedSampling =
  | prog ->
    -- apply ANF first
    let prog = use MExprPPLDelayedANF in normalizeTerm prog in
    -- figure out which variables to be delayed
    let envParam = createDEnvParam (mapEmpty nameCmp) prog in
    -- if a random variable 'x' needs to be sampled, replace those places with 'value x'
    let prog = replaceWithValue envParam prog in
    let prog = replaceReturn envParam prog in
    let prog =  replaceTmDelayed {env=envParam, delayedEnv=(mapEmpty nameCmp)} prog in
    let prog = replaceTyDelay prog in prog
end

