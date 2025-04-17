include "mexpr/externals.mc"
include "../../coreppl.mc"
include "../../parser.mc"
include "../dists.mc"

let conapp = use InferenceInterface in lam env. lam str. lam t. nconapp_ (_getConExn str env.env) t

lang MExprPPLDelayedCPS = MExprPPL + DPPLParser + MExprCPS
  sem exprCps env k =
  | TmLet ({ body = TmDelayed _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmDelay _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  end

lang TransformDsDist = TransformDist + MExprPPL +DPPLParser

  -- a parameter of a distribution can be either
  syn DParam =
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

  sem createDParam: Map Name DParam -> Expr -> Map Name DParam
  sem createDParam env =
  | TmLet ({body = TmDelayed p} & t) ->
    let env = mapInsert t.ident (DelayParam ()) env in
    createDParam (createDParam env t.body) t.inexpr
  | TmLet ({body= TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CAddf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    let v1Type = mapLookup v1.ident env in
    let v2Type = mapLookup v2.ident env in
    let env = affineAddTransform env t.ident (TmVar v1,TmVar v2) (v1Type, v2Type) in
    createDParam (createDParam env t.body) t.inexpr
  | TmLet ({body= TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CMulf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    let v1Type = mapLookup v1.ident env in
    let v2Type = mapLookup v2.ident env in
    let env = affineScaleTransform env t.ident (TmVar v1,TmVar v2) (v1Type, v2Type) in
    createDParam (createDParam env t.body) t.inexpr
  | t -> sfold_Expr_Expr createDParam env t

  sem replaceTyDistDelay env =
  | t ->
    let t = smap_Expr_Type (toRuntimeTyDistDelay env) t in
    let t = smap_Expr_TypeLabel (toRuntimeTyDistDelay env) t in
    let t = smap_Expr_Pat (replaceTyDistPatDelay env) t in
    let t = smap_Expr_Expr (replaceTyDistDelay env) t in
    withType (toRuntimeTyDistDelay env (tyTm t)) t

  sem toRuntimeTyDistDelay env =
  | TyDist t ->
    let cname = _getTyConExn "DelayedGraph_DsDist" env.env in
    TyApp {
      lhs = TyCon {ident=cname, info=t.info, data = tyunknown_},
      rhs = t.ty,
      info = t.info}
  | ty -> smap_Type_Type (toRuntimeTyDistDelay env) ty

  sem replaceTyDistPatDelay env =
  | p ->
    let p = smap_Pat_Pat (replaceTyDistPatDelay env) p in
    withTypePat (toRuntimeTyDistDelay env (tyPat p)) p

  sem transformDsDistributions env runtimeDelayEnv =
  | t ->
    let t = mapPre_Expr_Expr (transformTmDistDs env runtimeDelayEnv) t in
    replaceTyDistDelay runtimeDelayEnv t

  sem transformTmDistDs env runtimeDelayEnv =
  | TmDist t -> transformDsDist (withInfo t.info) env runtimeDelayEnv t.dist
  | t -> t

  sem assignDCons env runtimeDelayEnv =
  | TmVar v ->
    let varType = mapLookup v.ident env in
    match varType with Some varType then
      Some (assignDConsH (TmVar v) runtimeDelayEnv varType)
    else None ()
  | t -> error "not in ANF-form"

  sem assignDConsH t runtimeDelayEnv =
  | DelayParam _ -> (conapp runtimeDelayEnv "DelayedGraph_DelayParam" t)
  | AffineParam _ ->  t

  sem transformDsDist i env runtimeDelayEnv =
  | DBeta {a = a, b = b} ->
    let a = match assignDCons env runtimeDelayEnv a with Some x then x else
      (conapp runtimeDelayEnv "DelayedGraph_FloatParam" a) in
    let b = match assignDCons env runtimeDelayEnv b with Some x then x else
      (conapp runtimeDelayEnv "DelayedGraph_FloatParam" b) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistBeta" (i (autoty_record_ [("a", a), ("b", b)])))
  | DBernoulli {p = p} ->
    let p = match assignDCons env runtimeDelayEnv p with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" p) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistBernoulli" (i (autoty_record_ [("p", p)])))
  | DGaussian {mu = mu, sigma = sigma} ->
    match mu with TmVar v in
    let res = match mapLookup v.ident env with Some (AffineParam p) then
        let mu = match assignDCons env runtimeDelayEnv p.v with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" p.v) in
        (mu,p.meanScale,p.meanOffset)
      else let mu = match assignDCons env runtimeDelayEnv mu with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" mu) in
        (mu, float_ 1.,float_ 0.) in
    let sigma = match assignDCons env runtimeDelayEnv sigma with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" sigma) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistGaussian" (i (autoty_record_ [("mu", res.0), ("sigma", sigma), ("meanScale", res.1), ("meanOffset",res.2)])))
  | DCategorical {p = p} ->
    let p = match assignDCons env runtimeDelayEnv p with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_SeqFParam" p) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistCategorical" (i (autoty_record_ [("p", p)])))
  | DUniform {a = a, b = b} ->
    let a = match assignDCons env runtimeDelayEnv a with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" a) in
    let b = match assignDCons env runtimeDelayEnv b with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" b) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistUniform" (i (autoty_record_ [("a", a), ("b", b)])))
  | DPoisson {lambda = lambda} ->
    match lambda with TmVar v in
    let normalParams = let lambda = match assignDCons env runtimeDelayEnv lambda with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" lambda) in
        (lambda, float_ 1.) in
    let res = match mapLookup v.ident env with Some (AffineParam p) then
        match p.meanOffset with TmConst ({val=CFloat {val = v}}&t) then
          if eqf v 0. then
            let lambda = match assignDCons env runtimeDelayEnv p.v with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" p.v) in
            (lambda,p.meanScale)
          else normalParams
        else normalParams
      else normalParams in
    -- here match lambda with AffineParam though only scale, what happens if addition?
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistPoisson" (i (autoty_record_ [("lambda", res.0),("scale",res.1)])))
  | DBinomial {n = n, p = p} ->
    let n = match assignDCons env runtimeDelayEnv n with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_IntParam" n) in
    let p = match assignDCons env runtimeDelayEnv p with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" p) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistBinomial" (i (autoty_record_ [("n", n), ("p", p)])))
  | DNegBinomial {n = n, p = p} ->
      let n = match assignDCons env runtimeDelayEnv n with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_IntParam" n) in
      let p = match assignDCons env runtimeDelayEnv p with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" p) in
      i (conapp runtimeDelayEnv "DelayedGraph_DsDistNegBinomial" (i (autoty_record_ [("n", n), ("p", p)])))
  | DGamma {k = shape, theta = scale} ->
    let shape = match assignDCons env runtimeDelayEnv shape with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" shape) in
    let scale = match assignDCons env runtimeDelayEnv scale with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" scale) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistGamma" (i (autoty_record_ [("shape", shape), ("scale", scale)])))
  | DGeometric {p = p} ->
    let p = match assignDCons env runtimeDelayEnv p with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" p) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistGeometric" (i (autoty_record_ [("p", p)])))
  | DExponential {rate = rate} ->
    let rate = match assignDCons env runtimeDelayEnv rate with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" rate) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistExponential" (i (autoty_record_ [("rate", rate)])))
  | DDirichlet {a = a} -> let a = match assignDCons env runtimeDelayEnv a with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_SeqFParam" a) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistDirichlet" (i (autoty_record_ [("a", a)])))
  | DMultinomial {n = n, p = p} ->
    let n = match assignDCons env runtimeDelayEnv n with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_IntParam" n) in
    let p = match assignDCons env runtimeDelayEnv p with Some x then x else (conapp runtimeDelayEnv "DelayedGraph_FloatParam" p) in
    i (conapp runtimeDelayEnv "DelayedGraph_DsDistMultinomial" (i (autoty_record_ [("n", n), ("p", p)])))
  | _ -> error "No support for that dist"
end

lang DPPLDelayedTransform = TransformDsDist
   sem replaceTyDelay runtimeDelayEnv =
  | t ->
    let t = smap_Expr_Type (toRuntimeTyDelayVar runtimeDelayEnv) t in
    let t = smap_Expr_TypeLabel (toRuntimeTyDelayVar runtimeDelayEnv) t in
    let t = smap_Expr_Pat (replaceTyDelayVarPat runtimeDelayEnv) t in
    let t = smap_Expr_Expr (replaceTyDelay runtimeDelayEnv) t in
    withType (toRuntimeTyDelayVar runtimeDelayEnv (tyTm t)) t

  sem toRuntimeTyDelayVar runtimeDelayEnv =
  | TyDelayInt _ | TyDelayFloat _ | TyDelaySeqF _ -> ntycon_ (_getTyConExn "DelayedGraph_DelayVar" runtimeDelayEnv.env)
  | ty -> smap_Type_Type (toRuntimeTyDelayVar runtimeDelayEnv) ty

  sem replaceTyDelayVarPat runtimeDelayEnv =
  | p ->
    let p = smap_Pat_Pat (replaceTyDelayVarPat runtimeDelayEnv) p in
    withTypePat (toRuntimeTyDelayVar runtimeDelayEnv (tyPat p)) p

  sem typeMatch =
  | TyArrow {from=TyDelayFloat _, to=_} -> true
  | TyArrow {from=TyDelayInt _, to=_} -> true
  | TyArrow {from=TyDelaySeqF _, to=_} -> true
  | _ -> false

  sem replaceWithValue env runtimeDelayEnv =
  | TmLet ({body = TmAssume _} &t) ->
    TmLet {t with inexpr = replaceWithValue env runtimeDelayEnv t.inexpr}
  | TmLet ({body = TmObserve _} &t) ->
    TmLet {t with inexpr = replaceWithValue env runtimeDelayEnv t.inexpr}
  | TmLet ({body=TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CAddf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    TmLet {t with inexpr = replaceWithValue env runtimeDelayEnv t.inexpr}
  | TmLet ({body=TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CMulf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    TmLet {t with inexpr = replaceWithValue env runtimeDelayEnv t.inexpr}
  | TmLet ({body=TmDelayed p} &t) ->
    TmLet {t with inexpr = replaceWithValue env runtimeDelayEnv t.inexpr}
  | TmApp ({lhs = lhs, rhs=TmVar v2}&a1) ->
    let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
    let varType = mapLookup v2.ident env in
    let var = match varType with Some (DelayParam _) then
      if typeMatch (tyTm lhs) then (TmVar v2) else
      let conapp = nconapp_ (_getConExn "DelayedGraph_DelayParam" runtimeDelayEnv.env) (TmVar v2) in
      appFromEnv runtimeDelayEnv "value" [sampleT, conapp]
    else (TmVar v2) in
   TmApp {a1 with rhs=var}
  | t -> smap_Expr_Expr (replaceWithValue env runtimeDelayEnv) t

  sem replaceReturn env runtimeDelayEnv =
  | TmLet t -> TmLet {t with inexpr=replaceReturn env runtimeDelayEnv t.inexpr}
  | TmRecLets t -> TmRecLets {t with inexpr=replaceReturn env runtimeDelayEnv t.inexpr}
  | TmType t -> TmType {t with inexpr=replaceReturn env runtimeDelayEnv t.inexpr}
  | TmExt t -> TmExt {t with inexpr=replaceReturn env runtimeDelayEnv t.inexpr}
  | TmConDef t -> TmConDef {t with inexpr=replaceReturn env runtimeDelayEnv t.inexpr}
  | TmVar t ->
    let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
    let varType = mapLookup t.ident env in
    match varType with Some (DelayParam _) then
      let conapp = nconapp_ (_getConExn "DelayedGraph_DelayParam" runtimeDelayEnv.env) (TmVar t) in
      appFromEnv runtimeDelayEnv "value" [sampleT, conapp]
    else (TmVar t)
  | t -> t

  sem  affineAddTransformBody runtimeDelayEnv tbody sampleT v =
  | (Some (DelayParam _), Some (DelayParam _|AffineParam _)) ->  affineTransformBodyH runtimeDelayEnv sampleT addf_ v
  | (Some (AffineParam _), Some (AffineParam _ | DelayParam _)) ->  affineTransformBodyH runtimeDelayEnv sampleT addf_ v
  | (Some (DelayParam _), None ()) -> (conapp runtimeDelayEnv "DelayedGraph_AffineParam" (autoty_record_ [("aV", conapp runtimeDelayEnv "DelayedGraph_DelayParam" v.0), ("meanScale",float_ 1. ), ("meanOffset",v.1)]))
  | (None (), Some (DelayParam _)) -> (conapp runtimeDelayEnv "DelayedGraph_AffineParam" (autoty_record_ [("aV", conapp runtimeDelayEnv "DelayedGraph_DelayParam" v.1), ("meanScale",float_ 1. ), ("meanOffset",v.0)]))
  | (Some (AffineParam p), None ()) -> (conapp runtimeDelayEnv "DelayedGraph_AffineParam" (autoty_record_ [("aV", conapp runtimeDelayEnv "DelayedGraph_DelayParam" p.v), ("meanScale",p.meanScale ), ("meanOffset",addf_ v.1 p.meanOffset)]))
  | (None (), Some (AffineParam p)) -> (conapp runtimeDelayEnv "DelayedGraph_AffineParam" (autoty_record_ [("aV", conapp runtimeDelayEnv "DelayedGraph_DelayParam" p.v), ("meanScale",p.meanScale ), ("meanOffset",addf_ v.0 p.meanOffset)]))
  | t -> tbody

  sem  affineScaleTransformBody runtimeDelayEnv tbody sampleT v =
  | (Some (DelayParam _), Some (DelayParam _)) ->  affineTransformBodyH runtimeDelayEnv sampleT mulf_ v
  | (Some (DelayParam _), Some (AffineParam _)) ->  affineTransformBodyH runtimeDelayEnv sampleT mulf_ v
  | (Some (AffineParam _), Some (AffineParam _)) ->  affineTransformBodyH runtimeDelayEnv sampleT mulf_ v
  | (Some (AffineParam _), Some (DelayParam _)) ->  affineTransformBodyH runtimeDelayEnv sampleT mulf_ v
  | (Some (DelayParam _), None ()) -> (conapp runtimeDelayEnv "DelayedGraph_AffineParam" (autoty_record_ [("aV", conapp runtimeDelayEnv "DelayedGraph_DelayParam" v.0), ("meanScale",v.1 ), ("meanOffset",float_ 0.)]))
  | (None (), Some (DelayParam _)) -> (conapp runtimeDelayEnv "DelayedGraph_AffineParam" (autoty_record_ [("aV", conapp runtimeDelayEnv "DelayedGraph_DelayParam" v.1), ("meanScale", v.0 ), ("meanOffset",float_ 0.)]))
  | (Some (AffineParam p), None ()) -> (conapp runtimeDelayEnv "DelayedGraph_AffineParam" (autoty_record_ [("aV", conapp runtimeDelayEnv "DelayedGraph_DelayParam" p.v), ("meanScale",mulf_ p.meanScale v.1 ), ("meanOffset",mulf_ v.1 p.meanOffset)]))
  | (None (), Some (AffineParam p)) -> (conapp runtimeDelayEnv "DelayedGraph_AffineParam" (autoty_record_ [("aV", conapp runtimeDelayEnv "DelayedGraph_DelayParam" p.v), ("meanScale",mulf_ v.0 p.meanScale ), ("meanOffset",mulf_ v.0 p.meanOffset)]))
  | t -> tbody

  sem  affineTransformBodyH runtimeDelayEnv sampleT op =
  | v -> op (appFromEnv runtimeDelayEnv "value" [sampleT, (conapp runtimeDelayEnv "DelayedGraph_DelayParam" v.0)])
    (appFromEnv runtimeDelayEnv "value" [sampleT, (conapp runtimeDelayEnv "DelayedGraph_DelayParam" v.1)])

  sem replaceTmDelayed env runtimeDelayEnv =
  | TmDelay d ->
    -- transform the distributions to delay distributions
    let param = transformTmDistDs env.env runtimeDelayEnv (d.dist) in
    appFromEnv runtimeDelayEnv "createDelayVar" [param]
  | TmVar t ->
    match mapLookup t.ident env.delayedEnv with Some v then v else TmVar t --replace the delayed variables with delay variables
  | TmLet ({body=TmDelayed p} &t) ->
    let delayedEnv = mapInsert t.ident p.delay env.delayedEnv in
    (replaceTmDelayed {env with delayedEnv=delayedEnv} runtimeDelayEnv t.inexpr)
  | (TmAssume ({dist=dist}) | TmObserve ({dist=dist})) & t ->
        let param = (replaceTyDelay runtimeDelayEnv (replaceTmDelayed env runtimeDelayEnv dist)) in
        let rvIdent = nameSym "" in
        let code = match t with TmAssume _ then appFromEnv runtimeDelayEnv "createDelayVar" [param]
        else match t with TmObserve o then appFromEnv runtimeDelayEnv "createObsDelayVar" [param, o.value]
        else never in
      let vertex = nulet_ rvIdent code in
      let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
      let dst = appFromEnv runtimeDelayEnv "transformDsDist" [sampleT,
        appFromEnv runtimeDelayEnv "getMargDist" [nvar_ rvIdent]] in
      bindall_
      [ vertex
      , ulet_ "" (appFromEnv runtimeDelayEnv "valueDs" [sampleT,nvar_ rvIdent])
      ,  match t with TmAssume a then appFromEnv runtimeDelayEnv "getValue" [nvar_ rvIdent]
        else match t with TmObserve o then (TmObserve {o with dist = dst}) else never
      ]
  | TmLet ({body=TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CAddf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    let v1Type = mapLookup v1.ident env.env in
    let v2Type = mapLookup v2.ident env.env in
    match (match mapLookup v1.ident env.delayedEnv with Some v then
    (v,{env with delayedEnv=mapInsert t.ident v env.delayedEnv}) else (TmVar v1, env)) with (TmVar v1,env) in
    match (match mapLookup v2.ident env.delayedEnv with Some v then
    (v,{env with delayedEnv=mapInsert t.ident v env.delayedEnv}) else (TmVar v2, env)) with (TmVar v2,env) in
    let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
    let tbody =  affineAddTransformBody runtimeDelayEnv t.body sampleT (TmVar v1, TmVar v2) (v1Type,v2Type) in
    let tbody = replaceTyDelay runtimeDelayEnv (replaceTmDelayed env runtimeDelayEnv tbody) in
    TmLet {{{t with body = tbody} with inexpr=(replaceTmDelayed env runtimeDelayEnv t.inexpr)} with tyAnnot = tyunknown_}
  | TmLet ({body=TmApp ({lhs = TmApp ({lhs=TmConst ({val= (CMulf ())}&c),rhs=TmVar v1}&a2), rhs=TmVar v2}&a1)}&t) ->
    let v1Type = mapLookup v1.ident env.env in
    let v2Type = mapLookup v2.ident env.env in
    match (match mapLookup v1.ident env.delayedEnv with Some v then
    (v,{env with delayedEnv=mapInsert t.ident v env.delayedEnv}) else (TmVar v1, env)) with (TmVar v1,env) in
    match (match mapLookup v2.ident env.delayedEnv with Some v then
    (v,{env with delayedEnv=mapInsert t.ident v env.delayedEnv}) else (TmVar v2, env)) with (TmVar v2,env) in
    let sampleT = (ulam_ "d" (assume_ (var_ "d"))) in
    let tbody =  affineScaleTransformBody runtimeDelayEnv t.body sampleT (TmVar v1, TmVar v2) (v1Type,v2Type) in
    let tbody = (replaceTyDelay runtimeDelayEnv (replaceTmDelayed env runtimeDelayEnv tbody)) in
    TmLet {{{t with body = tbody} with inexpr=(replaceTmDelayed env runtimeDelayEnv t.inexpr)} with tyAnnot = tyunknown_}
  | t -> smap_Expr_Expr (replaceTmDelayed env runtimeDelayEnv) t
end

lang DPPLDelayedSampling = DPPLDelayedTransform + InferenceInterface + MExprPPLDelayedCPS
  sem delayedSampling runtimeDelayEnv =
  | prog ->
    let envParam = createDParam (mapEmpty nameCmp) prog in
    let prog = transformDsDistributions envParam runtimeDelayEnv prog in
    let prog = replaceWithValue envParam runtimeDelayEnv prog in
    let prog = replaceReturn envParam runtimeDelayEnv prog in
    let prog = replaceTmDelayed {env=envParam, delayedEnv=(mapEmpty nameCmp)} runtimeDelayEnv prog in
    let prog = replaceTyDelay runtimeDelayEnv prog in prog
end

