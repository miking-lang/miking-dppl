include "../infer-method.mc"

lang ImportanceSamplingMethod = InferMethodBase
  type ImportanceConfig =
    { particles : Expr -- : Int
    , earlyStop : Expr -- : Bool
    , cps : String
    , dynamicDelay : Bool
    , prune : Bool
    }
  syn InferMethod =
  | Importance ImportanceConfig

  sem pprintInferMethod indent env =
  | Importance x ->
    let i = pprintIncr indent in
    match pprintCode i env x.particles with (env, particles) in
    match pprintCode i env x.earlyStop with (env, earlyStop) in
    match pprintCode i env (str_ x.cps) with (env, cps) in
    let dynamicDelay = bool2string x.dynamicDelay in
    let prune = bool2string x.prune in
    ( env
    , join
      [ "(Importance "
      , "{ particles = ", particles
      , ", earlyStop = ", earlyStop
      , ", cps = ", cps
      , ", dynamicDelay = ", dynamicDelay
      , ", prune = ", prune
      , "})"
      ]
    )

  sem inferMethodFromCon info bindings =
  | "Importance" ->
    let expectedFields =
      [ ("particles", int_ _particlesDefault)
      , ("earlyStop", bool_ _earlyStopDefault)
      , ("cps", str_ _cpsDefault)
      , ("dynamicDelay", bool_ _dynamicDelayDefault)
      , ("prune", bool_ _pruneDefault)
      ] in
    match getFields info bindings expectedFields with [particles, earlyStop, cps, dynamicDelay, prune] in
    Importance
    { particles = particles
    , earlyStop = earlyStop
    , cps = _exprAsStringExn cps
    , dynamicDelay = _exprAsBoolExn dynamicDelay
    , prune = _exprAsBoolExn prune
    }

  sem inferMethodConfig info =
  | Importance x -> fieldsToRecord info
    [ ("particles", x.particles)
    , ("earlyStop", x.earlyStop)
    ]

  sem typeCheckInferMethod env info sampleType =
  | Importance x ->
    let int = TyInt {info = info} in
    let bool = TyBool {info = info} in
    let particles = typeCheckExpr env x.particles in
    unify env [info, infoTm particles] int (tyTm particles);
    let earlyStop = typeCheckExpr env x.earlyStop in
    unify env [info, infoTm earlyStop] bool (tyTm earlyStop);
    Importance {x with particles = particles, earlyStop = earlyStop}

  sem smapAccumL_InferMethod_Expr f acc =
  | Importance r ->
    match f acc r.particles with (acc, particles) in
    match f acc r.earlyStop with (acc, earlyStop) in
    (acc, Importance {r with particles = particles, earlyStop = earlyStop})

  sem retainDynamicDelayedSampling = | Importance x -> x.dynamicDelay
  sem retainPruning = | Importance x -> x.prune

  sem setRuns expr =
  | Importance r -> Importance {r with particles = expr}
end

let isLwOptions : OptParser (use ImportanceSamplingMethod in InferMethod) =
  use ImportanceSamplingMethod in
  let mk = lam particles. lam earlyStop. lam cps. lam dynamicDelay. lam prune. Importance
    { particles = int_ particles
    , earlyStop = bool_ earlyStop
    , cps = cps
    , dynamicDelay = dynamicDelay
    , prune = prune
    } in
  let method = optMap5 mk _particles _earlyStop _cps _dynamicDelay _prune in
  optMap2 (lam. lam x. x) (_methodFlag true "is") method
