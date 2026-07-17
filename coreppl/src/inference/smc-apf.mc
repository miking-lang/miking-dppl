include "../infer-method.mc"

lang APFMethod = InferMethodBase
  type APFConfig =
    { particles : Expr -- : Int
    , subsample : Expr -- : Bool
    , subsampleSize : Expr -- : Int
    , maxPropagations : Expr -- : Int
    , resample : String
    , prune : Bool
    , cps : String
    }
  syn InferMethod =
  | APF APFConfig

  sem pprintInferMethod indent env =
  | APF x ->
    let i = pprintIncr indent in
    match pprintCode i env x.particles with (env, particles) in
    match pprintCode i env x.subsample with (env, subsample) in
    match pprintCode i env x.subsampleSize with (env, subsampleSize) in
    match pprintCode i env x.maxPropagations with (env, maxPropagations) in
    match pprintCode i env (str_ x.resample) with (env, resample) in
    match pprintCode i env (str_ x.cps) with (env, cps) in
    let prune = bool2string x.prune in
    ( env
    , join
      [ "(APF "
      , "{ particles = ", particles
      , ", subsample = ", subsample
      , ", subsampleSize = ", subsampleSize
      , ", maxPropagations = ", maxPropagations
      , ", resample = ", resample
      , ", prune = ", prune
      , ", cps = ", cps
      , "})"
      ]
    )

  sem inferMethodFromCon info bindings =
  | "APF" ->
    let expectedFields =
      [ ("particles", int_ _particlesDefault)
      , ("subsample", bool_ _subsampleDefault)
      , ("subsampleSize", int_ _subsampleSizeDefault)
      , ("maxPropagations", int_ _maxPropagationsDefault)
      , ("resample", str_ _resampleDefault)
      , ("prune", bool_ _pruneDefault)
      , ("cps", str_ _cpsDefault)
      ] in
    match getFields info bindings expectedFields
      with [particles, subsample, subsampleSize, maxPropagations, resample, prune, cps] in
    APF
    { particles = particles
    , subsample = subsample
    , subsampleSize = subsampleSize
    , maxPropagations = maxPropagations
    , resample = _exprAsStringExn resample
    , prune = _exprAsBoolExn prune
    , cps = _exprAsStringExn cps
    }

  sem inferMethodConfig info =
  | APF x -> fieldsToRecord info
    [ ("particles", x.particles)
    , ("subsample", x.subsample)
    , ("subsampleSize", x.subsampleSize)
    , ("maxPropagations", x.maxPropagations)
    ]

  sem typeCheckInferMethod env info sampleType =
  | APF x ->
    let int = TyInt {info = info} in
    let bool = TyBool {info = info} in
    let particles = typeCheckExpr env x.particles in
    unify env [info, infoTm particles] int (tyTm particles);
    let subsample = typeCheckExpr env x.subsample in
    unify env [info, infoTm subsample] bool (tyTm subsample);
    let subsampleSize = typeCheckExpr env x.subsampleSize in
    unify env [info, infoTm subsampleSize] int (tyTm subsampleSize);
    let maxPropagations = typeCheckExpr env x.maxPropagations in
    unify env [info, infoTm maxPropagations] int (tyTm maxPropagations);
    APF {x with particles = particles, subsample = subsample, subsampleSize = subsampleSize, maxPropagations = maxPropagations}

  sem smapAccumL_InferMethod_Expr f acc =
  | APF r ->
    match f acc r.particles with (acc, particles) in
    match f acc r.subsample with (acc, subsample) in
    match f acc r.subsampleSize with (acc, subsampleSize) in
    match f acc r.maxPropagations with (acc, maxPropagations) in
    (acc, APF {r with particles = particles, subsample = subsample, subsampleSize = subsampleSize, maxPropagations = maxPropagations})

  sem setRuns expr =
  | APF r -> APF {r with particles = expr}
end

let smcApfOptions : OptParser (use APFMethod in InferMethod) =
  use APFMethod in
  let mk = lam particles. lam subsample. lam subsampleSize. lam maxPropagations. lam resample. lam prune. lam cps. APF
    { particles = int_ particles
    , subsample = bool_ subsample
    , subsampleSize = int_ subsampleSize
    , maxPropagations = int_ maxPropagations
    , resample = resample
    , prune = prune
    , cps = cps
    } in
  let method = optApply (optApply (optMap5 mk _particles _subsample _subsampleSize _maxPropagations _resample) _prune) _cps in
  optMap2 (lam. lam x. x) (_methodFlag false "smc-apf") method
