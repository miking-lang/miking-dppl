include "../infer-method.mc"

lang BPFMethod = InferMethodBase
  type BPFConfig =
    { particles : Expr -- : Int
    , subsample : Expr -- : Bool
    , subsampleSize : Expr -- : Int
    , resample : String
    , cps : String
    , prune : Bool
    , dynamicDelay : Bool
    }
  syn InferMethod =
  | BPF BPFConfig

  sem pprintInferMethod indent env =
  | BPF x ->
    let i = pprintIncr indent in
    match pprintCode i env x.particles with (env, particles) in
    match pprintCode i env x.subsample with (env, subsample) in
    match pprintCode i env x.subsampleSize with (env, subsampleSize) in
    match pprintCode i env (str_ x.resample) with (env, resample) in
    match pprintCode i env (str_ x.cps) with (env, cps) in
    let prune = bool2string x.prune in
    let dynamicDelay = bool2string x.dynamicDelay in
    ( env
    , join
      [ "(BPF "
      , "{ particles = ", particles
      , ", subsample = ", subsample
      , ", subsampleSize = ", subsampleSize
      , ", resample = ", resample
      , ", cps = ", cps
      , ", prune = ", prune
      , ", dynamicDelay = ", dynamicDelay
      , "})"
      ]
    )

  sem inferMethodFromCon info bindings =
  | "BPF" ->
    let expectedFields =
      [ ("particles", int_ _particlesDefault)
      , ("subsample", bool_ _subsampleDefault)
      , ("subsampleSize", int_ _subsampleSizeDefault)
      , ("resample", str_ _resampleDefault)
      , ("cps", str_ _cpsDefault)
      , ("prune", bool_ _pruneDefault)
      , ("dynamicDelay", bool_ _dynamicDelayDefault)
      ] in
    match getFields info bindings expectedFields
      with [particles, subsample, subsampleSize, resample, cps, prune, dynamicDelay] in
    BPF
    { particles = particles
    , subsample = subsample
    , subsampleSize = subsampleSize
    , resample = _exprAsStringExn resample
    , cps = _exprAsStringExn cps
    , prune = _exprAsBoolExn prune
    , dynamicDelay = _exprAsBoolExn dynamicDelay
    }

  sem inferMethodConfig info =
  | BPF x -> fieldsToRecord info
    [ ("particles", x.particles)
    , ("subsample", x.subsample)
    , ("subsampleSize", x.subsampleSize)
    ]

  sem typeCheckInferMethod env info sampleType =
  | BPF x ->
    let int = TyInt {info = info} in
    let bool = TyBool {info = info} in
    let particles = typeCheckExpr env x.particles in
    unify env [info, infoTm particles] int (tyTm particles);
    let subsampleSize = typeCheckExpr env x.subsampleSize in
    unify env [info, infoTm subsampleSize] int (tyTm subsampleSize);
    let subsample = typeCheckExpr env x.subsample in
    unify env [info, infoTm subsample] bool (tyTm subsample);
    BPF {x with particles = particles, subsample = subsample, subsampleSize = subsampleSize}

  sem smapAccumL_InferMethod_Expr env =
  | BPF r ->
    match f acc r.particles with (acc, particles) in
    match f acc r.subsample with (acc, subsample) in
    match f acc r.subsampleSize with (acc, subsampleSize) in
    (acc, BPF {r with particles = particles, subsample = subsample, subsampleSize = subsampleSize})

  sem retainPruning = | BPF x -> x.prune
  sem retainDynamicDelayedSampling = | BPF x -> x.dynamicDelay

  sem setRuns expr =
  | BPF r -> BPF {r with particles = expr}
end

let smcBpfOptions : OptParser (use BPFMethod in InferMethod) =
  use BPFMethod in
  let mk = lam particles. lam subsample. lam subsampleSize. lam resample. lam cps. lam prune. lam dynamicDelay. BPF
    { particles = int_ particles
    , subsample = bool_ subsample
    , subsampleSize = int_ subsampleSize
    , resample = resample
    , cps = cps
    , prune = prune
    , dynamicDelay = dynamicDelay
    } in
  let method =
    optApply (optApply (optMap5 mk _particles _subsample _subsampleSize _resample _cps) _prune) _dynamicDelay in
  optMap2 (lam. lam x. x) (_methodFlag false "smc-bpf") method
