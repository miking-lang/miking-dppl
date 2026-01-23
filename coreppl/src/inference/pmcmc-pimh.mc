include "../infer-method.mc"

lang PIMHMethod = InferMethodBase
  type PIMHConfig =
    { particles : Expr -- : Int
    , iterations : Expr -- : Int
    , cps : String
    }
  syn InferMethod =
  | PIMH PIMHConfig

  sem pprintInferMethod indent env =
  | PIMH x ->
    let i = pprintIncr indent in
    match pprintCode i env x.particles with (env, particles) in
    match pprintCode i env x.iterations with (env, iterations) in
    match pprintCode i env (str_ x.cps) with (env, cps) in
    ( env
    , join
      [ "(PIMH "
      , "{ particles = ", particles
      , ", iterations = ", iterations
      , ", cps = ", cps
      , "})"
      ]
    )

  sem inferMethodFromCon info bindings =
  | "PIMH" ->
    let expectedFields =
      [ ("iterations", int_ _iterationsDefault)
      , ("particles", int_ _particlesDefault)
      , ("cps", str_ _cpsDefault)
      ] in
    match getFields info bindings expectedFields with [particles, iterations, cps] in
    PIMH
    { particles = particles
    , iterations = iterations
    , cps = _exprAsStringExn cps
    }

  sem inferMethodConfig info =
  | PIMH x -> fieldsToRecord info
    [ ("iterations",x.iterations)
    , ("particles", x.particles)
    ]

  sem typeCheckInferMethod env info sampleType =
  | PIMH x ->
    let int = TyInt {info = info} in
    let iterations = typeCheckExpr env x.iterations in
    let particles = typeCheckExpr env x.particles in
    unify env [info, infoTm iterations] int (tyTm iterations);
    unify env [info, infoTm particles] int (tyTm particles);
    PIMH {x with iterations = iterations, particles = particles}

  sem smapAccumL_InferMethod_Expr f acc =
  | PIMH r ->
    match f acc r.iterations with (acc, iterations) in
    match f acc r.particles with (acc, particles) in
    (acc, PIMH {r with iterations = iterations, particles = particles})

  sem setRuns expr =
  | PIMH r -> PIMH {r with iterations = expr}
end

let pmcmcPimhOptions : OptParser (use PIMHMethod in InferMethod) =
  use PIMHMethod in
  let mk = lam particles. lam iterations. lam cps. PIMH
    { particles = int_ particles
    , iterations = int_ iterations
    , cps = cps
    } in
  let method = optMap3 mk _particles _iterations _cps in
  optMap2 (lam. lam x. x) (_methodFlag false "pmcmc-pimh") method
