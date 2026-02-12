--include "../infer-method.mc"
include "../coreppl.mc"

lang LightweightMCMCMethod = InferMethodBase + Assume
  type LightweightMCMCConfig =
    { keepSample : Expr -- : Int -> Bool
    , continue : Expr -- : (() -> a, a -> SampleInfo -> r -> (a, Bool)) for some 'a' , where SampleInfo can be found in "coreppl-to-mexpr/mcmc-lightweight/config.mc"
    , temperature : Expr -- : a -> Float for same 'a'
    , resampleBehavior : Expr -- : (acc -> Int -> (acc,([Bool], Int -> Int)))
    , debug : Expr -- : (a, a -> DebugInfo -> a) for some 'a', where DebugInfo can be found in "coreppl-to-mexpr/mcmc-lightweight/config.mc"
    , driftKernel : Bool
    , driftScale : Float
    , cps : String
    , align : Bool
    , debugAlignment : Option String
    }
  syn InferMethod =
  | LightweightMCMC LightweightMCMCConfig

  sem pprintInferMethod indent env =
  | LightweightMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.keepSample with (env, keepSample) in
    match pprintCode i env t.continue with (env, continue) in
    match pprintCode i env t.temperature with (env, temperature) in
    match pprintCode i env t.resampleBehavior with (env, resampleBehavior) in
    match pprintCode i env t.debug with (env, debug) in
    let driftScale = float2string t.driftScale in
    match pprintCode i env (str_ t.cps) with (env, cps) in
    match optionMapAccum (pprintCode i) env (optionMap str_ t.debugAlignment) with (env, debugAlignment) in
    let align = bool2string t.align in
    let driftKernel = bool2string t.driftKernel in
    ( env
    , join
      [ "(LightweightMCMC "
      , "{ keepSample = ", keepSample
      , ", continue = ", continue
      , ", temperature = ", temperature
      , ", resampleBehavior = ", resampleBehavior
      , ", debug = ", debug
      , ", driftKernel = ", driftKernel
      , ", driftScale = ", driftScale
      , ", cps = ", cps
      , ", align = ", align
      , ", debugAlignment = ", optionGetOr "\"\"" debugAlignment
      , "})"
      ]
    )

  sem inferMethodFromCon info bindings =
  | "LightweightMCMC" ->
    let expectedFields =
      [ ( "continue"
        , utuple_
          [ ulam_ "" (int_ _particlesDefault)
          , ulam_ "remaining" (ulam_ "" (ulam_ ""
            (utuple_ [subi_ (var_ "remaining") (int_ 1), neqi_ (var_ "remaining") (int_ 0)])))
          ]
        )
      , ("temperature", ulam_ "" (float_ 1.0))
      , ("keepSample", ulam_ "" true_)
      , ("debug", utuple_ [unit_, ulam_ "" (ulam_ "" unit_)])
      , ("resampleBehavior"
        , ulam_ "acc" (ulam_ "length"(
        ( utuple_ 
          [var_ "acc"
          , utuple_  [(create_ (var_ "length") (ulam_ "" (bool_ true)))
          , if_ (assume_ (bern_ (float_ _mcmcLightweightGlobalProbDefault))) 
            (negi_ (int_ 2))
            (assume_ (uniformDiscrete_ (int_ 0) (subi_ (var_ "length") (int_ 1))))]
          ]
        )))
        )
      --need to build a lambda as current behavior of MCMC lightweight 
      , ("driftKernel", bool_ _driftKernelDefault)
      , ("driftScale", float_ _driftScaleDefault)
      , ("cps", str_ _cpsDefault)
      , ("align", bool_ _alignDefault)
      , ("debugAlignment", str_ "")
      ] in
    match getFields info bindings expectedFields
    with [continue, temperature, keepSample, debug, resampleBehavior, driftKernel, driftScale, cps, align, debugAlignment] in
    LightweightMCMC
    { continue = continue
    , temperature = temperature
    , keepSample = keepSample
    , resampleBehavior = resampleBehavior
    , debug = debug
    , driftKernel = _exprAsBoolExn driftKernel
    , driftScale = _exprAsFloatExn driftScale
    , cps = _exprAsStringExn cps
    , align = _exprAsBoolExn align
    , debugAlignment = switch _exprAsStringExn debugAlignment
      case "" then None ()
      case str then Some str
      end
    }

  -- NOTE(vipa, 2025-04-15): 'inferMethodConfig' must be kept up to
  -- date with "coreppl-to-mexpr/mcmc-lightweight/config.mc"
  sem inferMethodConfig info =
  | LightweightMCMC t -> fieldsToRecord info
    [ ("continue", t.continue)
    , ("temperature", t.temperature)
    , ("keepSample", t.keepSample)
    , ("debug", t.debug)
    , ("resampleBehavior", t.resampleBehavior)
    , ("driftKernel", bool_ t.driftKernel)
    ]

  -- NOTE(vipa, 2025-04-15): 'inferMethodConfig' must be kept up to
  -- date with "coreppl-to-mexpr/mcmc-lightweight/config.mc"
  sem typeCheckInferMethod env info sampleType =
  | LightweightMCMC t ->
    let bool = TyBool {info = info} in
    let int = TyInt {info = info} in
    let float = TyFloat {info = info} in
    let continueState = newmonovar env.currentLvl info in
    let continue = typeCheckExpr env t.continue in
    let sampleInfo = tyrecord_ 
      [ ("weight", float) 
      , ("priorWeight", float)
      ] in
    let continueType = tytuple_
      [ tyarrow_ tyunit_ continueState
      , tyarrows_ [continueState, sampleInfo, sampleType, tytuple_ [continueState, bool]]
      ] in
    unify env [info, infoTm continue] continueType (tyTm continue);
    let temperature = typeCheckExpr env t.temperature in 
    let temperatureType = tyarrow_ continueState float in
    unify env [info, infoTm temperature] temperatureType (tyTm temperature);
    let debugState = newmonovar env.currentLvl info in
    let debug = typeCheckExpr env t.debug in
    let debugInfo = tyrecord_
      [ ("accepted", bool)
      ] in
    let debugType = tytuple_
      [ debugState
      , tyarrows_ [debugState, debugInfo, debugState]
      ] in
    unify env [info, infoTm debug] debugType (tyTm debug);
    let keepSample = typeCheckExpr env t.keepSample in
    unify env [info, infoTm keepSample] (tyarrow_ int bool) (tyTm keepSample);
    let resampleBehavior = typeCheckExpr env t.resampleBehavior in
    let resampleBehaviorType = tyarrows_ 
      [continueState, int, tytuple_ [continueState, tytuple_[tyseq_ bool, int]]]
    in
    unify env [info, infoTm resampleBehavior] resampleBehaviorType (tyTm resampleBehavior);
    LightweightMCMC { t with
      continue = continue,
      temperature = temperature,
      keepSample = keepSample,
      resampleBehavior = resampleBehavior,
      debug = debug
    }

  sem smapAccumL_InferMethod_Expr f acc =
  | LightweightMCMC r ->
    match f acc r.continue with (acc, continue) in
    match f acc r.temperature with (acc, temperature) in
    match f acc r.keepSample with (acc, keepSample) in
    match f acc r.resampleBehavior with (acc, resampleBehavior) in
    match f acc r.debug with (acc, debug) in
    (acc,
     LightweightMCMC {r with continue = continue, temperature = temperature, keepSample = keepSample, resampleBehavior = resampleBehavior, debug = debug})

  sem setRuns expr =
  | LightweightMCMC r -> LightweightMCMC {r with continue = utuple_
    [ ulam_ "" (expr)
    , ulam_ "remaining" (ulam_ "" (ulam_ ""
      (utuple_ [subi_ (var_ "remaining") (int_ 1), neqi_ (var_ "remaining") (int_ 0)])))
    ] }
end

let mcmcLightweightOptions : OptParser (use LightweightMCMCMethod in InferMethod) =
  use LightweightMCMCMethod in
  let mk = lam particles. lam globalProb. lam driftKernel. lam driftScale. lam cps. lam align. lam debugAlignment. LightweightMCMC
    { keepSample = ulam_ "" true_
    , continue = utuple_
      [ ulam_ "" (int_ particles)
      , ulam_ "remaining" (ulam_ "" (ulam_ ""
        (utuple_ [subi_ (var_ "remaining") (int_ 1), eqi_ (var_ "remaining") (int_ 0)])))
      ]
    , temperature = ulam_ "" (float_ 1.0)
    , resampleBehavior = 
        ulam_ "acc" (ulam_ "length"(
        ( utuple_ 
          [var_ "acc"
          , utuple_  [(create_ (var_ "length") (ulam_ "" (bool_ true)))
          , if_ (assume_ (bern_ (float_ globalProb)))
            (negi_ (int_ 2)) 
            (assume_ (uniformDiscrete_ (int_ 0) (subi_ (var_ "length") (int_ 1))))]
          ]
        )))
    , debug = utuple_ [unit_, ulam_ "" (ulam_ "" unit_)]
    , driftKernel = driftKernel
    , driftScale = driftScale
    , cps = cps
    , align = align
    , debugAlignment = debugAlignment
    } in
  let method = optApply (optApply (optMap5 mk _particles _mcmcLightweightGlobalProb _driftKernel _driftScale _cps) _align) _debugAlignment in
  optMap2 (lam. lam x. x) (_methodFlag false "mcmc-lightweight") method
