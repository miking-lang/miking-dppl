include "../coreppl.mc"
include "../dppl-arg.mc"

lang LightweightMCMCMethod = MExprPPL
  syn InferMethod =
  | LightweightMCMC {
      keepSample : Expr, -- Type (Int -> Bool)
      continue : Expr, -- Type (a, a -> r -> (a, Bool)) for some 'a'
      globalProb : Expr -- Type Float (range 0 to 1)
    }

  sem pprintInferMethod indent env =
  | LightweightMCMC t ->
    let i = pprintIncr indent in
    match pprintCode i env t.keepSample with (env, keepSample) in
    match pprintCode i env t.continue with (env, continue) in
    match pprintCode i env t.globalProb with (env, globalProb) in
    (env, join ["(LightweightMCMC {keepSample = ", keepSample,
                ", continue = ", continue,
                ", globalProb = ", globalProb, "})"])

  sem inferMethodFromCon info bindings =
  | "LightweightMCMC" ->
    let expectedFields =
      [ ( "continue"
        , utuple_
          [ int_ defaultArgs.particles
          , ulam_ "remaining"
            (utuple_ [subi_ (var_ "remaining") (int_ 1), eqi_ (var_ "remaining") (int_ 0)])
          ]
        )
      , ("keepSample", ulam_ "" true_)
      , ("globalProb", float_ defaultArgs.mcmcLightweightGlobalProb)
      ] in
    match getFields info bindings expectedFields
    with [continue, keepSample, globalProb] in
    LightweightMCMC {
      continue = continue, keepSample = keepSample, globalProb = globalProb
    }

  sem inferMethodFromOptions options =
  | "mcmc-lightweight" ->
    LightweightMCMC {
      -- Reusing particles option for now for iterations, maybe we need a
      -- better name
      continue = utuple_
        [ int_ defaultArgs.particles
        , ulam_ "remaining"
          (utuple_ [subi_ (var_ "remaining") (int_ 1), eqi_ (var_ "remaining") (int_ 0)])
        ],
      keepSample = ulam_ "" true_,
      globalProb = float_ options.mcmcLightweightGlobalProb
    }

  sem inferMethodConfig info =
  | LightweightMCMC t ->
    fieldsToRecord info [
      ("continue", t.continue),
      ("keepSample", t.keepSample),
      ("globalProb", t.globalProb)
    ]

  sem typeCheckInferMethod env info sampleType =
  | LightweightMCMC t ->
    let bool = TyBool {info = info} in
    let float = TyFloat {info = info} in
    let continueState = newmonovar env.currentLvl info in
    let continue = typeCheckExpr env t.continue in
    let continueType = tytuple_
      [ continueState
      , tyarrows_ [continueState, sampleType, tytuple_ [continueState, bool]]
      ] in
    unify env [info, infoTm continue] continueType (tyTm continue);
    let keepSample = typeCheckExpr env t.keepSample in
    unify env [info, infoTm keepSample] (tyarrow_ tyint_ bool) (tyTm keepSample);
    let globalProb = typeCheckExpr env t.globalProb in
    unify env [info, infoTm globalProb] float (tyTm globalProb);
    LightweightMCMC {
      continue = continue,
      keepSample = keepSample,
      globalProb = globalProb
    }

  sem smapAccumL_InferMethod_Expr f acc =
  | LightweightMCMC r ->
    match f acc r.continue with (acc, continue) in
    match f acc r.keepSample with (acc, keepSample) in
    match f acc r.globalProb with (acc, globalProb) in
    (acc,
     LightweightMCMC {r with continue = continue, keepSample = keepSample, globalProb = globalProb})

  sem setRuns expr =
  | LightweightMCMC r -> LightweightMCMC {r with continue = utuple_
    [ expr
    , ulam_ "remaining"
      (utuple_ [subi_ (var_ "remaining") (int_ 1), eqi_ (var_ "remaining") (int_ 0)])
    ] }
end
