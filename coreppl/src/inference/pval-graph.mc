include "../infer-method.mc"

lang SimplePValGraphMethod = InferMethodBase
  type SimplePValGraphConfig =
    { run : Expr -- Type described in coreppl-to-mexpr/pval-graph/config.mc
    }
  syn InferMethod =
  | SimplePValGraph SimplePValGraphConfig

  sem pprintInferMethod indent env =
  | SimplePValGraph x ->
    let i = pprintIncr indent in
    match pprintCode i env x.run with (env, run) in
    ( env
    , join
      [ "(SimplePValGraph "
      , "{ run = ", run
      , "})"
      ]
    )

  sem inferMethodFromCon info bindings =
  | "SimplePValGraph" ->
    let expectedFields =
      [ ("run", ulam_ "" (error_ (str_ "Inference error: SimplePValGraph got no run function")))
      ] in
    match getFields info bindings expectedFields with [run] in
    SimplePValGraph
    { run = run
    }

  sem inferMethodConfig info =
  | SimplePValGraph x -> fieldsToRecord info
    [ ("run", x.run)
    ]

  sem typeCheckInferMethod env info sampleType =
  | SimplePValGraph x ->
    let instanceName = nameSym "instance" in
    let tyinstance_ = ntyvar_ instanceName in
    let tyinterface_ = tyrecord_
      [ ( "instantiate"
        , tyarrow_ tyunit_ (tyrecord_ [("numAligned", tyint_), ("instance", tyinstance_)])
        )
      , ( "getWeight"
        , tyarrow_ tyinstance_ tyfloat_
        )
      , ( "getRet"
        , tyarrow_ tyinstance_ sampleType
        )
      , ( "step"
        , tyarrows_
          [ tyarrow_ tyfloat_ tybool_
          , tyseq_ tyint_
          , tyinstance_
          , tyrecord_ [("accept", tybool_), ("instance", tyinstance_)]
          ]
        )
      ] in
    let tyret_ = tyrecord_
      [ ("smc", tyseq_ (tytuple_ [tyfloat_, sampleType]))
      , ("mcmc", tyseq_ (tytuple_ [tybool_, sampleType]))
      ] in
    let tyrun_ = ntyall_ instanceName (tyarrow_ tyinterface_ tyret_) in
    let run = typeCheckExpr env x.run in
    unify env [infoTm run] tyrun_ (tyTm run);
    SimplePValGraph {x with run = run}

  sem smapAccumL_InferMethod_Expr f acc =
  | SimplePValGraph r ->
    match f acc r.run with (acc, run) in
    (acc, SimplePValGraph {r with run = run})

  sem setRuns expr =
  | SimplePValGraph _ -> error "Compiler error: cannot set number of runs for SimplePValGraph"
end
