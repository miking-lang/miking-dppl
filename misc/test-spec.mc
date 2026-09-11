include "stdlib::test-spec.mc"

mexpr

use TestSpec in

let substituter : Substituter =
  { noSubstituter with substitutions = mapFromSeq cmpChar
    [ ( 'c'
      , { tup =
          { actual = "MCORE_LIBS=$MCORE_LIBS:coreppl=$(ROOT)/coreppl/src %<cppl>"
          , deps = ["$(ROOT)/coreppl/src/<cppl>"]
          }
        , make =
          { actual = "MCORE_LIBS=$$MCORE_LIBS:coreppl=$(ROOT)/coreppl/src $(ROOT)/build/cppl"
          , deps = ["build/cppl"]
          }
        , friendly = "CPPL"
        }
      )
    , ( 'p'
      , { tup =
          { actual = "MIDPPL_PATH=$(ROOT) MCORE_LIBS=$MCORE_LIBS:coreppl=$(ROOT)/coreppl/src CPPL=`realpath %<cppl>` "
          , deps = ["$(ROOT)/coreppl/src/<cppl>"]
          }
        , make =
          { actual = "MIDPPL_PATH=$(ROOT) MCORE_LIBS=$$MCORE_LIBS:coreppl=$(ROOT)/coreppl/src CPPL=$(ROOT)/build/cppl "
          , deps = []
          }
        , friendly = ""
        }
      )
    ]
  } in
let directories = ["coreppl"] in
let location = Some
  { src = "misc/test-spec.mc"
  , exe = "misc/test"
  } in
testMain [substituter] directories location (lam api.
  let succ = Succ () in
  let fail = Fail () in
  let dont = Dont () in
  let allOf = lam ps. lam x. forAll (lam p. p x) ps in
  let anyOf = lam ps. lam x. any (lam p. p x) ps in
  let not = lam p. lam x. if p x then false else true in
  let elem = lam elems.
    let set = setOfSeq cmpString elems in
    lam x. setMem x set in
  -- All files with the given extension in the given directory (or a
  -- subdirectory thereof).
  let filesIn = lam dir. lam ext. allOf [strStartsWith dir, strEndsWith ext] in

  -- === Test MCore files ===

  let miCompile = api.midStep
    { tag = "miCompile"
    , uses = []
    , cmd = "mi compile --test %f --output %o"
    } in
  let miRun = api.endStep
    { tag = "miRun"
    , uses = [miCompile]
    , cmd = "%p command %i"
    } in

  api.tests []
    (anyOf (map (lam dir. filesIn dir ".mc")
      [ "coreppl/src/"
      , "coreppl/test/coreppl-to-mexpr/cli/"
      , "coreppl/test/coreppl-to-mexpr/static-delay/"
      ]))
    [(miCompile, succ), (miRun, succ)];

  -- NOTE(vipa, 2026-04-14): Skip the compiler itself, it's handled
  -- indirectly when doing other tests. Runtime files (and the file
  -- gathering them) cannot be compiled/executed standalone.
  api.tests []
    (anyOf
      [ elem
        [ "coreppl/src/cppl.mc"
        , "coreppl/src/coreppl-to-mexpr/top.mc"
        ]
      , lam p. strStartsWith "runtime" (basename p)
      ])
    [(miCompile, dont)];

  -- NOTE(vipa, 2026-04-14): This file isn't written to type-check yet
  api.tests []
    (eqString "coreppl/src/pgm.mc")
    [(miCompile, fail)];

  -- NOTE(vipa, 2026-04-14): This is a somewhat cheating way to make
  -- sure the (very) long inference tests aren't enabled by default,
  -- but can be turned on.
  let slow = api.dependency (lam. DepUnavailable ()) in
  api.tests [slow]
    (filesIn "coreppl/test/coreppl-to-mexpr/inference-accuracy/" ".mc")
    [(miCompile, succ), (miRun, succ)];

  -- === Test CorePPL files ===

  let cpplCompile = api.midStep
    { tag = "cpplCompile"
    , uses = []
    , cmd = "%c --seed 0 --test %f --output %o"
    } in
  let cpplRun = api.endStep
    { tag = "cpplRun"
    , uses = [cpplCompile]
    , cmd = "%p command %i"
    } in
  let adCompile = api.midStep
    { tag = "adCompile"
    , uses = []
    , cmd = "%c --seed 0 --test --auto-diff %f --output %o"
    } in
  let adRun = api.endStep
    { tag = "adRun"
    , uses = [adCompile]
    , cmd = "%p command %i"
    } in

  let cpplTest = anyOf (map (lam dir. filesIn dir ".dppl")
    [ "coreppl/test/coreppl-to-mexpr/infer/"
    , "coreppl/test/coreppl-to-mexpr/expectation/"
    , "coreppl/test/coreppl-to-mexpr/pruning/"
    ]) in
  -- NOTE(vipa, 2026-04-14): These files use `diff`, so they need
  -- `--auto-diff`.
  let cpplUsesDiff = strStartsWith "coreppl/test/coreppl-to-mexpr/infer/diff-" in

  api.tests []
    (allOf [cpplTest, not cpplUsesDiff])
    [(cpplCompile, succ), (cpplRun, succ)];

  api.tests []
    (allOf [cpplTest, cpplUsesDiff])
    [(adCompile, succ), (adRun, succ)];

  -- === Test DPPL files ===

  let cdpplCompile = api.midStep
    { tag = "cdpplCompile"
    , uses = []
    , cmd = "%c --seed 1 --test --dppl-typecheck %f --output %o"
    } in
  let cdpplRun = api.endStep
    { tag = "cdpplRun"
    , uses = [cdpplCompile]
    , cmd = "command %i"
    } in
  let adDpplCompile = api.midStep
    { tag = "adDpplCompile"
    , uses = []
    , cmd = "%c --seed 1 --test --auto-diff --dppl-typecheck %f --output %o"
    } in
  let adDpplRun = api.endStep
    { tag = "adDpplRun"
    , uses = [adDpplCompile]
    , cmd = "command %i"
    } in

  -- NOTE(vipa, 2026-04-14): dppl examples, i.e., `*-run.dppl` files,
  -- produce plot-data (json) rather than running utests, but are
  -- compiled just like the files they wrap.
  let dpplExampleJson = api.midStep
    { tag = "json"
    , uses = [cdpplCompile]
    , cmd = "command %i %o"
    } in
  let adDpplExampleJson = api.midStep
    { tag = "adJson"
    , uses = [adDpplCompile]
    , cmd = "command %i %o"
    } in

  let dpplTest = filesIn "coreppl/test/coreppl-to-mexpr/dppl/" ".dppl" in
  let isExample = strEndsWith "-run.dppl" in
  -- NOTE(vipa, 2026-09-09): These files use `diff`, so they need
  -- `--auto-diff` in addition to `--dppl-typecheck`. The same goes for
  -- the `*-run.dppl` wrappers that include them.
  let dpplUsesDiff =
    let withWrapper = lam path. [concat path ".dppl", concat path "-run.dppl"] in
    elem (join (map withWrapper
      [ "coreppl/test/coreppl-to-mexpr/dppl/diff-test"
      , "coreppl/test/coreppl-to-mexpr/dppl/examples/bayesian-parameter-estimation-ivp-sensitivity"
      , "coreppl/test/coreppl-to-mexpr/dppl/examples/bayesian-parameter-estimation-ivp-sensitivity-trace"
      , "coreppl/test/coreppl-to-mexpr/dppl/examples/ode-sensitivites-two-methods"
      , "coreppl/test/coreppl-to-mexpr/dppl/examples/ode-sensitivites-two-methods-scalar"
      , "coreppl/test/coreppl-to-mexpr/dppl/examples/tumor-inhibitor-rode"
      ])) in

  api.tests []
    (allOf [dpplTest, not isExample, not dpplUsesDiff])
    [(cdpplCompile, succ), (cdpplRun, succ)];

  api.tests []
    (allOf [dpplTest, not isExample, dpplUsesDiff])
    [(adDpplCompile, succ), (adDpplRun, succ)];

  api.tests []
    (allOf [dpplTest, isExample, not dpplUsesDiff])
    [(cdpplCompile, succ), (dpplExampleJson, succ)];

  api.tests []
    (allOf [dpplTest, isExample, dpplUsesDiff])
    [(adDpplCompile, succ), (adDpplExampleJson, succ)];

  ()
);

()
