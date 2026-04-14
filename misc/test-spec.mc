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
  let and = lam l. lam r. lam x. if l x then r x else false in
  let or = lam l. lam r. lam x. if l x then true else r x in
  let elem = lam elems.
    let set = setOfSeq cmpString elems in
    lam x. setMem x set in
  let dirIs = lam dir. lam path. eqString dir (dirname path) in

  -- === Test MCore files ===

  let miCompile = api.midStep
    { tag = "miCompile"
    , uses = []
    , cmd = "mi compile --test %f --output %o"
    } in
  let miRun = api.endStep
    { tag = "miRun"
    , uses = [miCompile]
    , cmd = "%pcommand %i"
    } in

  api.tests []
    (and (strStartsWith "coreppl/src/") (strEndsWith ".mc"))
    [(miCompile, Succ ()), (miRun, Succ ())];

  -- NOTE(vipa, 2026-04-14): Skip the compiler itself, it's handled
  -- indirectly when doing other tests
  api.tests []
    (eqString "coreppl/src/cppl.mc")
    [(miCompile, Dont ())];

  -- NOTE(vipa, 2026-04-14): Runtime files cannot be compiled/executed
  -- standalone
  api.tests []
    (or
      (eqString "coreppl/src/coreppl-to-mexpr/top.mc")
      (lam p. strStartsWith "runtime" (basename p)))
    [(miCompile, Dont ())];

  api.tests []
    (and (strStartsWith "coreppl/test/coreppl-to-mexpr/cli/") (strEndsWith ".mc"))
    [(miCompile, Succ ()), (miRun, Succ ())];

  -- NOTE(vipa, 2026-04-14): This is a somewhat cheating way to make
  -- sure the (very) long inference tests aren't enabled by default,
  -- but can be turned on.
  let slow = api.dependency (lam. DepUnavailable ()) in
  api.tests [slow]
    (and (strStartsWith "coreppl/test/coreppl-to-mexpr/inference-accuracy/") (strEndsWith ".mc"))
    [(miCompile, Succ ()), (miRun, Succ ())];

  api.tests []
    (and (strStartsWith "coreppl/test/coreppl-to-mexpr/static-delay/") (strEndsWith ".mc"))
    [(miCompile, Succ ()), (miRun, Succ ())];

  -- NOTE(vipa, 2026-04-14): This file isn't written to type-check yet
  api.tests []
    (eqString "coreppl/src/pgm.mc")
    [(miCompile, Fail ())];

  -- === Test CorePPL files ===

  let cpplCompile = api.midStep
    { tag = "cpplCompile"
    , uses = []
    , cmd = "%c --seed 0 --test %f --output %o"
    } in
  let cpplRun = api.endStep
    { tag = "cpplRun"
    , uses = [cpplCompile]
    , cmd = "%pcommand %i"
    } in

  api.tests []
    (and (strStartsWith "coreppl/test/coreppl-to-mexpr/infer/") (strEndsWith ".mc"))
    [(cpplCompile, Succ ()), (cpplRun, Succ ())];

  api.tests []
    (and (strStartsWith "coreppl/test/coreppl-to-mexpr/expectation/") (strEndsWith ".mc"))
    [(cpplCompile, Succ ()), (cpplRun, Succ ())];

  api.tests []
    (and (strStartsWith "coreppl/test/coreppl-to-mexpr/pruning/") (strEndsWith ".mc"))
    [(cpplCompile, Succ ()), (cpplRun, Succ ())];

  -- === Test DPPL files ===

  let cdpplCompile = api.midStep
    { tag = "cdpplCompile"
    , uses = []
    , cmd = "%c --seed 0 --test --dppl-typecheck %f --output %o"
    } in
  let cdpplRun = api.endStep
    { tag = "cdpplRun"
    , uses = [cdpplCompile]
    , cmd = "command %i"
    } in

  api.tests []
    (and (strStartsWith "coreppl/test/coreppl-to-mexpr/dppl/") (strEndsWith ".mc"))
    [(cdpplCompile, Succ ()), (cdpplRun, Succ ())];

  -- TODO(vipa, 2026-04-14): running plot.py?
  -- NOTE(vipa, 2026-04-14): dppl examples
  let dpplExampleCompile = api.midStep
    { tag = "dpplExampleCompile"
    , uses = []
    , cmd = "%c --seed 1 --test --dppl-typecheck %f --output %o"
    } in
  let dpplExampleJson = api.midStep
    { tag = "json"
    , uses = [dpplExampleCompile]
    , cmd = "command %i %o"
    } in

  api.tests []
    (and (strStartsWith "coreppl/test/coreppl-to-mexpr/dppl/examples/") (strEndsWith ".mc"))
    [(cdpplCompile, Dont ()), (cdpplRun, Dont ()), (dpplExampleCompile, Succ ()), (dpplExampleJson, Succ ())];

  -- NOTE(vipa, 2026-04-14): This file is different, it doesn't
  -- actually produce a json file, so don't run it as though it would
  api.tests []
    (eqString "coreppl/test/coreppl-to-mexpr/dppl/examples/rode.mc")
    [(dpplExampleJson, Dont ())];

  ()
);

()
