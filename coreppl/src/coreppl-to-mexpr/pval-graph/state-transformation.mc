-- This file provides a transformation from the output of
-- ./idealized-transformation.mc to the real PVal graph interface.

include "idealized-transformation.mc"

let _idx0 = stringToSid "0"
let _idx1 = stringToSid "1"

lang PValStateTransformation = TempLamAst + AutoTyRecord + IdealizedPValTransformation
  type PValTransEnv =
    { currStateName : Name
    , functions : Map Name Bool -- True if it needs (and returns) a state argument
    , p_pure : Expr
    , p_map : Expr
    , p_subMap : Expr
    , p_apply : Expr
    , p_subApply : Expr
    , p_bind : Expr
    , p_join : Expr
    , p_assume : Expr
    , p_select : Expr
    , p_weight : Expr
    , p_export : Expr
    , p_traverseSeq : Expr
    , storeAssume : Expr
    , storeSubmodel : Expr
    , storeExport : Expr
    , storeWeight : Expr
    , initSubmodel : Expr
    , mapAccumL : Expr
    }

  type Peeled e = (Expr -> Expr, e)

  sem pvalTrans : PValTransEnv -> Expr -> Expr
  sem pvalTrans env = | tm ->
    match pvalTransExprNoSub env tm with (wrap, tm) in
    let st = match tm with TmRecord x
      then mapFindExn _idx0 x.bindings
      else tupleproj_ 0 tm in
    wrap st

  sem pvalTransCall : PValTransEnv -> (Expr, [Expr]) -> (Option Int, Peeled Expr)
  sem pvalTransCall env =
  | (TmConst {val = CPPure _}, [val]) ->
    match pvalTransPeelNoSub env val with (stateName, (wrap, val)) in
    (None (), (wrap, autoty_tuple_ [nvar_ stateName, app_ env.p_pure val]))
  | (TmConst {val = CPMap _}, [f, val]) ->
    match peelLambdas f with (numLams, (wrapLams, body)) in
    match pvalTransPeelNoSub env val with (stateName, (wrapV, val)) in
    let innerStateName = nameSym "st" in
    match pvalTransExprNoSub {env with currStateName = innerStateName} body with (wrap1, body) in
    match peelState body with (innerName, (wrap2, bodyPeeled)) in
    if nameEq innerStateName innerName then
      -- NOTE(vipa, 2026-01-12): The body doesn't create any new
      -- nodes, thus we can use p_map
      (None (), (wrapV, appf3_ env.p_map (nvar_ stateName) (wrapLams (wrap1 (wrap2 bodyPeeled))) val))
    else
      -- NOTE(vipa, 2026-01-12): The body does create new nodes, thus
      -- we need p_subMap or p_subApply
      let store = env.storeSubmodel in
      let ist = env.initSubmodel in
      let f = wrapLams (nulam_ innerStateName (wrap1 body)) in
      if eqi numLams 1
      then (None (), (wrapV, appf5_ env.p_subMap (nvar_ stateName) store ist f val))
      else (Some (subi numLams 1), (wrapV, appf3_ env.p_map (nvar_ stateName) f val))
  | (TmConst {val = CPTraverseSeq _}, [f, val]) ->
    match pvalTransPeelNoSub env val with (stateName, (wrap, val)) in
    let f = maybeEtaExpand f in
    let innerStateName = nameSym "st" in
    match pvalTransExprNoSub {env with currStateName = innerStateName} f.body with (wrapB, body) in
    let f = nulam_ innerStateName (TmLam {f with body = wrapB body}) in
    (None (), (wrap, appf3_ env.p_traverseSeq (nvar_ stateName) f val))
  | (TmConst {val = CPApply _}, [f, val]) ->
    match pvalTransPeel env f with (sub, stateName, (wrapF, f)) in
    match pvalTransPeelNoSub {env with currStateName = stateName} val with (stateName, (wrapVal, val)) in
    let wrap = lam tm. wrapF (wrapVal tm) in
    match sub with Some 1 then
      let store = env.storeSubmodel in
      let ist = env.initSubmodel in
      (None (), (wrap, appf5_ env.p_subApply (nvar_ stateName) store ist f val))
    else
      (optionMap (lam x. subi x 1) sub, (wrap, appf3_ env.p_apply (nvar_ stateName) f val))
  | ( TmConst {val = CPJoin _}
    , [ TmApp
        { lhs = TmApp
          { lhs = TmConst {val = CPMap _}
          , rhs = f
          }
        , rhs = val
        }
      ]
    ) ->
    let f = maybeEtaExpand f in
    match pvalTransPeelNoSub env val with (stateName, (wrapV, val)) in
    let innerStateName = nameSym "st" in
    match pvalTransExprNoSub {env with currStateName = innerStateName} f.body with (wrap1, body) in
    match peelState body with (innerName, (wrap2, bodyPeeled)) in
    if nameEq innerStateName innerName then
      -- NOTE(vipa, 2025-11-19): The body doesn't create any new
      -- nodes, thus we can use p_select
      (None (), (wrapV, appf3_ env.p_select (nvar_ stateName) (TmLam {f with body = wrap1 (wrap2 bodyPeeled)}) val))
    else
      -- NOTE(vipa, 2025-11-19): The body *does* create new nodes,
      -- thus we need p_bind
      let store = env.storeSubmodel in
      let ist = env.initSubmodel in
      let f = nulam_ innerStateName (TmLam {f with body = wrap1 body}) in
      (None (), (wrapV, appf5_ env.p_bind (nvar_ stateName) store ist f val))
  | (TmConst {val = CPJoin _}, [val]) ->
    match pvalTransPeelNoSub env val with (stateName, (wrap, val)) in
    (None (), (wrap, appf2_ env.p_join (nvar_ stateName) val))
  | (TmConst {val = CPAssume _}, [dist]) ->
    match pvalTransPeelNoSub env dist with (stateName, (wrap, dist)) in
    let store = env.storeAssume in
    (None (), (wrap, appf3_ env.p_assume (nvar_ stateName) store dist))
  | ( TmConst {val = CPWeight _}
    , [ TmApp
        { lhs = TmApp
          { lhs = TmConst {val = CPMap _}
          , rhs = f
          }
        , rhs = val
        }
      ]
    ) ->
    match pvalTransPeelNoSub env val with (stateName, (wrap, val)) in
    let store = env.storeWeight in
    (None (), (wrap, autoty_tuple_ [appf4_ env.p_weight (nvar_ stateName) store f val, unit_]))
  | (TmConst {val = CPWeight _}, [weight]) ->
    match pvalTransPeelNoSub env weight with (stateName, (wrap, weight)) in
    let store = env.storeWeight in
    -- TODO(vipa, 2025-11-20): Build TmLam identity function
    (None (), (wrap, autoty_tuple_ [appf4_ env.p_weight (nvar_ stateName) store (tempLam_ (lam tm. tm)) weight, unit_]))
  | (f & TmVar {ident = ident}, args) ->
    match pvalTransSeqNoSub env args with (stateName, (wrap, args)) in
    if mapLookupOr false ident env.functions
    then (None (), (wrap, appSeq_ f (cons (nvar_ stateName) args)))
    else (None (), (wrap, autoty_tuple_ [nvar_ stateName, appSeq_ f args]))
  | (c & TmConst {val = CMap _}, [f, val]) ->
    match pvalTransPeelNoSub env val with (stateName, (wrapV, val)) in
    let f = maybeEtaExpand f in
    let innerStateName = nameSym "st" in
    match pvalTransExprNoSub {env with currStateName = innerStateName} f.body with (wrap1, body) in
    match peelState body with (innerName, (wrap2, bodyPeeled)) in
    if nameEq innerStateName innerName then
      -- NOTE(vipa, 2025-12-02): The body doesn't create any new
      -- nodes, stay with `CMap`
      (None (), (wrapV, autoty_tuple_ [nvar_ stateName, appf2_ c (TmLam {f with body = wrap1 (wrap2 bodyPeeled)}) val]))
    else
      -- NOTE(vipa, 2025-12-02): The body *does* create new nodes,
      -- switch to mapAccumL
      let f = nulam_ innerStateName (TmLam {f with body = wrap1 body}) in
      (None (), (wrapV, appf3_ env.mapAccumL f (nvar_ stateName) val))
  | (TmConst {val = CPExport _}, [val]) ->
    match pvalTransPeelNoSub env val with (stateName, (wrapV, val)) in
    (None (), (wrapV, autoty_tuple_ [appf3_ env.p_export (nvar_ stateName) env.storeExport val, unit_]))
  | (f & TmConst _, args) ->
    match pvalTransSeqNoSub env args with (stateName, (wrap, args)) in
    (None (), (wrap, autoty_tuple_ [nvar_ stateName, appSeq_ f args]))
  | (f, args) -> errorSingle [infoTm f] (concat "Missing case for pvalTransCall: " (expr2str (appSeq_ f args)))

  sem pvalTransDecl : PValTransEnv -> Decl -> (PValTransEnv, Expr -> Expr)
  sem pvalTransDecl env =
  | DeclLet (x & {body = TmLam _}) ->
    match peelLambdas x.body with (_, (wrapLams, body)) in
    let innerStateName = nameSym "st" in
    match pvalTransExprNoSub {env with currStateName = innerStateName} body with (wrap1, body) in
    match peelState body with (innerStateName2, (wrap2, peeledBody)) in
    if nameEq innerStateName innerStateName2
    then (env, bind_ (DeclLet {x with body = wrapLams (wrap1 (wrap2 peeledBody))}))
    else
      ( {env with functions = mapInsert x.ident true env.functions}
      , bind_ (DeclLet {x with body = nulam_ innerStateName (wrapLams (wrap1 body))})
      )
  | DeclLet x ->
    match pvalTransExprNoSub env x.body with (wrap1, body) in
    match _peelState (Some x.ident) body with (stateName, (wrap2, body)) in
    let alreadyBound = match body with TmVar v
      then nameEq v.ident x.ident
      else false in
    if alreadyBound then
      ({env with currStateName = stateName}, lam tm. wrap1 (wrap2 tm))
    else
      ({env with currStateName = stateName}, lam tm. wrap1 (wrap2 (bind_ (DeclLet {x with body = body}) tm)))
  | DeclRecLets x ->
    -- OPT(vipa, 2025-11-25): This implementation is simple (requiring
    -- no additional analysis to determine the statefulness ahead of
    -- time) but inefficient; it may retransform the same function
    -- many times.
    let initialGuess = foldl (lam acc. lam b. mapInsert b.ident false acc) (mapEmpty nameCmp) x.bindings in
    let mkNewGuess : PValTransEnv -> (Map Name Bool, [DeclLetRecord]) = lam env.
      let f = lam acc. lam binding.
        match peelLambdas binding.body with (_, (wrapLams, body)) in
        let st = nameSym "st" in
        match pvalTransExprNoSub {env with currStateName = st} body with (wrap1, body) in
        match peelState body with (newSt, (wrap2, bodyPeeled)) in
        if nameEq st newSt then
          ( mapInsert binding.ident false acc
          , {binding with body = wrapLams (wrap1 (wrap2 bodyPeeled))}
          )
        else
          ( mapInsert binding.ident true acc
          , {binding with body = nulam_ st (wrapLams (wrap1 body))}
          )
      in mapAccumL f (mapEmpty nameCmp) x.bindings in
    recursive let repeatUntilValid = lam guess.
      let guessEnv = {env with functions = mapUnion env.functions guess} in
      match mkNewGuess guessEnv with (newGuess, bindings) in
      if mapEq (lam a. lam b. if a then b else not b) guess newGuess
      then (guessEnv, bindings)
      else repeatUntilValid newGuess in
    match repeatUntilValid initialGuess with (env, bindings) in
    (env, bind_ (DeclRecLets {x with bindings = bindings}))
  | decl & (DeclType _ | DeclConDef _ | DeclExt _) -> (env, bind_ decl)
  | decl -> errorSingle [infoDecl decl] (concat "Missing case for pvalTransDecl: " (decl2str decl))

  sem pvalTransPeel : PValTransEnv -> Expr -> (Option Int, Name, Peeled Expr)
  sem pvalTransPeel env = | tm ->
    match pvalTransExpr env tm with (sub, (wrap, tm)) in
    match peelState tm with (st, (wrap2, tm)) in
    (sub, st, (lam tm. wrap (wrap2 tm), tm))

  sem pvalTransPeelNoSub : PValTransEnv -> Expr -> (Name, Peeled Expr)
  sem pvalTransPeelNoSub env = | tm ->
    match pvalTransPeel env tm with (None _, st, tm) then (st, tm) else
    errorSingle [infoTm tm] "Compiler error: expected all sub-models to be dischanged here, but they were not (in pvalTransPeelNoSub)"

  sem pvalTransSeqNoSub : PValTransEnv -> [Expr] -> (Name, Peeled [Expr])
  sem pvalTransSeqNoSub env = | seq ->
    let f = lam acc. lam tm.
      match pvalTransExprNoSub {env with currStateName = acc.st} tm with (wrap, tm) in
      match peelState tm with (st, (wrap2, tm)) in
      ({st = st, wraps = concat [wrap2, wrap] acc.wraps}, tm) in
    match mapAccumL f {st = env.currStateName, wraps = []} seq with ({st = st, wraps = wraps}, tms) in
    (st, (lam tm. foldl (lam acc. lam wrap. wrap acc) tm wraps, tms))

  sem pvalTransExprNoSub : PValTransEnv -> Expr -> Peeled Expr
  sem pvalTransExprNoSub env = | tm ->
    match pvalTransExpr env tm with (None _, ret) then ret else
    errorSingle [infoTm tm] "Compiler error: expected all sub-models to be dischanged here, but they were not (in pvalTransExprNoSub)"

  sem pvalTransExpr : PValTransEnv -> Expr -> (Option Int, Peeled Expr)
  sem pvalTransExpr env =
  | TmDecl x ->
    match pvalTransDecl env x.decl with (env, wrap) in
    match pvalTransExpr env x.inexpr with (sub, (wrapi, tm)) in
    (sub, (lam tm. wrap (wrapi tm), tm))
  | TmApp x ->
    recursive let work = lam f. lam env. lam args.
      switch f
      case TmApp x then
        work x.lhs env (cons x.rhs args)
      case TmDecl x then
        match pvalTransDecl env x.decl with (env, wrap) in
        match work x.inexpr env args with (sub, (wrapi, tm)) in
        (sub, (lam tm. wrap (wrapi tm), tm))
      case f then
        pvalTransCall env (f, args)
      end
    in work x.lhs env [x.rhs]
  | tm & TmLam _ -> errorSingle [infoTm tm] (concat "Encountered unbound lambda in final graph transformation: " (expr2str tm))
  | tm & (TempLam _ | TempFix _) -> errorSingle [infoTm tm] (concat "Encountered unbound templam/fix in final graph transformation: " (expr2str tm))
  | TmMatch (x & {els = TmNever _}) ->
    match pvalTransExprNoSub env x.target with (wrap1, target) in
    match peelState target with (stateName, (wrap2, target)) in
    match pvalTransExprNoSub {env with currStateName = stateName} x.thn with (wrapT, thn) in
    (None (), (lam tm. wrap1 (wrap2 (TmMatch {x with target = target, thn = wrapT tm})), thn))
  | TmMatch x ->
    match pvalTransExprNoSub env x.target with (wrap1, target) in
    match peelState target with (stateName, (wrap2, target)) in
    match pvalTransExprNoSub {env with currStateName = stateName} x.thn with (wrapT1, thn) in
    match peelState thn with (stateThn, (wrapT2, thnPeeled)) in
    match pvalTransExprNoSub {env with currStateName = stateName} x.els with (wrapE1, els) in
    match peelState els with (stateEls, (wrapE2, elsPeeled)) in
    if and (nameEq stateName stateThn) (nameEq stateName stateEls) then
      ( None ()
      , ( lam tm. wrap1 (wrap2 tm)
        , autoty_tuple_
          [ nvar_ stateName
          , TmMatch {x with target = target, thn = wrapT1 (wrapT2 thnPeeled), els = wrapE1 (wrapE2 elsPeeled)}
          ]
        )
      )
    else
      ( None ()
      , ( lam tm. wrap1 (wrap2 tm)
        , TmMatch {x with target = target, thn = wrapT1 thn, els = wrapE1 els}
        )
      )
  | tm & TmOpaque _ ->
    ( None ()
    , (lam tm. tm, autoty_tuple_ [nvar_ env.currStateName, tm])
    )
  | tm ->
    let f = lam acc. lam tm.
      match pvalTransExprNoSub acc.env tm with (wrap1, val) in
      match peelState val with (stateName, (wrap2, val)) in
      ({env = {acc.env with currStateName = stateName}, wraps = concat [wrap2, wrap1] acc.wraps}, val) in
    match smapAccumL_Expr_Expr f {env = env, wraps = []} tm with ({env = env, wraps = wraps}, tm) in
    ( None ()
    , ( lam tm. foldl (lam tm. lam wrap. wrap tm) tm wraps
      , autoty_tuple_ [nvar_ env.currStateName, tm]
      )
    )

  sem peelLambdas : Expr -> (Int, Peeled Expr)
  sem peelLambdas =
  | TmLam x ->
    match peelLambdas x.body with (count, (wrap, body)) in
    (addi count 1, (lam tm. TmLam {x with body = wrap tm}, body))
  | tm & TempLam _ ->
    peelLambdas (TmLam (maybeEtaExpand tm))
  -- NOTE(vipa, 2026-05-27): This case could technically look for any
  -- non-side-effecting rhs of a let, but for now we only need it to
  -- support renamings, so that's what we detect
  | TmDecl (x & {decl = DeclLet {body = TmVar _}}) ->
    match peelLambdas x.inexpr with (count, (wrap, body)) in
    (count, (lam tm. TmDecl {x with inexpr = wrap tm}, body))
  | tm -> (0, (lam tm. tm, tm))

  sem peelState : Expr -> (Name, Peeled Expr)
  sem peelState = | tm -> _peelState (None ()) tm

  sem _peelState : Option Name -> Expr -> (Name, Peeled Expr)
  sem _peelState defaultValName =
  | tm & TmRecord x ->
    -- NOTE(vipa, 2025-11-19): We know that pvalTransExpr, when given
    -- a value of type `a`, will produce a value of type `(PValState
    -- st, a)` for some `st`.
    match mapLookup _idx1 x.bindings with Some expr in
    switch mapLookup _idx0 x.bindings
    case Some (TmVar {ident = stIdent}) then
      (stIdent, (lam x. x, expr))
    case Some stTm then
      let stIdent = nameSym "st" in
      (stIdent, (bind_ (nulet_ stIdent stTm), expr))
    case None _ then
      errorSingle [x.info] (concat "Unexpected None in _peelState: " (expr2str tm))
    end
  | tm ->
    let stIdent = nameSym "st" in
    let valIdent = optionGetOrElse (lam. nameSym "x") defaultValName in
    let wrap = lam body.
      -- TODO(vipa, 2025-11-19): put some types here
      match_ tm (withTypePat (tytuple_ [tyunknown_, tyTm tm]) (ptuple_ [npvar_ stIdent, npvar_ valIdent]))
        body
        never_ in
    (stIdent, (wrap, nvar_ valIdent))
end

lang TestLang = DPPLParser + MExprLowerNestedPatterns + InlineSingleUse + PValStateTransformation
end

mexpr

use TestLang in

let debugStuff = ref false in

let transform = lam strs.
  let args =
    { _defaultBootParserParseMExprStringArg ()
    with allowFree = true
    , keywords = pplKeywords
    , builtin = cpplBuiltin
    } in
  let ast = parseMExprStringExn args (strJoin "\n" strs) in
  let ast = use DPPLParser in makeKeywords ast in
  let ast = symbolizeExpr symEnvDefault ast in
  let ast = typeCheck ast in
  (if deref debugStuff then
    printLn (expr2str ast)
   else ());
  let ast = lowerAll ast in
  (if deref debugStuff then
    printLn (expr2str ast)
   else ());
  let ast = inlineSingleUseLets ast in
  (if deref debugStuff then
    printLn (expr2str ast)
   else ());
  let initState =
    { specializations = mapEmpty nameCmp
    } in
  let initScope =
    { functionDefinitions = mapEmpty nameCmp
    , valueScope = mapEmpty nameCmp
    , revValueScope = mapEmpty nameCmp
    , conScope = mapEmpty nameCmp
    , depth = 0
    , tyConAsPure = mapEmpty nameCmp
    } in
  match specializeExpr initScope initState ast with (_, (ast, _)) in
  let initTransEnv =
    { currStateName = nameSym "st"
    , functions = mapEmpty nameCmp
    , p_pure = nvar_ (nameSym "p_pure")
    , p_map = nvar_ (nameSym "p_map")
    , p_subMap = nvar_ (nameSym "p_subMap")
    , p_apply = nvar_ (nameSym "p_apply")
    , p_subApply = nvar_ (nameSym "p_subApply")
    , p_bind = nvar_ (nameSym "p_bind")
    , p_join = nvar_ (nameSym "p_join")
    , p_assume = nvar_ (nameSym "p_assume")
    , p_select = nvar_ (nameSym "p_select")
    , p_weight = nvar_ (nameSym "p_weight")
    , p_export = nvar_ (nameSym "p_export")
    , p_traverseSeq = nvar_ (nameSym "p_traverseSeq")
    , mapAccumL = nvar_ (nameSym "mapAccumL")
    , storeAssume = nvar_ (nameSym "simpleStoreAssume")
    , storeSubmodel = nvar_ (nameSym "simpleStoreSubmodel")
    , storeExport = nvar_ (nameSym "simpleStoreExport")
    , storeWeight = nvar_ (nameSym "simpleStoreWeight")
    , initSubmodel = app_ (nvar_ (nameSym "simpleInit")) unit_
    } in
  match pvalTransExprNoSub initTransEnv ast with (wrap, tm) in
  (pprintCode 0 pprintEnvEmpty (wrap tm)).1 in

let printFailure = lam l. lam r. strJoin "\n"
  [ concat "    LHS: " (strReplace "\n" "\n         " l)
  , concat "    RHS: " (strReplace "\n" "\n         " r)
  ] in

utest transform
  [ "weight (addf (assume (Gaussian 0.0 1.0)) 2.0)"
  ]
with strJoin "\n"
  [ "match"
  , "  p_assume st simpleStoreAssume (p_pure (Gaussian 0. 1.))"
  , "with"
  , "  (st1, x)"
  , "in"
  , "(p_weight st1 simpleStoreWeight (/-temp-/lam x1."
  , "     addf x1 2.) x, {})"
  ]
using eqString
else printFailure
in

utest transform
  [ "let a = 2.0 in"
  , "weight (addf (assume (Gaussian 0.0 1.0)) (addf a a))"
  ]
with strJoin "\n"
  [ "match"
  , "  p_assume st simpleStoreAssume (p_pure (Gaussian 0. 1.))"
  , "with"
  , "  (st1, x)"
  , "in"
  , "(p_weight"
  , "  st1"
  , "  simpleStoreWeight"
  , "  (/-temp-/lam x1."
  , "     addf x1 (addf 2. 2.))"
  , "  x, {})"
  ]
using eqString
else printFailure
in

utest transform
  [ "let a = assume (Gaussian 0.0 1.0) in"
  , "weight (addf a a)"
  ]
with strJoin "\n"
  [ "match"
  , "  p_assume st simpleStoreAssume (p_pure (Gaussian 0. 1.))"
  , "with"
  , "  (st1, a)"
  , "in"
  , "match"
  , "  p_map st1 (lam x."
  , "       lam x1."
  , "         addf x x1) a"
  , "with"
  , "  (st2, x2)"
  , "in"
  , "match p_apply st2 x2 a with (st3, x3)"
  , "in"
  , "(p_weight st3 simpleStoreWeight (/-temp-/lam x4."
  , "     x4) x3, {})"
  ]
using eqString
else printFailure
in

utest transform
  [ "let f = lam x. addf 1.0 x in"
  , "addf (f 1.0) (f 2.0)"
  ]
with strJoin "\n"
  [ "let f = lam x."
  , "    addf 1. x in"
  , "(st, addf (f 1.) (f 2.))"
  ]
using eqString
else printFailure
in

utest transform
  [ "let f = lam x. addf (assume (Gaussian 1.0 0.0)) x in"
  , "addf (f 1.0) (f 2.0)"
  ]
with strJoin "\n"
  [ "let f ="
  , "  lam st4."
  , "    lam x5."
  , "      match"
  , "        p_assume st4 simpleStoreAssume (p_pure (Gaussian 1. 0.))"
  , "      with"
  , "        (st5, x6)"
  , "      in"
  , "      p_map st5 (lam x7."
  , "           addf x7 x5) x6"
  , "in"
  , "match f st 1. with (st1, x)"
  , "in"
  , "match"
  , "  p_map st1 (lam x1."
  , "       lam x2."
  , "         addf x1 x2) x"
  , "with"
  , "  (st2, x3)"
  , "in"
  , "match f st2 2. with (st3, x4)"
  , "in"
  , "p_apply st3 x3 x4"
  ]
using eqString
else printFailure
in

-- TODO(vipa, 2025-11-25): This doesn't generate what I want right
-- now, but there's an OPT comment in the idealized transformation
-- about it
utest transform
  [ "let draw = lam x. assume (Gaussian x 1.0) in"
  , "get [draw 0.0, draw 1.0] (assume (Categorical [0.5, 0.5]))"
  ]
with strJoin "\n"
  [ "let draw ="
  , "  lam st6."
  , "    lam x7."
  , "      p_assume st6 simpleStoreAssume (p_pure (Gaussian x7 1.))"
  , "in"
  , "match draw st 0. with (st1, x)"
  , "in"
  , "match"
  , "  p_map"
  , "    st1"
  , "    (lam x1."
  , "       lam x2."
  , "         get [ x1,"
  , "             x2 ])"
  , "    x"
  , "with"
  , "  (st2, x3)"
  , "in"
  , "match draw st2 1. with (st3, x4)"
  , "in"
  , "match p_apply st3 x3 x4 with (st4, x5)"
  , "in"
  , "match"
  , "  p_assume st4 simpleStoreAssume (p_pure (Categorical [ 0.5, 0.5 ]))"
  , "with"
  , "  (st5, x6)"
  , "in"
  , "p_apply st5 x5 x6"
  ]
using eqString
else printFailure
in

utest transform
  [ "let draw = lam x. assume (Gaussian x 1.0) in"
  , "let a = draw 0.0 in"
  , "let b = draw 1.0 in"
  , "switch assume (Categorical [0.5, 0.5])"
  , "case 0 then a"
  , "case 1 then b"
  , "end"
  ]
with strJoin "\n"
  [ "let draw ="
  , "  lam st3."
  , "    lam x2."
  , "      p_assume st3 simpleStoreAssume (p_pure (Gaussian x2 1.))"
  , "in"
  , "match"
  , "  p_assume st simpleStoreAssume (p_pure (Categorical [ 0.5, 0.5 ]))"
  , "with"
  , "  (st1, #var\"X\")"
  , "in"
  , "p_bind"
  , "  st1"
  , "  simpleStoreSubmodel"
  , "  (simpleInit {})"
  , "  (lam st2."
  , "     lam x."
  , "       let x1 = x in"
  , "       match x1 with 0"
  , "       then"
  , "         draw st2 0."
  , "       else match x1 with 1"
  , "       in"
  , "       draw st2 1.)"
  , "  #var\"X\""
  ]
using eqString
else printFailure
in

utest transform
  [ "let draw = lam x. assume (Gaussian x 1.0) in"
  , "let a = draw 0.0 in"
  , "let b = draw 1.0 in"
  , "let z = addf a b in"
  , "switch assume (Categorical [0.5, 0.5])"
  , "case 0 then a"
  , "case 1 then b"
  , "end"
  ]
with strJoin "\n"
  [ "let draw ="
  , "  lam st6."
  , "    lam x5."
  , "      p_assume st6 simpleStoreAssume (p_pure (Gaussian x5 1.))"
  , "in"
  , "match draw st 0. with (st1, a)"
  , "in"
  , "match draw st1 1. with (st2, b)"
  , "in"
  , "match"
  , "  p_map st2 (lam x."
  , "       lam x1."
  , "         addf x x1) a"
  , "with"
  , "  (st3, x2)"
  , "in"
  , "match p_apply st3 x2 b with (st4, z)"
  , "in"
  , "match"
  , "  p_assume st4 simpleStoreAssume (p_pure (Categorical [ 0.5, 0.5 ]))"
  , "with"
  , "  (st5, #var\"X\")"
  , "in"
  , "p_select"
  , "  st5"
  , "  (lam x3."
  , "     let x4 = x3 in"
  , "     match x4 with 0"
  , "     then"
  , "       a"
  , "     else match x4 with 1"
  , "     in"
  , "     b)"
  , "  #var\"X\""
  ]
using eqString
else printFailure
in

utest transform
  [ "if assume (Bernoulli 0.5)"
  , "then [assume (Gaussian 0.0 1.0)]"
  , "else [2.0]"
  ]
with strJoin "\n"
  [ "match"
  , "  p_assume st simpleStoreAssume (p_pure (Bernoulli 0.5))"
  , "with"
  , "  (st1, x)"
  , "in"
  , "p_bind"
  , "  st1"
  , "  simpleStoreSubmodel"
  , "  (simpleInit {})"
  , "  (lam st2."
  , "     lam x1."
  , "       let x2 = x1 in"
  , "       match x2 with true"
  , "       then"
  , "         match"
  , "           p_assume st2 simpleStoreAssume (p_pure (Gaussian 0. 1.))"
  , "         with"
  , "           (st3, x3)"
  , "         in"
  , "         p_map st3 (lam x4."
  , "              [ x4 ]) x3"
  , "       else"
  , "         (st2, p_pure [ 2. ]))"
  , "  x"
  ]
using eqString
else printFailure
in

utest transform
  [ "recursive let mul = lam acc. lam n. lam i."
  , "   match i with 0 then acc else mul (addi acc n) n (subi i 1)"
  , "in mul 0 (addi 1 (assume (Categorical [0.25, 0.25, 0.25, 0.25]))) 4"
  ]
with strJoin "\n"

  [ "recursive"
  , "  let mul ="
  , "    lam st."
  , "      lam acc."
  , "        lam n."
  , "          lam i."
  , "            match i with 0"
  , "            then"
  , "              (st, p_pure acc)"
  , "            else match p_map st (lam x."
  , "                   addi acc x) n with (st1, x1)"
  , "            in"
  , "            mul1 st1 x1 n (subi i 1)"
  , "  let mul1 ="
  , "    lam st2."
  , "      lam acc1."
  , "        lam n1."
  , "          lam i1."
  , "            match i1 with 0"
  , "            then"
  , "              (st2, acc1)"
  , "            else match"
  , "              p_map"
  , "                st2"
  , "                (lam x2."
  , "                   lam x3."
  , "                     addi x2 x3)"
  , "                acc1"
  , "            with"
  , "              (st3, x4)"
  , "            in"
  , "            match p_apply st3 x4 n1 with (st4, x5)"
  , "            in"
  , "            mul1 st4 x5 n1 (subi i1 1)"
  , "in"
  , "match"
  , "  p_assume"
  , "    st5"
  , "    simpleStoreAssume"
  , "    (p_pure (Categorical [ 0.25, 0.25, 0.25, 0.25 ]))"
  , "with"
  , "  (st6, x6)"
  , "in"
  , "match p_map st6 (lam x7."
  , "       addi 1 x7) x6 with (st7, x8)"
  , "in"
  , "mul st7 0 x8 4"
  ]
using eqString
else printFailure
in

()
