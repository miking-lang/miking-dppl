-- This file provides a transformation from the output of
-- ./idealized-transformation.mc to the real PVal graph interface.

include "idealized-transformation.mc"

let _idx0 = stringToSid "0"
let _idx1 = stringToSid "1"

lang PValStateTransformation = TempLamAst + AutoTyRecord + IdealizedPValTransformation
  type PValTransEnv =
    { currStateName : Name
    , functions : Map Name Bool -- True if it needs (and returns) a state argument
    , p_pure : Name
    , p_map : Name
    , p_subMap : Name
    , p_apply : Name
    , p_bind : Name
    , p_join : Name
    , p_assume : Name
    , p_select : Name
    , p_weight : Name
    , p_export : Name
    , p_traverseSeq : Name
    , storeAssume : Name
    , storeSubmodel : Name
    , storeExport : Name
    , storeWeight : Name
    , initSubmodel : Expr
    , mapAccumL : Name
    }

  type Peeled e = (Expr -> Expr, e)

  sem pvalTrans : PValTransEnv -> Expr -> Expr
  sem pvalTrans env = | tm ->
    match pvalTransExpr env tm with (wrap, tm) in
    let st = match tm with TmRecord x
      then mapFindExn _idx0 x.bindings
      else tupleproj_ 0 tm in
    nulam_ env.currStateName (wrap st)

  sem maybeEtaExpand : Expr -> {ident : Name, tyAnnot : Type, tyParam : Type, body : Expr, ty : Type, info : Info}
  sem maybeEtaExpand =
  | TmLam f -> f
  | tm ->
    let n = nameSym "x" in
    { ident = n
    , tyAnnot = tyunknown_
    , tyParam = tyunknown_
    , body = _app tm (nvar_ n)
    , ty = tyTm tm
    , info = infoTm tm
    }

  sem pvalTransCall : PValTransEnv -> (Expr, [Expr]) -> Peeled Expr
  sem pvalTransCall env =
  | (TmConst {val = CPPure _}, args) ->
    match pvalTransSeq env args with (stateName, (wrap, [val])) in
    (wrap, autoty_tuple_ [nvar_ stateName, app_ (nvar_ env.p_pure) val])
  | (TmConst {val = CPMap _}, [f, val]) ->
    match peelLambdas f with (wrapLams, body) in
    match pvalTransSeq env [val] with (stateName, (wrapV, [val])) in
    let innerStateName = nameSym "st" in
    match pvalTransExpr {env with currStateName = innerStateName} body with (wrap1, body) in
    match peelState body with (innerName, (wrap2, bodyPeeled)) in
    if nameEq innerStateName innerName then
      -- NOTE(vipa, 2026-01-12): The body doesn't create any new
      -- nodes, thus we can use p_map
      (wrapV, appf3_ (nvar_ env.p_map) (nvar_ stateName) (wrapLams (wrap1 (wrap2 bodyPeeled))) val)
    else
      -- NOTE(vipa, 2026-01-12): The body does create new nodes, thus
      -- we need p_subMap
      let store = nvar_ env.storeSubmodel in
      let ist = env.initSubmodel in
      let f = nulam_ innerStateName (wrapLams (wrap1 body)) in
      (wrapV, appf5_ (nvar_ env.p_subMap) (nvar_ stateName) store ist f val)
  | (TmConst {val = CPTraverseSeq _}, [f, val]) ->
    match pvalTransSeq env [val] with (stateName, (wrap, [val])) in
    let f = maybeEtaExpand f in
    let innerStateName = nameSym "st" in
    match pvalTransExpr {env with currStateName = innerStateName} f.body with (wrapB, body) in
    let f = nulam_ innerStateName (TmLam {f with body = wrapB body}) in
    (wrap, appf3_ (nvar_ env.p_traverseSeq) (nvar_ stateName) f val)
  | (TmConst {val = CPApply _}, args) ->
    match pvalTransSeq env args with (stateName, (wrap, [f, val])) in
    (wrap, appf3_ (nvar_ env.p_apply) (nvar_ stateName) f val)
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
    match pvalTransSeq env [val] with (stateName, (wrapV, [val])) in
    let innerStateName = nameSym "st" in
    match pvalTransExpr {env with currStateName = innerStateName} f.body with (wrap1, body) in
    match peelState body with (innerName, (wrap2, bodyPeeled)) in
    if nameEq innerStateName innerName then
      -- NOTE(vipa, 2025-11-19): The body doesn't create any new
      -- nodes, thus we can use p_select
      (wrapV, appf3_ (nvar_ env.p_select) (nvar_ stateName) (TmLam {f with body = wrap1 (wrap2 bodyPeeled)}) val)
    else
      -- NOTE(vipa, 2025-11-19): The body *does* create new nodes,
      -- thus we need p_bind
      let store = nvar_ env.storeSubmodel in
      let ist = env.initSubmodel in
      let f = nulam_ innerStateName (TmLam {f with body = wrap1 body}) in
      (wrapV, appf5_ (nvar_ env.p_bind) (nvar_ stateName) store ist f val)
  | (TmConst {val = CPJoin _}, args) ->
    match pvalTransSeq env args with (stateName, (wrap, [val])) in
    (wrap, appf2_ (nvar_ env.p_join) (nvar_ stateName) val)
  | (TmConst {val = CPAssume _}, args) ->
    match pvalTransSeq env args with (stateName, (wrap, [dist])) in
    let store = nvar_ env.storeAssume in
    (wrap, appf3_ (nvar_ env.p_assume) (nvar_ stateName) store dist)
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
    match pvalTransSeq env [val] with (stateName, (wrap, [val])) in
    let store = nvar_ env.storeWeight in
    (wrap, autoty_tuple_ [appf4_ (nvar_ env.p_weight) (nvar_ stateName) store f val, unit_])
  | (TmConst {val = CPWeight _}, args) ->
    match pvalTransSeq env args with (stateName, (wrap, [weight])) in
    let store = nvar_ env.storeWeight in
    -- TODO(vipa, 2025-11-20): Build TmLam identity function
    (wrap, autoty_tuple_ [appf4_ (nvar_ env.p_weight) (nvar_ stateName) store (TempLam (lam tm. tm)) weight, unit_])
  | (f & TmVar {ident = ident}, args) ->
    match pvalTransSeq env args with (stateName, (wrap, args)) in
    if mapLookupOr false ident env.functions
    then (wrap, appSeq_ f (cons (nvar_ stateName) args))
    else (wrap, autoty_tuple_ [nvar_ stateName, appSeq_ f args])
  | (c & TmConst {val = CMap _}, [f, val]) ->
    match pvalTransSeq env [val] with (stateName, (wrapV, [val])) in
    let f = maybeEtaExpand f in
    let innerStateName = nameSym "st" in
    match pvalTransExpr {env with currStateName = innerStateName} f.body with (wrap1, body) in
    match peelState body with (innerName, (wrap2, bodyPeeled)) in
    if nameEq innerStateName innerName then
      -- NOTE(vipa, 2025-12-02): The body doesn't create any new
      -- nodes, stay with `CMap`
      (wrapV, autoty_tuple_ [nvar_ stateName, appf2_ c (TmLam {f with body = wrap1 (wrap2 bodyPeeled)}) val])
    else
      -- NOTE(vipa, 2025-12-02): The body *does* create new nodes,
      -- switch to mapAccumL
      let f = nulam_ innerStateName (TmLam {f with body = wrap1 body}) in
      (wrapV, appf3_ (nvar_ env.mapAccumL) f (nvar_ stateName) val)
  | (TmConst {val = CPExport _}, [val]) ->
    match pvalTransSeq env [val] with (stateName, (wrapV, [val])) in
    (wrapV, autoty_tuple_ [appf3_ (nvar_ env.p_export) (nvar_ stateName) (nvar_ env.storeExport) val, unit_])
  | (f & TmConst _, args) ->
    match pvalTransSeq env args with (stateName, (wrap, args)) in
    (wrap, autoty_tuple_ [nvar_ stateName, appSeq_ f args])
  | (f, args) -> errorSingle [infoTm f] (concat "Missing case for pvalTransCall: " (expr2str (appSeq_ f args)))

  sem pvalTransDecl : PValTransEnv -> Decl -> (PValTransEnv, Expr -> Expr)
  sem pvalTransDecl env =
  | DeclLet (x & {body = TmLam _}) ->
    match peelLambdas x.body with (wrapLams, body) in
    let innerStateName = nameSym "st" in
    match pvalTransExpr {env with currStateName = innerStateName} body with (wrap1, body) in
    match peelState body with (innerStateName2, (wrap2, peeledBody)) in
    if nameEq innerStateName innerStateName2
    then (env, bind_ (DeclLet {x with body = wrapLams (wrap1 (wrap2 peeledBody))}))
    else
      ( {env with functions = mapInsert x.ident true env.functions}
      , bind_ (DeclLet {x with body = nulam_ innerStateName (wrapLams (wrap1 body))})
      )
  | DeclLet x ->
    match pvalTransExpr env x.body with (wrap1, body) in
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
        match peelLambdas binding.body with (wrapLams, body) in
        let st = nameSym "st" in
        match pvalTransExpr {env with currStateName = st} body with (wrap1, body) in
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

  sem pvalTransSeq : PValTransEnv -> [Expr] -> (Name, Peeled [Expr])
  sem pvalTransSeq env = | seq ->
    let f = lam acc. lam tm.
      match pvalTransExpr {env with currStateName = acc.st} tm with (wrap, tm) in
      match peelState tm with (st, (wrap2, tm)) in
      ({st = st, wraps = concat [wrap2, wrap] acc.wraps}, tm) in
    match mapAccumL f {st = env.currStateName, wraps = []} seq with ({st = st, wraps = wraps}, tms) in
    (st, (lam tm. foldl (lam acc. lam wrap. wrap acc) tm wraps, tms))

  sem pvalTransExpr : PValTransEnv -> Expr -> Peeled Expr
  sem pvalTransExpr env =
  | TmDecl x ->
    match pvalTransDecl env x.decl with (env, wrap) in
    match pvalTransExpr env x.inexpr with (wrapi, tm) in
    (lam tm. wrap (wrapi tm), tm)
  | TmApp x ->
    recursive let work = lam f. lam env. lam args.
      switch f
      case TmApp x then
        work x.lhs env (cons x.rhs args)
      case TmDecl x then
        match pvalTransDecl env x.decl with (env, wrap) in
        match work x.inexpr env args with (wrapi, tm) in
        (lam tm. wrap (wrapi tm), tm)
      case f then
        pvalTransCall env (f, args)
      end
    in work x.lhs env [x.rhs]
  | TmLam x -> errorSingle [x.info] "Encountered unbound lambda in final graph transformation."
  | tm & (TempLam _ | TempFix _) -> error (concat "Encountered unbound templam/fix in final graph transformation: " (expr2str tm))
  | TmMatch (x & {els = TmNever _}) ->
    match pvalTransExpr env x.target with (wrap1, target) in
    match peelState target with (stateName, (wrap2, target)) in
    match pvalTransExpr {env with currStateName = stateName} x.thn with (wrapT, thn) in
    (lam tm. wrap1 (wrap2 (TmMatch {x with target = target, thn = wrapT tm})), thn)
  | TmMatch x ->
    match pvalTransExpr env x.target with (wrap1, target) in
    match peelState target with (stateName, (wrap2, target)) in
    match pvalTransExpr {env with currStateName = stateName} x.thn with (wrapT1, thn) in
    match peelState thn with (stateThn, (wrapT2, thnPeeled)) in
    match pvalTransExpr {env with currStateName = stateName} x.els with (wrapE1, els) in
    match peelState els with (stateEls, (wrapE2, elsPeeled)) in
    if and (nameEq stateName stateThn) (nameEq stateName stateEls) then
      ( lam tm. wrap1 (wrap2 tm)
      , autoty_tuple_
        [ nvar_ stateName
        , TmMatch {x with target = target, thn = wrapT1 (wrapT2 thnPeeled), els = wrapE1 (wrapE2 elsPeeled)}
        ]
      )
    else
      ( lam tm. wrap1 (wrap2 tm)
      , TmMatch {x with target = target, thn = wrapT1 thn, els = wrapE1 els}
      )
  | tm ->
    let f = lam acc. lam tm.
      match pvalTransExpr acc.env tm with (wrap1, val) in
      match peelState val with (stateName, (wrap2, val)) in
      ({env = {acc.env with currStateName = stateName}, wraps = concat [wrap2, wrap1] acc.wraps}, val) in
    match smapAccumL_Expr_Expr f {env = env, wraps = []} tm with ({env = env, wraps = wraps}, tm) in
    ( lam tm. foldl (lam tm. lam wrap. wrap tm) tm wraps
    , autoty_tuple_ [nvar_ env.currStateName, tm]
    )

  sem peelLambdas : Expr -> Peeled Expr
  sem peelLambdas =
  | TmLam x ->
    match peelLambdas x.body with (wrap, body) in
    (lam tm. TmLam {x with body = wrap tm}, body)
  | tm -> (lam tm. tm, tm)

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
      match_ tm (ptuple_ [npvar_ stIdent, npvar_ valIdent])
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
    , conScope = mapEmpty nameCmp
    } in
  match specializeExpr initScope initState ast with (_, (ast, _)) in
  let initTransEnv =
    { currStateName = nameSym "st"
    , functions = mapEmpty nameCmp
    , p_pure = nameSym "p_pure"
    , p_map = nameSym "p_map"
    , p_apply = nameSym "p_apply"
    , p_bind = nameSym "p_bind"
    , p_assume = nameSym "p_assume"
    , p_select = nameSym "p_select"
    , p_weight = nameSym "p_weight"
    , p_export = nameSym "p_export"
    , p_traverseSeq = nameSym "p_traverseSeq"
    , mapAccumL = nameSym "mapAccumL"
    } in
  match pvalTransExpr initTransEnv ast with (wrap, tm) in
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
  , "  p_pure st (Gaussian 0. 1.)"
  , "with"
  , "  (st1, x)"
  , "in"
  , "match"
  , "    p_assume st1 never x"
  , "  with"
  , "    (st2, x1)"
  , "  in"
  , "  (p_weight st2 never (lam x2."
  , "         addf x2 2.) x1, {})"
]
using eqString
else printFailure
in

utest transform
  [ "let a = 2.0 in"
  , "weight (addf (assume (Gaussian 0.0 1.0)) (addf a a))"
  ]
with strJoin "\n"
  [ "let a = 2. in"
  , "match"
  , "  p_pure st (Gaussian 0. 1.)"
  , "with"
  , "  (st1, x)"
  , "in"
  , "match"
  , "    p_assume st1 never x"
  , "  with"
  , "    (st2, x1)"
  , "  in"
  , "  (p_weight st2 never (lam x2."
  , "         addf x2 (addf a a)) x1, {})"
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
  , "  p_pure st (Gaussian 0. 1.)"
  , "with"
  , "  (st1, x)"
  , "in"
  , "match"
  , "    p_assume st1 never x"
  , "  with"
  , "    (st2, a)"
  , "  in"
  , "  match"
  , "      p_map st2 addf a"
  , "    with"
  , "      (st3, x1)"
  , "    in"
  , "    match"
  , "        p_apply st3 x1 a"
  , "      with"
  , "        (st4, x2)"
  , "      in"
  , "      (p_weight st4 never (lam x3."
  , "             x3) x2, {})"
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
  , "    lam x3."
  , "      match"
  , "        p_pure st4 (Gaussian 1. 0.)"
  , "      with"
  , "        (st5, x4)"
  , "      in"
  , "      match"
  , "          p_assume st5 never x4"
  , "        with"
  , "          (st6, x5)"
  , "        in"
  , "        p_map st6 (lam x6."
  , "               addf x6 x3) x5"
  , "in"
  , "match"
  , "  f st 1."
  , "with"
  , "  (st1, x)"
  , "in"
  , "match"
  , "    p_map st1 addf x"
  , "  with"
  , "    (st2, x1)"
  , "  in"
  , "  match"
  , "      f st2 2."
  , "    with"
  , "      (st3, x2)"
  , "    in"
  , "    p_apply st3 x1 x2"
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
  , "  lam st8."
  , "    lam x7."
  , "      match"
  , "        p_pure st8 (Gaussian x7 1.)"
  , "      with"
  , "        (st9, x8)"
  , "      in"
  , "      p_assume st9 never x8"
  , "in"
  , "match"
  , "  draw st 0."
  , "with"
  , "  (st1, x)"
  , "in"
  , "match"
  , "    draw st1 1."
  , "  with"
  , "    (st2, x1)"
  , "  in"
  , "  match"
  , "      p_traverseSeq"
  , "        st2"
  , "        (lam st3."
  , "           lam x2."
  , "             (st3, x2))"
  , "        [ x,"
  , "          x1 ]"
  , "    with"
  , "      (st4, x3)"
  , "    in"
  , "    match"
  , "        p_map st4 get x3"
  , "      with"
  , "        (st5, x4)"
  , "      in"
  , "      match"
  , "          p_pure st5 (Categorical [ 0.5, 0.5 ])"
  , "        with"
  , "          (st6, x5)"
  , "        in"
  , "        match"
  , "            p_assume st6 never x5"
  , "          with"
  , "            (st7, x6)"
  , "          in"
  , "          p_apply st7 x4 x6"
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
  , "  lam st4."
  , "    lam x2."
  , "      match"
  , "        p_pure st4 (Gaussian x2 1.)"
  , "      with"
  , "        (st5, x3)"
  , "      in"
  , "      p_assume st5 never x3"
  , "in"
  , "match"
  , "  p_pure st (Categorical [ 0.5, 0.5 ])"
  , "with"
  , "  (st1, x)"
  , "in"
  , "match"
  , "    p_assume st1 never x"
  , "  with"
  , "    (st2, #var\"X\")"
  , "  in"
  , "  p_bind"
  , "      st2"
  , "      never"
  , "      never"
  , "      (lam st3."
  , "         lam x1."
  , "           let #var\"X1\" = x1 in"
  , "           match"
  , "             #var\"X1\""
  , "           with"
  , "             0"
  , "           then"
  , "             draw st3 0."
  , "           else match"
  , "             #var\"X1\""
  , "           with"
  , "             1"
  , "           in"
  , "           draw st3 1.)"
  , "      #var\"X\""
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
  , "  lam st7."
  , "    lam x3."
  , "      match"
  , "        p_pure st7 (Gaussian x3 1.)"
  , "      with"
  , "        (st8, x4)"
  , "      in"
  , "      p_assume st8 never x4"
  , "in"
  , "match"
  , "  draw st 0."
  , "with"
  , "  (st1, a)"
  , "in"
  , "match"
  , "    draw st1 1."
  , "  with"
  , "    (st2, b)"
  , "  in"
  , "  match"
  , "      p_map st2 addf a"
  , "    with"
  , "      (st3, x)"
  , "    in"
  , "    match"
  , "        p_apply st3 x b"
  , "      with"
  , "        (st4, z)"
  , "      in"
  , "      match"
  , "          p_pure st4 (Categorical [ 0.5, 0.5 ])"
  , "        with"
  , "          (st5, x1)"
  , "        in"
  , "        match"
  , "            p_assume st5 never x1"
  , "          with"
  , "            (st6, #var\"X\")"
  , "          in"
  , "          p_select"
  , "              st6"
  , "              (lam x2."
  , "                 let #var\"X1\" = x2 in"
  , "                 match"
  , "                   #var\"X1\""
  , "                 with"
  , "                   0"
  , "                 then"
  , "                   a"
  , "                 else match"
  , "                   #var\"X1\""
  , "                 with"
  , "                   1"
  , "                 in"
  , "                 b)"
  , "              #var\"X\""
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
  , "  p_pure st (Bernoulli 0.5)"
  , "with"
  , "  (st1, x)"
  , "in"
  , "match"
  , "    p_assume st1 never x"
  , "  with"
  , "    (st2, x1)"
  , "  in"
  , "  p_bind"
  , "      st2"
  , "      never"
  , "      never"
  , "      (lam st3."
  , "         lam x2."
  , "           let target = x2 in"
  , "           match"
  , "             target"
  , "           with"
  , "             true"
  , "           then"
  , "             match"
  , "               p_pure st3 (Gaussian 0. 1.)"
  , "             with"
  , "               (st4, x3)"
  , "             in"
  , "             match"
  , "                 p_assume st4 never x3"
  , "               with"
  , "                 (st5, x4)"
  , "               in"
  , "               p_traverseSeq"
  , "                   st5"
  , "                   (lam st6."
  , "                      lam x5."
  , "                        (st6, x5))"
  , "                   [ x4 ]"
  , "           else"
  , "             p_pure st3 [ 2. ])"
  , "      x1"
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
  , "            match"
  , "              i"
  , "            with"
  , "              0"
  , "            then"
  , "              p_pure st acc"
  , "            else match"
  , "              p_map st (addi acc) n"
  , "            with"
  , "              (st1, x)"
  , "            in"
  , "            mul1 st1 x n (subi i 1)"
  , "  let mul1 ="
  , "    lam st2."
  , "      lam acc1."
  , "        lam n1."
  , "          lam i1."
  , "            match"
  , "              i1"
  , "            with"
  , "              0"
  , "            then"
  , "              (st2, acc1)"
  , "            else match"
  , "              p_map st2 addi acc1"
  , "            with"
  , "              (st3, x1)"
  , "            in"
  , "            match"
  , "                p_apply st3 x1 n1"
  , "              with"
  , "                (st4, x2)"
  , "              in"
  , "              mul1 st4 x2 n1 (subi i1 1)"
  , "in"
  , "match"
  , "  p_pure st5 (Categorical [ 0.25, 0.25, 0.25, 0.25 ])"
  , "with"
  , "  (st6, x3)"
  , "in"
  , "match"
  , "    p_assume st6 never x3"
  , "  with"
  , "    (st7, x4)"
  , "  in"
  , "  match"
  , "      p_map st7 (addi 1) x4"
  , "    with"
  , "      (st8, x5)"
  , "    in"
  , "    mul st8 0 x5 4"
  ]
using eqString
else printFailure
in

()
