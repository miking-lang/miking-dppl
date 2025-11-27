include "coreppl::coreppl-to-mexpr/pval-graph/idealized-transformation.mc"

let _idx0 = stringToSid "0"
let _idx1 = stringToSid "1"

lang TestLang = DPPLParser + MExprLowerNestedPatterns + MExprConstantFold + InlineSingleUse + TempLamAst + IdealizedPValTransformation + AutoTyRecord
  type PValTransEnv =
    { currStateName : Name
    , functions : Map Name Bool -- True if it needs (and returns) a state argument
    , p_pure : Name
    , p_map : Name
    , p_apply : Name
    , p_bind : Name
    , p_assume : Name
    , p_select : Name
    , p_weight : Name
    , p_export : Name
    , p_traverseSeq : Name
    }

  type Peeled e = (Expr -> Expr, e)

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
    (wrap, appf2_ (nvar_ env.p_pure) (nvar_ stateName) val)
  | (TmConst {val = CPMap _}, [f, val]) ->
    match pvalTransSeq env [val] with (stateName, (wrap, [val])) in
    (wrap, appf3_ (nvar_ env.p_map) (nvar_ stateName) f val)
  | (TmConst {val = CPTraverseSeq _}, [f, val]) ->
    match pvalTransSeq env [val] with (stateName, (wrap, [val])) in
    let f = maybeEtaExpand f in
    let innerStateName = nameSym "st" in
    match pvalTrans {env with currStateName = innerStateName} f.body with (wrapB, body) in
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
    match pvalTrans {env with currStateName = innerStateName} f.body with (wrap1, body) in
    match peelState body with (innerName, (wrap2, bodyPeeled)) in
    if nameEq innerStateName innerName then
      -- NOTE(vipa, 2025-11-19): The body doesn't create any new
      -- nodes, thus we can use p_select
      (wrapV, appf3_ (nvar_ env.p_select) (nvar_ stateName) (TmLam {f with body = wrap1 (wrap2 bodyPeeled)}) val)
    else
      -- NOTE(vipa, 2025-11-19): The body *does* create new nodes,
      -- thus we need p_bind
      -- TODO(vipa, 2025-11-19): Implement store and ist
      let store = never_ in
      let ist = never_ in
      let f = nulam_ innerStateName (TmLam {f with body = wrap1 body}) in
      (wrapV, appf5_ (nvar_ env.p_bind) (nvar_ stateName) store ist f val)
  | (TmConst {val = CPAssume _}, args) ->
    match pvalTransSeq env args with (stateName, (wrap, [dist])) in
    let store = never_ in
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
    match pvalTransSeq env [f, val] with (stateName, (wrap, [f, val])) in
    -- TODO(vipa, 2025-11-20): Implement store
    let store = never_ in
    (wrap, autoty_tuple_ [appf4_ (nvar_ env.p_weight) (nvar_ stateName) store f val, unit_])
  | (TmConst {val = CPWeight _}, args) ->
    match pvalTransSeq env args with (stateName, (wrap, [weight])) in
    -- TODO(vipa, 2025-11-20): Implement store
    let store = never_ in
    -- TODO(vipa, 2025-11-20): Build TmLam identity function
    (wrap, autoty_tuple_ [appf4_ (nvar_ env.p_weight) (nvar_ stateName) store (TempLam (lam tm. tm)) weight, unit_])
  | (f & TmVar {ident = ident}, args) ->
    match pvalTransSeq env args with (stateName, (wrap, args)) in
    if mapLookupOr false ident env.functions
    then (wrap, appSeq_ f (cons (nvar_ stateName) args))
    else (wrap, autoty_tuple_ [nvar_ stateName, appSeq_ f args])
  | (f & TmConst _, args) ->
    match pvalTransSeq env args with (stateName, (wrap, args)) in
    (wrap, autoty_tuple_ [nvar_ stateName, appSeq_ f args])
  | (f, args) -> errorSingle [infoTm f] (concat "Missing case for pvalTransCall: " (expr2str (appSeq_ f args)))

  sem pvalTransDecl : PValTransEnv -> Decl -> (PValTransEnv, Expr -> Expr)
  sem pvalTransDecl env =
  | DeclLet (x & {body = TmLam _}) ->
    match peelLambdas x.body with (wrapLams, body) in
    let innerStateName = nameSym "st" in
    match pvalTrans {env with currStateName = innerStateName} body with (wrap1, body) in
    match peelState body with (innerStateName2, (wrap2, peeledBody)) in
    if nameEq innerStateName innerStateName2
    then (env, bind_ (DeclLet {x with body = wrapLams (wrap1 (wrap2 peeledBody))}))
    else
      ( {env with functions = mapInsert x.ident true env.functions}
      , bind_ (DeclLet {x with body = nulam_ innerStateName (wrapLams (wrap1 body))})
      )
  | DeclLet x ->
    match pvalTrans env x.body with (wrap1, body) in
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
        match pvalTrans {env with currStateName = st} body with (wrap1, body) in
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
  | decl -> errorSingle [infoDecl decl] (concat "Missing case for pvalTransDecl: " (decl2str decl))

  sem pvalTransSeq : PValTransEnv -> [Expr] -> (Name, Peeled [Expr])
  sem pvalTransSeq env = | seq ->
    let f = lam acc. lam tm.
      match pvalTrans {env with currStateName = acc.st} tm with (wrap, tm) in
      match peelState tm with (st, (wrap2, tm)) in
      ({st = st, wraps = concat [wrap2, wrap] acc.wraps}, tm) in
    match mapAccumL f {st = env.currStateName, wraps = []} seq with ({st = st, wraps = wraps}, tms) in
    (st, (lam tm. foldl (lam acc. lam wrap. wrap acc) tm wraps, tms))

  sem pvalTrans : PValTransEnv -> Expr -> Peeled Expr
  sem pvalTrans env =
  | TmDecl x ->
    match pvalTransDecl env x.decl with (env, wrap) in
    match pvalTrans env x.inexpr with (wrapi, tm) in
    (lam tm. wrap (wrapi tm), tm)
  | TmApp x ->
    recursive let work = lam f. lam args.
      match f with TmApp x
      then work x.lhs (cons x.rhs args)
      else pvalTransCall env (f, args)
    in work x.lhs [x.rhs]
  | TmLam x -> errorSingle [x.info] "Encountered unbound lambda in final graph transformation."
  | tm & (TempLam _ | TempFix _) -> error (concat "Encountered unbound templam/fix in final graph transformation: " (expr2str tm))
  | TmMatch (x & {els = TmNever _}) ->
    match pvalTrans env x.target with (wrap1, target) in
    match peelState target with (stateName, (wrap2, target)) in
    match pvalTrans {env with currStateName = stateName} x.thn with (wrapT, thn) in
    (lam tm. wrap1 (wrap2 (TmMatch {x with target = target, thn = wrapT tm})), thn)
  | TmMatch x ->
    match pvalTrans env x.target with (wrap1, target) in
    match peelState target with (stateName, (wrap2, target)) in
    match pvalTrans {env with currStateName = stateName} x.thn with (wrapT1, thn) in
    match peelState thn with (stateThn, (wrapT2, thnPeeled)) in
    match pvalTrans {env with currStateName = stateName} x.els with (wrapE1, els) in
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
    warnSingle [infoTm tm] "Default case for pvalTrans";
    let f = lam acc. lam tm.
      match pvalTrans acc.env tm with (wrap1, val) in
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
  | TmRecord x ->
    -- NOTE(vipa, 2025-11-19): We know that pvalTrans, when given a
    -- value of type `a`, will produce a value of type `(PValState st,
    -- a)` for some `st`. By construction, we also know that if there
    -- is such a tuple, then the first element is just a variable.
    match mapLookup _idx0 x.bindings with Some (TmVar {ident = stIdent}) in
    match mapLookup _idx1 x.bindings with Some expr in
    (stIdent, (lam x. x, expr))
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

mexpr

use TestLang in

let args =
  { _defaultBootParserParseMCoreFileArg ()
  with eliminateDeadCode = false
  , allowFree = true
  , keywords = pplKeywords
  , builtin = cpplBuiltin
  } in
let ast = parseMCoreFile args "test.mc" in
let ast = use DPPLParser in makeKeywords ast in
let ast = symbolizeExpr symEnvDefault ast in
let ast = typeCheck ast in
let ast = lowerAll ast in
let ast = inlineSingleUseLets ast in
let initState =
  { specializations = mapEmpty nameCmp
  } in
let initScope =
  { functionDefinitions = mapEmpty nameCmp
  , valueScope = mapEmpty nameCmp
  , conScope = mapEmpty nameCmp
  } in
match specializeExpr initScope initState ast with (_, (ast, _)) in
printLn (pprintCode 0 pprintEnvEmpty ast).1;
printLn "==========================";
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
  } in
match pvalTrans initTransEnv ast with (wrap, tm) in
printLn (pprintCode 0 pprintEnvEmpty (wrap tm)).1;

()
