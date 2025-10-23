include "coreppl/src/parser.mc"

lang Composed = DPPLParser
  syn Wrap =
  | Wrapped
  | Unused

  syn PTypeC rec =
  | PRecord (Map SID rec)
  | PSeq rec
  | PDist rec

  syn PTypeA =
  | PInt
  | PFloat
  | PBool
  | PChar

  sem mapPTypeC : all a. all b. (a -> b) -> PTypeC a -> PTypeC b
  sem mapPTypeC f =
  | PRecord x -> PRecord (mapMap f x)
  | PSeq x -> PSeq (f x)
  | PDist x -> PDist (f x)

  sem foldPTypeC : all a. all x. (a -> x -> a) -> a -> PTypeC x -> a
  sem foldPTypeC f acc =
  | PRecord x -> mapFoldWithKey (lam acc. lam. lam v. f acc v) acc x
  | PSeq x -> f acc x
  | PDist x -> f acc x

  syn PureType =
  | PureTypeC (PTypeC PureType)
  | PureTypeA PTypeA

  -- Invariant: for all `PType`s, if it is `PLater`, at least one
  -- descendant is `PHere {wrapped = Wrapped ()}` or `PHere {wrapped = Unused ()}`.
  syn PType =
  | PLater (PTypeC PType)
  | PNever PTypeA
  | PHere {wrapped : Wrap, ty : PureType}

  sem ptypeToString : PType -> String
  sem ptypeToString =
  | PHere x ->
    let prefix = switch x.wrapped
      case Wrapped _ then "!"
      case Unused _ then "~"
      end in
    concat prefix (pureTypeToString x.ty)
  | PNever x -> ptypeAToString x
  | PLater x -> ptypeCToString ptypeToString x

  sem ptypeAToString : PTypeA -> String
  sem ptypeAToString =
  | PInt _ -> "Int"
  | PFloat _ -> "Float"
  | PBool _ -> "Bool"
  | PChar _ -> "Char"

  sem pureTypeToString : PureType -> String
  sem pureTypeToString =
  | PureTypeC x -> ptypeCToString pureTypeToString x
  | PureTypeA x -> ptypeAToString x

  sem ptypeCToString : all x. (x -> String) -> PTypeC x -> String
  sem ptypeCToString f =
  | PRecord x ->
    let b = lam pair.
      match pair with (key, value) in
      join [sidToString key, " : ", f value] in
    join ["{", strJoin ", " (map b (mapBindings x)), "}"]
  | PSeq x ->
    join ["[", f x, "]"]
  | PDist x ->
    join ["(Dist ", f x, ")"]

  sem isPureIsh : PType -> Bool
  sem isPureIsh =
  | PNever _ -> true
  | PHere {wrapped = Unused _} -> true
  | PHere {wrapped = Wrapped _} -> false
  | PLater x ->
    let f = lam acc. lam v. if acc then isPureIsh v else false in
    foldPTypeC f true x

  sem isTopWrapped : PType -> Bool
  sem isTopWrapped =
  | PHere {wrapped = Wrapped _} -> true
  | _ -> false

  sem unwrapOnce : PType -> Either PTypeA (PTypeC PType)
  sem unwrapOnce =
  | PHere {ty = PureTypeA ty} -> Left ty
  | PHere {wrapped = Wrapped _, ty = PureTypeC ty} ->
    Right (mapPTypeC pureToPType ty)
  | PHere {wrapped = Unused _, ty = PureTypeC ty} ->
    Right (mapPTypeC (lam ty. PHere {wrapped = Unused (), ty = ty}) ty)
  | PNever x -> Left x
  | PLater x -> Right x

  sem pureToPType : PureType -> PType
  sem pureToPType =
  | PureTypeA x -> PNever x
  | PureTypeC x -> PLater (mapPTypeC pureToPType x)

  type PState =
    { functionDefinitions : Map Name {params : [Name], mayBeRecursive : Bool, body : Expr}
    , valueScope : Map Name PType
    , specializations : Map Name (Map [PType] (Name, PType, Option DeclLetRecord))
    }

  syn Const =
  | CPAssume
  | CPWeight
  | CPPure
  | CPMap
  | CPApply
  | CPJoin
  | CPTraverseSeq

  sem getConstStringCode indent =
  | CPAssume _ -> "p_assume"
  | CPWeight _ -> "p_weight"
  | CPPure _ -> "p_pure"
  | CPMap _ -> "p_map"
  | CPApply _ -> "p_apply"
  | CPJoin _ -> "p_join"
  | CPTraverseSeq _ -> "p_traverseSeq"

  syn Expr =
  -- NOTE(vipa, 2025-10-20): This should be a _linear_ function, and
  -- should always represent a pure function
  | TempLam (Expr -> Expr)

  sem isAtomic =
  | TempLam _ -> false

  sem pprintCode indent env =
  | TempLam f ->
    let x = nameSym "x" in
    match pprintVarName env x with (env, str) in
    match pprintCode (pprintIncr indent) env (f (nvar_ x)) with (env, body) in
    ( env
    , join ["lam ", str, ".", pprintNewline (pprintIncr indent), body]
    )

  sem ptyCmp : PType -> PType -> Int
  sem ptyCmp l = | r ->
    switch (l, r)
    case (PLater l, PLater r) then _ptyCCmp ptyCmp (l, r)
    case (PHere l, PHere r) then
      let res = subi (constructorTag l.wrapped) (constructorTag r.wrapped) in
      if neqi res 0 then res else
      recursive let work = lam l. lam r.
        switch (l, r)
        case (PureTypeA l, PureTypeA r) then _ptyACmp (l, r)
        case (PureTypeC l, PureTypeC r) then _ptyCCmp work (l, r)
        case (l, r) then subi (constructorTag l) (constructorTag r)
        end in
      work l.ty r.ty
    case (l, r) then
      subi (constructorTag l) (constructorTag r)
    end

  sem _ptyACmp : (PTypeA, PTypeA) -> Int
  sem _ptyACmp =
  | (PInt _, PInt _) -> 0
  | (PFloat _, PFloat _) -> 0
  | (PBool _, PBool _) -> 0
  | (PChar _, PChar _) -> 0
  | (l, r) ->
    let lt = constructorTag l in
    let rt = constructorTag r in
    if eqi lt rt
    then error "Missing equal case in _ptyACmp"
    else subi lt rt

  sem _ptyCCmp : all x. (x -> x -> Int) -> (PTypeC x, PTypeC x) -> Int
  sem _ptyCCmp rec =
  | (PDist l, PDist r) -> rec l r
  | (PSeq l, PSeq r) -> rec l r
  | (l, r) ->
    let lt = constructorTag l in
    let rt = constructorTag r in
    if eqi lt rt
    then error "Missing equal case in _ptyCCmp"
    else subi lt rt

  sem lubPType : PType -> PType -> PType
  sem lubPType l = | r -> _lubPType (l, r)

  sem _lubPType : (PType, PType) -> PType
  sem _lubPType =
  | (lty & PNever _, PNever _) -> lty
  | (lty & PHere {wrapped = l}, rty & PHere {wrapped = r}) ->
    switch (l, r)
    case (Wrapped _, _) then lty
    case (_, Wrapped _) then rty
    case (Unused _, _) then rty
    case (_, Unused _) then lty
    end
  | (lty & PHere {wrapped = l}, rty & (PLater _ | PNever _)) ->
    switch l
    case Wrapped _ then lty
    case Unused _ then rty
    end
  | (lty & (PLater _ | PNever _), rty & PHere {wrapped = r}) ->
    switch r
    case Wrapped _ then rty
    case Unused _ then lty
    end
  | (PLater (PSeq l), PLater (PSeq r)) -> PLater (PSeq (lubPType l r))
  | (PLater (PRecord l), PLater (PRecord r)) -> PLater (PRecord (mapIntersectWith lubPType l r))
  | (PLater (PDist l), PLater (PDist r)) -> PLater (PDist (lubPType l r))

  sem ensureWrapped : PType -> PType
  sem ensureWrapped = | ty ->
    recursive let work : PType -> PureType = lam ty.
      switch ty
      case PLater x then PureTypeC (mapPTypeC work x)
      case PHere x then x.ty
      case PNever x then PureTypeA x
      end in
    PHere {wrapped = Wrapped (), ty = work ty}

  sem adjustWrapping : (PType, PType) -> Expr -> Expr
  sem adjustWrapping =
  | (PNever _, PNever _) -> lam x. x
  | (PNever _, PHere {wrapped = Wrapped _}) -> app_ (uconst_ (CPPure ()))
  | (PHere {wrapped = Unused _}, _) -> lam x. x
  | (PHere {wrapped = Wrapped _}, PHere {wrapped = Wrapped _}) -> lam x. x
  | (PHere {wrapped = Wrapped _}, PLater _) -> lam tm. errorSingle [infoTm tm] "Tried to convert a value to a less wrapped value, which is impossible"
  | (PLater (PSeq ty), PHere {wrapped = Wrapped _, ty = PureTypeC (PSeq target)}) ->
    let f = TempLam (adjustWrapping (ty, PHere {wrapped = Wrapped (), ty = target})) in
    _app (_app (uconst_ (CPTraverseSeq ())) f)
  | (PLater (PSeq ty), PLater (PSeq target)) ->
    _app (_app (uconst_ (CMap ())) (TempLam (adjustWrapping (ty, target))))
  | (PLater (PRecord tys), PHere {wrapped = Wrapped _, ty = PureTypeC (PRecord targets)}) ->
    let mkAdjustment = lam l. lam r. adjustWrapping (l, PHere {wrapped = Wrapped (), ty = r}) in
    let adjustments = mapIntersectWith mkAdjustment tys targets in
    recursive let construct = lam prev. lam remaining.
      match remaining with [key] ++ remaining
      then TempLam (lam tm. construct (snoc prev (key, tm)) remaining)
      else TmRecord
        { bindings = mapFromSeq cmpSID prev
        , ty = tyunknown_
        , info = NoInfo ()
        } in
    let construct = app_ (uconst_ (CPPure ())) (construct [] (mapKeys adjustments)) in
    lam tm.
      match tm with TmRecord x then
        let args = mapValues (mapIntersectWith (lam adjust. lam tm. adjust tm) adjustments x.bindings) in
        foldl _apply construct args
      else
        let names = mapMap (lam f. (f, nameSym "p")) adjustments in
        match_ tm (PatRecord {bindings = mapMap (lam x. npvar_ x.1) names, info = NoInfo (), ty = tyunknown_})
          (foldl _apply construct (map (lam x. x.0 (nvar_ x.1)) (mapValues names)))
          never_
  | (PLater (PRecord tys), PLater (PRecord targets)) ->
    let adjustments = mapIntersectWith (lam l. lam r. adjustWrapping (l, r)) tys targets in
    recursive let construct = lam prev. lam remaining.
      match remaining with [key] ++ remaining
      then TempLam (lam tm. construct (snoc prev (key, tm)) remaining)
      else TmRecord
        { bindings = mapFromSeq cmpSID prev
        , ty = tyunknown_
        , info = NoInfo ()
        } in
    let construct = construct [] (mapKeys adjustments) in
    lam tm.
      match tm with TmRecord x then
        let args = mapValues (mapIntersectWith (lam adjust. lam tm. adjust tm) adjustments x.bindings) in
        foldl _app construct args
      else
        let names = mapMap (lam f. (f, nameSym "p")) adjustments in
        match_ tm (PatRecord {bindings = mapMap (lam x. npvar_ x.1) names, info = NoInfo (), ty = tyunknown_})
          (foldl _app construct (map (lam x. x.0 (nvar_ x.1)) (mapValues names)))
          never_
  | (PLater (PDist ty), PHere {wrapped = Wrapped _, ty = PureTypeC (PDist target)}) ->
    if isPureIsh ty
    then app_ (uconst_ (CPPure ()))
    else error "We somehow have a non-pure type parameter to a PDist, can't handle that"
  | (PLater (PDist ty), PLater (PDist target)) ->
    if isPureIsh target then lam tm. tm else error "We somehow need to wrap a type parameter to a PDist, can't do that"
  | (l, r) -> error (join ["Missing case in adjustWrapping: ", ptypeToString l, ", ", ptypeToString r])

  sem tyToPTypeX : all x. (PTypeA -> x) -> (PTypeC x -> x) -> Type -> x
  sem tyToPTypeX atom composite =
  | TyDist x -> composite (PDist (tyToPTypeX atom composite x.ty))
  | TyFloat _ -> atom (PFloat ())
  | TyBool _ -> atom (PBool ())
  | TyInt _ -> atom (PInt ())
  | TyChar _ -> atom (PChar ())
  | TySeq x -> composite (PSeq (tyToPTypeX atom composite x.ty))
  | ty -> printLn (getTypeStringCode 0 pprintEnvEmpty ty).1; errorSingle [infoTy ty] "Missing case for tyToPTypeX"

  sem tyToPureType : Type -> PureType
  sem tyToPureType = | ty -> tyToPTypeX (lam x. PureTypeA x) (lam x. PureTypeC x) ty

  sem tyToPurePType : Type -> PType
  sem tyToPurePType = | ty -> tyToPTypeX (lam x. PNever x) (lam x. PLater x) ty

  sem _app : Expr -> Expr -> Expr
  sem _app l = | r -> _app_ (l, r)

  sem isIdentity : (Expr -> Expr) -> Bool
  sem isIdentity = | f ->
    let n = nameSym "x" in
    match f (nvar_ n) with TmVar {ident = ident}
    then nameEq ident n
    else false

  sem isPure : (Expr -> Expr) -> Bool
  sem isPure = | f ->
    let n = nameSym "x" in
    match f (nvar_ n) with TmApp {lhs = TmConst {val = CPPure _}, rhs = TmVar {ident = ident}}
    then nameEq ident n
    else false

  -- This is semantically the same as `app_`, except it tries a number
  -- of rewrite rules, essentially evaluating some of it at compile
  -- time.
  sem _app_ : (Expr, Expr) -> Expr
  sem _app_ =
  -- we have a simple function, just apply it
  | (TempLam f, x) -> f x
  -- p_map id = id
  | (f & TmConst {val = CPMap _}, x & TempLam f2) ->
    if isIdentity f2 then TempLam (lam x. x) else app_ f x
  -- mapSeq id = id
  | (f & TmConst {val = CMap _}, x & TempLam f2) ->
    if isIdentity f2 then TempLam (lam x. x) else app_ f x
  -- p_map f (p_pure x) = p_pure (f x)
  | ( TmApp
      { lhs = TmConst {val = CPMap _}
      , rhs = f
      }
    , TmApp
      { lhs = TmConst {val = CPPure _}
      , rhs = x
      }
    ) ->
    app_ (uconst_ (CPPure ())) (_app f x)
  -- p_map f (p_map g x) = p_map (lam x. f (g x)) x
  | ( TmApp
      { lhs = TmConst {val = CPMap _}
      , rhs = f
      }
    , TmApp
      { lhs = TmApp
        { lhs = TmConst {val = CPMap _}
        , rhs = g
        }
      , rhs = x
      }
    ) ->
    appf2_ (uconst_ (CPMap ())) (TempLam (lam x. _app f (_app g x))) x
  -- p_map f (p_apply g x) = p_apply (p_map (lam g. lam x. f (g x)) g) x
  | ( TmApp
      { lhs = TmConst {val = CPMap _}
      , rhs = f
      }
    , TmApp
      { lhs = TmApp
        { lhs = TmConst {val = CPApply _}
        , rhs = g
        }
      , rhs = x
      }
    ) ->
    -- OPT(vipa, 2025-10-21): This case introduces some quadratic time
    -- complexity, e.g., in `f x1 x2 ... xn`, where x1 through xi are
    -- wrapped and all other x are pure we will use `_map` to go
    -- through i-1 `CPApply`s n-i times. Could probably be avoided by
    -- having an "apply-train" term that has the function plus a list
    -- of arguments.
    let f = TempLam (lam g. TempLam (lam x. _app f (_app g x))) in
    appf2_ (uconst_ (CPApply ())) (_map f g) x
  -- p_apply (p_pure f) = p_map f
  | ( TmConst {val = CPApply _}
    , TmApp
      { lhs = TmConst {val = CPPure _}
      , rhs = f
      }
    ) ->
    TempLam (_map f)
  -- p_apply f (p_pure x) = p_map (lam f. f x) f
  | ( TmApp
      { lhs = TmConst {val = CPApply _}
      , rhs = f
      }
    , TmApp
      { lhs = TmConst {val = CPPure _}
      , rhs = x
      }
    ) ->
    _map (TempLam (lam f. _app f x)) f
  -- p_traverseSeq pure = pure
  | (f & TmConst {val = CPTraverseSeq _}, x & TempLam f2) ->
    if isPure f2 then uconst_ (CPPure ()) else app_ f x
  -- p_join (p_pure x) = x
  | ( TmConst {val = CPJoin _}
    , TmApp
      { lhs = TmConst {val = CPPure _}
      , rhs = x
      }
    ) -> x
  -- base case, just make the TmApp
  | (f, x) -> app_ f x

  sem _map : Expr -> Expr -> Expr
  sem _map f = | x -> _app (_app (uconst_ (CPMap ())) f) x

  sem _apply : Expr -> Expr -> Expr
  sem _apply f = | a -> _app (_app (uconst_ (CPApply ())) f) a

  sem _join : Expr -> Expr
  sem _join = | x -> _app (uconst_ (CPJoin ())) x

  sem specializeCall : PState -> {f : Expr, args : [(Expr, PType)], ret : Type} -> (PState, (Expr, PType))
  sem specializeCall st =
  | {f = TmVar x, args = args, ret = retTy} ->
    match unzip args with (args, argTys) in
    match optionBind (mapLookup x.ident st.specializations) (mapLookup argTys) with Some (name, ty, _) then
      (st, (appSeq_ (nvar_ name) args, ty))
    else
      -- NOTE(vipa, 2025-10-21): Not previously specialized, do it.
      let definition =
        match mapLookup x.ident st.functionDefinitions
        with Some x then x
        else error (join ["Missing entry in functionDefinitions for ", nameGetStr x.ident]) in
      let prevValueScope = st.valueScope in
      let valueScope = foldl2 (lam m. lam n. lam ty. mapInsert n ty m) prevValueScope definition.params argTys in
      -- TODO(vipa, 2025-10-21): Handle recursive functions
      let name = nameSetNewSym x.ident in
      match
        let st = {st with valueScope = valueScope} in
        if definition.mayBeRecursive then
          recursive let speculate = lam guess.
            let spec = mapSingleton (seqCmp ptyCmp) argTys (name, guess, None ()) in
            let st2 = {st with specializations = mapInsertWith mapUnion x.ident spec st.specializations} in
            match specializeExpr st2 definition.body with (st2, (body, retTy)) in
            if eqi 0 (ptyCmp guess retTy) then (st2, (body, retTy)) else
            speculate retTy in
          speculate (PHere {wrapped = Unused (), ty = tyToPureType retTy})
        else specializeExpr st definition.body
      with (st, (body, retTy)) in
      let st = {st with valueScope = prevValueScope} in
      let def : DeclLetRecord =
        { ident = name
        , tyAnnot = tyunknown_
        , tyBody = tyunknown_
        , body = nulams_ definition.params body  -- TODO(vipa, 2025-10-21): probably resymbolize here
        , info = NoInfo ()
        } in
      let st =
        { st with valueScope = prevValueScope
        , specializations = mapInsertWith mapUnion x.ident
          (mapSingleton (seqCmp ptyCmp) argTys (name, retTy, Some def))
          st.specializations
        } in
      (st, (foldl app_ (nvar_ name) args, retTy))
  | {f = tm & (TempLam _ | TmConst _), args = args, ret = ret} ->
    let pureRet = tyToPurePType ret in
    if forAll (lam x. isPureIsh x.1) args then
      (st, (foldl _app tm (map (lam x. x.0) args), pureRet))
    else
      let tm = app_ (uconst_ (CPPure ())) tm in
      let args =
        let f = lam p.
          match p with (tm, ty) in
          adjustWrapping (ty, ensureWrapped ty) tm in
        map f args in
      (st, (foldl _apply tm args, ensureWrapped pureRet))
  | {f = tm} -> errorSingle [infoTm tm] "Missing case in specializeCall"

  sem specializeDeclPre : PState -> Decl -> PState
  sem specializeDeclPre st =
  | DeclLet x ->
    let prevValueScope = st.valueScope in
    match specializeExpr st x.body with (st, (tm, ty)) in
    let st = {st with valueScope = prevValueScope} in
    let spec = mapSingleton (seqCmp ptyCmp) [] (x.ident, ty, Some {x with body = tm}) in
    { st with valueScope = mapInsert x.ident ty st.valueScope
    , specializations = mapInsert x.ident spec st.specializations
    }
  | DeclLet {ident = ident, body = body & TmLam _} ->
    recursive let work = lam params. lam tm.
      match tm with TmLam x
      then work (snoc params x.ident) x.body
      else {params = params, mayBeRecursive = false, body = tm} in
    {st with functionDefinitions = mapInsert ident (work [] body) st.functionDefinitions}
  | DeclRecLets x ->
    recursive let work = lam params. lam tm.
      match tm with TmLam x
      then work (snoc params x.ident) x.body
      else {params = params, mayBeRecursive = true, body = tm} in
    let f = lam definitions. lam decl.
      mapInsert decl.ident (work [] decl.body) definitions in
    {st with functionDefinitions = foldl f st.functionDefinitions x.bindings}
  | decl -> errorSingle [infoDecl decl] "Missing case in specializeDeclPre"

  sem specializeDeclPost : PState -> Decl -> (PState, [Decl])
  sem specializeDeclPost st =
  | DeclLet {ident = ident} ->
    match mapLookup ident st.specializations with Some specs then
      let addDefinition = lam a. lam. lam trip.
        match trip with (_, _, Some decl)
        then snoc a (DeclLet decl)
        else error (join ["Missing definition for specialization in let for ", nameGetStr ident]) in
      ({st with specializations = mapRemove ident st.specializations}, mapFoldWithKey addDefinition [] specs)
    else (st, [])
  | DeclRecLets x ->
    let specs = join (map mapValues (mapOption (lam d. mapLookup d.ident st.specializations) x.bindings)) in
    let specs = mapOption (lam x. x.2) specs in
    let specializations = foldl (lam acc. lam decl. mapRemove decl.ident acc) st.specializations x.bindings in
    ({st with specializations = specializations}, [DeclRecLets {x with bindings = specs}])
  | decl -> errorSingle [infoDecl decl] "Missing case in specializeDeclPost"

  sem specializeExpr : PState -> Expr -> (PState, (Expr, PType))
  sem specializeExpr st =
  | TmDecl x ->
    let st = specializeDeclPre st x.decl in
    match specializeExpr st x.inexpr with (st, (tm, ty)) in
    match specializeDeclPost st x.decl with (st, newDecls) in
    (st, (bindall_ newDecls tm, ty))
  | TmApp x ->
    recursive let collectApps = lam acc. lam tm.
      match tm with TmApp x
      then collectApps (cons x.rhs acc) x.lhs
      else (tm, acc) in
    match collectApps [x.rhs] x.lhs with (f, args) in
    match mapAccumL specializeExpr st args with (st, args) in
    specializeCall st {f = f, args = args, ret = x.ty}
  | TmAssume x ->
    match specializeExpr st x.dist with (st, (dist, distTy)) in
    let elemTy =
      match ensureWrapped distTy with PHere {ty = PureTypeC (PDist x)} in
      PHere {wrapped = Wrapped (), ty = x} in
    let dist = adjustWrapping (distTy, ensureWrapped distTy) dist in
    let tm = app_ (uconst_ (CPAssume ())) dist in
    (st, (tm, elemTy))
  | TmDist x ->
    match mapAccumL specializeExpr st (distParams x.dist) with (st, args) in
    let l =
      recursive let work = lam prev. lam ps.
        match ps with [p] ++ ps
        then TempLam (lam tm. work (snoc prev tm) ps)
        else TmDist {x with dist = distWithParams x.dist prev} in
      work [] args in
    specializeCall st {f = l, args = args, ret = x.ty}
  | tm & TmVar x ->
    match mapLookup x.ident st.valueScope with Some ty
    then (st, (tm, ty))
    else errorSingle [x.info] "Missing entry in valueScope"
  | TmSeq x ->
    match mapAccumL specializeExpr st x.tms with (st, tms) in
    match unzip tms with (tms, tmTys) in
    let elemTy = match tmTys with [ty] ++ tmTys
      then foldl lubPType ty tmTys
      else match unwrapType x.ty with TySeq x in PHere {wrapped = Unused (), ty = tyToPureType x.ty} in
    let tms = zipWith (lam tmTy. adjustWrapping (tmTy, elemTy)) tmTys tms in
    (st, (TmSeq {x with tms = tms}, PLater (PSeq elemTy)))
  | TmRecord x ->
    match mapMapAccum (lam st. lam. lam tm. specializeExpr st tm) st x.bindings with (st, zipped) in
    let bindings = mapMap (lam x. x.0) zipped in
    let bindingsTy = mapMap (lam x. x.1) zipped in
    (st, (TmRecord {x with bindings = bindings}, PLater (PRecord bindingsTy)))
  -- TODO(vipa, 2025-10-24): Specialize for infallible patterns (els =
  -- never) so we don't get a sub-model for the then-branch
  -- TODO(vipa, 2025-10-24): Specialize for chains of matches on the
  -- same variable?
  | TmMatch x ->
    -- NOTE(vipa, 2025-10-22): We assume that the pattern is shallow,
    -- but not just a wildcard
    match specializeExpr st x.target with (st, (target, targetTy)) in
    let oldValueScope = st.valueScope in
    let st = addMatchNames st (unwrapOnce targetTy, x.pat) in
    match specializeExpr st x.thn with (st, (thn, thnTy)) in
    match specializeExpr {st with valueScope = oldValueScope} x.els with (st, (els, elsTy)) in
    let st = {st with valueScope = oldValueScope} in
    let lubTy = lubPType thnTy elsTy in
    if isTopWrapped targetTy then
      let pureType = tyToPurePType x.ty in
      let retTy = ensureWrapped pureType in
      if isPureIsh lubTy then
        -- NOTE(vipa, 2025-10-23): Both branches are pure, i.e., we
        -- can make this a `map`
        (st, (_map (TempLam (lam target. TmMatch {x with target = target, thn = thn, els = els})) target, retTy))
      else
        -- NOTE(vipa, 2025-10-23): At least one branch is not pure,
        -- i.e., this needs to be a `bind`
        let lubTy = ensureWrapped lubTy in
        let m = lam target. TmMatch
          { x with target = target
          , thn = adjustWrapping (thnTy, lubTy) thn
          , els = adjustWrapping (elsTy, lubTy) els
          } in
        (st, (_join (_map (TempLam m) target), retTy))
    else
      ( st
      , ( TmMatch
          { x with target = target
          , thn = adjustWrapping (thnTy, lubTy) thn
          , els = adjustWrapping (elsTy, lubTy) els
          }
        , lubTy
        )
      )
  | tm & TmNever x -> (st, (tm, PHere {wrapped = Unused (), ty = tyToPureType x.ty}))
  | tm & TmConst {val = CFloat _} -> (st, (tm, PNever (PFloat ())))
  | tm & TmConst {val = CInt _} -> (st, (tm, PNever (PInt ())))
  | tm & TmConst {val = CBool _} -> (st, (tm, PNever (PBool ())))
  | tm -> errorSingle [infoTm tm] "Missing case in specializeExpr"

  sem addMatchNames : PState -> (Either PTypeA (PTypeC PType), Pat) -> PState
  sem addMatchNames st =
  | (_, PatBool _) -> st
  | (_, PatInt _) -> st
  | (_, PatChar _) -> st
  | (Right (PRecord ty), PatRecord pat) ->
    let f = lam ty. lam pat.
      match pat with PatNamed {ident = PName ident}
      then lam acc. mapInsert ident ty acc
      else lam acc. acc in
    let valueScope = mapFoldWithKey (lam acc. lam. lam f. f acc) st.valueScope (mapIntersectWith f ty pat.bindings) in
    {st with valueScope = valueScope}
  | (Right (PSeq ty), PatSeqTot p) ->
    let f = lam acc. lam p.
      match p with PatNamed {ident = PName ident}
      then mapInsert ident ty acc
      else acc in
    {st with valueScope = foldl f st.valueScope p.pats}
  | (Right (PSeq ty), PatSeqEdge p) ->
    let f = lam acc. lam p.
      match p with PatNamed {ident = PName ident}
      then mapInsert ident ty acc
      else acc in
    let valueScope = foldl f st.valueScope p.prefix in
    let valueScope = foldl f valueScope p.postfix in
    let valueScope = match p.middle with PName ident
      then mapInsert ident (PLater (PSeq ty)) valueScope
      else valueScope in
    {st with valueScope = valueScope}
end

mexpr

use Composed in

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
let initState =
  { functionDefinitions = mapEmpty nameCmp
  , valueScope = mapEmpty nameCmp
  , specializations = mapEmpty nameCmp
  } in
match specializeExpr initState ast with (_, (ast, _)) in
printLn (pprintCode 0 pprintEnvEmpty ast).1
