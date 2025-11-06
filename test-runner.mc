include "coreppl/src/parser.mc"

lang Composed = DPPLParser
  syn Wrap =
  | Wrapped
  | Unused

  syn PTypeC rec =
  | PRecord (Map SID rec)
  | PSeq rec
  | PDist rec
  -- NOTE(vipa, 2025-10-24): We assume recursion for types is direct
  -- (no mutual recursion) and non-polymorphic. Constructors absent
  -- from the `Map` are `Unused`.
  | PUser (Map Name (RecTy rec))

  syn RecTy rec =
  | NoRec rec
  | Rec
  | RLater (PTypeC (RecTy rec))

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
  | PUser x ->
    recursive let work = lam recTy. switch recTy
      case NoRec p then NoRec (f p)
      case Rec _ then Rec ()
      case RLater x then RLater (mapPTypeC work x)
      end in
    PUser (mapMap work x)

  sem foldPTypeC : all a. all x. (a -> x -> a) -> a -> PTypeC x -> a
  sem foldPTypeC f acc =
  | PRecord x -> mapFoldWithKey (lam acc. lam. lam v. f acc v) acc x
  | PSeq x -> f acc x
  | PDist x -> f acc x
  | PUser x ->
    recursive let work = lam acc. lam recTy. switch recTy
      case NoRec p then f acc p
      case Rec _ then acc
      case RLater x then foldPTypeC work acc x
      end in
    mapFoldWithKey (lam acc. lam. lam recTy. work acc recTy) acc x

  sem mapAccumLPTypeC : all a. all x. all y. (a -> x -> (a, y)) -> PTypeC x -> (a, PTypeC y)

  sem fold2PTypeC : all a. all x. all y. (a -> x -> y -> a) -> a -> PTypeC x -> PTypeC y -> a

  sem map2PTypeC : all x. all y. all z. (x -> y -> z) -> PTypeC x -> PTypeC y -> PTypeC z
  sem map2PTypeC f l = | r -> map2PTypeC_ f (l, r)

  sem map2PTypeC_ : all x. all y. all z. (x -> y -> z) -> (PTypeC x, PTypeC y) -> PTypeC z
  sem map2PTypeC_ f =
  | (lty & PRecord l, rty & PRecord r) ->
    let showTy = lam ty. ptypeCToString (lam. "_") ty in
    let f = lam l. lam r.
      match (l, r) with (Some l, Some r) then Some (f l r)
      else error (join ["Tried to call map2PTypeC with two records with different sets of fields: ", showTy lty, " and ", showTy rty]) in
    PRecord (mapMerge f l r)
  | (PSeq l, PSeq r) -> PSeq (f l r)
  | (PDist l, PDist r) -> PDist (f l r)
  | (lty & PUser l, rty & PUser r) ->
    let showTy = lam ty. ptypeCToString (lam. "_") ty in
    recursive let work = lam l. lam r. switch (l, r)
      case (NoRec l, NoRec r) then NoRec (f l r)
      case (Rec _, Rec _) then Rec ()
      case (RLater l, RLater r) then RLater (map2PTypeC work l r)
      end in
    let f = lam l. lam r.
      match (l, r) with (Some l, Some r) then Some (work l r)
      else printErrorLn (join ["Tried to call map2PTypeC with two user types with different sets of constructors: ", showTy lty, " and ", showTy rty]); None () in
    PUser (mapMerge f l r)

  sem mapMPTypeCOption : all a. all b. (a -> Option b) -> PTypeC a -> Option (PTypeC b)
  sem mapMPTypeCOption f =
  | PRecord x -> optionMap (lam m. PRecord m)
    (mapFoldlOption (lam acc. lam k. lam v. optionMap (lam v. mapInsert k v acc) (f v)) (mapEmpty (mapGetCmpFun x)) x)
  | PSeq x -> optionMap (lam x. PSeq x) (f x)
  | PDist x -> optionMap (lam x. PDist x) (f x)
  | PUser x ->
    recursive let work = lam recTy. switch recTy
      case NoRec p then optionMap (lam p. NoRec p) (f p)
      case Rec _ then Some (Rec ())
      case RLater x then optionMap (lam x. RLater x) (mapMPTypeCOption work x)
      end in
    optionMap (lam x. PUser x)
      (mapFoldlOption (lam acc. lam k. lam v. optionMap (lam v. mapInsert k v acc) (work v)) (mapEmpty (mapGetCmpFun x)) x)

  sem foldRecTy : all a. all x. (a -> x -> a) -> a -> RecTy x -> a
  sem foldRecTy f acc =
  | NoRec x -> f acc x
  | Rec _ -> acc
  | RLater x -> foldPTypeC (foldRecTy f) acc x

  syn PureType =
  | PureTypeC (PTypeC PureType)
  | PureTypeA PTypeA

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
  | PUser x ->
    recursive let pRecTy = lam recTy. switch recTy
      case NoRec p then f p
      case Rec _ then "<rec>"
      case RLater r then cons '*' (ptypeCToString pRecTy r)
      end in
    let pBinding = lam pair.
      match pair with (conName, recTy) in
      join [nameGetStr conName, " ", pRecTy recTy] in
    join ["(", strJoin " + " (map pBinding (mapBindings x)), ")"]

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

  sem recTyAsPType : PType -> RecTy PType -> PType
  sem recTyAsPType recPoint =
  | NoRec p -> p
  | Rec _ -> recPoint
  | RLater p -> PLater (mapPTypeC (recTyAsPType recPoint) p)

  sem ptypeAsPureType : PType -> PureType
  sem ptypeAsPureType =
  | PLater x -> PureTypeC (mapPTypeC ptypeAsPureType x)
  | PHere x -> x.ty
  | PNever x -> PureTypeA x

  type PScope =
    { functionDefinitions : Map Name {params : [Name], mayBeRecursive : Bool, body : Expr}
    , valueScope : Map Name PType
    , conScope : Map Name (RecTy ())
    }

  type PState =
    { specializations : Map Name (Map [PType] (Name, PType, Option DeclLetRecord))
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
  | TempFix ((Expr -> Expr) -> Expr -> Expr)

  sem isAtomic =
  | TempLam _ -> false
  | TempFix _ -> false

  sem pprintCode indent env =
  | TempLam f ->
    let x = nameSym "x" in
    match pprintVarName env x with (env, str) in
    match pprintCode (pprintIncr indent) env (f (nvar_ x)) with (env, body) in
    ( env
    , join ["lam ", str, ".", pprintNewline (pprintIncr indent), body]
    )
  | TempFix f ->
    let x = nameSym "x" in
    let fName = nameSym "f" in
    match pprintVarName env fName with (env, fStr) in
    match pprintVarName env x with (env, xStr) in
    match pprintCode (pprintIncr indent) env (f (app_ (nvar_ fName)) (nvar_ x)) with (env, body) in
    ( env
    , join ["recursive let ", fStr, " = lam ", xStr, ".", pprintNewline (pprintIncr indent), body, " in ", fStr]
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
  | (PRecord l, PRecord r) -> mapCmp rec l r
  | (PUser l, PUser r) ->
    recursive let work = lam l. lam r. switch (l, r)
      case (NoRec l, NoRec r) then rec l r
      case (Rec _, Rec _) then 0
      case (RLater l, RLater r) then _ptyCCmp work (l, r)
      end in
    mapCmp work l r
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
  | (PLater (PUser l), PLater (PUser r)) ->
    recursive let lubRecTy = lam l. lam r. switch (l, r)
      case (NoRec l, NoRec r) then NoRec (lubPType l r)
      case (Rec _, Rec _) then Rec ()
      case (RLater l, RLater r) then RLater (map2PTypeC lubRecTy l r)
      end in
    PLater (PUser (mapMerge (optionOrWith lubRecTy) l r))

  sem ensureWrapped : PType -> PType
  sem ensureWrapped = | ty ->
    PHere {wrapped = Wrapped (), ty = ptypeAsPureType ty}

  sem underDecls : (Expr -> Expr) -> Expr -> Expr
  sem underDecls f =
  | TmDecl x -> TmDecl {x with inexpr = underDecls f x.inexpr}
  | tm -> f tm

  sem adjustWrapping : (PType, PType) -> Expr -> Expr
  sem adjustWrapping = | x ->
    let f = _adjustWrapping x in
    printLn (join ["adjust ", ptypeToString x.0, " to ", ptypeToString x.1, " by ", expr2str (TempLam f)]);
    underDecls f

  sem _adjustWrappingC : Bool -> PTypeC (Expr -> Expr) -> Expr -> Expr
  sem _adjustWrappingC shouldWrap =
  | PSeq f ->
    if shouldWrap
    then _app (_app (uconst_ (CPTraverseSeq ())) (TempLam f))
    else _app (_app (uconst_ (CMap ())) (TempLam f))
  | PRecord adjustments ->
    let isId = if shouldWrap then isPure else isIdentity in
    let pure = if shouldWrap then app_ (uconst_ (CPPure ())) else lam x. x in
    let apply = if shouldWrap then _apply else _app in
    if mapAll isId adjustments then pure else
    recursive let construct = lam prev. lam remaining.
      match remaining with [key] ++ remaining
      then TempLam (lam tm. construct (snoc prev (key, tm)) remaining)
      else TmRecord
        { bindings = mapFromSeq cmpSID prev
        , ty = tyunknown_
        , info = NoInfo ()
        } in
    let construct = pure (construct [] (mapKeys adjustments)) in
    lam tm.
      match tm with TmRecord x then
        let args = mapValues (mapIntersectWith (lam adjust. lam tm. adjust tm) adjustments x.bindings) in
        foldl apply construct args
      else
        match
          match tm with TmVar _ then (lam x. x, tm)
          else let n = nameSym "x" in (bind_ (nulet_ n tm), withType (tyTm tm) (nvar_ n))
        with (oBind, target) in
        oBind (foldl apply construct (mapValues (mapMapWithKey (lam sid. lam f. f (recordproj_ (sidToString sid) target)) adjustments)))
  | PDist f ->
    -- OPT(vipa, 2025-11-04): Could probably just skip the extra check
    (if if shouldWrap then isPure f else isIdentity f
     then ()
     else error (join ["We somehow have a non-pure type parameter to a PDist, can't handle that: ", bool2string shouldWrap, " ", expr2str (TempLam f)]));
    if shouldWrap
    then app_ (uconst_ (CPPure ()))
    else lam tm. tm
  | PUser adjustments ->
    let isId = if shouldWrap then isPure else isIdentity in
    let pure = if shouldWrap then app_ (uconst_ (CPPure ())) else lam x. x in
    let apply = if shouldWrap then _apply else _app in
    let f = lam allId. lam f. if allId then isId f else false in
    let allId = mapFoldWithKey
      (lam allId. lam. lam r. if allId then foldRecTy f true r else false) true adjustments in
    if allId then pure else
    let f = lam rec.
      recursive let work = lam recTy. switch recTy
        case NoRec adj then adj
        case Rec _ then rec
        case RLater x then _adjustWrappingC shouldWrap (mapPTypeC work x)
        end in
      let workTop = lam conName. lam recTy. lam tm.
        apply (pure (TempLam (nconapp_ conName))) (work recTy tm) in
      _switchExhaustive (mapMapWithKey workTop adjustments) in
    _app (TempFix f)
  | l ->
    error (join ["Missing case for _adjustWrappingC: ", ptypeCToString (lam. "_") l])

  sem _adjustWrapping : (PType, PType) -> Expr -> Expr
  sem _adjustWrapping =
  | (PNever _, PNever _) -> lam x. x
  | (PNever _, PHere {wrapped = Wrapped _}) -> app_ (uconst_ (CPPure ()))
  | (PHere {wrapped = Unused _}, _) -> lam x. x
  | (PHere {wrapped = Wrapped _}, PHere {wrapped = Wrapped _}) -> lam x. x
  | (PHere {wrapped = Wrapped _}, PLater _) -> lam tm. errorSingle [infoTm tm] "Tried to convert a value to a less wrapped value, which is impossible"
  | (PLater x, PHere {wrapped = Wrapped _, ty = PureTypeC y}) ->
    let y = mapPTypeC (lam ty. PHere {wrapped = Wrapped (), ty = ty}) y in
    _adjustWrappingC true (map2PTypeC (lam l. lam r. _adjustWrapping (l, r)) x y)
  | (PLater x, PLater y) ->
    _adjustWrappingC false (map2PTypeC (lam l. lam r. _adjustWrapping (l, r)) x y)
  | (l, r) -> error (join ["Missing case in _adjustWrapping: ", ptypeToString l, ", ", ptypeToString r])

  sem tyToPTypeX : all x. (PTypeA -> x) -> (PTypeC x -> x) -> (Type -> [Type] -> x) -> Type -> x
  sem tyToPTypeX atom composite custom =
  | TyDist x -> composite (PDist (tyToPTypeX atom composite custom x.ty))
  | TyFloat _ -> atom (PFloat ())
  | TyBool _ -> atom (PBool ())
  | TyInt _ -> atom (PInt ())
  | TyChar _ -> atom (PChar ())
  | TySeq x -> composite (PSeq (tyToPTypeX atom composite custom x.ty))
  | TyRecord x -> composite (PRecord (mapMap (tyToPTypeX atom composite custom) x.fields))
  | ty & (TyApp _ | TyCon _ | TyVar _) ->
    match getTypeArgs ty with (ty, tyArgs) in custom ty tyArgs
  | ty -> printLn (getTypeStringCode 0 pprintEnvEmpty ty).1; errorSingle [infoTy ty] "Missing case for tyToPTypeX"

  sem tyToPureType : Type -> PureType
  sem tyToPureType = | ty -> tyToPTypeX (lam x. PureTypeA x) (lam x. PureTypeC x) (lam. lam. PureTypeC (PUser (mapEmpty nameCmp))) ty

  sem tyToPurePType : Type -> PType
  sem tyToPurePType = | ty -> tyToPTypeX (lam x. PNever x) (lam x. PLater x) (lam. lam. PLater (PUser (mapEmpty nameCmp))) ty

  sem _switchExhaustive : Map Name (Expr -> Expr) -> Expr -> Expr
  sem _switchExhaustive cases =
  | tm & TmConApp x ->
    match mapLookup x.ident cases with Some f
    then f x.body
    else error "_switchExhaustive was called with a non-covered case"
  | tm ->
    if mapIsEmpty cases then tm else
    match
      match (mapSize cases, tm) with (1, _) | (_, TmVar _) then (lam x. x, tm) else
      let n = nameSym "target" in
      (bind_ (nulet_ n tm), nvar_ n)
    with (oBind, target) in
    let f = lam acc. lam conName. lam f.
      let n = nameSym "x" in
      match_ target (npcon_ conName (npvar_ n))
        (f (nvar_ n))
        acc in
    oBind (mapFoldWithKey f never_ cases)

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
  -- we have a simple recursive function, just apply it
  | (f & TempFix f2, x & !TmVar _) -> f2 (_app f) x
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

  sem specializeCall : PScope -> PState -> {f : Expr, args : [(Expr, PType)], ret : Type} -> (PState, (Expr, PType))
  sem specializeCall sc st =
  | {f = TmVar x, args = args, ret = retTy} ->
    match unzip args with (args, argTys) in
    match optionBind (mapLookup x.ident st.specializations) (mapLookup argTys) with Some (name, ty, _) then
      (st, (appSeq_ (nvar_ name) args, ty))
    else
      -- NOTE(vipa, 2025-10-21): Not previously specialized, do it.
      let definition =
        match mapLookup x.ident sc.functionDefinitions
        with Some x then x
        else error (join ["Missing entry in functionDefinitions for ", nameGetStr x.ident]) in
      let valueScope = foldl2 (lam m. lam n. lam ty. mapInsert n ty m) sc.valueScope definition.params argTys in
      let name = nameSetNewSym x.ident in
      match
        let sc = {sc with valueScope = valueScope} in
        if definition.mayBeRecursive then
          recursive let speculate = lam guess.
            let spec = mapSingleton (seqCmp ptyCmp) argTys (name, guess, None ()) in
            let st2 = {st with specializations = mapInsertWith mapUnion x.ident spec st.specializations} in
            match specializeExpr sc st2 definition.body with (st2, (body, retTy)) in
            if eqi 0 (ptyCmp guess retTy) then (st2, (body, retTy)) else
            speculate retTy in
          speculate (PHere {wrapped = Unused (), ty = tyToPureType retTy})
        else specializeExpr sc st definition.body
      with (st, (body, retTy)) in
      let def : DeclLetRecord =
        { ident = name
        , tyAnnot = tyunknown_
        , tyBody = tyunknown_
        , body = nulams_ definition.params body  -- TODO(vipa, 2025-10-21): probably resymbolize here
        , info = NoInfo ()
        } in
      let st =
        { specializations = mapInsertWith mapUnion x.ident
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

  sem specializeDeclPre : PScope -> PState -> Decl -> (PScope, PState)
  sem specializeDeclPre sc st =
  | DeclLet x ->
    match specializeExpr sc st x.body with (st, (tm, ty)) in
    let spec = mapSingleton (seqCmp ptyCmp) [] (x.ident, ty, Some {x with body = tm}) in
    ( {sc with valueScope = mapInsert x.ident ty sc.valueScope}
    , {st with specializations = mapInsert x.ident spec st.specializations}
    )
  | DeclLet {ident = ident, body = body & TmLam _} ->
    recursive let work = lam params. lam tm.
      match tm with TmLam x
      then work (snoc params x.ident) x.body
      else {params = params, mayBeRecursive = false, body = tm} in
    ({sc with functionDefinitions = mapInsert ident (work [] body) sc.functionDefinitions}, st)
  | DeclRecLets x ->
    recursive let work = lam params. lam tm.
      match tm with TmLam x
      then work (snoc params x.ident) x.body
      else {params = params, mayBeRecursive = true, body = tm} in
    let f = lam definitions. lam decl.
      mapInsert decl.ident (work [] decl.body) definitions in
    ({sc with functionDefinitions = foldl f sc.functionDefinitions x.bindings}, st)
  | DeclType _ -> (sc, st)
  | DeclConDef x ->
    let deconstructType : Type -> {carried : Type, tyName : Name, retParams : [Type]} = lam ty.
      match inspectType ty with TyArrow {from = carried, to = to} in
      match getTypeArgs to with (TyCon {ident = tyName}, retParams) in
      {carried = carried, tyName = tyName, retParams = retParams} in
    let tyInfo = deconstructType x.tyIdent in
    let distributeC = lam x.
      match mapMPTypeCOption eitherGetRight x with Some ptys
      then Right (PLater ptys)
      else Left (RLater (mapPTypeC (eitherEither (lam x. x) (lam. NoRec ())) x)) in
    let checkRecursion = lam ty. lam tyArgs.
      match ty with TyCon x then
        if nameEq x.ident tyInfo.tyName then
          if eqi 0 (seqCmp cmpType tyInfo.retParams tyArgs)
          then Left (Rec ())
          else errorSingle [x.info] "Found polymorphic recursion in constructor definition, this is not supported for now."
        else Right (PLater (PUser (mapEmpty nameCmp)))
      else Right (PLater (PUser (mapEmpty nameCmp))) in
    recursive let work = lam ty.
      match ty with TyVar _ then Left (NoRec ()) else
      tyToPTypeX (lam x. Right (PNever x)) distributeC checkRecursion ty in
    let recTy = eitherEither (lam x. x) (lam. NoRec ()) (work tyInfo.carried) in
    ({sc with conScope = mapInsert x.ident recTy sc.conScope}, st)
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
  | decl & (DeclType _ | DeclConDef _) -> (st, [decl])
  | decl -> errorSingle [infoDecl decl] "Missing case in specializeDeclPost"

  sem specializeExpr : PScope -> PState -> Expr -> (PState, (Expr, PType))
  sem specializeExpr sc st =
  | TmDecl x ->
    match specializeDeclPre sc st x.decl with (sc, st) in
    match specializeExpr sc st x.inexpr with (st, (tm, ty)) in
    match specializeDeclPost st x.decl with (st, newDecls) in
    (st, (bindall_ newDecls tm, ty))
  | TmApp x ->
    recursive let collectApps = lam acc. lam tm.
      match tm with TmApp x
      then collectApps (cons x.rhs acc) x.lhs
      else (tm, acc) in
    match collectApps [x.rhs] x.lhs with (f, args) in
    match mapAccumL (specializeExpr sc) st args with (st, args) in
    specializeCall sc st {f = f, args = args, ret = x.ty}
  | TmConApp x ->
    let argRecTy = match mapLookup x.ident sc.conScope
      with Some x then x
      else errorSingle [x.info] "Missing con name in conScope" in
    match specializeExpr sc st x.body with (st, (body, bodyTy)) in
    -- NOTE(vipa, 2025-10-27): collect all recursive occurrences of
    -- the type for lub, regardless of whether we're going to wrap the
    -- entire value. This is important to not forget a constructor
    -- that might appear.
    recursive let computeRecTy
      : RecTy () -> PType -> {recOccurs : [PType], recTy : RecTy PType, argTy : PType -> PType, wrapAboveRec : Bool}
      = lam argRecTy. lam bodyTy.
        switch argRecTy
        case NoRec _ then
          {recOccurs = [], recTy = NoRec bodyTy, argTy = lam. bodyTy, wrapAboveRec = false}
        case Rec _ then
          {recOccurs = [bodyTy], recTy = Rec (), argTy = lam bodyTy. bodyTy, wrapAboveRec = isTopWrapped bodyTy}
        case RLater recTy then
          -- NOTE(vipa, 2025-10-27): This will forget any `Unused`
          -- wrapping in the argument when above a `Rec`, not sure if
          -- that's desirable or avoidable
          match unwrapOnce bodyTy with Right ty in
          let res = map2PTypeC computeRecTy recTy ty in
          let isTopWrapped = isTopWrapped bodyTy in
          { recOccurs = foldPTypeC (lam a. lam x. concat a x.recOccurs) [] res
          , recTy = RLater (mapPTypeC (lam x. x.recTy) res)
          , argTy = if isTopWrapped
            then lam ty. ensureWrapped (PLater (mapPTypeC (lam x. x.argTy ty) res))
            else lam ty. PLater (mapPTypeC (lam x. x.argTy ty) res)
          , wrapAboveRec = foldPTypeC (lam a. lam x. or a x.wrapAboveRec) isTopWrapped res
          }
        end
    in
    let res = computeRecTy argRecTy bodyTy in
    let recTy = PLater (PUser (mapSingleton nameCmp x.ident res.recTy)) in
    let recTy = foldl lubPType recTy res.recOccurs in
    let argTy = res.argTy recTy in
    if res.wrapAboveRec then
      ( st
      , ( _map (TempLam (lam body. TmConApp {x with body = body})) (adjustWrapping (bodyTy, ensureWrapped argTy) body)
        , ensureWrapped recTy
        )
      )
    else
      (st, (nconapp_ x.ident (adjustWrapping (bodyTy, argTy) body), recTy))
  | TmAssume x ->
    match specializeExpr sc st x.dist with (st, (dist, distTy)) in
    let elemTy =
      match ensureWrapped distTy with PHere {ty = PureTypeC (PDist x)} in
      PHere {wrapped = Wrapped (), ty = x} in
    let dist = adjustWrapping (distTy, ensureWrapped distTy) dist in
    let tm = app_ (uconst_ (CPAssume ())) dist in
    (st, (tm, elemTy))
  | TmDist x ->
    match mapAccumL (specializeExpr sc) st (distParams x.dist) with (st, args) in
    let l =
      recursive let work = lam prev. lam ps.
        match ps with [p] ++ ps
        then TempLam (lam tm. work (snoc prev tm) ps)
        else TmDist {x with dist = distWithParams x.dist prev} in
      work [] args in
    specializeCall sc st {f = l, args = args, ret = x.ty}
  | tm & TmVar x ->
    match mapLookup x.ident sc.valueScope with Some ty
    then (st, (tm, ty))
    else errorSingle [x.info] "Missing entry in valueScope"
  | TmSeq x ->
    match mapAccumL (specializeExpr sc) st x.tms with (st, tms) in
    match unzip tms with (tms, tmTys) in
    let elemTy = match tmTys with [ty] ++ tmTys
      then foldl lubPType ty tmTys
      else match unwrapType x.ty with TySeq x in PHere {wrapped = Unused (), ty = tyToPureType x.ty} in
    let tms = zipWith (lam tmTy. adjustWrapping (tmTy, elemTy)) tmTys tms in
    (st, (TmSeq {x with tms = tms}, PLater (PSeq elemTy)))
  | TmRecord x ->
    match mapMapAccum (lam st. lam. lam tm. specializeExpr sc st tm) st x.bindings with (st, zipped) in
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
    match specializeExpr sc st x.target with (st, (target, targetTy)) in
    let thnSc = addMatchNames sc (unwrapOnce targetTy, x.pat) in
    match specializeExpr thnSc st x.thn with (st, (thn, thnTy)) in
    match specializeExpr sc st x.els with (st, (els, elsTy)) in
    let lubTy = lubPType thnTy elsTy in
    if isTopWrapped targetTy then
      -- NOTE(vipa, 2025-11-03): We're matching on a wrapped value,
      -- i.e., the return must be wrapped
      let pureType = tyToPurePType x.ty in
      let retTy = ensureWrapped pureType in
      if isPureIsh lubTy then
        -- NOTE(vipa, 2025-10-23): Both branches are otherwise pure,
        -- i.e., we can make this a `map`
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
      -- NOTE(vipa, 2025-11-03): The target is pure, i.e., we can
      -- adjust the branches individually
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

  sem addMatchNames : PScope -> (Either PTypeA (PTypeC PType), Pat) -> PScope
  sem addMatchNames sc =
  | (_, PatBool _) -> sc
  | (_, PatInt _) -> sc
  | (_, PatChar _) -> sc
  | (Right (PRecord ty), PatRecord pat) ->
    let f = lam ty. lam pat.
      match pat with PatNamed {ident = PName ident}
      then lam acc. mapInsert ident ty acc
      else lam acc. acc in
    let valueScope = mapFoldWithKey (lam acc. lam. lam f. f acc) sc.valueScope (mapIntersectWith f ty pat.bindings) in
    {sc with valueScope = valueScope}
  | (Right (PSeq ty), PatSeqTot p) ->
    let f = lam acc. lam p.
      match p with PatNamed {ident = PName ident}
      then mapInsert ident ty acc
      else acc in
    {sc with valueScope = foldl f sc.valueScope p.pats}
  | (Right (PSeq ty), PatSeqEdge p) ->
    let f = lam acc. lam p.
      match p with PatNamed {ident = PName ident}
      then mapInsert ident ty acc
      else acc in
    let valueScope = foldl f sc.valueScope p.prefix in
    let valueScope = foldl f valueScope p.postfix in
    let valueScope = match p.middle with PName ident
      then mapInsert ident (PLater (PSeq ty)) valueScope
      else valueScope in
    {sc with valueScope = valueScope}
  | (Right (recPoint & PUser ty), PatCon p) ->
    match mapLookup p.ident ty with Some recTy then
      match p.subpat with PatNamed {ident = PName ident} then
        {sc with valueScope = mapInsert ident (recTyAsPType (PLater recPoint) recTy) sc.valueScope}
      else sc
    else error "Pattern matched on a constructor that cannot appear here. Should deal with this, but later."
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
  { specializations = mapEmpty nameCmp
  } in
let initScope =
  { functionDefinitions = mapEmpty nameCmp
  , valueScope = mapEmpty nameCmp
  , conScope = mapEmpty nameCmp
  } in
match specializeExpr initScope initState ast with (_, (ast, _)) in
printLn (pprintCode 0 pprintEnvEmpty ast).1
