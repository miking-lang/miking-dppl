-- This file provides a transformation from CorePPL to an idealized
-- PVal API.
--
-- Assumptions:
-- * All pattern matches are shallow, i.e., all matches look only at
--   the top-most layer of structure in their target, and there are no
--   'and', 'or', or 'not' patterns,
-- * All lambdas are let-bound, and all references to them appear
--   fully applied.
--
-- The idealized PVal API assumes that PVal can be used directly as a
-- regular monad, plus builtins for assume and weight; all operations
-- are represented as new `Const` values. The primary difference with
-- the actual API is the absence of a state value that needs to be
-- passed between calls to `pure`, `map`, etc.

include "these.mc"
include "coreppl::parser.mc"
include "mexpr/shallow-patterns.mc"
include "mexpr/hoas.mc"
include "mexpr/inline-single-use-simple.mc"

lang IdealizedPValTransformation = Dist + Assume + Weight + Observe + TempLamAst + AppTypeUtils
  syn Wrap =
  | Wrapped
  | Unused

  syn PTypeC rec =
  | PRecord (Map SID rec)
  | PSeq rec
  | PDist rec
  -- NOTE(vipa, 2025-10-24): We assume recursion for types is direct
  -- (no mutual recursion) and non-polymorphic (exact same
  -- parameterization). Constructors absent from the `Map` are
  -- `Unused`.
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

  sem map2PTypeCExn : all x. all y. all z. (x -> y -> z) -> PTypeC x -> PTypeC y -> PTypeC z
  sem map2PTypeCExn f l = | r -> map2PTypeC (lam t. match t with These (x, y) in f x y) l r

  sem map2PTypeC : all x. all y. all z. (These x y -> z) -> PTypeC x -> PTypeC y -> PTypeC z
  sem map2PTypeC f l = | r -> map2PTypeC_ f (l, r)

  sem map2PTypeC_ : all x. all y. all z. (These x y -> z) -> (PTypeC x, PTypeC y) -> PTypeC z
  sem map2PTypeC_ f =
  | (lty & PRecord l, rty & PRecord r) ->
    let showTy = lam ty. ptypeCToString (lam. "_") ty in
    let f = lam l. lam r.
      match (l, r) with (Some l, Some r) then Some (f (These (l, r)))
      else error (join ["Tried to call map2PTypeC with two records with different sets of fields: ", showTy lty, " and ", showTy rty]) in
    PRecord (mapMerge f l r)
  | (PSeq l, PSeq r) -> PSeq (f (These (l, r)))
  | (PDist l, PDist r) -> PDist (f (These (l, r)))
  | (lty & PUser l, rty & PUser r) ->
    let showTy = lam ty. ptypeCToString (lam. "_") ty in
    recursive let work : These (RecTy x) (RecTy y) -> RecTy z = lam t.
      -- NOTE(vipa, 2025-11-11): I could do rank-2 polymorphism here
      -- to only pass one mkThese function, but since the function is
      -- short it's easier to just pass it twice.
      let handleOne = lam mkThese1. lam mkThese2. lam recTy. switch recTy
        case NoRec x then NoRec (f (mkThese1 x))
        case Rec _ then Rec ()
        case RLater x then RLater (mapPTypeC (lam x. work (mkThese2 x)) x)
        end in
      let handleTwo = lam l : RecTy x. lam r : RecTy y. switch (l, r)
        case (NoRec l, NoRec r) then NoRec (f (These (l, r)))
        case (Rec _, Rec _) then Rec ()
        case (RLater l, RLater r) then RLater (map2PTypeC work l r)
        end in
      theseThese (handleOne (lam x. This x) (lam x. This x)) (handleOne (lam x. That x) (lam x. That x)) handleTwo t in
    let f = lam l. lam r. switch (l, r)
      case (Some l, Some r) then Some (work (These (l, r)))
      case (Some l, None _) then Some (work (This l))
      case (None _, Some r) then Some (work (That r))
      end in
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

  sem purifyPType : PType -> PType
  sem purifyPType =
  | PNever x -> PNever x
  | PLater x -> PLater (mapPTypeC purifyPType x)
  | PHere {wrapped = Wrapped _, ty = ty} -> pureToPType ty
  | pty & PHere {wrapped = Unused _} -> pty

  type PScope =
    { functionDefinitions : Map Name {params : [Name], mayBeRecursive : Bool, body : Expr, depth : Int}
    , depth : Int
    , valueScope : Map Name (Name, PType)
    , conScope : Map Name (RecTy ())
    }

  type PState =
    { specializations : Map Name (Map [PType] {ident : Name, ty : PType, decl : Option DeclLetRecord, depth : Int})
    }

  syn Const =
  | CPAssume
  | CPWeight
  | CPPure
  | CPMap
  | CPApply
  | CPJoin
  | CPTraverseSeq
  | CPExport

  sem getConstStringCode indent =
  | CPAssume _ -> "px_assume"
  | CPWeight _ -> "px_weight"
  | CPPure _ -> "px_pure"
  | CPMap _ -> "px_map"
  | CPApply _ -> "px_apply"
  | CPJoin _ -> "px_join"
  | CPExport _ -> "px_export"
  | CPTraverseSeq _ -> "px_traverseSeq"

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
      case (RLater l, RLater r) then RLater (map2PTypeCExn lubRecTy l r)
      end in
    PLater (PUser (mapMerge (optionOrWith lubRecTy) l r))
  | (l, r) ->
    printLn (join ["Missing case in _lubPType: (", ptypeToString l, ", ", ptypeToString r, ")"]);
    never

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
    -- printLn (join ["adjust ", ptypeToString x.0, " to ", ptypeToString x.1, " by ", expr2str (TempLam f)]);
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
  | (fr & PLater x, to & PHere {wrapped = Wrapped _, ty = PureTypeC y}) ->
    let y = mapPTypeC (lam ty. PHere {wrapped = Wrapped (), ty = ty}) y in
    let f = lam t. switch t
      case These pair then _adjustWrapping pair
      case This _ then error (join ["Cannot adjust from ", ptypeToString fr, " to ", ptypeToString to, ", we'd drop a constructor"])
      case That _ then lam x. x
      end in
    _adjustWrappingC true (map2PTypeC f x y)
  | (fr & PLater x, to & PLater y) ->
    let f = lam t. switch t
      case These pair then _adjustWrapping pair
      case This _ then error (join ["Cannot adjust from ", ptypeToString fr, " to ", ptypeToString to, ", we'd drop a constructor"])
      case That _ then lam x. x
      end in
    _adjustWrappingC false (map2PTypeC f x y)
  | (l, r) -> error (join ["Missing case in _adjustWrapping: ", ptypeToString l, ", ", ptypeToString r])

  sem _tyToPTypeX : all x. (PTypeA -> x) -> (PTypeC Type -> x) -> (Type -> [Type] -> x) -> Type -> x
  sem _tyToPTypeX atom composite custom =
  | TyDist x -> composite (PDist x.ty)
  | TyFloat _ -> atom (PFloat ())
  | TyBool _ -> atom (PBool ())
  | TyInt _ -> atom (PInt ())
  | TyChar _ -> atom (PChar ())
  | TySeq x -> composite (PSeq x.ty)
  | TyRecord x -> composite (PRecord x.fields)
  | ty & (TyApp _ | TyCon _ | TyVar _) ->
    match getTypeArgs ty with (ty, tyArgs) in custom ty tyArgs
  | ty -> errorSingle [infoTy ty] (concat "Missing case for _tyToPTypeX: " (getTypeStringCode 0 pprintEnvEmpty ty).1)

  sem tyToPTypeX : all x. (PTypeA -> x) -> (PTypeC x -> x) -> (Type -> [Type] -> x) -> Type -> x
  sem tyToPTypeX atom composite custom = | ty ->
    _tyToPTypeX atom (lam pty. composite (mapPTypeC (tyToPTypeX atom composite custom) pty)) custom ty

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

  sem isPure : (Expr -> Expr) -> Bool
  sem isPure = | f ->
    let n = nameSym "x" in
    match f (nvar_ n) with TmApp {lhs = TmConst {val = CPPure _}, rhs = TmVar {ident = ident}}
    then nameEq ident n
    else false

  sem _app_ =
  -- p_map id = id
  | (f & TmConst {val = CPMap _}, x & TempLam f2) ->
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

  sem _map : Expr -> Expr -> Expr
  sem _map f = | x -> _app (_app (uconst_ (CPMap ())) f) x

  sem _apply : Expr -> Expr -> Expr
  sem _apply f = | a -> _app (_app (uconst_ (CPApply ())) f) a

  sem _join : Expr -> Expr
  sem _join = | x -> _app (uconst_ (CPJoin ())) x

  syn SpecializeCallKind =
  | SCKInflexible ()
  | SCKPolyFlexible Type
  | SCKDefFlexible (Name, {params : [Name], mayBeRecursive : Bool, body : Expr, depth : Int})
  sem specializeCall : PScope -> PState -> {f : Expr, args : [(Expr, PType)], ret : Type} -> (PState, (Expr, PType))
  sem specializeCall sc st =
  | {f = f & TmVar x, args = args, ret = retTy} ->
    match optionBind (mapLookup x.ident st.specializations) (mapLookup (map (lam x. x.1) args)) with Some {ident = name, ty = ty} then
      (st, (appSeq_ (nvar_ name) (map (lam x. x.0) args), ty))
    else match mapLookup x.ident sc.functionDefinitions with Some definition
    then _specializeCall sc st f args retTy (SCKDefFlexible (x.ident, definition))
    else _specializeCall sc st f args retTy (SCKInflexible ())
  | {f = f & TmConst {val = c}, args = args, ret = retTy} ->
    match tyConst c with ty & TyAll _
    then _specializeCall sc st f args retTy (SCKPolyFlexible ty)
    else _specializeCall sc st f args retTy (SCKInflexible ())
  | {f = f & TempLam _, args = args, ret = retTy} ->
    _specializeCall sc st f args retTy (SCKInflexible ())

  -- OPT(vipa, 2025-11-25): This currently checks if at least one
  -- argument is wrapped further than a polymorphic instantiation of
  -- the function could absorb and, if so, wraps *all* arguments. This
  -- is a bit overly cautious in at least two cases:
  -- * `addf a b` where `a` is pure and `b` is wrapped. This could be
  --   `map (addf a) b` rather than `apply (map addf (pure a)) b`. Of
  --   course, the latter will simplify to the former with `_app`, so
  --   it's not really a problem per se.
  -- * `get [x] i` where `x` and `i` are wrapped. This could be `join
  --   (map (get [x]) i)` rather than `apply (map get (traverse id
  --   [x])) i`. The latter does *not* simplify to the former.
  sem _specializeCall : PScope -> PState -> Expr -> [(Expr, PType)] -> Type -> SpecializeCallKind -> (PState, (Expr, PType))
  sem _specializeCall sc st tm args ret =
  | SCKInflexible _ ->
    let pureRet = tyToPurePType ret in
    -- NOTE(vipa, 2025-12-12): Ensure we're fully applying the
    -- function, since it might be an external, which may not appear
    -- in any other situation
    let tm = foldr (lam. lam tm. TempLam (_app tm)) tm args in
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
  | SCKDefFlexible (ident, definition) ->
    match unzip args with (args, argTys) in
    let params = map (lam n. (n, nameSetNewSym n)) definition.params in
    let valueScope = foldl2 (lam m. lam n. lam ty. mapInsert n.0 (n.1, ty) m) sc.valueScope params argTys in
    let name = nameSetNewSym ident in
    match
      let sc = {sc with valueScope = valueScope} in
      if definition.mayBeRecursive then
        recursive let speculate = lam guess.
          let spec = mapSingleton (seqCmp ptyCmp) argTys
            {ident = name, ty = guess, decl = None (), depth = definition.depth} in
          let localSpecializations = mapMap (mapFilter (lam x. gti x.depth definition.depth)) st.specializations in
          let nonLocalSpecializations = mapMap (mapFilter (lam x. leqi x.depth definition.depth)) st.specializations in
          let st2 = {st with specializations = mapInsertWith mapUnion ident spec nonLocalSpecializations} in
          match specializeExpr sc st2 definition.body with (st2, (body, retTy)) in
          if eqi 0 (ptyCmp guess retTy) then ({st2 with specializations = mapUnionWith mapUnion localSpecializations st2.specializations}, (body, retTy)) else
          speculate retTy in
        speculate (PHere {wrapped = Unused (), ty = tyToPureType ret})
      else specializeExpr sc st definition.body
    with (st, (body, retTy)) in
    let def : DeclLetRecord =
      { ident = name
      , tyAnnot = tyunknown_
      , tyBody = tyunknown_
      , body = nulams_ (map (lam x. x.1) params) body
      , info = NoInfo ()
      } in
    let st =
      let spec =
        { ident = name
        , ty = retTy
        , decl = Some def
        , depth = definition.depth
        } in
      { specializations = mapInsertWith mapUnion ident
        (mapSingleton (seqCmp ptyCmp) argTys spec)
        st.specializations
      } in
    (st, (foldl app_ (nvar_ name) args, retTy))
  | SCKPolyFlexible polyType ->
    match unzip args with (args, argPTys) in
    let retPTy = tyToPurePType ret in
    recursive let tyArgs = lam args. lam ty.
      match unwrapType ty with TyArrow x
      then tyArgs (snoc args x.from) x.to
      else (args, ty) in
    match tyArgs [] (stripTyAll polyType).1 with (argTys, retTy) in
    -- Collect the `PType`s used to instantiate each `TyVar`,
    -- combining them with `lubPType` since different `PType`s might
    -- appear at different places. Additionally, find wrappedness
    -- that cannot be absorbed by a `TyVar`.
    let mergeCollectInfo = lam l. lam r. (or l.0 r.0, mapUnionWith lubPType l.1 r.1) in
    let collectAtom = lam pty. lam ty. (isTopWrapped pty, mapEmpty nameCmp) in
    let collectCustom = lam pty. lam tyCon. lam tyArgs. switch tyCon
      case TyVar x then
        if null tyArgs
        then (false, mapSingleton nameCmp x.ident pty)
        else errorSingle [x.info] "Found a type application of a type variable, that's not supported presently."
      case TyCon x then
        errorSingle [x.info] "This transformation does not support any TmConst's with TyCon's in their types."
      end in
    recursive let collectComposite = lam pty. lam ty.
      let wrappedHere = isTopWrapped pty in
      match unwrapOnce pty with Right pty in
      let rec = map2PTypeCExn (lam pty. _tyToPTypeX (collectAtom pty) (collectComposite pty) (collectCustom pty)) pty ty in
      foldPTypeC mergeCollectInfo (wrappedHere, mapEmpty nameCmp) rec in
    let collectTy = lam pty. lam ty.
      _tyToPTypeX (collectAtom pty) (collectComposite pty) (collectCustom pty) ty in
    let f = lam acc. lam pty. lam ty. mergeCollectInfo acc (collectTy pty ty) in
    match foldl2 f (collectTy retPTy retTy) argPTys argTys with (needsFullWrap, varPTys) in
    -- Construct `PType`s using the collected `TyVar` `PType`s. We
    -- need to do this even if `needsFullWrap` is true in case a
    -- `TyVar` was instantiated with a `PUser` type with
    -- constructors.
    let constructCustom = lam tyCon. lam.
      match tyCon with TyVar x in mapFindExn x.ident varPTys in
    let construct = tyToPTypeX (lam x. PNever x) (lam x. PLater x) constructCustom in
    let targetArgPTys = if needsFullWrap
      then map (lam ty. ensureWrapped (construct ty)) argTys
      else map construct argTys in
    let retTy = if needsFullWrap
      then ensureWrapped (construct retTy)
      else construct retTy in
    let args = zipWith adjustWrapping (zip argPTys targetArgPTys) args in
    let pure = if needsFullWrap then app_ (uconst_ (CPPure ())) else lam x. x in
    let apply = if needsFullWrap then _apply else _app in
    (st, (foldl apply (pure tm) args, retTy))

  sem specializeDeclPre : PScope -> PState -> Decl -> (PScope, PState)
  sem specializeDeclPre sc st =
  | DeclLet x ->
    match specializeExpr {sc with depth = subi sc.depth 1} st x.body with (st, (tm, ty)) in
    let n = nameSetNewSym x.ident in
    let spec = mapSingleton (seqCmp ptyCmp) []
      { ident = n
      , ty = ty
      , decl = Some {x with ident = n, body = tm}
      , depth = sc.depth
      } in
    ( {sc with valueScope = mapInsert x.ident (n, ty) sc.valueScope}
    , {st with specializations = mapInsert x.ident spec st.specializations}
    )
  | DeclLet {ident = ident, body = body & TmLam _} ->
    recursive let work = lam params. lam tm.
      match tm with TmLam x
      then work (snoc params x.ident) x.body
      else {params = params, mayBeRecursive = false, body = tm, depth = sc.depth} in
    ({sc with functionDefinitions = mapInsert ident (work [] body) sc.functionDefinitions}, st)
  | DeclRecLets x ->
    recursive let work = lam params. lam tm.
      match tm with TmLam x
      then work (snoc params x.ident) x.body
      else {params = params, mayBeRecursive = true, body = tm, depth = sc.depth} in
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
  | DeclExt _ -> (sc, st)
  | decl -> errorSingle [infoDecl decl] "Missing case in specializeDeclPre"

  sem specializeDeclPost : PState -> Decl -> (PState, [Decl])
  sem specializeDeclPost st =
  | DeclLet {ident = ident} ->
    match mapLookup ident st.specializations with Some specs then
      let addDefinition = lam a. lam. lam spec.
        match spec.decl with Some decl
        then snoc a (DeclLet decl)
        else error (join ["Missing definition for specialization in let for ", nameGetStr ident]) in
      ({st with specializations = mapRemove ident st.specializations}, mapFoldWithKey addDefinition [] specs)
    else (st, [])
  | DeclRecLets x ->
    let specs = join (map mapValues (mapOption (lam d. mapLookup d.ident st.specializations) x.bindings)) in
    let specs = mapOption (lam x. x.decl) specs in
    let specializations = foldl (lam acc. lam decl. mapRemove decl.ident acc) st.specializations x.bindings in
    ({st with specializations = specializations}, [DeclRecLets {x with bindings = specs}])
  | decl & (DeclType _ | DeclConDef _ | DeclExt _) -> (st, [decl])
  | decl -> errorSingle [infoDecl decl] "Missing case in specializeDeclPost"

  sem specializeExprReturn : PScope -> PState -> Expr -> Expr
  sem specializeExprReturn sc st = | tm ->
    match specializeExpr sc st tm with (st, (tm, ty)) in
    let tm = adjustWrapping (ty, ensureWrapped ty) tm in
    app_ (uconst_ (CPExport ())) tm

  sem specializeExpr : PScope -> PState -> Expr -> (PState, (Expr, PType))
  sem specializeExpr sc st =
  | TmDecl x ->
    match specializeDeclPre {sc with depth = addi sc.depth 1} st x.decl with (sc, st) in
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
    (match x.ty with TyUnknown _ then warnSingle [x.info] (concat "no type on this app: " (expr2str (TmApp x))) else ());
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
          let res = map2PTypeCExn computeRecTy recTy ty in
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
  | TmWeight x ->
    match specializeExpr sc st x.weight with (st, (weight, weightTy)) in
    let weight = adjustWrapping (weightTy, ensureWrapped weightTy) weight in
    let tm = app_ (uconst_ (CPWeight ())) weight in
    (st, (tm, PLater (PRecord (mapEmpty cmpSID))))
  | TmObserve x ->
    let w = withType tyfloat_ (appf2_ (TempLam (lam v. app_ (uconst_ (CDistLogObserve ())) v)) x.dist x.value) in
    specializeExpr sc st (TmWeight {weight = w, ty = x.ty, info = x.info})
  | TmDist x ->
    match mapAccumL (specializeExpr sc) st (distParams x.dist) with (st, args) in
    let l =
      recursive let work = lam prev. lam ps.
        match ps with [p] ++ ps
        then TempLam (lam tm. work (snoc prev tm) ps)
        else TmDist {x with dist = distWithParams x.dist prev} in
      work [] args in
    specializeCall sc st {f = l, args = args, ret = x.ty}
  | TmVar x ->
    match mapLookup x.ident sc.valueScope with Some (n, ty)
    then (st, (TmVar {x with ident = n}, ty))
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
  | TmMatch x ->
    -- NOTE(vipa, 2025-10-22): We assume that the pattern is shallow,
    -- but not just a wildcard
    match specializeExpr sc st x.target with (st, (target, targetTy)) in
    match
      switch (isTopWrapped targetTy, x.target)
      case (true, TmVar x) then
        let n = nameSetNewSym x.ident in
        (Some n, {sc with valueScope = mapInsert x.ident (n, purifyPType targetTy) sc.valueScope})
      case (true, _) then
        let n = nameSym "target" in
        (Some n, sc)
      case (false, _) then
        (None (), sc)
      end
    with (oTargetName, sc) in
    match addMatchNames sc (unwrapOnce targetTy, x.pat) with (thnSc, pat) in
    match specializeExpr thnSc st x.thn with (st, (thn, thnTy)) in
    match
      -- NOTE(vipa, 2025-11-10): We special-case `match ... in` when
      -- it comes to PTypes
      match x.els with TmNever _
      then (st, (x.els, thnTy))
      else specializeExpr sc st x.els
    with (st, (els, elsTy)) in
    let lubTy = lubPType thnTy elsTy in
    match oTargetName with Some targetName then
      -- We're matching on a wrapped value, i.e., the return must be wrapped
      let retTy = ensureWrapped lubTy in
      let mkLam = lam adjThn. lam adjEls. lam target.
        bind_ (nulet_ targetName target)
          ( TmMatch
            { x with target = nvar_ targetName
            , pat = pat
            , thn = adjThn thn
            , els = adjEls els
            }
          ) in
      if isPureIsh lubTy then
        -- NOTE(vipa, 2025-10-23): Both branches are otherwise pure,
        -- i.e., we can make this a `map`
        let f = TempLam (mkLam (lam x. x) (lam x. x)) in
        (st, (_map f target, retTy))
      else
        -- NOTE(vipa, 2025-10-23): At least one branch is not pure,
        -- i.e., this needs to be a `bind`
        let f = TempLam (mkLam (adjustWrapping (thnTy, retTy)) (adjustWrapping (elsTy, retTy))) in
        (st, (_join (_map f target), retTy))
    else
      -- NOTE(vipa, 2025-11-03): The target is pure, i.e., we can
      -- adjust the branches individually
      ( st
      , ( TmMatch
          { x with target = target
          , pat = pat
          , thn = adjustWrapping (thnTy, lubTy) thn
          , els = adjustWrapping (elsTy, lubTy) els
          }
        , lubTy
        )
      )
  | TmNever {ty = TyUnknown _, info = info} -> errorSingle [info] "Never without type information"
  | tm & TmNever x -> (st, (tm, PHere {wrapped = Unused (), ty = tyToPureType x.ty}))
  | tm & TmConst {val = CFloat _} -> (st, (tm, PNever (PFloat ())))
  | tm & TmConst {val = CInt _} -> (st, (tm, PNever (PInt ())))
  | tm & TmConst {val = CBool _} -> (st, (tm, PNever (PBool ())))
  | tm -> errorSingle [infoTm tm] "Missing case in specializeExpr"

  sem addMatchNames : PScope -> (Either PTypeA (PTypeC PType), Pat) -> (PScope, Pat)
  sem addMatchNames sc =
  | (_, pat & PatBool _) -> (sc, pat)
  | (_, pat & PatInt _) -> (sc, pat)
  | (_, pat & PatChar _) -> (sc, pat)
  | (Right (PRecord ty), PatRecord pat) ->
    let f = lam ty : PType. lam pat : Pat.
      match pat with PatNamed (p & {ident = PName ident})
      then lam acc. let n = nameSetNewSym ident in (mapInsert ident (n, ty) acc, PatNamed {p with ident = PName n})
      else lam acc. (acc, pat) in
    match mapMapAccum (lam acc. lam. lam f. f acc) sc.valueScope (mapIntersectWith f ty pat.bindings)
      with (valueScope, bindings) in
    ({sc with valueScope = valueScope}, PatRecord {pat with bindings = bindings})
  | (Right (PSeq ty), PatSeqTot p) ->
    let f = lam acc. lam p.
      match p with PatNamed (p & {ident = PName ident})
      then let n = nameSetNewSym ident in (mapInsert ident (n, ty) acc, PatNamed {p with ident = PName n})
      else (acc, p) in
    match mapAccumL f sc.valueScope p.pats with (valueScope, pats) in
    ({sc with valueScope = valueScope}, PatSeqTot {p with pats = pats})
  | (Right (seqTy & PSeq ty), PatSeqEdge p) ->
    let f = lam acc. lam p.
      match p with PatNamed (p & {ident = PName ident})
      then let n = nameSetNewSym ident in (mapInsert ident (n, ty) acc, PatNamed {p with ident = PName n})
      else (acc, p) in
    match mapAccumL f sc.valueScope p.prefix with (valueScope, prefix) in
    match mapAccumL f valueScope p.postfix with (valueScope, postfix) in
    match
      match p.middle with PName ident
      then let n = nameSetNewSym ident in (mapInsert ident (n, PLater seqTy) valueScope, PName n)
      else (valueScope, p.middle)
    with (valueScope, middle) in
    ( {sc with valueScope = valueScope}
    , PatSeqEdge {p with prefix = prefix, postfix = postfix, middle = middle}
    )
  | (Right (recPoint & PUser ty), PatCon p) ->
    match mapLookup p.ident ty with Some recTy then
      match p.subpat with PatNamed (p2 & {ident = PName ident}) then
        let n = nameSetNewSym ident in
        ( {sc with valueScope = mapInsert ident (n, recTyAsPType (PLater recPoint) recTy) sc.valueScope}
        , PatCon {p with subpat = PatNamed {p2 with ident = PName n}}
        )
      else (sc, PatCon p)
    else error "Pattern matched on a constructor that cannot appear here. Should deal with this, but later."
  | (l, r) ->
    errorSingle [infoPat r] (concat "Missing case " (eitherEither ptypeAToString (ptypeCToString ptypeToString) l))
end

lang TestLang = DPPLParser + MExprLowerNestedPatterns + InlineSingleUse + TempLamAst + IdealizedPValTransformation
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
  (pprintCode 0 pprintEnvEmpty ast).1 in

let printFailure = lam l. lam r. strJoin "\n"
  [ concat "    LHS: " (strReplace "\n" "\n         " l)
  , concat "    RHS: " (strReplace "\n" "\n         " r)
  ] in

utest transform
  [ "assume (Gaussian 0.0 1.0)"
  ]
with strJoin "\n"
  [ "p_assume (p_pure (Gaussian 0. 1.))"
  ]
using eqString
else printFailure
in

utest transform
  [ "let x = 0.0 in"
  , "assume (Gaussian x 1.0)"
  ]
with strJoin "\n"
  [ "p_assume (p_pure (Gaussian 0. 1.))"
  ]
using eqString
else printFailure
in

utest transform
  [ "let x = assume (Gaussian 0.0 1.0) in"
  , "assume (Gaussian x 1.0)"
  ]
with strJoin "\n"
  [ "p_assume"
  , "  (p_map"
  , "     (lam x."
  , "        Gaussian x 1.)"
  , "     (p_assume (p_pure (Gaussian 0. 1.))))"
  ]
using eqString
else printFailure
in

utest transform
  [ "addf (assume (Gaussian 0.0 1.0)) (assume (Gaussian 1.0 1.0))"
  ]
with strJoin "\n"
  [ "p_apply"
  , "  (p_map addf (p_assume (p_pure (Gaussian 0. 1.))))"
  , "  (p_assume (p_pure (Gaussian 1. 1.)))"
  ]
using eqString
else printFailure
in

utest transform
  [ "addf (addf (assume (Gaussian 0.0 1.0)) (assume (Gaussian 1.0 1.0))) 2.0"
  ]
with strJoin "\n"
  [ "p_apply"
  , "  (p_map"
  , "     (lam x."
  , "        lam x1."
  , "          addf (addf x x1) 2.)"
  , "     (p_assume (p_pure (Gaussian 0. 1.))))"
  , "  (p_assume (p_pure (Gaussian 1. 1.)))"
  ]
using eqString
else printFailure
in

utest transform
  [ "let f = lam a. addf a 1.0 in"
  , "f 1.0"
  ]
with strJoin "\n"
  [ "addf 1. 1."
  ]
using eqString
else printFailure
in

utest transform
  [ "let f = lam a. addf a 1.0 in"
  , "addf (f 1.0) (f (assume (Gaussian 0.0 1.0)))"
  ]
with strJoin "\n"
  [ "let f = lam a1."
  , "    addf a1 1. in"
  , "let f1 = lam a."
  , "    p_map (lam x."
  , "         addf x 1.) a in"
  , "p_map (addf (f 1.)) (f1 (p_assume (p_pure (Gaussian 0. 1.))))"
  ]
using eqString
else printFailure
in

utest transform
  [ "let f = lam a. assume (Gaussian a 1.0) in"
  , "let g = lam a. f a in"
  , "addf (g 1.0) (g (assume (Gaussian 0.0 1.0)))"
  ]
with strJoin "\n"
  [ "let g = lam a1."
  , "    p_assume (p_pure (Gaussian a1 1.)) in"
  , "let g1 ="
  , "  lam a."
  , "    p_assume (p_map (lam x."
  , "            Gaussian x 1.) a)"
  , "in"
  , "p_apply"
  , "  (p_map addf (g 1.))"
  , "  (g1 (p_assume (p_pure (Gaussian 0. 1.))))"
  ]
using eqString
else printFailure
in

utest transform
  [ "recursive let sum : Float -> Float = lam x. sum (assume (Gaussian x 1.0)) in"
  , "sum 1.0"
  ]
with strJoin "\n"
  [ "recursive"
  , "  let sum = lam x."
  , "      sum1 (p_assume (p_pure (Gaussian x 1.)))"
  , "  let sum1 ="
  , "    lam x1."
  , "      sum1"
  , "        (p_assume (p_map (lam x2."
  , "                 Gaussian x2 1.) x1))"
  , "in"
  , "sum 1."
  ]
using eqString
else printFailure
in

utest transform
  [ "recursive"
  , "  let odd : Float -> Bool = lam x. even (assume (Gaussian x 1.0))"
  , "  let even = lam x. odd x"
  , "in even 1.0"
  ]
with strJoin "\n"
  [ "recursive"
  , "  let even = lam x."
  , "      even1 (p_assume (p_pure (Gaussian x 1.)))"
  , "  let even1 ="
  , "    lam x1."
  , "      even1"
  , "        (p_assume (p_map (lam x2."
  , "                 Gaussian x2 1.) x1))"
  , "in"
  , "even 1."
  ]
using eqString
else printFailure
in

utest transform
  [ "[1.0, 2.0]"
  ]
with strJoin "\n"
  [ "[ 1., 2. ]"
  ]
using eqString
else printFailure
in

utest transform
  [ "[1.0, assume (Gaussian 0.0 1.0)]"
  ]
with strJoin "\n"
  [ "[ p_pure 1.,"
  , "  p_assume (p_pure (Gaussian 0. 1.)) ]"
  ]
using eqString
else printFailure
in

utest transform
  [ "if true then 1 else 2"
  ]
with strJoin "\n"
  [ "match"
  , "  true"
  , "with"
  , "  true"
  , "then"
  , "  1"
  , "else"
  , "  2"
  ]
using eqString
else printFailure
in

utest transform
  [ "if true then assume (Gaussian 0.0 1.0) else 42.0"
  ]
with strJoin "\n"
  [ "match"
  , "  true"
  , "with"
  , "  true"
  , "then"
  , "  p_assume (p_pure (Gaussian 0. 1.))"
  , "else"
  , "  p_pure 42."
  ]
using eqString
else printFailure
in

utest transform
  [ "match (assume (Gaussian 0.0 1.0), 2.0) with (a, b) in addf a b"
  ]
with strJoin "\n"
  [ "match"
  , "  (p_assume (p_pure (Gaussian 0. 1.)), 2.)"
  , "with"
  , "  (field, field1)"
  , "in"
  , "p_map (lam x."
  , "       addf x field1) field"
  ]
using eqString
else printFailure
in

utest transform
  [ "if assume (Bernoulli 0.5) then false else true"
  ]
with strJoin "\n"
  [ "p_map"
  , "  (lam x."
  , "     let target = x in"
  , "     match"
  , "       target"
  , "     with"
  , "       true"
  , "     then"
  , "       false"
  , "     else"
  , "       true)"
  , "  (p_assume (p_pure (Bernoulli 0.5)))"
  ]
using eqString
else printFailure
in

utest transform
  [ "if assume (Bernoulli 0.5) then assume (Bernoulli 0.9) else false"
  ]
with strJoin "\n"
  [ "p_join"
  , "  (p_map"
  , "     (lam x."
  , "        let target = x in"
  , "        match"
  , "          target"
  , "        with"
  , "          true"
  , "        then"
  , "          p_assume (p_pure (Bernoulli 0.9))"
  , "        else"
  , "          p_pure false)"
  , "     (p_assume (p_pure (Bernoulli 0.5))))"
  ]
using eqString
else printFailure
in

utest transform
  [ "if true then (1, 2.0) else (2, assume (Gaussian 0.0 1.0))"
  ]
with strJoin "\n"
  [ "match"
  , "  true"
  , "with"
  , "  true"
  , "then"
  , "  (1, p_pure 2.)"
  , "else"
  , "  (2, p_assume (p_pure (Gaussian 0. 1.)))"
  ]
using eqString
else printFailure
in

utest transform
  [ "if assume (Bernoulli 0.5) then [1.0, 2.0] else [assume (Gaussian 0.0 1.0)]"
  ]
with strJoin "\n"
  [ "p_join"
  , "  (p_map"
  , "     (lam x."
  , "        let target = x in"
  , "        match"
  , "          target"
  , "        with"
  , "          true"
  , "        then"
  , "          p_pure [ 1., 2. ]"
  , "        else"
  , "          p_traverseSeq"
  , "            (lam x1."
  , "               x1)"
  , "            [ p_assume (p_pure (Gaussian 0. 1.)) ])"
  , "     (p_assume (p_pure (Bernoulli 0.5))))"
  ]
using eqString
else printFailure
in

utest transform
  [ "if true then [1., 2.] else [assume (Gaussian 0.0 1.0)]"
  ]
with strJoin "\n"
  [ "match"
  , "  true"
  , "with"
  , "  true"
  , "then"
  , "  [ p_pure 1.,"
  , "    p_pure 2. ]"
  , "else"
  , "  [ p_assume (p_pure (Gaussian 0. 1.)) ]"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Either a b in"
  , "con Left : all a. all b. a -> Either a b in"
  , "con Right : all a. all b. b -> Either a b in"
  , "if true then Left 1 else Right 2.0"
  ]
with strJoin "\n"
  [ "type Either a b in"
  , "con Left: all a1. all b1. a1 -> Either a1 b1 in"
  , "con Right: all a2. all b2. b2 -> Either a2 b2 in"
  , "match"
  , "  true"
  , "with"
  , "  true"
  , "then"
  , "  Left"
  , "    1"
  , "else"
  , "  Right"
  , "    2."
  ]
using eqString
else printFailure
in

utest transform
  [ "type Either a b in"
  , "con Left : all a. all b. a -> Either a b in"
  , "con Right : all a. all b. b -> Either a b in"
  , "if true then Left 1.0 else let x = assume (Gaussian 1.0 0.0) in Left x"
  ]
with strJoin "\n"
  [ "type Either a b in"
  , "con Left: all a1. all b1. a1 -> Either a1 b1 in"
  , "con Right: all a2. all b2. b2 -> Either a2 b2 in"
  , "match"
  , "  true"
  , "with"
  , "  true"
  , "then"
  , "  Left"
  , "    (p_pure 1.)"
  , "else"
  , "  Left"
  , "    (p_assume (p_pure (Gaussian 1. 0.)))"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Either a b in"
  , "con Left : all a. all b. a -> Either a b in"
  , "con Right : all a. all b. b -> Either a b in"
  , "if true then Left 1.0 else if assume (Bernoulli 0.5) then Left 2.0 else Left 3.0"
  ]
with strJoin "\n"
  [ "type Either a b in"
  , "con Left: all a1. all b1. a1 -> Either a1 b1 in"
  , "con Right: all a2. all b2. b2 -> Either a2 b2 in"
  , "match"
  , "  true"
  , "with"
  , "  true"
  , "then"
  , "  p_pure (Left"
  , "       1.)"
  , "else"
  , "  p_map"
  , "    (lam x."
  , "       let target = x in"
  , "       match"
  , "         target"
  , "       with"
  , "         true"
  , "       then"
  , "         Left"
  , "           2."
  , "       else"
  , "         Left"
  , "           3.)"
  , "    (p_assume (p_pure (Bernoulli 0.5)))"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Either a b in"
  , "con Left : all a. all b. a -> Either a b in"
  , "con Right : all a. all b. b -> Either a b in"
  , "if true then"
  , "  let x = assume (Gaussian 1.0 0.0) in"
  , "  Left x"
  , "else"
  , "  if assume (Bernoulli 0.5) then Left 2.0 else Left 3.0"
  ]
with strJoin "\n"
  [ "type Either a b in"
  , "con Left: all a1. all b1. a1 -> Either a1 b1 in"
  , "con Right: all a2. all b2. b2 -> Either a2 b2 in"
  , "match"
  , "  true"
  , "with"
  , "  true"
  , "then"
  , "  p_map"
  , "    (lam x."
  , "       Left"
  , "         x)"
  , "    (p_assume (p_pure (Gaussian 1. 0.)))"
  , "else"
  , "  p_map"
  , "    (lam x1."
  , "       let target = x1 in"
  , "       match"
  , "         target"
  , "       with"
  , "         true"
  , "       then"
  , "         Left"
  , "           2."
  , "       else"
  , "         Left"
  , "           3.)"
  , "    (p_assume (p_pure (Bernoulli 0.5)))"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "Node {x = 1.0, left = Leaf {x = 2.0}, right = Leaf {x = 3.0}}"
  ]
with strJoin "\n"
  [ "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "Node"
  , "  { x = 1.,"
  , "    left = Leaf"
  , "        { x = 2. },"
  , "    right = Leaf"
  , "        { x = 3. } }"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "let x = assume (Gaussian 0.0 1.0) in"
  , "Node {x = 1.0, left = Leaf {x = x}, right = Leaf {x = 3.0}}"
  ]
with strJoin "\n"
  [ "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "Node"
  , "  { x = 1.,"
  , "    left = Leaf"
  , "        { x = p_assume (p_pure (Gaussian 0. 1.)) },"
  , "    right = Leaf"
  , "        { x = p_pure 3. } }"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "let x = assume (Gaussian 0.0 1.0) in"
  , "let l = if assume (Bernoulli 0.5) then Leaf {x = x} else Leaf {x = 2.0} in"
  , "Node {x = 1.0, left = l, right = Leaf {x = 3.0}}"
  ]
with strJoin "\n"
  [ "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "p_map"
  , "  (lam x."
  , "     Node"
  , "       { x = 1., left = x, right = Leaf"
  , "             { x = 3. } })"
  , "  (p_join"
  , "     (p_map"
  , "        (lam x1."
  , "           let target = x1 in"
  , "           match"
  , "             target"
  , "           with"
  , "             true"
  , "           then"
  , "             p_map"
  , "               (lam x2."
  , "                  Leaf"
  , "                    { x = x2 })"
  , "               (p_assume (p_pure (Gaussian 0. 1.)))"
  , "           else"
  , "             p_pure (Leaf"
  , "                  { x = 2. }))"
  , "        (p_assume (p_pure (Bernoulli 0.5)))))"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "let merge = lam l. lam r."
  , "  let x = assume (Gaussian 0.0 1.0) in"
  , "  Node {x = x, left = l, right = r} in"
  , "merge (merge (Leaf {x = 1.0}) (Leaf {x = 2.0})) (Leaf {x = 3.0})"
  ]
with strJoin "\n"
  [ "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "let merge ="
  , "  lam l1."
  , "    lam r1."
  , "      Node"
  , "        { x = p_assume (p_pure (Gaussian 0. 1.)), left = l1, right = r1 }"
  , "in"
  , "let merge1 ="
  , "  lam l."
  , "    lam r."
  , "      Node"
  , "        { x = p_assume (p_pure (Gaussian 0. 1.)), left = l, right = r }"
  , "in"
  , "merge1"
  , "  (merge (Leaf"
  , "        { x = 1. }) (Leaf"
  , "        { x = 2. }))"
  , "  (Leaf"
  , "     { x = 3. })"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "if assume (Bernoulli 0.5)"
  , "then let x = assume (Gaussian 0.0 1.0) in Node {x = x, left = Leaf {x = 1.0}, right = Leaf {x = 2.0}}"
  , "else Leaf {x = 1.0}"
  ]
with strJoin "\n"
  [ "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "p_join"
  , "  (p_map"
  , "     (lam x."
  , "        let target = x in"
  , "        match"
  , "          target"
  , "        with"
  , "          true"
  , "        then"
  , "          p_map"
  , "            (lam x1."
  , "               Node"
  , "                 { x = x1,"
  , "                   left = Leaf"
  , "                       { x = 1. },"
  , "                   right = Leaf"
  , "                       { x = 2. } })"
  , "            (p_assume (p_pure (Gaussian 0. 1.)))"
  , "        else"
  , "          p_pure (Leaf"
  , "               { x = 1. }))"
  , "     (p_assume (p_pure (Bernoulli 0.5))))"
  ]
using eqString
else printFailure
in

utest transform
  [ "type List a in"
  , "con Nil : all a. () -> List a in"
  , "con Cons : all a. (a, List a) -> List a in"
  , "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "recursive let cluster = lam trees."
  , "  match trees with Cons tmp in"
  , "  match tmp with (tree, rest) in"
  , "  match rest with Cons tmp then"
  , "    match tmp with (r, trees) in"
  , "    let newX = assume (Gaussian 0.0 1.0) in"
  , "    cluster (Cons (Node {x = newX, left = tree, right = r}, trees))"
  , "  else tree in"
  , "cluster (Cons (Leaf {x = 0.0}, Cons (Leaf {x = 1.0}, Cons (Leaf {x = 2.0}, Nil ()))));"
  , "()"
  ]
with strJoin "\n"
  [ "type List a in"
  , "con Nil: all a1. () -> List a1 in"
  , "con Cons: all a2. (a2, List a2) -> List a2 in"
  , "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "recursive"
  , "  let cluster ="
  , "    lam trees."
  , "      match"
  , "        trees"
  , "      with"
  , "        Cons tmp"
  , "      in"
  , "      match"
  , "          tmp"
  , "        with"
  , "          (tree, rest)"
  , "        in"
  , "        match"
  , "            rest"
  , "          with"
  , "            Cons tmp1"
  , "          then"
  , "            match"
  , "              tmp1"
  , "            with"
  , "              (r, trees1)"
  , "            in"
  , "            cluster1"
  , "                (Cons"
  , "                   (Node"
  , "                     { x = p_assume (p_pure (Gaussian 0. 1.)), left = tree, right = r }, trees1))"
  , "          else"
  , "            tree"
  , "  let cluster1 ="
  , "    lam trees2."
  , "      match"
  , "        trees2"
  , "      with"
  , "        Cons tmp2"
  , "      in"
  , "      match"
  , "          tmp2"
  , "        with"
  , "          (tree1, rest1)"
  , "        in"
  , "        match"
  , "            rest1"
  , "          with"
  , "            Cons tmp3"
  , "          then"
  , "            match"
  , "              tmp3"
  , "            with"
  , "              (r1, trees3)"
  , "            in"
  , "            cluster1"
  , "                (Cons"
  , "                   (Node"
  , "                     { x = p_assume (p_pure (Gaussian 0. 1.)),"
  , "                       left = tree1,"
  , "                       right = r1 }, trees3))"
  , "          else"
  , "            tree1"
  , "in"
  , "(cluster"
  , "     (Cons"
  , "        (Leaf"
  , "          { x = 0. }, Cons"
  , "          (Leaf"
  , "            { x = 1. }, Cons"
  , "            (Leaf"
  , "              { x = 2. }, Nil"
  , "              {})))))"
  , "; {}"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "recursive let cluster = lam trees."
  , "  match trees with [tree] then tree else"
  , "  match trees with [l, r] ++ trees in"
  , "  let newX = assume (Gaussian 0.0 1.0) in"
  , "  cluster (cons (Node {x = newX, left = l, right = r}) trees) in"
  , "cluster [Leaf {x = 0.0}, Leaf {x = 1.0}, Leaf {x = 2.0}]"
  ]
with strJoin "\n"
  [ "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "recursive"
  , "  let cluster ="
  , "    lam trees."
  , "      match"
  , "        trees"
  , "      with"
  , "        [ _,"
  , "          _ ] ++ _"
  , "      then"
  , "        cluster1"
  , "          (cons"
  , "             (Node"
  , "                { x = p_assume (p_pure (Gaussian 0. 1.)),"
  , "                  left = get trees 0,"
  , "                  right = get trees 1 })"
  , "             (splitAt trees 2).1)"
  , "      else match"
  , "        trees"
  , "      with"
  , "        [ e ]"
  , "      in"
  , "      e"
  , "  let cluster1 ="
  , "    lam trees1."
  , "      match"
  , "        trees1"
  , "      with"
  , "        [ _,"
  , "          _ ] ++ _"
  , "      then"
  , "        cluster1"
  , "          (cons"
  , "             (Node"
  , "                { x = p_assume (p_pure (Gaussian 0. 1.)),"
  , "                  left = get trees1 0,"
  , "                  right = get trees1 1 })"
  , "             (splitAt trees1 2).1)"
  , "      else match"
  , "        trees1"
  , "      with"
  , "        [ e1 ]"
  , "      in"
  , "      e1"
  , "in"
  , "cluster"
  , "  [ Leaf"
  , "      { x = 0. },"
  , "    Leaf"
  , "      { x = 1. },"
  , "    Leaf"
  , "      { x = 2. } ]"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "match head (cons (Leaf {x = 0.0}) [Leaf {x = 1.0}]) with Leaf x in"
  , "addf x.x 1.0"
  ]
with strJoin "\n"
  [ "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "match"
  , "  head"
  , "    (cons (Leaf"
  , "          { x = 0. }) [ Leaf"
  , "           { x = 1. } ])"
  , "with"
  , "  Leaf carried"
  , "in"
  , "addf carried.x 1."
  ]
using eqString
else printFailure
in

utest transform
  [ "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "let x = assume (Gaussian 0.0 1.0) in"
  , "match head (cons (Leaf {x = x}) [Leaf {x = 1.0}]) with Leaf x in"
  , "addf x.x 1.0"
  ]
with strJoin "\n"
  [ "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "match"
  , "  head"
  , "    (cons"
  , "       (Leaf"
  , "          { x = p_assume (p_pure (Gaussian 0. 1.)) })"
  , "       [ Leaf"
  , "           { x = p_pure 1. } ])"
  , "with"
  , "  Leaf carried"
  , "in"
  , "p_map (lam x."
  , "       addf x 1.) carried.x"
  ]
using eqString
else printFailure
in

utest transform
  [ "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "let x = assume (Gaussian 0.0 1.0) in"
  , "match head (cons (Leaf {x = 1.0}) [Leaf {x = x}]) with Leaf x in"
  , "addf x.x 1.0"
  ]
with strJoin "\n"
  [ "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "match"
  , "  head"
  , "    (cons"
  , "       (Leaf"
  , "          { x = p_pure 1. })"
  , "       [ Leaf"
  , "           { x = p_assume (p_pure (Gaussian 0. 1.)) } ])"
  , "with"
  , "  Leaf carried"
  , "in"
  , "p_map (lam x."
  , "       addf x 1.) carried.x"
  ]
using eqString
else printFailure
in

utest transform
  [ "type List a in"
  , "con Nil : all a. () -> List a in"
  , "con Cons : all a. (a, List a) -> List a in"
  , "type Tree in"
  , "con Leaf : {x : Float} -> Tree in"
  , "con Node : {x : Float, left : Tree, right : Tree} -> Tree in"
  , "recursive let cluster = lam trees."
  , "  match trees with Cons (tree, Nil _) then tree else"
  , "  match trees with Cons (l, Cons (r, trees)) in"
  , "  let newX = assume (Gaussian 0.0 1.0) in"
  , "  cluster (Cons (Node {x = newX, left = l, right = r}, trees)) in"
  , "cluster (Cons (Leaf {x = 0.0}, Cons (Leaf {x = 1.0}, Cons (Leaf {x = 2.0}, Nil ()))));"
  , "()"
  ]
with strJoin "\n"
  [ "type List a in"
  , "con Nil: all a1. () -> List a1 in"
  , "con Cons: all a2. (a2, List a2) -> List a2 in"
  , "type Tree in"
  , "con Leaf: {x: Float} -> Tree in"
  , "con Node: {x: Float, left: Tree, right: Tree} -> Tree in"
  , "recursive"
  , "  let cluster ="
  , "    lam trees."
  , "      match"
  , "        trees"
  , "      with"
  , "        Cons carried"
  , "      in"
  , "      match"
  , "          carried"
  , "        with"
  , "          (field, field1)"
  , "        in"
  , "        match"
  , "            field1"
  , "          with"
  , "            Nil carried1"
  , "          then"
  , "            field"
  , "          else match"
  , "            field1"
  , "          with"
  , "            Cons carried2"
  , "          in"
  , "          match"
  , "              carried2"
  , "            with"
  , "              (field2, field3)"
  , "            in"
  , "            cluster1"
  , "                (Cons"
  , "                   (Node"
  , "                     { x = p_assume (p_pure (Gaussian 0. 1.)),"
  , "                       left = field,"
  , "                       right = field2 }, field3))"
  , "  let cluster1 ="
  , "    lam trees1."
  , "      match"
  , "        trees1"
  , "      with"
  , "        Cons carried3"
  , "      in"
  , "      match"
  , "          carried3"
  , "        with"
  , "          (field4, field5)"
  , "        in"
  , "        match"
  , "            field5"
  , "          with"
  , "            Nil carried4"
  , "          then"
  , "            field4"
  , "          else match"
  , "            field5"
  , "          with"
  , "            Cons carried5"
  , "          in"
  , "          match"
  , "              carried5"
  , "            with"
  , "              (field6, field7)"
  , "            in"
  , "            cluster1"
  , "                (Cons"
  , "                   (Node"
  , "                     { x = p_assume (p_pure (Gaussian 0. 1.)),"
  , "                       left = field4,"
  , "                       right = field6 }, field7))"
  , "in"
  , "(cluster"
  , "     (Cons"
  , "        (Leaf"
  , "          { x = 0. }, Cons"
  , "          (Leaf"
  , "            { x = 1. }, Cons"
  , "            (Leaf"
  , "              { x = 2. }, Nil"
  , "              {})))))"
  , "; {}"
  ]
using eqString
else printFailure
in

utest transform
  [ "switch assume (Categorical [0.25, 0.25, 0.25, 0.25])"
  , "case 0 then 0"
  , "case 1 then 1"
  , "case 2 then 2"
  , "case 3 then 3"
  , "end"
  ]
with strJoin "\n"
  [ "let #var\"X\" = p_assume (p_pure (Categorical [ 0.25, 0.25, 0.25, 0.25 ]))"
  , "in"
  , "p_map"
  , "  (lam x."
  , "     let #var\"X1\" = x in"
  , "     match"
  , "       #var\"X1\""
  , "     with"
  , "       0"
  , "     then"
  , "       0"
  , "     else match"
  , "       #var\"X1\""
  , "     with"
  , "       1"
  , "     then"
  , "       1"
  , "     else match"
  , "       #var\"X1\""
  , "     with"
  , "       2"
  , "     then"
  , "       2"
  , "     else match"
  , "       #var\"X1\""
  , "     with"
  , "       3"
  , "     in"
  , "     3)"
  , "  #var\"X\""
  ]
using eqString
else printFailure
in

utest transform
  [ "let x = if assume (Bernoulli 0.5) then (1, 2) else (2, 3) in"
  , "match x with (a, b) in"
  , "addi a b"
  ]
with strJoin "\n"
  [ "p_map"
  , "  (lam x."
  , "     let target ="
  , "       let target1 = x in"
  , "       match"
  , "         target1"
  , "       with"
  , "         true"
  , "       then"
  , "         (1, 2)"
  , "       else"
  , "         (2, 3)"
  , "     in"
  , "     match"
  , "       target"
  , "     with"
  , "       (a, b)"
  , "     in"
  , "     addi a b)"
  , "  (p_assume (p_pure (Bernoulli 0.5)))"
  ]
using eqString
else printFailure
in

utest transform
  [ "let x = if assume (Bernoulli 0.5) then (1, 2) else (2, 3) in"
  , "match x with (a, _) in"
  , "addi a (assume (Categorical [0.5, 0.5]))"
  ]
with strJoin "\n"
  [ "p_join"
  , "  (p_map"
  , "     (lam x."
  , "        let target ="
  , "          let target1 = x in"
  , "          match"
  , "            target1"
  , "          with"
  , "            true"
  , "          then"
  , "            (1, 2)"
  , "          else"
  , "            (2, 3)"
  , "        in"
  , "        match"
  , "          target"
  , "        with"
  , "          (a, _)"
  , "        in"
  , "        p_map (addi a) (p_assume (p_pure (Categorical [ 0.5, 0.5 ]))))"
  , "     (p_assume (p_pure (Bernoulli 0.5))))"
  ]
using eqString
else printFailure
in

()
