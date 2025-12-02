-- This file provides a transformation that removes second-class
-- functions, i.e., functions that are never stored in another value
-- or returned from a function, by specializing higher-order functions
-- to their functional inputs.
--
-- Assumptions:
-- * TODO(vipa, 2025-12-02): Likely something about being fully applied.
--
-- Note that the transformation will not ensure that all `ty` fields
-- are correct; re-run typechecking afterwards if that's desirable.

include "mexpr/ast.mc"
include "mexpr/pprint.mc"
include "coreppl::parser.mc"


let _cmpSym : Symbol -> Symbol -> Int = lam l. lam r. subi (sym2hash l) (sym2hash r)

-- === "recursive" ints, defined via `max` ===

type RMaxInt
con RMaxInt : {knownMin : Int, unknowns : Map Symbol (Ref (Option Int))} -> RMaxInt

let maxri : RMaxInt -> RMaxInt -> RMaxInt = lam l. lam r.
  match l with RMaxInt l in
  match r with RMaxInt r in
  RMaxInt
  { knownMin = maxi l.knownMin r.knownMin
  , unknowns = mapUnion l.unknowns r.unknowns
  }

let newRMaxInt : Int -> {val : RMaxInt, isInDependenciesFor : RMaxInt -> Bool, setVal : RMaxInt -> (RMaxInt, () -> Option Int)} = lam knownMin.
  let sym = gensym () in
  let r = ref (None ()) in
  { val = RMaxInt
    { knownMin = knownMin
    , unknowns = mapSingleton _cmpSym sym r
    }
  , isInDependenciesFor = lam rmi.
    match rmi with RMaxInt rmi in
    mapMem sym rmi.unknowns
  , setVal = lam val.
    match val with RMaxInt val in
    let unknowns = mapRemove sym val.unknowns in
    ( RMaxInt {val with unknowns = unknowns}
    , lam.
      let tryReadAndSet = lam.
        let max = mapFoldlOption (lam acc. lam. lam r. optionMap (maxi acc) (deref r)) val.knownMin unknowns in
        modref r max;
        max in
      optionOrElse tryReadAndSet (deref r)
    )
  }

let pureRMaxInt : Int -> RMaxInt = lam i. RMaxInt {knownMin = i, unknowns = mapEmpty _cmpSym}

-- === Actual Implementation ===

lang RemoveSecondClassFunctions = RecLetsDeclAst + LetDeclAst + VarAst + LamAst + AppAst + ConstAst + DataDeclAst + DataAst + DataPat + PrettyPrint
  syn BindingKind =
  | BKRef {depth : Int}
  | BKFuncDef {fPos : [Bool], body : Expr, depth : Int}
  | BKFuncArg {depth : RMaxInt, replacement : Expr}
  type RemSecLamEnv =
    { depth : Int
    , bindings : Map Name BindingKind
    }

  type RemSecLamState =
    { generated : Map {fName : Name, args : [Option {fName : Either Const Name, argCount : Int}]} {fName : Name, depth : RMaxInt}
    , toInsert : Map Int [Decl]
    , pendingRecursiveDefinitions : [(() -> Option Int, DeclLetRecord)]
    , depths : Map Name RMaxInt
    , currMaxDepthUsed : RMaxInt
    }

  sem lambdaFuncPositions : Expr -> [Bool]
  sem lambdaFuncPositions =
  | TmLam x ->
    -- TODO(vipa, 2025-11-28): Should this detect higher rank functions too?
    let here = match unwrapType x.tyParam with TyArrow _ then true else false in
    cons here (lambdaFuncPositions x.body)
  | _ -> []

  sem remSecLamDecl : RemSecLamEnv -> RemSecLamState -> Decl -> (RemSecLamEnv, RemSecLamState, Option Decl)
  sem remSecLamDecl env st =
  | decl & DeclConDef x ->
    ( {env with bindings = mapInsert x.ident (BKRef {depth = env.depth}) env.bindings}
    , st
    , Some decl
    )
  | DeclLet x ->
    let fPos = lambdaFuncPositions x.body in
    if someb fPos then
      -- NOTE(vipa, 2025-11-28): At least one second class function,
      -- record the definition so we can specialize it for particular
      -- lambda arguments later.
      ( {env with bindings = mapInsert x.ident (BKFuncDef {fPos = fPos, body = x.body, depth = env.depth}) env.bindings}
      , st
      , None ()
      )
    else
      -- NOTE(vipa, 2025-11-28): No second class function, insert it
      -- as normal
      match remSecLamExpr {env with depth = subi env.depth 1} st x.body with (st, body) in
      -- NOTE(vipa, 2025-11-28): Only put binding in the environment
      -- if it's a function, since those are the only ones we can't
      -- just capture on the spot and pass along (since that would be
      -- a second/first class function).
      ( if null fPos
        then env
        else {env with bindings = mapInsert x.ident (BKRef {depth = env.depth}) env.bindings}
      , if null fPos
        then st
        else {st with depths = mapInsert x.ident (pureRMaxInt env.depth) st.depths}
      , Some (DeclLet {x with body = body})
      )
  | DeclRecLets x ->
    let classify = lam pair. lam binding.
      match pair with (env, st) in
      let fPos = lambdaFuncPositions binding.body in
      if someb fPos then
        ( ( {env with bindings = mapInsert binding.ident (BKFuncDef {fPos = fPos, body = binding.body, depth = env.depth}) env.bindings}
          , st
          )
        , None ()
        )
      else
        ( ( {env with bindings = mapInsert binding.ident (BKRef {depth = env.depth}) env.bindings}
          , {st with depths = mapInsert binding.ident (pureRMaxInt env.depth) st.depths}
          )
        , Some binding
        ) in
    match mapAccumL classify (env, st) x.bindings with ((env, st), bindings) in
    let bindings = filterOption bindings in
    if null bindings then (env, st, None ()) else
    let remSecLamBinding = lam st. lam binding.
      match remSecLamExpr env st binding.body with (st, body) in
      (st, {binding with body = body}) in
    match mapAccumL remSecLamBinding st bindings with (st, bindings) in
    let declToBindings = lam decl. switch decl
      case DeclLet x then [x]
      case DeclRecLets x then x.bindings
      end in
    let toInsert = optionMapOr [] (lam decls. join (map declToBindings decls)) (mapLookup env.depth st.toInsert) in
    ( env
    , {st with toInsert = mapRemove env.depth st.toInsert}
    , Some (DeclRecLets {x with bindings = bindings})
    )
  | decl & DeclExt x ->
    ( {env with bindings = mapInsert x.ident (BKRef {depth = env.depth}) env.bindings}
    , {st with depths = mapInsert x.ident (pureRMaxInt env.depth) st.depths}
    , Some decl
    )
  | decl & DeclType _ -> (env, st, Some decl)
  | decl -> errorSingle [infoDecl decl] (concat "Missing case for decl: " (decl2str decl))

  sem remSecLamCall : RemSecLamEnv -> RemSecLamState -> Type -> (Expr, [Expr]) -> Option (RemSecLamState, Expr)
  sem remSecLamCall env st retTy =
  | (TmVar x, args) -> switch mapLookup x.ident env.bindings
    case Some (BKFuncDef (fd & {depth = depth})) then
      -- TODO(vipa, 2025-12-01): We're not keeping the symbolize
      -- invariant here, what's the most reasonable way to do that?
      let st = {st with currMaxDepthUsed = maxri st.currMaxDepthUsed (pureRMaxInt depth)} in
      recursive let collectApps = lam args. lam tm.
        switch tm
        case TmApp x then collectApps (cons x.rhs args) x.lhs
        case TmVar x then {fName = Right x.ident, args = args}
        case TmConst x then {fName = Left x.val, args = args}
        end in
      let f = lam isF. lam tm.
        if isF then
          let x = collectApps [] tm in
          (x.args, Some {fName = x.fName, argCount = length x.args})
        else ([tm], None ()) in
      let argSpecs = zipWith f fd.fPos args in
      match unzip argSpecs with (args, keyArgs) in
      let key = {fName = x.ident, args = keyArgs} in
      match
        match mapLookup key st.generated with Some g then
          ({st with currMaxDepthUsed = maxri st.currMaxDepthUsed g.depth}, g.fName)
        else
        let fName = nameSetNewSym x.ident in
        let rmi = newRMaxInt depth in
        let prevMaxDepth = st.currMaxDepthUsed in
        let st =
          { st with currMaxDepthUsed = pureRMaxInt depth
          , generated = mapInsert key {fName = fName, depth = rmi.val} st.generated
          , depths = mapInsert fName rmi.val st.depths
          } in
        match
          recursive let collectLams = lam acc. lam tm.
            match tm with TmLam x
            then collectLams (snoc acc x) x.body
            else (acc, tm) in
          match collectLams [] fd.body with (lamRecords, bodyUnderLam) in
          let f = lam argSpec. lam lamRecord. lam env.
            match argSpec with (args, Some key) then
              let names = map (lam. nameSym "c") args in
              let mkLamRecord = lam n. lam tm.
                {lamRecord with ident = n, tyAnnot = tyunknown_, tyParam = tyTm tm} in
              let records = zipWith mkLamRecord names args in
              -- TODO(vipa, 2025-12-01): This has some missing types
              -- and info-fields
              let replacement = foldl2 (lam tm. lam n. lam arg. app_ tm (withType (tyTm arg) (nvar_ n)))
                (eitherEither uconst_ nvar_ key.fName)
                names
                args in
              let depth = optionGetOr (pureRMaxInt 0) (optionBind (eitherGetRight key.fName) (lam n. mapLookup n st.depths)) in
              ({env with bindings = mapInsert lamRecord.ident (BKFuncArg {replacement = replacement, depth = depth}) env.bindings}, records)
            else (env, [lamRecord]) in
          match mapAccumL (lam env. lam f. f env) env (zipWith f argSpecs lamRecords) with (env, lamRecords) in
          match remSecLamExpr env st bodyUnderLam with (st, bodyUnderLam) in
          (st, foldr (lam l. lam tm. TmLam {l with body = tm}) bodyUnderLam (join lamRecords))
        with (st, body) in
        let binding : DeclLetRecord =
          { ident = fName
          , tyAnnot = tyunknown_
          , tyBody = tyunknown_
          , body = body
          , info = infoTm body
          } in
        let fullDepthUsed = st.currMaxDepthUsed in
        match rmi.setVal fullDepthUsed with (depth, getDepth) in
        let st = {st with currMaxDepthUsed = maxri prevMaxDepth depth} in
        -- OPT(vipa, 2025-11-28): We could probably look at
        -- fullDepthUsed, if it's empty then we can use a let,
        -- otherwise we need a reclet?
        match getDepth () with Some depth then
          if rmi.isInDependenciesFor fullDepthUsed then
            let tryResolve = lam pair.
              match pair with (try, binding) in
              match try () with Some depth
              then Right (depth, [binding])
              else Left pair in
            recursive let work = lam acc. lam pending.
              switch eitherPartition (map tryResolve pending)
              case (pending, []) then (acc, pending)
              case (pending, resolved) then
                work (foldl (lam acc. lam pair. mapInsertWith concat pair.0 pair.1 acc) acc resolved) pending
              end in
            match work (mapSingleton subi depth [binding]) st.pendingRecursiveDefinitions with (resolved, pending) in
            let resolved = mapMap (lam bindings. [DeclRecLets {bindings = bindings, info = NoInfo ()}]) resolved in
            ({st with pendingRecursiveDefinitions = pending, toInsert = mapUnionWith concat st.toInsert resolved}, fName)
          else
            ({st with toInsert = mapInsertWith concat depth [DeclLet binding] st.toInsert}, fName)
        else
          ({st with pendingRecursiveDefinitions = snoc st.pendingRecursiveDefinitions (getDepth, binding)}, fName)
      with (st, fName) in
      Some (st, withType retTy (appSeq_ (nvar_ fName) (join args)))
    case _ then None ()
    end
  | _ -> None ()

  sem remSecLamExpr : RemSecLamEnv -> RemSecLamState -> Expr -> (RemSecLamState, Expr)
  sem remSecLamExpr env st =
  | TmDecl x ->
    match remSecLamDecl {env with depth = addi 1 env.depth} st x.decl with (env, st, oDecl) in
    match remSecLamExpr env st x.inexpr with (st, inexpr) in
    let inexpr = match mapLookup env.depth st.toInsert with Some decls
      then bindall_ decls inexpr
      else inexpr in
    let inexpr = match oDecl with Some decl
      then TmDecl {x with decl = decl, inexpr = inexpr}
      else inexpr in
    ({st with toInsert = mapRemove env.depth st.toInsert}, inexpr)
  | TmApp x ->
    recursive let work = lam xs. lam args. lam tm.
      match tm with TmApp x
      then work (cons x xs) (cons x.rhs args) x.lhs
      else (tm, xs, args) in
    match work [x] [x.rhs] x.lhs with (f, xs, args) in
    match mapAccumL (remSecLamExpr env) st args with (st, args) in
    match remSecLamCall env st x.ty (f, args) with Some ret then ret else
    -- NOTE(vipa, 2025-11-28): Reconstruct with the original TmApp
    -- fields
    match remSecLamExpr env st f with (st, f) in
    (st, foldl2 (lam acc. lam x. lam arg. TmApp {x with lhs = acc, rhs = arg}) f xs args)
  | tm & TmVar x ->
    let res = mapLookup x.ident env.bindings in
    let st = switch res
      case Some (BKRef {depth = depth}) then
        {st with currMaxDepthUsed = maxri st.currMaxDepthUsed (pureRMaxInt depth)}
      case Some (BKFuncArg {depth = depth}) then
        {st with currMaxDepthUsed = maxri st.currMaxDepthUsed depth}
      case _ then st
      end in
    match res with Some (BKFuncArg {replacement = replacement})
    then (st, replacement)
    else (st, tm)
  | tm & TmConApp {ident = ident} ->
    let st = match mapLookup ident env.bindings with Some (BKRef {depth = depth})
      then {st with currMaxDepthUsed = maxri st.currMaxDepthUsed (pureRMaxInt depth)}
      else st in
    smapAccumL_Expr_Expr (remSecLamExpr env) st tm
  | tm ->
    let st = sfold_Expr_Pat (remSecLamPat env) st tm in
    smapAccumL_Expr_Expr (remSecLamExpr env) st tm

  sem remSecLamPat : RemSecLamEnv -> RemSecLamState -> Pat -> RemSecLamState
  sem remSecLamPat env st =
  | pat & PatCon {ident = ident} ->
    let st = match mapLookup ident env.bindings with Some (BKRef {depth = depth})
      then {st with currMaxDepthUsed = maxri st.currMaxDepthUsed (pureRMaxInt depth)}
      else st in
    sfold_Pat_Pat (remSecLamPat env) st pat
  | pat -> sfold_Pat_Pat (remSecLamPat env) st pat
end
