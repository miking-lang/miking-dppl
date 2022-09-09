include "mexpr/ast.mc"

lang CPPLDesymbolizeBase = MExprAst
  type DesymEnv = Map Name Name

  sem stripSymbol : DesymEnv -> Name -> Name
  sem stripSymbol env =
  | id ->
    match mapLookup id env with Some id then id
    else id

  sem addStrippedSymbol : DesymEnv -> Name -> (DesymEnv, Name)
  sem addStrippedSymbol env =
  | id ->
    if nameHasSym id then
      let noSymId = nameNoSym (nameGetStr id) in
      (mapInsert id noSymId env, noSymId)
    else (env, id)

  sem desymbolize : Expr -> Expr
  sem desymbolize =
  | t -> desymbolizeExpr (mapEmpty nameCmp) t

  sem desymbolizeExpr : DesymEnv -> Expr -> Expr
  sem desymbolizeExpr env =
  | TmVar t ->
    TmVar {t with ident = stripSymbol env t.ident,
                  ty = desymbolizeType env t.ty}
  | t ->
    let t = withType (desymbolizeType env (tyTm t)) t in
    let t = smap_Expr_Type (desymbolizeType env) t in
    let t = smap_Expr_Pat (desymbolizePat env) t in
    smap_Expr_Expr (desymbolizeExpr env) t

  sem desymbolizeType : DesymEnv -> Type -> Type
  sem desymbolizeType env =
  | TyVar t -> TyVar {t with ident = stripSymbol env t.ident}
  | TyCon t -> TyCon {t with ident = stripSymbol env t.ident}
  | TyAll t ->
    TyAll {t with ident = stripSymbol env t.ident,
                  ty = desymbolizeType env t.ty}
  | ty -> smap_Type_Type (desymbolizeType env) ty

  sem desymbolizePat : DesymEnv -> Pat -> Pat
  sem desymbolizePat env =
  | pat ->
    let pat = withTypePat (desymbolizeType env (tyPat pat)) pat in
    smap_Pat_Pat (desymbolizePat env) pat
end

lang CPPLDesymbolizeTypes = CPPLDesymbolizeBase
  sem desymbolizeExpr env =
  | TmType t ->
    match addStrippedSymbol env t.ident with (env, ident) in
    match mapAccumL addStrippedSymbol env t.params with (env, params) in
    TmType {t with ident = ident,
                   params = params,
                   tyIdent = desymbolizeType env t.tyIdent,
                   inexpr = desymbolizeExpr env t.inexpr,
                   ty = desymbolizeType env t.ty}
  | TmConDef t ->
    match addStrippedSymbol env t.ident with (env, ident) in
    TmConDef {t with ident = ident,
                     tyIdent = desymbolizeType env t.tyIdent,
                     inexpr = desymbolizeExpr env t.inexpr,
                     ty = desymbolizeType env t.ty}
  | TmConApp t ->
    TmConApp {t with ident = stripSymbol env t.ident,
                     body = desymbolizeExpr env t.body,
                     ty = desymbolizeType env t.ty}

  sem desymbolizePat env =
  | PatCon t ->
    PatCon {t with ident = stripSymbol env t.ident,
                   subpat = desymbolizePat env t.subpat,
                   ty = desymbolizeType env t.ty}
end

lang CPPLDesymbolizeExternals = CPPLDesymbolizeBase
  sem desymbolizeExpr env =
  | TmExt t ->
    match addStrippedSymbol env t.ident with (env, ident) in
    TmExt {t with ident = ident,
                  tyIdent = desymbolizeType env t.tyIdent,
                  inexpr = desymbolizeExpr env t.inexpr,
                  ty = desymbolizeType env t.ty}
end

lang CPPLRemoveDuplicate = MExprAst
  -- Removes duplicated type definitions.
  -- TODO(larshum, 2022-09-09): This approach assumes all type and constructor
  -- definitions in a program use distinct strings as identifiers. This should
  -- be replaced with a different approach (using type equality?).
  sem removeDuplicateTypes : Expr -> Expr
  sem removeDuplicateTypes =
  | ast -> removeDuplicateTypesH (setEmpty nameCmp) ast

  sem removeDuplicateTypesH : Set Name -> Expr -> Expr
  sem removeDuplicateTypesH defined =
  | TmType t ->
    if setMem t.ident defined then removeDuplicateTypesH defined t.inexpr
    else
      let defined = setInsert t.ident defined in
      TmType {t with inexpr = removeDuplicateTypesH defined t.inexpr}
  | TmConDef t ->
    if setMem t.ident defined then removeDuplicateTypesH defined t.inexpr
    else
      let defined = setInsert t.ident defined in
      TmConDef {t with inexpr = removeDuplicateTypesH defined t.inexpr}
  | t -> smap_Expr_Expr (removeDuplicateTypesH defined) t

  -- Removes duplicated declarations of externals
  sem removeDuplicateExternals : Expr -> Expr
  sem removeDuplicateExternals =
  | ast -> removeDuplicateExternalsH (setEmpty nameCmp) ast

  sem removeDuplicateExternalsH : Set Name -> Expr -> Expr
  sem removeDuplicateExternalsH defined =
  | TmExt t ->
    if setMem t.ident defined then removeDuplicateExternalsH defined t.inexpr
    else
      let defined = setInsert t.ident defined in
      TmExt {t with inexpr = removeDuplicateExternalsH defined t.inexpr}
  | t -> smap_Expr_Expr (removeDuplicateExternalsH defined) t
end

lang CPPLDedup = CPPLDesymbolizeTypes + CPPLDesymbolizeExternals +
                 CPPLRemoveDuplicate

  sem deduplicateTypesAndExternals : Expr -> Expr
  sem deduplicateTypesAndExternals =
  | ast ->
    let ast = desymbolize ast in
    let ast = removeDuplicateTypes ast in
    removeDuplicateExternals ast
end
