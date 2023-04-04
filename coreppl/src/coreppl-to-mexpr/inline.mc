include "../coreppl.mc"

lang DPPLInline = MExprPPL

  -- Assumes proper symbolization and ANF
  sem inlineSingleUseFun : Expr -> Expr
  sem inlineSingleUseFun =
  | expr ->
    -- Count uses of lambdas
    let m = determineFunInline (mapEmpty nameCmp) expr in
    -- printLn (strJoin ",\n" (map
    --   (lam t. join [(nameGetStr t.0), "->", (bool2string t.1)])
    --   (mapToSeq m)));

    -- Inline functions
    match inlineSingleUseFunH m (mapEmpty nameCmp) expr with (_,expr) in
    expr

  sem determineFunInline : Map Name Bool -> Expr -> Map Name Bool
  sem determineFunInline m =
  | TmLet t ->
    let m = determineFunInline m t.body in
    let m =
      match t.body with TmLam _ then mapInsert t.ident false m
      else m -- Only track functions
    in
    determineFunInline m t.inexpr
  | TmRecLets ({bindings = [b]} & t) ->
    let m = mapInsert b.ident false m in
    let m = determineFunInline m b.body in
    determineFunInline m t.inexpr
  | TmVar t ->
    match mapLookup t.ident m with Some b then
      if b then mapRemove t.ident m -- More than a single use
      else mapInsert t.ident true m -- First use
    else m
  | expr -> sfold_Expr_Expr determineFunInline m expr

  sem inlineSingleUseFunH : Map Name Bool
                         -> Map Name Expr -> Expr -> (Map Name Expr, Expr)
  sem inlineSingleUseFunH m me =
  | TmLet t ->
    match inlineSingleUseFunH m me t.body with (me,body) in
    let inline = match mapLookup t.ident m with Some true then true else false in
    let me = if inline then mapInsert t.ident body me else me in
    match inlineSingleUseFunH m me t.inexpr with (me,inexpr) in
    if inline then
      match mapLookup t.ident me with None _ then
        -- The function was inlined, remove it
        (me, inexpr)
      else
        (me, TmLet {t with body = body, inexpr = inexpr})
    else
      (me, TmLet {t with body = body, inexpr = inexpr})
  | TmRecLets ({bindings = [b]} & t) ->
    match inlineSingleUseFunH m (mapRemove b.ident me) b.body with (me,body) in
    let inline = match mapLookup b.ident m with Some true then true else false in
    let me = if inline then mapInsert b.ident body me else me in
    match inlineSingleUseFunH m me t.inexpr with (me,inexpr) in
    if inline then
      match mapLookup b.ident me with None _ then
        -- The function was inlined, remove it
        (me, inexpr)
      else
        (me, TmRecLets {
            t with bindings = [{b with body = body}], inexpr = inexpr
          })
    else
      (me, TmRecLets {
          t with bindings = [{b with body = body}], inexpr = inexpr
        })
  | TmApp t & tm ->
    match smapAccumL_Expr_Expr (inlineSingleUseFunH m) me tm with (me,TmApp t) in
    match t.lhs with TmVar tv then
      match
        match mapLookup tv.ident me with Some lhs then
          (mapRemove tv.ident me, TmApp {t with lhs = lhs})
        else (me, TmApp t)
      with (me, TmApp t) in

      -- Also transform direct lambda applications to lets
      match t.lhs with TmLam tl then
        let letBody = (nlet_ tl.ident tl.tyAnnot t.rhs) in
        -- NOTE(dlunde,2023-04-04): Currently discards the overall type
        (me, withInfo (infoTm tm) (bind_ letBody tl.body))
      else (me, TmApp t)

    else (me, TmApp t)
  | expr -> smapAccumL_Expr_Expr (inlineSingleUseFunH m) me expr

end
