include "../coreppl.mc"
include "../parser.mc"
include "../dppl-arg.mc"

include "name.mc"
include "peval/peval.mc"
include "mexpr/demote-recursive.mc"
include "mexpr/shallow-patterns.mc"
include "mexpr/extract.mc"
include "mexpr/lamlift.mc"

lang DPPLExtract =
  DPPLParser + MExprExtract + MExprLambdaLiftAllowSpineCapture + MExprDemoteRecursive +
  MExprPEval + MExprLowerNestedPatterns

  type ModelRepr = {
    -- Call this to extract the model *after* applying the given
    -- function to the entire AST
    extractAst : (Expr -> Expr) -> Expr,
    method : InferMethod,
    params : [(Name, Type)]
  }

  -- Takes an AST and extracts the models used within infer expressions.
  --
  -- The returned value is a tuple consisting of:
  -- 1. The original AST, where the infer expressions have been replaced with
  --    calls to the inference algorithms.
  -- 2. A map with an entry for each inference method. Every inference method
  --    is mapped to a sequence of records describing each model.
  sem extractInfer : TransformationOptions -> Map InferMethod Name -> Expr
                  -> (Expr, Map Name FinalOrderedLamLiftSolution, Map Name ModelRepr)
  sem extractInfer options runtimes =
  | ast ->
    match bindInferExpressions runtimes ast with (data, ast) in
    -- NOTE(vipa, 2025-05-16): Bindings that are allowed to be
    -- captured will be included in the extracted model code, and thus
    -- re-run each time the model is re-run. We may need to transform
    -- functions (e.g., if the inference method uses cps), thus we
    -- only allow the capture of bindings whose type includes a
    -- function, hoping that the repeated work is regligible or
    -- non-existent.
    recursive let includesFunction = lam ty.
      match ty with TyArrow _ then true else
      sfold_Type_Type (lam acc. lam ty. if acc then acc else includesFunction ty) false ty in
    match liftLambdasWithSolutionsMaybeAllowSpineCapture (lam x. if includesFunction x.ty then AllowCapture () else DisallowCapture ()) ast with (solutions, ast) in

    let modelAsts : Map Name ModelRepr =
      mapMapWithKey
        (lam inferId. lam method.
          match mapLookup inferId solutions with Some sol then
            let params = map (lam x. (mapFindExn x.0 sol.varsToParams, x.1)) sol.vars in
            let extractAst = lam f. extractInferAst options inferId (f ast) in
            {extractAst = extractAst, method = method, params = params}
          else error "Lambda lifting was not correctly applied to infer")
        data in

    -- Construct a map from the identifier of an infer binding to its
    -- corresponding inference method configuration and a runtime entry.
    let inferData : Map Name (InferMethod, Name) =
      mapMapWithKey
        (lam. lam model.
          match model with {method = method} in
          match mapLookup method runtimes with Some runId then
            (method, runId)
          else
            match pprintInferMethod 0 pprintEnvEmpty method with (_, method) in
            error (join ["Missing runtime for method (", method, ")"]))
        modelAsts in

    (removeInfers inferData ast, solutions, modelAsts)

  -- Binds all infer expressions in the provided AST to a variable, such that
  -- an expression 'infer e' is rewritten as 'let t = e in t'. The result is
  -- the updated AST, and a map from the newly introduced identifiers to the
  -- inference method they involve.
  sem bindInferExpressions : Map InferMethod Name -> Expr -> (Map Name InferMethod, Expr)
  sem bindInferExpressions runtimes =
  | ast -> bindInferExpressionsH runtimes (mapEmpty nameCmp) ast

  sem bindInferExpressionsH : Map InferMethod Name -> Map Name InferMethod
                           -> Expr -> (Map Name InferMethod, Expr)
  sem bindInferExpressionsH runtimes acc =
  | TmInfer t ->
    let inferId = nameSym "t" in
    let info = t.info in
    match mapLookup t.method runtimes with Some runId then
      let inferBinding = TmDecl
        { decl = DeclLet
          { ident = inferId
          , tyAnnot = tyarrow_ tyunit_ (tyTm t.model)
          , tyBody = tyarrow_ tyunit_ (tyTm t.model)
          , body = TmLam
            {  ident = nameNoSym ""
            , tyAnnot = tyunit_
            , tyParam = tyunit_
            , body = TmApp {lhs = t.model, rhs = unit_, ty = tyTm t.model, info = info}
            , ty = tyarrow_ tyunit_ (tyTm t.model)
            , info = info
            }
          , info = info
          }
        , inexpr = TmApp
          { lhs = TmApp
            { lhs = nvar_ runId
            , rhs = inferMethodConfig t.info t.method
            , ty = tyunknown_
            , info = t.info
            }
          , rhs = TmVar {ident = inferId, ty = t.ty, info = info, frozen = false}
          , ty = tyunknown_
          , info = t.info
          }
        , ty = t.ty
        , info = info
        } in
      (mapInsert inferId t.method acc, inferBinding)
    else
      errorSingle [t.info] "Missing infer runtime"
  | t -> smapAccumL_Expr_Expr (bindInferExpressionsH runtimes) acc t

  -- Extracts an AST consisting of the model whose binding has the provided
  -- inference ID.
  sem extractInferAst : TransformationOptions -> Name -> Expr -> Expr
  sem extractInferAst options inferId =
  | ast ->
    let ast = extractAst (setOfSeq nameCmp [inferId]) ast in
    let ast = inlineInferBinding inferId ast in
    -- printLn (mexprPPLToString ast);
    let ast = demoteRecursive ast in
    -- Suggestion by Johan and Oscar to do pattern lowering before peval
    let ast =
      switch options.extractSimplification
      case "none" then ast
      case "inline" then
        -- NOTE(dlunde,2023-05-22): Call inlineSingleUse twice to further simplify
        -- some cases. We probably want to repeat it until fixpoint. Or, replace
        -- inlineSingleUse with something better. The current implementation is
        -- quite hacky and have not been properly analyzed or tested.
        inlineSingleUse (inlineSingleUse ast)
      case "peval" then
        -- NOTE(2023-06-20,dlunde): Suggestion by Oscar and Johan to use
        -- pattern lowering here. Required even?
        -- let ast = lowerAll ast in
        peval ast
      case _ then
        error (join ["Unknown extract simplification: ",
                     options.extractSimplification])
      end
    in
    -- printLn "-----------------------";
    -- printLn (mexprPPLToString ast);
    ast

  -- Inlines the body of the infer binding without lambdas. This places the
  -- model code in the top-level of the program.
  sem inlineInferBinding : Name -> Expr -> Expr
  sem inlineInferBinding inferId =
  | TmDecl (x & {decl = DeclLet t}) ->
    if nameEq t.ident inferId then bodyWithoutLambdas t.body
    else TmDecl {x with inexpr = inlineInferBinding inferId x.inexpr}
  | TmDecl (x & {decl = DeclRecLets t}) ->
    recursive let findInferBinding = lam bindings.
      match bindings with [bind] ++ bindings then
        if nameEq bind.ident inferId then Some bind
        else findInferBinding bindings
      else None ()
    in
    let removeInferBinding = lam bindings.
      filter (lam bind. not (nameEq bind.ident inferId)) bindings
    in
    match findInferBinding t.bindings with Some bind then
      -- Inline the body of the infer binding and place it after the recursive
      -- let-expression.
      let inexpr = bodyWithoutLambdas bind.body in
      TmDecl {x with decl = DeclRecLets {t with bindings = removeInferBinding t.bindings},
                        inexpr = inexpr}
    else TmDecl {x with inexpr = inlineInferBinding inferId x.inexpr}
  | t -> smap_Expr_Expr (inlineInferBinding inferId) t

  sem bodyWithoutLambdas : Expr -> Expr
  sem bodyWithoutLambdas =
  | TmLam t -> bodyWithoutLambdas t.body
  | t -> withType (TyUnknown {info = infoTm t}) t

  sem removeInfers : Map Name (InferMethod, Name) -> Expr -> Expr
  sem removeInfers inferData =
  | ast ->
    removeInferBindings inferData ast

  -- Removes bindings of identifiers that correspond to extracted infer
  -- expressions from the AST. Note that lambda lifting ensures all bindings
  -- are on top-level, so we do not need to consider the bodies of
  -- let-expressions.
  sem removeInferBindings
    : all data. Map Name data -> Expr -> Expr
  sem removeInferBindings inferData =
  | TmDecl (x & {decl = DeclLet t}) ->
    if mapMem t.ident inferData then removeInferBindings inferData x.inexpr
    else TmDecl {x with inexpr = removeInferBindings inferData x.inexpr}
  | TmDecl (x & {decl = DeclRecLets t}) ->
    let filterBinding = lam bind.
      if mapMem bind.ident inferData then None ()
      else Some bind
    in
    TmDecl {x with decl = DeclRecLets {t with bindings = filterOption (map filterBinding t.bindings)},
                      inexpr = removeInferBindings inferData x.inexpr}
  | t -> smap_Expr_Expr (removeInferBindings inferData) t

  -- Assumes proper symbolization and ANF
  sem inlineSingleUse : Expr -> Expr
  sem inlineSingleUse =
  | expr ->
    -- Count uses of lambdas
    let m = determineInline (mapEmpty nameCmp) expr in
    -- printLn (strJoin ",\n" (map
    --   (lam t. join [(nameGetStr t.0), "->", (bool2string t.1)])
    --   (mapToSeq m)));

    -- Inline functions
    match inlineSingleUseH m (mapEmpty nameCmp) expr with (_,expr) in
    expr

  sem determineInline : Map Name Bool -> Expr -> Map Name Bool
  sem determineInline m =
  | TmDecl (x & {decl = DeclLet t}) ->
    let m = determineInline m t.body in
    let m =
      -- Only inline certain things. In particular, do not move constructs with
      -- side-effects
      match t.body with TmLam _ | TmVar _ then mapInsert t.ident false m
      else m
    in
    determineInline m x.inexpr
  | TmVar t ->
    match mapLookup t.ident m with Some b then
      if b then mapRemove t.ident m -- More than a single use
      else mapInsert t.ident true m -- First use
    else m
  | expr -> sfold_Expr_Expr determineInline m expr

  sem inlineSingleUseH : Map Name Bool
                         -> Map Name Expr -> Expr -> (Map Name Expr, Expr)
  sem inlineSingleUseH m me =
  | TmDecl (x & {decl = DeclLet t}) ->
    match inlineSingleUseH m me t.body with (me,body) in
    let inline = match mapLookup t.ident m with Some true then true else false in
    let me = if inline then mapInsert t.ident body me else me in
    match inlineSingleUseH m me x.inexpr with (me,inexpr) in
    if inline then
      match mapLookup t.ident me with None _ then
        -- The function was inlined, remove it
        (me, inexpr)
      else
        (me, TmDecl {x with decl = DeclLet {t with body = body}, inexpr = inexpr})
    else
      (me, TmDecl {x with decl = DeclLet {t with body = body}, inexpr = inexpr})
  | TmApp t & tm ->
    recursive let rec = lam me. lam expr.
      match expr with TmApp t then
        let rhs = inlineSingleUseH m me t.rhs in
        match rec rhs.0 t.lhs with (ls, me, lhs) in
        match lhs with TmLam tl then
          let l = nlet_ tl.ident tl.tyAnnot rhs.1 in
          (cons l ls, me, tl.body)
        else (ls, me, TmApp {t with lhs = lhs, rhs = rhs.1})
      else
        match inlineSingleUseH m me expr with (me, expr) in
        ([], me, expr)
    in
    match rec me tm with (ls, me, tm) in
    (me, foldr bind_ tm (reverse ls))

  | TmVar t ->
    match mapLookup t.ident me with Some expr then (mapRemove t.ident me, expr)
    else (me, TmVar t)

  | expr -> smapAccumL_Expr_Expr (inlineSingleUseH m) me expr
end

let extractInfer = use DPPLExtract in extractInfer
