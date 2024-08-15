include "../coreppl.mc"
include "../parser.mc"
include "../dppl-arg.mc"

include "name.mc"
include "peval/peval.mc"
include "mexpr/demote-recursive.mc"
include "mexpr/shallow-patterns.mc"
include "mexpr/extract.mc"
include "mexpr/lamlift.mc"
include "pmexpr/utils.mc"

lang DPPLExtract =
  DPPLParser + MExprExtract + MExprLambdaLift + MExprDemoteRecursive +
  MExprPEval + MExprLowerNestedPatterns

  type ModelRepr = {
    ast : Expr,
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
  sem extractInfer : Options -> Map InferMethod InferRuntimeEntry -> Expr
                  -> (Expr, Map Name ModelRepr)
  sem extractInfer options runtimes =
  | ast ->
    match bindInferExpressions runtimes ast with (data, ast) in
    match liftLambdasWithSolutions ast with (solutions, ast) in

    -- NOTE(vipa, 2023-10-10): This code was written before
    -- `liftLambdasWithSolutions` handled tyVars, thus we discard that
    -- information for now.
    let solutions = mapMap (lam x. x.vars) solutions in

    let modelAsts : Map Name ModelRepr =
      mapMapWithKey
        (lam inferId. lam method.
          match mapLookup inferId solutions with Some paramMap then
            let params = mapBindings paramMap in
            let ast = extractInferAst options inferId ast in
            {ast = ast, method = method, params = params}
          else error "Lambda lifting was not correctly applied to infer")
        data in

    -- Construct a map from the identifier of an infer binding to its
    -- corresponding inference method configuration and a runtime entry.
    let inferData : Map Name (InferMethod, InferRuntimeEntry) =
      mapMapWithKey
        (lam. lam model.
          match model with {method = method} in
          match mapLookup method runtimes with Some entry then
            (method, entry)
          else
            match pprintInferMethod 0 pprintEnvEmpty method with (_, method) in
            error (join ["Missing runtime for method (", method, ")"]))
        modelAsts in

    (removeInfers solutions inferData ast, modelAsts)

  -- Binds all infer expressions in the provided AST to a variable, such that
  -- an expression 'infer e' is rewritten as 'let t = e in t'. The result is
  -- the updated AST, and a map from the newly introduced identifiers to the
  -- inference method they involve.
  sem bindInferExpressions : Map InferMethod InferRuntimeEntry -> Expr
                          -> (Map Name InferMethod, Expr)
  sem bindInferExpressions runtimes =
  | ast -> bindInferExpressionsH runtimes (mapEmpty nameCmp) ast

  sem bindInferExpressionsH : Map InferMethod InferRuntimeEntry
                           -> Map Name InferMethod
                           -> Expr -> (Map Name InferMethod, Expr)
  sem bindInferExpressionsH runtimes acc =
  | TmInfer t ->
    let inferId = nameSym "t" in
    let info = t.info in
    let inferBinding = TmLet {
      ident = inferId,
      tyAnnot = tyarrow_ tyunit_ (tyTm t.model),
      tyBody = tyarrow_ tyunit_ (tyTm t.model),
      body = TmLam {
        ident = nameNoSym "",
        tyAnnot = tyunit_,
        tyParam = tyunit_,
        body = TmApp {
          lhs = t.model, rhs = unit_, ty = tyTm t.model, info = info},
        ty = tyarrow_ tyunit_ (tyTm t.model), info = info},
      inexpr = TmVar {ident = inferId, ty = t.ty, info = info, frozen = false},
      ty = t.ty, info = info} in
    (mapInsert inferId t.method acc, inferBinding)
  | t -> smapAccumL_Expr_Expr (bindInferExpressionsH runtimes) acc t

  -- Extracts an AST consisting of the model whose binding has the provided
  -- inference ID.
  sem extractInferAst : Options -> Name -> Expr -> Expr
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
  | TmLet t ->
    if nameEq t.ident inferId then bodyWithoutLambdas t.body
    else TmLet {t with inexpr = inlineInferBinding inferId t.inexpr}
  | TmRecLets t ->
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
      TmRecLets {t with bindings = removeInferBinding t.bindings,
                        inexpr = inexpr}
    else TmRecLets {t with inexpr = inlineInferBinding inferId t.inexpr}
  | t -> smap_Expr_Expr (inlineInferBinding inferId) t

  sem bodyWithoutLambdas : Expr -> Expr
  sem bodyWithoutLambdas =
  | TmLam t -> bodyWithoutLambdas t.body
  | t -> withType (TyUnknown {info = infoTm t}) t

  sem removeInfers : Map Name (Map Name Type)
                  -> Map Name (InferMethod, InferRuntimeEntry) -> Expr -> Expr
  sem removeInfers solutions inferData =
  | ast ->
    let ast = removeInferBindings inferData ast in
    replaceInferApplication solutions inferData ast

  -- Removes bindings of identifiers that correspond to extracted infer
  -- expressions from the AST. Note that lambda lifting ensures all bindings
  -- are on top-level, so we do not need to consider the bodies of
  -- let-expressions.
  sem removeInferBindings
    : Map Name (InferMethod, InferRuntimeEntry) -> Expr -> Expr
  sem removeInferBindings inferData =
  | TmLet t ->
    if mapMem t.ident inferData then removeInferBindings inferData t.inexpr
    else TmLet {t with inexpr = removeInferBindings inferData t.inexpr}
  | TmRecLets t ->
    let filterBinding = lam bind.
      if mapMem bind.ident inferData then None ()
      else Some bind
    in
    TmRecLets {t with bindings = filterOption (map filterBinding t.bindings),
                      inexpr = removeInferBindings inferData t.inexpr}
  | t -> smap_Expr_Expr (removeInferBindings inferData) t

  -- Replaces the application of the infer binding with a call to the run
  -- function of the corresponding runtime.
  sem replaceInferApplication : Map Name (Map Name Type)
                              -> Map Name (InferMethod, InferRuntimeEntry)
                              -> Expr -> Expr
  sem replaceInferApplication solutions inferData =
  | e & (TmVar _ | TmApp _) ->
    let collectAppArgs = lam e.
      recursive let work = lam acc. lam e.
        match e with TmApp t then work (cons t.rhs acc) t.lhs
        else (e, acc)
      in work [] e
    in
    match collectAppArgs e with (TmVar {ident = id, info = info}, args) then
      match mapLookup id inferData with Some (method, entry) then
        appf2_ (nvar_ entry.runId)
          (inferMethodConfig info method)
          (appSeq_ (nvar_ id) args)
      else smap_Expr_Expr (replaceInferApplication solutions inferData) e
    else e
  | t -> smap_Expr_Expr (replaceInferApplication solutions inferData) t

  sem bindingsUsed : Set Name -> Bool -> Expr -> Bool
  sem bindingsUsed bs acc =
  | TmVar t -> if setMem t.ident bs then true else acc
  | expr ->
    if acc then acc else
      sfold_Expr_Expr (bindingsUsed bs) acc expr

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
  | TmLet t ->
    let m = determineInline m t.body in
    let m =
      -- Only inline certain things. In particular, do not move constructs with
      -- side-effects
      match t.body with TmLam _ | TmVar _ then mapInsert t.ident false m
      else m
    in
    determineInline m t.inexpr
  | TmVar t ->
    match mapLookup t.ident m with Some b then
      if b then mapRemove t.ident m -- More than a single use
      else mapInsert t.ident true m -- First use
    else m
  | expr -> sfold_Expr_Expr determineInline m expr

  sem inlineSingleUseH : Map Name Bool
                         -> Map Name Expr -> Expr -> (Map Name Expr, Expr)
  sem inlineSingleUseH m me =
  | TmLet t ->
    match inlineSingleUseH m me t.body with (me,body) in
    let inline = match mapLookup t.ident m with Some true then true else false in
    let me = if inline then mapInsert t.ident body me else me in
    match inlineSingleUseH m me t.inexpr with (me,inexpr) in
    if inline then
      match mapLookup t.ident me with None _ then
        -- The function was inlined, remove it
        (me, inexpr)
      else
        (me, TmLet {t with body = body, inexpr = inexpr})
    else
      (me, TmLet {t with body = body, inexpr = inexpr})
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

  -- Extracts solveode terms to a map from ODE solver methods to names. Each
  -- solveode term is replaced by an application of an identifier the named
  -- according to the returned map.
  sem extractSolveODE : Expr -> (Map ODESolverMethod Name, Expr)
  sem extractSolveODE =| tm ->
    recursive let inner = lam acc. lam tm.
      match tm with TmSolveODE r then
        let f : Name -> Expr = lam name.
          appf4_ (nvar_ name)
            (odeSolverMethodConfig r.info r.method) r.model r.init r.endTime
        in
        optionMapOrElse
          (lam.
            let name = nameSym (odeSolverMethodToString r.method) in
            let acc = mapInsert r.method name acc in
            (acc, f name))
          (lam name. (acc, f name))
          (mapLookup r.method acc)
      else smapAccumL_Expr_Expr inner acc tm
    in
    inner (mapEmpty cmpODESolverMethod) tm
end

let extractInfer = use DPPLExtract in extractInfer
let extractSolveODE = use DPPLExtract in extractSolveODE
