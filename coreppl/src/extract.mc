include "coreppl.mc"
include "parser.mc"

include "name.mc"
include "mexpr/extract.mc"
include "mexpr/lamlift.mc"

lang DPPLExtract = DPPLParser + MExprExtract + MExprLambdaLift
  type ModelRepr = (Expr, [(Name, Type)])

  -- Takes an AST and extracts the models used within infer expressions.
  --
  -- The returned value is a tuple consisting of:
  -- 1. The original AST, where the infer expressions have been replaced with
  --    calls to the inference algorithms.
  -- 2. A map with an entry for each inference method. Every inference method
  --    is mapped to a sequence of records describing each model.
  sem extractInfer : Map InferMethod RuntimeEntry -> Expr
                  -> (Expr, Map InferMethod (Map Name ModelRepr))
  sem extractInfer runtimes =
  | ast ->
    -- Symbolize and type-check the AST (required by lambda lifting)
    let ast = symbolize ast in
    let ast = typeCheck ast in

    match bindInferExpressions runtimes ast with (data, ast) in
    match liftLambdasWithSolutions ast with (solutions, ast) in

    let ast = eliminateThunkParameter data ast in

    let modelAsts =
      mapMapWithKey
        (lam method. lam.
          -- Collect the identifiers of all infer expressions that use the
          -- current inference method.
          let inferIds =
            mapFoldWithKey
              (lam acc. lam id. lam m.
                if eqInferMethod method m then cons id acc
                else acc)
              [] data in

          -- Construct a map from each infer identifier to an extracted AST.
          mapFromSeq nameCmp
            (map
              (lam inferId.
                match mapLookup inferId solutions with Some paramMap then
                  let params = mapBindings paramMap in
                  let astx = extractInferAst inferId ast in
                  (inferId, (astx, params))
                else error "Lambda lifting was not correctly applied to infer")
              inferIds))
        runtimes in

    -- Map the infer identifiers to the identifiers used by its corresponding
    -- runtime.
    let inferIds : Map Name RuntimeEntry =
      mapMapWithKey
        (lam id. lam method.
          match mapLookup method runtimes with Some entry then
            entry
          else error (join ["Runtime not found for method '",
                            inferMethodToString method, "'"]))
        data in

    (removeInfers solutions inferIds ast, modelAsts)

  -- Binds all infer expressions in the provided AST to a variable, such that
  -- an expression 'infer e' is rewritten as 'let t = e in t'. The result is
  -- the updated AST, and a map from the newly introduced identifiers to the
  -- inference method they involve.
  sem bindInferExpressions : Map InferMethod RuntimeEntry -> Expr
                          -> (Map Name InferMethod, Expr)
  sem bindInferExpressions runtimes =
  | ast -> bindInferExpressionsH runtimes (mapEmpty nameCmp) ast

  sem bindInferExpressionsH : Map InferMethod RuntimeEntry
                           -> Map Name InferMethod
                           -> Expr -> (Map Name InferMethod, Expr)
  sem bindInferExpressionsH runtimes acc =
  | TmInfer t ->
    let inferId = nameSym "t" in
    let info = t.info in
    let inferBinding = TmLet {
      ident = inferId,
      tyBody = tyTm t.model,
      body = t.model,
      inexpr = TmVar {ident = inferId, ty = tyTm t.model, info = info, frozen = false},
      ty = tyTm t.model, info = info} in
    (mapInsert inferId t.method acc, inferBinding)
  | t -> smapAccumL_Expr_Expr (bindInferExpressionsH runtimes) acc t

  sem eliminateThunkParameter : Map Name InferMethod -> Expr -> Expr
  sem eliminateThunkParameter inferMethods =
  | t -> t

  -- Extracts an AST consisting of the model of all infers using the same
  -- inference method, and the expressions these depend on.
  sem extractInferAst : Name -> Expr -> Expr
  sem extractInferAst inferId =
  | ast ->
    let ast = extractAst (setOfSeq nameCmp [inferId]) ast in
    inlineInferBinding inferId ast

  -- Inlines the body of the infer binding without lambdas. This places the
  -- model code in the top-level of the program.
  sem inlineInferBinding : Name -> Expr -> Expr
  sem inlineInferBinding inferId =
  | TmLet t ->
    if nameEq t.ident inferId then bodyWithoutLambdas t.body
    else TmLet {t with inexpr = inlineInferBinding inferId t.inexpr}
  | t -> smap_Expr_Expr (inlineInferBinding inferId) t

  sem bodyWithoutLambdas : Expr -> Expr
  sem bodyWithoutLambdas =
  | TmLam t -> bodyWithoutLambdas t.body
  | t -> t

  sem removeInfers : Map Name (Map Name Type) -> Map Name RuntimeEntry -> Expr -> Expr
  sem removeInfers solutions inferIds =
  | ast ->
    let ast = removeInferBindings inferIds ast in
    replaceInferApplication solutions inferIds ast

  -- Removes bindings of identifiers that correspond to extracted infer
  -- expressions from the AST. Note that lambda lifting ensures all bindings
  -- are on top-level, so we do not need to consider the bodies of
  -- let-expressions.
  sem removeInferBindings : Map Name RuntimeEntry -> Expr -> Expr
  sem removeInferBindings inferIds =
  | TmLet t ->
    if mapMem t.ident inferIds then removeInferBindings inferIds t.inexpr
    else TmLet {t with inexpr = removeInferBindings inferIds t.inexpr}
  | t -> smap_Expr_Expr (removeInferBindings inferIds) t

  -- Replaces the application of the infer binding with a call to the run
  -- function of the corresponding runtime.
  sem replaceInferApplication : Map Name (Map Name Type) -> Map Name RuntimeEntry
                              -> Expr -> Expr
  sem replaceInferApplication solutions inferIds =
  | app & (TmApp t) ->
    let collectAppArgs = lam app.
      recursive let work = lam acc. lam e.
        match e with TmApp t then work (cons t.rhs acc) t.lhs
        else (e, acc)
      in work [] app
    in
    match collectAppArgs app with (TmVar {ident = id}, args) then
      match mapLookup id inferIds with Some ids then
        match mapLookup ids.runId solutions with Some paramMap then
          let params =
            map
              (lam idTy.
                match idTy with (id, ty) in
                TmVar {ident = id, ty = ty, info = infoTy ty, frozen = false})
              (mapBindings paramMap) in
          appSeq_
            (nvar_ ids.runId)
            (snoc params (appSeq_ (nvar_ id) args))
        else app
      else app
    else app
  | t -> smap_Expr_Expr (replaceInferApplication solutions inferIds) t
end

let extractInfer = use DPPLExtract in extractInfer
