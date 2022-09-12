include "coreppl.mc"
include "parser.mc"

include "name.mc"
include "mexpr/extract.mc"
include "mexpr/lamlift.mc"

lang DPPLExtract = DPPLParser + MExprExtract + MExprLambdaLift
  -- The data representing an infer expression.
  type ModelData = {
    -- The AST representation of the model.
    ast : Expr,

    -- The inference method used for this model.
    method : InferMethod,

    -- Identifiers for the 'model' and 'run' bindings generated for defining
    -- the model-specific code and the inference-specific run function.
    modelId : Name,
    runId : Name,

    -- Parameters captured for the model binding by lambda lifting
    params : [(Name, Type)]
  }

  sem defaultModelData : InferMethod -> ModelData
  sem defaultModelData =
  | method -> { ast = unit_, method = method, modelId = nameSym "model"
              , runId = nameNoSym "", params = [] }

  -- Takes an AST and extracts the models used within infer expressions.
  --
  -- The returned value is a tuple consisting of:
  -- 1. The original AST, where the infer expressions have been replaced with
  --    calls to the inference algorithms.
  -- 2. A map with an entry for each inference method. Every inference method
  --    is mapped to a sequence of records describing each model.
  sem extractInfer : Expr -> (Expr, Map InferMethod [ModelData])
  sem extractInfer =
  | ast ->
    -- Symbolize and type-check the AST (required by lambda lifting)
    let ast = symbolize ast in
    let ast = typeCheck ast in

    match bindInferExpressions ast with (data, ast) in
    match liftLambdasWithSolutions ast with (solutions, ast) in

    let modelData : Map Name ModelData =
      mapFoldWithKey
        (lam acc. lam. lam models.
          let runId = nameSym "run" in
          mapFoldWithKey
            (lam acc. lam modelId. lam modelData.
              let ast = extractInferAst ast modelId in
              let params =
                match mapLookup modelId solutions with Some params then
                  mapBindings params
                else []
              in
              let modelData =
                {modelData with ast = ast, runId = runId, params = params} in
              mapInsert modelId modelData acc)
            acc models)
        (mapEmpty nameCmp) data in
    let inferData : Map InferMethod [ModelData] =
      mapFoldWithKey
        (lam acc. lam modelId. lam modelData.
          match mapLookup modelData.method acc with Some s then
            mapInsert modelData.method (snoc s modelData) acc
          else
            mapInsert modelData.method [modelData] acc)
        (mapEmpty cmpInferMethod) modelData in
    (removeInfers solutions modelData ast, inferData)

  -- Binds all infer expressions in the provided AST to a variable, such that
  -- an expression 'infer e' is rewritten as 'let t = e in t'. The result is
  -- the updated AST and a sequence containing the newly introduced identifiers
  -- 't'.
  sem bindInferExpressions : Expr -> (Map InferMethod (Map Name ModelData), Expr)
  sem bindInferExpressions =
  | ast -> bindInferExpressionsH (mapEmpty cmpInferMethod) ast

  sem bindInferExpressionsH : Map InferMethod (Map Name ModelData) -> Expr
                           -> (Map InferMethod (Map Name ModelData), Expr)
  sem bindInferExpressionsH inferData =
  | TmInfer t ->
    let inferId = nameSym "t" in
    let info = t.info in
    let inferBinding = TmLet {
      ident = inferId,
      tyBody = tyTm t.model,
      body = t.model,
      inexpr = TmApp {
        lhs = TmVar {ident = inferId, ty = tyTm t.model, info = info, frozen = false},
        rhs = TmRecord {bindings = mapEmpty cmpSID, ty = tyunknown_, info = info},
        ty = tyunknown_, info = info},
      ty = tyTm t.model, info = info} in
    let inferData =
      let modelMap =
        match mapLookup t.method inferData with Some modelMap then modelMap
        else mapEmpty nameCmp in
      let modelMap = mapInsert inferId (defaultModelData t.method) modelMap in
      mapInsert t.method modelMap inferData in
    (inferData, inferBinding)
  | t -> smapAccumL_Expr_Expr bindInferExpressionsH inferData t

  -- Extracts an AST consisting of the model of an 'infer' and the expressions
  -- it depends on.
  sem extractInferAst : Expr -> Name -> Expr
  sem extractInferAst ast =
  | inferId ->
    let inferAst = extractAst (setOfSeq nameCmp [inferId]) ast in
    let inferAst = inlineInferBinding inferId inferAst in
    inferAst

  -- Inlines the body of the binding corresponding to the extracted 'infer'.
  sem inlineInferBinding : Name -> Expr -> Expr
  sem inlineInferBinding inferId =
  | TmLet t ->
    if nameEq t.ident inferId then innerLambdaBody t.body
    else TmLet {t with inexpr = inlineInferBinding inferId t.inexpr}
  | TmRecLets t ->
    TmRecLets {t with inexpr = inlineInferBinding inferId t.inexpr}
  | TmType t -> TmType {t with inexpr = inlineInferBinding inferId t.inexpr}
  | TmConDef t -> TmConDef {t with inexpr = inlineInferBinding inferId t.inexpr}
  | TmUtest t -> TmUtest {t with next = inlineInferBinding inferId t.next}
  | TmExt t -> TmExt {t with inexpr = inlineInferBinding inferId t.inexpr}
  | t -> t

  sem innerLambdaBody : Expr -> Expr
  sem innerLambdaBody =
  | TmLam t -> innerLambdaBody t.body
  | t -> t

  sem removeInfers : Map Name (Map Name Type) -> Map Name ModelData -> Expr -> Expr
  sem removeInfers solutions data =
  | ast ->
    let ast = removeInferBindings data ast in
    replaceInferApplications solutions data ast

  -- Removes the bindings that correspond to the extracted bodies of 'infer'
  -- expressions from the AST.
  sem removeInferBindings : Map Name ModelData -> Expr -> Expr
  sem removeInferBindings data =
  | TmLet t ->
    if mapMem t.ident data then removeInferBindings data t.inexpr
    else TmLet {t with inexpr = removeInferBindings data t.inexpr}
  | t -> smap_Expr_Expr (removeInferBindings data) t

  sem replaceInferApplications : Map Name (Map Name Type) -> Map Name ModelData
                              -> Expr -> Expr
  sem replaceInferApplications solutions data =
  | app & (TmApp t) ->
    let collectAppArgs = lam app.
      recursive let work = lam acc. lam e.
        match e with TmApp t then work (cons t.rhs acc) t.lhs
        else (e, acc)
      in work [] app
    in
    match collectAppArgs app with (TmVar {ident = id}, args) then
      match mapLookup id data with Some modelData then
        let paramVars =
          map
            (lam entry.
              match entry with (id, ty) in
              TmVar {ident = id, ty = ty, info = t.info, frozen = false})
            modelData.params in
        app_
          (nvar_ modelData.runId)
          (appSeq_ (nvar_ modelData.modelId) paramVars)
      else app
    else app
  | t -> smap_Expr_Expr (replaceInferApplications solutions data) t
end

let extractInfer = use DPPLExtract in extractInfer
