include "mexpr/phase-stats.mc"
include "mexpr/inline-single-use-simple.mc"
include "mexpr/lamlift.mc"
include "../dists.mc"
include "../../inference/pval-graph.mc"

include "./remove-second-class-lambdas.mc"
include "./idealized-transformation.mc"
include "./state-transformation.mc"
include "./eta-expansion.mc"

lang SimplePValGraphCompiler
  = SimplePValGraphMethod + PhaseStats + InferenceInterface
  + LowerNestedPatterns + InlineSingleUse + RemoveSecondClassFunctions
  + IdealizedPValTransformation + PValStateTransformation + EtaExpansion
  + TransformDist + MExprLambdaLift

  sem pickRuntime = | SimplePValGraph _ -> ("pval-graph/runtime-pval-simple.mc", mapEmpty cmpString)
  sem pickCompiler = | SimplePValGraph x -> compileSimplePValGraph x

  sem _cmpInferMethod = | (SimplePValGraph _, SimplePValGraph _) -> 0

  sem liftTysAndConstructors : Expr -> Expr
  sem liftTysAndConstructors = | tm ->
    match liftTysAndConstructorsExpr [] tm with (decls, tm) in
    bindall_ decls tm

  sem liftTysAndConstructorsExpr : [Decl] -> Expr -> ([Decl], Expr)
  sem liftTysAndConstructorsExpr decls =
  | TmDecl {decl = decl & DeclType x, inexpr = inexpr} ->
    liftTysAndConstructorsExpr (snoc decls decl) inexpr
  | TmDecl {decl = decl & DeclConDef _, inexpr = inexpr} ->
    liftTysAndConstructorsExpr (snoc decls decl) inexpr
  | tm & TmOpaque _ -> (decls, tm)
  | tm -> smapAccumL_Expr_Expr liftTysAndConstructorsExpr decls tm

  sem compileSimplePValGraph : SimplePValGraphConfig -> InferenceInterface -> Expr
  sem compileSimplePValGraph config = | x ->
    let log = mkPhaseLogState x.options.debugDumpPhases x.options.debugPhases x.options.invariantsToCheck in

    let preprocess = lam ast.
      -- let ast = lowerAll ast in
      -- let ast = inlineSingleUseLets ast in
      -- let ast = etaExpand false as in
      ast in
    let ast = x.extractNoHigherOrderConsts preprocess in
    endPhaseStatsExpr log "extract-no-higher-one" ast;

    let ast = lowerAll ast in
    endPhaseStatsExpr log "lower-all-one" ast;

    let ast = inlineSingleUseLets ast in
    endPhaseStatsExpr log "inline-single-use-lets-one" ast;

    let ast = stripTempLam ast in
    endPhaseStatsExpr log "strip-temp-lam-one" ast;

    -- TODO(vipa, 2026-05-26): Everything after this point, to the
    -- next TODO with the same date, is here to make sure we have a
    -- well-typed AST where all function calls are statically known,
    -- and all functions appear either fully applied, or not applied
    -- at all. The current path goes through remSecLamExpr, which can
    -- fail, and is somewhat fickle. It also isn't good enough at
    -- producing a well-typed AST afterwards, so we have to re-run
    -- typecheck, which also makes it more complicated to return
    -- custom data types (because it looks to the typechecker like
    -- they escape here, even though they won't in the greater
    -- program.

    let ast = etaExpand false ast in
    endPhaseStatsExpr log "eta-expand-one" ast;

    let ast = liftLambdas ast in
    endPhaseStatsExpr log "lift-lambdas-one" ast;

    let initEnv =
      { depth = 0
      , bindings = mapEmpty nameCmp
      } in
    let initState =
      let cmpF = lam l. lam r.
        let res = eitherCmp cmpConst nameCmp l.fName r.fName in
        if neqi 0 res then res else
        subi l.argCount r.argCount in
      let cmpGen = lam l. lam r.
        let res = nameCmp l.fName r.fName in
        if neqi 0 res then res else
        seqCmp (optionCmp cmpF) l.args r.args in
      { generated = mapEmpty cmpGen
      , toInsert = mapEmpty subi
      , pendingRecursiveDefinitions = []
      , currMaxDepthUsed = pureRMaxInt 0
      , depths = mapEmpty nameCmp
      } in
    let ast = stripTempLam (remSecLamExpr initEnv initState ast).1 in
    endPhaseStatsExpr log "remove-second-class-lambdas-one" ast;

    let freeVariables = freeVars ast in
    let freeVariables : Map Name Type =
      recursive let work = lam acc. lam tm.
        match tm with TmVar x then
          if setMem x.ident freeVariables
          then mapInsert x.ident x.ty acc
          else acc
        else sfold_Expr_Expr work acc tm in
      work (mapEmpty nameCmp) ast in
    let tyEnv = { typcheckEnvEmpty with varEnv = freeVariables } in

    let ast = liftTysAndConstructors ast in
    endPhaseStatsExpr log "lift-tys-and-constructors-one" ast;

    recursive let typeCheckDeclIgnoreEscape = lam tyEnv. lam tm.
      -- NOTE(vipa, 2026-05-26): Typechecking constructors and the
      -- like normally check that the newly bound types do not escape,
      -- but that's directly bad here, because we have a local *copy*
      -- of each type in the AST we've extracted, we'll remove it
      -- later and reuse what's bound outside, so what looks like an
      -- escape in the local AST isn't one in the larger AST. This
      -- function is implements typeCheckExpr for TmDecl as normal,
      -- except it doesn't do this escape check.
      match tm with TmDecl x then
        match typeCheckDecl tyEnv x.decl with (tyEnv, decl) in
        let inexpr = typeCheckDeclIgnoreEscape tyEnv x.inexpr in
        TmDecl {x with decl = decl, inexpr = inexpr, ty = tyTm inexpr}
      else typeCheckExpr tyEnv tm in
    -- NOTE(vipa, 2025-12-02): This is regrettable, but it's kinda
    -- hard to substitute in correct types inside polymorphic
    -- functions that have been specialized. We want something better
    -- here later.
    let ast = removeMetaVarExpr (typeCheckDeclIgnoreEscape tyEnv ast) in
    endPhaseStatsExpr log "typecheck-one" ast;

    let initState =
      { specializations = mapEmpty nameCmp
      } in
    let initScope =
      { functionDefinitions = mapEmpty nameCmp
      , depth = 0
      , valueScope = mapEmpty nameCmp
      , revValueScope = mapEmpty nameCmp
      , conScope = mapEmpty nameCmp
      , tyConAsPure = mapEmpty nameCmp
      } in
    let ast = stripTempLam (specializeExprReturn initScope initState freeVariables ast) in
    endPhaseStatsExpr log "idealized-transformation-one" ast;

    -- TODO(vipa, 2026-05-26): This is the end of the block mentioned
    -- in the TODO above with the same date.

    let getPValVar = lam str. appFromEnv x.runtime (concat "vSimplePValGraph_" str) [] in
    let initTransEnv =
      { currStateName = x.stateName
      , functions = mapEmpty nameCmp
      , p_pure = getPValVar "p_pure"
      , p_map = getPValVar "p_map"
      , p_subMap = getPValVar "p_subMap"
      , p_apply = getPValVar "p_apply"
      , p_subApply = getPValVar "p_subApply"
      , p_bind = getPValVar "p_bind"
      , p_join = getPValVar "p_join"
      , p_assume = getPValVar "p_assume"
      , p_select = getPValVar "p_select"
      , p_weight = getPValVar "p_weight"
      , p_export = getPValVar "p_export"
      , p_traverseSeq = getPValVar "p_traverseSeq"
      , mapAccumL = getPValVar "reexportMapAccumL"
      , storeAssume = getPValVar "simpleStoreAssume"
      , storeSubmodel = getPValVar "simpleStoreSubmodel"
      , storeExport = getPValVar "simpleStoreExport"
      , storeWeight = getPValVar "simpleStoreWeight"
      , initSubmodel = seq_ []
      } in
    let ast = pvalTrans initTransEnv ast in
    endPhaseStatsExpr log "state-transformation-one" ast;

    let ast = stripTempLam ast in
    endPhaseStatsExpr log "strip-temp-lam-one" ast;

    let ast = mapPre_Expr_Expr (transformTmDist x.dists) ast in
    endPhaseStatsExpr log "transform-tm-one" ast;

    ast
end
