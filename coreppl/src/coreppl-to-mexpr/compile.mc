include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/type.mc"
include "sys.mc"

include "../coreppl.mc"
include "../extract.mc"
include "../inference-common/smc.mc"
include "../parser.mc"
include "../dppl-arg.mc"
include "../transformation.mc"

include "./common.mc"
include "runtimes.mc"

-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile =
  MExprPPL + Resample + Externals + DPPLParser + DPPLExtract + LoadRuntime +
  Transformation

  sem transformModelAst : Options -> Expr -> Expr
  sem transformModelAst options =
  | modelAst ->
    -- Transform the model AST, if the flag is set
    let ast =
      if options.transform then
        transform modelAst
      else modelAst in

    -- Optionally print the model AST
    (if options.printModel then
      printLn (mexprPPLToString ast)
    else ());

    ast

  sem mexprCompile : Options -> Map InferMethod RuntimeEntry -> Expr
                  -> Map Name ModelRepr -> Expr
  sem mexprCompile options runtimes mainAst =
  | modelAsts ->
    let modelAsts =
      mapMapWithKey
        (lam id. lam model.
          match model with {ast = ast, method = method, params = params} in
          match loadCompiler options method with (_, compile) in
          match mapLookup method runtimes with Some entry then
            let compileExt = compile entry.externals in
            let modelAst = transformModelAst options ast in
            let ast = compileModel compileExt entry id model in
            removeExternalDefs entry.externals ast
          else
            match pprintInferMethod 0 pprintEnvEmpty method with (_, method) in
            error (join ["Runtime definition missing for (", method, ")"]))
        modelAsts in

    -- We use a map from model identifier to model AST, as we do not have to
    -- distinguish between which inference method they use at this point.
    let modelMap : Map Name Expr =
      mapFoldWithKey
        (lam acc. lam id. lam model.
          mapInsert id model acc)
        (mapEmpty nameCmp) modelAsts in

    -- Insert all models into the main AST at the first point where any of the
    -- models are used.
    let prog = insertModels modelMap mainAst in

    -- TODO(larshum, 2022-10-18): If the returned AST has been type-checked, we
    -- get an error when re-parsing the pretty-printed program with mi, because
    -- recursive-let expressions have missing TyAlls. Therefore we return the
    -- non type-checked version for now.
    (if options.debugMExprCompile then
      -- Check that the combined program type checks
      typeCheck prog; ()
    else ());

    -- Return complete program
    prog

  sem compileModel : (Expr -> Expr) -> RuntimeEntry -> Name -> ModelRepr -> Expr
  sem compileModel compile entry modelId =
  | {ast = modelAst, params = modelParams} ->

    -- Symbolize model using the symbolization environment from the top-level
    -- of its corresponding runtime as a starting point.
    let prog = symbolizeExpr entry.topSymEnv modelAst in

    -- Apply inference-specific transformation
    let prog = compile prog in

    -- Update all unsymbolized references to refer to their corresponding
    -- definition in the runtime AST.
    match entry.topSymEnv with {varEnv = varEnv, conEnv = conEnv, tyConEnv = tyConEnv} in
    let stateVarId = nameSym "state" in
    let unsymIds =
      mapUnion
        (mapUnion (mapUnion varEnv conEnv) tyConEnv)
        (mapFromSeq cmpString (map (lam id. (nameGetStr id, id)) [stateVarId]))
    in
    let prog = replaceUnsymbolizedIds unsymIds prog in

    nulet_ modelId
      (nlams_ (snoc modelParams (stateVarId, ntycon_ entry.stateId)) prog)

  sem replaceUnsymbolized : Map String Name -> Name -> Name
  sem replaceUnsymbolized ids =
  | id ->
    if not (nameHasSym id) then
      optionGetOrElse (lam. id) (mapLookup (nameGetStr id) ids)
    else id

  sem replaceUnsymbolizedIds : Map String Name -> Expr -> Expr
  sem replaceUnsymbolizedIds ids =
  | TmVar t ->
    TmVar {t with ident = replaceUnsymbolized ids t.ident,
                  ty = replaceUnsymbolizedIdsType ids t.ty}
  | TmType t ->
    TmType {t with ident = replaceUnsymbolized ids t.ident,
                   tyIdent = replaceUnsymbolizedIdsType ids t.tyIdent,
                   inexpr = replaceUnsymbolizedIds ids t.inexpr,
                   ty = replaceUnsymbolizedIdsType ids t.ty}
  | TmConDef t ->
    TmConDef {t with ident = replaceUnsymbolized ids t.ident,
                     tyIdent = replaceUnsymbolizedIdsType ids t.tyIdent,
                     inexpr = replaceUnsymbolizedIds ids t.inexpr,
                     ty = replaceUnsymbolizedIdsType ids t.ty}
  | TmConApp t ->
    TmConApp {t with ident = replaceUnsymbolized ids t.ident,
                     body = replaceUnsymbolizedIds ids t.body,
                     ty = replaceUnsymbolizedIdsType ids t.ty}
  | t ->
    let t = smap_Expr_Expr (replaceUnsymbolizedIds ids) t in
    let t = smap_Expr_Type (replaceUnsymbolizedIdsType ids) t in
    let t = smap_Expr_Pat (replaceUnsymbolizedIdsPat ids) t in
    withType (replaceUnsymbolizedIdsType ids (tyTm t)) t

  sem replaceUnsymbolizedIdsType : Map String Name -> Type -> Type
  sem replaceUnsymbolizedIdsType ids =
  | TyCon t ->
    TyCon {t with ident = replaceUnsymbolized ids t.ident}
  | ty -> smap_Type_Type (replaceUnsymbolizedIdsType ids) ty

  sem replaceUnsymbolizedIdsPat : Map String Name -> Pat -> Pat
  sem replaceUnsymbolizedIdsPat ids =
  | PatCon t ->
    PatCon {t with ident = replaceUnsymbolized ids t.ident,
                   subpat = replaceUnsymbolizedIdsPat ids t.subpat}
  | p ->
    let p = smap_Pat_Pat (replaceUnsymbolizedIdsPat ids) p in
    withTypePat (replaceUnsymbolizedIdsType ids (tyPat p)) p

  -- We insert all models before the first binding where any of them are used.
  -- This is simple but correct, as we only need them to be placed after the
  -- runtime code.
  sem insertModels : Map Name Expr -> Expr -> Expr
  sem insertModels models =
  | TmLet t ->
    if modelUsedInBody models false t.body then
      bindall_ (snoc (mapValues models) (TmLet t))
    else TmLet {t with inexpr = insertModels models t.inexpr}
  | TmRecLets t ->
    let modelUsedInBinding = lam bind. modelUsedInBody models false bind.body in
    if any identity (map modelUsedInBinding t.bindings) then
      bindall_ (snoc (mapValues models) (TmRecLets t))
    else TmRecLets {t with inexpr = insertModels models t.inexpr}
  | TmType t -> TmType {t with inexpr = insertModels models t.inexpr}
  | TmConDef t -> TmConDef {t with inexpr = insertModels models t.inexpr}
  | TmUtest t -> TmUtest {t with next = insertModels models t.next}
  | TmExt t -> TmExt {t with inexpr = insertModels models t.inexpr}
  | t -> bindall_ (snoc (mapValues models) t)

  sem modelUsedInBody : Map Name Expr -> Bool -> Expr -> Bool
  sem modelUsedInBody models acc =
  | TmVar t ->
    if acc then acc
    else if mapMem t.ident models then true
    else false
  | t -> sfold_Expr_Expr (modelUsedInBody models) acc t
end

let mexprCompile = use MExprCompile in mexprCompile

mexpr

let parse = parseMExprPPLString in

-- TODO(dlunde,2022-10-19): We should also add a `simpleInfer` test that uses
-- the new infer keyword.
let simple = parse "
let t : () -> Float = lam.
  let x = assume (Beta 10.0 5.0) in
  let obs = true in
  observe obs (Bernoulli x);
  x
in
t {}
" in
match use MExprFindSym in findNamesOfStrings ["t"] simple with [Some modelId] in

let truefn = lam. lam. true in

-- TODO(dlunde,2022-10-19): "with debugMExprCompile = true" currently has no
-- effect. This should be fixed so that we also parse and type check the
-- runtime files as part of the utests.
let dummyOptions = {default with debugMExprCompile = true} in

let compileModel = lam methodStr. lam modelAst.
  use MExprCompile in
  let inferMethod = inferMethodFromOptions dummyOptions methodStr in
  match loadCompiler dummyOptions inferMethod with (runtime, compile) in
  let entry = loadRuntimeEntry inferMethod runtime in
  let modelRepr = {ast = modelAst, method = inferMethod, params = []} in
  compileModel (compile (setEmpty cmpString)) entry modelId modelRepr
in

-- Simple tests that ensure compilation of a simple model throws no errors
utest compileModel "mexpr-importance" simple with () using truefn in
utest compileModel "mexpr-apf" simple with () using truefn in
utest compileModel "mexpr-bpf" simple with () using truefn in
utest compileModel "mexpr-mcmc-naive" simple with () using truefn in
utest compileModel "mexpr-mcmc-trace" simple with () using truefn in
utest compileModel "mexpr-mcmc-lightweight" simple with () using truefn in

()
