include "mexpr/duplicate-code-elimination.mc"
include "mexpr/symbolize.mc"
include "mexpr/utils.mc"

include "../parser.mc"
include "../src-location.mc"
include "counter/compile.mc"

lang LoadRuntimeR = DPPLParser + MExprSym + MExprFindSym + MExprEliminateDuplicateCode

  type RuntimeREntry = {
    ast : Expr,
    runId : Name,
    runtimeParam: (String,Type),
    topSymEnv: SymEnv
  }

  type RuntimesR = {
      entries : Map RuntimeMethod RuntimeREntry,
      ast:Expr
  }

  /-sem loadCompilerR : Options -> RuntimeRMethod -> (String, (Expr,Expr) -> Expr)
  sem loadCompilerR options =
  | Counter _ -> compilerCounter options
  | _ -> error "Unsupported runtime method"

  sem loadRuntimesR : Options -> Expr -> Map RuntimeRMethod RuntimeREntry
  sem loadRuntimesR options =
  | ast -> loadRuntimesRH options (mapEmpty cmpRuntimeRMethod) methods

  sem loadRuntimesRH : Options -> Map RuntimeRMethod RuntimeREntry -> RuntimeRMethod
                   -> Map RuntimeRMethod RuntimeREntry
  sem loadRuntimesRH options acc =
  | method ->
    if mapMem method acc then acc
    else
      -- A runtime for the current infer method has not been loaded. Parse and
      -- symbolize the runtime of the corresponding method, and add it to the
      -- accumulated map.
      match loadCompilerR options method with (runtime, _) in
      mapInsert method (loadRuntimeREntry method runtime) acc
  | t -> sfold_Expr_Expr (loadRuntimesRH options) acc t
-/
  sem loadRuntimeREntry : RuntimeMethod -> String -> RuntimeREntry
  sem loadRuntimeREntry method =
  | runtime ->
    let parse = use BootParser in parseMCoreFile {
      defaultBootParserParseMCoreFileArg with
        eliminateDeadCode = false,
        allowFree = true
      }
    in
    let runtime = parse (join [corepplSrcLoc, "/coreppl-to-mexpr/", runtime]) in
    let runtime = symbolizeAllowFree runtime in
    let runtimePid = "counter" in
    match findRequiredRuntimeRIds ["run","Counter"] method runtime with (runId, runtimeParamId) in
    let runtimeType = findRType method runtimeParamId runtime in
    { ast = runtime, runId = runId, runtimeParam = (runtimePid,runtimeType),
     topSymEnv = addTopNames symEnvEmpty runtime }

  -- Finds a pre-defined list of identifiers in the given runtime AST, which
  -- are assumed to be present in all runtimes.
  sem findRequiredRuntimeRIds : [String] -> RuntimeMethod -> Expr -> (Name,Name)
  sem findRequiredRuntimeRIds runtimeStrings method =
  | runtime ->
    let ids = findNamesOfStrings runtimeStrings runtime in
    let ids = map (lam e. match e with Some e then e
    else error "Missing identifier") ids in
    (get ids 0, get ids 1)

  sem findRType : RuntimeMethod -> Name -> Expr -> Type
  sem findRType method runtimeParamId =
  | runtime ->
    match findRTypeH runtimeParamId (None ()) runtime with Some ty then ty
    else error "could not find type definition"

  sem findRTypeH : Name -> Option Type -> Expr -> Option Type
  sem findRTypeH runtimeParamId acc =
  | TmType {ident = id, params = params, tyIdent = tyIdent, inexpr = inexpr} ->
    if nameEq id runtimeParamId then
      -- Create a type application for each type parameter of the type.
      recursive let g = lam ty. lam params.
        match params with [_] ++ params then
          tyapp_ ty (TyUnknown {info = infoTy ty})
        else ty
      in
      let display = g (ntycon_ runtimeParamId) params in

      -- Replace all inner type variables with TyUnknown to let the
      -- type-checker can correctly infer them.
      recursive let unknownTyVars = lam ty.
        match ty with TyVar t then TyUnknown {info = t.info}
        else smap_Type_Type unknownTyVars ty
      in
      let content = unknownTyVars tyIdent in
      Some (TyAlias {display = display, content = content})
    else findRTypeH runtimeParamId acc inexpr
  | t -> sfold_Expr_Expr (findRTypeH runtimeParamId) acc t
end
