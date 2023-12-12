-- CorePPL compiler, targeting the RootPPL framework

include "../coreppl.mc"
include "../parser.mc"
include "../dppl-arg.mc"
include "../cfa.mc"

include "../inference/smc.mc"

include "rootppl.mc"

include "mexpr/ast-builder.mc"
include "mexpr/type-check.mc"
include "mexpr/lamlift.mc"

include "assoc-seq.mc"
include "name.mc"
include "map.mc"

include "c/ast.mc"
include "c/pprint.mc"
include "c/compile.mc"

include "mexpr/pprint.mc"

-----------------------------------------
-- BASE LANGUAGE FRAGMENT FOR COMPILER --
-----------------------------------------

lang MExprPPLRootPPLCompile = MExprPPL + Resample + RootPPL + MExprCCompileAlloc
  + SeqTypeNoStringTypeLift + MExprPPLCFA + MExprLambdaLift + SMCCommon

  -- Compiler internals
  syn CExpr =
  | CEResample {} -- Indicates resample locations

  sem printCExpr (env: PprintEnv) =
  | CEResample {} -> (env, "<<<CEResample>>>")

  sem sfold_CExpr_CExpr f acc =
  | CEResample _ -> acc

  sem smap_CExpr_CExpr (f: CExpr -> CExpr) =
  | CEResample _ & t -> t

  -- Compilation of distributions
  sem compileDist (env: CompileCEnv) =
  | DBernoulli { p = p } -> CDBern { p = compileExpr env p }
  | DBeta { a = a, b = b } ->
    CDBeta { a = compileExpr env a, b = compileExpr env b }
  | DCategorical { p = p } -> CDCategorical { p = compileExpr env p }
  | DBinomial { n = n, p = p } ->
    CDBinomial { n = compileExpr env n, p = compileExpr env p }
  | DMultinomial { n = n, p = p } ->
    CDMultinomial { n = compileExpr env n, p = compileExpr env p }
  | DDirichlet { a = a } -> CDDirichlet { a = compileExpr env a }
  | DExponential { rate = rate } -> CDExp { rate = compileExpr env rate }
  | DEmpirical { samples = samples } ->
    CDEmpirical { samples = compileExpr env samples }
  | DUniform { a = a, b = b } ->
    CDUniform { a = compileExpr env a, b = compileExpr env b }
  | DGaussian { mu = mu, sigma = sigma } ->
    CDNormal { mu = compileExpr env mu, sigma = compileExpr env sigma }
  | DPoisson { lambda = lambda } ->
    CDPoisson { lambda = compileExpr env lambda }
  | DGamma { k = k, theta = theta } ->
    CDGamma { k = compileExpr env k, theta = compileExpr env theta}

  -- Extensions to C expression compilation
  sem compileExpr (env: CompileCEnv) =
  | TmAssume _ -> error "Assume without direct distribution"
  | TmAssume { dist = TmDist { dist = dist }} ->
    CESample { dist = compileDist env dist }
  | TmWeight { weight = weight } -> CEWeight { weight = compileExpr env weight }
  | TmObserve { value = value, dist = TmDist { dist = dist }} ->
    CEObserve { value = compileExpr env value, dist = compileDist env dist }
  | TmResample _ -> CEResample {}

  -- Add error reporting for dist type
  sem compileType (env: CompileCEnv) =
  | TyDist _ & ty -> errorSingle [infoTy ty] "TyDist in compileType"

  -- Do not lift polymorphic types. NOTE(dlunde,2021-10-08): This allows for
  -- some simple cases, like using `get` for lists.
  sem typeLiftType (env : TypeLiftEnv) =
  | TySeq {info = _, ty = TyVar _ | TyUnknown _} & ty -> (env,ty)

end

lang MExprPPLRootPPLCompileANF = MExprPPLRootPPLCompile + MExprANF

  -- ANF override
  -- Ensures distributions are not lifted by ANF (first-class distributions not
  -- supported in RootPPL)
  sem normalize (k : Expr -> Expr) =
  | TmAssume ({ dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeDist
      (lam dist. k (TmAssume { t with dist = TmDist { td with dist = dist } }))
      dist
  | TmObserve ({ value = value, dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeName
      (lam value.
        normalizeDist
          (lam dist.
             k (TmObserve {{ t with value = value }
                               with dist = TmDist { td with dist = dist}}))
          dist)
      value

end

lang MExprPPLRootPPLCompileANFAll = MExprPPLRootPPLCompile + MExprANFAll
end

----------------------
-- ROOTPPL COMPILER --
----------------------

type RPProg = use RootPPL in RPProg

-- Generate code for compiled RootPPL AST
let printCompiledRPProg: RPProg -> String = use MExprPPLRootPPLCompile in
  lam rpprog: RPProg.
    printRPProg cCompilerNames rpprog

-- Debug function during compilation.
let _debugPrint: [CTop] -> [CTop] -> [CStmt] -> [CStmt] -> String =
  use MExprPPLRootPPLCompile in
  lam types: [CTop].
  lam tops: [CTop].
  lam detInits: [CStmt].
  lam stochInits: [CStmt].
    let addName = lam env. lam name.
      match pprintAddStr env name with Some env then env
      else error (join ["Duplicate name in _debugPrint: ", nameGetStr name])
    in
    let env = foldl addName pprintEnvEmpty cCompilerNames in
    match mapAccumL (printCTop 0) env types with (env,types) in
    match mapAccumL (printCTop 0) env tops with (env,tops) in
    match printCStmts 0 env detInits with (env,detInits) in
    match printCStmts 0 env stochInits with (env,stochInits) in

      let types = strJoin (pprintNewline 0) types in
      let tops = strJoin (pprintNewline 0) tops in
      join [
        "--- TYPES ---\n",
        types,"\n\n",
        "--- TOPS ---\n",
        tops,"\n\n",
        "--- DET INITS ---\n",
        detInits,"\n\n",
        "--- STOCH INITS ---\n",
        stochInits,"\n\n"
      ]

-- Compiler names
let nameRetAddr = nameSym "ra"
let nameRetValLoc = nameSym "retValLoc"
let nameInit = nameSym "init"
let nameSF = nameSym "sf"
let nameGlobal = nameSym "global"
let nameCallSF = nameSym "callsf"
let nameStack = nameSym "stack"
let nameStackPtr = nameSym "stackPtr"
let nameRet = nameSym "ret"
let nameGlobalTy = nameSym "GLOBAL"
let nameStartBlock = nameSym "start"

type Expr = use MExprPPLRootPPLCompile in Expr
type Type = use MExprPPLRootPPLCompile in Type

-- Strip unnecessary lets. Leaves lets around TmApps when they are returned from
-- functions rather than bound in a let, to allow for correct stack handling.
let removeRedundantLets: Expr -> Expr = use MExprPPLRootPPLCompile in
  lam expr: Expr.
    recursive let rec: Bool -> Expr -> Expr = lam inLet. lam expr.

      -- Let
      match expr with TmLet ({
        ident = idLet, body = body, inexpr = inexpr
      } & t) then
        let ok = match body with TmApp _ then inLet else true in
        if ok then
          let f = lam. TmLet {{ t with body = rec true body }
                                  with inexpr = rec inLet inexpr } in
          match inexpr with TmVar {ident = idExpr} then
            if nameEq idLet idExpr then (rec inLet) body
            else f ()
          else f ()
        else smap_Expr_Expr (rec inLet) expr

      -- Lambda
      else match expr with TmLam _ then smap_Expr_Expr (rec false) expr

      -- Not a let
      else smap_Expr_Expr (rec inLet) expr

    in rec false expr

-- Find the categories of all identifiers. Assumes there are no first-class
-- functions (proper CFA required in that case). Possible categories:
-- * 0: Deterministic
-- * 1: Stochastic
-- * 2: Resample
let catIdents: Expr -> Map Name Int =
  use MExprPPLRootPPLCompile in
  lam expr: Expr.
    recursive let rec = lam acc: Map Name Int. lam expr: Expr.

      -- Function for checking the category of an expression
      let cat: Expr -> Int = lam expr: Expr.
        recursive let rec: Int -> Expr -> Int =
          lam cat: Int. lam expr: Expr.
            match cat with 2 then 2
            else match expr with TmResample _ then 2
            else match expr with TmAssume _ then maxi cat 1
            else match expr with TmWeight _ then maxi cat 1
            else match expr with TmVar { ident = ident } then
              match mapLookup ident acc with Some ncat then maxi cat ncat
              else cat
            else sfold_Expr_Expr rec cat expr
        in rec 0 expr
      in

      let updateAcc = lam acc. lam ident. lam expr.
        mapInsert ident (cat expr) acc
      in

      -- Iterate through top-level
      match expr with TmLet {
        ident = ident, body = body, inexpr = inexpr
      } then
        rec (updateAcc acc ident body) inexpr

      else match expr with TmRecLets {
        bindings = bindings, inexpr = inexpr
      } then
        -- If recursive functions, make two passes through them
        let f = lam acc.
          foldl (lam acc. lam b: RecLetBinding.
            match b with { ident = ident, body = body } in
            updateAcc acc ident body
          ) acc bindings in
        rec (f (f acc)) inexpr

      -- Ignore the rest
      else match expr with
        TmType { inexpr = inexpr }
        | TmConDef { inexpr = inexpr }
        | TmExt { inexpr = inexpr }
      then rec acc inexpr
      else acc

    in rec (mapEmpty nameCmp) expr

-- RootPPL compile function
let rootPPLCompileH: Options -> [(Name,Type)] -> Map Name Int -> Expr -> RPProg =
  use MExprPPLRootPPLCompile in
  lam options: Options.
  lam typeEnv: [(Name,Type)].
  lam identCats: Map Name Int.
  lam prog: Expr.

    -------------------------
    -- RUN BASE C COMPILER --
    -------------------------

    match compile typeEnv (defaultCompileCOptions ()) prog
    with (env, types, tops, inits, retTy) in

    -- print (_debugPrint types tops inits inits);

    ------------------------
    -- COMMON DEFINITIONS --
    ------------------------

    -- Function indicating if a functions never calls resample (directly or
    -- indirectly)
    let resampleFree: Name -> Bool = lam n.
      match mapLookup n identCats with Some 2 then false else true
    in

    -- Unwraps pointer type one step
    let tyDeref: CType -> CType = lam ty: CType.
      match ty with CTyPtr { ty = ty } then ty
      else error "Non-pointer in tyDeref"
    in

    ------------------------
    -- TRANSLATE TOP DEFS --
    ------------------------

    let isIdentDet: Name -> Bool = lam id: Name.
      match mapLookup id identCats with Some cat then eqi cat 0 else true
    in

    -- Iterate over tops and replace deterministic definitions with BBLOCK_DATA.
    -- Also remove stochastic definitions and add to globals (Name + Type)
    let res = foldl (lam acc: ([CTop],[(Name,CType)]). lam top: CTop.
        match top
        with CTDef { ty = ! CTyFun _ & ty, id = Some id, init = init } then
          match init with Some _ then error "Can't handle initializers." else
          if isIdentDet id then
            let def =
              match ty
              with CTyArray { ty = ty, size = Some (CEInt { i = len }) } then
                CTBBlockData { ty = ty, id = id, len = len }
              else
                CTBBlockDataSingle { ty = ty, id = id }
            in
            (snoc acc.0 def, acc.1)
          else
            match ty with CTyArray _ then
              error "Non-deterministic allocation of pointer-accessed data structure."
            else
              (acc.0, snoc acc.1 (id,ty))
        else (snoc acc.0 top, acc.1)
      ) ([],[]) tops in

    match res with (tops,globals) in

    ------------------
    -- DIVIDE INITS --
    ------------------

    let isExprDet: CExpr -> Bool = lam expr: CExpr.
      recursive let rec: Bool -> CExpr -> Bool = lam acc. lam expr.
        match acc with false then false
        else match expr with CEResample _ | CESample _ | CEWeight _ then false
        else match expr with CEVar { id = id }
                           | CEApp { fun = id }
                           | CEMember { id = id }
                           | CEArrow { id = id } then
          if isIdentDet id then sfold_CExpr_CExpr rec acc expr else false
        else sfold_CExpr_CExpr rec acc expr
      in rec true expr
    in

    let isStmtDet: CStmt -> Bool = lam stmt: CStmt.
      let exprsDet = sfold_CStmt_CExpr
        (lam acc. lam expr. match acc with false then false else isExprDet expr)
        true stmt
      in
      recursive let rec: Bool -> CStmt -> Bool = lam acc. lam stmt.
        match acc with false then false
        else match stmt with CSDef { id = Some id } then
          if isIdentDet id then sfold_CStmt_CStmt rec acc stmt else false
        else sfold_CStmt_CStmt rec acc stmt
      in
      if exprsDet then rec true stmt else false
    in

    let inits = foldl (lam acc: ([CStmt],[CStmt]). lam stmt: CStmt.
        -- Ensure return is always stochastic
        match stmt with CSRet _ then (acc.0, snoc acc.1 stmt)
        else if isStmtDet stmt then (snoc acc.0 stmt, acc.1)
        else (acc.0, snoc acc.1 stmt)
      ) ([],[]) inits
    in

    match inits with (detInits, stochInits) in

    -- print (_debugPrint types tops detInits stochInits);

    ------------------------
    -- ADD BBLOCK_HELPERS --
    ------------------------

    -- Stack and stack pointer expressions
    let stack = CEMember { lhs = CEPState {}, id = nameStack } in
    let stackPtr = CEMember { lhs = CEPState {}, id = nameStackPtr } in

    let globalDefBase = lam pstate: CExpr.
      let ty = CTyStruct { id = Some nameGlobalTy, mem = None () } in
      CSDef {
        ty = CTyPtr { ty = ty },
        id = Some nameGlobal,
        init = Some (CIExpr {
          expr = CECast {
            ty = CTyPtr { ty = ty },
            rhs = pstate
          }
        })
      }
    in

    -- Global frame
    let globalDef = globalDefBase stack in

    let res = foldl (lam acc: ([CTop], Map Name Name). lam top: CTop.
        match top with CTFun ({ id = id } & r) then
          match mapLookup id identCats with Some cat then

            -- Deterministic function, create BBLOCK_HELPER version as well
            match cat with 0 then
              let det = top in
              let n = nameSym (concat "STOCH_" (nameGetStr id)) in
              let detFunMap = mapInsert id n acc.1 in
              let stoch =
                CTBBlockHelper {{r with id = n}
                                   with body = cons globalDef r.body } in
              let tops = concat acc.0 [det, stoch] in
              (tops,detFunMap)

            -- Stochastic function, replace with BBLOCK_HELPER
            else match cat with 1 then
              let bbh = CTBBlockHelper {r with body = cons globalDef r.body} in
              (snoc acc.0 bbh, acc.1)

            -- Function with resample, handled elsewhere
            else (snoc acc.0 top, acc.1)

          else error "Category not found"
        else (snoc acc.0 top, acc.1)
      ) ([], mapEmpty nameCmp) tops in

    match res with (tops, detFunMap) in

    -- print (_debugPrint types tops detInits stochInits);

    ----------------------------
    -- DETERMINE STACK FRAMES --
    ----------------------------

    -- Stack frame type, containing relevant names and types
    type StackFrame = {
      id: Name,
      mem: [(Name,CType)],
      ret: CType,
      params: [Name]
    } in

    -- Initialize a new stack frame
    let newStackFrame: Name -> CType -> StackFrame = lam name. lam ret. {
      id = nameSym (concat "STACK_" (nameGetStr name)),
      mem = [], ret = ret, params = []
    } in

    -- Accumulator used when determining stack frames
    type AccStackFrames = {
      -- Current stack frame
      sf: StackFrame,
      -- Names defined in current block
      defs: [(Name,CType)],
      -- Names defined in previous blocks for a function
      prevDefs: [(Name,CType)],
      -- Indicates whether or not a list of CStmt contains a BBLOCK split
      hasSplit: Bool
    } in

    -- Empty accumulator
    let emptyAccSF: StackFrame -> AccStackFrames = lam sf: StackFrame.
      { sf = sf, defs = [], prevDefs = [], hasSplit = false }
    in

    -- Add all local variables used in an expression that traverse BBLOCK
    -- boundaries to the accumulator
    recursive let addLocals: AccStackFrames -> CExpr -> AccStackFrames =
      lam acc: AccStackFrames. lam expr: CExpr.
        let lookup = assocSeqLookup {eq = nameEq} in
        let sf = acc.sf in
        match expr with CEVar { id = id } then
          match lookup id sf.mem with Some _ then acc
          else match lookup id acc.prevDefs with Some ty then
            {acc with sf = {sf with mem = cons (id,ty) sf.mem}}
          else acc
        else sfold_CExpr_CExpr addLocals acc expr
    in

    -- Mark the end of a BBLOCK in the accumulator
    let nextBBlock: AccStackFrames -> AccStackFrames = lam acc: AccStackFrames.
      {{{ acc with defs = [] }
              with prevDefs = concat acc.defs acc.prevDefs }
              with hasSplit = true }
    in

    -- Given an initial accumulator (see above), find the locals used in a
    -- function (= list of statements)
    recursive let findLocals: AccStackFrames -> [CStmt] -> AccStackFrames =
      lam acc: AccStackFrames. lam stmts: [CStmt].
        let acc = {acc with hasSplit = false} in
        foldl (lam acc: AccStackFrames. lam stmt: CStmt.

          -- Add def, if applicable
          let acc =
            match stmt with CSDef { ty = ty, id = Some name }
            then { acc with defs = cons (name,ty) acc.defs } else acc
          in

          -- Add used locals from prevDef
          let acc: AccStackFrames = sfold_CStmt_CExpr addLocals acc stmt in

          -- Block split on applications
          match stmt with CSDef { init = Some (CIExpr { expr = CEApp app }) }
                        | CSExpr { expr = CEApp app }
                        | CSExpr { expr = CEBinOp {
                            op = COAssign {}, rhs = CEApp app }
                          } then
            if resampleFree app.fun then acc
            else nextBBlock acc

          -- Block split on resample
          else match stmt with CSExpr { expr = CEResample _ } then
            nextBBlock acc

          -- If statement, requires special handling
          else match stmt with CSIf { thn = thn, els = els } then
            let accThn = findLocals acc thn in
            let accEls = findLocals {acc with sf = accThn.sf} els in
            let acc = {acc with sf = accEls.sf} in
            if or (accThn.hasSplit) (accEls.hasSplit) then nextBBlock acc
            else acc

          -- Rest
          else match stmt with CSDef _ | CSExpr _ | CSRet _ | CSNop _ then acc

          else error "Unsupported term in findLocals"

        ) acc stmts
    in

    -- Recurse over all top-level definitions and determine stack frames for
    -- functions with resamples
    let sfs: [(Name,StackFrame)] =
        foldl (lam acc: [(Name,StackFrame)]. lam top: CTop.
          match top
          with CTFun { ret = ret, id = id, params = params, body = body } then

            -- Resample-free functions are ignored here
            if resampleFree id then acc else

            -- Initialize accumulator for new function
            let sf = {{ newStackFrame id ret with
              mem = map (lam t: (CType,Name). (t.1,t.0)) params} with
              params = map (lam t: (CType,Name). t.1) params
            } in
            let lAcc = emptyAccSF sf in

            -- Get locals that traverse BBLOCK boundaries in function
            let lAcc = findLocals lAcc body in

            -- Record stack frame
            snoc acc (id,lAcc.sf)

          else acc) [] tops
    in

    -- Set up stack frame for initialization code
    let stochInitSF: StackFrame =
      let sf = newStackFrame nameInit retTy in
      let acc = findLocals (emptyAccSF sf) stochInits in
      acc.sf
    in

    -- Stackframe lookup
    let getStackFrame: Name -> [(Name,StackFrame)] -> StackFrame =
      lam id: Name. lam sfs: [(Name,StackFrame)].
        match assocSeqLookup {eq=nameEq} id sfs with Some sf then sf
        else error "Stack frame does not exist in getStackFrame"
    in

    -- Helper function for getting the stack frame of a top.
    let topStackFrame: CTop -> StackFrame =
      lam top.
        let id = match top with CTFun { id = id } then id
                 else error "Invalid top in topStackFrame" in
        getStackFrame id sfs
    in


    --------------------------------------
    -- TRANSLATE DEFINITIONS IN BBLOCKS --
    --------------------------------------

    let replaceDefs: [(Name,CType)] -> CStmt -> [CStmt] =
      lam locals: [(Name,CType)].
      lam stmt: CStmt.

        recursive let rec = lam stmt: CStmt.

          match stmt with CSDef { ty = ty, id = Some id, init = init } then
            let l = any (lam l: (Name,CType). nameEq id l.0) locals in

            -- Local definition
            if l then
              match ty with CTyArray _ then
                error "Non-deterministic allocation of pointer-accessed data structure."
              else match init with Some init then
                -- Replace with assignment
                match init with CIExpr { expr = expr } then
                  [CSExpr { expr = CEBinOp {
                     op = COAssign {},
                     lhs = CEVar { id = id },
                     rhs = expr
                   }}]
                else error "Non-CIExpr initializer in replaceDefs"

              -- Remove definition if it does not initialize anything
              else []

            -- Leave other definitions as they are
            else [stmt]

          -- Recurse
          else [sreplace_CStmt_CStmt rec stmt]

        in rec stmt
    in

    let tops: [CTop] = map (lam top.
      match top with CTFun { id = id } then
        if resampleFree id then top else
        let locals = (topStackFrame top).mem in
        sreplace_CTop_CStmt (replaceDefs locals) top
      else top
    ) tops in
    let stochInits: [CStmt] =
      join (map (replaceDefs stochInitSF.mem) stochInits) in

    -- print (_debugPrint types tops detInits stochInits);

    -----------------------------------
    -- TRANSLATE LOCAL VARIABLE USES --
    -----------------------------------

    let replaceVar: [(Name,CType)] -> CExpr -> CExpr =
      lam locals: [(Name,CType)].
      lam expr: CExpr.
        recursive let rec = lam expr: CExpr.
          match expr with CEVar { id = id } then
            match assocSeqLookup {eq=nameEq} id locals with Some cty then
              CEArrow { lhs = CEVar { id = nameSF }, id = id }
            else expr

          else smap_CExpr_CExpr rec expr
        in rec expr
    in

    let tops: [CTop] = map (lam top.
      match top with CTFun { id = id } then
        if resampleFree id then top else
        let locals = (topStackFrame top).mem in
        smap_CTop_CExpr (replaceVar locals) top
      else top
    ) tops in
    let stochInits: [CStmt] =
      map (smap_CStmt_CExpr (replaceVar stochInitSF.mem)) stochInits in


    ----------------------------------
    -- SPLIT FUNCTIONS INTO BBLOCKS --
    ----------------------------------

    -- Type used for indicating what action to take at endpoints.
    -- Collapse => Collapse stack
    -- Block => Jump to block. Block name can be either predetermined (Some _)
    -- or generated at the endpoint (None _)
    type Next in
    con Collapse: () -> Next in
    con Block: Option Name -> Next in

    -- Accumulator used when splitting function into BBLOCKs
    type AccSplit = {
      -- Indicates what to do at an endpoint
      next: Next,
      -- Indicates if a split has occurred
      hasSplit: Bool,
      -- Accumulated block
      block: [CStmt],
      -- Accumulated set of top-level definitions
      tops: [CTop]
    } in

    -- Empty accumulator
    let emptyAccSplit =
      { next = Collapse (), hasSplit = false, block = [], tops = [] }
    in

    -- C statement for setting the NEXT from an expression
    let setNextFromExpr: CExpr -> CStmt = lam expr.
      (CSExpr { expr = (CEBinOp { op = COAssign {}, lhs = CENext {}
                                , rhs = expr })})
    in

    -- C statement for setting the NEXT to a BBLOCK name
    let setNext: Name -> CStmt = lam name.
      setNextFromExpr (CEVar { id = name })
    in

    -- Used when reaching endpoints, ensures the accumulator has a name for the
    -- next block.
    let initNextName: AccSplit -> AccSplit = lam acc: AccSplit.
      match acc.next with Block (Some name) then acc
      else match acc.next with Block (None ()) then
        {acc with next = Block (Some (nameSym "bblock"))}
      else error "Error in initNextName"
    in

    -- Get the name for the next block from accumulator
    let getNextName: AccSplit -> Name = lam acc: AccSplit.
      match acc.next with Block (Some name) then name
      else error "Error in getNextName"
    in

    -- BBLOCK_CALL(name, NULL)
    let jumpBBlock = lam name: Name.
      CSExpr { expr = CEApp {
        fun = nameBblockJump, args = [CEVar {id = name}, CEVar {id = nameNull}]
      } }
    in

    -- BBLOCK_CALL(DATA_POINTER(bblocksArr)[expr], NULL)
    let callFromExpr = lam expr: CExpr.
        CSExpr { expr = CEApp {
          fun = nameBblockJump, args = [
            expr, CEVar { id = nameNull }
          ] }
        }
    in

    -- Deref shorthand
    let derefExpr: CExpr -> CExpr = lam expr.
      CEUnOp { op = CODeref {}, arg = expr}
    in

    -- Converts an expression representing an absolute address to a relative
    -- address.
    let toRelAddr: CExpr -> CType -> CExpr =
      lam expr: CExpr. lam ty: CType.
        CECast {
          ty = ty,
          rhs = CEBinOp {
            op = COSub {},
            lhs = CECast {
              ty = CTyPtr { ty = CTyChar {} },
              rhs = expr
            },
            rhs = stack
          }
        }
    in

    -- Converts an expression representing a relative address (with given type)
    -- to an absolute address.
    let toAbsAddr: CExpr -> CType -> CExpr = lam expr. lam ty.
      CECast {
        ty = ty,
        rhs = CEBinOp {
          op = COAdd {},
          lhs = stack,
          rhs = CECast {
            ty = CTyVar { id = nameUIntPtr },
            rhs = expr
          }
        }
      }
    in

    -- Construct a BBLOCK from the currently accumulated statements
    let createBlock: StackFrame -> Name -> AccSplit -> AccSplit =
      lam sf: StackFrame. lam name: Name. lam acc: AccSplit.

        let ty = CTyStruct { id = Some sf.id, mem = None ()} in

        let sf = CSDef {
          ty = CTyPtr { ty = ty },
          id = Some nameSF,
          init = Some (CIExpr {
              expr = toAbsAddr (
                CEBinOp {
                  op = COSub {},
                  lhs = stackPtr,
                  rhs = CESizeOfType { ty = ty }
                }
              ) (CTyPtr { ty = ty })
            }
          )
        } in

        let bb = CTBBlock { id = name, body = concat [globalDef, sf] acc.block }
        in {acc with tops = snoc acc.tops bb}
    in

    -- Generate code for shifting the stack pointer by a stack frame
    let modStackPtr: CBinOp -> StackFrame -> CStmt =
      lam op: CBinOp. lam sf: StackFrame.

        let ty = CTyStruct { id = Some sf.id, mem = None ()} in

        CSExpr { expr = CEBinOp {
          op = COAssign {},
          lhs = stackPtr,
          rhs = CEBinOp {
            op = op,
            lhs = stackPtr,
            rhs = CESizeOfType { ty = ty }
          }
        }}
    in

    let decrementStackPtr = modStackPtr (COSub {}) in
    let incrementStackPtr = modStackPtr (COAdd {}) in

    -- Generate code for calling a BBLOCK
    let constructCall:
        StackFrame -> Name -> [CExpr] -> Option CExpr -> CExpr -> [CStmt] =
      lam sf: StackFrame.
      lam fun: Name.
      lam args: [CExpr].
      lam ret: Option CExpr.
      lam ra: CExpr.

        let ty = CTyStruct { id = Some sf.id, mem = None ()} in

        let callsf = CSDef {
          ty = CTyPtr { ty = ty },
          id = Some nameCallSF,
          init = Some (CIExpr { expr = toAbsAddr stackPtr (CTyPtr { ty = ty }) })
        } in

        let f: Name -> CExpr -> CStmt = lam n. lam expr.
          CSExpr { expr = CEBinOp {
            op = COAssign {},
            lhs = CEArrow {lhs = CEVar {id = nameCallSF}, id = n},
            rhs = expr
          }}
        in

        let ra = f nameRetAddr ra in
        let args = zipWith f sf.params args in
        let ret =
          match ret with Some expr then
            [f nameRetValLoc
               (toRelAddr (CEUnOp { op = COAddrOf {}, arg = expr })
                  (CTyPtr { ty = sf.ret }))]
          else []
        in

        let incr = incrementStackPtr sf in

        let call = jumpBBlock fun in

        join [[callsf, ra], args, ret, [incr, call]]
    in

    let constructCallFromStmt: StackFrame -> CStmt -> CExpr -> [CStmt] =
      lam sf: StackFrame.
      lam stmt: CStmt.
      lam ra: CExpr.
        let app: { fun: Name, args: [CExpr] } =
          match stmt with CSDef { init = Some (CIExpr { expr = CEApp app }) }
             | CSExpr { expr = CEBinOp { op = COAssign {}, rhs = CEApp app } }
             | CSExpr { expr = CEApp app }
          then app else error "Non-application in constructCallFromStmt"
        in
        let ret: Option CExpr =
          match stmt with
            CSExpr { expr = CEBinOp {
              op = COAssign {}, lhs = expr, rhs = CEApp _ }
            } then
            match sf.ret with ! CTyVoid _ then Some expr
            else error "Error in constructCallFromStmt"
          else None ()
        in
        constructCall sf app.fun app.args ret ra
    in

    -- Split a function (= list of statements) into BBLOCKs
    recursive let splitStmts: AccSplit -> StackFrame -> [CStmt] -> AccSplit =
      lam acc: AccSplit. lam sf: StackFrame. lam stmts: [CStmt].

        match stmts with [stmt] ++ stmts then

          -- Function application
          match stmt
          with CSDef { init = Some (CIExpr { expr = CEApp app }) }
             | CSExpr { expr = CEBinOp { op = COAssign {}, rhs = CEApp app } }
             | CSExpr { expr = CEApp app }
          then
            if resampleFree app.fun then
              splitStmts {acc with block = snoc acc.block stmt} sf stmts
            else
              let acc = {acc with hasSplit = true} in
              let funStackFrame = getStackFrame app.fun sfs in
              match (stmts, acc.next) with ([], Block _) then
                -- Special handling of this call avoids an empty BBLOCK
                let acc = initNextName acc in
                let ra = getNextName acc in
                let call = constructCallFromStmt funStackFrame stmt
                             (CEVar { id = ra }) in
                let block = concat acc.block call in
                {acc with block = block}
              else
                -- Not last statement, but end of block
                let accNext = {acc with block = []} in
                let accNext = splitStmts accNext sf stmts in
                let ra = nameSym "bblock" in
                let accNext = createBlock sf ra accNext in

                let call = constructCallFromStmt funStackFrame stmt
                             (CEVar { id = ra }) in
                let block = concat acc.block call in
                {accNext with block = block}

          -- Resample
          else match stmt with CSExpr { expr = CEResample _ } then
            let acc = {acc with hasSplit = true} in
            match stmts with [] then
              -- Resample as last statment _and_ end of block
              match acc.next with Collapse _ then
                let collapse = decrementStackPtr sf in
                let block = snoc acc.block collapse in
                {acc with block =
                   snoc block (setNextFromExpr
                                 (CEArrow {
                                   lhs = CEVar { id = nameSF }, id = nameRetAddr
                                  })
                              ) }
              else match acc.next with Block _ then
                -- Set NEXT to next block
                let acc = initNextName acc in
                let name = getNextName acc in
                {acc with block = snoc acc.block (setNext name)}
              else never
            else
              -- CASE: Not last statement, but end of block
              let accNext = {acc with block = []} in
              let accNext = splitStmts accNext sf stmts in
              let name = nameSym "bblock" in
              let accNext = createBlock sf name accNext in
              -- Simply set NEXT to next block
              {accNext with block = snoc acc.block (setNext name)}

          -- Write to return value
          else match stmt with CSRet { val = Some val } then
            let stmt = CSExpr { expr = CEBinOp {
              op = COAssign {},
              lhs =
                let expr =
                  CEArrow { lhs = CEVar { id = nameSF }, id = nameRetValLoc } in
                derefExpr (toAbsAddr expr (CTyPtr { ty = sf.ret })),
              rhs = val
            }}
            in splitStmts {acc with block = snoc acc.block stmt} sf stmts

          -- Not a function application, resample, or return. Just accumulate
          -- and continue
          else match stmt with CSDef _ | CSExpr _ | CSNop _ then
            splitStmts {acc with block = snoc acc.block stmt} sf stmts

          -- If statement
          else match stmt with CSIf { cond = cond, thn = thn, els = els } then
            let next = match stmts with [] then acc.next else Block (None ()) in
            let accThn = splitStmts {{{acc with block = []}
                                           with hasSplit = false}
                                           with next = next} sf thn in
            let accEls = splitStmts {{accThn with block = []}
                                             with hasSplit = false} sf els in

            -- No split in branches, just continue
            if and (not accThn.hasSplit) (not accEls.hasSplit) then
              let stmt =
                CSIf { cond = cond, thn = accThn.block, els = accEls.block }
              in
              match stmts with [] then
                {acc with block = snoc acc.block stmt}
              else
                splitStmts {acc with block = snoc acc.block stmt} sf stmts

            -- At least one split in branches
            else
              let update = lam acc: AccSplit.
                match accEls.next with Block (Some name) then
                  if acc.hasSplit then acc
                  else {acc with block = snoc acc.block (jumpBBlock name)}
                else acc
              in
              let accThn = update accThn in
              let accEls = update accEls in
              let stmt =
                CSIf { cond = cond, thn = accThn.block, els = accEls.block } in
              let block = snoc acc.block stmt in

              -- Create new block for stmts if needed
              match stmts with [] then
                {{accEls with block = block} with hasSplit = true}
              else
                let accStmts = {{accEls with block = []}
                                        with next = acc.next} in
                let accStmts = splitStmts accStmts sf stmts in
                let name = getNextName accEls in
                let accStmts = createBlock sf name accStmts in
                {{accStmts with block = block}
                           with hasSplit = true}

          else error "Not supported in splitStmts"

        -- End of block without split
        else match stmts with [] then

          match acc.next with Collapse _ then
            let collapse = decrementStackPtr sf in
            let block = snoc acc.block collapse in
            {acc with block =
               snoc block
                 (callFromExpr (CEArrow {
                                 lhs = CEVar { id = nameSF }, id = nameRetAddr
                               })) }

          else match acc.next with Block _ then
            if acc.hasSplit then
              let acc = initNextName acc in
              let name = getNextName acc in
              {acc with block = snoc acc.block (jumpBBlock name)}
            else acc
          else never

        else never
    in

    -- Split a function
    let splitFunction: AccSplit -> CTop -> AccSplit =
      lam acc: AccSplit. lam top: CTop.
        match top with CTFun { id = id, body = body } then
          let sf = (getStackFrame id sfs) in
          let acc = {{ acc with next = Collapse ()}
                           with block = []} in
          let acc = splitStmts acc sf body in
          createBlock sf id acc
        else error "Non-CTFun in splitFunction"
    in

    -- Iterate over top-level definitions and split functions into BBLOCKs
    let splitFunctions: AccSplit -> [CTop] -> AccSplit =
      lam acc: AccSplit. lam tops: [CTop].
        foldl (lam acc: AccSplit. lam top: CTop.

          -- Split functions
          match top with CTFun { id = id } then
            if resampleFree id then
              {acc with tops = snoc acc.tops top}
            else splitFunction acc top

          -- Leave everything else intact
          else {acc with tops = snoc acc.tops top}

        ) acc tops
    in

    -- Split the init function
    let splitInit: AccSplit -> StackFrame -> [CStmt] -> [CTop] =
      lam acc: AccSplit. lam sf: StackFrame. lam stmts: [CStmt].
        let acc = {{ acc with next = Collapse ()}
                         with block = []} in
        let acc = splitStmts acc sf stmts in
        match createBlock sf nameInit acc with { tops = tops } in tops
    in

    -- Split functions into BBLOCKs
    let accSplit = emptyAccSplit in
    let tops = match splitFunctions accSplit tops with { tops = tops }
               in tops in
    let tops = concat tops (splitInit accSplit stochInitSF stochInits) in

    -- print (_debugPrint types tops detInits stochInits);

    ------------------------------------
    -- TRANSLATE GLOBAL VARIABLE REFS --
    ------------------------------------

    let replaceGlobalVar: Bool -> CExpr -> CExpr =
      lam stoch: Bool.
      lam expr: CExpr.
        recursive let rec = lam expr: CExpr.
          match expr with CEVar { id = id } then

            -- Stochastic global
            match assocSeqLookup {eq=nameEq} id globals with Some cty then
              CEArrow { lhs = CEVar { id = nameGlobal }, id = id }

            -- Leave other variables
            else expr

          else smap_CExpr_CExpr rec expr
        in rec expr
    in

    let tops: [CTop] = map (lam top.
      match top with CTFun _ then
        smap_CTop_CExpr (replaceGlobalVar false) top
      else match top with CTBBlock _ | CTBBlockHelper _ then
        smap_CTop_CExpr (replaceGlobalVar true) top
      else top
    ) tops in
    let detInits = map (smap_CStmt_CExpr (replaceGlobalVar false)) detInits in


    -----------------------------------------------
    -- TRANSLATE TO BBLOCK_HELPER FUNCTION CALLS --
    -----------------------------------------------

    -- * Replace BBLOCK_HELPER calls with BBLOCK_CALL + switch to stoch version if in block is a stoch function.
    let replaceCall: CExpr -> CExpr =
      lam expr: CExpr.
        recursive let rec = lam expr: CExpr.
          let expr =
            match expr with CEApp { fun = fun, args = args } then
              match mapLookup fun detFunMap with Some fun then
                CEApp {
                  fun = nameBblockCall, args = concat [CEVar { id = fun }] args
                }
              else match mapLookup fun identCats with Some (! 0) then
                CEApp {
                  fun = nameBblockCall, args = concat [CEVar { id = fun }] args
                }
              else expr
            else expr
          in
          smap_CExpr_CExpr rec expr
        in rec expr
    in


    let tops: [CTop] = map (lam top.
      match top with CTBBlock _ | CTBBlockHelper _ then
        smap_CTop_CExpr replaceCall top
      else top
    ) tops in

    ------------------------------------------------------------
    -- COLLECT/ADD FUNCTION/BBLOCK/BBLOCK_HELPER DECLARATIONS --
    ------------------------------------------------------------

    let res = foldl (lam acc: ([CTop],[CTop]). lam top.

        -- Remove all preexisting declarations
        match top with CTDef { ty = CTyFun _ }
                     | CTBBlockDecl _
                     | CTBBlockHelperDecl _ then
          acc

        else match top with CTBBlock { id = id } then
          (snoc acc.0 (CTBBlockDecl { id = id }), snoc acc.1 top)

        else match top
        with CTBBlockHelper { ret = ret, id = id, params = params } then
          let params = map (lam p: (CType, Name). p.0) params in
          ( snoc acc.0
              (CTBBlockHelperDecl { ret = ret, id = id, params = params }),
            snoc acc.1 top )

        else match top
        with CTFun { ret = ret, id = id, params = params } then
          let params = map (lam p: (CType, Name). p.0) params in
          let funTy = CTyFun { ret = ret, params = params } in
          let decl = CTDef { ty = funTy, id = Some id, init = None () } in
          (snoc acc.0 decl, snoc acc.1 top)

        else (acc.0, snoc acc.1 top)

      ) ([],[]) tops
    in

    match res with (funDecls, tops) in

    -----------------------------
    -- PUT EVERYTHING TOGETHER --
    -----------------------------

    -- Initialize PSTATE.stack = sizeof(GLOBAL)
    let initStackPtr = CSExpr { expr = CEBinOp {
      op = COAssign {},
      lhs = stackPtr,
      rhs = CESizeOfType {
        ty = CTyStruct { id = Some nameGlobalTy, mem = None () }
      }
    }} in

    -- Define start and end BBLOCKs
    let initRet =
      match retTy with ! CTyVoid _ then
        Some (CEArrow { lhs = CEVar { id = nameGlobal }, id = nameRet })
      else None () in
    let nameEndBlock = nameSym "end" in
    let initCall =
      constructCall stochInitSF nameInit
        [] initRet (CEVar { id = nameEndBlock }) in
    let startBBlock = CTBBlock {
      id = nameStartBlock,
      body = join [
        [globalDef, initStackPtr],
        initCall
      ]
    } in
    let endBBlock = CTBBlock {
      id = nameEndBlock,
      body = [setNextFromExpr (CEVar { id = nameNull })]
    } in
    let startBBlockDecl = CTBBlockDecl { id = nameStartBlock } in
    let endBBlockDecl = CTBBlockDecl { id = nameEndBlock } in

    -- Convert a [(Name,CType)] to [(CType, Option Name)].
    let convertMem = map (lam t: (Name,CType). (t.1, Some t.0)) in

    -- Global frame type definition
    let gf = CTDef {
      ty = CTyStruct {
        id = Some nameGlobalTy,
        mem = Some (join [
          match retTy with ! CTyVoid _ then
            [(retTy, Some nameRet)]
          else [],
          convertMem globals
        ])
      }, id = None (), init = None ()
    } in

    -- Convert StackFrame to C struct type definition
    let stackFrameToTopTy: StackFrame -> CTop = lam sf.
      let mem = convertMem sf.mem in
      CTDef {
        ty = CTyStruct {
          id = Some sf.id,
          mem = Some (join [
            [(CTyVar { id = namePplFuncTy }, Some nameRetAddr)],
            match sf.ret with ! CTyVoid _ then
              [(CTyPtr { ty = sf.ret }, Some nameRetValLoc)]
            else [],
            mem
          ])
        },
        id = None (),
        init = None ()
      }
    in

    -- Stack frame types
    let stochInitSF = stackFrameToTopTy stochInitSF in
    let sfs = map (lam t: (Name,StackFrame). stackFrameToTopTy t.1) sfs in

    -- Callback handling for supported types
    let callback =
      if options.printSamples then
        let i = nameSym "i" in
        let specifier =
          switch retTy
          case CTyInt _ | CTyInt32 _ | CTyInt64 _ | CTyChar _ then "%d"
          case CTyFloat _ | CTyDouble _ then "%f"
          case _ then error "Printing of values of the returned type is currently not supported. Please use the --no-print-samples option."
          end
        in Some [
          -- int i = 0;
          CSDef { ty = CTyInt {}, id = Some i, init = Some (CIExpr { expr = CEInt { i = 0 }}) },
          -- while (i < N) {
          CSWhile { cond = CEBinOp { op = COLt {}, lhs = CEVar { id = i }, rhs = CEVar { id = nameCallbackParticles }}, body = [

            -- struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATES[i].stack));
            let pstatesExpr = CEBinOp { op = COSubScript {}, lhs = CEVar { id = namePStates }, rhs = CEVar { id = i } }
            in globalDefBase (CEMember { lhs = pstatesExpr, id = nameStack }),

            -- printf("%<specifier> %f\n", global.ret, WEIGHTS[i])
            CSExpr { expr = CEApp { fun = _printf, args = [
              CEString { s = join [specifier, " %f\n"] },
              CEArrow { lhs = CEVar { id = nameGlobal}, id = nameRet },
              CEBinOp { op = COSubScript {},
                lhs = CEVar { id = nameWeights },
                rhs = CEVar { id = i }
              }
            ] } },

            -- i++;
            CSExpr { expr = CEBinOp { op = COAssign {},
              lhs = CEVar{ id = i },
              rhs = CEBinOp {
                op = COAdd {},
                lhs = CEVar { id = i },
                rhs = CEInt { i = 1 }
              }
            } }
          ] }
        ]
      else
        None ()
    in

    RPProg {
      includes = join [
        cIncludes,
        [ "\"inference/smc/smc.cuh\""
        , "<stdint.h>"
        , "<stdio.h>"
        ]
      ],
      startBlock = nameStartBlock,
      -- pStateTy = Some (CTyStruct { id = None (), mem = Some progStateMem }),
      pStateTy = None (),
      callback = callback,
      types = types,
      tops = join [
        [gf, stochInitSF],
        sfs,
        [startBBlockDecl, endBBlockDecl],
        funDecls,
        [startBBlock, endBBlock],
        tops
      ],
      pre = detInits
    }


-- Entry point for compiler
let rootPPLCompile: Options -> Expr -> RPProg =
  lam options. lam prog.

  use MExprPPLRootPPLCompileANF in

  -- print (mexprPPLToString prog); print "\n\n";
  -- dprint prog; print "\n\n";

  -- Symbolize with empty environment
  let prog: Expr = symbolizeExpr symEnvEmpty prog in

  -- Type check (and annotate)
  let prog: Expr = typeCheck prog in

  -- print (mexprPPLToString prog); print "\n\n";

  -- Lift lambdas
  let prog: Expr = liftLambdas prog in

  -- print (mexprPPLToString prog); print "\n\n";

  -- ANF transformation
  let prog: Expr = normalizeTerm prog in

  -- Type check (again, due to ANF externals...)
  let prog: Expr = typeCheck prog in

  -- Resample at aligned weight if enabled
  let prog =
    -- Add resample after weights (given a predicate)
    match      options.resample with "likelihood" then addResample (lam. true) prog
    else match options.resample with "manual" then prog
    else match options.resample with "align"  then

      -- Do a _full_ ANF transformation (required by CFA and alignment)
      use MExprPPLRootPPLCompileANFAll in
      let progANFAll: Expr = normalizeTerm prog in

      -- Do static analysis for stochastic value flow and alignment
      let unaligned: Set Name = extractUnaligned (alignCfa progANFAll) in
      let isAligned: Name -> Bool = lam n. not (setMem n unaligned) in

      addResample isAligned prog

    else error "Invalid resample option"
  in

  -- dprint prog; print "\n\n";
  -- print (mexprPPLToString prog); print "\n\n";

  -- Type lift
  match typeLift prog with (env, prog) in

  -- print (mexprPPLToString prog); print "\n\n";

  -- Remove redundant lets
  let prog: Expr = removeRedundantLets prog in

  -- print (mexprPPLToString prog); print "\n\n";

  -- Find categories for identifiers
  let ci: Map Name Int = catIdents prog in

  -- Run RootPPL compiler
  let rpprog: RPProg = rootPPLCompileH options env ci prog in

  -- print (printCompiledRPProg rpprog); print "\n\n";

  rpprog

-------------
--- TESTS ---
-------------
-- NOTE(dlunde,2021-10-26): These tests are not that good. Optimally, we
-- should decompose the compiler components (now just one big function), and
-- then run unit tests in some way on each decomposed part in isolation. For
-- now, they are at least better than nothing

lang Test = MExprPPLRootPPLCompileANF
end

mexpr
use Test in

let test = lam options. lam cpplstr.
  let cppl = parseMExprPPLString cpplstr in
  let rppl = rootPPLCompile options cppl in
  printCompiledRPProg rppl
in

let simple = "
let x = assume (Beta 1.0 1.0) in
observe true (Bernoulli x);
x
------------------------" in

utest test default simple with strJoin "\n" [
  "#include <stdint.h>",
  "#include <stdio.h>",
  "#include <math.h>",
  "#include \"inference/smc/smc.cuh\"",
  "#include <stdint.h>",
  "#include <stdio.h>",
  "INIT_MODEL_STACK()",
  "struct GLOBAL {double ret; double x;};",
  "struct STACK_init {pplFunc_t ra; double (*retValLoc);};",
  "BBLOCK_DECLARE(start);",
  "BBLOCK_DECLARE(end);",
  "BBLOCK_DECLARE(init);",
  "BBLOCK(start, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  ((PSTATE.stackPtr) = (sizeof(struct GLOBAL)));",
  "  struct STACK_init (*callsf) = (( struct STACK_init (*) ) ((PSTATE.stack) + (( uintptr_t ) (PSTATE.stackPtr))));",
  "  ((callsf->ra) = end);",
  "  ((callsf->retValLoc) = (( double (*) ) ((( char (*) ) (&(global->ret))) - (PSTATE.stack))));",
  "  ((PSTATE.stackPtr) = ((PSTATE.stackPtr) + (sizeof(struct STACK_init))));",
  "  BBLOCK_JUMP(init, NULL);",
  "})",
  "BBLOCK(end, {",
  "  (NEXT = NULL);",
  "})",
  "BBLOCK(init, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  struct STACK_init (*sf) = (( struct STACK_init (*) ) ((PSTATE.stack) + (( uintptr_t ) ((PSTATE.stackPtr) - (sizeof(struct STACK_init))))));",
  "  ((global->x) = (SAMPLE(beta, 1., 1.)));",
  "  (OBSERVE(bernoulli, (global->x), 1));",
  "  ((*(( double (*) ) ((PSTATE.stack) + (( uintptr_t ) (sf->retValLoc))))) = (global->x));",
  "  ((PSTATE.stackPtr) = ((PSTATE.stackPtr) - (sizeof(struct STACK_init))));",
  "  BBLOCK_JUMP((sf->ra), NULL);",
  "})",
  "CALLBACK(callback, {",
  "  int i = 0;",
  "  while ((i < N)) {",
  "    struct GLOBAL (*global) = (( struct GLOBAL (*) ) ((PSTATES[i]).stack));",
  "    printf(\"%f %f\\n\", (global->ret), (WEIGHTS[i]));",
  "    (i = (i + 1));",
  "  }",
  "})",
  "MAIN({",
  "  FIRST_BBLOCK(start);",
  "  SMC(callback);",
  "})"
] using eqString in

-- Check that different alignment approaches do not crash
utest test { default with resample = "likelihood" } simple with () using lam. lam. true in
utest test { default with resample = "align" } simple with () using lam. lam. true in

let nestedIfs = "
recursive let f: Float -> Float =
 lam p.
  let s1 = assume (Gamma p p) in
  resample;
  let s2 =
    if geqf s1 1. then 2.
    else 3. in
  let s3 =
    if leqf s2 4. then
      let s4 =
        if eqf s2 5. then 6.
        else f 7. in
      addf s4 s4
    else 8. in
  mulf s3 s3
in

f 1.0; 1.0
----------------------" in

utest test { default with printSamples = false } nestedIfs with strJoin "\n" [
  "#include <stdint.h>",
  "#include <stdio.h>",
  "#include <math.h>",
  "#include \"inference/smc/smc.cuh\"",
  "#include <stdint.h>",
  "#include <stdio.h>",
  "INIT_MODEL_STACK()",
  "struct GLOBAL {double ret; double _;};",
  "struct STACK_init {pplFunc_t ra; double (*retValLoc);};",
  "struct STACK_f {pplFunc_t ra; double (*retValLoc); double s4; double s3; double s1; double p;};",
  "BBLOCK_DECLARE(start);",
  "BBLOCK_DECLARE(end);",
  "BBLOCK_DECLARE(bblock);",
  "BBLOCK_DECLARE(bblock1);",
  "BBLOCK_DECLARE(bblock2);",
  "BBLOCK_DECLARE(f);",
  "BBLOCK_DECLARE(bblock3);",
  "BBLOCK_DECLARE(init);",
  "BBLOCK(start, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  ((PSTATE.stackPtr) = (sizeof(struct GLOBAL)));",
  "  struct STACK_init (*callsf) = (( struct STACK_init (*) ) ((PSTATE.stack) + (( uintptr_t ) (PSTATE.stackPtr))));",
  "  ((callsf->ra) = end);",
  "  ((callsf->retValLoc) = (( double (*) ) ((( char (*) ) (&(global->ret))) - (PSTATE.stack))));",
  "  ((PSTATE.stackPtr) = ((PSTATE.stackPtr) + (sizeof(struct STACK_init))));",
  "  BBLOCK_JUMP(init, NULL);",
  "})",
  "BBLOCK(end, {",
  "  (NEXT = NULL);",
  "})",
  "BBLOCK(bblock, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  struct STACK_f (*sf) = (( struct STACK_f (*) ) ((PSTATE.stack) + (( uintptr_t ) ((PSTATE.stackPtr) - (sizeof(struct STACK_f))))));",
  "  ((sf->s3) = ((sf->s4) + (sf->s4)));",
  "  BBLOCK_JUMP(bblock1, NULL);",
  "})",
  "BBLOCK(bblock1, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  struct STACK_f (*sf) = (( struct STACK_f (*) ) ((PSTATE.stack) + (( uintptr_t ) ((PSTATE.stackPtr) - (sizeof(struct STACK_f))))));",
  "  double t;",
  "  (t = ((sf->s3) * (sf->s3)));",
  "  ((*(( double (*) ) ((PSTATE.stack) + (( uintptr_t ) (sf->retValLoc))))) = t);",
  "  ((PSTATE.stackPtr) = ((PSTATE.stackPtr) - (sizeof(struct STACK_f))));",
  "  BBLOCK_JUMP((sf->ra), NULL);",
  "})",
  "BBLOCK(bblock2, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  struct STACK_f (*sf) = (( struct STACK_f (*) ) ((PSTATE.stack) + (( uintptr_t ) ((PSTATE.stackPtr) - (sizeof(struct STACK_f))))));",
  "  char t1;",
  "  (t1 = ((sf->s1) >= 1.));",
  "  double s2;",
  "  if ((t1 == 1)) {",
  "    (s2 = 2.);",
  "  } else {",
  "    (s2 = 3.);",
  "  }",
  "  char t2;",
  "  (t2 = (s2 <= 4.));",
  "  if ((t2 == 1)) {",
  "    char t3;",
  "    (t3 = (s2 == 5.));",
  "    if ((t3 == 1)) {",
  "      ((sf->s4) = 6.);",
  "      BBLOCK_JUMP(bblock, NULL);",
  "    } else {",
  "      struct STACK_f (*callsf) = (( struct STACK_f (*) ) ((PSTATE.stack) + (( uintptr_t ) (PSTATE.stackPtr))));",
  "      ((callsf->ra) = bblock);",
  "      ((callsf->p) = 7.);",
  "      ((callsf->retValLoc) = (( double (*) ) ((( char (*) ) (&(sf->s4))) - (PSTATE.stack))));",
  "      ((PSTATE.stackPtr) = ((PSTATE.stackPtr) + (sizeof(struct STACK_f))));",
  "      BBLOCK_JUMP(f, NULL);",
  "    }",
  "  } else {",
  "    ((sf->s3) = 8.);",
  "    BBLOCK_JUMP(bblock1, NULL);",
  "  }",
  "})",
  "BBLOCK(f, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  struct STACK_f (*sf) = (( struct STACK_f (*) ) ((PSTATE.stack) + (( uintptr_t ) ((PSTATE.stackPtr) - (sizeof(struct STACK_f))))));",
  "  ((sf->s1) = (SAMPLE(gamma, (sf->p), (sf->p))));",
  "  (NEXT = bblock2);",
  "})",
  "BBLOCK(bblock3, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  struct STACK_init (*sf) = (( struct STACK_init (*) ) ((PSTATE.stack) + (( uintptr_t ) ((PSTATE.stackPtr) - (sizeof(struct STACK_init))))));",
  "  ((*(( double (*) ) ((PSTATE.stack) + (( uintptr_t ) (sf->retValLoc))))) = 1.);",
  "  ((PSTATE.stackPtr) = ((PSTATE.stackPtr) - (sizeof(struct STACK_init))));",
  "  BBLOCK_JUMP((sf->ra), NULL);",
  "})",
  "BBLOCK(init, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  struct STACK_init (*sf) = (( struct STACK_init (*) ) ((PSTATE.stack) + (( uintptr_t ) ((PSTATE.stackPtr) - (sizeof(struct STACK_init))))));",
  "  struct STACK_f (*callsf) = (( struct STACK_f (*) ) ((PSTATE.stack) + (( uintptr_t ) (PSTATE.stackPtr))));",
  "  ((callsf->ra) = bblock3);",
  "  ((callsf->p) = 1.);",
  "  ((callsf->retValLoc) = (( double (*) ) ((( char (*) ) (&(global->_))) - (PSTATE.stack))));",
  "  ((PSTATE.stackPtr) = ((PSTATE.stackPtr) + (sizeof(struct STACK_f))));",
  "  BBLOCK_JUMP(f, NULL);",
  "})",
  "MAIN({",
  "  FIRST_BBLOCK(start);",
  "  SMC(NULL);",
  "})"
] using eqString in

let lift = "
let f = lam x: Float.
  let val = assume (Beta x x) in
  let inner = lam obs: Bool.
    observe true (Bernoulli val)
  in
  inner true;
  val
in
f 1.0
------------------------" in

utest test default lift with strJoin "\n" [
  "#include <stdint.h>",
  "#include <stdio.h>",
  "#include <math.h>",
  "#include \"inference/smc/smc.cuh\"",
  "#include <stdint.h>",
  "#include <stdio.h>",
  "INIT_MODEL_STACK()",
  "struct GLOBAL {double ret; double t;};",
  "struct STACK_init {pplFunc_t ra; double (*retValLoc);};",
  "BBLOCK_DECLARE(start);",
  "BBLOCK_DECLARE(end);",
  "void inner(double, char);",
  "BBLOCK_HELPER_DECLARE(STOCH_inner, void, double, char);",
  "BBLOCK_HELPER_DECLARE(f, double, double);",
  "BBLOCK_DECLARE(init);",
  "BBLOCK(start, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  ((PSTATE.stackPtr) = (sizeof(struct GLOBAL)));",
  "  struct STACK_init (*callsf) = (( struct STACK_init (*) ) ((PSTATE.stack) + (( uintptr_t ) (PSTATE.stackPtr))));",
  "  ((callsf->ra) = end);",
  "  ((callsf->retValLoc) = (( double (*) ) ((( char (*) ) (&(global->ret))) - (PSTATE.stack))));",
  "  ((PSTATE.stackPtr) = ((PSTATE.stackPtr) + (sizeof(struct STACK_init))));",
  "  BBLOCK_JUMP(init, NULL);",
  "})",
  "BBLOCK(end, {",
  "  (NEXT = NULL);",
  "})",
  "void inner(double val, char obs) {",
  "  (OBSERVE(bernoulli, val, 1));",
  "}",
  "BBLOCK_HELPER(STOCH_inner, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  (OBSERVE(bernoulli, val, 1));",
  "}, void, double val, char obs)",
  "BBLOCK_HELPER(f, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  double val1;",
  "  (val1 = (SAMPLE(beta, x, x)));",
  "  BBLOCK_CALL(STOCH_inner, val1, 1);",
  "  return val1;",
  "}, double, double x)",
  "BBLOCK(init, {",
  "  struct GLOBAL (*global) = (( struct GLOBAL (*) ) (PSTATE.stack));",
  "  struct STACK_init (*sf) = (( struct STACK_init (*) ) ((PSTATE.stack) + (( uintptr_t ) ((PSTATE.stackPtr) - (sizeof(struct STACK_init))))));",
  "  ((global->t) = BBLOCK_CALL(f, 1.));",
  "  ((*(( double (*) ) ((PSTATE.stack) + (( uintptr_t ) (sf->retValLoc))))) = (global->t));",
  "  ((PSTATE.stackPtr) = ((PSTATE.stackPtr) - (sizeof(struct STACK_init))));",
  "  BBLOCK_JUMP((sf->ra), NULL);",
  "})",
  "CALLBACK(callback, {",
  "  int i = 0;",
  "  while ((i < N)) {",
  "    struct GLOBAL (*global) = (( struct GLOBAL (*) ) ((PSTATES[i]).stack));",
  "    printf(\"%f %f\\n\", (global->ret), (WEIGHTS[i]));",
  "    (i = (i + 1));",
  "  }",
  "})",
  "MAIN({",
  "  FIRST_BBLOCK(start);",
  "  SMC(callback);",
  "})"
] using eqString in

()
