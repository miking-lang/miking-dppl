-- CorePPL compiler, targeting the RootPPL framework

include "../coreppl/coreppl.mc"
-- include "../models/crbd.mc"

include "rootppl.mc"

include "mexpr/ast-builder.mc"

include "assoc-seq.mc"
include "name.mc"
include "map.mc"

include "c/ast.mc"
include "c/pprint.mc"
include "c/compile.mc"

-----------------------------------------
-- BASE LANGUAGE FRAGMENT FOR COMPILER --
-----------------------------------------

lang MExprPPLRootPPLCompile = MExprPPL + RootPPL + MExprCCompile

  -- Compiler internals
  syn CExpr =
  | CEAlloc {} -- Allocation placeholder
  | CEResample {} -- Indicates resample locations

  sem printCExpr (env: PprintEnv) =
  | CEAlloc {} -> (env, "<<<CEAlloc>>>")
  | CEResample {} -> (env, "<<<CEResample>>>")

  sem sfold_CExpr_CExpr (f: a -> CExpr -> a) (acc: a) =
  | CEAlloc _ -> acc
  | CEResample _ -> acc

  sem smap_CExpr_CExpr (f: CExpr -> CExpr) =
  | CEAlloc _ & t -> t
  | CEResample _ & t -> t

  -- ANF override
  -- Ensures argument to assume is not lifted by ANF (first-class distributions
  -- not supported in RootPPL)
  sem normalize (k : Expr -> Expr) =
  | TmAssume ({ dist = TmDist ({ dist = dist } & td) } & t) ->
    normalizeDist
      (lam dist. k (TmAssume { t with dist = TmDist { td with dist = dist } }))
      dist

  -- Extensions
  sem compileDist (env: CompileCEnv) =
  | DBernoulli { p = p } -> CDBern { p = compileExpr env p }
  | DBeta { a = a, b = b } ->
    CDBeta { a = compileExpr env a, b = compileExpr env b }
  | DCategorical { p = p } -> CDCategorical { p = compileExpr env p }
  | DMultinomial { n = n, p = p } ->
    CDMultinomial { n = compileExpr env n, p = compileExpr env p }
  | DDirichlet { a = a } -> CDDirichlet { a = compileExpr env a }
  | DExponential { rate = rate } -> CDExp { rate = compileExpr env rate }
  | DEmpirical { samples = samples } ->
    CDEmpirical { samples = compileExpr env samples }
  | DUniform { a = a, b = b } ->
    CDUniform { a = compileExpr env a, b = compileExpr env b }
  | DPoisson { lambda = lambda } ->
    CDPoisson { lambda = compileExpr env lambda }
  | DGamma { k = k, theta = theta } ->
    CDGamma { k = compileExpr env k, theta = compileExpr env theta}

  -- Extensions
  sem compileExpr (env: CompileCEnv) =
  | TmAssume _ -> error "Assume without direct distribution"
  | TmAssume { dist = TmDist { dist = dist }} ->
    CESample { dist = compileDist env dist }
  | TmWeight { weight = weight } -> CEWeight { weight = compileExpr env weight }
  | TmResample _ -> CEResample {}

  -- Allocation
  sem alloc (name: Name) =
  -- | CTyVar { id = tyName } & ty ->
  | CTyPtr { ty = CTyStruct { id = Some ident, mem = None() } & ty } & ptrTy ->
    let allocName = nameSym "alloc" in
    [
      -- Placeholder definition of pointer to allocated struct
      { ty = ptrTy
      , id = Some name
      , init = Some (CIExpr { expr = CEAlloc {} })
      }
    ]
  | _ -> error "Incorrect type in alloc"

end

----------------------
-- ROOTPPL COMPILER --
----------------------

-- Generate code for compiled RootPPL AST
let printCompiledRPProg: RPProg -> String = use MExprPPLRootPPLCompile in
  lam rpprog: RPProg.
    printRPProg cCompilerNames rpprog

-- Compiler names
let nameRetAddr = nameSym "ra"
let nameRetLoc = nameSym "retLoc"
let nameInit = nameSym "init"
let nameSF = nameSym "sf"
let nameGlobal = nameSym "global"
let nameCallSF = nameSym "callsf"
let nameStack = nameSym "stack"
let nameStackPtr = nameSym "stackPtr"
let nameRet = nameSym "ret"
let nameGlobalTy = nameSym "GLOBAL"

-- RootPPL compile function
let rootPPLCompileH: [(Name,Type)] -> [Name] -> Expr -> RPProg =
  use MExprPPLRootPPLCompile in
  lam typeEnv: [(Name,Type)].
  lam globalNames: [Name].
  lam prog: Expr.

    -------------------------
    -- RUN BASE C COMPILER --
    -------------------------

    match compile typeEnv prog with (env, types, tops, inits) then

    ------------------------
    -- COMMON DEFINITIONS --
    ------------------------

    -- Set of externals used in the program
    let externals = mapValues env.externals in

    -- Function indicating which functions are free of resamples
    let resampleFree: Name -> Bool = lam n.
      if any (nameEq n) externals then true
      else false -- This is too conservative
    in


    -- Extract the name and type from a definition, or recurse further.
    recursive let extractDef: Map Name Type -> CStmt -> Map Name Type =
      lam map. lam stmt.
        match stmt with CSDef { ty = ty, id = Some id } then
          mapInsert id ty map
        else sfold_CStmt_CStmt extractDef map stmt
    in

    -- Map from all names to their C types
    let defMap: Map Name CType =
      let m = mapEmpty nameCmp in
      let m = foldl (lam m. lam top.
        let m = match top with CTFun { params = params } then
                    foldl (lam m. lam t. mapInsert t.1 t.0 m) m params
                  else m in
        sfold_CTop_CStmt extractDef m top
      ) m tops in
      let m = foldl extractDef m inits in
      m
    in

    ----------------------------
    -- DETERMINE STACK FRAMES --
    ----------------------------

    -- Stackframe type, containing relevant names and types
    type StackFrame = {
      id: Name,
      mem: [(Name,CType)],
      ret: CType,
      params: [Name]
    } in

    -- Initialize a new stack frame
    let newStackFrame: StackFrame = lam name. lam ret. {
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

    -- Unwraps pointer type one step
    let tyDeref: CType -> CType = lam ty: CType.
      match ty with CTyPtr { ty = ty } then ty
      else error "Non-pointer in tyDeref"
    in

    -- Add a local alloc to accumulator
    let addLocalAlloc: AccStackFrames -> (Name,CType) -> AccStackFrames =
      lam acc: AccStackFrames. lam e: (Name,CType).
        -- Guaranteed to not already exist, so no check needed here
        {acc with sf = {acc.sf with mem = cons (e.0,tyDeref e.1) acc.sf.mem}}
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
        foldl (lam acc: AccStackFrames. lam stmt: Stmt.

          -- Add def, if applicable
          let acc =
            match stmt with CSDef { ty = ty, id = Some name }
            then { acc with defs = cons (name,ty) acc.defs } else acc
          in

          -- Always add pointers to allocated structs to locals, since these
          -- might implicitly traverse block boundaries through other
          -- variables.
          let acc =
            match stmt with CSDef {
              ty = ty,
              id = Some id,
              init = Some (CIExpr { expr = CEAlloc {} })
            } then addLocalAlloc acc (id,ty) else acc
          in

          -- Add used locals from prevDef
          let acc =
            match stmt with CSDef _ | CSExpr _ | CSRet _ | CSNop _ then
              sfold_CStmt_CExpr addLocals acc stmt
            else match stmt with CSIf { cond = cond } then
              addLocals acc cond
            else error "Unsupported term when adding locals"
          in

          -- Block split on applications
          match stmt with CSDef { init = Some (CIExpr { expr = CEApp app }) }
                        | CSExpr { expr = CEApp app } then
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
    -- functions
    let sfs: [(Name,StackFrame)] =
        foldl (lam acc: [(Name,StackFrame)]. lam top: CTop.
          match top
          with CTFun { ret = ret, id = id, params = params, body = body } then

            if resampleFree id then acc else

            -- Initialize accumulator for new function
            let sf = {{ newStackFrame id ret with
              mem = map (lam t. (t.1,t.0)) params} with
              params = map (lam t. t.1) params
            } in
            let lAcc = emptyAccSF sf in

            -- Get locals that traverse BBLOCK boundaries in function
            let lAcc = findLocals lAcc body in

            -- Record stack frame
            snoc acc (id,lAcc.sf)

          else acc) [] tops
    in

    -- Top-level return type
    let retTy: CType = compileType env (ty prog) in

    -- Get stack frame for initialization code
    let initSF: StackFrame =
        let sf = newStackFrame nameInit retTy in
        let acc = findLocals (emptyAccSF sf) inits in
        acc.sf
    in

    -------------------------------------------------------
    -- FILTER OUT GLOBAL VARIABLES FROM INIT STACK FRAME --
    -------------------------------------------------------

    -- Retrieve global allocations from a list of C statements
    let globals: [(Name,Ctype)] =
        foldl (lam acc: [(Name,CType)]. lam stmt: CStmt.

          -- Global struct allocation
          match stmt with CSDef {
            ty = ty,
            id = Some name,
            init = Some (CIExpr { expr = CEAlloc {} })
          } then
            if any (nameEq name) globalNames then snoc acc (name, tyDeref ty)
            else acc

          -- Global non-struct allocation
          else match stmt with CSDef { ty = ty, id = Some name } then
            if any (nameEq name) globalNames then snoc acc (name,ty)
            else acc

          else acc

        ) [] inits
    in

    let f = lam g: [(Name,CType)].
      filter (lam l. not (any (lam r. nameEq l.0 r.0) g)) in
    let initSF: StackFrame = { initSF with mem =
      filter (lam l. not (any (lam r. nameEq l.0 r.0) globals)) initSF.mem
    } in

    ----------------------------------
    -- REMOVE FUNCTION DECLARATIONS --
    ----------------------------------

    let tops: [CTop] = foldl (lam acc. lam top.
      match top with CTDef { ty = CTyFun _ } then acc else snoc acc top
    ) [] tops in


    -------------------------
    -- REPLACE DEFINITIONS --
    -------------------------

    -- Used for removing def statements, since allocation for the corresponding
    -- variables is handled elsewhere.
    let replaceDefs: [(Name,CType)] -> CStmt -> [CStmt] =
      lam locals: [(Name,CType)].
      lam stmt: CStmt.

        recursive let rec = lam stmt: CStmt.

          -- Always remove cealloc
          match stmt with CSDef { init = Some (CIExpr { expr = CEAlloc {} }) }
          then []

          -- Replace def if local or global
          else match stmt with CSDef { id = Some id, init = init } then
            let g = any (lam g. nameEq id g.0) globals in
            let l = any (lam l. nameEq id l.0) locals in
            if or g l then
              match init with Some init then
                match init with CIExpr { expr = expr } then
                  [CSExpr { expr = CEBinOp {
                    op = COAssign {},
                    lhs = CEVar { id = id },
                    rhs = expr
                  }}]
                else match init with None () then
                  error "Non-CIExpr initializer in replaceDefs"
                else never
              else []
            else [stmt]

          -- Recurse
          else [sreplace_CStmt_CStmt rec stmt]

        in rec stmt
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

    -- Replace defs
    let tops: [CTop] = map (lam top.
      match top with CTFun { id = id } then
        let locals = if resampleFree id then [] else (topStackFrame top).mem in
        sreplace_CTop_CStmt (replaceDefs locals) top
      else error "Not a CTFun when replacing defs"
    ) tops in
    let inits: [CStmt] = join (map (replaceDefs initSF.mem) inits) in


    --------------------------
    -- REPLACE DEREFERENCES --
    --------------------------

    -- Stack and stack pointer expressions
    let stack = CEMember { lhs = CEPState {}, id = nameStack } in
    let stackPtr = CEMember { lhs = CEPState {}, id = nameStackPtr } in

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

    -- Convert expressions part of dereferencing to absolute addresses.
    let replaceDeref: CExpr -> CExpr =
      lam expr: CExpr.
        recursive let rec = lam expr: CExpr.

          match expr with CEArrow { lhs = lhs, id = id } then

            match lhs with CEVar { id = lhsId } then
              let err = lam t. error "Type not found in replaceDeref" in
              CEArrow {
                lhs = toAbsAddr lhs (mapFindOrElse err lhsId defMap),
                id = id
              }
            else error "Non-var at LHS of arrow deref"

          else match expr with CEUnOp { op = CODeref _, arg = arg } then
            error "TODO in replaceDeref?"

          else smap_CExpr_CExpr rec expr

        in rec expr
    in

    let tops: [CTop] = map (lam top.
      smap_CTop_CExpr replaceDeref top
    ) tops in
    let inits: [CStmt] = map (smap_CStmt_CExpr replaceDeref) inits in


    ---------------------------
    -- REPLACE VARIABLE USES --
    ---------------------------

    -- Replace variables to refer to the correct location (in globals or
    -- stackframe)
    let replaceVar: [(Name,CType)] -> StackFrame -> CExpr -> CExpr =
      lam locals: [(Name,CType)].
      lam expr: CExpr.
        recursive let rec = lam expr: CExpr.
          match expr with CEVar { id = id } then

            match assocSeqLookup {eq=nameEq} id globals with Some cty then
              match cty with CTyStruct _ then
                -- Stored in globals as non-pointer but must be converted to
                -- relative pointer
                let abs = CEUnOp { op = COAddrOf {},
                                   arg = CEArrow {
                                     lhs = CEVar { id = nameGlobal },
                                     id = id
                                   } } in
                toRelAddr abs (CTyPtr { ty = cty })
              else
                -- Stored directly in globals
                CEArrow { lhs = CEVar { id = nameGlobal }, id = id }

            else match assocSeqLookup {eq=nameEq} id locals with Some cty then
              match cty with CTyStruct _ then
                -- Stored in stack frame as non-pointer but must be converted
                -- to relative pointer
                let abs = CEUnOp { op = COAddrOf {},
                                   arg = CEArrow {
                                     lhs = CEVar { id = nameSF },
                                     id = id
                                   } } in
                toRelAddr abs (CTyPtr { ty = cty })

              else
                -- Stored directly in stack frame
                CEArrow { lhs = CEVar { id = nameSF }, id = id }

            -- Leave other variables
            else expr

          else smap_CExpr_CExpr rec expr
        in rec expr
    in

    let tops: [CTop] = map (lam top.
      match top with CTFun { id = id } then
        let locals = if resampleFree id then [] else (topStackFrame top).mem in
        smap_CTop_CExpr (replaceVar locals) top
      else error "Not a CTFun when replacing var uses"
    ) tops in
    let inits: [CStmt] = map (smap_CStmt_CExpr (replaceVar initSF.mem)) inits in


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
      -- Indicates if a split has occured at the end of current block
      hasSplit: Bool,
      -- Accumulated block
      block: [CStmt],
      -- Accumulated set of top-level definitions
      tops: [CTop]
    } in

    -- Empty accumulator
    let emptyAccSplit = lam sfs.
      { next = Collapse (), hasSplit = false, block = [], sfs = sfs, tops = [] }
    in

    -- C statement for setting the PC to a name
    let setPCFromExpr: CExpr -> CExpr = lam expr.
      (CSExpr { expr = (CEBinOp { op = COAssign {}, lhs = CEPC {}
                                , rhs = expr })})
    in

    -- C statement for setting the PC to a BBLOCK name
    let setPC: Name -> CExpr = lam name.
      setPCFromExpr (CEBBlockName { name = name })
    in

    -- Used when reaching endpoints, ensures the accumulator has a name for the
    -- next block.
    let initNextName: AccSplit -> AccSplit = lam acc: AccSplit.
      match acc.next with Block (Some name) then acc
      else match acc.next with Block (None ()) then
        {acc with next = Block (Some (nameSym "lazy"))}
      else error "Error in initNextName"
    in

    -- Get the name for the next block from accumulator
    let getNextName: AccSplit -> Name = lam acc: AccSplit.
      match acc.next with Block (Some name) then name
      else error "Error in getNextName"
    in

    -- BBLOCK_CALL(name, NULL)
    let callBBlock = lam name: Name.
      CSExpr { expr = CEApp {
        fun = nameBblockCall, args = [CEVar {id = name}, CEVar {id = nameNull}]
      } }
    in

    -- BBLOCK_CALL(DATA_POINTER(bblocksArr)[expr])
    let callFromExpr = lam expr: CExpr.
        CSExpr { expr = CEApp {
          fun = nameBblockCall, args = [
            CEBinOp {
              op = COSubScript {},
              lhs = CEApp {
                fun = nameDataPointer,
                args = [ CEVar { id = nameBblocksArr}]
              },
              rhs = expr
            },
            CEVar { id = nameNull }
          ] }
        }
    in


    -- Deref shorthand
    let derefExpr: CExpr -> CExpr = lam expr.
      CEUnOp { op = CODeref {}, arg = expr}
    in

    -- Global frame
    let globalDef =
      let ty = CTyStruct { id = Some nameGlobalTy, mem = None () } in
      CSDef {
        ty = CTyPtr { ty = ty },
        id = Some nameGlobal,
        init = Some (CIExpr {
          expr = CECast {
            ty = CTyPtr { ty = ty },
            rhs = stack
          }
        })
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

    -- Generate code for collapsing a stack frame.
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
        StackFrame -> Name -> [CExpr] -> Option CExpr -> Name -> [CStmt] =
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
            [f nameRetLoc
               (toRelAddr (CEUnOp { op = COAddrOf {}, arg = expr })
                  (CTyPtr { ty = sf.ret }))]
          else []
        in

        let incr = incrementStackPtr sf in

        let call = callBBlock fun in

        join [[callsf, ra], args, ret, [incr, call]]
    in

    let constructCallFromStmt: StackFrame -> CStmt -> Name -> [Cstmt] =
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
                             (CEBBlockName { name = ra }) in
                let block = concat acc.block call in
                {acc with block = block}
              else
                -- Not last statement, but end of block
                let accNext = {acc with block = []} in
                let accNext = splitStmts accNext sf stmts in
                let ra = nameSym "bblock" in
                let accNext = createBlock sf ra accNext in

                let call = constructCallFromStmt funStackFrame stmt
                             (CEBBlockName { name = ra }) in
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
                   snoc block (setPCFromExpr
                                 (CEArrow {
                                   lhs = CEVar { id = nameSF }, id = nameRetAddr
                                  })
                              ) }
              else match acc.next with Block _ then
                -- Set PC to next block
                let acc = initNextName acc in
                let name = getNextName acc in
                {acc with block = snoc acc.block (setPC name)}
              else never
            else
              -- CASE: Not last statement, but end of block
              let accNext = {acc with block = []} in
              let accNext = splitStmts accNext sf stmts in
              let name = nameSym "bblock" in
              let accNext = createBlock sf name accNext in
              -- Simply set PC to next block
              {accNext with block = snoc acc.block (setPC name)}

          -- Write to return value
          else match stmt with CSRet { val = Some val } then
            let stmt = CSExpr { expr = CEBinOp {
              op = COAssign {},
              lhs =
                let expr =
                  CEArrow { lhs = CEVar { id = nameSF }, id = nameRetLoc } in
                derefExpr (toAbsAddr expr (CTyPtr { ty = sf.ret })),
              rhs = val
            }}
            in splitStmts {acc with block = snoc acc.block stmt} sf stmts

          -- Not a function application, resample, or return. Just accumulate
          -- and continue
          else match stmt with CSDef _ | CSExpr _ | CSRet _ | CSNop _ then
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
              splitStmts {acc with block = snoc acc.block stmt} sf stmts

            -- At least one split in branches
            else
              let update = lam acc: AccSplit.
                match accEls.next with Block (Some name) then
                  if acc.hasSplit then acc
                  else {acc with block = snoc acc.block (callBBlock name)}
                else acc
              in
              let accThn = update accThn in
              let accEls = update accEls in
              let stmt =
                CSIf { cond = cond, thn = accThn.block, els = accEls.block } in
              let block = snoc acc.block stmt in

              -- Create new block for stmts if needed
              let accStmts = {{ accEls with hasSplit = true }
                                       with next = acc.next } in
              match stmts with [] then {accStmts with block = block}
              else
                let accStmts = {accStmts with block = []} in
                let accStmts = splitStmts accStmts sf stmts in
                let name = getNextName accEls in
                let accStmts = createBlock sf name accStmts in
                {accStmts with block = block}

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
              {acc with block = snoc acc.block (callBBlock name)}
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
    -- NOTE(dlunde,2021-05-21): Currently, _all_ functions are split. Later on,
    -- we only want to split where necessary.
    let splitFunctions: AccSplit -> [CTop] -> AccSplit =
      lam acc: AccSplit. lam tops: [CTop].
        foldl (lam acc: AccSplit. lam top: CTop.

          -- Split functions
          match top with CTFun { id = id } then
            if resampleFree id then {acc with tops = snoc acc.tops top}
            else splitFunction acc top

          -- Remove all function declarations, this is handled separately in any
          -- case.
          else match top with CTDef { ty = CTyFun _ } then
            error "CTDef should have been removed before splitFunctions"

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
        match createBlock sf nameInit acc with { tops = tops } then tops
        else never
    in

    -- Split functions into BBLOCKs
    let accSplit = emptyAccSplit sfs in
    let tops = match splitFunctions accSplit tops with { tops = tops }
               then tops else never in
    let tops = concat tops (splitInit accSplit initSF inits) in


    -------------------
    -- FINAL TOUCHES --
    -------------------

    -- Stack size (should be determined from command line arg)
    let stackSize = 100000 in

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
    let nameEnd = nameSym "end" in
    let initCall =
      constructCall initSF nameInit
        [] initRet (CEBBlockName { name = nameEnd }) in
    let startBBlock = CTBBlock {
      id = nameSym "start",
      body = join [
        [globalDef, initStackPtr],
        initCall
      ]
    } in
    let endBBlock = CTBBlock {
      id = nameEnd,
      body = [setPCFromExpr (CEInt { i = negi 1 })]
    } in

    -- PSTATE contents
    -- let progStateMem = [
    --   ( CTyArray { ty = CTyChar {}, size = Some (CEInt { i = stackSize })}
    --   , Some nameStack ),
    --   ( CTyVar { id = nameUIntPtr }, Some nameStackPtr)
    -- ] in

    -- Convert a [(Name,CType)] to [(CType, Option Name)].
    let convertMem = map (lam t. (t.1, Some t.0)) in

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
            [(CTyInt {}, Some nameRetAddr)],
            match sf.ret with ! CTyVoid _ then
              [(CTyPtr { ty = sf.ret }, Some nameRetLoc)]
            else [],
            mem
          ])
        },
        id = None (),
        init = None ()
      }
    in

    -- Stack frame types
    let initSF = stackFrameToTopTy initSF in
    let sfs = map (lam t. stackFrameToTopTy t.1) sfs in

    RPProg {
      includes = join [
        cIncludes,
        [ "\"inference/smc/smc.cuh\""
        , "<stdint.h>"
        ]
      ],
      -- pStateTy = Some (CTyStruct { id = None (), mem = Some progStateMem }),
      pStateTy = None (),
      types = types,
      tops = join [[gf, initSF], sfs, [startBBlock, endBBlock], tops]
    }

    else never

-- Get the names of all globally accessible non-function data.
let findGlobalNames: Expr -> [Name] = use MExprPPLRootPPLCompile in
  lam expr: Expr.
    recursive let rec = lam acc: [Name]. lam expr: Expr.
      match expr with TmLet { ident = ident, body = ! TmLam _, inexpr = inexpr }
      then rec (cons ident acc) inexpr
      else match expr with
        TmLet { inexpr = inexpr }
        | TmRecLets { inexpr = inexpr }
        | TmType { inexpr = inexpr }
        | TmConDef { inexpr = inexpr }
        | TmExt { inexpr = inexpr }
      then rec acc inexpr
      else acc
    in rec [] expr

-- Entry point for compiler
let rootPPLCompile: Expr -> RPProg = use MExprPPLRootPPLCompile in lam prog.

  -- print (expr2str prog); print "\n\n";
  -- dprint prog; print "\n\n";

  -- Symbolize with empty environment
  let prog: Expr = symbolizeExpr symEnvEmpty prog in

  -- Find global non-function definitions
  let globals: [Name] = findGlobalNames prog in

  -- Type annotate
  let prog: Expr = typeAnnot prog in

  -- ANF transformation
  let prog: Expr = normalizeTerm prog in

  -- TODO Find resample-free functions

  -- TODO Find shared global data

  -- Type lift
  match typeLift prog with (env, prog) then

    -- print (expr2str prog); print "\n\n";

    -- Run RootPPL compiler
    let rpprog: RPProg = rootPPLCompileH env globals prog in

    -- print (printCompiledRPProg rpprog); print "\n\n";

    rpprog

  else never

mexpr
-- use MExprPPLRootPPLCompile in
-- rootPPLCompile crbd;

()
